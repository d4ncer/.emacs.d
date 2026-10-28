;;; +agent-shell-egress.el --- Measure where arena members connect -*- lexical-binding: t; -*-

;;; Commentary:
;; An opt-in observer of one sandboxed arena member's network destinations,
;; saved as a metadata-only JSON record beside the member's report.  A
;; panel opens a session before the member's adapter starts, the drive
;; attaches the adapter's process, and the panel closes the session once
;; the member's processes are stopped.  Nothing here can stop, delay or
;; change a member: every failure becomes a fixed reason code.
;;
;; On Linux the sandbox launcher runs tshark in the member's outer user and
;; network namespaces, where it sees packets on tap0 before slirp4netns
;; translates them; see +agent-shell-sandbox-launch.sh.  It writes chosen
;; header fields only into a private folder this library reads on close.
;; On macOS this library samples the sockets of the adapter's process tree
;; with lsof, plus processes working in the sandbox as uncertain matches.
;; Observers run with a minimal environment, never the agent's.
;;
;; See docs/AGENT-SHELL-PSTACK-ARENA.md, "Measuring egress".

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(require '+agent-shell-sandbox)

(defvar +agent-shell-egress-interval 1
  "Seconds between macOS socket samples.")

(defvar +agent-shell-egress--lsof "/usr/sbin/lsof"
  "The lsof a macOS observer runs.")

(defvar +agent-shell-egress--ps "/bin/ps"
  "The ps a macOS observer runs.")

(defconst +agent-shell-egress--environment
  '("PATH=/usr/bin:/bin:/usr/sbin:/sbin" "LANG=C" "DISPLAY")
  "The whole environment of every observer process Emacs starts.")

(defconst +agent-shell-egress--cwd-every 5
  "Samples per scan for processes working in the sandbox.")

(defconst +agent-shell-egress--max-destinations 512
  "Most destinations one record holds.")

(defconst +agent-shell-egress--max-names 16
  "Most names one destination holds.")

(defconst +agent-shell-egress--max-output (* 8 1024 1024)
  "Most bytes of one observer command's output read.")

(defconst +agent-shell-egress--max-events (* 64 1024 1024)
  "Most bytes of a Linux observer's field output read.")

(defconst +agent-shell-egress--max-record (* 4 1024 1024)
  "Largest record file read.")

(defconst +agent-shell-egress--close-wait 1
  "Seconds a close waits for an observer command still running.")

(defconst +agent-shell-egress--reasons
  '(helper-missing namespace-entry-failed capture-permission-denied
    readiness-timeout observer-exited observer-not-started
    observer-interrupted capacity-exceeded no-samples artifact-missing
    artifact-invalid)
  "Fixed codes saying why measurement was unavailable or partial.")

(defconst +agent-shell-egress--limits
  '(encrypted-names delegated-traffic sampled-sockets names-unknown
    unconnected-udp direction-unknown uncertain-attribution
    capacity-exceeded unparsed-events)
  "Fixed codes naming what a record cannot show.")

(defconst +agent-shell-egress--backend-limits
  '((linux-tshark encrypted-names delegated-traffic)
    (macos-lsof sampled-sockets names-unknown unconnected-udp
                direction-unknown delegated-traffic))
  "Limits every record of each backend has.")

(defconst +agent-shell-egress--states
  '(observing partial finished unavailable not-started)
  "Record states.  All but `observing' are final.")

(define-error '+agent-shell-egress-invalid "Invalid egress record")

;;;; Records

(cl-defstruct (+agent-shell-egress-record
               (:constructor +agent-shell-egress--make-record)
               (:copier nil))
  "What was observed of one member's network destinations.
IDENTITY is a plist of :panel, :arena, :role and :kind.  BACKEND is
`linux-tshark', `macos-lsof' or nil when the member never started.
REASON is a code from `+agent-shell-egress--reasons' or nil.  WRITTEN
says the record reached its file; it is not saved."
  identity backend (state 'observing) reason started-at ended-at observers
  limits destinations (written nil))

(cl-defstruct (+agent-shell-egress-observer
               (:constructor +agent-shell-egress--make-observer)
               (:copier nil))
  "One observation method's outcome.  STATE is `observing', `stopped',
`interrupted' or `unavailable'.  DROPPED counts what it reported losing,
or is nil when unknown."
  method (state 'observing) reason dropped)

(cl-defstruct (+agent-shell-egress-destination
               (:constructor +agent-shell-egress--make-destination)
               (:copier nil))
  "One numeric endpoint a member reached and its evidence.
PROTOCOL is `tcp', `udp' or `dns'.  NAMES holds (NAME . SOURCE) with
SOURCE `dns' or `sni'; on a `dns' destination they are the questions it
was asked.  COUNT is TCP connection attempts, UDP datagrams or DNS
queries on Linux, and distinct sampled sockets on macOS.  SOURCE is the
method that saw it: `tshark', `lsof', or `lsof-cwd' when only a process
working in the sandbox, not one known to descend from the adapter, held
it."
  ip port protocol names first-seen last-seen (count 0) source)

(defun +agent-shell-egress--time (seconds)
  "Return SECONDS, a float time, as UTC ISO 8601 text."
  (format-time-string "%FT%T.%3NZ" seconds t))

(defun +agent-shell-egress--json (record)
  "Return RECORD as `json-serialize' takes it, saved fields only."
  (let ((identity (+agent-shell-egress-record-identity record))
        (name (lambda (symbol) (if symbol (symbol-name symbol) :null))))
    `((version . 1)
      (panel . ,(plist-get identity :panel))
      (arena . ,(or (plist-get identity :arena) :null))
      (role . ,(plist-get identity :role))
      (kind . ,(symbol-name (plist-get identity :kind)))
      (backend . ,(funcall name (+agent-shell-egress-record-backend record)))
      (state . ,(symbol-name (+agent-shell-egress-record-state record)))
      (reason . ,(funcall name (+agent-shell-egress-record-reason record)))
      (started_at . ,(or (+agent-shell-egress-record-started-at record) :null))
      (ended_at . ,(or (+agent-shell-egress-record-ended-at record) :null))
      (observers
       . ,(vconcat
           (mapcar (lambda (observer)
                     `((method . ,(symbol-name
                                   (+agent-shell-egress-observer-method
                                    observer)))
                       (state . ,(symbol-name
                                  (+agent-shell-egress-observer-state
                                   observer)))
                       (reason . ,(funcall
                                   name (+agent-shell-egress-observer-reason
                                         observer)))
                       (dropped . ,(or (+agent-shell-egress-observer-dropped
                                        observer)
                                       :null))))
                   (+agent-shell-egress-record-observers record))))
      (limits . ,(vconcat (mapcar #'symbol-name
                                  (+agent-shell-egress-record-limits record))))
      (destinations
       . ,(vconcat
           (mapcar
            (lambda (destination)
              `((ip . ,(+agent-shell-egress-destination-ip destination))
                (port . ,(+agent-shell-egress-destination-port destination))
                (protocol . ,(symbol-name
                              (+agent-shell-egress-destination-protocol
                               destination)))
                (names . ,(vconcat
                           (mapcar (pcase-lambda (`(,name . ,source))
                                     `((name . ,name)
                                       (source . ,(symbol-name source))))
                                   (+agent-shell-egress-destination-names
                                    destination))))
                (first_seen . ,(+agent-shell-egress-destination-first-seen
                                destination))
                (last_seen . ,(+agent-shell-egress-destination-last-seen
                               destination))
                (count . ,(+agent-shell-egress-destination-count destination))
                (source . ,(symbol-name
                            (+agent-shell-egress-destination-source
                             destination)))))
            (+agent-shell-egress-record-destinations record)))))))

(defun +agent-shell-egress--write (record file)
  "Save RECORD to FILE, 0600 in a 0700 folder, through a rename.
Return whether it was saved; a failure leaves RECORD only in memory."
  (let ((temporary nil))
    (setf (+agent-shell-egress-record-written record)
          (condition-case nil
              (let ((directory (file-name-directory file)))
                (make-directory directory t)
                (set-file-modes directory #o700)
                (setq temporary (make-temp-file
                                 (expand-file-name ".egress-" directory)))
                (set-file-modes temporary #o600)
                (let ((coding-system-for-write 'utf-8-unix))
                  (write-region (concat (json-serialize
                                         (+agent-shell-egress--json record))
                                        "\n")
                                nil temporary nil 'silent))
                (rename-file temporary file t)
                t)
            (error (when temporary (ignore-errors (delete-file temporary)))
                   nil)))))

(defconst +agent-shell-egress--name-regexp
  (rx bos (+ (any "a-z0-9_-")) (* "." (+ (any "a-z0-9_-"))) eos)
  "A host name as a record holds it.")

(defun +agent-shell-egress--name (text)
  "Return TEXT as a lowercase host name, or nil when it is not one."
  (when (stringp text)
    (let ((name (downcase (string-remove-suffix "." text))))
      (and (<= 1 (length name) 253)
           (string-match-p +agent-shell-egress--name-regexp name)
           (seq-every-p (lambda (label) (<= (length label) 63))
                        (split-string name "\\."))
           name))))

(defconst +agent-shell-egress--ipv4-regexp
  (rx bos (= 3 (** 1 3 digit) ".") (** 1 3 digit) eos)
  "A numeric IPv4 address.")

(defconst +agent-shell-egress--ipv6-regexp
  (rx bos (** 2 39 (any "0-9a-f:")) (? "%" (** 1 32 (any "a-z0-9_.-"))) eos)
  "A numeric IPv6 address with an optional scope.")

(defun +agent-shell-egress--ip (text)
  "Return TEXT as a normalized numeric IP address, or nil."
  (when (stringp text)
    (let ((ip (downcase text)))
      (cond ((string-match-p +agent-shell-egress--ipv4-regexp ip)
             (and (seq-every-p (lambda (octet) (<= (string-to-number octet) 255))
                               (split-string ip "\\."))
                  ip))
            ((and (string-match-p +agent-shell-egress--ipv6-regexp ip)
                  (string-search ":" ip))
             ip)))))

(defun +agent-shell-egress--port (value)
  "Return VALUE, a string or number, as a port number, or nil."
  (let ((number (cond ((natnump value) value)
                      ((and (stringp value) (string-match-p "\\`[0-9]\\{1,5\\}\\'"
                                                            value))
                       (string-to-number value)))))
    (and number (<= number 65535) number)))

(defconst +agent-shell-egress--time-regexp
  (rx bos (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "T" (= 2 digit) ":"
      (= 2 digit) ":" (= 2 digit) (? "." (** 1 9 digit)) "Z" eos)
  "A UTC ISO 8601 time as a record holds it.")

(defun +agent-shell-egress--invalid (what)
  "Signal that a record is invalid because of WHAT, a fixed phrase."
  (signal '+agent-shell-egress-invalid (list what)))

(defun +agent-shell-egress--field (object key check what &optional nullable)
  "Return OBJECT's KEY parsed by CHECK, or signal naming WHAT.
CHECK returns :bad for a value it refuses.  NULLABLE allows null, which
is returned as nil."
  (let ((value (alist-get key object :absent)))
    (cond ((eq value :absent)
           (+agent-shell-egress--invalid (format "missing %s" what)))
          ((and nullable (eq value :null)) nil)
          (t (let ((parsed (funcall check value)))
               (if (eq parsed :bad)
                   (+agent-shell-egress--invalid (format "bad %s" what))
                 parsed))))))

(defun +agent-shell-egress--keys (object keys what)
  "Signal unless OBJECT, an alist, has exactly KEYS; WHAT names it."
  (unless (and (listp object) (seq-every-p #'consp object)
               (equal (sort (mapcar #'car object) #'string<)
                      (sort (copy-sequence keys) #'string<)))
    (+agent-shell-egress--invalid (format "unexpected %s fields" what))))

(defun +agent-shell-egress--member (symbols)
  "Return a check parsing a string naming one of SYMBOLS as that symbol."
  (lambda (value)
    (or (and (stringp value) (car (memq (intern-soft value) symbols))) :bad)))

(defun +agent-shell-egress--text (pattern)
  "Return a check accepting a string matching PATTERN."
  (lambda (value)
    (if (and (stringp value) (string-match-p pattern value)) value :bad)))

(defun +agent-shell-egress--natnum (value)
  "Return VALUE if it is a whole number, else :bad."
  (if (natnump value) value :bad))

(defun +agent-shell-egress--list (check)
  "Return a check parsing a list whose elements each pass CHECK."
  (lambda (value)
    (if (not (listp value)) :bad
      (let ((items (mapcar check value)))
        (if (memq :bad items) :bad items)))))

(defun +agent-shell-egress--parse (object)
  "Return the record parsed JSON OBJECT holds, or signal why it is invalid."
  (+agent-shell-egress--keys
   object '(version panel arena role kind backend state reason started_at
            ended_at observers limits destinations)
   "record")
  (unless (equal (alist-get 'version object) 1)
    (+agent-shell-egress--invalid "unknown version"))
  (let* ((id (+agent-shell-egress--text "\\`[A-Za-z0-9_-]\\{1,64\\}\\'"))
         (time (+agent-shell-egress--text +agent-shell-egress--time-regexp))
         (reason (+agent-shell-egress--member +agent-shell-egress--reasons))
         (observer
          (lambda (value)
            (+agent-shell-egress--keys value '(method state reason dropped)
                                       "observer")
            (+agent-shell-egress--make-observer
             :method (+agent-shell-egress--field
                      value 'method
                      (+agent-shell-egress--member '(tshark lsof)) "method")
             :state (+agent-shell-egress--field
                     value 'state
                     (+agent-shell-egress--member
                      '(observing stopped interrupted unavailable))
                     "observer state")
             :reason (+agent-shell-egress--field value 'reason reason
                                                 "observer reason" t)
             :dropped (+agent-shell-egress--field
                       value 'dropped #'+agent-shell-egress--natnum "dropped"
                       t))))
         (name
          (lambda (value)
            (+agent-shell-egress--keys value '(name source) "name")
            (cons (+agent-shell-egress--field
                   value 'name
                   (lambda (text)
                     (if (equal text (+agent-shell-egress--name text)) text
                       :bad))
                   "name")
                  (+agent-shell-egress--field
                   value 'source (+agent-shell-egress--member '(dns sni))
                   "name source"))))
         (destination
          (lambda (value)
            (+agent-shell-egress--keys
             value '(ip port protocol names first_seen last_seen count source)
             "destination")
            (+agent-shell-egress--make-destination
             :ip (+agent-shell-egress--field
                  value 'ip (lambda (text)
                              (if (equal text (+agent-shell-egress--ip text))
                                  text
                                :bad))
                  "address")
             :port (+agent-shell-egress--field
                    value 'port
                    (lambda (port) (or (+agent-shell-egress--port port) :bad))
                    "port")
             :protocol (+agent-shell-egress--field
                        value 'protocol
                        (+agent-shell-egress--member '(tcp udp dns))
                        "protocol")
             :names (+agent-shell-egress--field
                     value 'names
                     (lambda (names)
                       (if (and (listp names)
                                (<= (length names)
                                    +agent-shell-egress--max-names))
                           (funcall (+agent-shell-egress--list name) names)
                         :bad))
                     "names")
             :first-seen (+agent-shell-egress--field value 'first_seen time
                                                     "time")
             :last-seen (+agent-shell-egress--field value 'last_seen time
                                                    "time")
             :count (+agent-shell-egress--field
                     value 'count #'+agent-shell-egress--natnum "count")
             :source (+agent-shell-egress--field
                      value 'source
                      (+agent-shell-egress--member '(tshark lsof lsof-cwd))
                      "source"))))
         (destinations (alist-get 'destinations object)))
    (unless (and (listp destinations)
                 (<= (length destinations)
                     +agent-shell-egress--max-destinations))
      (+agent-shell-egress--invalid "too many destinations"))
    (+agent-shell-egress--make-record
     :identity
     (list :panel (+agent-shell-egress--field object 'panel id "panel")
           :arena (+agent-shell-egress--field object 'arena id "arena" t)
           :role (+agent-shell-egress--field object 'role id "role")
           :kind (+agent-shell-egress--field
                  object 'kind (+agent-shell-egress--member '(candidate judge))
                  "kind"))
     :backend (+agent-shell-egress--field
               object 'backend
               (+agent-shell-egress--member '(linux-tshark macos-lsof))
               "backend" t)
     :state (+agent-shell-egress--field
             object 'state
             (+agent-shell-egress--member +agent-shell-egress--states) "state")
     :reason (+agent-shell-egress--field object 'reason reason "reason" t)
     :started-at (+agent-shell-egress--field object 'started_at time "time" t)
     :ended-at (+agent-shell-egress--field object 'ended_at time "time" t)
     :observers (+agent-shell-egress--field
                 object 'observers (+agent-shell-egress--list observer)
                 "observers")
     :limits (+agent-shell-egress--field
              object 'limits
              (+agent-shell-egress--list
               (+agent-shell-egress--member +agent-shell-egress--limits))
              "limits")
     :destinations (+agent-shell-egress--field
                    object 'destinations
                    (+agent-shell-egress--list destination) "destinations")
     :written t)))

(defun +agent-shell-egress-read (file identity)
  "Return the record in FILE if it is valid and describes IDENTITY.
IDENTITY's :panel, :role and :kind must match; its :arena may differ,
since a host's arena has an id of its own.  Signal
`+agent-shell-egress-invalid' with a fixed phrase otherwise; the
message never quotes the file."
  (when (or (file-symlink-p file) (not (file-regular-p file)))
    (+agent-shell-egress--invalid "not a regular file"))
  (when (> (file-attribute-size (file-attributes file))
           +agent-shell-egress--max-record)
    (+agent-shell-egress--invalid "too large"))
  (let* ((object (condition-case nil
                     (with-temp-buffer
                       (let ((coding-system-for-read 'utf-8-unix))
                         (insert-file-contents file))
                       (json-parse-buffer :object-type 'alist
                                          :array-type 'list
                                          :null-object :null
                                          :false-object :false))
                   (error (+agent-shell-egress--invalid "not JSON"))))
         (record (+agent-shell-egress--parse object))
         (found (+agent-shell-egress-record-identity record)))
    (dolist (key '(:panel :role :kind))
      (unless (equal (plist-get found key) (plist-get identity key))
        (+agent-shell-egress--invalid "another member's record")))
    record))

(defun +agent-shell-egress-import (source identity file)
  "Save the record in SOURCE as IDENTITY's record at FILE and return it.
A missing or invalid SOURCE saves an unavailable record instead."
  (let ((record
         (condition-case err
             (if (file-exists-p source)
                 (+agent-shell-egress-read source identity)
               (+agent-shell-egress--make-record
                :identity identity :state 'unavailable
                :reason 'artifact-missing))
           (+agent-shell-egress-invalid
            (ignore err)
            (+agent-shell-egress--make-record
             :identity identity :state 'unavailable
             :reason 'artifact-invalid)))))
    (+agent-shell-egress--write record file)
    record))

;;;; Sessions

(cl-defstruct (+agent-shell-egress--session
               (:constructor +agent-shell-egress--make-session)
               (:copier nil))
  "One member's live observation.  RECORD is the only record it changes.
FILE is where the panel keeps it.  DIRECTORY is a Linux observer's
private folder.  ROOTS holds the sandbox folders whose processes are
uncertain members on macOS.  PID-FUNCTION returns the adapter's pid.
KNOWN and UNCERTAIN map pids to their start time.  TABLE maps a key to
each destination and TUPLES the macOS sockets counted."
  record file directory roots pid-function
  (known (make-hash-table)) (uncertain (make-hash-table))
  (table (make-hash-table :test #'equal)) (tuples (make-hash-table :test #'equal))
  timer busy (ticks 0) (samples 0) closing closed callbacks)

(defun +agent-shell-egress--limit (record code)
  "Add limit CODE to RECORD."
  (unless (memq code (+agent-shell-egress-record-limits record))
    (setf (+agent-shell-egress-record-limits record)
          (append (+agent-shell-egress-record-limits record) (list code)))))

(defun +agent-shell-egress--note (session ip port protocol seconds source
                                          &optional name name-source count)
  "Record that SESSION's member reached IP and PORT over PROTOCOL.
SECONDS is when, SOURCE the method that saw it.  NAME from NAME-SOURCE
joins its names and COUNT, 1 by default, its count."
  (let* ((table (+agent-shell-egress--session-table session))
         (record (+agent-shell-egress--session-record session))
         (key (format "%s %d %s" ip port protocol))
         (time (+agent-shell-egress--time seconds))
         (destination (gethash key table)))
    (when (and (null destination)
               (< (hash-table-count table)
                  +agent-shell-egress--max-destinations))
      (setq destination (puthash key (+agent-shell-egress--make-destination
                                      :ip ip :port port :protocol protocol
                                      :first-seen time :source source)
                                 table)))
    (if (null destination)
        (+agent-shell-egress--limit record 'capacity-exceeded)
      (setf (+agent-shell-egress-destination-last-seen destination) time)
      (cl-incf (+agent-shell-egress-destination-count destination)
               (or count 1))
      (when (and (eq source 'lsof)
                 (eq (+agent-shell-egress-destination-source destination)
                     'lsof-cwd))
        (setf (+agent-shell-egress-destination-source destination) 'lsof))
      (when-let* ((name (and name (cons name name-source)))
                  ((not (member name (+agent-shell-egress-destination-names
                                      destination)))))
        (if (>= (length (+agent-shell-egress-destination-names destination))
                +agent-shell-egress--max-names)
            (+agent-shell-egress--limit record 'capacity-exceeded)
          (setf (+agent-shell-egress-destination-names destination)
                (append (+agent-shell-egress-destination-names destination)
                        (list name))))))))

(defun +agent-shell-egress--destinations (session)
  "Return SESSION's destinations, ordered by address, port and protocol."
  (sort (hash-table-values (+agent-shell-egress--session-table session))
        :key (lambda (destination)
               (format "%s %05d %s"
                       (+agent-shell-egress-destination-ip destination)
                       (+agent-shell-egress-destination-port destination)
                       (+agent-shell-egress-destination-protocol destination)))))

(defun +agent-shell-egress-open (sandbox file identity)
  "Start observing SANDBOX's member, saving what is seen to FILE.
IDENTITY is a plist of :panel, :arena, :role and :kind.  Return the
session and keep it in SANDBOX's egress slot for its launch to use.
With SANDBOX nil the member never started: save a not-started record
and return a closed session.  A record already in FILE is kept instead,
since an earlier runner made it; one still observing is marked
interrupted.  A failure to observe is recorded, never signalled."
  (let* ((backend (and sandbox (if (eq +agent-shell-sandbox-backend 'bwrap)
                                   'linux-tshark
                                 'macos-lsof)))
         (record (+agent-shell-egress--make-record
                  :identity identity :backend backend
                  :state (if sandbox 'observing 'not-started)
                  :limits (copy-sequence
                           (alist-get backend
                                      +agent-shell-egress--backend-limits))))
         (session (+agent-shell-egress--make-session
                   :record record :file file :closed (null sandbox))))
    (if (null sandbox)
        (+agent-shell-egress--recover session)
      (setf (+agent-shell-egress-record-started-at record)
            (+agent-shell-egress--time (float-time))
            (+agent-shell-egress-record-observers record)
            (list (+agent-shell-egress--make-observer
                   :method (if (eq backend 'linux-tshark) 'tshark 'lsof))))
      (setf (+agent-shell-sandbox-egress sandbox) session)
      (condition-case nil
          (if (eq backend 'linux-tshark)
              (let ((directory (concat file ".d")))
                (when (file-exists-p directory)
                  (+agent-shell-egress--reap
                   (expand-file-name "lease" directory))
                  (delete-directory directory t))
                (make-directory directory t)
                (set-file-modes directory #o700)
                (setf (+agent-shell-egress--session-directory session)
                      directory))
            (if (not (and (file-executable-p +agent-shell-egress--lsof)
                          (file-executable-p +agent-shell-egress--ps)))
                (+agent-shell-egress--fail session 'helper-missing)
              (setf (+agent-shell-egress--session-roots session)
                    (list (+agent-shell-sandbox-root sandbox)
                          (+agent-shell-sandbox-tmp sandbox))
                    (+agent-shell-egress--session-timer session)
                    (run-at-time 0 +agent-shell-egress-interval
                                 #'+agent-shell-egress--sample session))))
        (error (+agent-shell-egress--fail session 'observer-not-started))))
    (+agent-shell-egress--write (+agent-shell-egress--session-record session)
                                file)
    session))

(defun +agent-shell-egress--recover (session)
  "Take over the record an earlier runner left in SESSION's file."
  (let* ((file (+agent-shell-egress--session-file session))
         (directory (concat file ".d"))
         (identity (+agent-shell-egress-record-identity
                    (+agent-shell-egress--session-record session))))
    (when-let* ((record (ignore-errors
                          (+agent-shell-egress-read file identity))))
      (when (eq (+agent-shell-egress-record-state record) 'observing)
        (dolist (observer (+agent-shell-egress-record-observers record))
          (setf (+agent-shell-egress-observer-state observer) 'interrupted))
        (setf (+agent-shell-egress-record-state record) 'partial
              (+agent-shell-egress-record-reason record) 'observer-interrupted))
      (setf (+agent-shell-egress--session-record session) record))
    (when (file-directory-p directory)
      (ignore-errors
        (+agent-shell-egress--reap (expand-file-name "lease" directory))
        (delete-directory directory t)))))

(defun +agent-shell-egress--fail (session reason)
  "Mark SESSION's observer unavailable for REASON."
  (let ((observer (car (+agent-shell-egress-record-observers
                        (+agent-shell-egress--session-record session)))))
    (setf (+agent-shell-egress-observer-state observer) 'unavailable
          (+agent-shell-egress-observer-reason observer) reason)))

(defun +agent-shell-egress-attach (session pid-function)
  "Let SESSION find its member's adapter by calling PID-FUNCTION.
PID-FUNCTION returns the adapter's pid, or nil before it runs or once it
has ended.  Only macOS sampling uses it."
  (when session
    (setf (+agent-shell-egress--session-pid-function session) pid-function)))

(defun +agent-shell-egress-launch-environment (session)
  "Return the entries SESSION adds to a Linux launcher's own environment.
They name its observer's private folder and never reach the sandbox."
  (when-let* ((directory (and session
                              (not (+agent-shell-egress--session-closing
                                    session))
                              (+agent-shell-egress--session-directory
                               session))))
    (list (concat "PA_SANDBOX_EGRESS=" directory))))

(defun +agent-shell-egress-close (session callback)
  "Finish SESSION, save its final record and call CALLBACK with it.
Call it once per call, from a timer, even when saving fails.  A close
while one is under way waits for it.  The member's processes should be
stopped first, since nothing is observed after this."
  (cond
   ((+agent-shell-egress--session-closed session)
    (run-at-time 0 nil callback (+agent-shell-egress--session-record session)))
   (t
    (push callback (+agent-shell-egress--session-callbacks session))
    (unless (+agent-shell-egress--session-closing session)
      (setf (+agent-shell-egress--session-closing session) t)
      (when-let* ((timer (+agent-shell-egress--session-timer session)))
        (cancel-timer timer))
      (let ((deadline (+ (float-time) +agent-shell-egress--close-wait)))
        (cl-labels ((settle ()
                      (if (and (+agent-shell-egress--session-busy session)
                               (< (float-time) deadline))
                          (run-at-time 0.05 nil #'settle)
                        (+agent-shell-egress--finish session))))
          (run-at-time 0 nil #'settle)))))))

(defun +agent-shell-egress--finish (session)
  "Settle SESSION's record, save it and call its close callbacks."
  (let ((record (+agent-shell-egress--session-record session)))
    (condition-case nil
        (if (+agent-shell-egress--session-directory session)
            (+agent-shell-egress--finish-linux session)
          (+agent-shell-egress--finish-macos session))
      (error (+agent-shell-egress--fail session 'observer-exited)
             (setf (+agent-shell-egress-record-state record) 'unavailable
                   (+agent-shell-egress-record-reason record)
                   'observer-exited)))
    (setf (+agent-shell-egress-record-ended-at record)
          (+agent-shell-egress--time (float-time))
          (+agent-shell-egress-record-destinations record)
          (+agent-shell-egress--destinations session)
          (+agent-shell-egress--session-closed session) t)
    (+agent-shell-egress--write record
                                (+agent-shell-egress--session-file session))
    (let ((callbacks (reverse (+agent-shell-egress--session-callbacks
                               session))))
      (setf (+agent-shell-egress--session-callbacks session) nil)
      (dolist (callback callbacks)
        (funcall callback record)))))

(defun +agent-shell-egress--settle (session reason)
  "Set SESSION's final state from its observer, REASON why it is partial."
  (let* ((record (+agent-shell-egress--session-record session))
         (observer (car (+agent-shell-egress-record-observers record))))
    (if (eq (+agent-shell-egress-observer-state observer) 'unavailable)
        (setf (+agent-shell-egress-record-state record) 'unavailable
              (+agent-shell-egress-record-reason record)
              (+agent-shell-egress-observer-reason observer))
      (when (eq (+agent-shell-egress-observer-state observer) 'observing)
        (setf (+agent-shell-egress-observer-state observer) 'stopped))
      (setf (+agent-shell-egress-record-state record)
            (if (or reason
                    (+agent-shell-egress-observer-reason observer)
                    (memq 'capacity-exceeded
                          (+agent-shell-egress-record-limits record))
                    (memq 'unparsed-events
                          (+agent-shell-egress-record-limits record))
                    (when-let* ((dropped (+agent-shell-egress-observer-dropped
                                          observer)))
                      (> dropped 0)))
                'partial
              'finished)
            (+agent-shell-egress-record-reason record)
            (or reason (+agent-shell-egress-observer-reason observer))))))

;;;; Linux: tshark in the member's namespaces

(defconst +agent-shell-egress--fields
  '(time ipv4 ipv6 tcp-port udp-port syn ack response query a aaaa sni)
  "The columns of the launcher's tshark field output, in order.")

(defun +agent-shell-egress--true-p (text)
  "Return whether tshark field TEXT is a set flag."
  (member text '("1" "True")))

(defun +agent-shell-egress--fold-line (session line answers)
  "Fold one tshark field LINE into SESSION, using and updating ANSWERS.
ANSWERS maps an address to the name a DNS answer gave it.  Return nil
when LINE is malformed."
  (let ((columns (split-string line "\t")))
    (when (= (length columns) (length +agent-shell-egress--fields))
      (let* ((field (lambda (name)
                      (let ((text (nth (seq-position
                                        +agent-shell-egress--fields name)
                                       columns)))
                        (unless (string-empty-p text) text))))
             (seconds (let ((text (funcall field 'time)))
                        (and text (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'"
                                                  text)
                             (string-to-number text))))
             (ip (+agent-shell-egress--ip (or (funcall field 'ipv4)
                                              (funcall field 'ipv6))))
             (tcp (+agent-shell-egress--port (funcall field 'tcp-port)))
             (udp (+agent-shell-egress--port (funcall field 'udp-port)))
             (port (or tcp udp))
             (split (lambda (name)
                      (when-let* ((text (funcall field name)))
                        (split-string text ","))))
             (query (when-let* ((text (funcall field 'query)))
                      (+agent-shell-egress--name
                       (car (split-string text ",")))))
             (sni (when-let* ((text (funcall field 'sni)))
                    (+agent-shell-egress--name text))))
        (when (and seconds ip port)
          (cond
           ((+agent-shell-egress--true-p (funcall field 'response))
            (when query
              (dolist (answer (append (funcall split 'a)
                                      (funcall split 'aaaa)))
                (when-let* ((address (+agent-shell-egress--ip answer)))
                  (puthash address query answers)))))
           (query
            (+agent-shell-egress--note session ip port 'dns seconds 'tshark
                                       query 'dns))
           (t
            (let* ((protocol (if tcp 'tcp 'udp))
                   (start (and tcp (+agent-shell-egress--true-p
                                    (funcall field 'syn))
                               (not (+agent-shell-egress--true-p
                                     (funcall field 'ack)))))
                   (answer (gethash ip answers)))
              (when (or start sni (eq protocol 'udp))
                (+agent-shell-egress--note
                 session ip port protocol seconds 'tshark
                 answer 'dns (if (or start (eq protocol 'udp)) 1 0))
                (when sni
                  (+agent-shell-egress--note session ip port protocol seconds
                                             'tshark sni 'sni 0))))))
          t)))))

(defun +agent-shell-egress--fold-events (session file)
  "Fold the tshark field output in FILE into SESSION's record."
  (let ((answers (make-hash-table :test #'equal))
        (record (+agent-shell-egress--session-record session))
        (size (file-attribute-size (file-attributes file))))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents file nil 0 +agent-shell-egress--max-events))
      (when (> size +agent-shell-egress--max-events)
        (+agent-shell-egress--limit record 'capacity-exceeded))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (point) (line-end-position))))
          (unless (or (string-empty-p line)
                      (+agent-shell-egress--fold-line session line answers))
            (+agent-shell-egress--limit record 'unparsed-events)))
        (forward-line 1)))))

(defun +agent-shell-egress--linux-status (file)
  "Return the launcher's observer status in FILE as a plist.
It has :ready, :reason, a code, :exit, the observer's exit status, :early
when it exited before the member did, and :dropped."
  (let (status)
    (when (file-regular-p file)
      (dolist (line (with-temp-buffer
                      (insert-file-contents file nil 0 4096)
                      (split-string (buffer-string) "\n" t)))
        (pcase line
          ("ready" (setq status (plist-put status :ready t)))
          ((rx bos "unavailable " (let code (+ (any "a-z-"))) eos)
           (setq status (plist-put status :reason
                                   (or (car (memq (intern code)
                                                  +agent-shell-egress--reasons))
                                       'observer-exited))))
          ((rx bos "exit " (let code (+ digit)) (let early (? " early")) eos)
           (setq status (plist-put (plist-put status :exit
                                              (string-to-number code))
                                   :early (not (string-empty-p early)))))
          ((rx bos "dropped " (let count (+ digit)) eos)
           (setq status (plist-put status :dropped
                                   (string-to-number count)))))))
    (cons (file-regular-p file) status)))

(defun +agent-shell-egress--reap (lease)
  "Kill the observer named in LEASE if that very process still runs.
LEASE holds its pid and its start time from /proc, so a pid that has
since been reused is left alone."
  (when (file-regular-p lease)
    (pcase (split-string (with-temp-buffer
                           (insert-file-contents lease nil 0 256)
                           (buffer-string)))
      (`(,pid ,start)
       (let ((stat (format "/proc/%s/stat" pid)))
         (when (and (string-match-p "\\`[0-9]+\\'" pid) (file-readable-p stat))
           ;; The command name ends at the last ")" and may hold spaces.
           (let ((fields (with-temp-buffer
                           (insert-file-contents stat)
                           (goto-char (point-max))
                           (when (search-backward ")" nil t)
                             (split-string (buffer-substring (1+ (point))
                                                             (point-max)))))))
             ;; Field 22, the start time, is the 20th after the name.
             (when (equal (nth 19 fields) start)
               (ignore-errors
                 (signal-process (string-to-number pid) 'SIGKILL))))))))))

(defun +agent-shell-egress--finish-linux (session)
  "Read SESSION's observer folder into its record, then delete it."
  (let* ((directory (+agent-shell-egress--session-directory session))
         (record (+agent-shell-egress--session-record session))
         (observer (car (+agent-shell-egress-record-observers record)))
         (events (expand-file-name "events" directory)))
    (unwind-protect
        (pcase-let ((`(,found . ,status)
                     (+agent-shell-egress--linux-status
                      (expand-file-name "status" directory))))
          (+agent-shell-egress--reap (expand-file-name "lease" directory))
          (when (file-regular-p events)
            (+agent-shell-egress--fold-events session events))
          (setf (+agent-shell-egress-observer-dropped observer)
                (plist-get status :dropped))
          (let ((ready (plist-get status :ready))
                (exit (plist-get status :exit)))
            (cond
             ((not found)
              (+agent-shell-egress--fail session 'observer-not-started))
             ((not (or ready (> (hash-table-count
                                 (+agent-shell-egress--session-table session))
                                0)))
              (+agent-shell-egress--fail
               session (or (plist-get status :reason) 'observer-exited)))
             (t
              (cond ((null exit)
                     (setf (+agent-shell-egress-observer-state observer)
                           'interrupted
                           (+agent-shell-egress-observer-reason observer)
                           'observer-interrupted))
                    ;; SIGXFSZ: the launcher's file size limit stopped it.
                    ((eql exit 153)
                     (+agent-shell-egress--limit record 'capacity-exceeded))
                    ((plist-get status :early)
                     (setf (+agent-shell-egress-observer-reason observer)
                           'observer-exited)))
              (unless (or ready (+agent-shell-egress-observer-reason observer))
                (setf (+agent-shell-egress-observer-reason observer)
                      (or (plist-get status :reason) 'readiness-timeout))))))
          (+agent-shell-egress--settle session nil))
      (ignore-errors (delete-directory directory t)))))

;;;; macOS: socket samples of the adapter's process tree

(defun +agent-shell-egress--run (program arguments callback)
  "Run PROGRAM with ARGUMENTS, then call CALLBACK.
CALLBACK gets PROGRAM's standard output, or nil when it could not run,
failed or printed too much.  It runs with
`+agent-shell-egress--environment' only, and its errors are discarded."
  (let* ((buffer (generate-new-buffer " *pa-egress*" t))
         (default-directory "/")
         (process-environment +agent-shell-egress--environment)
         (done (lambda (output)
                 (when (buffer-live-p buffer) (kill-buffer buffer))
                 (funcall callback output))))
    (condition-case nil
        (make-process
         :name "pa-egress" :buffer buffer :noquery t :connection-type 'pipe
         :command (append (list "/bin/sh" "-c" "exec \"$0\" \"$@\" 2>/dev/null"
                                program)
                          arguments)
         :filter (lambda (process text)
                   (when (buffer-live-p (process-buffer process))
                     (with-current-buffer (process-buffer process)
                       (goto-char (point-max))
                       (insert text)
                       (when (> (buffer-size) +agent-shell-egress--max-output)
                         (delete-process process)))))
         :sentinel (lambda (process _event)
                     (unless (process-live-p process)
                       (funcall done
                                (and (eq (process-status process) 'exit)
                                     (memql (process-exit-status process) '(0 1))
                                     (buffer-live-p buffer)
                                     (<= (buffer-size buffer)
                                         +agent-shell-egress--max-output)
                                     (with-current-buffer buffer
                                       (buffer-string)))))))
      (error (funcall done nil)))))

(defun +agent-shell-egress--processes (output)
  "Return ps OUTPUT as a list of (PID PPID START)."
  (seq-keep (lambda (line)
              (when (string-match "\\` *\\([0-9]+\\) +\\([0-9]+\\) +\\(.*[^ ]\\) *\\'"
                                  line)
                (list (string-to-number (match-string 1 line))
                      (string-to-number (match-string 2 line))
                      (match-string 3 line))))
            (split-string output "\n" t)))

(defun +agent-shell-egress--track (session processes)
  "Update SESSION's known processes from PROCESSES, a ps snapshot.
Known processes are the adapter and its descendants, kept while their
start time holds even once reparented."
  (let ((known (+agent-shell-egress--session-known session))
        (starts (make-hash-table)))
    (pcase-dolist (`(,pid ,_ ,start) processes)
      (puthash pid start starts))
    (dolist (table (list known (+agent-shell-egress--session-uncertain session)))
      (maphash (lambda (pid start)
                 (unless (equal start (gethash pid starts))
                   (remhash pid table)))
               table))
    (when-let* ((function (+agent-shell-egress--session-pid-function session))
                (root (ignore-errors (funcall function)))
                (start (gethash root starts)))
      (puthash root start known))
    (let ((parents (mapcar (pcase-lambda (`(,pid ,ppid ,_)) (cons pid ppid))
                           processes)))
      (dolist (pid (hash-table-keys known))
        (dolist (child (+agent-shell-sandbox--descendants pid parents))
          (when (< (hash-table-count known) 4096)
            (puthash child (gethash child starts) known)))))
    (dolist (pid (hash-table-keys known))
      (remhash pid (+agent-shell-egress--session-uncertain session)))
    starts))

(defun +agent-shell-egress--working (session output starts)
  "Add the processes lsof OUTPUT shows working in SESSION's sandbox.
STARTS maps pids to their start time.  They are uncertain members."
  (let ((prefixes (mapcar #'file-name-as-directory
                          (+agent-shell-egress--session-roots session)))
        (known (+agent-shell-egress--session-known session))
        (uncertain (+agent-shell-egress--session-uncertain session))
        pid)
    (dolist (line (split-string output "\n" t))
      (pcase (aref line 0)
        (?p (setq pid (string-to-number (substring line 1))))
        (?n (let ((path (file-name-as-directory (substring line 1))))
              (when (and pid (not (eql pid (emacs-pid)))
                         (not (gethash pid known))
                         (gethash pid starts)
                         (< (hash-table-count uncertain) 4096)
                         (seq-some (lambda (prefix)
                                     (string-prefix-p prefix path))
                                   prefixes))
                (puthash pid (gethash pid starts) uncertain))))))))

(defun +agent-shell-egress--endpoint (text)
  "Return lsof endpoint TEXT as (IP . PORT), or nil."
  (when (string-match (rx bos (or (seq "[" (group-n 1 (+ (not "]"))) "]")
                                  (group-n 1 (+ (not (any "[]:")))))
                          ":" (group-n 2 (+ digit)) eos)
                      text)
    (let* ((port (+agent-shell-egress--port (match-string 2 text)))
           (ip (+agent-shell-egress--ip (match-string 1 text))))
      (and ip port (cons ip port)))))

(defun +agent-shell-egress--sockets (session output seconds)
  "Fold the sockets lsof OUTPUT lists into SESSION, sampled at SECONDS."
  (let ((known (+agent-shell-egress--session-known session))
        (tuples (+agent-shell-egress--session-tuples session))
        pid protocol)
    (dolist (line (split-string output "\n" t))
      (pcase (aref line 0)
        (?p (setq pid (string-to-number (substring line 1)) protocol nil))
        (?f (setq protocol nil))
        (?P (setq protocol (pcase (substring line 1)
                             ("TCP" 'tcp) ("UDP" 'udp))))
        (?n
         (when-let* ((protocol protocol)
                     (ends (split-string (substring line 1) "->"))
                     ((= 2 (length ends)))
                     (peer (+agent-shell-egress--endpoint (cadr ends))))
           (let* ((key (format "%s %s" protocol (substring line 1)))
                  (new (not (gethash key tuples))))
             (when (and new (< (hash-table-count tuples) 65536))
               (puthash key t tuples))
             (+agent-shell-egress--note
              session (car peer) (cdr peer) protocol seconds
              (if (gethash pid known) 'lsof 'lsof-cwd) nil nil
              (if new 1 0)))))))))

(defun +agent-shell-egress--sample (session)
  "Take one socket sample of SESSION's member unless one is under way."
  (unless (or (+agent-shell-egress--session-busy session)
              (+agent-shell-egress--session-closing session))
    (setf (+agent-shell-egress--session-busy session) t)
    (let ((cwd (zerop (% (+agent-shell-egress--session-ticks session)
                         +agent-shell-egress--cwd-every)))
          (seconds (float-time)))
      (cl-incf (+agent-shell-egress--session-ticks session))
      (cl-labels
          ((done () (setf (+agent-shell-egress--session-busy session) nil))
           (sockets ()
             (let ((pids (append (hash-table-keys
                                  (+agent-shell-egress--session-known session))
                                 (hash-table-keys
                                  (+agent-shell-egress--session-uncertain
                                   session)))))
               (if (null pids) (done)
                 (+agent-shell-egress--run
                  +agent-shell-egress--lsof
                  (list "-w" "-n" "-P" "-a" "-p"
                        (mapconcat #'number-to-string pids ",")
                        "-i" "-F" "pfPn")
                  (lambda (output)
                    (when (and output (not (+agent-shell-egress--session-closed
                                            session)))
                      (cl-incf (+agent-shell-egress--session-samples session))
                      (+agent-shell-egress--sockets session output seconds))
                    (done)))))))
        (+agent-shell-egress--run
         +agent-shell-egress--ps '("-axo" "pid=,ppid=,lstart=")
         (lambda (output)
           (if (null output) (done)
             (let ((starts (+agent-shell-egress--track
                            session (+agent-shell-egress--processes output))))
               (if (not cwd) (sockets)
                 (+agent-shell-egress--run
                  +agent-shell-egress--lsof
                  '("-w" "-n" "-P" "-d" "cwd" "-F" "pn")
                  (lambda (output)
                    (when output
                      (+agent-shell-egress--working session output starts))
                    (sockets))))))))))))

(defun +agent-shell-egress--finish-macos (session)
  "Settle SESSION's sampled record."
  (let ((record (+agent-shell-egress--session-record session)))
    (when (seq-some (lambda (destination)
                      (eq (+agent-shell-egress-destination-source destination)
                          'lsof-cwd))
                    (hash-table-values
                     (+agent-shell-egress--session-table session)))
      (+agent-shell-egress--limit record 'uncertain-attribution))
    (when (and (zerop (+agent-shell-egress--session-samples session))
               (not (eq (+agent-shell-egress-observer-state
                         (car (+agent-shell-egress-record-observers record)))
                        'unavailable)))
      (+agent-shell-egress--fail session 'no-samples))
    (+agent-shell-egress--settle session nil)
    (unless (eq (+agent-shell-egress-record-state record) 'unavailable)
      (setf (+agent-shell-egress-record-state record) 'partial))))

(provide '+agent-shell-egress)
;;; +agent-shell-egress.el ends here
