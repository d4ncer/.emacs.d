;;; agent-shell-egress-test.el --- Tests for arena egress records -*- lexical-binding: t; -*-

;;; Commentary:
;; Record, Linux field output and macOS sampling tests.  The Linux tests
;; feed the folder the launcher would fill; the sampling tests run ps and
;; lsof and skip off macOS.  Every address and name is a test value.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-egress)

(defconst +agent-shell-egress-test--canary "CANARY-7f3e9b"
  "Text that must never reach a record.")

(defmacro +agent-shell-egress-test--with (&rest body)
  "Run BODY with `directory', a fresh folder, and `sandbox' rooted in it."
  (declare (indent 0))
  `(let* ((directory (file-name-as-directory
                      (file-truename (make-temp-file "pa-egress-" t))))
          (sandbox (+agent-shell-sandbox--make
                    :root (directory-file-name directory)
                    :tmp (expand-file-name "tmp" directory)))
          (identity (list :panel "p1" :arena "p1-a1" :role "opus"
                          :kind 'candidate)))
     (unwind-protect (progn ,@body)
       (delete-directory directory t))))

(defun +agent-shell-egress-test--close (session)
  "Close SESSION and return its record once the callback has it."
  (let ((record nil) (calls 0) (deadline (+ (float-time) 10)))
    (+agent-shell-egress-close session (lambda (value)
                                         (cl-incf calls)
                                         (setq record value)))
    (while (and (not record) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (accept-process-output nil 0.05)
    (should (= calls 1))
    record))

(defun +agent-shell-egress-test--text (file)
  "Return FILE's contents."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun +agent-shell-egress-test--destinations (record)
  "Return RECORD's destinations as (IP PORT PROTOCOL NAMES COUNT SOURCE)."
  (mapcar (lambda (destination)
            (list (+agent-shell-egress-destination-ip destination)
                  (+agent-shell-egress-destination-port destination)
                  (+agent-shell-egress-destination-protocol destination)
                  (+agent-shell-egress-destination-names destination)
                  (+agent-shell-egress-destination-count destination)
                  (+agent-shell-egress-destination-source destination)))
          (+agent-shell-egress-record-destinations record)))

(ert-deftest +agent-shell-egress-test-not-started ()
  "Save a not-started record for a member that never launched."
  (+agent-shell-egress-test--with
    (let* ((file (expand-file-name "p1/judge.egress.json" directory))
           (identity (list :panel "p1" :arena nil :role "judge" :kind 'judge))
           (record (+agent-shell-egress-test--close
                    (+agent-shell-egress-open nil file identity))))
      (should (eq 'not-started (+agent-shell-egress-record-state record)))
      (should (+agent-shell-egress-record-written record))
      (should (= #o600 (file-modes file)))
      (should (= #o700 (file-modes (file-name-directory file))))
      (let ((read (+agent-shell-egress-read file identity)))
        (should (eq 'not-started (+agent-shell-egress-record-state read)))
        (should (equal identity (+agent-shell-egress-record-identity read)))
        (should-not (+agent-shell-egress-record-destinations read))))))

(ert-deftest +agent-shell-egress-test-write-fails ()
  "Keep a record that cannot be saved in memory, marked unsaved."
  (+agent-shell-egress-test--with
    (let* ((locked (expand-file-name "locked" directory))
           (file (expand-file-name "p1/opus.egress.json" locked)))
      (make-directory locked)
      (set-file-modes locked #o500)
      (unwind-protect
          (let ((record (+agent-shell-egress-test--close
                         (+agent-shell-egress-open nil file identity))))
            (should (eq 'not-started (+agent-shell-egress-record-state record)))
            (should-not (+agent-shell-egress-record-written record))
            (should-not (file-exists-p file)))
        (set-file-modes locked #o700)))))

(ert-deftest +agent-shell-egress-test-read-refuses ()
  "Refuse a record of another member, an unknown field and a link,
without quoting what the file held."
  (+agent-shell-egress-test--with
    (let* ((file (expand-file-name "opus.egress.json" directory))
           (link (expand-file-name "link.egress.json" directory)))
      (+agent-shell-egress-test--close
       (+agent-shell-egress-open nil file identity))
      (should-error (+agent-shell-egress-read
                     file (plist-put (copy-sequence identity) :role "codex"))
                    :type '+agent-shell-egress-invalid)
      (make-symbolic-link file link)
      (should-error (+agent-shell-egress-read link identity)
                    :type '+agent-shell-egress-invalid)
      (let ((text (+agent-shell-egress-test--text file)))
        (with-temp-file file
          (insert (replace-regexp-in-string
                   "\"limits\"" (format "\"%s\":1,\"limits\""
                                        +agent-shell-egress-test--canary)
                   text))))
      (let ((err (should-error (+agent-shell-egress-read file identity)
                               :type '+agent-shell-egress-invalid)))
        (should-not (string-search +agent-shell-egress-test--canary
                                   (error-message-string err)))))))

(ert-deftest +agent-shell-egress-test-import ()
  "Copy a valid record, and save an unavailable one for a bad or
missing source."
  (+agent-shell-egress-test--with
    (let ((source (expand-file-name "host/opus.egress.json" directory))
          (target (expand-file-name "laptop/opus.egress.json" directory)))
      (should (eq 'artifact-missing
                  (+agent-shell-egress-record-reason
                   (+agent-shell-egress-import source identity target))))
      (should (eq 'unavailable
                  (+agent-shell-egress-record-state
                   (+agent-shell-egress-read target identity))))
      (make-directory (file-name-directory source) t)
      (with-temp-file source (insert "{\"version\":1}"))
      (should (eq 'artifact-invalid
                  (+agent-shell-egress-record-reason
                   (+agent-shell-egress-import source identity target))))
      (+agent-shell-egress-test--close
       (+agent-shell-egress-open
        nil source (plist-put (copy-sequence identity) :arena "host-arena")))
      (let ((record (+agent-shell-egress-import source identity target)))
        (should (eq 'not-started (+agent-shell-egress-record-state record)))
        (should (equal "host-arena"
                       (plist-get (+agent-shell-egress-record-identity
                                   (+agent-shell-egress-read target identity))
                                  :arena)))))))

;;;; Linux

(ert-deftest +agent-shell-egress-test-recovers-interrupted ()
  "Keep the record a runner that died while observing left, as
interrupted, and remove its observer's folder."
  (+agent-shell-egress-test--with
    (let ((+agent-shell-sandbox-backend 'bwrap)
          (file (expand-file-name "p1/opus.egress.json" directory)))
      (+agent-shell-egress-open sandbox file identity)
      (should (file-directory-p (concat file ".d")))
      (let ((record (+agent-shell-egress-test--close
                     (+agent-shell-egress-open nil file identity))))
        (should (equal '(partial observer-interrupted linux-tshark)
                       (list (+agent-shell-egress-record-state record)
                             (+agent-shell-egress-record-reason record)
                             (+agent-shell-egress-record-backend record))))
        (should (eq 'partial (+agent-shell-egress-record-state
                              (+agent-shell-egress-read file identity))))
        (should-not (file-exists-p (concat file ".d")))))))

(defun +agent-shell-egress-test--linux (sandbox file identity status events)
  "Close a Linux session for SANDBOX at FILE whose launcher left STATUS.
EVENTS are lines of tshark field output; nil STATUS leaves no status.
Return the record."
  (let* ((+agent-shell-sandbox-backend 'bwrap)
         (session (+agent-shell-egress-open sandbox file identity))
         (folder (concat file ".d")))
    (should (equal (list (concat "PA_SANDBOX_EGRESS=" folder))
                   (+agent-shell-egress-launch-environment
                    (+agent-shell-sandbox-egress sandbox))))
    (should (= #o700 (file-modes folder)))
    (when status
      (with-temp-file (expand-file-name "status" folder) (insert status)))
    (with-temp-file (expand-file-name "events" folder)
      (insert (mapconcat (lambda (line) (concat line "\n")) events "")))
    (prog1 (+agent-shell-egress-test--close session)
      (should-not (file-exists-p folder)))))

(defun +agent-shell-egress-test--line (&rest fields)
  "Return a tshark field line with FIELDS, a plist of column values."
  (mapconcat (lambda (column) (or (plist-get fields column) ""))
             '(:time :ipv4 :ipv6 :tcp :udp :syn :ack :response :query :a
                     :aaaa :sni)
             "\t"))

(ert-deftest +agent-shell-egress-test-linux-fields ()
  "Turn tshark fields into destinations named by DNS answers and SNI.
A malformed line and one carrying a canary outside the fields read are
counted, never copied."
  (+agent-shell-egress-test--with
    (let* ((file (expand-file-name "p1/opus.egress.json" directory))
           (canary +agent-shell-egress-test--canary)
           (line #'+agent-shell-egress-test--line)
           (record
            (+agent-shell-egress-test--linux
             sandbox file identity "ready\nexit 0\ndropped 0\n"
             (list
              (funcall line :time "1.0" :ipv4 "10.0.2.3" :udp "53"
                       :query "build.example.org")
              (funcall line :time "1.1" :ipv4 "10.0.2.100" :udp "40000"
                       :response "True" :query "build.example.org"
                       :a "192.0.2.10,192.0.2.11")
              (funcall line :time "2.0" :ipv4 "192.0.2.10" :tcp "443"
                       :syn "True" :ack "False")
              (funcall line :time "2.1" :ipv4 "192.0.2.10" :tcp "443"
                       :syn "1" :ack "0")
              (funcall line :time "2.2" :ipv4 "192.0.2.10" :tcp "443"
                       :syn "False" :ack "True" :sni "Build.Example.Org")
              (funcall line :time "3.0" :ipv4 "198.51.100.7" :udp "443"
                       :sni "cache.example.net")
              (funcall line :time "3.1" :ipv4 "198.51.100.7" :tcp "80"
                       :syn "False" :ack "True")
              (concat (funcall line :time "4.0" :ipv4 "192.0.2.10" :tcp "443"
                               :syn "1" :ack "0")
                      "\tGET /" canary " HTTP/1.1")
              (funcall line :time "4.1" :ipv4 canary :tcp "443" :syn "1")
              "not a field line"))))
      (should (equal
               '(("10.0.2.3" 53 dns (("build.example.org" . dns)) 1 tshark)
                 ("192.0.2.10" 443 tcp (("build.example.org" . dns)
                                        ("build.example.org" . sni))
                  2 tshark)
                 ("198.51.100.7" 443 udp (("cache.example.net" . sni)) 1
                  tshark))
               (+agent-shell-egress-test--destinations record)))
      (should (eq 'partial (+agent-shell-egress-record-state record)))
      (should (memq 'unparsed-events (+agent-shell-egress-record-limits record)))
      (should (memq 'encrypted-names (+agent-shell-egress-record-limits record)))
      (should (eql 0 (+agent-shell-egress-observer-dropped
                      (car (+agent-shell-egress-record-observers record)))))
      (should-not (string-search canary (+agent-shell-egress-test--text file)))
      (should (equal (+agent-shell-egress-test--destinations record)
                     (+agent-shell-egress-test--destinations
                      (+agent-shell-egress-read file identity)))))))

(ert-deftest +agent-shell-egress-test-linux-states ()
  "Say how a Linux observer ended from the status its launcher wrote."
  (+agent-shell-egress-test--with
    (let ((syn (+agent-shell-egress-test--line
                :time "1.0" :ipv4 "192.0.2.10" :tcp "443" :syn "1" :ack "0")))
      (pcase-dolist (`(,status ,events ,state ,reason ,limit)
                     `((nil nil unavailable observer-not-started nil)
                       ("unavailable helper-missing\n" nil unavailable
                        helper-missing nil)
                       ("unavailable namespace-entry-failed\n" nil unavailable
                        namespace-entry-failed nil)
                       ("ready\nexit 0\n" (,syn) finished nil nil)
                       ("ready\n" (,syn) partial observer-interrupted nil)
                       ("ready\nexit 153\n" (,syn) partial nil
                        capacity-exceeded)
                       ("ready\nexit 1 early\n" (,syn) partial observer-exited
                        nil)
                       ("unavailable readiness-timeout\nexit 0\n" (,syn)
                        partial readiness-timeout nil)
                       ("ready\nexit 0\ndropped 3\n" (,syn) partial nil nil)))
        (let* ((file (expand-file-name "p1/opus.egress.json" directory))
               (record (+agent-shell-egress-test--linux
                        sandbox file identity status events)))
          (should (equal (list status state reason)
                         (list status
                               (+agent-shell-egress-record-state record)
                               (+agent-shell-egress-record-reason record))))
          (when limit
            (should (memq limit (+agent-shell-egress-record-limits
                                 record)))))))))

;;;; macOS

(defun +agent-shell-egress-test--wait (predicate &optional seconds)
  "Wait up to SECONDS, 10 by default, for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should (funcall predicate)))

(ert-deftest +agent-shell-egress-test-samples-process-tree ()
  "See a socket a descendant of the adapter holds, and none of the
canary it sends."
  (skip-unless (and (eq system-type 'darwin) (executable-find "python3")
                    (file-executable-p "/usr/sbin/lsof")))
  (+agent-shell-egress-test--with
    (let* ((+agent-shell-sandbox-backend 'sandbox-exec)
           (+agent-shell-egress-interval 0.2)
           (received "")
           (server (make-network-process
                    :name "pa-egress-test" :server t :host "127.0.0.1"
                    :service t :noquery t
                    :filter (lambda (_ text)
                              (setq received (concat received text)))))
           (port (process-contact server :service))
           (adapter
            (make-process
             :name "pa-egress-adapter" :noquery t :connection-type 'pipe
             :command
             (list "/bin/sh" "-c"
                   (format "python3 -c \"
import socket, time
connection = socket.create_connection(('127.0.0.1', %d))
connection.sendall(b'GET /%s HTTP/1.1\\r\\n\\r\\n')
time.sleep(30)\"; :" port +agent-shell-egress-test--canary))))
           (file (expand-file-name "p1/opus.egress.json" directory)))
      (unwind-protect
          (let ((session (+agent-shell-egress-open sandbox file identity)))
            (+agent-shell-egress-attach
             session (lambda () (process-id adapter)))
            (+agent-shell-egress-test--wait
             (lambda () (string-search +agent-shell-egress-test--canary
                                       received)))
            (+agent-shell-egress-test--wait
             (lambda ()
               (> (hash-table-count
                   (+agent-shell-egress--session-table session))
                  0)))
            (let ((record (+agent-shell-egress-test--close session)))
              (should (equal `(("127.0.0.1" ,port tcp nil 1 lsof))
                             (+agent-shell-egress-test--destinations record)))
              (should (eq 'partial (+agent-shell-egress-record-state record)))
              (should (eq 'macos-lsof (+agent-shell-egress-record-backend
                                       record)))
              (should (memq 'sampled-sockets
                            (+agent-shell-egress-record-limits record)))
              (should-not (string-search
                           +agent-shell-egress-test--canary
                           (+agent-shell-egress-test--text file)))))
        (+agent-shell-sandbox-kill-tree (process-id adapter))
        (delete-process adapter)
        (delete-process server)))))

(ert-deftest +agent-shell-egress-test-missing-lsof ()
  "Record a missing lsof as unavailable without starting a sampler."
  (+agent-shell-egress-test--with
    (let* ((+agent-shell-sandbox-backend 'sandbox-exec)
           (+agent-shell-egress--lsof (expand-file-name "no-lsof" directory))
           (session (+agent-shell-egress-open
                     sandbox (expand-file-name "opus.egress.json" directory)
                     identity)))
      (should-not (+agent-shell-egress--session-timer session))
      (let ((record (+agent-shell-egress-test--close session)))
        (should (equal '(unavailable helper-missing)
                       (list (+agent-shell-egress-record-state record)
                             (+agent-shell-egress-record-reason record))))))))

(ert-deftest +agent-shell-egress-test-observer-environment ()
  "Run observer commands with only PATH and LANG of this Emacs's
environment, which here holds a login."
  (skip-unless (file-executable-p "/usr/sbin/lsof"))
  (+agent-shell-egress-test--with
    (let* ((+agent-shell-sandbox-backend 'sandbox-exec)
           (+agent-shell-egress-interval 0.2)
           (dump (expand-file-name "env" directory))
           (+agent-shell-egress--ps (expand-file-name "ps" directory))
           (process-environment (cons (concat "PA_LOGIN="
                                              +agent-shell-egress-test--canary)
                                      process-environment)))
      (with-temp-file +agent-shell-egress--ps
        (insert (format "#!/bin/sh\nenv > %s\n" dump)))
      (set-file-modes +agent-shell-egress--ps #o700)
      (let ((session (+agent-shell-egress-open
                      sandbox (expand-file-name "opus.egress.json" directory)
                      identity)))
        (+agent-shell-egress-test--wait (lambda () (file-exists-p dump)))
        (+agent-shell-egress-test--close session))
      (let ((names (mapcar (lambda (line) (car (split-string line "=")))
                           (split-string (+agent-shell-egress-test--text dump)
                                         "\n" t))))
        (should (member "PATH" names))
        (should-not (seq-difference names '("PATH" "LANG" "PWD" "SHLVL" "_")))
        (should-not (string-search +agent-shell-egress-test--canary
                                   (+agent-shell-egress-test--text dump)))))))

(provide 'agent-shell-egress-test)
;;; agent-shell-egress-test.el ends here
