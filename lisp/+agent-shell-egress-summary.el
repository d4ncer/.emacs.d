;;; +agent-shell-egress-summary.el --- Review recorded destinations -*- lexical-binding: t; -*-

;;; Commentary:
;; Aggregate metadata records without loading the panel machinery.  The view
;; and both exports share one table, ordered by agents and then destination.

;;; Code:

(require '+agent-shell-egress)

(defvar +agent-shell-pstack-panel-directory
  (locate-user-emacs-file "var/pstack-panels/"))
(defvar +agent-shell-pstack-panel--panels)

(declare-function +agent-shell-pstack-panel--egress-identity
                  "+agent-shell-pstack-panel")
(declare-function +agent-shell-pstack-panel-members "+agent-shell-pstack-panel")
(declare-function +agent-shell-pstack-panel-member-role
                  "+agent-shell-pstack-panel")

(defconst +agent-shell-egress-summary--columns
  '((agents . "Agents") (destination . "Destination") (port . "Port")
    (protocol . "Protocol") (observed . "Observed")
    (repositories . "Repositories") (panels . "Panels") (members . "Members")
    (first_seen . "First seen") (last_seen . "Last seen") (records . "Records")
    (partial . "Partial") (unavailable . "Unavailable"))
  "Fields shared by the view and exports.")

(defvar-local +agent-shell-egress-summary--data nil)
(defvar-local +agent-shell-egress-summary--since nil)

(defun +agent-shell-egress-summary--date (date)
  "Return DATE (YYYY-MM-DD) as UTC midnight, or signal a user error."
  (condition-case nil
      (progn
        (unless (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
                                date)
          (error "Bad date"))
        (let ((time (encode-time 0 0 0 (string-to-number (substring date 8 10))
                                 (string-to-number (substring date 5 7))
                                 (string-to-number (substring date 0 4)) t)))
          (unless (equal date (format-time-string "%F" time t))
            (error "Bad date"))
          (+agent-shell-egress--time time)))
    (error (user-error "Use a valid date in YYYY-MM-DD form"))))

(defun +agent-shell-egress-summary--live-identity (identity)
  "Return available live metadata for record IDENTITY, without loading it."
  (when (and (boundp '+agent-shell-pstack-panel--panels)
             (hash-table-p +agent-shell-pstack-panel--panels)
             (fboundp '+agent-shell-pstack-panel--egress-identity))
    (when-let* ((panel (gethash (plist-get identity :panel)
                               +agent-shell-pstack-panel--panels))
                (member (seq-find
                         (lambda (member)
                           (equal (plist-get identity :role)
                                  (+agent-shell-pstack-panel-member-role member)))
                         (+agent-shell-pstack-panel-members panel))))
      (+agent-shell-pstack-panel--egress-identity panel member))))

(defun +agent-shell-egress-summary--names (destination)
  "Return DESTINATION's host evidence, excluding DNS questions."
  (unless (eq 'dns (+agent-shell-egress-destination-protocol destination))
    (+agent-shell-egress-destination-names destination)))

(defun +agent-shell-egress-summary--before-p (a b)
  "Return whether UTC timestamp A precedes B, at any saved precision."
  (time-less-p (date-to-time a) (date-to-time b)))

(defun +agent-shell-egress-summary--collect (directory &optional since)
  "Aggregate all records under DIRECTORY starting on or after UTC SINCE.
SINCE is an ISO timestamp, or nil.  Records without a start or end time
use their file modification time.  Invalid records are counted and skipped."
  (let ((rows (make-hash-table :test #'equal))
        (repositories (make-hash-table :test #'equal))
        (coverage (make-hash-table :test #'equal))
        (counts (make-hash-table :test #'eq))
        records first last (total 0) (invalid 0) (excluded 0))
    (dolist (file (when (file-directory-p directory)
                   (directory-files-recursively directory "\\.egress\\.json\\'")))
      (cl-incf total)
      (condition-case nil
          (let* ((record (+agent-shell-egress-read file))
                 (start (or (+agent-shell-egress-record-started-at record)
                            (+agent-shell-egress-record-ended-at record)
                            (+agent-shell-egress--time
                             (file-attribute-modification-time
                              (file-attributes file)))))
                 (end (or (+agent-shell-egress-record-ended-at record) start)))
            (if (and since (+agent-shell-egress-summary--before-p start since))
                (cl-incf excluded)
              (push record records)
              (setq first (if (or (null first)
                                  (+agent-shell-egress-summary--before-p start first))
                              start first)
                    last (if (or (null last)
                                 (+agent-shell-egress-summary--before-p last end))
                             end last))))
        ((+agent-shell-egress-invalid file-error) (cl-incf invalid))))
    ;; Mixed versions can leave a repository on only one member's record.
    (dolist (record records)
      (let ((identity (+agent-shell-egress-record-identity record)))
        (when-let* ((repository (plist-get identity :repository)))
          (cl-pushnew repository
                      (gethash (plist-get identity :panel) repositories)
                      :test #'equal))))
    (dolist (record records)
      (let* ((identity (+agent-shell-egress-record-identity record))
             (live (+agent-shell-egress-summary--live-identity identity))
             (panel (plist-get identity :panel))
             (member (list panel (plist-get identity :role)))
             (agent (or (plist-get identity :agent) (plist-get live :agent)
                        "unknown"))
             (peers (gethash panel repositories))
             (repository (or (plist-get identity :repository)
                             (plist-get live :repository)
                             (when (= (length peers) 1) (car peers)) "unknown"))
             (state (+agent-shell-egress-record-state record))
             (destinations (+agent-shell-egress-record-destinations record))
             (backend (symbol-name
                       (or (+agent-shell-egress-record-backend record) 'none)))
             (bucket (cond ((seq-some #'+agent-shell-egress-summary--names
                                     destinations) 'named)
                           (destinations 'addresses-only) (t 'empty)))
             (backend-counts (or (gethash backend coverage)
                                 (puthash backend (make-hash-table) coverage))))
        (cl-incf (gethash bucket backend-counts 0))
        (cl-incf (gethash state counts 0))
        (dolist (destination destinations)
          (let* ((names (+agent-shell-egress-summary--names destination))
                 (port (+agent-shell-egress-destination-port destination))
                 (protocol (+agent-shell-egress-destination-protocol destination))
                 (evidence
                  (or names
                      (list (cons (+agent-shell-egress-destination-ip destination)
                                  (if (eq protocol 'dns) 'dns
                                    (+agent-shell-egress-destination-source
                                     destination)))))))
            (dolist (name evidence)
              (let* ((key (list (car name) port protocol))
                     (row (or (gethash key rows)
                              (puthash key (list :destination (car name)
                                                 :port port :protocol protocol)
                                       rows)))
                     (start (+agent-shell-egress-destination-first-seen destination))
                     (end (+agent-shell-egress-destination-last-seen destination)))
                (dolist (pair `((:agents . ,agent) (:repositories . ,repository)
                                (:panels . ,panel) (:members . ,member)
                                (:observed . ,(symbol-name (cdr name)))))
                  (cl-pushnew (cdr pair) (plist-get row (car pair)) :test #'equal))
                (cl-pushnew record (plist-get row :records) :test #'eq)
                (when (eq state 'partial)
                  (cl-pushnew record (plist-get row :partial) :test #'eq))
                (when (eq state 'unavailable)
                  (cl-pushnew record (plist-get row :unavailable) :test #'eq))
                (when (or (null (plist-get row :first))
                          (+agent-shell-egress-summary--before-p
                           start (plist-get row :first)))
                  (setf (plist-get row :first) start))
                (when (or (null (plist-get row :last))
                          (+agent-shell-egress-summary--before-p
                           (plist-get row :last) end))
                  (setf (plist-get row :last) end))
                (puthash key row rows)))))))
    (let (table backends)
      (maphash
       (lambda (_ row)
         (push `((destination . ,(plist-get row :destination))
                 (port . ,(plist-get row :port))
                 (protocol . ,(symbol-name (plist-get row :protocol)))
                 (observed . ,(vconcat (sort (plist-get row :observed) #'string<)))
                 (agents . ,(vconcat (sort (plist-get row :agents) #'string<)))
                 (repositories . ,(vconcat (sort (plist-get row :repositories)
                                                #'string<)))
                 (panels . ,(length (plist-get row :panels)))
                 (members . ,(length (plist-get row :members)))
                 (first_seen . ,(plist-get row :first))
                 (last_seen . ,(plist-get row :last))
                 (records . ,(length (plist-get row :records)))
                 (partial . ,(length (plist-get row :partial)))
                 (unavailable . ,(length (plist-get row :unavailable)))) table))
       rows)
      (maphash
       (lambda (backend buckets)
         (push (format "%s: %d named, %d addresses only, %d empty"
                       backend (gethash 'named buckets 0)
                       (gethash 'addresses-only buckets 0)
                       (gethash 'empty buckets 0)) backends)) coverage)
      (list :rows (sort table
                        (lambda (a b)
                          (string< (+agent-shell-egress-summary--sort-key a)
                                   (+agent-shell-egress-summary--sort-key b))))
            :total total :records (length records) :invalid invalid
            :excluded excluded :counts counts :first first :last last
            :coverage (sort backends #'string<)))))

(defun +agent-shell-egress-summary--cell (value)
  "Return a table cell for VALUE."
  (cond ((vectorp value) (string-join (append value nil) "; "))
        ((null value) "unknown") (t (format "%s" value))))

(defun +agent-shell-egress-summary--sort-key (row)
  "Return ROW's sort key, grouping destinations by the agents that use them."
  (format "%s\t%s\t%s\t%05d"
          (+agent-shell-egress-summary--cell (alist-get 'agents row))
          (alist-get 'destination row) (alist-get 'protocol row)
          (alist-get 'port row)))

(defun +agent-shell-egress-summary--table (rows)
  "Return ROWS as an aligned text table with column headings."
  (let* ((cells (cons (mapcar #'cdr +agent-shell-egress-summary--columns)
                      (mapcar
                       (lambda (row)
                         (mapcar (lambda (column)
                                   (+agent-shell-egress-summary--cell
                                    (alist-get (car column) row)))
                                 +agent-shell-egress-summary--columns)) rows)))
         (widths (apply #'cl-mapcar
                        (lambda (&rest values)
                          (apply #'max (mapcar #'string-width values))) cells))
         (format-string (concat (mapconcat (lambda (width)
                                            (format "%%-%ds" width))
                                          widths "  ") "\n")))
    (mapconcat (lambda (row) (apply #'format format-string row)) cells "")))

(defun +agent-shell-egress-summary--refresh (&rest _)
  "Refresh the current summary from its source records."
  (setq +agent-shell-egress-summary--data
        (+agent-shell-egress-summary--collect +agent-shell-pstack-panel-directory
                                             +agent-shell-egress-summary--since))
  (let* ((inhibit-read-only t)
         (data +agent-shell-egress-summary--data)
         (counts (plist-get data :counts)))
    (erase-buffer)
    (insert (format "Egress %s to %s (UTC). Filter: %s\n"
                    (or (plist-get data :first) "unknown")
                    (or (plist-get data :last) "unknown")
                    (or +agent-shell-egress-summary--since "all dates"))
            (format (concat "%d records of %d; %d excluded, %d invalid; "
                            "%d partial, %d unavailable, %d not started, "
                            "%d observing\n")
                    (plist-get data :records) (plist-get data :total)
                    (plist-get data :excluded) (plist-get data :invalid)
                    (gethash 'partial counts 0) (gethash 'unavailable counts 0)
                    (gethash 'not-started counts 0) (gethash 'observing counts 0))
            (string-join (plist-get data :coverage) "\n")
            "\nSorted by agents, then destination. g: refresh; e: export CSV/JSON\n\n"
            (+agent-shell-egress-summary--table (plist-get data :rows)))
    (goto-char (point-min))))

(defun +agent-shell-egress-summary-export (file format)
  "Export the current summary table to FILE as FORMAT (`csv' or `json')."
  (interactive
   (list (read-file-name "Export egress table: ")
         (intern (completing-read "Format: " '("csv" "json") nil t))))
  (unless +agent-shell-egress-summary--data
    (user-error "Open an egress summary first"))
  (unless (memq format '(csv json)) (user-error "Choose CSV or JSON"))
  (let ((rows (plist-get +agent-shell-egress-summary--data :rows))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-buffer
      (if (eq format 'json)
          (insert (json-serialize (vconcat rows)) "\n")
        (dolist (row (cons (mapcar (lambda (column) (cons (car column) (cdr column)))
                                  +agent-shell-egress-summary--columns) rows))
          (insert (mapconcat
                   (lambda (column)
                     (concat "\"" (string-replace
                                    "\"" "\"\""
                                    (+agent-shell-egress-summary--cell
                                     (alist-get (car column) row))) "\""))
                   +agent-shell-egress-summary--columns ",") "\n")))
      (write-region (point-min) (point-max) file nil 'silent))
    (set-file-modes file #o600)))

(defvar +agent-shell-egress-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'+agent-shell-egress-summary--refresh)
    (define-key map (kbd "e") #'+agent-shell-egress-summary-export)
    map))

(define-derived-mode +agent-shell-egress-summary-mode special-mode "Egress"
  "Review recorded egress destinations grouped by agent."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'+agent-shell-egress-summary--refresh))

;;;###autoload
(defun +agent-shell-egress-summary (&optional since)
  "Show all recorded destinations, grouped by agent.
With a prefix argument, ask for an inclusive starting date (UTC YYYY-MM-DD).
Export the displayed table with `e'.  DNS questions are not host evidence;
unnamed addresses are kept separate even if another record names that IP."
  (interactive (list (when current-prefix-arg
                       (read-string "Records starting on or after (YYYY-MM-DD): "))))
  (let ((date (when since (+agent-shell-egress-summary--date since))))
    (with-current-buffer (get-buffer-create "*Agent shell egress*")
      (+agent-shell-egress-summary-mode)
      (setq +agent-shell-egress-summary--since date)
      (+agent-shell-egress-summary--refresh)
      (pop-to-buffer (current-buffer)))))

(provide '+agent-shell-egress-summary)
;;; +agent-shell-egress-summary.el ends here
