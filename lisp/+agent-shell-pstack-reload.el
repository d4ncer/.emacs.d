;;; +agent-shell-pstack-reload.el --- Reload live pstack code -*- lexical-binding: t; -*-

;;; Commentary:
;; Reload only active libraries, adopting changed defaults that are still in
;; use, and carry live panels across struct layout changes.

;;; Code:

(require 'cl-lib)

(defvar +agent-shell-pstack-panel--panels)
(defvar +agent-shell-pstack-panel--schema)
(defvar +agent-shell-arena-remote--arenas)

(defconst +agent-shell-pstack-reload--libraries
  '(+agent-shell-sandbox +agent-shell-drive +agent-shell-arena-git
    +agent-shell-egress +agent-shell-pstack +agent-shell-pstack-panel
    +agent-shell-pstack-panels +agent-shell-arena-remote
    +agent-shell-pstack-arena-trace)
  "Pstack libraries in dependency order.")

(defvar +agent-shell-pstack-reload--carrying nil
  "Non-nil while `+agent-shell-pstack-reload' loads and carries panels.
The panel library's schema guard then leaves every panel alone.")

(defun +agent-shell-pstack-reload--minor-mode-p (symbol)
  "Return non-nil if SYMBOL is a minor mode variable."
  (or (memq symbol minor-mode-list)
      (eq (get symbol 'custom-set) 'custom-set-minor-mode)))

(defun +agent-shell-pstack-reload--default (symbol)
  "Return SYMBOL's evaluated standard value in a list, or nil on error."
  (when-let* ((standard (get symbol 'standard-value)))
    (condition-case nil
        (list (eval (car standard) t))
      (error nil))))

;;;; Carrying panels

(defun +agent-shell-pstack-reload--layouts (files)
  "Return (TYPE . SLOT-NAMES) for each record struct the sources of FILES define."
  (let (layouts)
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents (concat (file-name-sans-extension file) ".el"))
        (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                (when (eq (car-safe form) 'cl-defstruct)
                  (let* ((spec (cadr form))
                         (name (if (consp spec) (car spec) spec))
                         (options (and (consp spec) (cdr spec)))
                         (slots (cddr form)))
                    (when (stringp (car slots))
                      (setq slots (cdr slots)))
                    (unless (assq :type (cl-remove-if-not #'consp options))
                      (push (cons name (mapcar (lambda (slot)
                                                 (if (consp slot) (car slot)
                                                   slot))
                                               slots))
                            layouts))))))
          (end-of-file nil))))
    layouts))

(defun +agent-shell-pstack-reload--slots (type)
  "Return TYPE's slot names as defined now, or nil if it is no struct."
  (mapcar #'car (cl-remove 'cl-tag-slot
                           (ignore-errors (cl-struct-slot-info type))
                           :key #'car)))

(defun +agent-shell-pstack-reload--names (object)
  "Return the slot names OBJECT was made with, or `unknown'.
Slots are only appended, so a record shorter than its struct holds the
first slots."
  (let ((names (+agent-shell-pstack-reload--slots (type-of object)))
        (count (1- (length object))))
    (if (<= count (length names)) (take count names) 'unknown)))

(defun +agent-shell-pstack-reload--snapshot (roots types)
  "Snapshot every record of TYPES reachable from ROOTS, slot by slot by name.
Return a hash table from each record to an alist of (SLOT . VALUE), or to
`unknown' when its layout cannot be told."
  (let ((snapshots (make-hash-table :test #'eq))
        (seen (make-hash-table :test #'eq))
        (pending (copy-sequence roots)))
    (while pending
      (let ((value (pop pending)))
        (unless (gethash value seen)
          (cond
           ((consp value)
            (puthash value t seen)
            (push (car value) pending)
            (push (cdr value) pending))
           ((and (recordp value) (memq (type-of value) types))
            (puthash value t seen)
            (let ((names (+agent-shell-pstack-reload--names value)))
              (puthash value
                       (if (eq names 'unknown)
                           'unknown
                         (mapcar (lambda (name)
                                   (let ((slot (cl-struct-slot-value
                                                (type-of value) name value)))
                                     (push slot pending)
                                     (cons name slot)))
                                 names))
                       snapshots)))))))
    snapshots))

(defun +agent-shell-pstack-reload--losses (root snapshots layouts)
  "Return the slots records reachable from ROOT would lose under LAYOUTS.
SNAPSHOTS is from `+agent-shell-pstack-reload--snapshot'.  A slot is lost
when it holds a value and the new layout lacks it; each loss is TYPE.SLOT."
  (let ((seen (make-hash-table :test #'eq))
        (pending (list root))
        losses)
    (while pending
      (let* ((value (pop pending))
             (snapshot (and (not (gethash value seen))
                            (gethash value snapshots))))
        (cond
         ((gethash value seen))
         ((consp value)
          (puthash value t seen)
          (push (car value) pending)
          (push (cdr value) pending))
         ((eq snapshot 'unknown)
          (puthash value t seen)
          (cl-pushnew (format "%s (unknown layout)" (type-of value)) losses
                      :test #'equal))
         (snapshot
          (puthash value t seen)
          (let ((names (alist-get (type-of value) layouts)))
            (pcase-dolist (`(,name . ,slot) snapshot)
              (push slot pending)
              (when (and slot (not (memq name names)))
                (cl-pushnew (format "%s.%s" (type-of value) name) losses
                            :test #'equal))))))))
    (nreverse losses)))

(defun +agent-shell-pstack-reload--rebuild (snapshots)
  "Return a function carrying a value over to the structs defined now.
A record whose layout is unchanged keeps its identity; any other is
rebuilt from its snapshot in SNAPSHOTS, slot by slot by name, new slots
taking their defaults.  Shared records stay shared, and lists are
updated in place."
  (let ((carried (make-hash-table :test #'eq))
        carry)
    (setq carry
          (lambda (value)
            (let ((snapshot (gethash value snapshots)))
              (cond
               ((gethash value carried))
               ((consp value)
                (puthash value value carried)
                (let ((cell value))
                  (while (consp cell)
                    (let ((head (funcall carry (car cell))))
                      (unless (eq head (car cell))
                        (setcar cell head)))
                    (if (and (consp (cdr cell))
                             (not (gethash (cdr cell) carried)))
                        (progn (setq cell (cdr cell))
                               (puthash cell cell carried))
                      (let ((tail (funcall carry (cdr cell))))
                        (unless (eq tail (cdr cell))
                          (setcdr cell tail)))
                      (setq cell nil))))
                value)
               ((consp snapshot)
                (let* ((type (type-of value))
                       (info (cl-remove 'cl-tag-slot (cl-struct-slot-info type)
                                        :key #'car))
                       (same (equal (mapcar #'car info)
                                    (mapcar #'car snapshot)))
                       (record (if same value
                                 (make-record type (length info) nil))))
                  (puthash value record carried)
                  (cl-loop
                   for (name default) in info
                   for index from 1
                   do (aset record index
                            (if-let* ((slot (assq name snapshot)))
                                (funcall carry (cdr slot))
                              (eval default t))))
                  record))
               (t value)))))
    carry))

(defun +agent-shell-pstack-reload--carry (snapshots)
  "Carry the panels and remote records over with SNAPSHOTS.
Point both tables at the carried records, give every panel the current
schema, and return how many panels changed."
  (let ((carry (+agent-shell-pstack-reload--rebuild snapshots))
        (count 0))
    (when (boundp '+agent-shell-pstack-panel--panels)
      (maphash
       (lambda (id panel)
         (let ((new (funcall carry panel)))
           (when (and (recordp new)
                      (eq (type-of new) '+agent-shell-pstack-panel))
             (let ((schema (cl-struct-slot-offset (type-of new) 'schema)))
               (unless (and (eq new panel)
                            (eql (aref new schema)
                                 +agent-shell-pstack-panel--schema))
                 (cl-incf count))
               (aset new schema +agent-shell-pstack-panel--schema)))
           (puthash id new +agent-shell-pstack-panel--panels)))
       +agent-shell-pstack-panel--panels))
    (when (boundp '+agent-shell-arena-remote--arenas)
      (maphash (lambda (id remote)
                 (puthash id (funcall carry remote)
                          +agent-shell-arena-remote--arenas))
               +agent-shell-arena-remote--arenas))
    count))

(defun +agent-shell-pstack-reload--roots ()
  "Return the panels and remote records that live in memory."
  (append (and (boundp '+agent-shell-pstack-panel--panels)
               (hash-table-values +agent-shell-pstack-panel--panels))
          (and (boundp '+agent-shell-arena-remote--arenas)
               (hash-table-values +agent-shell-arena-remote--arenas))))

(defun +agent-shell-pstack-reload--blockers (snapshots layouts)
  "Return (ID . LOSSES) for each unfinished panel LAYOUTS cannot carry."
  (when (boundp '+agent-shell-pstack-panel--panels)
    (let (blockers)
      (maphash
       (lambda (id panel)
         (let ((snapshot (gethash panel snapshots)))
           (unless (and (consp snapshot)
                        (eq 'finished (alist-get 'phase snapshot)))
             (when-let* ((losses (+agent-shell-pstack-reload--losses
                                  panel snapshots layouts)))
               (push (cons id losses) blockers)))))
       +agent-shell-pstack-panel--panels)
      (sort blockers (lambda (a b) (string< (car a) (car b)))))))

;;;; Command

;;;###autoload
(defun +agent-shell-pstack-reload ()
  "Reload the already loaded pstack libraries in dependency order.
Adopt changed option defaults only where the live value still equals the
old default.  Preserve minor modes.  Carry every panel and remote record
over to the new struct layouts by slot name, and refuse to load anything
when an unfinished panel holds a slot the new code removes.  Options
whose standard values cannot be evaluated are left alone."
  (interactive)
  (let (libraries options modes reset layouts snapshots
        (carried 0)
        (load-prefer-newer t))
    (dolist (feature +agent-shell-pstack-reload--libraries)
      (when (featurep feature)
        (when-let* ((history
                     (cl-find-if
                      (lambda (entry)
                        (member (cons 'provide feature) (cdr entry)))
                      load-history)))
          (push (cons feature history) libraries)
          (dolist (symbol (cdr history))
            (when (and (symbolp symbol) (boundp symbol))
              (when (and (memq symbol global-minor-modes)
                         (default-value symbol))
                (cl-pushnew symbol modes))
              (unless (+agent-shell-pstack-reload--minor-mode-p symbol)
                (when-let* ((default
                             (+agent-shell-pstack-reload--default symbol)))
                  (push (cons symbol (car default)) options))))))))
    (setq libraries (nreverse libraries)
          options (nreverse options)
          layouts (+agent-shell-pstack-reload--layouts
                   (mapcar #'cadr libraries))
          snapshots (+agent-shell-pstack-reload--snapshot
                     (+agent-shell-pstack-reload--roots)
                     (mapcar #'car layouts)))
    (when-let* ((blockers (+agent-shell-pstack-reload--blockers
                           snapshots layouts)))
      (user-error "pstack: reloaded nothing; the new code cannot carry \
running panel%s %s; retry after %s"
                  (if (cdr blockers) "s" "")
                  (mapconcat (lambda (blocker)
                               (format "%s (%s)" (car blocker)
                                       (string-join (cdr blocker) ", ")))
                             blockers "; ")
                  (if (cdr blockers) "they finish" "it finishes")))
    (unwind-protect
        (let ((+agent-shell-pstack-reload--carrying t))
          (dolist (library libraries)
            (load (file-name-sans-extension (cadr library)) nil t))
          (setq carried (+agent-shell-pstack-reload--carry snapshots))
          (dolist (option options)
            (let ((symbol (car option))
                  (old (cdr option)))
              (when (and (boundp symbol)
                         (not (+agent-shell-pstack-reload--minor-mode-p
                               symbol))
                         (equal (default-value symbol) old))
                (when-let* ((new (+agent-shell-pstack-reload--default symbol)))
                  (unless (equal old (car new))
                    (custom-reevaluate-setting symbol)
                    (push symbol reset)))))))
      (dolist (mode modes)
        (unless (default-value mode)
          (funcall mode 1))))
    (message "pstack: reloaded %d libraries (%s); options reset: %s; \
carried over %d pstack panel%s"
             (length libraries)
             (if libraries (mapconcat (lambda (entry) (symbol-name (car entry)))
                                      libraries ", ") "none")
             (if reset (mapconcat #'symbol-name (nreverse reset) ", ") "none")
             carried (if (= carried 1) "" "s"))))

(provide '+agent-shell-pstack-reload)
;;; +agent-shell-pstack-reload.el ends here
