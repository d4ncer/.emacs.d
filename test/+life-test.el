;;; +life-test.el --- Tests for +life.el -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the life knowledge base helpers.
;; Run: emacs --batch -Q -L elpaca/builds/dash/ -L elpaca/builds/s/
;;        -L elpaca/builds/org/ -L elpaca/repos/vulpea/ -L lisp/ -L test/
;;        -l ert -l test-helper -l +life-test -f ert-run-tests-batch-and-exit

;;; Code:

(require 'test-helper)
(require '+life)

;;;; ---------------------------------------------------------------
;;;; Helpers
;;;; ---------------------------------------------------------------

;;; --active-p uses vulpea-meta-get (file-based, not struct-based)
;;; because org files store meta under * Metadata heading which
;;; vulpea DB extraction does not index into the note struct.

(ert-deftest +life/--active-p/in-progress-is-active ()
  "Notes with status 'in progress' are active."
  (let ((note (+life-test/make-note :id "1" :tags '("project"))))
    (cl-letf (((symbol-function 'vulpea-meta-get)
               (lambda (_id prop &optional _type)
                 (when (equal prop "status") "in progress"))))
      (should (+life/--active-p note)))))

(ert-deftest +life/--active-p/not-started-is-active ()
  "Notes with status 'not started' are active."
  (let ((note (+life-test/make-note :id "2" :tags '("goal"))))
    (cl-letf (((symbol-function 'vulpea-meta-get)
               (lambda (_id prop &optional _type)
                 (when (equal prop "status") "not started"))))
      (should (+life/--active-p note)))))

(ert-deftest +life/--active-p/complete-is-inactive ()
  "Notes with status 'complete' are not active."
  (let ((note (+life-test/make-note :id "3" :tags '("project"))))
    (cl-letf (((symbol-function 'vulpea-meta-get)
               (lambda (_id prop &optional _type)
                 (when (equal prop "status") "complete"))))
      (should-not (+life/--active-p note)))))

(ert-deftest +life/--active-p/abandoned-is-inactive ()
  "Notes with status 'abandoned' are not active."
  (let ((note (+life-test/make-note :id "4" :tags '("project"))))
    (cl-letf (((symbol-function 'vulpea-meta-get)
               (lambda (_id prop &optional _type)
                 (when (equal prop "status") "abandoned"))))
      (should-not (+life/--active-p note)))))

(ert-deftest +life/--active-p/no-status-is-active ()
  "Notes without a status meta are considered active."
  (let ((note (+life-test/make-note :id "5" :tags '("idea"))))
    (cl-letf (((symbol-function 'vulpea-meta-get)
               (lambda (_id _prop &optional _type) nil)))
      (should (+life/--active-p note)))))

(ert-deftest +life/--children-of/finds-children-by-parent-link ()
  "Returns notes whose parent meta links to the given ID."
  (let* ((child-a (+life-test/make-note :id "c1" :tags '("project")))
         (child-b (+life-test/make-note :id "c2" :tags '("goal")))
         (unrelated (+life-test/make-note :id "c3" :tags '("project"))))
    (cl-letf (((symbol-function 'vulpea-db-query-by-links-some)
               (lambda (_ids) (list child-a child-b unrelated)))
              ((symbol-function 'vulpea-meta-get)
               (lambda (id prop &optional _type)
                 (when (equal prop "parent")
                   (pcase id
                     ("c1" "P1") ("c2" "P1") ("c3" "OTHER"))))))
      (let ((result (+life/--children-of "P1")))
        (should (= 2 (length result)))
        (should (member child-a result))
        (should (member child-b result))))))

(ert-deftest +life/--children-of/filters-by-level-tag ()
  "When LEVEL-TAG is given, only children with that tag are returned."
  (let* ((project (+life-test/make-note :id "c1" :tags '("project")))
         (goal (+life-test/make-note :id "c2" :tags '("goal"))))
    (cl-letf (((symbol-function 'vulpea-db-query-by-links-some)
               (lambda (_ids) (list project goal)))
              ((symbol-function 'vulpea-meta-get)
               (lambda (_id prop &optional _type)
                 (when (equal prop "parent") "P1"))))
      (let ((result (+life/--children-of "P1" "project")))
        (should (= 1 (length result)))
        (should (equal "c1" (vulpea-note-id (car result))))))))

(ert-deftest +life/--children-of/returns-nil-when-no-children ()
  "Returns nil when no notes are parented to the given ID."
  (cl-letf (((symbol-function 'vulpea-db-query-by-links-some)
             (lambda (_ids) nil)))
    (should-not (+life/--children-of "NONEXISTENT"))))

;;;; ---------------------------------------------------------------
;;;; Capture — Initiative
;;;; ---------------------------------------------------------------

(ert-deftest +life/capture-initiative/creates-project-with-correct-args ()
  "Creating a project initiative passes correct args to vulpea-create."
  (let* ((parent-note (+life-test/make-note :id "G1" :title "Goal One"
                                            :tags '("goal" "initiative")))
         (created-note (+life-test/make-note :id "NEW1" :title "My Project"))
         (create-args nil)
         (visited nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "My Project"))
              ((symbol-function 'completing-read)
               (lambda (prompt &rest _)
                 (cond
                  ((string-prefix-p "Level" prompt) "project")
                  ((string-prefix-p "Status" prompt) "in progress"))))
              ((symbol-function 'vulpea-select)
               (lambda (&rest _) parent-note))
              ((symbol-function 'vulpea-create)
               (lambda (&rest args) (setq create-args args) created-note))
              ((symbol-function 'vulpea-visit)
               (lambda (note &rest _) (setq visited note))))
      (+life/capture-initiative)
      (should create-args)
      ;; First arg: title
      (should (equal "My Project" (nth 0 create-args)))
      ;; Second arg: nil (file-name)
      (should (null (nth 1 create-args)))
      ;; Keyword args
      (let ((plist (nthcdr 2 create-args)))
        (should (equal '("project" "initiative") (plist-get plist :tags)))
        (let ((meta (plist-get plist :meta)))
          (should (equal "project" (cdr (assoc "level" meta))))
          (should (equal "in progress" (cdr (assoc "status" meta))))
          (should (equal parent-note (cdr (assoc "parent" meta)))))
        (should (stringp (plist-get plist :body)))
        (should (string-match-p "\\* Tasks" (plist-get plist :body))))
      (should (eq created-note visited)))))

(ert-deftest +life/capture-initiative/creates-pillar-without-parent ()
  "Creating a pillar does not prompt for parent."
  (let* ((created-note (+life-test/make-note :id "PIL1" :title "Health"))
         (create-args nil)
         (select-called nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "Health"))
              ((symbol-function 'completing-read)
               (lambda (prompt &rest _)
                 (cond
                  ((string-prefix-p "Level" prompt) "pillar")
                  ((string-prefix-p "Status" prompt) "not started"))))
              ((symbol-function 'vulpea-select)
               (lambda (&rest _) (setq select-called t) nil))
              ((symbol-function 'vulpea-create)
               (lambda (&rest args) (setq create-args args) created-note))
              ((symbol-function 'vulpea-visit)
               (lambda (&rest _) nil)))
      (+life/capture-initiative)
      (should-not select-called)
      (let ((plist (nthcdr 2 create-args)))
        (should (equal '("pillar" "initiative") (plist-get plist :tags)))
        (let ((meta (plist-get plist :meta)))
          (should-not (assoc "parent" meta)))
        (should (string-match-p "\\* Vision" (plist-get plist :body)))))))

(ert-deftest +life/capture-initiative/creates-goal-with-body ()
  "Creating a goal includes Description, Artefacts, Timeline headings."
  (let* ((parent (+life-test/make-note :id "PIL1" :title "Career"
                                       :tags '("pillar" "initiative")))
         (created (+life-test/make-note :id "G1" :title "My Goal"))
         (create-args nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "My Goal"))
              ((symbol-function 'completing-read)
               (lambda (prompt &rest _)
                 (cond
                  ((string-prefix-p "Level" prompt) "goal")
                  ((string-prefix-p "Status" prompt) "not started"))))
              ((symbol-function 'vulpea-select)
               (lambda (&rest _) parent))
              ((symbol-function 'vulpea-create)
               (lambda (&rest args) (setq create-args args) created))
              ((symbol-function 'vulpea-visit)
               (lambda (&rest _) nil)))
      (+life/capture-initiative)
      (let* ((plist (nthcdr 2 create-args))
             (body (plist-get plist :body)))
        (should (string-match-p "\\* Description" body))
        (should (string-match-p "\\* Artefacts" body))
        (should (string-match-p "\\* Timeline" body))))))

;;;; ---------------------------------------------------------------
;;;; Capture — Person
;;;; ---------------------------------------------------------------

(ert-deftest +life/capture-person/creates-with-correct-args ()
  "Person capture creates note with person tag, handle, and meta."
  (let* ((org-note (+life-test/make-note :id "ORG1" :title "Canva"
                                         :tags '("org")))
         (created (+life-test/make-note :id "P1" :title "Jane Doe"))
         (create-args nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (cond
                  ((string-prefix-p "Person" prompt) "Jane Doe")
                  ((string-prefix-p "Role" prompt) "Engineer"))))
              ((symbol-function 'vulpea-select)
               (lambda (&rest _) org-note))
              ((symbol-function 'vulpea-create)
               (lambda (&rest args) (setq create-args args) created))
              ((symbol-function 'vulpea-visit)
               (lambda (&rest _) nil)))
      (+life/capture-person)
      (should create-args)
      (should (equal "Jane Doe" (nth 0 create-args)))
      (should (null (nth 1 create-args)))
      (let* ((plist (nthcdr 2 create-args))
             (tags (plist-get plist :tags))
             (meta (plist-get plist :meta)))
        (should (member "person" tags))
        (should (member "@JaneDoe" tags))
        (should (equal "person" (cdr (assoc "type" meta))))
        (should (equal "Engineer" (cdr (assoc "role" meta))))
        (should (equal org-note (cdr (assoc "org" meta))))))))

(ert-deftest +life/capture-person/skips-empty-role ()
  "When role is empty string, it is not included in meta."
  (let* ((created (+life-test/make-note :id "P2" :title "Bob"))
         (create-args nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (cond
                  ((string-prefix-p "Person" prompt) "Bob")
                  ((string-prefix-p "Role" prompt) ""))))
              ((symbol-function 'vulpea-select)
               (lambda (&rest _) (make-vulpea-note)))
              ((symbol-function 'vulpea-create)
               (lambda (&rest args) (setq create-args args) created))
              ((symbol-function 'vulpea-visit)
               (lambda (&rest _) nil)))
      (+life/capture-person)
      (let* ((plist (nthcdr 2 create-args))
             (meta (plist-get plist :meta)))
        (should-not (assoc "role" meta))))))

;;;; ---------------------------------------------------------------
;;;; Capture — Org
;;;; ---------------------------------------------------------------

(ert-deftest +life/capture-org/creates-with-correct-args ()
  "Org capture creates note with org tag and type meta."
  (let* ((parent-org (+life-test/make-note :id "ORG0" :title "Parent Co"
                                           :tags '("org")))
         (created (+life-test/make-note :id "ORG1" :title "Design System"))
         (create-args nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "Design System"))
              ((symbol-function 'vulpea-select)
               (lambda (&rest _) parent-org))
              ((symbol-function 'vulpea-create)
               (lambda (&rest args) (setq create-args args) created))
              ((symbol-function 'vulpea-visit)
               (lambda (&rest _) nil)))
      (+life/capture-org)
      (should (equal "Design System" (nth 0 create-args)))
      (should (null (nth 1 create-args)))
      (let* ((plist (nthcdr 2 create-args))
             (tags (plist-get plist :tags))
             (meta (plist-get plist :meta)))
        (should (equal '("org") tags))
        (should (equal "org" (cdr (assoc "type" meta))))
        (should (equal parent-org (cdr (assoc "parent-org" meta))))))))

;;;; ---------------------------------------------------------------
;;;; Capture — Idea
;;;; ---------------------------------------------------------------

(ert-deftest +life/capture-idea/creates-with-correct-args ()
  "Idea capture creates note with idea+seedling tags and body skeleton."
  (let* ((created (+life-test/make-note :id "I1" :title "Fast Feedback"))
         (create-args nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "Fast Feedback"))
              ((symbol-function 'vulpea-create)
               (lambda (&rest args) (setq create-args args) created))
              ((symbol-function 'vulpea-visit)
               (lambda (&rest _) nil)))
      (+life/capture-idea)
      (should (equal "Fast Feedback" (nth 0 create-args)))
      (should (null (nth 1 create-args)))
      (let* ((plist (nthcdr 2 create-args))
             (tags (plist-get plist :tags))
             (meta (plist-get plist :meta))
             (body (plist-get plist :body)))
        (should (equal '("idea" "seedling") tags))
        (should (equal "idea" (cdr (assoc "type" meta))))
        (should (string-match-p "\\* Abstract" body))
        (should (string-match-p "\\* Notes" body))
        (should (string-match-p "\\* Related reading" body))))))

;;;; ---------------------------------------------------------------
;;;; Zoom views
;;;; ---------------------------------------------------------------

(ert-deftest +life/view-pillars/queries-by-pillar-tag ()
  "Pillars view queries notes tagged 'pillar' and visits selection."
  (let* ((pillar (+life-test/make-note :id "PIL1" :title "Career"
                                       :tags '("pillar" "initiative")))
         (queried-tags nil)
         (visited nil))
    (cl-letf (((symbol-function 'vulpea-db-query-by-tags-every)
               (lambda (tags) (setq queried-tags tags) (list pillar)))
              ((symbol-function 'vulpea-select-from)
               (lambda (_prompt notes &rest _) (car notes)))
              ((symbol-function 'vulpea-visit)
               (lambda (note &rest _) (setq visited note))))
      (+life/view-pillars)
      (should (equal '("pillar") queried-tags))
      (should (eq pillar visited)))))

(ert-deftest +life/view-goals/filters-to-active-only ()
  "Goals view excludes completed and abandoned goals."
  (let* ((active (+life-test/make-note :id "G1" :title "Active Goal"
                                       :tags '("goal" "initiative")))
         (done (+life-test/make-note :id "G2" :title "Done Goal"
                                     :tags '("goal" "initiative")))
         (presented-notes nil))
    (cl-letf (((symbol-function 'vulpea-db-query-by-tags-every)
               (lambda (_tags) (list active done)))
              ((symbol-function 'vulpea-meta-get)
               (lambda (id prop &optional _type)
                 (when (equal prop "status")
                   (pcase id
                     ("G1" "in progress")
                     ("G2" "complete")))))
              ((symbol-function 'vulpea-select-from)
               (lambda (_prompt notes &rest _)
                 (setq presented-notes notes)
                 (car notes)))
              ((symbol-function 'vulpea-visit)
               (lambda (&rest _) nil)))
      (+life/view-goals)
      (should (= 1 (length presented-notes)))
      (should (equal "G1" (vulpea-note-id (car presented-notes)))))))

(ert-deftest +life/view-projects/filters-to-active-only ()
  "Projects view excludes completed and abandoned projects."
  (let* ((active (+life-test/make-note :id "PR1" :title "Active"
                                       :tags '("project" "initiative")))
         (abandoned (+life-test/make-note :id "PR2" :title "Abandoned"
                                          :tags '("project" "initiative")))
         (presented nil))
    (cl-letf (((symbol-function 'vulpea-db-query-by-tags-every)
               (lambda (_tags) (list active abandoned)))
              ((symbol-function 'vulpea-meta-get)
               (lambda (id prop &optional _type)
                 (when (equal prop "status")
                   (pcase id
                     ("PR1" "in progress")
                     ("PR2" "abandoned")))))
              ((symbol-function 'vulpea-select-from)
               (lambda (_prompt notes &rest _)
                 (setq presented notes)
                 (car notes)))
              ((symbol-function 'vulpea-visit)
               (lambda (&rest _) nil)))
      (+life/view-projects)
      (should (= 1 (length presented)))
      (should (equal "PR1" (vulpea-note-id (car presented)))))))

(ert-deftest +life/view-today/opens-journal-and-agenda ()
  "Today view calls vulpea-journal-today then org-agenda with 'd'."
  (let ((journal-called nil)
        (agenda-args nil))
    (cl-letf (((symbol-function 'delete-other-windows) (lambda () nil))
              ((symbol-function 'vulpea-journal-today)
               (lambda () (setq journal-called t)))
              ((symbol-function 'org-agenda)
               (lambda (&rest args) (setq agenda-args args))))
      (+life/view-today)
      (should journal-called)
      (should (equal '(nil "d") agenda-args)))))

;;;; ---------------------------------------------------------------
;;;; Navigation — uses vulpea-buffer-meta-get (reads current buffer)
;;;; ---------------------------------------------------------------

(ert-deftest +life/go-to-parent/visits-parent-note ()
  "Go-to-parent reads parent from current buffer meta and visits it."
  (let* ((parent (+life-test/make-note :id "PAR1" :title "Parent Goal"))
         (visited nil))
    (cl-letf (((symbol-function 'org-entry-get)
               (lambda (&rest _) "CUR1"))
              ((symbol-function 'vulpea-buffer-meta-get)
               (lambda (prop type &optional _bound)
                 (when (and (equal prop "parent") (eq type 'note))
                   parent)))
              ((symbol-function 'vulpea-visit)
               (lambda (note &rest _) (setq visited note))))
      (+life/go-to-parent)
      (should (eq parent visited)))))

(ert-deftest +life/go-to-parent/errors-when-no-parent ()
  "Signals user-error when current buffer has no parent meta."
  (cl-letf (((symbol-function 'org-entry-get)
             (lambda (&rest _) "CUR1"))
            ((symbol-function 'vulpea-buffer-meta-get)
             (lambda (_prop _type &optional _bound) nil)))
    (should-error (+life/go-to-parent) :type 'user-error)))

(ert-deftest +life/show-children/presents-children-for-selection ()
  "Show-children finds children and presents them via vulpea-select-from."
  (let* ((child (+life-test/make-note
                 :id "CH1" :title "Child" :tags '("project")))
         (presented nil)
         (visited nil))
    (cl-letf (((symbol-function 'org-entry-get)
               (lambda (&rest _) "PAR1"))
              ((symbol-function 'vulpea-db-query-by-links-some)
               (lambda (_ids) (list child)))
              ((symbol-function 'vulpea-meta-get)
               (lambda (_id prop &optional _type)
                 (when (equal prop "parent") "PAR1")))
              ((symbol-function 'vulpea-select-from)
               (lambda (_prompt notes &rest _)
                 (setq presented notes)
                 (car notes)))
              ((symbol-function 'vulpea-visit)
               (lambda (note &rest _) (setq visited note))))
      (+life/show-children)
      (should (= 1 (length presented)))
      (should (eq child visited)))))

(ert-deftest +life/show-children/errors-when-no-children ()
  "Signals user-error when current note has no children."
  (cl-letf (((symbol-function 'org-entry-get)
             (lambda (&rest _) "PAR1"))
            ((symbol-function 'vulpea-db-query-by-links-some)
             (lambda (_ids) nil)))
    (should-error (+life/show-children) :type 'user-error)))

(ert-deftest +life/show-stakeholders/finds-stakeholder-notes ()
  "Show-stakeholders reads roles from current buffer meta."
  (let* ((owner (+life-test/make-note :id "OWN1" :title "Raghu"))
         (coach (+life-test/make-note :id "COA1" :title "Vivian"))
         (presented nil))
    (cl-letf (((symbol-function 'org-entry-get)
               (lambda (&rest _) "CUR1"))
              ((symbol-function 'vulpea-buffer-meta-get)
               (lambda (prop type &optional _bound)
                 (when (eq type 'note)
                   (pcase prop
                     ("owner" owner)
                     ("coach" coach)))))
              ((symbol-function 'vulpea-select-from)
               (lambda (_prompt notes &rest _)
                 (setq presented notes)
                 (car notes)))
              ((symbol-function 'vulpea-visit)
               (lambda (&rest _) nil)))
      (+life/show-stakeholders)
      (should (= 2 (length presented))))))

(ert-deftest +life/show-stakeholders/errors-when-no-stakeholders ()
  "Signals user-error when buffer has no stakeholder meta."
  (cl-letf (((symbol-function 'org-entry-get)
             (lambda (&rest _) "CUR1"))
            ((symbol-function 'vulpea-buffer-meta-get)
             (lambda (_prop _type &optional _bound) nil)))
    (should-error (+life/show-stakeholders) :type 'user-error)))

(ert-deftest +life/person-initiatives/finds-initiatives-linking-to-person ()
  "Person-initiatives finds initiatives that link to the current person."
  (let* ((initiative (+life-test/make-note
                      :id "INIT1" :title "My Project"
                      :tags '("project" "initiative")))
         (non-initiative (+life-test/make-note
                          :id "NOTE1" :title "Random Note"
                          :tags '("idea")))
         (presented nil))
    (cl-letf (((symbol-function 'org-entry-get)
               (lambda (&rest _) "PERSON1"))
              ((symbol-function 'vulpea-db-query-by-links-some)
               (lambda (_ids) (list initiative non-initiative)))
              ((symbol-function 'vulpea-select-from)
               (lambda (_prompt notes &rest _)
                 (setq presented notes)
                 (car notes)))
              ((symbol-function 'vulpea-visit)
               (lambda (&rest _) nil)))
      (+life/person-initiatives)
      (should (= 1 (length presented)))
      (should (equal "INIT1" (vulpea-note-id (car presented)))))))

(ert-deftest +life/person-initiatives/errors-when-no-initiatives ()
  "Signals user-error when no initiatives reference the person."
  (cl-letf (((symbol-function 'org-entry-get)
             (lambda (&rest _) "PERSON1"))
            ((symbol-function 'vulpea-db-query-by-links-some)
             (lambda (_ids) nil)))
    (should-error (+life/person-initiatives) :type 'user-error)))

;;;; ---------------------------------------------------------------
;;;; Migration — remove * Metadata heading
;;;; ---------------------------------------------------------------

(ert-deftest +life/migrate/removes-metadata-heading ()
  "Removes the * Metadata heading, promoting description list to file level."
  (let ((file (make-temp-file "life-test-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID:       test-uuid\n:END:\n"
                    "#+title: Test Note\n"
                    "#+filetags: :project:initiative:\n\n"
                    "* Metadata\n"
                    "- level :: project\n"
                    "- status :: in progress\n"
                    "- parent :: [[id:P1][Parent]]\n\n"
                    "* Description\nSome content.\n"))
          (should (+life/migrate-remove-metadata-heading file))
          (let ((result (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
            ;; Metadata heading gone
            (should-not (string-match-p "^\\* Metadata" result))
            ;; Description list items preserved at file level
            (should (string-match-p "^- level :: project" result))
            (should (string-match-p "^- status :: in progress" result))
            (should (string-match-p "^- parent :: \\[\\[id:P1\\]\\[Parent\\]\\]" result))
            ;; Other headings preserved
            (should (string-match-p "^\\* Description" result))
            (should (string-match-p "Some content\\." result))))
      (delete-file file))))

(ert-deftest +life/migrate/skips-files-without-metadata-heading ()
  "Returns nil for files that don't have a * Metadata heading."
  (let ((file (make-temp-file "life-test-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+title: Plain Note\n\n- type :: idea\n\n* Notes\n"))
          (should-not (+life/migrate-remove-metadata-heading file)))
      (delete-file file))))

(ert-deftest +life/migrate/batch-processes-directory ()
  "Batch migration processes all org files in a directory."
  (let ((dir (make-temp-file "life-test-dir-" t)))
    (unwind-protect
        (progn
          ;; File with Metadata heading
          (with-temp-file (expand-file-name "a.org" dir)
            (insert "#+title: A\n\n* Metadata\n- type :: person\n\n* Notes\n"))
          ;; File without
          (with-temp-file (expand-file-name "b.org" dir)
            (insert "#+title: B\n\n- type :: idea\n"))
          ;; Non-org file (should be skipped)
          (with-temp-file (expand-file-name "c.txt" dir)
            (insert "* Metadata\n- irrelevant\n"))
          (should (= 1 (+life/migrate-remove-metadata-headings dir)))
          ;; Verify a.org was fixed
          (let ((result (with-temp-buffer
                          (insert-file-contents (expand-file-name "a.org" dir))
                          (buffer-string))))
            (should-not (string-match-p "^\\* Metadata" result))
            (should (string-match-p "^- type :: person" result))))
      (delete-directory dir t))))

(provide '+life-test)
;;; +life-test.el ends here
