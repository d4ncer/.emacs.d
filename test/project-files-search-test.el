;;; project-files-search-test.el --- Search worker behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise matching, ranking and lifecycle through the real Python worker.

;;; Code:

(require 'ert)
(require '+project-files)

(defmacro +project-files-test--with-search (&rest body)
  "Run BODY with +write and +query helpers for an isolated search worker."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "file-search-" t)))
          (manifest (expand-file-name "files.nul" root))
          (channel nil) (pending "") (response nil) (request-id 0)
          (response-ids nil) handle)
     (unwind-protect
         (cl-labels
             ((+write (names)
                (let ((coding-system-for-write 'utf-8-unix))
                  (with-temp-file manifest
                    (dolist (name names) (insert name "\0")))))
              (+send (query preferences limit)
                (setq response nil)
                (cl-incf request-id)
                (process-send-string
                 channel
                 (concat
                  (json-serialize
                   (list :id request-id :manifest manifest :query query
                         :preferences preferences :limit limit)) "\n")))
              (+query (query &optional preferences limit)
                (+send query (or preferences '(:recent [])) (or limit 500))
                (+project-files-test--wait
                 (lambda () (and response
                                 (eql request-id (plist-get response :id)))))
                (when (plist-get response :error)
                  (error "%s" (plist-get response :error)))
                (plist-get response :candidates)))
           (setq handle
                 (+project-files--start-search
                  (executable-find "rg")
                  (lambda (process data)
                    (setq pending (concat pending data))
                    (while (string-match "\n" pending)
                      (let ((end (match-end 0)))
                        (setq response (+project-files--search-response
                                        process (substring pending 0 (1- end)))
                              pending (substring pending end))
                        (push (plist-get response :id) response-ids))))
                  (lambda (process) (setq channel process)) #'ignore))
           (+project-files-test--wait (lambda () channel))
           ,@body)
       (+project-files--stop-search handle)
       (when handle
         (+project-files-test--wait
          (lambda ()
            (not (process-live-p (plist-get handle :process)))))
         (should (zerop (process-exit-status (plist-get handle :process)))))
       (delete-directory root t))))

(ert-deftest +project-files-test-search-literal-and-terms ()
  (+project-files-test--with-search
    (+write '("ui/Button.test.ts" "ui/button.ts" "test/helper.ts"
              "other/button[1].ts" "other/button1.ts"))
    (should (equal (+query "TEST button") '("ui/Button.test.ts")))
    (should (equal (+query "button TEST") '("ui/Button.test.ts")))
    (should (equal (+query "button[1]") '("other/button[1].ts")))
    (should-not (+query "nothere"))))

(ert-deftest +project-files-test-search-filename-records-are-lossless ()
  (+project-files-test--with-search
    (let ((names (list "space dir/button name.ts" "new\nline/button.ts"
                       "slash\\quote\"/button.ts" "日本語/button.ts"
                       (concat "nested/" (apply #'concat (make-list 65 "long/"))
                               "button.ts")
                       "-button.ts" "shell$(never-execute)/button.ts")))
      (+write names)
      (should (equal (sort (+query "button") #'string<) (sort names #'string<)))
      (should (equal (+query "-button") '("-button.ts")))
      (should (equal (+query "shell$(never-execute)")
                     '("shell$(never-execute)/button.ts"))))))

(ert-deftest +project-files-test-search-limit-considers-the-entire-pool ()
  (+project-files-test--with-search
    (+write (append (cl-loop for i below 1000
                             collect (format "button/subtree%d/helper.ts" i))
                    '("src/button.ts")))
    (should (equal (+query "button" nil 1) '("src/button.ts")))
    (let ((all (+query "button" nil 2000)))
      (should (= 1001 (length all)))
      (should (equal (+query "button" nil 64) (seq-take all 64))))))

(ert-deftest +project-files-test-search-basename-and-recency-order ()
  (+project-files-test--with-search
    (+write '("old/button.ts" "new/button.ts" "mybutton.ts"))
    (should (equal (+query "button" '(:recent ["mybutton.ts" "new/button.ts"]))
                   '("new/button.ts" "old/button.ts" "mybutton.ts")))))

(ert-deftest +project-files-test-search-overlay-add-delete-and-deduplicate ()
  (+project-files-test--with-search
    (+write '("old/button.ts" "keep/button.ts" "keep/button.ts"))
    (should (equal (+query "button"
                           '(:include ["new/button.ts" "new/other.ts"]
                             :exclude ["old/button.ts"]))
                   '("new/button.ts" "keep/button.ts")))))

(ert-deftest +project-files-test-search-missing-snapshot-keeps-new-files ()
  (+project-files-test--with-search
    (should (equal (+query "button" '(:include ["new/button.ts"]))
                   '("new/button.ts")))))

(ert-deftest +project-files-test-search-worker-reuses-and-supersedes ()
  (+project-files-test--with-search
    (+write '("button.ts" "other.ts"))
    (+send "button" '(:recent []) 500)
    (+send "other" '(:recent []) 500)
    (+project-files-test--wait
     (lambda () (and response (eql request-id (plist-get response :id)))))
    (should (equal (plist-get response :candidates) '("other.ts")))
    (should (process-live-p (plist-get handle :process)))
    (let ((pid (process-id (plist-get handle :process))))
      (should (equal (+query "button") '("button.ts")))
      (should (= pid (process-id (plist-get handle :process)))))))

(ert-deftest +project-files-test-search-cancels-a-broad-running-query ()
  (+project-files-test--with-search
    (+write (cons "other.ts"
                  (cl-loop for i below 100000
                           collect (format "button/subtree%d/helper.ts" i))))
    (+send "button" '(:recent []) 500)
    (let ((obsolete request-id))
      (accept-process-output nil 0.03)
      (+send "other" '(:recent []) 500)
      (+project-files-test--wait
       (lambda () (and response (eql request-id (plist-get response :id)))))
      (should (equal (plist-get response :candidates) '("other.ts")))
      (should-not (memq obsolete response-ids)))
    ;; Closing the picker during another broad search also exits cleanly.
    (+send "button" '(:recent []) 500)))

(provide 'project-files-search-test)
;;; project-files-search-test.el ends here
