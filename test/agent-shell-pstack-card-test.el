;;; agent-shell-pstack-card-test.el --- Request card tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise card content, markdown copies, controls and live panel updates.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-support)
(require '+agent-shell-pstack-card)

(defmacro +agent-shell-pstack-card-test--with (&rest body)
  "Run BODY with deterministic parser agents and isolated panel state."
  (declare (indent 0))
  `(let ((+agent-shell-pstack-card-mode t)
         (+agent-shell-sandbox-backend 'fixture)
         (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
         (+agent-shell-pstack-panel--request-digests
          (make-hash-table :test #'equal))
         (agent-shell-markdown-render-functions
          '(+agent-shell-pstack-card--render-hook)))
     (cl-letf (((symbol-function '+agent-shell-pstack-panel--agents)
                (lambda () '(codex claude-code)))
               ((symbol-function '+agent-shell-pstack-panel--candidate-agents)
                (lambda () '(codex claude-code))))
       ,@body)))

(defun +agent-shell-pstack-card-test--source (shape &optional brief)
  "Return a complete request fence for SHAPE with optional BRIEF."
  (concat
   "```pstack-panel\n"
   (json-serialize
    `((version . 1) (shape . ,shape) (brief . ,(or brief "Find the cause."))
      (timeout . 120)
      (members . [((role . "reader") (agent . "codex")
                   (focus . "Trace the parser"))])
      ,@(when (equal shape "arena")
          '((rubric . ["Correct behavior" "Readable implementation" "Focused verification"])
            (judge . ((agent . "claude-code"))) (judge_timeout . 60)))))
   "\n```\n"))

(defun +agent-shell-pstack-card-test--text (shape)
  "Return the card text for a parsed request of SHAPE."
  (+agent-shell-pstack-card--text
   (+agent-shell-pstack-panel--parse
    (+agent-shell-pstack-panel--block
     (+agent-shell-pstack-card-test--source shape)))))

(ert-deftest +agent-shell-pstack-card-test-how ()
  (+agent-shell-pstack-card-test--with
    (let ((text (+agent-shell-pstack-card-test--text "how")))
      (should (string-search "how · investigate and explain" text))
      (should (string-search "1 member\n  reader · codex · Trace the parser" text))
      (should (string-search "Brief:\n  Find the cause." text))
      (should (string-search "Timeout: 120s" text))
      (should-not (string-search "Panel " text)))))

(ert-deftest +agent-shell-pstack-card-test-interrogate ()
  (+agent-shell-pstack-card-test--with
    (let ((text (+agent-shell-pstack-card-test--text "interrogate")))
      (should (string-search "interrogate · adversarial review" text))
      (should (string-search "reader · codex · Trace the parser" text)))))

(ert-deftest +agent-shell-pstack-card-test-delegate ()
  (+agent-shell-pstack-card-test--with
    (let ((text (+agent-shell-pstack-card-test--text "delegate")))
      (should (string-search "delegate · implement, test and commit" text))
      (should (string-search "1 member" text))
      (should (string-search "reader · codex · Trace the parser" text))
      (should-not (string-search "Judge:" text)))))

(ert-deftest +agent-shell-pstack-card-test-arena ()
  (+agent-shell-pstack-card-test--with
    (let ((text (+agent-shell-pstack-card-test--text "arena")))
      (should (string-search "arena · implement, compare and judge" text))
      (should (string-search "1 member" text))
      (should (string-search "reader · codex · Trace the parser" text))
      (should (string-search "Rubric:\n  • Correct behavior" text))
      (should (string-search "• Readable implementation" text))
      (should (string-search "Judge: claude-code" text))
      (should (string-search "Timeouts: candidates 120s · judge 60s" text)))))

(ert-deftest +agent-shell-pstack-card-test-multiple-members ()
  (+agent-shell-pstack-card-test--with
    (let ((request (+agent-shell-pstack-panel--parse
                    (+agent-shell-pstack-panel--block
                     (+agent-shell-pstack-card-test--source "how")))))
      (setf (+agent-shell-pstack-panel-members request)
            (append (+agent-shell-pstack-panel-members request)
                    (list (+agent-shell-pstack-panel--make-member
                           :role "reviewer" :agent 'claude-code))))
      (let ((text (+agent-shell-pstack-card--text request)))
        (should (string-search "2 members" text))
        (should (string-search "reviewer · claude-code · no focus specified"
                               text))))))

(ert-deftest +agent-shell-pstack-card-test-malformed ()
  (+agent-shell-pstack-card-test--with
    (dolist (body '("not json" "{}"))
      (let* ((source (concat "```pstack-panel\n" body "\n```\n"))
             (reason (condition-case err
                         (+agent-shell-pstack-panel--parse body)
                       (user-error (error-message-string err)))))
        (with-temp-buffer
          (insert (agent-shell-markdown-convert source))
          (should (string-search reason (get-text-property (point-min) 'display)))
          (should (equal source (buffer-substring-no-properties
                                 (point-min) (point-max)))))))))

(ert-deftest +agent-shell-pstack-card-test-toggle-and-copy ()
  (+agent-shell-pstack-card-test--with
    (let ((source (+agent-shell-pstack-card-test--source "how")))
      (with-temp-buffer
        (insert (agent-shell-markdown-convert source))
        (goto-char (point-min))
        (setq buffer-read-only t)
        (let ((card (get-text-property (point) 'display)))
          (should (stringp card))
          (+agent-shell-pstack-card-toggle-source)
          (should-not (get-text-property (point) 'display))
          (should (equal source (buffer-substring-no-properties
                                 (point-min) (point-max))))
          (+agent-shell-pstack-card-toggle-source)
          (should (equal card (get-text-property (point) 'display)))
          (should (equal source (agent-shell-markdown-reconstruct
                                 (point-min) (point-max))))
          (let ((kill-ring nil))
            (copy-region-as-kill (point-min) (point-max))
            (should (equal source (substring-no-properties (car kill-ring))))))))))

(ert-deftest +agent-shell-pstack-card-test-fold-and-wrap ()
  (+agent-shell-pstack-card-test--with
    (with-temp-buffer
      (insert (agent-shell-markdown-convert
               (+agent-shell-pstack-card-test--source
                "how" (concat (mapconcat #'identity
                                          (make-list 70 "words to wrap") " ")
                              " END-OF-BRIEF"))))
      (goto-char (point-min))
      (let ((text (get-text-property (point) 'display)))
        (should (string-search "TAB: expand brief" text))
        (should-not (string-search "END-OF-BRIEF" text)))
      (+agent-shell-pstack-card-toggle-brief)
      (let ((text (get-text-property (point) 'display)))
        (should (string-search "END-OF-BRIEF" text))
        (should (string-search "TAB: fold brief" text))
        (dolist (line (split-string text "\n"))
          (should (<= (string-width line) 72))))
      (+agent-shell-pstack-card-toggle-brief)
      (should-not (string-search "END-OF-BRIEF"
                                 (get-text-property (point) 'display))))))

(ert-deftest +agent-shell-pstack-card-test-existing-panel-and-copies ()
  (+agent-shell-pstack-card-test--with
    (let* ((source (+agent-shell-pstack-card-test--source "how"))
           (block (+agent-shell-pstack-panel--block source))
           (id (+agent-shell-pstack-panel--id "session" block))
           (panel (+agent-shell-pstack-panel--parse block)))
      (with-temp-buffer
        (setq-local agent-shell--state '((:session . ((:id . "session")))))
        (insert (agent-shell-markdown-convert source))
        (setf (+agent-shell-pstack-panel-id panel) id
              (+agent-shell-pstack-panel-phase panel) 'running)
        (puthash id panel +agent-shell-pstack-panel--panels)
        (+agent-shell-pstack-card--update-buffers)
        (should (string-search (concat "Panel " id " · running · C-c P")
                               (get-text-property (point-min) 'display)))
        (let ((copy (buffer-string)))
          (with-temp-buffer
            (insert copy)
            (setf (+agent-shell-pstack-panel-phase panel) 'finished)
            (+agent-shell-pstack-card--update-buffers)
            (should (string-search "· finished · C-c P"
                                   (get-text-property (point-min) 'display)))
            (should (equal source (agent-shell-markdown-reconstruct
                                   (point-min) (point-max))))))))))

(ert-deftest +agent-shell-pstack-card-test-panel-in-temporary-renderer ()
  (+agent-shell-pstack-card-test--with
    (let* ((source (+agent-shell-pstack-card-test--source "delegate"))
           (block (+agent-shell-pstack-panel--block source))
           (id (+agent-shell-pstack-panel--id "session" block))
           (panel (+agent-shell-pstack-panel--parse block)))
      (with-temp-buffer
        (setq-local agent-shell--state '((:session . ((:id . "session")))))
        (setf (+agent-shell-pstack-panel-id panel) id
              (+agent-shell-pstack-panel-lead panel) (current-buffer)
              (+agent-shell-pstack-panel-phase panel) 'delegating)
        (puthash id panel +agent-shell-pstack-panel--panels)
        (let ((rendered (agent-shell-markdown-convert source)))
          (should (string-search "· delegating · C-c P"
                                 (get-text-property 0 'display rendered))))
        (let* ((other (+agent-shell-pstack-panel--parse block))
               (other-id (+agent-shell-pstack-panel--id "other-session" block)))
          (with-temp-buffer
            (setq-local agent-shell--state
                        '((:session . ((:id . "other-session")))))
            (setf (+agent-shell-pstack-panel-id other) other-id
                  (+agent-shell-pstack-panel-lead other) (current-buffer))
            (puthash other-id other +agent-shell-pstack-panel--panels)
            (let ((rendered (agent-shell-markdown-convert source)))
              (should-not (string-search
                           "Panel " (get-text-property 0 'display rendered))))
            (insert source)
            (+agent-shell-pstack-card-refresh)
            (should (string-search
                     other-id (get-text-property (point-min) 'display)))))))))

(ert-deftest +agent-shell-pstack-card-test-id-collision ()
  (+agent-shell-pstack-card-test--with
    (let* ((source (+agent-shell-pstack-card-test--source "delegate"))
           (block (+agent-shell-pstack-panel--block source))
           (short-id (+agent-shell-pstack-panel--id "session" block))
           (digest (secure-hash 'sha256 (concat "session\0" block)))
           (panel (+agent-shell-pstack-panel--parse block)))
      (puthash short-id "other request" +agent-shell-pstack-panel--request-digests)
      (setf (+agent-shell-pstack-panel-id panel) digest)
      (puthash digest panel +agent-shell-pstack-panel--panels)
      (with-temp-buffer
        (setq-local agent-shell--state '((:session . ((:id . "session")))))
        (insert source)
        (+agent-shell-pstack-card-refresh)
        (should (string-search digest (get-text-property (point-min) 'display)))))))

(ert-deftest +agent-shell-pstack-card-test-incomplete-and-other-language ()
  (+agent-shell-pstack-card-test--with
    (with-temp-buffer
      (insert "```pstack-panel\n{}\n")
      (agent-shell-markdown-replace-markup)
      (+agent-shell-pstack-card-refresh)
      (should-not (text-property-not-all
                   (point-min) (point-max) '+agent-shell-pstack-card-source nil))
      (goto-char (point-max))
      (insert "```\n")
      (agent-shell-markdown-replace-markup :complete t)
      (should (get-text-property (point-min) '+agent-shell-pstack-card-source)))
    (with-temp-buffer
      (insert (agent-shell-markdown-convert "```json\n{}\n```\n"))
      (+agent-shell-pstack-card-refresh)
      (should-not (text-property-not-all
                   (point-min) (point-max) '+agent-shell-pstack-card-source nil)))))

(ert-deftest +agent-shell-pstack-card-test-refresh-existing ()
  (+agent-shell-pstack-card-test--with
    (let* ((source (+agent-shell-pstack-card-test--source "how"))
           (rendered (let ((agent-shell-markdown-render-functions nil))
                       (agent-shell-markdown-convert source))))
      (with-temp-buffer
        (insert rendered)
        (+agent-shell-pstack-card-refresh)
        (let ((pos (text-property-not-all
                    (point-min) (point-max) '+agent-shell-pstack-card-source nil)))
          (should pos)
          (should (string-search "pstack how" (get-text-property pos 'display)))
          (goto-char pos)
          (+agent-shell-pstack-card-toggle-source)
          (should-not (get-text-property pos 'display))
          (+agent-shell-pstack-card-toggle-source)
          (should (string-search "pstack how" (get-text-property pos 'display))))
        (should (equal source (agent-shell-markdown-reconstruct
                               (point-min) (point-max))))))))

(ert-deftest +agent-shell-pstack-card-test-mode-and-bindings ()
  (let ((agent-shell-markdown-render-functions nil)
        (+agent-shell-pstack-panel-change-hook nil)
        (+agent-shell-pstack-card-mode nil))
    (unwind-protect
        (progn
          (eval (+config-test-declaration "modules/mod-ai.el"
                                          '+agent-shell-pstack-card) t)
          (should (memq #'+agent-shell-pstack-card--render-hook
                        agent-shell-markdown-render-functions))
          (should (memq #'+agent-shell-pstack-card--update-buffers
                        +agent-shell-pstack-panel-change-hook))
          (should (eq #'+agent-shell-pstack-card-toggle-source
                      (lookup-key +agent-shell-pstack-card-map (kbd "RET"))))
          (should (eq #'+agent-shell-pstack-card-toggle-source
                      (lookup-key +agent-shell-pstack-card-map [mouse-1])))
          (should (eq #'+agent-shell-pstack-card-toggle-brief
                      (lookup-key +agent-shell-pstack-card-map (kbd "TAB"))))
          (+agent-shell-pstack-card-test--with
            (with-temp-buffer
              (insert (agent-shell-markdown-convert
                       (+agent-shell-pstack-card-test--source "how")))
              (+agent-shell-pstack-card-mode -1)
              (should-not (get-text-property (point-min) 'display))
              (should-not (get-text-property (point-min) 'keymap))
              (+agent-shell-pstack-card-mode 1)
              (+agent-shell-pstack-card-refresh)
              (should (get-text-property (point-min) 'display)))))
      (+agent-shell-pstack-card-mode -1))
    (should-not agent-shell-markdown-render-functions)
    (should-not +agent-shell-pstack-panel-change-hook)))

(provide 'agent-shell-pstack-card-test)
;;; agent-shell-pstack-card-test.el ends here
