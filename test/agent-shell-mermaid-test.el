;;; agent-shell-mermaid-test.el --- Mermaid tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the markdown pipeline and asynchronous renderer with Mermaid CLI.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-markdown)
(require '+agent-shell-mermaid nil t)

(defmacro +agent-shell-mermaid-test--isolated (&rest body)
  "Run BODY with isolated rendering state and cache."
  (declare (indent 0))
  `(let ((+agent-shell-mermaid-cache-directory
          (make-temp-file "mermaid-test-" t))
         (+agent-shell-mermaid--jobs (make-hash-table :test #'equal))
         (+agent-shell-mermaid--failures (make-hash-table :test #'equal))
         (+agent-shell-mermaid-mode t)
         (agent-shell-markdown-render-functions
          '(+agent-shell-mermaid--render-hook)))
     (unwind-protect
         (progn ,@body)
       (maphash (lambda (_key proc) (delete-process proc))
                +agent-shell-mermaid--jobs)
       (delete-directory +agent-shell-mermaid-cache-directory t))))

(defun +agent-shell-mermaid-test--wait ()
  "Wait up to thirty seconds for render jobs to finish."
  (let ((deadline (+ (float-time) 30)))
    (while (and (> (hash-table-count +agent-shell-mermaid--jobs) 0)
                (< (float-time) deadline))
      (accept-process-output nil 0.1)))
  (should (= 0 (hash-table-count +agent-shell-mermaid--jobs))))

(ert-deftest +agent-shell-mermaid-test-copied-response-and-cache ()
  "Render a copied response after its temporary work buffer has died."
  (should (fboundp '+agent-shell-mermaid--render-hook))
  (+agent-shell-mermaid-test--isolated
    (let ((source "```mermaid\nflowchart TD\nA --> B\n```\n"))
      (with-temp-buffer
        (insert (agent-shell-markdown-convert source))
        (should (equal source
                       (agent-shell-markdown-reconstruct (point-min) (point-max))))
        (+agent-shell-mermaid-test--wait)
        (should (eq 'image (car (get-text-property (point-min) 'display))))
        (should (equal source
                       (agent-shell-markdown-reconstruct (point-min) (point-max))))
        ;; Identical content renders immediately from the disk cache.
        (with-temp-buffer
          (insert (agent-shell-markdown-convert source))
          (should (eq 'image (car (get-text-property (point-min) 'display))))
          (should (= 0 (hash-table-count +agent-shell-mermaid--jobs))))))))

(ert-deftest +agent-shell-mermaid-test-incomplete-fence ()
  "Leave a streaming Mermaid block alone until its closing fence arrives."
  (should (fboundp '+agent-shell-mermaid--render-hook))
  (+agent-shell-mermaid-test--isolated
    (with-temp-buffer
      (insert "```mermaid\nflowchart TD\nA --> B\n")
      (agent-shell-markdown-replace-markup)
      (should-not (get-text-property (point-min) '+agent-shell-mermaid-source))
      (should (= 0 (hash-table-count +agent-shell-mermaid--jobs)))
      (goto-char (point-max))
      (insert "```\n")
      (agent-shell-markdown-replace-markup :complete t)
      (+agent-shell-mermaid-test--wait)
      (should (eq 'image (car (get-text-property (point-min) 'display)))))))

(ert-deftest +agent-shell-mermaid-test-existing-block ()
  "Refresh source blocks whose fences agent-shell has already removed."
  (should (fboundp '+agent-shell-mermaid-refresh))
  (+agent-shell-mermaid-test--isolated
    (let* ((source "```mermaid\nflowchart LR\nA --> B\n```\n")
           (rendered (let ((agent-shell-markdown-render-functions nil))
                       (agent-shell-markdown-convert source))))
      (with-temp-buffer
        (insert rendered)
        (+agent-shell-mermaid-refresh)
        (+agent-shell-mermaid-test--wait)
        (let ((pos (text-property-not-all
                    (point-min) (point-max) '+agent-shell-mermaid-source nil)))
          (should pos)
          (should (equal (get-text-property pos '+agent-shell-mermaid-source)
                         "flowchart LR\nA --> B")))
        (should (text-property-not-all (point-min) (point-max) 'display nil))
        (should (equal source
                       (agent-shell-markdown-reconstruct (point-min) (point-max))))))))

(ert-deftest +agent-shell-mermaid-test-failed-render ()
  "Keep invalid Mermaid source readable and avoid retrying every chunk."
  (should (fboundp '+agent-shell-mermaid--render-hook))
  (+agent-shell-mermaid-test--isolated
    (let ((source "```mermaid\nnot-a-diagram\n```\n"))
      (with-temp-buffer
        (insert (agent-shell-markdown-convert source))
        (+agent-shell-mermaid-test--wait)
        (should-not (get-text-property (point-min) 'display))
        (should (equal source (buffer-substring-no-properties
                               (point-min) (point-max))))
        (should (= 1 (hash-table-count +agent-shell-mermaid--failures)))
        (agent-shell-markdown-convert source)
        (should (= 0 (hash-table-count +agent-shell-mermaid--jobs)))))))


(ert-deftest +agent-shell-mermaid-test-open-rendered-image ()
  "Open a copied inline diagram with RET or a mouse click."
  (let ((file (make-temp-file "mermaid-open-" nil ".png"))
        (source "flowchart TD\nA --> B")
        image-buffer)
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (base64-decode-string
                     (concat "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1Pe"
                             "AAAADElEQVR4nGP4//8/AAX+Av4N70a4AAAAAElFTkSuQmCC")))
            (write-region (point-min) (point-max) file nil 'silent))
          (with-temp-buffer
            (insert source)
            (+agent-shell-mermaid--display (point-min) (point-max) file)
            (let ((rendered (buffer-string)))
              (with-temp-buffer
                (insert rendered)
                (goto-char (point-min))
                (let* ((map (get-text-property (point) 'keymap))
                       (open (and map (lookup-key map (kbd "RET")))))
                  (should (commandp open))
                  (should (eq open (lookup-key map [mouse-1])))
                  (let ((auto-mode-alist
                         (if (display-images-p)
                             auto-mode-alist
                           (cons '("\\.png\\'" . fundamental-mode)
                                 auto-mode-alist))))
                    (save-window-excursion
                      (call-interactively open)
                      (setq image-buffer (window-buffer (selected-window)))
                      (with-current-buffer image-buffer
                        (should (file-equal-p buffer-file-name file))
                        (when (display-images-p)
                          (should (eq major-mode 'image-mode)))))))))
            (should (equal source (buffer-substring-no-properties
                                   (point-min) (point-max))))))
      (when (buffer-live-p image-buffer) (kill-buffer image-buffer))
      (delete-file file))))

(provide 'agent-shell-mermaid-test)
;;; agent-shell-mermaid-test.el ends here
