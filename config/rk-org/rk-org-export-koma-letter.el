;;; rk-org-export-koma-letter.el --- Export Koma letter with C-c C-c.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Version: 0.1

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;; Adds a C-c C-c handler for exporting the Koma letter at point.

;;; Code:

(require 'f)
(require 'org)
(require 'ox-latex)
(require 'ox-koma-letter)

(defconst rk-org-export-koma-letter-latex-class "
\\documentclass[paper=A4,pagesize,fromalign=right,
               fromrule=aftername,fromphone,fromemail,
               version=last]{scrlttr2}
\\usepackage[utf8]{inputenc}
\\usepackage[normalem]{ulem}
\\usepackage{booktabs}
\\usepackage{graphicx}
[NO-DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]")

(defun rk-org-export-koma-letter--subtree-content ()
  "Return the content of the subtree at point as a string."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun rk-org-export-koma-letter--subtree-write-content (dest)
  "Write the contents of the subtree at point to a file at DEST."
  (interactive (list (read-file-name "Write subtree to: " nil nil nil ".org")))
  (f-write-text (rk-org-export-koma-letter--subtree-content) 'utf-8 dest)
  (when (called-interactively-p nil)
    (message "Subtree written to %s" dest)))

;;;###autoload
(defun rk-org-export-koma-letter-at-subtree (dest)
  "Define a command to export the koma letter subtree at point to PDF.
With a prefix arg, prompt for the output destination. Otherwise
generate use the name of the current file to generate the
exported file's name. The PDF will be created at DEST."
  (interactive
   (list (if current-prefix-arg
             (ido-read-file-name "Destination: " nil nil nil ".pdf")
           (concat (f-no-ext (buffer-file-name)) ".pdf"))))

  (let ((tmpfile (make-temp-file "org-export-" nil ".org")))
    (rk-org-export-koma-letter--subtree-write-content tmpfile)
    (with-current-buffer (find-file-noselect tmpfile)
      (unwind-protect
          (-if-let (exported (org-koma-letter-export-to-pdf))
              (f-move exported dest)
            (error "Export failed"))
        (kill-buffer)))
    (start-process " open" nil "open" dest)
    (message "opening %s..." dest)))

(defun rk-org-export-koma-letter--handler ()
  "Export the koma letter at point."
  (when (ignore-errors
          (s-matches? (rx "latex_class:" (* space) "koma")
                      (rk-org-export-koma-letter--subtree-content)))
    (call-interactively 'rk-org-export-koma-letter-at-subtree)
    'export-koma-letter))

(provide 'rk-org-export-koma-letter)

;;; rk-org-export-koma-letter.el ends here
