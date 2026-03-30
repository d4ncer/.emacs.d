;;; +org-format.el --- Auto-format org buffers -*- lexical-binding: t; -*-

;; Adapted from: https://github.com/chrisbarrett/nursery

;;; Commentary:
;; Automatically format org-mode buffers on save.
;; Normalises blank lines around headings, aligns tables,
;; and cleans up trailing whitespace.

;;; Code:

(require 'org)

(defgroup +org-format nil
  "Automatically format org buffers on save."
  :group 'productivity
  :prefix "+org-format-")

(defcustom +org-format-blank-lines-before-subheadings 1
  "Number of blank lines between a heading and preceding content.
Only applies to subheadings."
  :group '+org-format
  :type 'integer)

(defcustom +org-format-blank-lines-before-first-heading 1
  "Number of blank lines between a heading and preceding content.
Only applies to the first level-1 heading in the document."
  :group '+org-format
  :type 'integer)

(defcustom +org-format-blank-lines-before-level-1-headings 1
  "Number of blank lines between a heading and preceding content.
Only applies to level-1 headings in the document."
  :group '+org-format
  :type 'integer)

(defcustom +org-format-blank-lines-before-content 0
  "Number of blank lines after the heading line and any property drawers."
  :group '+org-format
  :type 'integer)

(defcustom +org-format-blank-lines-before-meta 0
  "Number of blank lines between headers and subsequent planning & drawers."
  :group '+org-format
  :type 'integer)

(defcustom +org-format-align-all-tables t
  "Whether to align tables on save."
  :group '+org-format
  :type 'boolean)

(defun +org-format--ensure-empty-lines (n)
  "Ensure there are N blank lines before the current line."
  (save-excursion
    (goto-char (line-beginning-position))
    (unless (bobp)
      (forward-char -1)
      (let ((start (point)))
        (when (search-backward-regexp (rx (not (any space "\n"))))
          (ignore-errors
            (forward-char 1)
            (delete-region (point) start))))
      (insert (make-string n ?\n)))))

(defun +org-format--in-archived-heading-p ()
  "Return non-nil if point is within an archived heading."
  (save-excursion
    (when (org-before-first-heading-p)
      (org-forward-heading-same-level 1))
    (let ((tags (org-get-tags)))
      (seq-contains-p tags org-archive-tag))))

(defun +org-format--delete-blank-lines ()
  "Delete blank lines around point."
  (beginning-of-line)
  (when (looking-at "[ \t]*$")
    (delete-region (point)
                   (if (re-search-backward "[^ \t\n]" nil t)
                       (progn (forward-line 1) (point))
                     (point-min))))
  (when (looking-at "^[ \t]*\n\\'")
    (delete-region (point) (point-max))))

(defun +org-format--headings (scope)
  "Format headings in SCOPE."
  (let ((seen-first-heading-p))
    (org-map-entries (lambda ()
                       (org-with-wide-buffer
                        (let* ((level (car (org-heading-components)))
                               (headline-spacing (cond
                                                  ((and (equal 1 level) (not seen-first-heading-p))
                                                   (setq seen-first-heading-p t)
                                                   +org-format-blank-lines-before-first-heading)
                                                  ((equal 1 level)
                                                   +org-format-blank-lines-before-level-1-headings)
                                                  (t
                                                   +org-format-blank-lines-before-subheadings))))
                          (+org-format--ensure-empty-lines headline-spacing)))

                       (unless (and (fboundp 'org-transclusion-within-transclusion-p)
                                    (org-transclusion-within-transclusion-p))
                         (forward-line 1)
                         (+org-format--delete-blank-lines)
                         (+org-format--ensure-empty-lines +org-format-blank-lines-before-meta)
                         (org-end-of-meta-data t)
                         (+org-format--ensure-empty-lines +org-format-blank-lines-before-content)))
                     t
                     scope)))

;;;###autoload
(defun +org-format-buffer ()
  "Format the current `org-mode' buffer."
  (interactive)
  (when (and (derived-mode-p 'org-mode)
             (not (bound-and-true-p org-capture-mode)))
    (let ((scope (when (+org-format--in-archived-heading-p)
                   'tree)))
      (org-with-wide-buffer
       (when +org-format-align-all-tables
         (org-table-map-tables #'org-table-align t))
       (+org-format--headings scope)
       (goto-char (point-max))
       (+org-format--delete-blank-lines)))))

(defvar +org-format-on-save-mode-hook-depth 95
  "Depth for the `before-save-hook' entry.")

;;;###autoload
(define-minor-mode +org-format-on-save-mode
  "Minor mode to enable formatting on buffer save in `org-mode'."
  :lighter nil
  (cond
   (+org-format-on-save-mode
    (add-hook 'before-save-hook #'+org-format-buffer +org-format-on-save-mode-hook-depth t))
   (t
    (remove-hook 'before-save-hook #'+org-format-buffer t))))

(provide '+org-format)
;;; +org-format.el ends here
