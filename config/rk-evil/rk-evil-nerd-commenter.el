;;; rk-evil-nerd-commenter.el --- Supporting functions for evil-nerd-commenter.

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(autoload 'evilnc-comment-or-uncomment-lines "evil-nerd-commenter")
(autoload 'evilnc-comment-or-uncomment-paragraphs "evil-nerd-commenter")
(autoload 'evilnc-comment-or-uncomment-to-the-line "evil-nerd-commenter")
(autoload 'evilnc-copy-and-comment-lines "evil-nerd-commenter")


(defun rk-evil-nerd-commenter/comment-or-uncomment-lines-inverse (&optional arg)
  "Comment or uncomment lines.
ARG is passed to `evilnc-comment-or-uncomment-lines'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-lines arg)))

(defun rk-evil-nerd-commenter/comment-or-uncomment-lines (&optional arg)
  "Comment or uncomment lines.
ARG is passed to `evilnc-comment-or-uncomment-lines'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-lines arg)))

(defun rk-evil-nerd-commenter/copy-and-comment-lines-inverse (&optional arg)
  "Copy and comment lines.
ARG is passed to `evilnc-copy-and-comment-lines'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-copy-and-comment-lines arg)))

(defun rk-evil-nerd-commenter/copy-and-comment-lines (&optional arg)
  "Copy and comment lines.
ARG is passed to `evilnc-copy-and-comment-lines'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-copy-and-comment-lines arg)))

(defun rk-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line-inverse (&optional arg)
  "Comment or uncomment lines.
ARG is passed to `evilnc-comment-or-uncomment-to-the-line'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-to-the-line arg)))

(defun rk-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line (&optional arg)
  "Comment or uncomment lines.
ARG is passed to `evilnc-comment-or-uncomment-to-the-line'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-to-the-line arg)))

(defun rk-evil-nerd-commenter/comment-or-uncomment-paragraphs-inverse (&optional arg)
  "Comment or uncomment paragraphs.
ARG is passed to `evilnc-comment-or-uncomment-paragraphs-inverse'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-paragraphs arg)))

(defun rk-evil-nerd-commenter/comment-or-uncomment-paragraphs (&optional arg)
  "Comment or uncomment paragraphs.
ARG is passed to `evilnc-comment-or-uncomment-paragraphs'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-paragraphs arg)))

(provide 'rk-evil-nerd-commenter)

;;; rk-evil-nerd-commenter.el ends here
