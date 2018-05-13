;;; rk-org-capture-url.el --- Utilities for capturing URLs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)

(autoload 'thing-at-point-url-at-point "thingatpt")

(defun rk-org-capture-url--parse-html-title (html)
  "Extract the title from an HTML document."
  (-let (((_ title) (s-match (rx "<title>" (group (* nonl)) "</title>") html))
         ((_ charset) (-map 'intern (s-match (rx "charset=" (group (+ (any "-" alnum)))) html))))
    (if (-contains? coding-system-list charset)
        (decode-coding-string title charset)
      title)))

(defun rk-org-capture-url--retrieve-html (url)
  "Download the resource at URL and attempt to extract an HTML title."
  (unless (s-matches? (rx "." (or "pdf" "mov" "mp4" "m4v" "aiff" "wav" "mp3") eol) url)
    (with-current-buffer (url-retrieve-synchronously url t)
      (buffer-string))))

(defun rk-org-capture-url--last-url-kill ()
  "Return the most recent URL in the kill ring or X pasteboard."
  (--first (s-matches? (rx bos (or "http" "https" "www")) it)
           (cons (current-kill 0 t) kill-ring)))

(defun rk-org-capture-url--read-string-with-default (prompt default &optional initial-input history)
  (read-string (concat (if default (format "%s (default %s)" prompt default) prompt) ": ")
               initial-input history default))

(defun rk-org-capture-url-read-url ()
  "Return a URL capture template string for use with `org-capture'."
  (let* ((default (or (thing-at-point-url-at-point) (rk-org-capture-url--last-url-kill)))
         (url (rk-org-capture-url--read-string-with-default "URL" default))
         (title (rk-org-capture-url--parse-html-title (rk-org-capture-url--retrieve-html url))))
    (format "* [[%s][%s]]" url (or title url))))

(provide 'rk-org-capture-url)

;;; rk-org-capture-url.el ends here
