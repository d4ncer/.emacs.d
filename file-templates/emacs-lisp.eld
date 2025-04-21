(nil
 '(progn
    (setq file-name (file-name-nondirectory (buffer-file-name)))
    (setq feature-name (file-name-sans-extension file-name)))

 ";;; " file-name " --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

" _ "

(provide '" feature-name ")

;;; " file-name " ends here"


 '(progn
    (makunbound 'file-name)
    (makunbound 'feature-name)))
