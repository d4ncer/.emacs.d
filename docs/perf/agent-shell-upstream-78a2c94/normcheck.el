;;; normcheck.el --- Add blank-line-insensitive digests to bench label checks -*- lexical-binding: t; -*-
(defun +norm--squeeze (text)
  (replace-regexp-in-string "\n\\(?:[ \t]*\n\\)+" "\n\n" text))
(advice-add '+bench--label-checks :filter-return
            (lambda (result)
              (append
               result
               (list
                :norm-text-sha1
                (mapcar (lambda (shell)
                          (with-current-buffer shell
                            (sha1 (+norm--squeeze (buffer-substring-no-properties
                                                   (point-min) (point-max))))))
                        +norm--shells)
                :norm-label-sha1
                (mapcar (lambda (shell)
                          (with-current-buffer shell
                            (sha1 (prin1-to-string
                                   (mapcar (lambda (label)
                                             (list (+norm--squeeze
                                                    (buffer-substring-no-properties
                                                     (nth 0 label) (nth 1 label)))
                                                   (nth 2 label)))
                                           (+bench--labels shell))))))
                        +norm--shells)
                :label-count (mapcar (lambda (s) (length (+bench--labels s))) +norm--shells)))))
(defvar +norm--shells nil)
(advice-add '+bench--label-checks :before (lambda (shells) (setq +norm--shells shells)))
