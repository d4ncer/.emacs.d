;; -*- lexical-binding: t; -*-

(defun +roam-node-find (&optional other-window)
  "Find an org-roam node. See `org-roam-node-find'.

With optional prefix arg OTHER-WINDOW, visit the node in another
window."
  (interactive "P")
  (org-roam-node-find other-window
                      nil
                      (lambda (node)
                        (let* ((tags (org-roam-node-tags node))
                               (disallowed (flatten-list (list '("daily")
                                                               (when (and (bound-and-true-p timekeep-mode)
                                                                          (org-clocking-p))
                                                                 "private")))))
                          (null (seq-intersection tags disallowed))))))

(provide '+roam)
