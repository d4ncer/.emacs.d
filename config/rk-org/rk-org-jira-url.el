;;; rk-org-jira-url.el --- Utilities for creating JIRA URLs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(autoload 's-upcase "s")
(autoload 'org-insert-link "org")

(defconst rk-org--jira-base-url-template
  "https://moviohq.atlassian.net/browse/%s")

(defun rk-org--create-jira-url (ticket project)
  "Create a URL for TICKET, scoped to the Green project by default, unless PROJECT is specified."
  (interactive (let ((ticket (read-string "Ticket number: " nil))
                     (project (read-string "Project: " "Green")))
                 (list ticket project)))
  (let ((ticket-and-project (format "%s-%s" (s-upcase project) ticket)))
    (org-insert-link nil (format rk-org--jira-base-url-template ticket-and-project) nil)))

(provide 'rk-org-jira-url)

;;; rk-org-jira-url.el ends here
