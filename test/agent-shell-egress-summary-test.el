;;; agent-shell-egress-summary-test.el --- Egress review tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the view and both exports against records on disk.

;;; Code:

(require 'ert)
(require '+agent-shell-egress-summary)
(require 'agent-shell-egress-test)

(defun +agent-shell-egress-summary-test--record
    (directory panel role agent repository backend state day &optional names ip)
  "Write a fixture in DIRECTORY for PANEL, ROLE, AGENT and REPOSITORY.
Use BACKEND, STATE, DAY, NAMES and IP for its observation."
  (let* ((file (expand-file-name (format "%s/%s.egress.json" panel role) directory))
         (start (format "2026-01-%02dT12:00:00.000Z" day))
         (end (format "2026-01-%02dT13:00:00.000Z" day))
         (record
          (+agent-shell-egress--make-record
           :identity (append (list :panel panel :arena nil :role role :kind 'candidate)
                             (when agent (list :agent agent))
                             (when repository (list :repository repository)))
           :backend backend :state state :started-at start :ended-at end
           :destinations
           (unless (memq state '(unavailable not-started))
             (list (+agent-shell-egress--make-destination
                    :ip (or ip "192.0.2.1") :port 443 :protocol 'tcp :names names
                    :first-seen start :last-seen end :count 1
                    :source (if (eq backend 'macos-lsof) 'lsof 'tshark)))))))
    (should (+agent-shell-egress--write record file))
    file))

(defmacro +agent-shell-egress-summary-test--with (&rest body)
  "Run BODY against several backend fixtures in `directory'."
  (declare (indent 0))
  `(+agent-shell-egress-test--with
     (+agent-shell-egress-summary-test--record
      directory "p1" "alpha" "codex" "~/src/example" 'linux-tshark 'finished 1
      '(("api.example.com" . dns) ("api.example.com" . sni)))
     (+agent-shell-egress-summary-test--record
      directory "p1" "beta" "claude-code" "~/src/example" 'linux-tshark 'partial 2
      '(("api.example.com" . sni)) "192.0.2.2")
     (+agent-shell-egress-summary-test--record
      directory "p2" "alpha" "codex" "~/src/other" 'linux-tshark 'partial 3
      '(("api.example.com" . dns)) "192.0.2.3")
     (+agent-shell-egress-summary-test--record
      directory "p2" "beta" "claude-code" "~/src/other" 'macos-lsof 'partial 3)
     (+agent-shell-egress-summary-test--record
      directory "p3" "alpha" "codex" "~/src/example" 'macos-lsof 'unavailable 4)
     (+agent-shell-egress-summary-test--record
      directory "p3" "judge" "codex" "~/src/example" nil 'not-started 4)
     ,@body))

(ert-deftest +agent-shell-egress-summary-test-aggregate ()
  "Merge host names across addresses, members and panels without double counting."
  (+agent-shell-egress-summary-test--with
    (let* ((data (+agent-shell-egress-summary--collect directory))
           (rows (plist-get data :rows))
           (host (seq-find (lambda (row) (equal (alist-get 'destination row)
                                               "api.example.com")) rows)))
      (should (= 6 (plist-get data :records)))
      (should (= 2 (length rows)))
      (should (equal ["claude-code" "codex"] (alist-get 'agents host)))
      (should (equal ["~/src/example" "~/src/other"]
                     (alist-get 'repositories host)))
      (should (equal ["dns" "sni"] (alist-get 'observed host)))
      (should (= 2 (alist-get 'panels host)))
      (should (= 3 (alist-get 'members host)))
      (should (= 3 (alist-get 'records host)))
      (should (= 2 (alist-get 'partial host)))
      (should (= 0 (alist-get 'unavailable host)))
      (should (equal "2026-01-01T12:00:00.000Z" (alist-get 'first_seen host)))
      (should (equal "2026-01-03T13:00:00.000Z" (alist-get 'last_seen host)))
      (should (= 3 (gethash 'partial (plist-get data :counts))))
      (should (= 1 (gethash 'unavailable (plist-get data :counts))))
      (should (= 1 (gethash 'not-started (plist-get data :counts))))
      (should (member "linux-tshark: 3 named, 0 addresses only, 0 empty"
                      (plist-get data :coverage)))
      (should (member "macos-lsof: 0 named, 1 addresses only, 1 empty"
                      (plist-get data :coverage)))
      (should (equal ["lsof"] (alist-get 'observed (car rows)))))))

(ert-deftest +agent-shell-egress-summary-test-date ()
  "Filter records by their start date, inclusively at UTC midnight."
  (+agent-shell-egress-summary-test--with
    (let* ((data (+agent-shell-egress-summary--collect
                  directory (+agent-shell-egress-summary--date "2026-01-03")))
           (host (cadr (plist-get data :rows))))
      (should (= 4 (plist-get data :records)))
      (should (= 2 (plist-get data :excluded)))
      (should (= 1 (alist-get 'members host)))
      (should (equal ["codex"] (alist-get 'agents host))))
    (should-not (plist-get (+agent-shell-egress-summary--collect
                           directory (+agent-shell-egress-summary--date "2027-01-01"))
                          :rows))
    (dolist (date '("2026-02-30" "2026-13-01" "yesterday" "2026-1-1"))
      (should-error (+agent-shell-egress-summary--date date) :type 'user-error))))

(ert-deftest +agent-shell-egress-summary-test-identity ()
  "Round-trip new identity fields and continue reading version one."
  (+agent-shell-egress-test--with
    (let* ((file (+agent-shell-egress-summary-test--record
                  directory "p1" "alpha" "codex" "~/src/example"
                  'linux-tshark 'finished 1))
           (record (+agent-shell-egress-read file)))
      (should (equal "codex" (plist-get (+agent-shell-egress-record-identity record)
                                             :agent)))
      (should (equal "~/src/example"
                     (plist-get (+agent-shell-egress-record-identity record)
                                :repository)))
      (let ((object (copy-tree (+agent-shell-egress--json record))))
        (setf (alist-get 'version object) 1)
        (setq object (assq-delete-all 'repository (assq-delete-all 'agent object)))
        (with-temp-file file (insert (json-serialize object))))
      (should-not (plist-get (+agent-shell-egress-record-identity
                              (+agent-shell-egress-read file)) :agent))
      (let ((row (car (plist-get (+agent-shell-egress-summary--collect directory)
                                 :rows))))
        (should (equal ["unknown"] (alist-get 'agents row)))
        (should (equal ["unknown"] (alist-get 'repositories row))))
      ;; A sibling with structured identity can recover the panel's repository.
      (+agent-shell-egress-summary-test--record
       directory "p1" "beta" "claude-code" "~/src/example" 'macos-lsof 'partial 2)
      (let ((rows (plist-get (+agent-shell-egress-summary--collect directory) :rows)))
        (should (seq-every-p (lambda (row)
                              (equal ["~/src/example"]
                                     (alist-get 'repositories row))) rows)))
      (let ((object (copy-tree (+agent-shell-egress--json record))))
        (dolist (bad '("https://example.com/token" "~/src/example\nsecret"))
          (setf (alist-get 'repository object) bad)
          (should-error (+agent-shell-egress--parse object)
                        :type '+agent-shell-egress-invalid))))))

(ert-deftest +agent-shell-egress-summary-test-panel-identity ()
  "Use the agent identifier and manifest top level, including for nested cwd."
  (require '+agent-shell-pstack-panel)
  (let* ((member (+agent-shell-pstack-panel--make-member
                  :role "review" :agent 'codex :kind 'candidate))
         (panel (+agent-shell-pstack-panel--make
                 :id "p1" :cwd "/tmp/example/subdir/"
                 :members (list member)
                 :arena (+agent-shell-arena-git--manifest
                         :id "a1" :toplevel "/tmp/example/")))
         (identity (+agent-shell-pstack-panel--egress-identity panel member))
         (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal)))
    (should (equal "codex" (plist-get identity :agent)))
    (should (equal "/tmp/example" (plist-get identity :repository)))
    (puthash "p1" panel +agent-shell-pstack-panel--panels)
    (should (equal identity
                   (+agent-shell-egress-summary--live-identity
                    '(:panel "p1" :role "review"))))))

(ert-deftest +agent-shell-egress-summary-test-dns-and-invalid ()
  "Do not describe DNS questions as hosts reached, or hide malformed records."
  (+agent-shell-egress-test--with
    (let* ((file (+agent-shell-egress-summary-test--record
                  directory "p1" "alpha" "codex" "~/src/example"
                  'linux-tshark 'finished 1 '(("api.example.com" . dns))))
           (record (+agent-shell-egress-read file))
           (destination (car (+agent-shell-egress-record-destinations record))))
      (setf (+agent-shell-egress-destination-protocol destination) 'dns
            (+agent-shell-egress-destination-port destination) 53)
      (+agent-shell-egress--write record file)
      (with-temp-file (expand-file-name "bad.egress.json" directory) (insert "bad"))
      (let* ((data (+agent-shell-egress-summary--collect directory))
             (row (car (plist-get data :rows))))
        (should (= 1 (plist-get data :invalid)))
        (should (equal "192.0.2.1" (alist-get 'destination row)))
        (should (= 53 (alist-get 'port row)))
        (should (equal ["dns"] (alist-get 'observed row)))))))

(ert-deftest +agent-shell-egress-summary-test-view-and-export ()
  "Render a read-only view and export exactly its rows, with CSV escaping."
  (+agent-shell-egress-summary-test--with
    (let ((+agent-shell-pstack-panel-directory directory))
      (unwind-protect
          (save-window-excursion
            (+agent-shell-egress-summary)
            (with-current-buffer "*Agent shell egress*"
              (should buffer-read-only)
              (should (derived-mode-p 'special-mode))
              (should (string-search "3 partial, 1 unavailable" (buffer-string)))
              (should (string-search "linux-tshark: 3 named" (buffer-string)))
              (let ((csv (expand-file-name "summary.csv" directory))
                    (json (expand-file-name "summary.json" directory))
                    (rows (plist-get +agent-shell-egress-summary--data :rows)))
                (+agent-shell-egress-summary-export json 'json)
                (with-temp-buffer
                  (insert-file-contents json)
                  (should (equal rows (append (json-parse-buffer :object-type 'alist)
                                               nil))))
                (should (= #o600 (file-modes json)))
                (+agent-shell-egress-summary-export csv 'csv)
                (let ((text (+agent-shell-egress-test--text csv)))
                  (should (= 3 (length (split-string text "\n" t))))
                  (should (string-search "\"api.example.com\",\"443\",\"tcp\"" text)))
                (setf (alist-get 'repositories (car rows)) ["/tmp/example,\"quoted\""])
                (+agent-shell-egress-summary-export csv 'csv)
                (should (string-search "\"/tmp/example,\"\"quoted\"\"\""
                                       (+agent-shell-egress-test--text csv))))
              (+agent-shell-egress-summary "2026-01-03")
              (should (= 4 (plist-get +agent-shell-egress-summary--data :records)))
              (revert-buffer)
              (should (= 4 (plist-get +agent-shell-egress-summary--data :records)))))
        (when-let* ((buffer (get-buffer "*Agent shell egress*")))
          (kill-buffer buffer))))))

(provide 'agent-shell-egress-summary-test)
;;; agent-shell-egress-summary-test.el ends here
