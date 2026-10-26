;;; agent-shell-pstack-arena-trace-test.el --- Tests for arena traces -*- lexical-binding: t; -*-

;;; Commentary:
;; The report parser and summarizer run on captured log stream lines.  The
;; command tests trace real commands in scratch repositories under
;; sandbox-exec and skip off macOS.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-arena-git-test)
(require '+agent-shell-pstack-arena-trace)

(defconst +arena-trace-test--lines
  '("Filtering the log data using \"sender == \"Sandbox\" AND composedMessage \
ENDSWITH \"pa-trace-1234\"\""
    "{\"timezoneName\":\"\",\"messageType\":\"Default\",\"eventType\":\"logEvent\",\"source\":null,\"formatString\":\"%s\",\"userID\":0,\"activityIdentifier\":0,\"subsystem\":\"\",\"category\":\"\",\"threadID\":66343845,\"senderImageUUID\":\"CAB6E5F0-8492-3DED-B04B-66D2CC8E3C61\",\"backtrace\":{\"frames\":[{\"imageOffset\":168600,\"imageUUID\":\"CAB6E5F0-8492-3DED-B04B-66D2CC8E3C61\"}]},\"bootUUID\":\"\",\"processImagePath\":\"\\/kernel\",\"senderImagePath\":\"\\/System\\/Library\\/Extensions\\/Sandbox.kext\\/Contents\\/MacOS\\/Sandbox\",\"timestamp\":\"2026-10-26 14:01:29.753792+1300\",\"machTimestamp\":37780632483278,\"eventMessage\":\"Sandbox: touch(61311) allow file-write-create \\/private\\/tmp\\/pa-probe-x\\npa-trace-1234\",\"processImageUUID\":\"D069070E-9313-3103-9083-B78E4737AA20\",\"traceID\":17575040253956,\"processID\":0,\"senderProgramCounter\":168600,\"parentActivityIdentifier\":0}"
    "{\"eventMessage\":\"Sandbox: cat(61312) allow file-read-data \\/private\\/tmp\\/pa-probe-secret\\/f\\npa-trace-1234\",\"processID\":0}"
    "{\"eventMessage\":\"4 duplicate reports for Sandbox: cat(61314) allow file-read-data \\/private\\/tmp\\/pa-probe-secret\\/f\\npa-trace-1234\",\"processID\":0}"
    "{\"eventMessage\":\"Sandbox: touch(61313) allow file-write* \\/private\\/tmp\\/pa-probe-x\\npa-trace-1234\",\"processID\":0}"
    "{\"eventMessage\":\"Sandbox: Python(61036) allow file-write-create \\/private\\/tmp\\/a dir (1)\\/f\\npa-trace-1234\",\"processID\":0}"
    "{\"eventMessage\":\"Sandbox: touch(70000) allow file-write-create \\/private\\/tmp\\/other-run\\npa-trace-9999\",\"processID\":0}"
    "{\"eventMessage\":\"Sandbox: com.apple.WebKit.Networking(46745) deny(1) mach-lookup com.apple.diagnosticd\",\"processID\":0}"
    "{\"eventMessage\":\"Sandbox: git(61028) allow file-read-metadata \\/Users\\/me\\/work\",\"processID\":0}")
  "Log stream lines captured from a trace, with foreign lines mixed in.")

(ert-deftest +arena-trace-test-parse ()
  "Keep only this run's tagged sandbox reports, duplicates included."
  (should (equal '(("file-write-create" "/private/tmp/pa-probe-x" "touch")
                   ("file-read-data" "/private/tmp/pa-probe-secret/f" "cat")
                   ("file-read-data" "/private/tmp/pa-probe-secret/f" "cat")
                   ("file-write*" "/private/tmp/pa-probe-x" "touch")
                   ("file-write-create" "/private/tmp/a dir (1)/f" "Python"))
                 (delq nil (mapcar (lambda (line)
                                     (+agent-shell-pstack-arena-trace-parse
                                      line "pa-trace-1234"))
                                   +arena-trace-test--lines)))))

(defconst +arena-trace-test--denials
  '("{\"timezoneName\":\"\",\"messageType\":\"Error\",\"eventType\":\"logEvent\",\"source\":null,\"formatString\":\"%s\",\"userID\":0,\"activityIdentifier\":0,\"subsystem\":\"\",\"category\":\"\",\"threadID\":66965836,\"senderImageUUID\":\"CAB6E5F0-8492-3DED-B04B-66D2CC8E3C61\",\"backtrace\":{\"frames\":[{\"imageOffset\":168600,\"imageUUID\":\"CAB6E5F0-8492-3DED-B04B-66D2CC8E3C61\"}]},\"bootUUID\":\"E7C96E42-5EBE-4E3A-8A6E-A8D7C4B50751\",\"processImagePath\":\"\\/kernel\",\"senderImagePath\":\"\\/System\\/Library\\/Extensions\\/Sandbox.kext\\/Contents\\/MacOS\\/Sandbox\",\"timestamp\":\"2026-10-26 15:09:47.855003+1300\",\"machTimestamp\":37878985972077,\"eventMessage\":\"Sandbox: touch(81663) deny(1) file-write-create \\/private\\/tmp\\/pa-probe\\/denied-file\\npa-probe-tag-1\",\"processImageUUID\":\"D069070E-9313-3103-9083-B78E4737AA20\",\"traceID\":17575040258052,\"processID\":0,\"senderProgramCounter\":168600,\"parentActivityIdentifier\":0}"
    "{\"eventMessage\":\"Sandbox: bash(81664) deny(1) file-write-create \\/private\\/tmp\\/pa-probe\\/denied-2\\npa-probe-tag-1\",\"processID\":0}"
    "{\"eventMessage\":\"Sandbox: Python(82489) deny(1) network-outbound \\/private\\/tmp\\/pa-probe\\/t.sock\\npa-probe-tag-2\",\"processID\":0}"
    "{\"eventMessage\":\"Sandbox: com.apple.WebKit.Networking(46745) deny(1) mach-lookup com.apple.diagnosticd\",\"processID\":0}"
    "{\"eventMessage\":\"Sandbox: cat(81670) allow file-read-data \\/dev\\/null\\npa-probe-tag-1 done\",\"processID\":0}"
    "{\"count\":3,\"finished\":1}")
  "Log lines captured from enforced profiles, with foreign lines mixed in.
The first is verbatim log show --style ndjson output for a profile whose
deny rules carried (with message \"pa-probe-tag-1\"), and the next two
keep only the eventMessage of lines captured the same way.")

(ert-deftest +arena-trace-test-parse-denials ()
  "Keep only the refusals tagged with this run's tag."
  (should (equal '(("file-write-create" "/private/tmp/pa-probe/denied-file"
                    "touch")
                   ("file-write-create" "/private/tmp/pa-probe/denied-2"
                    "bash"))
                 (delq nil (mapcar (lambda (line)
                                     (+agent-shell-pstack-arena-trace-parse
                                      line "pa-probe-tag-1"))
                                   +arena-trace-test--denials))))
  (should (equal '(("network-outbound" "/private/tmp/pa-probe/t.sock"
                    "Python"))
                 (delq nil (mapcar (lambda (line)
                                     (+agent-shell-pstack-arena-trace-parse
                                      line "pa-probe-tag-2"))
                                   +arena-trace-test--denials)))))

(ert-deftest +arena-trace-test-summarize ()
  "Group by operation, count distinct paths, and collapse crowded folders
other than the home folder and its parents."
  (should
   (equal
    '(("file-read-data" ("/Users/me/lead/README" 1 ("cat")))
      ("file-write-create"
       ("/Users/me/a" 1 ("touch")) ("/Users/me/b" 1 ("touch"))
       ("/Users/me/c" 1 ("touch"))
       ("/private/tmp/go-build1/" 4 ("compile" "go"))
       ("/private/tmp/x" 1 ("python3"))))
    (+agent-shell-pstack-arena-trace-summarize
     '(("file-write-create" "/private/tmp/go-build1/b001/_pkg_.a" "compile")
       ("file-write-create" "/private/tmp/go-build1/b001/importcfg" "go")
       ("file-write-create" "/private/tmp/go-build1/b002/_pkg_.a" "compile")
       ("file-write-create" "/private/tmp/go-build1/b003/_pkg_.a" "compile")
       ("file-write-create" "/private/tmp/go-build1/b003/_pkg_.a" "go")
       ("file-write-create" "/private/tmp/x" "python3")
       ("file-write-create" "/Users/me/a" "touch")
       ("file-write-create" "/Users/me/b" "touch")
       ("file-write-create" "/Users/me/c" "touch")
       ("file-read-data" "/Users/me/lead/README" "cat")
       ("file-read-data" "/Users/me/lead/README" "cat"))
     "/Users/me")))
  (should (equal '(("file-write-create" ("/private/tmp/" 3 ("a" "b" "c"))))
                 (+agent-shell-pstack-arena-trace-summarize
                  '(("file-write-create" "/private/tmp/1" "a")
                    ("file-write-create" "/private/tmp/2" "b")
                    ("file-write-create" "/private/tmp/3" "c"))
                  "/Users/me"))))

;;;; Commands

(defun +arena-trace-test--wait (predicate)
  "Wait up to 30 seconds for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) 30)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should (funcall predicate)))

(defun +arena-trace-test--text (run)
  "Return the text of RUN's buffer."
  (with-current-buffer (+agent-shell-pstack-arena-trace-run-buffer run)
    (buffer-string)))

(defun +arena-trace-test--trace (repo command &optional enforce)
  "Trace COMMAND in a candidate of REPO; return the run once cleaned up.
With ENFORCE, run it under the candidate profile instead."
  (let ((run (+agent-shell-pstack-arena-trace repo command enforce)))
    (+arena-trace-test--wait
     (lambda () (string-match-p "^\\(Removed the throwaway worktree\\|Cleanup\\)"
                                (+arena-trace-test--text run))))
    run))

(defmacro +arena-trace-test--on-macos (&rest body)
  "Run BODY in a scratch arena only where traces work."
  (declare (indent 0))
  `(progn
     (skip-unless (and (eq system-type 'darwin)
                       (file-executable-p "/usr/bin/sandbox-exec")
                       (file-executable-p "/usr/bin/log")
                       (executable-find "python3")))
     (+arena-git-test--with-scratch
       (let ((+agent-shell-pstack-panel-directory
              (expand-file-name "reports/" +arena-git-test--scratch)))
         ,@body))))

(defun +arena-trace-test--cleaned-p (repo)
  "Return whether REPO has no arena worktree or folder left."
  (and (equal (+arena-git-test--git repo "worktree" "list" "--porcelain")
              (format "worktree %s\nHEAD %s\nbranch refs/heads/main"
                      (directory-file-name repo)
                      (+arena-git-test--git repo "rev-parse" "HEAD")))
       (not (and (file-directory-p +agent-shell-pstack-arena-directory)
                 (directory-files +agent-shell-pstack-arena-directory nil
                                  "\\`[^.]")))))

(ert-deftest +arena-trace-test-reports-what-it-would-refuse ()
  "Let writes outside the sandbox and reads of the lead through, report
them and nothing inside, then clean up after a failing command.
The kernel drops the odd report, so each kind is made several times."
  (+arena-trace-test--on-macos
    (let* ((repo (+arena-git-test--repo "a.txt" "a\n" "b.txt" "b\n"
                                        "c.txt" "c\n"))
           (outside (format "/private/tmp/pa-trace-test-%08x"
                            (random #x100000000)))
           (run (+arena-trace-test--trace
                 repo (format "touch inside.txt && python3 -c \"import os
os.mkdir('%s')
for name in 'xyz': open(os.path.join('%s', name), 'w')\" && cat %s && exit 3"
                              outside outside
                              (mapconcat (lambda (name)
                                           (expand-file-name name repo))
                                         '("a.txt" "b.txt" "c.txt") " "))))
           (text (+arena-trace-test--text run))
           (reports (substring text (or (string-search "What the" text) 0))))
      (unwind-protect
          (progn
            (should (file-exists-p (expand-file-name "z" outside)))
            (should (string-match-p "^Exit status 3\\.$" text))
            (should (string-match-p
                     (format "^file-write-create\n\\(  .*\n\\)*  %s.*  (.*Python"
                             (regexp-quote outside))
                     reports))
            (should (string-match-p
                     (format "^file-read-data\n\\(  .*\n\\)*  %s.*  (.*cat"
                             (regexp-quote repo))
                     reports))
            (should-not (string-search "inside.txt" reports))
            (should (string-match-p "^Output:\na\nb\nc$" text))
            (should (+arena-trace-test--cleaned-p repo)))
        (delete-directory outside t)
        (kill-buffer (+agent-shell-pstack-arena-trace-run-buffer run))))))

(ert-deftest +arena-trace-test-enforced-refusals ()
  "Under the candidate profile, refuse a write outside the sandbox and
report it, but not the writes the profile allows."
  (+arena-trace-test--on-macos
    (let* ((repo (+arena-git-test--repo "a.txt" "a\n"))
           (outside (format "/private/tmp/pa-enforce-test-%08x"
                            (random #x100000000)))
           (run (+arena-trace-test--trace
                 repo (format "touch inside.txt \"$TMPDIR/t\"; for i in 1 2 3; \
do touch %s; done; echo after" outside)
                 t))
           (text (+arena-trace-test--text run))
           (reports (substring text (or (string-search "What the" text) 0))))
      (unwind-protect
          (progn
            (should (string-prefix-p "Enforced arena run of" text))
            (should-not (file-exists-p outside))
            (should (string-match-p "^Exit status 0\\.$" text))
            (should (string-search "What the candidate profile refused, by \
operation." text))
            (should (string-match-p
                     (format "^file-write-create\n\\(  .*\n\\)*  %s  (touch)"
                             (regexp-quote outside))
                     reports))
            (should-not (string-search "inside.txt" reports))
            (should (string-match-p "Operation not permitted\nafter$" text))
            (should (+arena-trace-test--cleaned-p repo)))
        (when (file-exists-p outside) (delete-file outside))
        (kill-buffer (+agent-shell-pstack-arena-trace-run-buffer run))))))

(defconst +arena-trace-test--lock
  "lock() { mkdir -p \"$1/locked/inner\" && touch \"$1/locked/inner/file\" \
&& chmod 444 \"$1/locked/inner/file\" \
&& chmod 555 \"$1/locked/inner\" \"$1/locked\"; }; "
  "Shell function making a read-only tree, like Bazel's outputs, in $1.")

(ert-deftest +arena-trace-test-cleans-read-only-trees ()
  "Remove read-only trees the command left in its private temp folder and
its worktree."
  (+arena-trace-test--on-macos
    (let* ((repo (+arena-git-test--repo "a.txt" "a\n"))
           (run (+arena-trace-test--trace
                 repo (concat +arena-trace-test--lock
                              "lock \"$TMPDIR\" && lock .")))
           (text (+arena-trace-test--text run))
           (sandbox (+agent-shell-pstack-arena-trace-run-sandbox run)))
      (unwind-protect
          (progn
            (should (string-match-p "^Exit status 0\\.$" text))
            (should (string-search "Removed the throwaway worktree." text))
            (should-not (file-exists-p (+agent-shell-sandbox-tmp sandbox)))
            (should (+arena-trace-test--cleaned-p repo)))
        (kill-buffer (+agent-shell-pstack-arena-trace-run-buffer run))))))

(ert-deftest +arena-trace-test-cleanup-continues ()
  "Remove the worktree even when a private folder cannot be deleted, and
say why the folder stayed."
  (+arena-trace-test--on-macos
    (let* ((repo (+arena-git-test--repo "a.txt" "a\n"))
           (run (+arena-trace-test--trace
                 repo "touch \"$TMPDIR/pinned\" && chflags uchg \"$TMPDIR/pinned\""))
           (text (+arena-trace-test--text run))
           (tmp (+agent-shell-sandbox-tmp
                 (+agent-shell-pstack-arena-trace-run-sandbox run))))
      (unwind-protect
          (progn
            (should (string-match-p "^Exit status 0\\.$" text))
            (should (string-match-p "^Could not delete .*pinned" text))
            (should (string-search "Removed the throwaway worktree." text))
            (should (+arena-trace-test--cleaned-p repo)))
        (+agent-shell-sandbox-test-unlock tmp)
        (delete-directory tmp t)
        (kill-buffer (+agent-shell-pstack-arena-trace-run-buffer run))))))

(ert-deftest +arena-trace-test-read-only-git ()
  "Run git status in a candidate without touching the lead's git directory."
  (+arena-trace-test--on-macos
    (let* ((repo (+arena-git-test--repo "a.txt" "a\n"))
           (run (+arena-trace-test--trace repo "git status --short"))
           (text (+arena-trace-test--text run)))
      (unwind-protect
          (progn
            (should (string-match-p "^Exit status 0\\.$" text))
            (should (string-search "Nothing outside the candidate's sandbox \
was reported." text))
            (should (+arena-trace-test--cleaned-p repo)))
        (kill-buffer (+agent-shell-pstack-arena-trace-run-buffer run))))))

(ert-deftest +arena-trace-test-killed-buffer ()
  "Killing the buffer mid-run kills the command and cleans up."
  (+arena-trace-test--on-macos
    (let* ((repo (+arena-git-test--repo "a.txt" "a\n"))
           (run (+agent-shell-pstack-arena-trace repo "sleep 30")))
      (+arena-trace-test--wait
       (lambda () (eq 'running (+agent-shell-pstack-arena-trace-run-phase
                                run))))
      (let ((process (+agent-shell-pstack-arena-trace-run-process run)))
        (kill-buffer (+agent-shell-pstack-arena-trace-run-buffer run))
        (+arena-trace-test--wait (lambda () (+arena-trace-test--cleaned-p
                                             repo)))
        (should-not (process-live-p process))
        (should (eq 'finished (+agent-shell-pstack-arena-trace-run-phase
                               run)))))))

(ert-deftest +arena-trace-test-stop-while-preparing ()
  "Stopping before the command starts cleans up without running it."
  (+arena-trace-test--on-macos
    (let* ((repo (+arena-git-test--repo "a.txt" "a\n"))
           (marker (expand-file-name "ran" +arena-git-test--scratch))
           (run (+agent-shell-pstack-arena-trace
                 repo (concat "touch " marker))))
      (+agent-shell-pstack-arena-trace-stop run)
      (+arena-trace-test--wait
       (lambda () (string-search "Stopped." (+arena-trace-test--text run))))
      (+arena-trace-test--wait (lambda () (+arena-trace-test--cleaned-p repo)))
      (should-not (file-exists-p marker))
      (kill-buffer (+agent-shell-pstack-arena-trace-run-buffer run)))))

(provide 'agent-shell-pstack-arena-trace-test)
;;; agent-shell-pstack-arena-trace-test.el ends here
