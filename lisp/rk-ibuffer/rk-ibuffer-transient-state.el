;;; rk-ibuffer.el --- Configuration for ibuffer transient state  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'evil-transient-state)

(evil-transient-state-define rk-ibuffer-main
  :title "IBuffer Transient State"
  :foreign-keys run
  :doc "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
_RET_: visit  | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
 ^^           | ^^            | ^^               | ^^
  _o_: other  | _q_: quit     | _._: toggle      | ^^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------

"
  :bindings
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" rk-ibuffer-mark-transient-state/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" rk-ibuffer-action-transient-state/body :color blue)

  ("g" ibuffer-update)
  ("s" rk-ibuffer-sort-transient-state/body :color blue)
  ("/" rk-ibuffer-filter-transient-state/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(evil-transient-state-define rk-ibuffer-mark
  :title "Mark"
  :columns 5
  :on-exit (rk-ibuffer-transient-state/body)
  :bindings
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" rk-ibuffer-main-transient-state/body "back" :color blue))

(evil-transient-state-define rk-ibuffer-action
  :title "Action"
  :columns 4
  :on-exit (if (eq major-mode 'ibuffer-mode)
               (rk-ibuffer-main-transient-state/body))
  :bindings
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" rk-ibuffer-main-transient-state/body "back"))

(evil-transient-state-define rk-ibuffer-sort
  :title "Sort"
  :columns 3
  :bindings
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" rk-ibuffer-main-transient-state/body "back" :color blue))

(evil-transient-state-define rk-ibuffer-filter
  :title "Filter"
  :columns 4
  :bindings
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" rk-ibuffer-main-transient-state/body "back" :color blue))

(provide 'rk-ibuffer-transient-state)

;;; rk-ibuffer-transient-state.el ends here
