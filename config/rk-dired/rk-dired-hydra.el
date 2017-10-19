;;; rk-dired-hydra.el --- Configuration for Dired hydra  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'evil-transient-state)

(evil-transient-state-define rk-dired-main
  :title "Dired Transient State"
  :doc "

 ^Basic^           | ^View^        |
-^-----^-----------+-^----^---+-^-------^--------+-^----^-------
_+_: mkdir         | _v_: view          _(_: details        _i_: insert-subdir  wdired
_C_: copy          | _O_: view other    _)_: omit-mode      _$_: hide-subdir    C-x C-q : edit
_D_: delete        | _o_: open other    _l_: redisplay      _w_: kill-subdir    C-c C-c : commit
_R_: rename        | _M_: chmod         _g_: revert buf     _e_: ediff          C-c ESC : abort
_Y_: rel symlink   | _G_: chgrp         _s_: sort           _=_: pdiff
_S_: symlink       | _A_: find regexp   _._: toggle hydra   _?_: summary
_r_: rsync         | _Q_: repl regexp
_z_: compress-file |
_Z_: compress      |
"
  :bindings
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil "quit dired")
  ("." nil "toggle hydra" :color blue))

(provide 'rk-dired-hydra)

;;; rk-dired-hydra.el ends here
