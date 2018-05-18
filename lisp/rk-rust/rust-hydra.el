;;; rust-hydra.el --- Rust hydra  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;

;;; Code:

(require 'evil-transient-state)
(require 'cargo-process)

(evil-transient-state-define rust-hydra
  :title "Rust"
  :doc "
 ^Project^           | ^Test^            | ^Misc^
-^-------^------------+-^----^-------------+-^----^--------------------------
_b_: build           | _t_: all tests    | _._: repeat last cargo command
_c_: clean           | _o_: file tests   | _s_: search
_u_: update          | _f_: current test | _x_: run
_d_: docs            | _e_: benchmarks   |

_q_: quit
"
  :bindings
  ("b" cargo-process-build)
  ("c" cargo-process-clean)
  ("u" cargo-process-update)
  ("d" cargo-process-doc)
  ("t" cargo-process-test)
  ("o" cargo-process-current-file-tests)
  ("f" cargo-process-current-test)
  ("e" cargo-process-bench)
  ("." cargo-process-repeat)
  ("s" cargo-process-search)
  ("x" cargo-process-run)
  ("q" nil))

(provide 'rust-hydra)

;;; rust-hydra.el ends here
