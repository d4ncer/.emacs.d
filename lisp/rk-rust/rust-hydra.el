;;; rust-hydra.el --- Rust hydra  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;

;;; Code:

(require 'pretty-hydra)
(require 'cargo-process)

(pretty-hydra-define rk-rust--main
  (:title "Rust" :quit-key "q")
  ("Project"
   (("b" cargo-process-build "build")
    ("C" cargo-process-check "clean")
    ("u" cargo-process-update "update")
    ("d" cargo-process-doc "doc"))
   "Test"
   (("t" cargo-process-test "all tests")
    ("o" cargo-process-current-file-tests "file tests")
    ("f" cargo-process-current-test "current test")
    ("e" cargo-process-bench "benchmarks"))
   "Misc"
   (("." cargo-process-repeat "repeat last cmd")
    ("s" cargo-process-search "search")
    ("x" cargo-process-run "run")
    ("c" cargo-process-check "check"))))

(provide 'rust-hydra)

;;; rust-hydra.el ends here
