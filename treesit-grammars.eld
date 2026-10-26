;; Grammar sources for the configured Emacs 32 modes.  Grammars track each
;; repository default branch; fix query breakage forward.
((bash "https://github.com/tree-sitter/tree-sitter-bash")
 (c "https://github.com/tree-sitter/tree-sitter-c")
 (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
 (css "https://github.com/tree-sitter/tree-sitter-css")
 (doxygen
  "https://github.com/tree-sitter-grammars/tree-sitter-doxygen")
 (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
 (heex "https://github.com/phoenixframework/tree-sitter-heex")
 (html "https://github.com/tree-sitter/tree-sitter-html")
 (java "https://github.com/tree-sitter/tree-sitter-java")
 (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
 (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
 (json "https://github.com/tree-sitter/tree-sitter-json")
 (markdown
  "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
  :source-dir "tree-sitter-markdown/src")
 (markdown-inline
  "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
  :source-dir "tree-sitter-markdown-inline/src")
 (nix "https://github.com/nix-community/tree-sitter-nix")
 (python "https://github.com/tree-sitter/tree-sitter-python")
 (rust "https://github.com/tree-sitter/tree-sitter-rust")
 (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
      :source-dir "tsx/src")
 (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
	     :source-dir "typescript/src")
 (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
