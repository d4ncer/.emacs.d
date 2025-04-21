org-mode

(q "#+begin_quote " n> q n "#+end_quote" >)
(ex "#+begin_example " p n q n "#+end_example" >)
(s "#+begin_src " p n q n "#+end_src" >)
(sh "#+begin_src sh" n> q n "#+end_src" >)
(el "#+begin_src emacs-lisp" n> q n "#+end_src" >)
(elx "#+begin_src elixir" n> q n "#+end_src" >)
(ts "#+begin_src typescript" n> q n "#+end_src" >)
(js "#+begin_src javascript" n> q n "#+end_src" >)
(C "#+begin_src C" n> q n "#+end_src" >)
