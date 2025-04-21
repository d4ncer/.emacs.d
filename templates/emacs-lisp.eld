emacs-lisp-mode

(s "(setq " p ")")
(sl "(setq-local " p ")")
(l "(let ((" p "))" n> q ")")
(it "(lambda (" (p "it") ")" n> q ")")

(i "(if " q ")")
(w "(when " q ")")
(u "(unless " q ")")
(wh "(while " p n> q ")")
(c "(cond" n> "(" q ")" n> ")")
(wl "(when-let* ((" p "))" n> q ")")
(il "(if-let* ((" p "))" n> q ")")
(cc "(condition-case err" n> p n> "(err " q "))")

(p "(progn" n> q ")")
(p1 "(prog1 " p n> q ")")
(ie "(ignore-errors" n> q ")")

(dl "(dolist (" p ")" n> q ")")

(pc "(pcase " q n> ")")
(pce "(pcase-exhaustive " q n> ")")
(pdl "(pcase-dolist (" p ")" n> q ")")
(pl "(pcase-let (" p ")" n> q ")")
(pla "(pcase-lambda (" p ")" n> q ")")

(v "(defvar " (p "NAME") " " (p "VALUE") ")")
(dc "(defconst " (p "NAME") " " (p "VALUE") ")")
(d "(defun " (p "NAME") " (" p ")" n> q ")")
(dm "(defmacro " (p "NAME") " (" p ")" n> q ")")
(da "(define-advice " (p "TARGET-FN") " (:" (p "WHERE" where) " ("
    ;; Generate arg-list based on advice type.
    (pcase where
      ("around"
       "fn &rest args")
      ("filter-return"
       "result")
      (_ "&rest args"))

    ") " (p "NAME") ")" n> q ")")
(ert "(ert-deftest " (p "NAME") " (" p ")" n> q ")")

(atl "(add-to-list '" q ")")
(as "(cl-assert " p " t " "\"" p "\")")

(a "(with-eval-after-load '" (p "FEATURE") n> q ")")
(se "(save-excursion" n> q ")")
(ewc "(eval-when-compile" n> q ")")
(eac "(eval-and-compile" n> q ")")
(r "(require '" q ")")
(al "(autoload '" (p "IDENT" sym) " "
    "\""
    (if-let* ((file (symbol-file (if (stringp sym) (intern sym) sym))))
        (file-name-sans-extension (file-name-nondirectory file))
      "FEAT")
    "\")")

(lbp "(line-beginning-position)")
(lep "(line-end-position)")
(pt "(point)")
(pm "(point-min)")
(pM "(point-max)")
(wcb "(with-current-buffer " p n> q ")")
(bs "(buffer-substring " q ")")
(bsl "(buffer-substring (line-beginning-position) (line-end-position))")

(m "(message \"" p "\" " q ")")

(up "(use-package " q ")")
(in "(interactive)")
(gc "(goto-char " q ")")
(ah "(add-hook '" (p "HOOK") " " (p "FN") q ")")
