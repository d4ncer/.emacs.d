;; +evil-collection.el --- Supporting utils for evil-collection config -*- lexical-binding: t; -*-

;;; Commentary:

;; Evil-collection is indispensable for setting up a consistent set of vim
;; bindings across most Emacs modes I'm likely to use. However, it loads some
;; stuff too eagerly, and the load order is confusing, making it hard to
;; customise.

;; Doom works around this by rolling their own load sequence for
;; evil-collection. Their code is cribbed below.

;;; Code:

(require '+corelib)

(autoload 'evil-collection-init "evil-collection")

(defvar evil-collection-setup-minibuffer nil)

;; https://raw.githubusercontent.com/emacs-evil/evil-collection/master/evil-collection.el
(defvar evil-collection-mode-list
  `(2048-game
    ag
    alchemist
    anaconda-mode
    apropos
    arc-mode
    atomic-chrome
    auto-package-update
    beginend
    bluetooth
    bm
    bookmark
    (buff-menu "buff-menu")
    bufler
    calc
    calendar
    cider
    citre
    cmake-mode
    color-rg
    comint
    company
    compile
    consult
    corfu
    crdt
    (csv "csv-mode")
    (custom cus-edit)
    cus-theme
    dape
    dashboard
    daemons
    deadgrep
    debbugs
    debug
    devdocs
    dictionary
    diff-hl
    diff-mode
    dired
    dired-sidebar
    disk-usage
    distel
    doc-view
    docker
    eat
    ebib
    ebuku
    edbi
    edebug
    ediff
    eglot
    elpaca
    ement
    explain-pause-mode
    eldoc
    elfeed
    elisp-mode
    elisp-refs
    elisp-slime-nav
    embark
    emms
    ,@(when (>= emacs-major-version 29) '(emoji))
    epa
    ert
    eshell
    eval-sexp-fu
    evil-mc
    eww
    fanyi
    finder
    flycheck
    flymake
    forge
    free-keys
    geiser
    ggtags
    git-timemachine
    gited
    gnus
    go-mode
    gptel
    grep
    guix
    hackernews
    helm
    help
    helpful
    hg-histedit
    hungry-delete
    hyrolo
    ibuffer
    (image image-mode)
    image-dired
    image+
    imenu
    imenu-list
    (indent "indent")
    indium
    info
    ivy
    js2-mode
    ,@(when (>= emacs-major-version 30) '(kmacro))
    leetcode
    lispy
    lms
    log-edit
    log-view
    lsp-ui-imenu
    lua-mode
    kotlin-mode
    macrostep
    man
    (magit magit-submodule) ;; See https://github.com/emacs-evil/evil-collection/issues/637
    magit-repos
    magit-section
    magit-todos
    markdown-mode
    ,@(when evil-collection-setup-minibuffer '(minibuffer))
    monky
    mpc
    mpdel
    mpdired
    mu4e
    mu4e-conversation
    neotree
    newsticker
    notmuch
    nov
    omnisharp
    org
    org-present
    org-roam
    osx-dictionary
    outline
    p4
    (package-menu package)
    pass
    (pdf pdf-view)
    popup
    proced
    (process-menu simple)
    prodigy
    profiler
    p-search
    python
    quickrun
    racer
    racket-describe
    realgud
    reftex
    replace ;; For `occur'.
    restclient
    rg
    ripgrep
    rjsx-mode
    robe
    rtags
    ruby-mode
    scheme
    scroll-lock
    selectrum
    sh-script
    ,@(when (>= emacs-major-version 28) '(shortdoc))
    simple
    simple-mpc
    slime
    sly
    smerge-mode
    snake
    so-long
    speedbar
    ,@(when (>= emacs-major-version 27) '(tab-bar))
    tablist
    tabulated-list
    tar-mode
    telega
    (term term ansi-term multi-term)
    tetris
    ,@(when (>= emacs-major-version 27) '(thread))
    tide
    timer-list
    transmission
    trashed
    tuareg
    typescript-mode
    ultra-scroll
    vc-annotate
    vc-dir
    vc-git
    vdiff
    vertico
    view
    vlf
    vterm
    vundo
    w3m
    wdired
    wgrep
    which-key
    with-editor
    woman
    xref
    xwidget
    yaml-mode
    youtube-dl
    zmusic
    (ztree ztree-diff ztree-dir))
  "List of modes supported by evil-collection. Elements are
either target mode symbols or lists which `car' is the mode
symbol and `cdr' the packages to register.")

(defvar +evil-collection-disabled-list
  '(anaconda-mode
    buff-menu
    calc
    comint
    company
    custom
    eldoc
    elisp-mode
    ert
    free-keys
    helm
    help
    image
    indent
    kmacro
    kotlin-mode
    lispy
    outline
    replace
    shortdoc
    simple
    slime
    tab-bar))

;; We handle loading evil-collection ourselves
(defvar evil-collection--supported-modes nil)

(defun +evil-collection-init (module &optional disabled-list)
  "Initialize evil-collection-MODULE.

Unlike `evil-collection-init', this respects `+evil-collection-disabled-list',
and complains if a module is loaded too early (during startup)."
  (unless (memq (or (car-safe module) module) disabled-list)
    (+log "editor:evil: loading evil-collection-%s %s"
          (or (car-safe module) module)
          (if after-init-time "" "(too early!)"))
    (with-demoted-errors "evil-collection error: %s"
      (evil-collection-init (list module)))))

(defun +evil-collection-defer-install-to-mode-activation ()
  (add-transient-hook! 'help-mode (+evil-collection-init 'help))
  (add-transient-hook! 'Buffer-menu-mode (+evil-collection-init '(buff-menu "buff-menu")))
  (add-transient-hook! 'calc-mode (+evil-collection-init 'calc))
  (add-transient-hook! 'image-mode (+evil-collection-init 'image))
  (add-transient-hook! 'emacs-lisp-mode (+evil-collection-init 'elisp-mode))
  (add-transient-hook! 'occur-mode (+evil-collection-init 'replace))
  (add-transient-hook! 'indent-rigidly (+evil-collection-init '(indent "indent")))
  (add-transient-hook! 'kmacro-menu-mode (+evil-collection-init 'kmacro))
  (add-transient-hook! 'process-menu-mode (+evil-collection-init '(process-menu simple)))
  (add-transient-hook! 'shortdoc-mode (+evil-collection-init 'shortdoc))
  (add-transient-hook! 'tabulated-list-mode (+evil-collection-init 'tabulated-list))
  (add-transient-hook! 'tab-bar-mode (+evil-collection-init 'tab-bar))

  (dolist (mode evil-collection-mode-list)
    (dolist (req (or (cdr-safe mode) (list mode)))
      (with-eval-after-load req
        (+evil-collection-init mode +evil-collection-disabled-list)))))

(provide '+evil-collection)
