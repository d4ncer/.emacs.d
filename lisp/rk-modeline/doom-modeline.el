;;; doom-modeline.el --- Doom modeline  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;; Doom modeline

;;; Code:

;;; ui/doom-modeline/config.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

(require 'all-the-icons)
(require 'doom-helpers)
(require 'doom-add-hook)
(require 'shrink-path)

(use-package eldoc-eval
  :straight t
  :config
  (defun +doom-modeline-eldoc (text)
    (concat (when (display-graphic-p)
              (+doom-modeline--make-xpm
               (face-background 'doom-modeline-eldoc-bar nil t)
               +doom-modeline-height
               +doom-modeline-bar-width))
            text))

  ;; Show eldoc in the mode-line with `eval-expression'
  (defun +doom-modeline--show-eldoc (input)
    "Display string STR in the mode-line next to minibuffer."
    (with-current-buffer (eldoc-current-buffer)
      (let* ((str              (and (stringp input) input))
             (mode-line-format (or (and str (or (+doom-modeline-eldoc str) str))
                                   mode-line-format))
             mode-line-in-non-selected-windows)
        (force-mode-line-update)
        (sit-for eldoc-show-in-mode-line-delay))))
  (setq eldoc-in-minibuffer-show-fn #'+doom-modeline--show-eldoc)

  (eldoc-in-minibuffer-mode +1))

;; Keep `+doom-modeline-current-window' up-to-date
(defvar +doom-modeline-current-window (frame-selected-window))

(defun +doom-modeline|set-selected-window (&rest _)
  "Sets `+doom-modeline-current-window' appropriately"
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq +doom-modeline-current-window win))))

(add-hook 'window-configuration-change-hook #'+doom-modeline|set-selected-window)
(add-hook 'focus-in-hook #'+doom-modeline|set-selected-window)
(advice-add #'handle-switch-frame :after #'+doom-modeline|set-selected-window)
(advice-add #'select-window :after #'+doom-modeline|set-selected-window)

;;
;; Variables
;;

(defvar +doom-modeline-height 39
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar +doom-modeline-bar-width 7
  "How wide the mode-line bar should be (only respected in GUI emacs).")

(defvar +doom-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "TODO")

(defvar +doom-modeline-buffer-file-name-style 'relative-to-project
  "Determines the style used by `+doom-modeline-buffer-file-name'.
Given ~/Projects/FOSS/emacs/lisp/comint.el
truncate-upto-project => ~/P/F/emacs/lisp/comint.el
truncate-upto-root => ~/P/F/e/lisp/comint.el
truncate-all => ~/P/F/e/l/comint.el
relative-from-project => emacs/lisp/comint.el
relative-to-project => lisp/comint.el
file-name => comint.el")

;; externs
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar iedit-mode nil)
(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)


;;
;; Custom faces
;;

(defgroup +doom-modeline nil
  ""
  :group 'doom)

(defface doom-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group '+doom-modeline)

(defface doom-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group '+doom-modeline)

(defface doom-modeline-project-root-dir
  '((t (:weight light)))
  "Face used for the dir part of the mode-line buffer path."
  :group '+doom-modeline)

(defface doom-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `+doom-modeline--anzu', `+doom-modeline--evil-substitute' and
`iedit'"
  :group '+doom-modeline)

(defface doom-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+doom-modeline)

(defface doom-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

(defface doom-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

;; Bar
(defface doom-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group '+doom-modeline)

(defface doom-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active."
  :group '+doom-modeline)

(defface doom-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group '+doom-modeline)


;;
;; Modeline helpers
;;

(defsubst active ()
  (eq (selected-window) +doom-modeline-current-window))

;; Inspired from `powerline's `pl/make-xpm'.
(defun +doom-modeline--make-xpm (color height width)
  "Create an XPM bitmap."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (create-image
      (concat
       (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
               (length (car data))
               (length data)
               color
               color)
       (apply #'concat
              (cl-loop with idx = 0
                       with len = (length data)
                       for dl in data
                       do (cl-incf idx)
                       collect
                       (concat "\""
                               (cl-loop for d in dl
                                        if (= d 0) collect (string-to-char " ")
                                        else collect (string-to-char "."))
                               (if (eq idx len) "\"};" "\",\n")))))
      'xpm t :ascent 'center))))

(defun +doom-modeline-buffer-file-name ()
  "Propertized `buffer-file-name' based on `+doom-modeline-buffer-file-name-style'."
  (propertize
   (pcase +doom-modeline-buffer-file-name-style
     ('truncate-upto-project (+doom-modeline--buffer-file-name 'shrink))
     ('truncate-upto-root (+doom-modeline--buffer-file-name-truncate))
     ('truncate-all (+doom-modeline--buffer-file-name-truncate t))
     ('relative-to-project (+doom-modeline--buffer-file-name-relative))
     ('relative-from-project (+doom-modeline--buffer-file-name-relative 'include-project))
     ('file-name (propertize (file-name-nondirectory buffer-file-name)
                             'face
                             (let ((face (or (and (buffer-modified-p)
                                                  'doom-modeline-buffer-modified)
                                             (and (active)
                                                  'doom-modeline-buffer-file))))
                               (when face `(:inherit ,face))))))
   'help-echo buffer-file-truename))

(defun +doom-modeline--buffer-file-name-truncate (&optional truncate-tail)
  "Propertized `buffer-file-name' that truncates every dir along path.
If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory buffer-file-truename)))
        (active (active)))
    (if (null dirs)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (let ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified)))
        (let ((dirname (car dirs))
              (basename (cdr dirs))
              (dir-faces (or modified-faces (if active 'doom-modeline-project-root-dir)))
              (file-faces (or modified-faces (if active 'doom-modeline-buffer-file))))
          (concat (propertize (concat dirname
                                      (if truncate-tail (substring basename 0 1) basename)
                                      "/")
                              'face (if dir-faces `(:inherit ,dir-faces)))
                  (propertize (file-name-nondirectory buffer-file-name)
                              'face (if file-faces `(:inherit ,file-faces)))))))))

(defun +doom-modeline--buffer-file-name-relative (&optional include-project)
  "Propertized `buffer-file-name' showing directories relative to project's root only."
  (let ((root (projectile-project-root))
        (active (active)))
    (if (null root)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (let* ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified))
             (relative-dirs (file-relative-name (file-name-directory buffer-file-truename)
                                                (if include-project (concat root "../") root)))
             (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
             (file-faces (or modified-faces (if active 'doom-modeline-buffer-file))))
        (if (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                (propertize (file-name-nondirectory buffer-file-truename)
                            'face (if file-faces `(:inherit ,file-faces))))))))

(defun +doom-modeline--buffer-file-name (truncate-project-root-parent)
  "Propertized `buffer-file-name'.
If TRUNCATE-PROJECT-ROOT-PARENT is t space will be saved by truncating it down
fish-shell style.
Example:
~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/lisp/comint.el"
  (let* ((project-root (projectile-project-root))
         (file-name-split (shrink-path-file-mixed project-root
                                                  (file-name-directory buffer-file-truename)
                                                  buffer-file-truename))
         (active (active)))
    (if (null file-name-split)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (pcase-let ((`(,root-path-parent ,project ,relative-path ,filename) file-name-split))
        (let ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified)))
          (let ((sp-faces       (or modified-faces (if active 'font-lock-comment-face)))
                (project-faces  (or modified-faces (if active 'font-lock-string-face)))
                (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
                (file-faces     (or modified-faces (if active 'doom-modeline-buffer-file))))
            (let ((sp-props       `(,@(if sp-faces       `(:inherit ,sp-faces))      ,@(if active '(:weight bold))))
                  (project-props  `(,@(if project-faces  `(:inherit ,project-faces)) ,@(if active '(:weight bold))))
                  (relative-props `(,@(if relative-faces `(:inherit ,relative-faces))))
                  (file-props     `(,@(if file-faces     `(:inherit ,file-faces)))))
              (concat (propertize (if truncate-project-root-parent
                                      root-path-parent
                                    (abbreviate-file-name project-root))
                                  'face sp-props)
                      (propertize (concat project "/") 'face project-props)
                      (if relative-path (propertize relative-path 'face relative-props))
                      (propertize filename 'face file-props)))))))))


;;
;; Segments
;;

(def-modeline-segment! buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (active) 'doom-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (all-the-icons-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

;;
(def-modeline-segment! buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat (cond (buffer-read-only
                 (concat (all-the-icons-octicon
                          "lock"
                          :face 'doom-modeline-warning
                          :v-adjust -0.05)
                         " "))
                ((buffer-modified-p)
                 (concat (all-the-icons-faicon
                          "floppy-o"
                          :face 'doom-modeline-buffer-modified
                          :v-adjust -0.0575)
                         " "))
                ((and buffer-file-name
                      (not (file-exists-p buffer-file-name)))
                 (concat (all-the-icons-octicon
                          "circle-slash"
                          :face 'doom-modeline-urgent
                          :v-adjust -0.05)
                         " "))
                ((buffer-narrowed-p)
                 (concat (all-the-icons-octicon
                          "fold"
                          :face 'doom-modeline-warning
                          :v-adjust -0.05)
                         " ")))
          (if buffer-file-name
              (+doom-modeline-buffer-file-name)
            "%b")))

;;
(def-modeline-segment! buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (propertize
   "%b"
   'face (cond ((and buffer-file-name (buffer-modified-p))
                'doom-modeline-buffer-modified)
               ((active) 'doom-modeline-buffer-file))))

;;
(def-modeline-segment! buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          "  "))

;;
(def-modeline-segment! major-mode
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (format-mode-line mode-name)
           (when (stringp mode-line-process)
             mode-line-process)
           (and (featurep 'face-remap)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount)))
   'face (if (active) 'doom-modeline-buffer-major-mode)))

;;
(def-modeline-segment! vcs
  "Displays the current branch, colored based on its state."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (active))
            (all-the-icons-default-adjust -0.1))
        (if (not active)
            "  "
          (concat "  "
                  (cond ((memq state '(edited added))
                         (if active (setq face 'doom-modeline-info))
                         (all-the-icons-octicon
                          "git-compare"
                          :face face
                          :v-adjust -0.05))
                        ((eq state 'needs-merge)
                         (if active (setq face 'doom-modeline-info))
                         (all-the-icons-octicon "git-merge" :face face))
                        ((eq state 'needs-update)
                         (if active (setq face 'doom-modeline-warning))
                         (all-the-icons-octicon "arrow-down" :face face))
                        ((memq state '(removed conflict unregistered))
                         (if active (setq face 'doom-modeline-urgent))
                         (all-the-icons-octicon "alert" :face face))
                        (t
                         (if active (setq face 'font-lock-doc-face))
                         (all-the-icons-octicon
                          "git-compare"
                          :face face
                          :v-adjust -0.05)))
                  " "
                  (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                              'face (if active face))
                  " "))))))

;;
(defun +doom-ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat (if vc-mode " " "  ")
          (when icon
            (concat
             (all-the-icons-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text +doom-modeline-vspc)))
          (when text
            (propertize text 'face face))
          (if vc-mode "  " " ")))

(def-modeline-segment! flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (if (active)
      (when (boundp 'flycheck-last-status-change)
        (pcase flycheck-last-status-change
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (+doom-ml-icon "do_not_disturb_alt"
                                            (number-to-string sum)
                                            (if .error 'doom-modeline-urgent 'doom-modeline-warning)
                                            -0.25)))
                       (+doom-ml-icon "check" nil 'doom-modeline-info)))
          ('running     (+doom-ml-icon "access_time" nil 'font-lock-doc-face -0.25))
          ('no-checker  (+doom-ml-icon "sim_card_alert" "-" 'font-lock-doc-face))
          ('errored     (+doom-ml-icon "sim_card_alert" "Error" 'doom-modeline-urgent))
          ('interrupted (+doom-ml-icon "pause" "Interrupted" 'font-lock-doc-face))))
    "  "))
;; ('interrupted (+doom-ml-icon "x" "Interrupted" 'font-lock-doc-face)))))

;;
(defsubst doom-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(def-modeline-segment! selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (active) (or mark-active (eq evil-state 'visual)))
    (let ((reg-beg (region-beginning))
          (reg-end (region-end)))
      (propertize
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max)))))
         (cond ((or (bound-and-true-p rectangle-mark-mode)
                    (eq 'block evil-visual-selection))
                (let ((cols (abs (- (doom-column reg-end)
                                    (doom-column reg-beg)))))
                  (format "%dx%dB" lines cols)))
               ((eq 'line evil-visual-selection)
                (format "%dL" lines))
               ((> lines 1)
                (format "%dC %dL" (- (1+ reg-end) reg-beg) lines))
               (t
                (format "%dC" (- (1+ reg-end) reg-beg)))))
       'face 'doom-modeline-highlight))))


;;
(defun +doom-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'doom-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'doom-modeline-panel)
              sep
              (all-the-icons-octicon "triangle-right"
                                     :face 'doom-modeline-panel
                                     :v-adjust -0.05)
              sep))))

(defsubst +doom-modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and evil-mode
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (active) 'doom-modeline-panel))))

(defun doom-themes--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst +doom-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and iedit-mode iedit-occurrences-overlays)
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn (iedit-prev-occurrence)
                               (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'doom-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (active) 'doom-modeline-panel))))

(def-modeline-segment! matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (+doom-modeline--macro-recording)
                      (+doom-modeline--evil-substitute)
                      (+doom-modeline--iedit))))
    (or (and (not (equal meta "")) meta)
        (if buffer-file-name " %p "))))

;; TODO Include other information
(def-modeline-segment! media-info
  "Metadata regarding the current file, such as dimensions for images."
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
             (image-size (image-get-display-property) :pixels)
           (format "  %dx%d  " width height)))))

(def-modeline-segment! bar
  "The bar regulates the height of the mode-line in GUI Emacs.
Returns \"\" to not break --no-window-system."
  (if (display-graphic-p)
      (+doom-modeline--make-xpm
       (face-background (if (active)
                            'doom-modeline-bar
                          'doom-modeline-inactive-bar)
                        nil t)
       +doom-modeline-height
       +doom-modeline-bar-width)
    ""))


;;
;; Mode lines
;;

(def-modeline! main
  (bar matches " " buffer-info vcs selection-info flycheck))

(def-modeline! minimal
  (bar matches " " buffer-info media-info))

(def-modeline! special
  (bar matches " " buffer-info-simple selection-info flycheck))

(def-modeline! project
  (bar buffer-default-directory))

(def-modeline! media
  (bar " %b  " media-info))


;;
;; Hooks
;;

(defun +doom-modeline|init ()
  "Set the default modeline."
  (doom-set-modeline 'main t)

  ;; This scratch buffer is already created and doesn't get a modeline. For the
  ;; love of Emacs, someone give the man a modeline!
  (with-current-buffer "*scratch*"
    (doom-set-modeline 'main)))

(defun +doom-modeline|set-special-modeline ()
  (doom-set-modeline 'special))

(defun +doom-modeline|set-media-modeline ()
  (doom-set-modeline 'media))

(defun +doom-modeline|set-project-modeline ()
  (doom-set-modeline 'project))


;;
;; Bootstrap
;;

(add-hook '+doom-dashboard-mode-hook #'+doom-modeline|set-project-modeline)

(add-hook 'image-mode-hook   #'+doom-modeline|set-media-modeline)
(add-hook 'org-src-mode-hook #'+doom-modeline|set-special-modeline)

(provide 'doom-modeline)

;;; doom-modeline.el ends here
