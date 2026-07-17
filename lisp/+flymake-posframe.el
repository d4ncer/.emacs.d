;;; +flymake-posframe.el --- Display flymake diagnostics at point in a posframe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; Show the flymake diagnostic under point in a child frame.
;;
;; This replaces the unmaintained `flymake-posframe' package, which broke on
;; Emacs 30+ when the internal `flymake--diag-text' accessor was removed. We use
;; the public `flymake-diagnostic-text' / `flymake-diagnostic-type' accessors
;; instead.
;;
;; The display function runs from `post-command-hook', where a signalled error
;; aborts every hook function queued after it -- which silently breaks unrelated
;; things like corfu's auto-popup. `+flymake-posframe-display' therefore traps
;; its own errors rather than letting them escape into the hook.

;;; Code:

(require 'flymake)
(require 'posframe)

(defgroup +flymake-posframe nil
  "Display flymake diagnostics at point using a posframe."
  :group 'flymake
  :prefix "+flymake-posframe-")

;;; Customisation

(defcustom +flymake-posframe-error-prefix "!"
  "String displayed before an error message."
  :group '+flymake-posframe
  :type '(choice (const :tag "No prefix" nil) string))

(defcustom +flymake-posframe-warning-prefix "⚠"
  "String displayed before a warning message."
  :group '+flymake-posframe
  :type '(choice (const :tag "No prefix" nil) string))

(defcustom +flymake-posframe-note-prefix "ℹ"
  "String displayed before a note message."
  :group '+flymake-posframe
  :type '(choice (const :tag "No prefix" nil) string))

(defcustom +flymake-posframe-default-prefix "?"
  "String displayed before a message of any other type."
  :group '+flymake-posframe
  :type '(choice (const :tag "No prefix" nil) string))

(defcustom +flymake-posframe-buffer " *+flymake-posframe*"
  "Name of the buffer backing the posframe."
  :group '+flymake-posframe
  :type 'string)

(defcustom +flymake-posframe-timeout 0
  "Seconds after which the posframe auto-hides.  Zero means never."
  :group '+flymake-posframe
  :type 'integer)

(defcustom +flymake-posframe-internal-border-width 6
  "Width of the posframe's internal border."
  :group '+flymake-posframe
  :type 'integer)

(defcustom +flymake-posframe-max-width 75
  "Maximum width of the posframe, in columns."
  :group '+flymake-posframe
  :type '(choice (const :tag "Unlimited" nil) integer))

(defcustom +flymake-posframe-max-height nil
  "Maximum height of the posframe, in lines."
  :group '+flymake-posframe
  :type '(choice (const :tag "Unlimited" nil) integer))

(defcustom +flymake-posframe-parameters nil
  "Extra frame parameters for the posframe."
  :group '+flymake-posframe
  :type 'alist)

;;; Faces

(defface +flymake-posframe-face
  '((t :inherit tooltip))
  "Face supplying the posframe's foreground and background colours."
  :group '+flymake-posframe)

(defface +flymake-posframe-border-face
  '((t :inherit vertical-border))
  "Face supplying the posframe's border colour."
  :group '+flymake-posframe)

(defface +flymake-posframe-prefix-error-face
  '((t :inherit flymake-error-echo))
  "Face for the prefix on error messages."
  :group '+flymake-posframe)

(defface +flymake-posframe-prefix-warning-face
  '((t :inherit flymake-warning-echo))
  "Face for the prefix on warning messages."
  :group '+flymake-posframe)

(defface +flymake-posframe-prefix-note-face
  '((t :inherit flymake-note-echo))
  "Face for the prefix on note messages."
  :group '+flymake-posframe)

(defface +flymake-posframe-prefix-default-face
  '((t :inherit shadow))
  "Face for the prefix on messages of any other type."
  :group '+flymake-posframe)

;;; Internals

(defvar-local +flymake-posframe--last-diag nil
  "The diagnostic currently shown, used to avoid redundant redisplay.")

(defvar +flymake-posframe-hide-hooks '(pre-command-hook post-command-hook)
  "Hooks that trigger automatic removal of the posframe.")

(defun +flymake-posframe--diagnostic-at-point ()
  "Return the flymake diagnostic under point, or nil."
  (get-char-property (point) 'flymake-diagnostic))

(defun +flymake-posframe--prefix (diag)
  "Return a cons of (STRING . FACE) to display before DIAG's message."
  (pcase (flymake--lookup-type-property (flymake-diagnostic-type diag)
                                        'flymake-category)
    ('flymake-error   (cons +flymake-posframe-error-prefix
                            '+flymake-posframe-prefix-error-face))
    ('flymake-warning (cons +flymake-posframe-warning-prefix
                            '+flymake-posframe-prefix-warning-face))
    ('flymake-note    (cons +flymake-posframe-note-prefix
                            '+flymake-posframe-prefix-note-face))
    (_                (cons +flymake-posframe-default-prefix
                            '+flymake-posframe-prefix-default-face))))

(defun +flymake-posframe--visible-p ()
  "Return non-nil if the posframe is currently visible.
Unlike the original package, this tolerates the backing buffer not
existing yet, which is the case before the first display."
  (when-let* ((buf (get-buffer +flymake-posframe-buffer))
              (frame (buffer-local-value 'posframe--frame buf)))
    (and (frame-live-p frame) (frame-visible-p frame))))

(defun +flymake-posframe-hide ()
  "Hide the posframe and detach the hooks that trigger hiding."
  (posframe-hide +flymake-posframe-buffer)
  (setq +flymake-posframe--last-diag nil)
  (dolist (hook +flymake-posframe-hide-hooks)
    (remove-hook hook #'+flymake-posframe-hide t)))

(defun +flymake-posframe--show (diag)
  "Show DIAG's message in the posframe."
  (let ((prefix (+flymake-posframe--prefix diag)))
    (setq +flymake-posframe--last-diag diag)
    (posframe-show
     +flymake-posframe-buffer
     :internal-border-width +flymake-posframe-internal-border-width
     :border-color (face-background '+flymake-posframe-border-face nil t)
     :max-width +flymake-posframe-max-width
     :max-height +flymake-posframe-max-height
     :timeout +flymake-posframe-timeout
     :foreground-color (face-foreground '+flymake-posframe-face nil t)
     :background-color (face-background '+flymake-posframe-face nil t)
     :override-parameters +flymake-posframe-parameters
     :string (concat (when (car prefix)
                       (concat (propertize (car prefix) 'face (cdr prefix)) " "))
                     (flymake-diagnostic-text diag)))
    ;; Keep keyboard focus on the parent frame.
    (when-let* ((buf (get-buffer +flymake-posframe-buffer))
                (frame (buffer-local-value 'posframe--frame buf)))
      (redirect-frame-focus frame (frame-parent frame)))
    (dolist (hook +flymake-posframe-hide-hooks)
      (add-hook hook #'+flymake-posframe-hide nil t))))

(defun +flymake-posframe-display ()
  "Display the diagnostic under point, if any.

Errors are trapped and reported via `message': this runs from
`post-command-hook', and a signal here would skip every remaining hook
function -- including the ones driving completion popups."
  (condition-case err
      (when (and flymake-mode (display-graphic-p))
        (if-let* ((diag (+flymake-posframe--diagnostic-at-point)))
            (unless (and (eq diag +flymake-posframe--last-diag)
                         (+flymake-posframe--visible-p))
              (+flymake-posframe--show diag))
          (+flymake-posframe-hide)))
    (error
     (message "+flymake-posframe-display: %s" (error-message-string err)))))

;;; Mode

;;;###autoload
(define-minor-mode +flymake-posframe-mode
  "Display the flymake diagnostic under point in a child frame."
  :lighter nil
  :group '+flymake-posframe
  (if +flymake-posframe-mode
      (add-hook 'post-command-hook #'+flymake-posframe-display nil t)
    (remove-hook 'post-command-hook #'+flymake-posframe-display t)
    (+flymake-posframe-hide)))

(provide '+flymake-posframe)
;;; +flymake-posframe.el ends here
