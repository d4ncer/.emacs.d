;;; -*- lexical-binding: t; -*-

(defconst +colors-yellow "#b58900")
(defconst +colors-red "#dc322f")
(defconst +colors-magenta "#d33682")
(defconst +colors-blue "#268bd2")
(defconst +colors-green "#859900")

;; KLUDGE nano-subtle is needed for nano-modeline
(defconst +colors-subtle "#ECEFF1")
(defface nano-subtle
  `((t :background ,+colors-subtle))
  "Face with a subtle background color.")

(defvar +theme-light nil)
(defvar +theme-dark nil)

(defvar +theme-changed-hook nil)

(cl-defgeneric +system-theme-query (system-type))

(cl-defmethod +system-theme-query ((_ (eql 'darwin)))
  (shell-command-to-string "defaults read -g AppleInterfaceStyle"))

(cl-defmethod +system-theme-query ((_ (eql 'gnu/linux)))
  (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme"))

(defun +after-enable-theme ()
  "Delete posframes after changing themes."
  (when (fboundp 'posframe-delete-all)
    (posframe-delete-all))
  ;; Force org buffers to refontify to fix org-bullet properties.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (font-lock-flush (point-min) (point-max))))))

(add-hook '+theme-changed-hook #'+after-enable-theme)

;;;###autoload
(defun +theme-for-system-theme ()
  (if (string-match-p "dark" (+system-theme-query system-type))
      +theme-dark
    +theme-light))

;;;###autoload
(defun +theme-update ()
  "Sync the Emacs theme with the system."
  (let* ((inhibit-redisplay t)
         (updated-theme (+theme-for-system-theme)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme updated-theme t)
    (run-hooks '+theme-changed-hook)))

(provide '+theme)
