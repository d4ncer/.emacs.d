;;; -*- lexical-binding: t; -*-

(defvar +theme-light nil)
(defvar +theme-dark nil)

(defvar +theme-changed-hook nil)

(cl-defgeneric +system-theme-query (system-type))

(cl-defmethod +system-theme-query ((_ (eql 'darwin)))
  (shell-command-to-string "defaults read -g AppleInterfaceStyle"))

(cl-defmethod +system-theme-query ((_ (eql 'gnu/linux)))
  (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme"))

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
