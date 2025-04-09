;;; +pulsar.el --- Supporting functions for pulsar configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defmacro +with-eval-pulse (start end &rest body)
  "Pulse a region to indicate whether a command ran or signalled an error.

START and END are the buffer locations to pulse after evaluating BODY.

START & END are evaluated after BODY has completed, and thus after any
buffer modifications have happened."
  (declare (indent 2))
  (let ((gfailed (gensym "failed-"))
        (gerr (gensym "err-"))
        (gstart (gensym "start-"))
        (gend (gensym "end-")))
    `(let (,gfailed)
       (unwind-protect
           (condition-case ,gerr
               (progn ,@body)
             (t
              (setq ,gfailed t)
              (signal (car ,gerr) (cdr ,gerr))))
         (deactivate-mark)
         (when pulsar-mode
           (let ((,gstart ,start)
                 (,gend ,end))
             (when (and ,gstart ,gend)
               (pulsar--pulse nil
                              (if ,gfailed 'pulsar-red 'pulsar-green)
                              ,gstart
                              ,gend))))))))


(provide '+pulsar)

;;; +pulsar.el ends here
