;;; +agent-shell-pstack-card.el --- Inline pstack requests -*- lexical-binding: t; -*-

;;; Commentary:
;; Render complete pstack-panel fences as cards.  Display text properties
;; survive temporary markdown buffers and viewport copies without changing
;; the source.  Refresh an existing conversation with
;; `+agent-shell-pstack-card-refresh'.

;;; Code:

(require 'agent-shell-markdown)
(require '+agent-shell-pstack-panel)

(declare-function agent-shell-shell-buffer "agent-shell")
(declare-function agent-shell-session-id "agent-shell")

(defgroup +agent-shell-pstack-card nil
  "Readable pstack requests in agent-shell."
  :group '+agent-shell-pstack)

(defface +agent-shell-pstack-card-title
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the request shape."
  :group '+agent-shell-pstack-card)

(defface +agent-shell-pstack-card-detail
  '((t :inherit shadow))
  "Face for card controls and panel state."
  :group '+agent-shell-pstack-card)

(defface +agent-shell-pstack-card-error
  '((t :inherit error))
  "Face for rejected requests."
  :group '+agent-shell-pstack-card)

(defvar +agent-shell-pstack-card-map (make-sparse-keymap)
  "Text property keymap for cards and their source.")

(defvar +agent-shell-pstack-card-mode)

(defun +agent-shell-pstack-card--wrap (text width)
  "Return TEXT wrapped to WIDTH columns with two spaces of indentation."
  (with-temp-buffer
    (insert text)
    (let ((fill-column width)
          (fill-prefix "  ")
          (sentence-end-double-space nil))
      (indent-region (point-min) (point-max) 2)
      (fill-region (point-min) (point-max)))
    (string-trim-right (buffer-string))))

(defun +agent-shell-pstack-card--text (request &optional panel expanded)
  "Return a card for parsed REQUEST and live PANEL, with brief EXPANDED.
REQUEST may be a rejection string.  This function reads only its arguments."
  (let ((lines
         (if (stringp request)
             (list (propertize (concat "pstack-panel rejected: " request)
                               'face '+agent-shell-pstack-card-error))
           (let* ((shape (+agent-shell-pstack-panel-shape request))
                  (members (+agent-shell-pstack-panel-members request))
                  (workers (seq-remove
                            (lambda (member)
                              (eq (+agent-shell-pstack-panel-member-kind member)
                                  'judge))
                            members))
                  (brief (split-string
                          (+agent-shell-pstack-card--wrap
                           (+agent-shell-pstack-panel-brief request) 72) "\n"))
                  (folded (> (length brief) 3)))
             (append
              (list
               (propertize
                (format "pstack %s · %s" shape
                        (pcase shape
                          ('how "investigate and explain")
                          ('interrogate "adversarial review")
                          ('why "find the reasons, with sources")
                          ('swarm "check each slice")
                          ('arena "implement, compare and judge")
                          ('delegate "implement, test and commit")))
                'face '+agent-shell-pstack-card-title)
               (format "%d member%s" (length workers)
                       (if (= 1 (length workers)) "" "s")))
              (mapcar
               (lambda (member)
                 (+agent-shell-pstack-card--wrap
                  (format "%s · %s · %s"
                          (+agent-shell-pstack-panel-member-role member)
                          (+agent-shell-pstack-panel-member-agent member)
                          (or (+agent-shell-pstack-panel-member-focus member)
                              "no focus specified")) 72))
               workers)
              (list "Brief:")
              (if (and folded (not expanded)) (seq-take brief 3) brief)
              (when folded
                (list (propertize
                       (if expanded "  [TAB: fold brief]"
                         "  … [TAB: expand brief]")
                       'face '+agent-shell-pstack-card-detail)))
              (when (eq shape 'arena)
                (append
                 (list "Rubric:")
                 (mapcar (lambda (criterion)
                           (+agent-shell-pstack-card--wrap
                            (concat "• " criterion) 72))
                         (+agent-shell-pstack-panel-rubric request))
                 (list (format "Judge: %s"
                               (+agent-shell-pstack-panel-member-agent
                                (seq-find
                                 (lambda (member)
                                   (eq 'judge
                                       (+agent-shell-pstack-panel-member-kind
                                        member))) members))))))
              (list (if (eq shape 'arena)
                        (format "Timeouts: candidates %ss · judge %ss"
                                (+agent-shell-pstack-panel-timeout request)
                                (+agent-shell-pstack-panel-judge-timeout request))
                      (format "Timeout: %ss"
                              (+agent-shell-pstack-panel-timeout request)))))))))
    (when panel
      (setq lines
            (append lines
                    (list (propertize
                           (format "Panel %s · %s · C-c P shows it"
                                   (+agent-shell-pstack-panel-id panel)
                                   (+agent-shell-pstack-panel-phase panel))
                           'face '+agent-shell-pstack-card-detail)))))
    (string-join
     (append lines
             (list (propertize "[RET or click: source]"
                               'face '+agent-shell-pstack-card-detail)))
     "\n")))

(defun +agent-shell-pstack-card--find (session block)
  "Find the panel requested by BLOCK in SESSION, including id collisions."
  (let* ((short-id (+agent-shell-pstack-panel--id (or session "") block))
         (digest (secure-hash 'sha256 (concat session "\0" block)))
         (previous (gethash short-id +agent-shell-pstack-panel--request-digests)))
    (+agent-shell-pstack-panel--find
     (if (and previous (not (equal previous digest))) digest short-id))))

(defun +agent-shell-pstack-card--panel (start)
  "Find the panel for the request at START using its session and block."
  (let* ((block (get-text-property start '+agent-shell-pstack-card-source))
         (lead (cond
                ((local-variable-p 'agent-shell--state) (current-buffer))
                ((derived-mode-p 'agent-shell-viewport-view-mode)
                 (agent-shell-shell-buffer
                  :viewport-buffer (current-buffer)
                  :no-error t :no-create t)))))
    (if lead
        (+agent-shell-pstack-card--find
         (agent-shell-session-id :shell-buffer lead) block)
      (or (when-let* ((id (get-text-property start '+agent-shell-pstack-card-id)))
            (+agent-shell-pstack-panel--find id))
          ;; Temporary markdown buffers have no shell state.  Only associate
          ;; their source with a panel when exactly one session matches it.
          (let (matches)
            (maphash
             (lambda (_id panel)
               (when-let* ((lead (+agent-shell-pstack-panel-lead panel))
                           ((buffer-live-p lead))
                           (found (+agent-shell-pstack-card--find
                                   (agent-shell-session-id :shell-buffer lead)
                                   block)))
                 (cl-pushnew found matches)))
             +agent-shell-pstack-panel--panels)
            (when (= (length matches) 1) (car matches)))))))

(defun +agent-shell-pstack-card--display (start end)
  "Update the card over START to END, preserving the underlying text."
  (let* ((panel (+agent-shell-pstack-card--panel start))
         (source (get-text-property start '+agent-shell-pstack-card-show-source))
         (text (unless source
                 (+agent-shell-pstack-card--text
                  (get-text-property start '+agent-shell-pstack-card-request)
                  panel
                  (get-text-property start '+agent-shell-pstack-card-expanded)))))
    (with-silent-modifications
      (add-text-properties
       start end
       `(display ,text keymap ,+agent-shell-pstack-card-map pointer hand
         help-echo "RET or click: toggle source; TAB: expand or fold brief"))
      (when panel
        (put-text-property start end '+agent-shell-pstack-card-id
                           (+agent-shell-pstack-panel-id panel))))))

(defun +agent-shell-pstack-card--claim (start end source markdown)
  "Claim START to END for SOURCE, retaining original MARKDOWN for copying."
  (let ((request (condition-case err
                     (+agent-shell-pstack-panel--parse source)
                   (user-error (error-message-string err)))))
    (with-silent-modifications
      (add-text-properties
       start end
       `(+agent-shell-pstack-card-source ,source
         +agent-shell-pstack-card-request ,request
         agent-shell-markdown-source ,markdown
         agent-shell-markdown-frozen t
         rear-nonsticky (agent-shell-markdown-frozen
                         agent-shell-markdown-source
                         +agent-shell-pstack-card-source
                         +agent-shell-pstack-card-request
                         +agent-shell-pstack-card-id
                         +agent-shell-pstack-card-expanded
                         +agent-shell-pstack-card-show-source
                         display keymap pointer help-echo))))
    (+agent-shell-pstack-card--display start end)))

(defun +agent-shell-pstack-card--render-hook (context)
  "Claim only complete pstack-panel blocks in markdown CONTEXT."
  (when +agent-shell-pstack-card-mode
    (dolist (block (map-elt context :source-blocks))
      (when (and (equal (map-elt block :language) "pstack-panel")
                 (map-elt block :complete))
        (let ((start (map-nested-elt block '(:block :start)))
              (end (map-nested-elt block '(:block :end))))
          (unless (get-text-property start '+agent-shell-pstack-card-source)
            (when (eq (char-before end) ?\n) (setq end (1- end)))
            (+agent-shell-pstack-card--claim
             start end (map-elt block :body)
             (buffer-substring-no-properties start end)))))))
  nil)

(defun +agent-shell-pstack-card--visit (function)
  "Call FUNCTION with each claimed span's start and end in this buffer."
  (save-restriction
    (widen)
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (let ((end (next-single-property-change
                    pos '+agent-shell-pstack-card-source nil (point-max))))
          (when (get-text-property pos '+agent-shell-pstack-card-source)
            (funcall function pos end))
          (setq pos end))))))

(defun +agent-shell-pstack-card--update-buffers ()
  "Update panel state in every surviving copy of a card."
  (when +agent-shell-pstack-card-mode
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (+agent-shell-pstack-card--visit
         #'+agent-shell-pstack-card--display)))))

(defun +agent-shell-pstack-card--toggle (property event)
  "Toggle PROPERTY on the card at point, moving to mouse EVENT if supplied."
  (when (mouse-event-p event) (mouse-set-point event))
  (unless (get-text-property (point) '+agent-shell-pstack-card-source)
    (user-error "No pstack card at point"))
  (let* ((start (or (previous-single-property-change
                     (1+ (point)) '+agent-shell-pstack-card-source)
                    (point-min)))
         (end (next-single-property-change
               (point) '+agent-shell-pstack-card-source nil (point-max)))
         (value (not (get-text-property start property))))
    (with-silent-modifications (put-text-property start end property value))
    (+agent-shell-pstack-card--display start end)))

(defun +agent-shell-pstack-card-toggle-source (&optional event)
  "Toggle between the card at point and its original source, or mouse EVENT."
  (interactive (list last-nonmenu-event))
  (+agent-shell-pstack-card--toggle '+agent-shell-pstack-card-show-source event))

(defun +agent-shell-pstack-card-toggle-brief (&optional event)
  "Expand or fold the card's brief at point, or mouse EVENT."
  (interactive (list last-nonmenu-event))
  (+agent-shell-pstack-card--toggle '+agent-shell-pstack-card-expanded event))

(defun +agent-shell-pstack-card--claim-existing ()
  "Claim complete blocks whose fences markdown has already styled away."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^pstack-panel ⧉" nil t)
      (let* ((start (match-beginning 0))
             (body (next-single-property-change
                    start 'agent-shell-markdown-source-block-body
                    nil (point-max))))
        (when (and (get-text-property
                    start 'agent-shell-markdown-source-block-rendered)
                   (< body (point-max))
                   (get-text-property body
                                      'agent-shell-markdown-source-block-body)
                   (not (get-text-property start '+agent-shell-pstack-card-source)))
          (let* ((end (next-single-property-change
                       body 'agent-shell-markdown-source-block-body
                       nil (point-max)))
                 (markdown (get-text-property body 'agent-shell-markdown-source))
                 (source (+agent-shell-pstack-panel--block markdown)))
            (when source
              (+agent-shell-pstack-card--claim start end source markdown))))))))

;;;###autoload
(defun +agent-shell-pstack-card-refresh ()
  "Render pstack cards in the current conversation and update panel state."
  (interactive)
  (unless +agent-shell-pstack-card-mode
    (user-error "Enable +agent-shell-pstack-card-mode first"))
  (save-restriction
    (widen)
    (+agent-shell-pstack-card--claim-existing)
    (+agent-shell-pstack-card--render-hook (agent-shell-markdown-context))
    (+agent-shell-pstack-card--visit #'+agent-shell-pstack-card--display)))

;;;###autoload
(define-minor-mode +agent-shell-pstack-card-mode
  "Toggle readable pstack request cards in agent-shell and its viewports."
  :global t :group '+agent-shell-pstack-card
  (if +agent-shell-pstack-card-mode
      (progn
        (add-hook 'agent-shell-markdown-render-functions
                  #'+agent-shell-pstack-card--render-hook)
        (add-hook '+agent-shell-pstack-panel-change-hook
                  #'+agent-shell-pstack-card--update-buffers)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'agent-shell-mode
                                  'agent-shell-viewport-view-mode)
              (+agent-shell-pstack-card-refresh)))))
    (remove-hook 'agent-shell-markdown-render-functions
                 #'+agent-shell-pstack-card--render-hook)
    (remove-hook '+agent-shell-pstack-panel-change-hook
                 #'+agent-shell-pstack-card--update-buffers)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (+agent-shell-pstack-card--visit
         (lambda (start end)
           (with-silent-modifications
             (remove-text-properties
              start end '(display nil keymap nil pointer nil help-echo nil)))))))))

(provide '+agent-shell-pstack-card)
;;; +agent-shell-pstack-card.el ends here
