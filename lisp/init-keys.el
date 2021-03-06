(when *is-a-mac*
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'meta)
  (setq mac-right-command-modifier 'super)
  (setq mac-option-modifier 'super)
  (setq mac-right-option-modifier nil)
  (setq mac-function-modifier 'hyper))

(global-set-key (kbd "C-c v") #'delete-other-windows-vertically)
(global-set-key (kbd "C-c b") #'rename-buffer)
(global-set-key (kbd "C-c e") #'flycheck-list-errors)
(global-set-key (kbd "C-`") #'other-frame)

(use-package windmove
  :config
  (windmove-default-keybindings))

(global-set-key (kbd "s-0") #'delete-window)
(global-set-key (kbd "s-1") #'delete-other-windows)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") #'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") #'split-and-follow-vertically)

(global-set-key (kbd "C-<left>") #'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") #'shrink-window)
(global-set-key (kbd "C-<up>") #'enlarge-window)

(defun copy-file-path ()
  "Copy the path of the file currently open in the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (kill-new filename)
          (message "Filename placed in kill ring"))
      (message "Current buffer is not associated with a file"))))

(defun unfill-paragraph ()
  "The inverse of 'fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

(global-set-key (kbd "C-c f") #'copy-file-path)
(global-set-key (kbd "M-Q") #'unfill-paragraph)

(defun quit-other-window ()
  "Quits the other window."
  (interactive)
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (when (eq (key-binding "q") 'quit-window)
      (quit-window nil win-other))
    (select-window win-curr)))

(global-set-key (kbd "C-c q") #'quit-other-window)

(defun crux-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (insert "\n")
  (if electric-indent-inhibit
      ;; We can't use `indent-according-to-mode' in languages like Python,
      ;; as there are multiple possible indentations with different meanings.
      (let* ((indent-end (progn (crux-move-to-mode-line-start) (point)))
             (indent-start (progn (move-beginning-of-line nil) (point)))
             (indent-chars (buffer-substring indent-start indent-end)))
        (forward-line -1)
        ;; This new line should be indented with the same characters as
        ;; the current line.
        (insert indent-chars))
    ;; Just use the current major-mode's indent facility.
    (forward-line -1)
    (indent-according-to-mode)))

(defun crux-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (crux-smart-open-line-above)
    (move-end-of-line nil)
    (newline-and-indent)))

(global-set-key (kbd "<S-return>") #'crux-smart-open-line)
(global-set-key (kbd "<C-S-return>") #'crux-smart-open-line-above)

(defun crux-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun crux-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)


(defun crux-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c D") #'crux-delete-file-and-buffer)

(defun crux-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer)

(defun setup-athena-workspace ()
  (interactive)

  ;; Open the main App container.
  (find-file "~/Code/athena_web/frontend/src/containers/App.js")

  ;; Open treemacs.
  (treemacs-select-window)

  ;; Focus back on the main buffer.
  (other-window 1)

  ;; Create a new eyebrowse arrangement.
  (eyebrowse-switch-to-window-config-2)

  ;; Open a terminal, switch to the project directory.
  (vterm "docker-term")
  (vterm-insert "cd ~/Code/athena_web/")
  (vterm-send-return)

  ;; Open another terminal in a split and switch to the project directory.
  (split-and-follow-horizontally)
  (vterm "npm-term")
  (vterm-insert "cd ~/Code/athena_web/frontend/")
  (vterm-send-return)

  ;; Another terminal, this one for tests.
  (split-and-follow-vertically)
  (vterm "test-term")
  (vterm-insert "cd ~/Code/athena_web/frontend/")
  (vterm-send-return)

  ;; A final terminal, this one for random tasks.
  (other-window 1)
  (split-and-follow-vertically)
  (vterm "frontend-term")
  (vterm-insert "cd ~/Code/athena_web/frontend/")
  (vterm-send-return)

  ;; Switch back to the main window config.
  (eyebrowse-switch-to-window-config-1))

(global-set-key (kbd "C-c w") #'setup-athena-workspace)

(provide 'init-keys)
