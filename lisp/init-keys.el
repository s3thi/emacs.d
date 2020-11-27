;; Swap Ctrl and Cmd on macOS.
(when *is-a-mac*
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'super))

(global-set-key (kbd "s-l") #'goto-line)
(global-set-key (kbd "s-f") #'flush-lines)
(global-set-key (kbd "M-s-0") #'delete-other-windows-vertically)
(global-set-key (kbd "C-`") #'other-window)

(use-package windmove
  :config
  (windmove-default-keybindings))

(global-set-key (kbd "s-0") #'delete-window)
(global-set-key (kbd "s-1") #'delete-other-windows)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "s-2") #'split-and-follow-horizontally)
(global-set-key (kbd "s-3") #'split-and-follow-vertically)

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

(provide 'init-keys)
