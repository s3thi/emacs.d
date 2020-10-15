;; Swap Ctrl and Cmd on macOS.
(when *is-a-mac*
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'super))

;; I don't want to use arrow keys for navigating through text.
(global-unset-key [M-left])
(global-unset-key [M-right])
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

(defun load-init-file ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun load-user-emacs-directory ()
  (interactive)
  (find-file user-emacs-directory))

(global-set-key (kbd "s-l") #'goto-line)
(global-set-key (kbd "C-c 0") #'delete-other-windows-vertically)

(use-package windmove
  :config
  (windmove-default-keybindings))

(global-set-key (kbd "s-0") #'delete-window)
(global-set-key (kbd "s-1") #'delete-other-windows)
(global-set-key (kbd "s-2") #'split-window-below)
(global-set-key (kbd "s-3") #'split-window-right)

(provide 'init-keys)
