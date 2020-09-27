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

(global-set-key (kbd "C-c k") #'delete-other-windows-vertically)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(defun load-init-file ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun load-user-emacs-directory ()
  (interactive)
  (find-file user-emacs-directory))

(global-set-key (kbd "C-c i i") #'load-init-file)
(global-set-key (kbd "C-c i d") #'load-user-emacs-directory)

(provide 'init-keys)
