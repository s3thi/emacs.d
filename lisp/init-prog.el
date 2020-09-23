(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (setq company-tooltip-align-annotations t))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind
  ("C-c [" . #'flycheck-previous-error)
  ("C-c ]" . #'flycheck-next-error))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  :bind-keymap
  ("C-," . projectile-command-map))

(use-package deadgrep
  :ensure t
  :bind ("C-c d" . #'deadgrep))

(provide 'init-prog)
