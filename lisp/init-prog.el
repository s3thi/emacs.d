(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort))
  :config
  (setq company-idle-delay 0.1)
  (setq company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind
  ("C-c [" . #'flycheck-previous-error)
  ("C-c ]" . #'flycheck-next-error))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  :bind-keymap
  ("C-," . projectile-command-map))

(use-package deadgrep
  :ensure t
  :bind ("C-c d" . #'deadgrep))

(provide 'init-prog)
