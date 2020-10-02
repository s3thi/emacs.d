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
  ("s-[" . #'flycheck-previous-error)
  ("s-]" . #'flycheck-next-error))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  :bind-keymap
  ("s-p" . projectile-command-map)
  :bind
  ("s-o" . #'projectile-find-file))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings)
  (add-hook 'rg-mode-hook
            '(lambda ()
               (switch-to-buffer-other-window "*rg*"))))

(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode))


(provide 'init-prog)
