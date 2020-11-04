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

(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode))

(use-package company-emoji
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package flycheck
  :ensure t
  :diminish
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind
  ("C-<" . #'flycheck-previous-error)
  ("C->" . #'flycheck-next-error))

(use-package projectile
  :ensure t
  :diminish
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  :bind
  ("s-o" . #'projectile-find-file)
  :bind-keymap
  ("s-p" . projectile-command-map))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rg
  :ensure t
  :init
  (setq rg-keymap-prefix (kbd "s-r"))
  :config
  (rg-enable-default-bindings))
  
(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode))

(use-package yasnippet
  :ensure t
  :diminish
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
  :config
  (yas-global-mode 1))

(provide 'init-prog)
