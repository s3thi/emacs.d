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

(use-package company-box
  :ensure t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

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
  ("C-c f" . #'projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map))

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
  (setq rg-keymap-prefix (kbd "C-c s"))
  :config
  (rg-enable-default-bindings))
  
(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode))

(use-package yasnippet
  :ensure t
  :after company
  :diminish
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-;") yas-maybe-expand))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

(use-package docker
  :ensure t
  :bind ("C-c o" . docker))

(global-set-key (kbd "M-;") #'comment-line)

(provide 'init-prog)
