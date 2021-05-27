(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-rust-server "rust-analyzer")
  :hook
  ((rust-mode . lsp)
   (csharp-mode . lsp)))

(use-package lsp-ui
  :ensure t)

(use-package lsp-ivy
  :ensure t)

(use-package lsp-treemacs
  :ensure t)

(provide 'init-lsp)
