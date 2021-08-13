(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-rust-server "rust-analyzer")
  :hook
  ((rust-mode . lsp)
   (csharp-mode . lsp)))

(provide 'init-lsp)
