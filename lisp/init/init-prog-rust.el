;;; init-prog-rust.el --- Rust development configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Rust support.

;;; Code:

(use-package rust-mode
  :ensure t
  :hook ((rust-mode . eglot-ensure))
  :init
  (setq rust-mode-treesitter-derive t)
  (setq rust-format-on-save t))

(use-package rustic
  :ensure t
  :after (rust-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(provide 'init-prog-rust)

;;; init-prog-rust.el ends here
