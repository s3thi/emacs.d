;;; init-search.el --- Search configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Search tools and configuration.

;;; Code:

;; Use ripgrep for searching.
(use-package rg
  :ensure t
  :init
  (setq rg-command-line-flags '("--sort path"))
  :config
  (rg-enable-default-bindings))

(provide 'init-search)

;;; init-search.el ends here
