;;; init-prog.el --- Programming configuration -*- lexical-binding: t -*-

;;; Commentary:
;; General programming tools: diagnostics, tree-sitter, LSP, and formatting.

;;; Code:

;;;; Flymake ----------------------------------------------------------------------

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;;;; Tree-sitter ------------------------------------------------------------------

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(astro javascript typescript tsx python rust))
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

;;;; Eglot ------------------------------------------------------------------------

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l h" . eldoc)
              ("C-c l R" . eglot-reconnect))
  :config
  (setq eglot-autoshutdown t))

;;;; Code formatting --------------------------------------------------------------

;; apheleia formats code asynchronously on save without blocking Emacs. It
;; supports Prettier and many other formatters out of the box, and preserves
;; point position via RCS patching.
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode 1))

(provide 'init-prog)

;;; init-prog.el ends here
