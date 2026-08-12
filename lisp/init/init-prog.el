;;; init-prog.el --- Programming configuration -*- lexical-binding: t -*-

;;; Commentary:
;; General programming configuration.

;;; Code:

;; No soft wrapping in prog-mode.
(add-hook 'prog-mode-hook
          (lambda () (setq-local truncate-lines t)))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(astro javascript typescript tsx python rust))
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l h" . eldoc)
              ("C-c l R" . eglot-reconnect))
  :config
  (setq eglot-autoshutdown t))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode 1))

(provide 'init-prog)

;;; init-prog.el ends here
