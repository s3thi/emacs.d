(setq-default indent-tabs-mode nil)

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . #'er/expand-region))

(setq save-interprogram-paste-before-kill t)

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(provide 'init-editing)
