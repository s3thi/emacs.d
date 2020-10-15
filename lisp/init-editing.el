(setq-default indent-tabs-mode nil)

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . #'er/expand-region))

(use-package smartparens
  :ensure t
  :diminish
  :config
  (require 'smartparens-config)
  (sp-with-modes '(js-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  (add-hook 'prog-mode-hook #'smartparens-mode))

(setq save-interprogram-paste-before-kill t)

(provide 'init-editing)
