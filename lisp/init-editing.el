(server-start)
(setq-default indent-tabs-mode nil)
(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

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

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package emojify
  :ensure t
  :after org
  :config
  (setq emojify-emoji-styles '(unicode))
  (setq emojify-display-style 'unicode))

(provide 'init-editing)
