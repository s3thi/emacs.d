(use-package counsel
  :after ivy
  :ensure t
  :bind
  ("C-x C-r" . #'counsel-recentf)
  :config
  (counsel-mode))

(provide 'init-counsel)
