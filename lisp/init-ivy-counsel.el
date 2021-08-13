(use-package ivy
  :ensure t
  :diminish
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :ensure t
  :diminish
  :config
  (counsel-mode))

(provide 'init-ivy-counsel)
