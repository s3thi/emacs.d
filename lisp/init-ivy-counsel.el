(use-package ivy
  :ensure t
  :diminish
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (ivy-mode 1))

(use-package prescient
  :ensure t
  :after (ivy counsel)
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :ensure t
  :after prescient
  :config
  (ivy-prescient-mode))

(use-package counsel
  :after ivy
  :ensure t
  :diminish
  :config
  (counsel-mode))

(provide 'init-ivy-counsel)
