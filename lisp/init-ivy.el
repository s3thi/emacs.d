(use-package ivy
  :ensure t
  :diminish
  :config
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

(provide 'init-ivy)
