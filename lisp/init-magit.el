(use-package magit
  :ensure t)

(setq auth-sources '("~/.authinfo"))

(use-package forge
  :ensure t
  :after magit)

(global-set-key (kbd "s-m") 'magit-status)

(provide 'init-magit)
