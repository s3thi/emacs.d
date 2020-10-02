(use-package magit
  :ensure t)

(setq auth-sources '("~/.authinfo"))

(use-package forge
  :ensure t
  :after magit)

(global-set-key (kbd "s-g") 'magit-status)

(provide 'init-magit)
