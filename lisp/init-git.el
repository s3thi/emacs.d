(use-package magit
  :ensure t)

(setq auth-sources '("~/.authinfo"))

(use-package forge
  :ensure t
  :after magit)

(global-set-key (kbd "s-m") 'magit-status)

(use-package git-gutter
  :ensure t
  :diminish
  :config
  (global-git-gutter-mode +1))

(global-set-key (kbd "C-x C-g") 'git-gutter)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

(provide 'init-magit)
