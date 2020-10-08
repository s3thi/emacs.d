(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode)
(set-frame-font "Fira Code 12" nil t)
(setq help-window-select t)
(setq visible-bell t)

(use-package diminish
  :after use-package
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

(use-package perspective
  :ensure t
  :diminish
  :config
  (persp-mode)
  (global-set-key (kbd "C-x C-b") #'persp-ibuffer)
  (setq persp-state-default-file (expand-file-name "perspective.el" user-emacs-directory))
  (add-hook 'kill-emacs-hook #'persp-state-save)
  :bind
  ("C-x b" . #'persp-switch-to-buffer*)
  ("C-x k" . #'persp-kill-buffer*)
  ("s-<tab>" . #'persp-next)
  ("s-`" . #'persp-switch-last)
  :bind-keymap
  ("s-e" . perspective-map))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(provide 'init-ui)
