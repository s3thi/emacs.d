(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(column-number-mode 1)
(show-paren-mode)
(set-frame-font "Fira Code 12" nil t)
(global-hl-line-mode t)
(setq help-window-select t)

(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package perspective
  :ensure t
  :config
  (persp-mode)
  (global-set-key (kbd "C-x C-b") #'persp-ibuffer)
  (setq persp-state-default-file (expand-file-name "perspective.el" user-emacs-directory))
  (add-hook 'kill-emacs-hook #'persp-state-save)
  :bind
  ("C-x b" . #'persp-switch-to-buffer*)
  ("C-x k" . #'persp-kill-buffer*)
  ("s-<tab>" . #'persp-next)
  :bind-keymap
  ("s-e" . perspective-map))

(provide 'init-ui)
