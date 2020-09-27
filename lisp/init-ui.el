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

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(provide 'init-ui)
