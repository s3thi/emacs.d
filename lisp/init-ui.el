(setq inhibit-startup-screen t)
(tool-bar-mode -1)

(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (not (display-graphic-p))
  (menu-bar-mode -1))

(column-number-mode 1)
(show-paren-mode)
(set-frame-font "Iosevka 14" nil t)

(when (boundp 'set-fontset-font)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(setq visible-bell t)
(winner-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(use-package diminish
  :after use-package
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-light-soft t))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "M-j") 'avy-goto-char-2))

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c w"))
  (setq eyebrowse-wrap-around t)
  (setq eyebrowse-switch-back-and-forth t)
  (setq eyebrowse-new-workspace t)
  :config
  (eyebrowse-mode t))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package buffer-move
  :ensure t)

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (setq aw-scope 'frame)
  :bind
  ("M-o" . #'ace-window))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))

(use-package treemacs
  :ensure t
  :config
  (global-set-key (kbd "C-\\") #'treemacs-select-window)
  (setq treemacs-no-png-images t)
  (treemacs-fringe-indicator-mode 'always))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package vc
  :diminish)

(provide 'init-ui)
