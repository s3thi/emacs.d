(setq inhibit-startup-screen t)
(tool-bar-mode -1)

(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (not (display-graphic-p))
  (menu-bar-mode -1))

(column-number-mode 1)
(show-paren-mode)

(when *is-a-mac*
    (set-frame-font "Rec Mono Duotone 14" nil t))

(when *is-a-pc*
    (set-frame-font "Rec Mono Duotone 11" nil t))

;; Enable emoji on macOS.
(when (boundp 'set-fontset-font)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(setq visible-bell t)

;; Winner mode lets you undo changes in the window configuration.
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
  (load-theme 'gruvbox-dark-soft t))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

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
