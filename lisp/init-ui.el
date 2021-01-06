(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode)
(set-frame-font "Fira Code 13" nil t)
(setq visible-bell t)
(winner-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ns-use-proxy-icon nil)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(defun make-frame-title ()
    (format "Workspace %d of %s"
            (eyebrowse--get 'current-slot)
            (mapcar 'car (eyebrowse--get 'window-configs))))

(defun set-frame-title ()
  (setq frame-title-format (make-frame-title)))

(use-package diminish
  :after use-package
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-light-hard t))

(use-package modus-themes
  :ensure t)

(use-package spacemacs-theme
  :ensure t
  :no-require t)

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "M-j") 'avy-goto-char-2))

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "s-w"))
  (setq eyebrowse-wrap-around t)
  (setq eyebrowse-switch-back-and-forth t)
  (setq eyebrowse-new-workspace t)
  :bind
  ("C-{" . #'eyebrowse-prev-window-config)
  ("C-}" . #'eyebrowse-next-window-config)
  ("M-`" . #'eyebrowse-last-window-config)
  ("M-1" . #'eyebrowse-switch-to-window-config-1)
  ("M-2" . #'eyebrowse-switch-to-window-config-2)
  ("M-3" . #'eyebrowse-switch-to-window-config-3)
  ("M-4" . #'eyebrowse-switch-to-window-config-4)
  ("M-5" . #'eyebrowse-switch-to-window-config-5)
  ("M-6" . #'eyebrowse-switch-to-window-config-6)
  ("M-7" . #'eyebrowse-switch-to-window-config-7)
  ("M-8" . #'eyebrowse-switch-to-window-config-8)
  ("M-9" . #'eyebrowse-switch-to-window-config-9)
  :config
  (setq eyebrowse-mode-line-style nil)
  (eyebrowse-mode t)
  (add-hook 'eyebrowse-post-window-switch-hook #'set-frame-title))

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

(provide 'init-ui)
