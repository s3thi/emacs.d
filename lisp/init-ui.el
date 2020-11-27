(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode)
(set-frame-font "Fira Code 13" nil t)
(setq help-window-select t)
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

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Themes I like: challenger-deep, henna, laserwave, miramare,
  ;; old-hope, peacock, snazzy, rouge, tomorrow-night, zenburn,
  ;; dracula.
  (load-theme 'doom-henna t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-wrap-around t)
  (setq eyebrowse-switch-back-and-forth t)
  (setq eyebrowse-new-workspace t)
  :bind
  ("C-{" . #'eyebrowse-prev-window-config)
  ("C-}" . #'eyebrowse-next-window-config)
  ("C-'" . #'eyebrowse-switch-to-window-config)
  ("C-\"" . #'eyebrowse-rename-window-config)
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
  :bind
  ("M-o" . #'ace-window))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-encoding nil))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(defun split-window-below-ratio (&optional arg)
  "Split the current window 30/70 rather than 50/50.
A single-digit prefix argument gives the top window ARG * 10%."
  (interactive "P")
  (let ((proportion (* (or arg 3) 0.1)))
    (split-window-below (round (* proportion (window-height))))))

(defun split-window-right-ratio (&optional arg)
  "Split the current window 70/30 rather than 50/50.
A single-digit prefix argument gives the left window ARG * 10%."
  (interactive "P")
  (let ((proportion (* (or arg 7) 0.1)))
    (split-window-right (round (* proportion (window-width))))))

(global-set-key (kbd "C-c x 2") #'split-window-below-ratio)
(global-set-key (kbd "C-c x 3") #'split-window-right-ratio)

(provide 'init-ui)
