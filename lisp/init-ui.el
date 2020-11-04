(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode)
(set-frame-font "Fira Code 14" nil t)
(setq help-window-select t)
(setq visible-bell t)
(winner-mode 1)
(setq browse-url-browser-function 'xwidget-webkit-browse-url)
(fset 'yes-or-no-p 'y-or-n-p)

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
  :bind
  ("C-x b" . #'persp-switch-to-buffer*)
  ("C-x k" . #'persp-kill-buffer*)
  ("C-}" . #'persp-next)
  ("C-{" . #'persp-prev)
  ("C-`" . #'persp-switch-last)
  ("C-M-o" . #'persp-switch)
  :bind-keymap
  ("C-c p" . perspective-map))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package buffer-move
  :ensure t
  :bind
  ("M-<right>" . #'buf-move-right)
  ("M-<left>" . #'buf-move-left)
  ("M-<up>" . #'buf-move-up)
  ("M-<down>" . #'buf-move-down))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("M-o" . #'ace-window))

(use-package xwwp
  :ensure t
  :config
  (xwwp-follow-link-completion-system 'ivy)
  :bind
  (:map xwidget-webkit-mode-map
        ("v" . xwwp-follow-link)))

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
