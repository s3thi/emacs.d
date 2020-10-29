(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode)
(set-frame-font "Fira Code 12" nil t)
(setq help-window-select t)
(setq visible-bell t)
(winner-mode 1)
(setq browse-url-browser-function 'xwidget-webkit-browse-url)

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
  ("C-}" . #'persp-next)
  ("C-{" . #'persp-prev)
  ("C-`" . #'persp-switch-last)
  ("C-\"" . #'persp-switch)
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

(provide 'init-ui)
