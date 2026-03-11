;;; init-ui.el --- User interface configuration -*- lexical-binding: t -*-

;;; Commentary:
;; UI settings, fonts, scrolling, window navigation, and theme.

;;; Code:

;; Remove UI elements we don't like. Some UI elements are configured in
;; early-init.el for faster startup.
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

;; And add UI elements that are nice to have.
(column-number-mode 1)
(show-paren-mode 1)

;; Ask before quitting.
(setq confirm-kill-emacs #'yes-or-no-p)

;; Enable right click menus.
(when (display-graphic-p)
  (context-menu-mode))

;; Allow resizing the frame by pixels instead of rounding the frame size to
;; characters. This prevents an irritating situation on macOS where sometimes
;; there's a bit of space left between the bottom of the Emacs frame and the
;; bottom of the screen when maximizing Emacs using an external window manager.
(setq frame-resize-pixelwise t)

;; Disable mouse wheel text scaling. This gets triggered accidentally on
;; trackpads and is never useful.
(defalias 'mouse-wheel-text-scale #'ignore)

;; Show total number of matches while searching.
(setq-default isearch-lazy-count t)

;; Use ibuffer instead of the default buffer list UI.
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

;; minions-mode only shows the current major mode in the modeline, tucking away
;; all the minor modes into a menu.
(use-package minions
  :ensure t
  :config
  (minions-mode 1))

;; which-key displays helpful suggestions in the minibuffer when you hit the
;; first part of a long keychord. Built into Emacs 30+.
(which-key-mode 1)

;; Browse the Emacs undo history visually.
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

;; Set a nice font. The font size will have to be different across platforms, so
;; we check the OS we're running on before setting it.
(when s3thi/is-a-mac
  (set-face-attribute 'default nil
                      :family "Berkeley Mono Variable"
                      :height 160)
  (set-face-attribute 'variable-pitch nil
                      :family "iA Writer Duospace"
                      :height 170))

;; scroll-conservatively specifies the number of lines to scroll the buffer in
;; order to bring the cursor back on the screen when it moves off-screen. If
;; moving that much doesn't bring the cursor back, Emacs will scroll the buffer
;; by as many lines as it takes to bring the cursor to the exact center of the
;; screen.
;;
;; This behavior is irritating and jarring, causing huge jumps as you move
;; around a file. Luckily, setting this variable to a number larger than 100
;; tells Emacs to never scroll by large amounts, and instead scroll the screen
;; just enough to keep the cursor visible.
;;
;; In summary, setting scroll-conservatively to 101 will make Emacs behave
;; like every other text editor on the planet.
(setq scroll-conservatively 101)

;; scroll-margin specifies the number of lines of margin at the top or bottom of
;; the window. As soon as the cursor gets closer than this to the top of or
;; bottom of a window, Emacs will start scrolling.
;;
;; Having a nonzero scroll margin makes mouse scrolling basically impossible.
;; Eh, whatever.
(setq scroll-margin 3)

;; How many lines of continuity to maintain when scrolling by screenfuls.
(setq next-screen-context-lines 5)

;; Enable smooth scrolling.
(pixel-scroll-precision-mode 1)

;; Use savehist to save minibuffer history.
(use-package savehist
  :init
  (savehist-mode))

;; Use ace-window for navigating open windows.
(use-package ace-window
  :ensure t
  :bind* (("M-o" . ace-window))
  :init
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame))

;; Use avy to jump around.
(use-package avy
  :ensure t
  :bind* (("C-'" . avy-goto-char-2)))

;; Let's install some Doom Emacs themes!
(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; Load the theme.
  (load-theme 'doom-one t))

;; Let's also use the nice modeline from Doom Emacs.
(use-package doom-modeline
  :ensure t
  :init
  ;; Use icons.
  (setq doom-modeline-icon t)
  ;; Don't display encoding.
  (setq doom-modeline-buffer-encoding nil)
  ;; Don't display VCS state.
  (setq doom-modeline-vcs-icon nil)
  ;; Don't display error information.
  (setq doom-modeline-check-icon nil)
  ;; Show project name.
  (setq doom-modeline-project-name t)
  ;; Show LSP status.
  (setq doom-modeline-lsp t)
  ;; Show word count.
  (setq doom-modeline-enable-word-count t)
  ;; Major modes in which to display word count continuously.
  (setq doom-modeline-continuous-word-count-modes
        '(markdown-mode gfm-mode org-mode))
  (doom-modeline-mode 1))

;; We want nice icons.
(use-package nerd-icons
  :ensure t
  :init
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

;; ... and we want to show them in dired.
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; ... and in ibuffer.
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; ... and in completion UIs.
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; The following function adds a top padding to the current buffer. It does so
;; by setting the header line to an empty string, increasing its line height,
;; and setting its background to be the same as the buffer background. The
;; downside of doing this is that it may mess up the header line in modes that
;; actually make use of it (e.g. Eglot).
(defun s3thi/add-top-padding ()
  "Add top padding to the current buffer."
  (interactive)
  (unless (bound-and-true-p s3thi/--top-padding-applied)
    (setq-local s3thi/--top-padding-applied t)
    (setq-local header-line-format "")
    (face-remap-add-relative 'header-line :height 400)
    (let ((bg (face-attribute 'default :background)))
      (face-remap-add-relative 'header-line :background bg))))

(provide 'init-ui)

;;; init-ui.el ends here
