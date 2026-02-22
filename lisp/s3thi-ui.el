;;; s3thi-ui.el --- User interface configuration -*- lexical-binding: t -*-

;;; Commentary:
;; General UI settings, fonts, scrolling, minibuffer completion, window
;; navigation, and theme.

;;; Code:

;;;; General ----------------------------------------------------------------------

;; Remove UI elements we don't like. Note that tool-bar-mode and
;; scroll-bar-mode are disabled in early-init.el for faster startup.
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

;; And add UI elements that are nice to have.
(column-number-mode 1)
(show-paren-mode 1)
(setq confirm-kill-emacs #'yes-or-no-p)

;; Allow resizing the frame by pixels instead of rounding the frame size to
;; characters. This prevents an irritating situation on macOS where sometimes
;; there's a bit of space left between the bottom of the Emacs frame and the
;; bottom of the screen when maximizing Emacs using an external window manager.
(setq frame-resize-pixelwise t)

;; Disable mouse wheel text scaling. This gets triggered accidentally and is
;; never useful.
(defalias 'mouse-wheel-text-scale #'ignore)

;; Show total number of matches while searching.
(setq-default isearch-lazy-count t)

;; minions-mode only shows the current major mode in the modeline, tucking
;; away all the minor modes into a menu.
(use-package minions
  :ensure t
  :config
  (minions-mode 1))

;; which-key displays helpful suggestions in the minibuffer when you hit the
;; first part of a long keychord. It is built into Emacs 30.
(which-key-mode 1)

;; vundo lets me browse the Emacs undo history visually. Unlike undo-tree, it
;; works on top of the built-in undo system rather than replacing it, which
;; avoids corruption issues. Call M-x vundo to open the tree, then use f / b
;; to move between branches and n / p to move forward and back in time. Press
;; q to quit.
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

;; Maximize the Emacs frame on startup. To start every new frame maximized
;; instead of just the first frame, you can add the following options to
;; default-frame-alist instead of initial-frame-alist. Additionally, it's
;; possible to toggle the maximized status of an Emacs frame by calling
;; toggle-frame-maximized, which is bound to M-<f10> by default.
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;;; Fonts ------------------------------------------------------------------------

;; Set a nice font. The font size will have to be different across platforms,
;; so we check the OS we're running on before setting it.
(when s3thi/is-a-mac
  (set-face-attribute 'default nil
                      :family "Berkeley Mono Variable"
                      :height 160)
  (set-face-attribute 'variable-pitch nil
                      :family "iA Writer Duospace"
                      :height 180))

;;;; Scrolling --------------------------------------------------------------------

;; scroll-conservatively specifies the number of lines to scroll the buffer in
;; order to bring the cursor back on the screen when it moves off-screen. If
;; moving that much doesn't bring the cursor back, Emacs will scroll the
;; buffer by as many lines as it takes to bring the cursor to the exact center
;; of the screen.
;;
;; This behavior is irritating and jarring, causing huge jumps as you move
;; around a file. Luckily, setting this variable to a number larger than 100
;; tells Emacs to never scroll by large amounts, and instead scroll the screen
;; just enough to keep the cursor visible.
;;
;; In summary, setting scroll-conservatively to 101 will make Emacs behave
;; like every other text editor on the planet.
(setq scroll-conservatively 101)

;; scroll-margin specifies the number of lines of margin at the top or bottom
;; of the window. As soon as the cursor gets closer than this to the top of or
;; bottom of a window, Emacs will start scrolling.
(setq scroll-margin 3)

;; Enable smooth scrolling.
(pixel-scroll-precision-mode 1)

;;;; Minibuffer completion --------------------------------------------------------

;; Use vertico as the completion UI.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Use savehist to save minibuffer history.
(use-package savehist
  :init
  (savehist-mode))

;; Use orderless to filter completions.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enable annotations in the minibuffer using marginalia.
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;; Window navigation ------------------------------------------------------------

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

;;;; Theme ------------------------------------------------------------------------

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
  (doom-themes-org-config))

;; Load a theme.
(load-theme 'doom-one t)

;; The following function adds a top padding to the current buffer. It does so
;; by setting the header line to an empty string, increasing its line height,
;; and setting its background to be the same as the buffer background. The
;; downside of doing this is that it may mess up the header line in modes that
;; actually make use of it (e.g. Eglot).
;;
;; This is currently not used anywhere.
(defun s3thi/add-top-padding ()
  "Add top padding to the current buffer."
  (interactive)
  (unless (bound-and-true-p s3thi/--top-padding-applied)
    (setq-local s3thi/--top-padding-applied t)
    (setq-local header-line-format "")
    (face-remap-add-relative 'header-line :height 400)
    (let ((bg (face-attribute 'default :background)))
      (face-remap-add-relative 'header-line :background bg))))

(provide 's3thi-ui)

;;; s3thi-ui.el ends here
