;; init.el --- Initialization file for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; Initialization file for Emacs.

;;; Code:

(setq user-full-name "Ankur Sethi"
      user-mail-address "contact@ankursethi.in")

(defvar s3thi/is-a-mac (eq system-type 'darwin))

;; Add ~/.emacs.d/lisp/ to the load path.
(add-to-list 'load-path
             (expand-file-name "lisp/" user-emacs-directory))

;; Load my personal blog helpers.
(require 'alive-and-well)
(aaw-initialize)
(global-set-key (kbd "C-c w") #'aaw-new-post)

;; Store configuration created by custom in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; General settings.
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode)
(global-hl-line-mode 1)
(setq ring-bell-function 'ignore)
(setq confirm-kill-emacs #'yes-or-no-p)
(winner-mode 1)

(when s3thi/is-a-mac
  (set-frame-font "Cascadia Code 14" nil t))

;; Don't make the screen jump when scroll off the top/bottom of the buffer.
;; TODO figure out why/how this setting works.
(setq scroll-conservatively 100)

;; Don't need the initial scratch buffer message.
(setq initial-scratch-message "")

;; Really, really, REALLY use UTF-8 everywhere.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defun s3thi/find-init-file ()
  "Find init file."
  (interactive)
  (find-file user-init-file))

;; Some general keybindings.
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "M-;") #'comment-line)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "<f12>") #'bookmark-bmenu-list)
(global-set-key (kbd "C-<f12>") #'bookmark-set)
(global-set-key (kbd "C-c i") #'s3thi/find-init-file)

;; Disable C-z to suspend in GUI Emacs.
(when window-system
  (global-unset-key (kbd "C-z")))

;; Enable some commands that are disabled by default.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Sentences end with a single space, not double space.
(setq sentence-end-double-space nil)

;; Don't create garbage files.
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)

;; Automatically load files from disk when they change.
(global-auto-revert-mode 1)
(setq dired-auto-revert-buffer t)

;; Always use spaces for indentation.
(setq-default indent-tabs-mode nil)

;; In modes where we are forced to use tabs (e.g go-mode), set tab
;; width to 4.
(setq-default tab-width 4)

;; Fill paragraphs so the lines are 80 characters wide.
(setq-default fill-column 80)

;; Put the most recently killed/yanked text into the system clipboard.
(setq save-interprogram-paste-before-kill t)

;; Store secrets in this file. What could possibly go wrong?
(setq auth-sources '("~/.authinfo"))

;; Initialize package.
(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Setup auto updating for installed packages.
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

;; Configure exec-path.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "WORKON_HOME"))

;; Add node_modules to the exec-path.
(use-package add-node-modules-path
  :ensure t
  :hook js-mode)

;; Hide certain modes from the modeline.
(use-package diminish
  :ensure t)

;; Gruvbox is timeless.
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft t))

;; Modus Operandi is a great light theme, but Modus Vivendi is a bit too harsh
;; for my eyes.
(use-package modus-themes
  :ensure t)

;; Use Vertico for minibuffer completions.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Add annotations in the minibuffer using Marginalia.
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Use Orderless for searching through completions in the minibuffer.
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; I get by with a little help from which-key.
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode 1))

;; Magit is dope.
(use-package magit
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

;; Highlight modified lines on the fly.
(use-package diff-hl
  :ensure t
  :after magit
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

;; Use company for autocomplete menus.
(use-package company
  :ensure t
  :diminish
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort))
  :config
  (setq company-idle-delay 0.1)
  (setq company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil))

(defun s3thi/prog-mode-hook ()
  "Customizations for 'prog-mode' only."
  (setq truncate-lines t) ;; Turn off soft wrapping.
  (display-line-numbers-mode))

(add-hook 'prog-mode-hook #'s3thi/prog-mode-hook)

(global-subword-mode 1)

;; JavaScript indent level.
(setq js-indent-level 2)

;; Treat all JS as JSX.
(add-hook 'js-mode-hook #'js-jsx-enable)

(use-package flycheck
  :ensure t
  :diminish
  :init
  (setq flycheck-indication-mode nil)
  :config
  (global-flycheck-mode))

;; Automatically format JS code with prettier.
(use-package prettier-js
  :diminish
  :ensure t
  :hook ((js-mode . prettier-js-mode)))

;; Syntax highlighting for JSON.
(use-package json-mode
  :ensure t)

;; Syntax highlighting for Rust.
(use-package rustic
  :ensure t
  :diminish
  :init
  (setq rustic-format-on-save t))

;; Support for Go.
(use-package go-mode
  :ensure t
  :hook ((go-mode . (lambda ()
                      (add-hook 'before-save-hook #'gofmt-before-save nil t)))))

;; REST client.
(use-package restclient
  :ensure t)

;; LSP config.
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  :hook ((js-mode . lsp)))

;; Markdown.
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package eldoc
  :diminish)

(use-package hl-todo
  :ensure t
  :diminish
  :hook ((prog-mode . hl-todo-mode)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . #'er/expand-region)))

(use-package string-inflection
  :ensure t
  :bind (("C-c C-u" . #'string-inflection-all-cycle)))

(use-package move-dup
  :ensure t
  :bind (("M-<up>"   . move-dup-move-lines-up)
         ("C-M-<up>" . move-dup-duplicate-up)
         ("M-<down>"   . move-dup-move-lines-down)
         ("C-M-<down>" . move-dup-duplicate-down)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (global-set-key (kbd "C-c y") #'yas-insert-snippet)
  (global-set-key (kbd "C-;") #'yas-expand))

(use-package crux
  :ensure t
  :bind (("C-c o" . #'crux-open-with)
         ("C-c u" . #'crux-view-url)
         ("C-c e" . #'crux-eval-and-replace)
         ("C-c d" . #'crux-duplicate-current-line-or-region)
         ("C-c D" . #'crux-delete-file-and-buffer)
         ("C-c r" . #'crux-rename-file-and-buffer)
         ("C-c k" . #'crux-kill-other-buffers)
         ("C-c S" . #'crux-find-shell-init-file)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package buffer-move
  :ensure t)

(use-package browse-kill-ring
  :ensure t)

;; Start the Emacs server.
(unless (server-running-p)
  (server-start))

(provide 'init)

;;; init.el ends here
