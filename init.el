;; init.el --- Initialization file for Emacs

;;; Commentary:
;; Initialization file for Emacs.

;;; Code:

;; I like to use different settings on different systems.
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-pc* (eq system-type 'windows-nt))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; Store configuration created by custom in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Minor UI settings.
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode)
(setq visible-bell t)

;; Disable C-z to suspend in GUI Emacs.
(when window-system
  (global-unset-key (kbd "C-z")))

;; Enable some commands that are disabled by default.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Clicking a file in dired should open it in the same window.
(eval-after-load "dired"
  `(progn
     (define-key dired-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file)))

;; Set fonts.
(when *is-a-mac*
    (set-frame-font "DM Mono 14" nil t))

(when *is-a-pc*
  (set-frame-font "Fira Code 11" nil t))

(when *is-a-linux*
  (set-frame-font "DM Mono 11" nil t))

;; Enable emoji on macOS.
(when (boundp 'set-fontset-font)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; Initialize package.
(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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

;; Store secrets in this file. What could possibly go wrong?
(setq auth-sources '("~/.authinfo"))

;; Magit is dope.
(use-package magit
  :ensure t)

;; Projectile for project management, because I'm too lazy to figure
;; out project.el.
(use-package projectile
  :ensure t
  :diminish
  :init
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

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
  (setq company-selection-wrap-around t))

;; Display line numbers.
(global-display-line-numbers-mode t)

;; Turn off soft wrapping for source code only.
(add-hook 'prog-mode-hook
          (lambda () (setq truncate-lines t)))

;; JavaScript indent level.
(setq js-indent-level 2)

;; Treat all JS as JSX.
(add-hook 'js-mode-hook #'js-jsx-enable)

;; Shortcut for commenting a line.
(global-set-key (kbd "M-;") #'comment-line)

;; Automatically format JS code with prettier.
(use-package prettier-js
  :ensure t
  :init
  (add-hook 'js-mode-hook #'prettier-js-mode))

(defun setup-tide-mode ()
  "Run tide, eldoc, flycheck, company when entering a JS file."
  (interactive)
  (if buffer-file-name
      (progn
        (tide-setup)
        (flycheck-mode +1)
        (setq flycheck-check-syntax-automatically '(save mode-enabled))
        (eldoc-mode +1)
        (tide-hl-identifier-mode +1)
        (company-mode +1))))

;; Language server for JS and TS.
(use-package tide
  :ensure t
  :init
  (add-hook 'js-mode-hook #'setup-tide-mode))

;; It's surprising that this isn't included in Emacs by default?!
(use-package json-mode
  :ensure t)

;; Syntax highlighting for Rust.
(use-package rustic
  :ensure t
  :diminish)

;; Support for Go.
(use-package go-mode
  :ensure t
  :hook ((go-mode . (lambda ()
                      (add-hook 'before-save-hook #'gofmt-before-save nil t)))))

;; REST client.
(use-package restclient
  :ensure t)

;; Bookmarks.
(use-package bm
  :ensure t
  :init
  (setq bm-restore-repository-on-load t)
  :config
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  (setq-default bm-buffer-persistence t)
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  :bind (("C-<f2>" . #'bm-toggle)
         ("<f2>" . #'bm-next)
         ("M-<f2>" . #'bm-previous)))

;; LSP config.
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"))

;; Markdown.
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . visual-line-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Start a server so other clients can connect to this.
(server-start)

(provide 'init)

;;; init.el ends here
