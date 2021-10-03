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
(column-number-mode 1)
(show-paren-mode)
(setq visible-bell t)

;; Use a built-in theme.
(load-theme 'tango-dark t)

;; Set fonts.
(when *is-a-mac*
    (set-frame-font "Fira Code 14" nil t))

(when *is-a-pc*
  (set-frame-font "Fira Code 11" nil t))

(when *is-a-linux*
  (set-frame-font "Fira Code 10" nil t))

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

;; Package to mark logical regions of code.
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . #'er/expand-region))

;; Put the most recently killed/yanked text into the system clipboard.
(setq save-interprogram-paste-before-kill t)

;; Use ivy as a completion framework.
(use-package ivy
  :ensure t
  :diminish
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (ivy-mode 1))

;; Use counsel to enhance emacs functions with ivy.
(use-package counsel
  :after ivy
  :ensure t
  :diminish
  :config
  (counsel-mode))

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
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  :bind
  ("C-c f" . #'projectile-find-file)
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

;; Display line numbers, but only when in a source code file.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; Language server for JS and TS.
(use-package tide
  :ensure t
  :init
  (add-hook 'js-mode-hook #'setup-tide-mode))

;; It's surprising that this isn't included in Emacs by default?!
(use-package json-mode
  :ensure t)


;; Syntax highlighting for Rust.
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

;; LSP config.
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-rust-server "rust-analyzer")
  :hook
  ((rust-mode . lsp)))

;; Markdown.
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . visual-line-mode)
  :init (setq markdown-command "multimarkdown"))

;; Use ripgrep for searching.
(use-package rg
  :ensure t
  :init
  (setq rg-keymap-prefix (kbd "C-c s"))
  :config
  (rg-enable-default-bindings))

(provide 'init)

;;; init.el ends here
