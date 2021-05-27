(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Working directory should be the home directory.
(when (string= default-directory "/")
  (setq default-directory "~/")
  (with-current-buffer "*Messages*"
    (setq default-directory "~/")))

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-pc* (eq system-type 'windows-nt))

;; Store configuration created by custom in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Configure the Emacs exec-path.
(require 'init-exec-path)

;; UI configuration.
(require 'init-ui)

;; Custom keybindings.
(require 'init-keys)

;; General programming settings.
(require 'init-prog)

;; Ivy and Counsel.
(require 'init-ivy-counsel)

;; File related settings.
(require 'init-files)

;; Settings for editing text.
(require 'init-editing)

;; Magit settings.
(require 'init-git)

;; Terminal.
(require 'init-term)

;; JavaScript specific settings.
(require 'init-js)

;; Python specific settings.
(require 'init-python)

;; Rust specific settings.
(require 'init-rust)

;; C# specific settings.
(require 'init-csharp)

;; LSP
(require 'init-lsp)
