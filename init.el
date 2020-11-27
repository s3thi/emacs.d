;; Use ~/.emacs.d/lisp/ to store custom configurations, broken down
;; by areas of functionality.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Working directory should be the home directory.
(when (string= default-directory "/")
  (setq default-directory "~/")
  (with-current-buffer "*Messages*"
    (setq default-directory "~/")))

(defconst *is-a-mac* (eq system-type 'darwin))

;; Store configuration created by custom in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Initialize packages and elpa.
(require 'init-elpa)

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
(require 'init-ivy)
(require 'init-counsel)

;; File related settings.
(require 'init-files)

;; Settings for editing text.
(require 'init-editing)

;; Magit settings.
(require 'init-git)

;; Terminal.
(require 'init-term)

;; Misc.
(require 'init-misc)

;; JavaScript specific settings.
(require 'init-js)

;; Python specific settings.
(require 'init-python)

;; Rust specific settings.
(require 'init-rust)

;; LSP
(require 'init-lsp)
