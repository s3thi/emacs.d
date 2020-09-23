;; Use ~/.emacs.d/lisp/ to store custom configurations, broken down
;; by areas of functionality.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

;; Settings for uniquify.
(require 'init-uniquify)

;; Ivy, Counsel, and Swiper.
(require 'init-ivy)
(require 'init-counsel)
(require 'init-swiper)

;; File related settings.
(require 'init-files)

;; Terminal settings.
(require 'init-term)

;; Window management settings.
(require 'init-windows)

;; JavaScript specific settings.
(require 'init-js)
