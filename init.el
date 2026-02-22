;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs configuration for Ankur Sethi <contact@ankursethi.com>.
;;
;; This file loads the configuration modules from the lisp/ directory.
;; Platform-specific early initialization lives in early-init.el.

;;; Code:

(add-to-list
 'load-path
 (expand-file-name "lisp/init/" user-emacs-directory))

(require 'init-package)
(require 'init-essentials)
(require 'init-ui)
(require 'init-completion)
(require 'init-dired)
(require 'init-project)
(require 'init-editing)
(require 'init-prose)
(require 'init-markdown)
(require 'init-org)
(require 'init-notes)
(require 'init-search)
(require 'init-vc)
(require 'init-terminal)
(require 'init-programming) ;; Break into general programming and web
(require 'init-input-methods)

;; Start the server.
(unless (server-running-p)
  (server-start))

(provide 'init)

;;; init.el ends here
