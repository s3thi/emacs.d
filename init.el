;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs configuration for Ankur Sethi <contact@ankursethi.com>.
;;
;; This file loads the configuration modules from the lisp/ directory.
;; Platform-specific early initialization lives in early-init.el.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 's3thi-bootstrap)
(require 's3thi-ui)
(require 's3thi-files)
(require 's3thi-editing)
(require 's3thi-writing)
(require 's3thi-search)
(require 's3thi-vc)
(require 's3thi-terminal)
(require 's3thi-programming)
(require 's3thi-keys)

;; Let's start the server.
(server-start)

(provide 'init)

;;; init.el ends here
