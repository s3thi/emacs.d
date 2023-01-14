;; init.el --- Initialization file for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; Initialization file for Emacs.

;;; Code:

;; Initialize package.
(require 'package)
(add-to-list 'package-archives
             '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Load Org-Babel defined config.
(org-babel-load-file
 (concat user-emacs-directory "init.org"))

(provide 'init)

;;; init.el ends here.
