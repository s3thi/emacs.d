;;; init.el --- Initialization file for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; My personal initialization file for Emacs. This file only bootstraps up a
;; package manager and some basic initialization settings. After that, it loads
;; my literate Emacs configuration from a separate org-mode file.

;;; Code:

;; Initialize package and enable MELPA.
(require 'package)
(add-to-list 'package-archives
             '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Update package list if it doesn't exist.
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Set up auto-updating for installed packages.
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

;; Make sure we load org-mode before anything else so we can tangle our literate
;; configuration.
(use-package org)

;; Define configuration file paths.
(defvar s3thi/cfg-file-base
  (concat user-emacs-directory "README"))
(defvar s3thi/cfg-file-in
  (concat s3thi/cfg-file-base ".org"))
(defvar s3thi/cfg-file-out
  (concat s3thi/cfg-file-base ".el"))

;; Tangle configuration file from the org-mode input.
(when (file-newer-than-file-p s3thi/cfg-file-in
			      s3thi/cfg-file-out)
  (org-babel-tangle-file s3thi/cfg-file-in s3thi/cfg-file-out))

;; Load tangled configuration file.
(load s3thi/cfg-file-out)

(provide 'init)

;;; init.el ends here.
