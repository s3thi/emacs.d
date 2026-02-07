;;; init.el --- Initialization file for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; My personal initialization file for Emacs. This file only bootstraps up
;; some basic initialization settings. After that, it loads my literate Emacs
;; configuration from a separate org-mode file.

;;; Code:

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
