;;; early-init.el --- Early init file for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This package sets up early initialization options for my Emacs
;; config.

;;; Code:

;; Increase GC threshold.
(setq gc-cons-threshold (* 128 1024 1024))

;; Disable UI elements early for faster startup.
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Use dark mode and transparent titlebar on macOS.
(when (eq system-type 'darwin)
  (push '(ns-appearance . dark) default-frame-alist)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Don't resize frame when font size, scroll bars, etc. change.
(setq frame-inhibit-implied-resize t)

;; Don't make packages available at startup. We'll enable them later.
(setq package-enable-at-startup nil)

;; Suppress echo area message.
(setq inhibit-startup-echo-area-message user-login-name)

(provide 'early-init)

;;; early-init.el ends here
