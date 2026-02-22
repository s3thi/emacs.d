;;; init-org.el --- Org configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org-mode editing config.

;;; Code:

;; Note that visual-wrap-prefix-mode, which we use for Markdown, doesn't work
;; well with org-mode when org-indent-mode is also turned on. If you're running
;; into weird issues, it's probably because of that.
(use-package org
  :hook ((org-mode . visual-fill-column-mode)
         (org-mode . visual-line-mode)
         (org-mode . (lambda () (setq-local line-spacing s3thi/prose-line-spacing))))
  :config
  (setq org-startup-folded 'content)
  (setq org-M-RET-may-split-line '((headline . nil)))
  (setq org-list-demote-modify-bullet
        '(("-" . "+") ("+" . "*") ("*" . "-"))))

(provide 'init-org)

;;; init-org.el ends here
