;;; init-prose.el --- Prose configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Basic prose editing configuration.

;;; Code:

;; Line spacing for prose modes.
(defvar s3thi/prose-line-spacing 0.5
  "Line spacing to use in prose modes like Org and Markdown.")

(defvar s3thi/prose-header-scaling '(1.4 1.3 1.2 1.1 1.0 1.0)
  "Header scaling values for levels 1-6 in prose modes.")

;; Count words in buffer or region.
(global-set-key (kbd "M-=") #'count-words)

;; jinx is a fast, modern spell checker that checks the entire visible buffer
;; at once.
(use-package jinx
  :ensure t
  :hook ((markdown-mode . jinx-mode)
         (org-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US en_GB hi"))

;; mixed-pitch allows variable-pitch fonts for prose while keeping code blocks
;; monospace.
(use-package mixed-pitch
  :ensure t
  :hook ((markdown-mode . mixed-pitch-mode)
         (org-mode . mixed-pitch-mode))
  :config
  (setq mixed-pitch-set-height t))

(provide 'init-prose)

;;; init-prose.el ends here
