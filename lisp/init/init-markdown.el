;;; init-markdown.el --- Markdown configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Markdown editing config.

;;; Code:

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . visual-fill-column-mode)
         (markdown-mode . visual-wrap-prefix-mode)
         (markdown-mode . (lambda () (setq-local line-spacing s3thi/prose-line-spacing)))
         (markdown-mode . (lambda () (setq-local fill-column 64))))
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-header-scaling t)
  (setq markdown-header-scaling-values s3thi/prose-header-scaling)
  (setq markdown-hide-urls t)
  (setq markdown-special-ctrl-a/e t)
  :config
  (setq markdown-command "pandoc")
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-asymmetric-header t))

(provide 'init-markdown)

;;; init-markdown.el ends here
