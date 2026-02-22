;;; s3thi-search.el --- Search configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Ripgrep integration and notes tag searching.

;;; Code:

;; Use ripgrep for searching.
(use-package rg
  :ensure t
  :init
  (setq rg-command-line-flags '("--sort path"))
  :config
  (rg-enable-default-bindings)
  (dolist (tag s3thi/notes-tags)
    (let ((name (nth 0 tag))
          (key (nth 1 tag))
          (label (nth 2 tag)))
      (eval `(rg-define-search ,(intern (concat "search-notes-tag-" name))
               ,(format "Search for the #%s tag in notes" name)
               :query ,(concat "#" name)
               :format literal
               :files "*.md"
               :flags ("--sort path")
               :dir ,s3thi/notes-directory
               :menu ("Tags" ,key ,label))))))

(provide 's3thi-search)

;;; s3thi-search.el ends here
