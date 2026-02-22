;;; s3thi-editing.el --- Text editing configuration -*- lexical-binding: t -*-

;;; Commentary:
;; General text editing defaults, subword mode, yasnippet, and unfill.

;;; Code:

;; Enable useful text editing commands that are disabled by default.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Make sure sentences end with single spaces, not double spaces. This makes
;; functions that operate on prose behave better (such as those in org-mode
;; and markdown-mode).
(setq sentence-end-double-space nil)

;; Always use spaces for indentation. Affects all modes, unless we override it
;; later. The only programming language I've used that mandates the use of
;; tabs rather than spaces is Go, so it's safe to set this here and override
;; it for Go if I ever write it again.
(setq-default indent-tabs-mode nil)

;; In modes where we are forced to use tabs, set the tab width to 4.
(setq-default tab-width 4)

;; Set fill-column manually, to make sure it's always what I expect.
(setq-default fill-column 80)

;; If there is some text already present in the system clipboard when we run
;; an Emacs command that kills text, make sure that is preserved by pushing it
;; into the kill ring.
;;
;; Since we've configured Emacs to put text into the system clipboard -- in
;; addition to the kill ring -- when we kill it, this setting ensures that we
;; never lose whatever might have already been in the clipboard when we perform
;; a kill operation. Not always useful, but a nice to have.
(setq save-interprogram-paste-before-kill t)

;; Make word movement commands take CamelCase words into account.
(use-package subword
  :config
  (global-subword-mode 1))

;; Snippets are useful, right?
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand))

;; Sometimes it's useful to "unfill" paragraphs.
(use-package unfill
  :ensure t)

(provide 's3thi-editing)

;;; s3thi-editing.el ends here
