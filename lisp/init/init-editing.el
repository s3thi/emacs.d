;;; init-editing.el --- Text editing configuration -*- lexical-binding: t -*-

;;; Commentary:
;; General text editing defaults.

;;; Code:

;; Enable useful text editing commands that are disabled by default.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Set encoding to UTF-8.
(prefer-coding-system 'utf-8)

;; Always use spaces for indentation. Affects all modes, unless we override it
;; later. The only programming language I've used that mandates the use of tabs
;; rather than spaces is Go, so it's safe to set this here and override it for
;; Go if I ever write it again.
(setq-default indent-tabs-mode nil)

;; In modes where we are forced to use tabs, set the tab width to 4.
(setq-default tab-width 4)

;; Set fill-column manually, to make sure it's always what I expect.
(setq-default fill-column 80)

;; If there is some text already present in the system clipboard when we run an
;; Emacs command that kills text, make sure that is preserved by pushing it into
;; the kill ring.
;;
;; Since we've configured Emacs to put text into the system clipboard -- in
;; addition to the kill ring -- when we kill it, this setting ensures that we
;; never lose whatever might have already been in the clipboard when we perform
;; a kill operation. Not always useful, but a nice to have.
(setq save-interprogram-paste-before-kill t)

;; Make sure sentences end with single spaces, not double spaces. This makes
;; functions that operate on prose behave better (such as those in org-mode and
;; markdown-mode).
(setq sentence-end-double-space nil)

;; Duplicate current line, region, selection, etc.
(global-set-key (kbd "C-c d") #'duplicate-dwim)

;; Make word movement commands take CamelCase words into account.
(use-package subword
  :config
  (global-subword-mode 1))

;; Snippets are useful, right? I don't use snippets a lot, I only enable this
;; package in case I need complex snippets for programming modes. For most
;; simple insertions, I use dabbrev. A bonus is that dabbrev works in the
;; minibuffer too.
(use-package yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
              ("C-c y" . yas-insert-snippet)
              ("C-c Y" . yas-new-snippet))
  :init
  (setq yas-verbosity 2)
  :config
  (yas-global-mode 1))

;; Sometimes it's useful to "unfill" paragraphs.
(use-package unfill
  :ensure t)

;; Limit text width in certain modes.
(use-package visual-fill-column
  :ensure t
  :init
  (setq-default visual-fill-column-center-text t)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(provide 'init-editing)

;;; init-editing.el ends here
