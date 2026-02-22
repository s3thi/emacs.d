;;; init-terminal.el --- Terminal configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Terminal setup.

;;; Code:

;; Use vterm as terminal emulator.
(use-package vterm
  :ensure t
  :init
  (setq vterm-buffer-name-string "vterm %s"))

;; Use multi-vterm to manage multiple vterm buffers.
(use-package multi-vterm
  :ensure t
  :bind (("C-c t t" . multi-vterm)
         ("C-c t d" . multi-vterm-dedicated-toggle)
         ("C-c t n" . multi-vterm-next)
         ("C-c t p" . multi-vterm-prev)))

(provide 'init-terminal)

;;; init-terminal.el ends here
