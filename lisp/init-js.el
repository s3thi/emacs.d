(setq js-indent-level 2)

(use-package prettier-js
  :ensure t
  :init
  (add-hook 'js-mode-hook #'prettier-js-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :init
  (add-hook 'js-mode-hook #'setup-tide-mode))

(use-package json-mode
  :ensure t)

(provide 'init-js)
