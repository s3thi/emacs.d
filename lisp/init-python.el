(use-package elpy
  :ensure t
  :after flycheck
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook (lambda ()
                              (add-hook 'before-save-hook
                                        'elpy-black-fix-code nil t)))

  (elpy-enable))

(provide 'init-python)
