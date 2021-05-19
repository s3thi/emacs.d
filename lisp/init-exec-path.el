(when *is-a-mac*
  (use-package exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))
    (exec-path-from-shell-copy-env "WORKON_HOME")))

(use-package add-node-modules-path
  :ensure t
  :hook js-mode)

(provide 'init-exec-path)
