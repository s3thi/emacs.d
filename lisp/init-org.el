(use-package org
  :ensure t
  :config
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-startup-folded nil)
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (global-set-key (kbd "C-c o p") #'org-mark-ring-goto))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-roam
  :after org
  :ensure t
  :diminish
  :init
  (setq org-roam-directory "~/Org/Roam/")
  :config
  (add-hook 'after-init-hook #'org-roam-mode)
  (global-set-key (kbd "C-c o f") #'org-roam-find-file)
  (global-set-key (kbd "C-c o i") #'org-roam-insert)
  (global-set-key (kbd "C-c o o") #'org-roam)
  (global-set-key (kbd "C-c o d") #'deft))

(use-package deft
  :ensure t
  :init
  (setq deft-extensions '("org"))
  (setq deft-directory "~/Org/"))

(provide 'init-org)
