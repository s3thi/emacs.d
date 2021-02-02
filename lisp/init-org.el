(use-package org
  :ensure t
  :init
  (setq org-agenda-files '("~/Dropbox/Org/Agenda/hobbies.org"
                           "~/Dropbox/Org/Agenda/household.org"
                           "~/Dropbox/Org/Agenda/inbox.org"
                           "~/Dropbox/Org/Agenda/personal.org"
                           "~/Dropbox/Org/Agenda/summarize.org"
                           "~/Dropbox/Org/Agenda/work.org"))
  (setq org-refile-targets '((org-agenda-files :level . 1)))
  (setq org-refile-use-outline-path 'file)
  (setq org-directory "~/Dropbox/Org/")
  (setq org-default-notes-file "~/Dropbox/Org/Agenda/inbox.org")
  (setq org-capture-templates
        '(("t" "Task" entry
           (file "~/Dropbox/Org/Agenda/inbox.org")
           "* TODO %i%?\n")))
  (setq org-startup-folded nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  :config
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (global-set-key (kbd "C-c o p") #'org-mark-ring-goto)
  (global-set-key (kbd "C-c o a") #'org-agenda)
  (global-set-key (kbd "C-c o c") #'org-capture))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-roam
  :after org
  :ensure t
  :diminish
  :init
  (setq org-roam-directory "~/Dropbox/Org/Roam/")
  :config
  (add-hook 'after-init-hook #'org-roam-mode)
  (global-set-key (kbd "C-c o f") #'org-roam-find-file)
  (global-set-key (kbd "C-c o i") #'org-roam-insert)
  (global-set-key (kbd "C-c o o") #'org-roam))

(use-package deft
  :ensure t
  :init
  (setq deft-extensions '("org"))
  (setq deft-directory "~/Dropbox/Org/Roam/")
  :config
  (global-set-key (kbd "C-c /") #'deft))

(provide 'init-org)
