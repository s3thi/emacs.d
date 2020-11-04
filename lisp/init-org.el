(use-package org
  :init
  (setq org-startup-folded nil))

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/Documents/Org/Journal")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-journal-time-prefix "\n** ")
  (setq org-journal-hide-entries-p nil))

(provide 'init-org)
