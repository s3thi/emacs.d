(use-package mu4e
  :config
  (setq mu4e-view-show-addresses t)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-attachments-dir "~/Downloads")
  (setq  mu4e-maildir "~/Mail/main")
  (setq mu4e-refile-folder "/Archive")
  (setq mu4e-sent-folder "/Sent")
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-trash-folder "/Trash")
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-confirm-quit nil)
  (setq mu4e-completing-read-function 'ivy-completing-read))

(use-package mu4e-views
  :after mu4e
  :ensure t
  :bind
  (:map mu4e-headers-mode-map
	("v" . mu4e-views-mu4e-select-view-msg-method)
	("M-n" . mu4e-views-cursor-msg-view-window-down)
	("M-p" . mu4e-views-cursor-msg-view-window-up))
  :config
  (setq mu4e-views-completion-method 'ivy)
  (setq mu4e-views-default-view-method "html")
  (mu4e-views-mu4e-use-view-msg-method "html")
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window))

(setq message-send-mail-function 'smtpmail-send-it)
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.fastmail.com")
(setq smtpmail-smtp-user "contact@ankursethi.in")
(setq smtpmail-smtp-service 465)
(setq smtpmail-stream-type 'ssl)

(use-package org-mu4e
  :after mu4e org
  :config
  (setq org-mu4e-convert-to-html t))

(provide 'init-mail)
