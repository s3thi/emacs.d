(use-package mu4e
  :config
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-sent-messages-behavior 'delete))

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

(provide 'init-mail)
