;;; s3thi-keys.el --- Utilities and key bindings -*- lexical-binding: t -*-

;;; Commentary:
;; crux utilities, global keybindings, and C-x 8 extensions.

;;; Code:

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c p" . crux-kill-buffer-truename)))

;; Disable C-z to suspend in GUI Emacs. By default, hitting C-z in GUI Emacs
;; will minimize the editor, which is very annoying. This disables that
;; behavior. On terminal Emacs, this will still allow us to suspend the editor
;; and go back to our shell.
(when window-system
  (global-unset-key (kbd "C-z")))

;; Convenient for editing this configuration file.
(global-set-key (kbd "C-c i") (lambda ()
                                "Open the Emacs configuration file."
                                (interactive)
                                (find-file "~/.emacs.d/init.el")))

;; Bind some useful built-in commands.
(global-set-key (kbd "C-c d") #'duplicate-dwim)
(global-set-key (kbd "M-=") #'count-words)

;; Add a few things to the C-x 8 bindings.
(use-package iso-transl
  :config
  (define-key iso-transl-ctl-x-8-map "r" [?₹])
  (define-key iso-transl-ctl-x-8-map ".3" [?…])
  (define-key iso-transl-ctl-x-8-map "m" [?—]))

(provide 's3thi-keys)

;;; s3thi-keys.el ends here
