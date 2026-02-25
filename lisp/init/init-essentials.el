;;; init-essentials.el --- Essential configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Essential configuration settings that set up good defaults.

;;; Code:

;; Personal information.
(setq user-full-name "Ankur Sethi"
      user-mail-address "contact@ankursethi.com")

;; Set the platform.
(defvar s3thi/is-a-mac (eq system-type 'darwin))
(defvar s3thi/is-a-pc (eq system-type 'windows-nt))
(defvar s3thi/is-a-linux (eq system-type 'gnu/linux))

;; Copy environment variables from the shell. This is needed on macOS and Linux
;; because GUI Emacs doesn't inherit the shell environment.
(use-package exec-path-from-shell
  :ensure t
  :if (or s3thi/is-a-mac s3thi/is-a-linux)
  :config
  (exec-path-from-shell-initialize))

;; Store configuration created by custom in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Don't create garbage files.
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)

;; Don't pollute the current directory with auto-save files. Instead, put them
;; all in one place.
(let ((auto-save-dir (expand-file-name "auto-save/" user-emacs-directory)))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))

;; Turn on auto-revert-mode for all files. This will automatically reload files
;; from disk every time they're changed from outside Emacs.
;;
;; Also ensure that Dired, VCS, etc. buffers are also auto reverted.
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose t)
(global-auto-revert-mode 1)

;; Save place in files.
(save-place-mode 1)

;; Keep track of recently opened files.
(use-package recentf
  :config
  (setq recentf-max-saved-items 200)
  (recentf-mode 1))

;; If a read-only file is opened, use view-mode instead of the regular mode.
;; This ensures you can't accidentally change or overwrite the file.
(setq view-read-only t)

;; Use crux for some common utilities.
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

(defun s3thi/edit-init-file ()
  "Open the Emacs configuration file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; Convenient shortcut for editing this configuration file.
(global-set-key (kbd "C-c i") #'s3thi/edit-init-file)

(provide 'init-essentials)

;;; init-essentials.el ends here
