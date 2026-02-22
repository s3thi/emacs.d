;;; s3thi-files.el --- Files and buffers configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Dired, backups, auto-save, buffer behavior, and project settings.

;;; Code:

;;;; Dired ------------------------------------------------------------------------

;; Start Dired with details hidden. Press ( to toggle.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; If there are two dired buffers on the screen, automatically make the second
;; buffer the target for move, copy, etc. operations.
(setq dired-dwim-target t)

;;;; Backups and auto-save --------------------------------------------------------

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

;;;; Buffer behavior --------------------------------------------------------------

;; Turn on auto-revert-mode for all files. This will automatically reload
;; files from disk every time they're changed from outside Emacs.
;;
;; Also ensure that Dired, VCS, etc. buffers are also auto reverted.
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
(setq auto-revert-verbose t)

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

;;;; Projects ---------------------------------------------------------------------

;; project-vc-extra-root-markers is a list of file names or glob patterns
;; which mark a project's root in addition to the default .git, .hg and other
;; common markers. This allows you to mark non-code directories as projects
;; that the built-in project.el recognizes.
(setq project-vc-extra-root-markers
      '(".project.el" ".jj" "package.json" "deno.json"))

;; Make sure the macOS .DS_Store files don't show up in project-find-file.
(setq project-vc-ignores '(".DS_Store"))

(provide 's3thi-files)

;;; s3thi-files.el ends here
