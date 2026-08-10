;;; init-file-management.el --- File management configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configure file management.

;;; Code:

;; Start Dired with details hidden. Press ( to toggle.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; If there are two dired buffers on the screen, automatically make the second
;; buffer the target for move, copy, etc. operations.
(setq dired-dwim-target t)

;; Move files to system trash instead of deleting them outright.
(setq delete-by-moving-to-trash t)

;; Highlight lines in dired.
(add-hook 'dired-mode-hook #'hl-line-mode)

;; Rename files using the version control system, if present.
(setq dired-vc-rename-file t)

;; Use GNU ls if it's available on macOS and pass extra switches.
(when (and s3thi/is-a-mac
           (executable-find "gls"))
  (setq insert-directory-program "gls")
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group"))

(provide 'init-file-management)

;;; init-file-management.el ends here
