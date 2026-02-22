;;; init-dired.el --- Dired configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configure dired.

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

(provide 'init-dired)

;;; init-dired.el ends here
