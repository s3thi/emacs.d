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

;; Use Dirvish mainly to enhance Dired.
(use-package dirvish
  :ensure t
  :after init-notes

  :init
  (dirvish-override-dired-mode)

  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   `(("h" "~/"                          "Home")
     ("d" "~/Documents/"                "Documents")
     ("D" "~/Downloads/"                "Downloads")
     ("n" ,s3thi/notes-directory         "Notes")))

  :config
  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-large-directory-threshold 1000)
  (setq dirvish-preview-dired-sync-omit t)

  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

;; Additional syntax highlighting for dired.
(use-package diredfl
  :ensure t
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(provide 'init-file-management)

;;; init-file-management.el ends here
