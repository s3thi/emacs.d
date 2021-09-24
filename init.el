(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Working directory should be the home directory.
(when (string= default-directory "/")
  (setq default-directory "~/")
  (with-current-buffer "*Messages*"
    (setq default-directory "~/")))

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-pc* (eq system-type 'windows-nt))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; Store configuration created by custom in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Set-up package.
(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Setup auto updating.
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

;; Configure the Emacs exec-path.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "WORKON_HOME"))

(use-package add-node-modules-path
  :ensure t
  :hook js-mode)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)

(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (not (display-graphic-p))
  (menu-bar-mode -1))

(column-number-mode 1)
(show-paren-mode)

(when *is-a-mac*
  (set-frame-font "Rec Mono Duotone 14" nil t))

(when *is-a-pc*
  (set-frame-font "Rec Mono Duotone 11" nil t))

(when *is-a-linux*
  (set-frame-font "Fira Code 10" nil t))

(when (or *is-a-pc* *is-a-linux*)
  (menu-bar-mode -1))

;; Enable emoji on macOS.
(when (boundp 'set-fontset-font)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(setq visible-bell t)

(use-package diminish
  :after use-package
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft t))

(use-package treemacs
  :ensure t
  :config
  (global-set-key (kbd "C-\\") #'treemacs-select-window)
  (setq treemacs-no-png-images t)
  (treemacs-fringe-indicator-mode 'always))

(use-package vc
  :diminish)

(global-set-key (kbd "C-c v") #'delete-other-windows-vertically)
(global-set-key (kbd "C-c b") #'rename-buffer)
(global-set-key (kbd "M-;") #'comment-line)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") #'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") #'split-and-follow-vertically)

(defun copy-file-path ()
  "Copy the path of the file currently open in the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (kill-new filename)
          (message "Filename placed in kill ring"))
      (message "Current buffer is not associated with a file"))))

(global-set-key (kbd "C-c c") #'copy-file-path)

(defun unfill-paragraph ()
  "The inverse of 'fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

(global-set-key (kbd "M-Q") #'unfill-paragraph)

(defun quit-other-window ()
  "Quits the other window."
  (interactive)
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (when (eq (key-binding "q") 'quit-window)
      (quit-window nil win-other))
    (select-window win-curr)))

(global-set-key (kbd "C-c q") #'quit-other-window)

(defun crux-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun crux-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)

(defun crux-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c D") #'crux-delete-file-and-buffer)

(defun crux-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer)

;; General programming settings.
(require 'init-prog)

;; Ivy and Counsel.
(require 'init-ivy-counsel)

;; File related settings.
(require 'init-files)

;; Settings for editing text.
(require 'init-editing)

;; Magit settings.
(require 'init-git)

;; Terminal.
(require 'init-term)

;; JavaScript specific settings.
(require 'init-js)

;; Python specific settings.
(require 'init-python)

;; Rust specific settings.
(require 'init-rust)

;; C# specific settings.
(require 'init-csharp)

;; LSP
(require 'init-lsp)
