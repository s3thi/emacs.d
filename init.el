;; init.el --- Initialization file for Emacs

;;; Commentary:
;; Initialization file for Emacs.

;;; Code:

(setq user-full-name "Ankur Sethi"
      user-mail-address "contact@ankursethi.in")

;; Add ~/.emacs.d/lisp/ to the load path.
(add-to-list 'load-path
             (expand-file-name "lisp/" user-emacs-directory))

;; Load my personal blog helpers.
(require 'alive-and-well)
(global-set-key (kbd "C-c w") #'aaw/new-post)

;; Store configuration created by custom in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; General settings.
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode)
(global-hl-line-mode 1)
(set-frame-font "DM Mono 11" nil t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)
(global-display-line-numbers-mode t)

;; Don't make the screen jump when scroll off the top/bottom of the buffer.
(setq scroll-conservatively 100)

;; Really, really, REALLY use UTF-8 everywhere.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Some general keybindings.
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "M-;") #'comment-line)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-c I") (lambda ()
                                (interactive)
                                (find-file user-init-file)))
(global-set-key (kbd "C-0") #'delete-window)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-below)
(global-set-key (kbd "C-3") #'split-window-right)

;; Disable C-z to suspend in GUI Emacs.
(when window-system
  (global-unset-key (kbd "C-z")))

;; Enable some commands that are disabled by default.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Don't create garbage files.
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)

;; Automatically load files from disk when they change.
(global-auto-revert-mode 1)
(setq dired-auto-revert-buffer t)

;; Always use spaces for indentation.
(setq-default indent-tabs-mode nil)

;; In modes where we are forced to use tabs (e.g go-mode), set tab
;; width to 4.
(setq-default tab-width 4)

;; Fill paragraphs so the lines are 80 characters wide.
(setq-default fill-column 80)

;; Put the most recently killed/yanked text into the system clipboard.
(setq save-interprogram-paste-before-kill t)

;; Store secrets in this file. What could possibly go wrong?
(setq auth-sources '("~/.authinfo"))

;; Initialize package.
(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Initialize use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Setup auto updating for installed packages.
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

;; Configure exec-path.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "WORKON_HOME"))

;; Add node_modules to the exec-path.
(use-package add-node-modules-path
  :ensure t
  :hook js-mode)

;; Hide certain modes from the modeline.
(use-package diminish
  :ensure t
  :config
  (diminish 'subword-mode))

;; Color themes.
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-light-hard t))

;; Use Vertico for minibuffer completions.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Add annotations in the minibuffer using Marginalia.
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Use Orderless for searching through completions in the minibuffer.
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; I get by with a little help from which-key.
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode 1))

;; Magit is dope.
(use-package magit
  :ensure t)

;; Highlight modified lines on the fly.
(use-package diff-hl
  :ensure t
  :after magit
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

;; Use company for autocomplete menus.
(use-package company
  :ensure t
  :diminish
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort))
  :config
  (setq company-idle-delay 0.1)
  (setq company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; Turn off soft wrapping for source code only.
            (setq truncate-lines t)))

(global-subword-mode 1)

;; JavaScript indent level.
(setq js-indent-level 2)

;; Treat all JS as JSX.
(add-hook 'js-mode-hook #'js-jsx-enable)

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-indication-mode nil)
  :config
  (global-flycheck-mode))

;; Automatically format JS code with prettier.
(use-package prettier-js
  :diminish
  :ensure t
  :hook ((js-mode . prettier-js-mode)))

(defun setup-tide-mode ()
  "Run tide, eldoc, flycheck, company when entering a JS file."
  (interactive)
  (if buffer-file-name
      (progn
        (tide-setup)
        (flycheck-mode +1)
        (setq flycheck-check-syntax-automatically '(save mode-enabled))
        (eldoc-mode +1)
        (tide-hl-identifier-mode +1)
        (company-mode +1))))

;; Language server for JS and TS.
(use-package tide
  :ensure t
  :init
  (add-hook 'js-mode-hook #'setup-tide-mode))

;; Syntax highlighting for JSON.
(use-package json-mode
  :ensure t)

;; Syntax highlighting for Rust.
(use-package rustic
  :ensure t
  :diminish)

;; Support for Go.
(use-package go-mode
  :ensure t
  :hook ((go-mode . (lambda ()
                      (add-hook 'before-save-hook #'gofmt-before-save nil t)))))

;; REST client.
(use-package restclient
  :ensure t)

;; LSP config.
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"))

(defun s3thi/is-inside-frontmatter ()
  "In a Markdown file, this returns t if the point is inside a
frontmatter section"
  (save-excursion
    (let ((current-position (point))
          (begin (point-min))
          (end (progn (beginning-of-buffer)
                      (search-forward-regexp "^---$")
                      (search-forward-regexp "^---$")
                      (point))))
      (and (> current-position begin)
           (< current-position end)))))

(defun s3thi/markdown-auto-fill ()
  "Set up auto-fill-mode for Markdown buffers"
  (auto-fill-mode)
  (setq auto-fill-function #'s3thi/markdown-auto-fill-function))

(defun s3thi/markdown-auto-fill-function ()
  "A custom auto-fill function for Markdown files that disables
filling inside YAML frontmatter (if it exists)."
  (unless (s3thi/is-inside-frontmatter)
    (do-auto-fill)))

;; Markdown.
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . s3thi/markdown-auto-fill))
  :init (setq markdown-command "multimarkdown"))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package eldoc
  :diminish)

(use-package hl-todo
  :ensure t
  :diminish
  :hook ((prog-mode . hl-todo-mode)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . #'er/expand-region)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (global-set-key (kbd "C-c y") #'yas-insert-snippet)
  (global-set-key (kbd "C-;") #'yas-expand))

(use-package crux
  :ensure t
  :bind (("C-c o" . #'crux-open-with)
         ("C-c u" . #'crux-view-url)
         ("C-c e" . #'crux-eval-and-replace)
         ("C-c d" . #'crux-duplicate-current-line-or-region)
         ("C-c D" . #'crux-delete-file-and-buffer)
         ("C-c r" . #'crux-rename-file-and-buffer)
         ("C-c k" . #'crux-kill-other-buffers)
         ("C-c S" . #'crux-find-shell-init-file)))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)

         ;; Open the default org directory in dired.
         ("<f12>" . (lambda ()
                      (interactive)
                      (dired org-directory))))
  :hook ((org-mode . auto-fill-mode))
  :config
  (setq org-directory "~/Dropbox/Org/")
  (setq org-default-notes-file "~/Dropbox/Org/gtd/inbox.org")
  (add-to-list 'auto-mode-alist `("\\.org-archive$" . org-mode))
  (setq org-agenda-files `("~/Dropbox/Org/gtd/inbox.org"
                           "~/Dropbox/Org/gtd/inbox-mobile.org"
                           "~/Dropbox/Org/gtd/tasks.org"
                           "~/Dropbox/Org/gtd/projects.org"
                           "~/Dropbox/Org/gtd/gluttony.org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "WAITING(w)"
                    "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("NEXT" . "purple")
          ("IN-PROGRESS" . (:foreground "OliveDrab4" :weight bold))
          ("WAITING" . org-warning)
          ("SOMEDAY" . "gray")
          ("DONE" . org-done)
          ("CANCELLED" .  org-done)))
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes '(confirm))
  (setq org-log-into-drawer t)
  (setq org-archive-location "%s-archive::")
  (setq org-log-done t)
  (setq org-log-reschedule t)
  (setq org-capture-templates
        '(("t" "todo" entry (file "~/Dropbox/Org/gtd/inbox.org")
           "* TODO %?")
          ("p" "project todo" entry (file+headline "~/Dropbox/Org/gtd/projects.org" "Refile")
           "* TODO %?")
          ("g" "gluttony" entry (file+headline "~/Dropbox/Org/gtd/gluttony.org" "Refile")
           "* TODO %?")))
  (setq org-agenda-custom-commands
        '(("y" "My agenda view"
           ((agenda "")
            (todo "IN-PROGRESS" ((org-agenda-overriding-header "\nDoing now:")
                                 (org-agenda-files
                                  '("~/Dropbox/Org/gtd/tasks.org"
                                    "~/Dropbox/Org/gtd/gluttony.org"))))
            (todo "WAITING" ((org-agenda-overriding-header "\nWaiting on:")))
            (alltodo "" ((org-agenda-overriding-header "\nInboxes:")
                         (org-agenda-files
                          '("~/Dropbox/Org/gtd/inbox.org"
                            "~/Dropbox/Org/gtd/inbox-mobile.org"))))
            (alltodo "" ((org-agenda-overriding-header "\nUnscheduled:")
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'timestamp))
                         (org-agenda-files '("~/Dropbox/Org/gtd/tasks.org")))))
           ((org-agenda-block-separator nil)))
          ("o" "Things to do someday"
           ((todo "SOMEDAY"))
           ((org-agenda-overriding-header "Things to do someday:")
            (org-agenda-files
             (remove "~/Dropbox/Org/gtd/projects.org" org-agenda-files)))))))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package avy
  :ensure t
  :bind (("C-:" . #'avy-goto-char-2)))

;; Start a server so other clients can connect to this.
(server-start)

(provide 'init)

;;; init.el ends here
