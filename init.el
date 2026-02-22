;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs configuration for Ankur Sethi <contact@ankursethi.com>.
;;
;; This file contains the complete Emacs configuration. Platform-specific
;; early initialization lives in early-init.el.

;;; Code:

;;; Bootstrap =====================================================================

;; Set encoding to UTF-8.
(prefer-coding-system 'utf-8)

;; Personal information.
(setq user-full-name "Ankur Sethi"
      user-mail-address "contact@ankursethi.com")

;; Platform specific variables and personal settings.
(defvar s3thi/is-a-mac (eq system-type 'darwin))
(defvar s3thi/is-a-pc (eq system-type 'windows-nt))
(defvar s3thi/is-a-penguin (eq system-type 'gnu/linux))

(defvar s3thi/notes-tags
  '(("blog"        "C-t b" "Blog")
    ("therapy"     "C-t t" "Therapy")
    ("programming" "C-t p" "Programming")
    ("idea"        "C-t i" "Idea")
    ("homework"    "C-t h" "Homework")
    ("writing"     "C-t w" "Writing")
    ("hip-hop"     "C-t H" "Hip-hop")
    ("someday"     "C-t s" "Someday"))
  "List of tags used in notes. Each entry is (TAG-NAME MENU-KEY MENU-LABEL).")

(defvar s3thi/notes-directory
  "/Users/s3thi/Library/Mobile Documents/27N4MQEA55~pro~writer/Documents/"
  "Directory where notes are stored.")

;; Copy environment variables from the shell. This is needed on macOS and
;; Linux because GUI Emacs doesn't inherit the shell environment.
(use-package exec-path-from-shell
  :ensure t
  :if (or s3thi/is-a-mac s3thi/is-a-penguin)
  :config
  (exec-path-from-shell-initialize))

;; Remind me to upgrade packages if it has been more than 7 days since the
;; last upgrade. The timestamp is stored in ~/.emacs.d/.last-package-upgrade.
;; Run M-x package-upgrade-all to upgrade and dismiss the reminder.
(defvar s3thi/package-upgrade-reminder-interval 7
  "Number of days between package upgrade reminders.")

(defvar s3thi/package-upgrade-timestamp-file
  (expand-file-name ".last-package-upgrade" user-emacs-directory)
  "File that stores the timestamp of the last package upgrade.")

(defun s3thi/package-upgrade-reminder ()
  "Display a reminder if packages haven't been upgraded recently."
  (let ((last-upgrade-time
         (when (file-exists-p s3thi/package-upgrade-timestamp-file)
           (with-temp-buffer
             (insert-file-contents s3thi/package-upgrade-timestamp-file)
             (seconds-to-time (string-to-number (buffer-string)))))))
    (when (or (not last-upgrade-time)
              (> (float-time (time-subtract (current-time) last-upgrade-time))
                 (* s3thi/package-upgrade-reminder-interval 86400)))
      (message "It's been more than %d days since your last package upgrade. Run M-x package-upgrade-all."
               s3thi/package-upgrade-reminder-interval))))

(defun s3thi/record-package-upgrade (&rest _)
  "Record the current time as the last package upgrade time."
  (write-region (number-to-string (float-time)) nil
                s3thi/package-upgrade-timestamp-file nil 'quiet))

(advice-add 'package-upgrade-all :after #'s3thi/record-package-upgrade)
(add-hook 'emacs-startup-hook #'s3thi/package-upgrade-reminder)

;;; User interface ================================================================

;;;; General ----------------------------------------------------------------------

;; Remove UI elements we don't like. Note that tool-bar-mode and
;; scroll-bar-mode are disabled in early-init.el for faster startup.
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

;; And add UI elements that are nice to have.
(column-number-mode 1)
(show-paren-mode 1)
(setq confirm-kill-emacs #'yes-or-no-p)

;; Allow resizing the frame by pixels instead of rounding the frame size to
;; characters. This prevents an irritating situation on macOS where sometimes
;; there's a bit of space left between the bottom of the Emacs frame and the
;; bottom of the screen when maximizing Emacs using an external window manager.
(setq frame-resize-pixelwise t)

;; Disable mouse wheel text scaling. This gets triggered accidentally and is
;; never useful.
(defalias 'mouse-wheel-text-scale #'ignore)

;; Show total number of matches while searching.
(setq-default isearch-lazy-count t)

;; minions-mode only shows the current major mode in the modeline, tucking
;; away all the minor modes into a menu.
(use-package minions
  :ensure t
  :config
  (minions-mode 1))

;; which-key displays helpful suggestions in the minibuffer when you hit the
;; first part of a long keychord. It is built into Emacs 30.
(which-key-mode 1)

;; vundo lets me browse the Emacs undo history visually. Unlike undo-tree, it
;; works on top of the built-in undo system rather than replacing it, which
;; avoids corruption issues. Call M-x vundo to open the tree, then use f / b
;; to move between branches and n / p to move forward and back in time. Press
;; q to quit.
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

;; Maximize the Emacs frame on startup. To start every new frame maximized
;; instead of just the first frame, you can add the following options to
;; default-frame-alist instead of initial-frame-alist. Additionally, it's
;; possible to toggle the maximized status of an Emacs frame by calling
;; toggle-frame-maximized, which is bound to M-<f10> by default.
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;;; Fonts ------------------------------------------------------------------------

;; Set a nice font. The font size will have to be different across platforms,
;; so we check the OS we're running on before setting it.
(when s3thi/is-a-mac
  (set-face-attribute 'default nil
                      :family "Berkeley Mono Variable"
                      :height 160)
  (set-face-attribute 'variable-pitch nil
                      :family "iA Writer Duospace"
                      :height 180))

;;;; Scrolling --------------------------------------------------------------------

;; scroll-conservatively specifies the number of lines to scroll the buffer in
;; order to bring the cursor back on the screen when it moves off-screen. If
;; moving that much doesn't bring the cursor back, Emacs will scroll the
;; buffer by as many lines as it takes to bring the cursor to the exact center
;; of the screen.
;;
;; This behavior is irritating and jarring, causing huge jumps as you move
;; around a file. Luckily, setting this variable to a number larger than 100
;; tells Emacs to never scroll by large amounts, and instead scroll the screen
;; just enough to keep the cursor visible.
;;
;; In summary, setting scroll-conservatively to 101 will make Emacs behave
;; like every other text editor on the planet.
(setq scroll-conservatively 101)

;; scroll-margin specifies the number of lines of margin at the top or bottom
;; of the window. As soon as the cursor gets closer than this to the top of or
;; bottom of a window, Emacs will start scrolling.
(setq scroll-margin 3)

;; Enable smooth scrolling.
(pixel-scroll-precision-mode 1)

;;;; Minibuffer completion --------------------------------------------------------

;; Use vertico as the completion UI.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Use savehist to save minibuffer history.
(use-package savehist
  :init
  (savehist-mode))

;; Use orderless to filter completions.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enable annotations in the minibuffer using marginalia.
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;; Window navigation ------------------------------------------------------------

;; Use ace-window for navigating open windows.
(use-package ace-window
  :ensure t
  :bind* (("M-o" . ace-window))
  :init
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame))

;; Use avy to jump around.
(use-package avy
  :ensure t
  :bind* (("C-'" . avy-goto-char-2)))

;;;; Theme ------------------------------------------------------------------------

;; Let's install some Doom Emacs themes!
(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Load a theme.
(load-theme 'doom-one t)

;; The following function adds a top padding to the current buffer. It does so
;; by setting the header line to an empty string, increasing its line height,
;; and setting its background to be the same as the buffer background. The
;; downside of doing this is that it may mess up the header line in modes that
;; actually make use of it (e.g. Eglot).
;;
;; This is currently not used anywhere.
(defun s3thi/add-top-padding ()
  "Add top padding to the current buffer."
  (interactive)
  (unless (bound-and-true-p s3thi/--top-padding-applied)
    (setq-local s3thi/--top-padding-applied t)
    (setq-local header-line-format "")
    (face-remap-add-relative 'header-line :height 400)
    (let ((bg (face-attribute 'default :background)))
      (face-remap-add-relative 'header-line :background bg))))

;;; Files and buffers =============================================================

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

;;; Text editing ==================================================================

;; Enable useful text editing commands that are disabled by default.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Make sure sentences end with single spaces, not double spaces. This makes
;; functions that operate on prose behave better (such as those in org-mode
;; and markdown-mode).
(setq sentence-end-double-space nil)

;; Always use spaces for indentation. Affects all modes, unless we override it
;; later. The only programming language I've used that mandates the use of
;; tabs rather than spaces is Go, so it's safe to set this here and override
;; it for Go if I ever write it again.
(setq-default indent-tabs-mode nil)

;; In modes where we are forced to use tabs, set the tab width to 4.
(setq-default tab-width 4)

;; Set fill-column manually, to make sure it's always what I expect.
(setq-default fill-column 80)

;; If there is some text already present in the system clipboard when we run
;; an Emacs command that kills text, make sure that is preserved by pushing it
;; into the kill ring.
;;
;; Since we've configured Emacs to put text into the system clipboard -- in
;; addition to the kill ring -- when we kill it, this setting ensures that we
;; never lose whatever might have already been in the clipboard when we perform
;; a kill operation. Not always useful, but a nice to have.
(setq save-interprogram-paste-before-kill t)

;; Make word movement commands take CamelCase words into account.
(use-package subword
  :config
  (global-subword-mode 1))

;; Snippets are useful, right?
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand))

;; Sometimes it's useful to "unfill" paragraphs.
(use-package unfill
  :ensure t)

;;; Writing =======================================================================

;;;; Prose defaults ---------------------------------------------------------------

;; Line spacing for prose modes.
(defvar s3thi/prose-line-spacing 0.5
  "Line spacing to use in prose modes like Org and Markdown.")

(defvar s3thi/prose-header-scaling '(1.4 1.3 1.2 1.1 1.0 1.0)
  "Header scaling values for levels 1-6 in prose modes.")

;; Add visual-fill-column to limit text width in certain modes.
(use-package visual-fill-column
  :ensure t
  :init
  (setq-default visual-fill-column-center-text t)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

;; jinx is a fast, modern spell checker that checks the entire visible buffer
;; at once.
(use-package jinx
  :ensure t
  :hook ((markdown-mode . jinx-mode)
         (org-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US en_GB hi"))

;; mixed-pitch allows variable-pitch fonts for prose while keeping code blocks
;; monospace.
(use-package mixed-pitch
  :ensure t
  :hook ((markdown-mode . mixed-pitch-mode)
         (org-mode . mixed-pitch-mode))
  :config
  (setq mixed-pitch-set-height t))

;; जब मैं toggle-input-method कॉल करूं तो कौनसा इन्पुट मैथड सेलेक्ट होना चाहिये.
(setq default-input-method "devanagari-itrans")

;; कभी चीटशीट देखने की ज़रूरत पढ़ जाये तो.
(global-set-key (kbd "C-c I") (lambda ()
                                (interactive)
                                "Describe the devanagari-itrans input method"
                                (describe-input-method "devanagari-itrans")))

;;;; Org --------------------------------------------------------------------------

;; Note that visual-wrap-prefix-mode, which we use for Markdown, doesn't work
;; well with org-mode when org-indent-mode is also turned on. If you're
;; running into weird issues, it's probably because of that.
(use-package org
  :hook ((org-mode . visual-fill-column-mode)
         (org-mode . visual-line-mode)
         (org-mode . (lambda () (setq-local line-spacing s3thi/prose-line-spacing))))
  :config
  (setq org-startup-folded 'content)
  (setq org-M-RET-may-split-line '((headline . nil)))
  (setq org-list-demote-modify-bullet
        '(("-" . "+") ("+" . "*") ("*" . "-"))))

;;;; Markdown ---------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . visual-fill-column-mode)
         (markdown-mode . visual-wrap-prefix-mode)
         (markdown-mode . (lambda () (setq-local line-spacing s3thi/prose-line-spacing)))
         (markdown-mode . (lambda () (setq-local fill-column 64))))
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-header-scaling t)
  (setq markdown-header-scaling-values s3thi/prose-header-scaling)
  (setq markdown-hide-urls t)
  (setq markdown-special-ctrl-a/e t)
  :config
  (setq markdown-command "pandoc")
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-asymmetric-header t))

;; Highlight tags from s3thi/notes-tags in Markdown buffers.
;; TODO: this should be a minor mode.
(font-lock-add-keywords
 'markdown-mode
 (mapcar (lambda (tag)
           (list (concat "#" (car tag) "\\b") 0 ''warning 'prepend))
         s3thi/notes-tags))

;;;; Daily notes and journal entries ----------------------------------------------

(defun s3thi/date-tree-path (root &optional time)
  (let* ((year (format-time-string "%Y" time))
         (month (format-time-string "%m" time))
         (month-name (format-time-string "%B" time))
         (day (format-time-string "%d" time))
         (day-name (format-time-string "%A" time))
         (month-directory (format "%s-%s %s" year month month-name))
         (file-name (format "%s-%s-%s %s.md" year month day day-name))
         (root-absolute-path (expand-file-name root s3thi/notes-directory))
         (year-directory-path (expand-file-name year root-absolute-path))
         (month-directory-path (expand-file-name month-directory year-directory-path))
         (file-path (expand-file-name file-name month-directory-path)))
    file-path))

(defun s3thi/find-daily-note (&optional time)
  "Open today's daily note."
  (interactive)
  (find-file (s3thi/date-tree-path "02 Daily notes" time)))

(defun s3thi/find-journal-entry (&optional time)
  "Open today's journal entry."
  (interactive)
  (find-file (s3thi/date-tree-path "03 Journal" time)))

(global-set-key (kbd "<f8>") #'s3thi/find-daily-note)
(global-set-key (kbd "C-<f8>") #'s3thi/find-journal-entry)

;; Algorithm for navigating to previous/next daily note or journal entry:
;;
;; For prev:
;; - Find all available entries in the current month
;; - Parse them into date objects
;; - Sort the objects in ascending order
;; - If there's a previous date available this month, simply open it
;; - If there's no previous date available this month, or if we're at the
;;   start of the month, then do this:
;;   + Get current month as a number by parsing the month directory name
;;   + Go up one directory and get a list of months
;;   + Grab month numbers out of the list and sort them
;;   + If there's a previous month available, list all the files inside it
;;     and do what we did for files before
;;   + If there's no previous month available this year, or we're at the
;;     start of the year, do this:
;;     * Go up one directory and get a list of years
;;     * Sort them as numbers
;;     * If there's a previous year available, go into it
;;     * Then do what we did for months
;;     * If there's no previous year available, give up with a message
;;
;; Next will be similar, just in the opposite direction.
;;
;; Can have get-prev-file-in-month, get-prev-month-in-year, and
;; get-prev-year-in-root helpers.
;;
;; Pseudocode:
;;
;;   current = get_current_journal_entry()
;;   current_year = get_current_year(current)
;;   current_month = get_current_month(current)
;;   prev = None
;;   while not prev:
;;       prev = get_prev_in_month(current_month)
;;       if prev: break
;;
;;       current_month = get_prev_month_in_year(current_year)
;;       if current_month: continue
;;
;;       current_year = get_prev_year_in_root()
;;       if not current_year: break

;;; Search ========================================================================

;; Use ripgrep for searching.
(use-package rg
  :ensure t
  :init
  (setq rg-command-line-flags '("--sort path"))
  :config
  (rg-enable-default-bindings)
  (dolist (tag s3thi/notes-tags)
    (let ((name (nth 0 tag))
          (key (nth 1 tag))
          (label (nth 2 tag)))
      (eval `(rg-define-search ,(intern (concat "search-notes-tag-" name))
               ,(format "Search for the #%s tag in notes" name)
               :query ,(concat "#" name)
               :format literal
               :files "*.md"
               :flags ("--sort path")
               :dir ,s3thi/notes-directory
               :menu ("Tags" ,key ,label))))))

;;; Version control ===============================================================

;; Love me some Magit.
(use-package magit
  :ensure t)

;; And also some Majutsu.
(use-package majutsu
  :ensure t
  :vc (:url "https://github.com/0WD0/majutsu")
  :bind (("C-x j" . majutsu)))

;;; Terminal ======================================================================

;; Use vterm as terminal emulator.
(use-package vterm
  :ensure t
  :init
  (setq vterm-buffer-name-string "vterm %s"))

;; Use multi-vterm to manage multiple vterm buffers.
(use-package multi-vterm
  :ensure t
  :bind (("C-c t t" . multi-vterm)
         ("C-c t d" . multi-vterm-dedicated-toggle)
         ("C-c t n" . multi-vterm-next)
         ("C-c t p" . multi-vterm-prev)))

;;; Programming ===================================================================

;;;; Flymake ----------------------------------------------------------------------

(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

;;;; Tree-sitter ------------------------------------------------------------------

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(astro javascript typescript tsx python rust))
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

;;;; Eglot ------------------------------------------------------------------------

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l h" . eldoc)
              ("C-c l R" . eglot-reconnect))
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               `(astro-ts-mode . ,s3thi/astro-lsp-config))
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode typescript-ts-mode) . s3thi/eglot-server-for-ts)))

;;;; Code formatting --------------------------------------------------------------

;; apheleia formats code asynchronously on save without blocking Emacs. It
;; supports Prettier and many other formatters out of the box, and preserves
;; point position via RCS patching.
(use-package apheleia
    :ensure t
    :config
    (setf (alist-get 'prettier-astro apheleia-formatters)
          '("apheleia-npx" "prettier" "--stdin-filepath" filepath
            "--plugin=prettier-plugin-astro" "--parser=astro"))
    (setf (alist-get 'astro-ts-mode apheleia-mode-alist)
          '(prettier-astro))
    (apheleia-global-mode 1))

;;;; Node, Deno, and Astro --------------------------------------------------------

(use-package astro-ts-mode
  :ensure t
  :mode "\\.astro\\'")

(let ((astro-recipe (make-treesit-auto-recipe
                     :lang 'astro
                     :ts-mode 'astro-ts-mode
                     :url "https://github.com/virchau13/tree-sitter-astro"
                     :revision "master"
                     :source-dir "src")))
  (add-to-list 'treesit-auto-recipe-list astro-recipe))

;; Define some configuration for the Astro LSP. We use this a few times later.
(defvar s3thi/astro-lsp-config
  '("astro-ls" "--stdio"
   :initializationOptions
   (:typescript (:tsdk "node_modules/typescript/lib"))))

;; This function finds node_modules/.bin directories from the current file's
;; directory up to the home directory and adds them to the buffer-local
;; exec-path. This allows Emacs to find project-local LSP servers and other
;; tools without installing them globally.
(defun s3thi/add-node-modules-to-exec-path ()
  "Add node_modules/.bin directories to exec-path for the current buffer.
Searches from the current file's directory up to and including the
user's home directory, adding any node_modules/.bin directories found
to a buffer-local exec-path."
  (interactive)
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name))
          (home (expand-file-name "~"))
          (found-paths '()))
      ;; Walk up the directory tree until we pass home
      (while (and dir (string-prefix-p home dir))
        (let ((node-bin (expand-file-name "node_modules/.bin" dir)))
          (when (file-directory-p node-bin)
            (push node-bin found-paths)))
        ;; Move to parent directory
        (let ((parent (file-name-directory (directory-file-name dir))))
          (setq dir (unless (string= parent dir) parent))))
      ;; Make exec-path buffer-local and add found paths
      (when found-paths
        (make-local-variable 'exec-path)
        (dolist (path found-paths)
          (add-to-list 'exec-path path))))))

;; Configuring the language server for Deno here so I can use it later in the
;; Eglot configuration section.

;; We need to make sure eglot is loaded before we try any subclassing
;; shenanigans.
(use-package eglot)

(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
  "Passes through required deno initialization options"
  (list
   :enable t
   :unstable t))

;; This function decides whether a project uses Node, Deno, or Astro and
;; picks the right TypeScript LSP server.
(defun s3thi/eglot-server-for-ts (&optional interactive)
  "Returns the correct LSP server for TypeScript based on project type."
  (let ((project-root (project-root (project-current))))
    (cond
     ((and project-root
          (or (file-exists-p (expand-file-name "deno.json" project-root))
              (file-exists-p (expand-file-name "deno.jsonc" project-root))))
      (list 'eglot-deno "deno" "lsp"))
     ((and project-root
           (file-exists-p (expand-file-name "astro.config.mjs" project-root)))
      s3thi/astro-lsp-config)
     (t
      (list 'eglot-lsp-server "typescript-language-server" "--stdio")))))

;;;; Web development hooks --------------------------------------------------------

;; Unified setup for JS, TS, and Astro modes. This ensures node_modules is
;; added to exec-path before Eglot starts. Code formatting is handled globally
;; by apheleia.
(defun s3thi/setup-web-dev ()
    "Set up environment for JS/TS/Astro development."
    (s3thi/add-node-modules-to-exec-path)
    (eglot-ensure))

  (add-hook 'js-ts-mode-hook #'s3thi/setup-web-dev)
  (add-hook 'typescript-ts-mode-hook #'s3thi/setup-web-dev)
  (add-hook 'astro-ts-mode-hook #'s3thi/setup-web-dev)

;;; Utilities and key bindings ====================================================

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

;;; Server ========================================================================

;; Let's start the server.
(server-start)

(provide 'init)

;;; init.el ends here
