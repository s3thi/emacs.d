;;; s3thi-writing.el --- Writing configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Prose editing setup: visual-fill-column, spell checking, mixed-pitch,
;; input methods, Org mode, Markdown, and daily notes/journal.

;;; Code:

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

(provide 's3thi-writing)

;;; s3thi-writing.el ends here
