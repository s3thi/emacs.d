;;; init-notes.el --- Notes configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Initialization for my personal notes functions.

;;; Code:

;; TODO: a lot of this stuff only makes sense in the notes directory.
;; It should become a minor mode that only applies to those files.

(defvar s3thi/notes-tags
  '(("blog"        "C-t b" "Blog")
    ("therapy"     "C-t t" "Therapy")
    ("programming" "C-t p" "Programming")
    ("idea"        "C-t i" "Idea")
    ("homework"    "C-t h" "Homework")
    ("writing"     "C-t w" "Writing")
    ("hip-hop"     "C-t H" "Hip-hop")
    ("someday"     "C-t s" "Someday")
    ("commonplace" "C-t c" "Commonplace"))
  "List of tags used in notes. Each entry is (TAG-NAME MENU-KEY MENU-LABEL).")

;; Highlight tags from s3thi/notes-tags in Markdown buffers.
(defun s3thi/highlight-tags ()
  (font-lock-add-keywords
   'markdown-mode
   (mapcar (lambda (tag)
             (list (concat "#" (car tag) "\\b") 0 ''warning 'prepend))
           s3thi/notes-tags)))

(with-eval-after-load 'markdown-mode (s3thi/highlight-tags))

;; Add keywords to the ripgrep search transient menu.
(defun s3thi/define-tag-search ()
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

(with-eval-after-load 'rg (s3thi/define-tag-search))

(defvar s3thi/notes-directory
  "/Users/s3thi/Library/Mobile Documents/27N4MQEA55~pro~writer/Documents/"
  "Directory where notes are stored.")

(defun s3thi/date-tree-path (root &optional time)
  "Return date-tree path for today's note or for the note at time."
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

(provide 'init-notes)

;;; init-notes.el ends here
