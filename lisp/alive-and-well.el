;;; alive-and-well.el --- Functions for publishing to my blog -*- lexical-binding: t -*-

;; Copyright (C) 2021 Ankur Sethi

;; Author: Ankur Sethi <contact@ankursethi.in>
;; Homepage: https://github.com/s3thi/emacs.d
;; Keywords: blog

;; Package-Requires: ((emacs "27"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Convenience functions for publishing to my personal blog that lives at
;; https://ankursethi.in and runs on Eleventy.

;;; Code:

(defvar aaw--post-template
"\
---
title: \"%s\"
date: %s
tags:
  - tag1
  - tag2
  - tag3
layout: layouts/post.njk
permalink: %s
---

Write your post here.

")

(defvar aaw--page-template
"\
---
title: \"%s\"
layout: layouts/page.njk
templateClass: tmpl-post
eleventyNavigation:
  key: \"%s\"
  order: %d
permalink: %s
---

Write your page content here.

")

(defvar aaw--blog-directory (expand-file-name "~/Code/ankursethi.in/"))

(defun aaw--is-inside-frontmatter ()
  "Return t if point is inside a Markdown frontmatter section."
  (save-excursion
    (let ((current-position (point))
          (begin (progn
                   (goto-char (point-min))
                   (search-forward-regexp "^---$" nil t)))
          (end (search-forward-regexp "^---$" nil t)))
      (and begin
           end
           (> current-position begin)
           (< current-position end)))))

(defun aaw--slugify (title)
  "Convert TITLE into a URL-save slug."
  (let* ((downcased (downcase title))
         (without-punctuation
          (replace-regexp-in-string "[[:punct:]]" "" downcased))
         (slugified
          (replace-regexp-in-string "[[:space:]]" "-" without-punctuation)))
    slugified))

(defun aaw--current-date-list ()
  "Return current date as a three-elment list.

The shape of the list is (year month day)."
  (let* ((time (current-time))
         (year (format-time-string "%Y" time))
         (month (format-time-string "%m" time))
         (day (format-time-string "%d" time)))
    `(,year ,month ,day)))

(defun aaw--date-list-to-string (date-list)
  "Convert DATE-LIST to a string with format \"year-month-day\"."
  (format "%s-%s-%s"
          (nth 0 date-list)
          (nth 1 date-list)
          (nth 2 date-list)))

(defun aaw--make-file-path (date-list slug)
  "Create a file path for a post using DATE-LIST and SLUG."
  (concat
   aaw--blog-directory
   "posts/"
   (format "%s-%s.md" (aaw--date-list-to-string date-list) slug)))

(defun aaw--make-permalink (date-list slug)
  "Create a permalink for a post using DATE-LIST and SLUG."
  (format "/%s/%s/%s/%s/"
          (nth 0 date-list)
          (nth 1 date-list)
          (nth 2 date-list)
          slug))

(defun aaw--last-monday ()
  "Return an Emacs date for last Monday."
  (parse-time-string
   (with-temp-buffer
     (call-process "env" nil t nil "date" "-d" "last monday")
     (or (bobp) (delete-backward-char 1))
     (buffer-string))))

(defun aaw--make-weeknote-title ()
  "Create a title for a new weeknote."
  (let ((last-monday (aaw--last-monday)))
    (format-time-string "Week of %-e %B, %Y" last-monday)))

(defun aaw-new-post ()
  "Create a new post."
  (interactive)
  (let* ((title (read-from-minibuffer "Enter post title: "))
         (slug (aaw--slugify title))
         (date-list (aaw--current-date-list))
         (date-string (aaw--date-list-to-string date-list))
         (post-path (aaw--make-file-path date-list slug))
         (permalink (aaw--make-permalink date-list slug))
         (post-content (format aaw--post-template title date-string permalink)))
    (find-file post-path)
    (insert post-content)
    (save-buffer)))

(defun aaw-new-weeknonnte ()
  "Create a new weeknote."
  (interactive)
  ;; Maybe just reuse the aaw-new-post logic?
  )

(defun aaw--file-in-blog-directory-p (file)
  "Return t if FILE is inside the blog directory, nil otherwise."
  (let ((file-full-path (expand-file-name file)))
    (string-prefix-p aaw--blog-directory file-full-path)))

(defun aaw--get-frontmatter-single-key-value (key)
  "Return value for KEY in the YAML frontmatter of buffer.

Return nil if no value is found."
    (save-excursion
      (goto-char (point-min))
      (when (and
             (search-forward (format "%s: " key) nil t)
             (aaw--is-inside-frontmatter))
        (string-trim
         (buffer-substring-no-properties (point) (line-end-position))
         "\""
         "\""))))

(defun aaw--set-frontmatter-single-key-value (key value)
  "Set value of KEY to VALUE in the YAML frontmatter of buffer."
    (save-excursion
      (goto-char (point-min))
      (when (and
             (search-forward (format "%s: " key) nil t)
             (aaw--is-inside-frontmatter))
        (kill-line)
        (insert (format "\"%s\"" value)))))

(defun aaw--post-title ()
  "Return current post's title from YAML frontmatter."
  (aaw--get-frontmatter-single-key-value "title"))

(defun aaw--post-date-list ()
  "Return current post's date as a date list."
  (split-string (aaw--get-frontmatter-single-key-value "date") "-"))

(defun aaw--post-permalink ()
  "Return current post's permalink from YAML frontmatter."
  (aaw--get-frontmatter-single-key-value "permalink"))

(defun aaw--update-metadata-on-save ()
  (when (aaw--file-in-blog-directory-p buffer-file-name)
    (let* ((title (aaw--post-title))
           (slug (aaw--slugify title))
           (date-list (aaw--post-date-list))
           (post-permalink (aaw--post-permalink))
           (post-path buffer-file-name)
           (new-post-path (aaw--make-file-path date-list slug))
           (new-post-permalink (aaw--make-permalink date-list slug))
           (post-path-changed-p (not (string= post-path new-post-path)))
           (post-permalink-changed-p
            (not (string= post-permalink new-post-permalink))))
      (when post-path-changed-p
        (rename-file post-path new-post-path)
        (set-visited-file-name new-post-path t t))
      (when post-permalink-changed-p
        (aaw--set-frontmatter-single-key-value "permalink" new-post-permalink))
      (when (or post-path-changed-p post-permalink-changed-p)
        (save-buffer)))))

(defun aaw-initialize ()
  (add-hook 'markdown-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        #'aaw--update-metadata-on-save
                        nil
                        'make-it-local))))

(provide 'alive-and-well)

;;; alive-and-well.el ends here
