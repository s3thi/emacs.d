;;; alive-and-well.el --- convenience functions for publishing to my personal blog -*- lexical-binding: t -*-

;; Copyright (C) 2021 Ankur Sethi

;; Author: Ankur Sethi <contact@ankursethi.in>
;; Homepage: https://github.com/s3thi/emacs.d
;; Keywords: convenience

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

(setq aaw/post-template
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

(setq aaw/page-template
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

(setq aaw/blog-directory (expand-file-name "~/Code/ankursethi.in/"))

(setq aaw/clean-punct-regexp "[[:punct:]]")
(setq aaw/slugify-regexp "[[:space:]]")

(defun aaw/slugify (title)
  (let* ((downcased (downcase title))
         (without-punctuation
          (replace-regexp-in-string aaw/clean-punct-regexp "" downcased))
         (slugified
          (replace-regexp-in-string aaw/slugify-regexp "-" without-punctuation)))
    slugified))
  
(defun aaw/new-post ()
  (interactive)
  (let* ((title (read-from-minibuffer "Enter post title: "))
         (slugified-title (aaw/slugify title))
         (time (current-time))
         (year (format-time-string "%Y" time))
         (month (format-time-string "%m" time))
         (day (format-time-string "%d" time))
         (date-string (format "%s-%s-%s" year month day))
         (post-file-path
          (concat
           aaw/blog-directory
           "posts/"
           (format "%s-%s.md" date-string slugified-title)))
         (permalink (format "/%s/%s/%s/%s/" year month day slugified-title))
         (post-content (format aaw/post-template title date-string permalink)))
    (find-file post-file-path)
    (insert post-content)
    (save-buffer)))

;; Get current date and grab all components separately.
;; Use date to create a permalink path.
;; Get buffer content by formatting string template.
;; Create new file in the correct location.
;; Insert the template into the file and save it.
;; Switch user to the new buffer.

(defun aaw/new-page ()
  (message "Creating new page ..."))

(provide 'alive-and-well)

;;; alive-and-well.el ends here
