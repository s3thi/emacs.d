;;; s3thi-bootstrap.el --- Bootstrap configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Core setup: encoding, personal info, platform detection, environment
;; variables, and package upgrade reminders.

;;; Code:

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

(provide 's3thi-bootstrap)

;;; s3thi-bootstrap.el ends here
