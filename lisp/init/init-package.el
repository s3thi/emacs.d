;;; init-package.el --- Package configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Package setup.

;;; Code:

(require 'package)

;; Allow updating built-in packages.
(setq package-install-upgrade-built-in t)

;; Enable MELPA.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Initialize packages.
(package-initialize)

(defvar s3thi/package-upgrade-reminder-interval 7
  "Number of days between package upgrade reminders.")

(defvar s3thi/package-upgrade-timestamp-file
  (expand-file-name ".last-package-upgrade" user-emacs-directory)
  "File that stores the timestamp of the last package upgrade.")

;; Remind me to upgrade packages if it has been more than 7 days since the last
;; upgrade. The timestamp is stored in ~/.emacs.d/.last-package-upgrade. Run M-x
;; package-upgrade-all to upgrade and dismiss the reminder.
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

(provide 'init-package)

;;; init-package.el ends here
