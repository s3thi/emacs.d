;;; early-init.el --- Early init file for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This package sets up early initialization options for my Emacs
;; config.

;;; Code:

(defun s3thi/append-env-var (var-name value)
  "Append VALUE to the beginning of current value of env variable VAR-NAME."
  (setenv var-name (if (getenv var-name)
                       (format "%s:%s" value (getenv var-name))
                     value)))

;; Make sure Emacs can find GCC and libgccjit on macOS.
(if (eq system-type 'darwin)
    (let ((gccjitpath "/opt/homebrew/lib/gcc/current:/opt/homebrew/lib"))
      (mapc (lambda (var-name) (s3thi/append-env-var var-name gccjitpath))
            '("LIBRARY_PATH" "LD_LIBRARY_PATH" "PATH"))))

(provide 'early-init)

;;; early-init.el ends here
