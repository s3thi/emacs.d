(defun my-append-env-var (var-name value)
  "Append VALUE to the beginning of current value of env variable VAR-NAME."
  (setenv var-name (if (getenv var-name)
                       (format "%s:%s" value (getenv var-name))
                     value)))

;; Make sure Emacs can find GCC and libgccjit on macOS.
(if (eq system-type 'darwin)
    (let ((gccjitpath "/opt/homebrew/lib/gcc/current:/opt/homebrew/lib"))
      (mapc (lambda (var-name) (my-append-env-var var-name gccjitpath))
            '("LIBRARY_PATH" "LD_LIBRARY_PATH" "PATH"))))
