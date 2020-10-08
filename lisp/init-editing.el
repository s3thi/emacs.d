(defun s3thi/smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

Source: https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/"
  (interactive "^p")
  (setq arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                #'s3thi/smart-move-beginning-of-line)


(setq-default indent-tabs-mode nil)

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . #'er/expand-region))

(use-package smartparens
  :ensure t
  :diminish
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))

(setq save-interprogram-paste-before-kill t)

(provide 'init-editing)
