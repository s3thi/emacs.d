;;; s3thi-vc.el --- Version control configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Magit and other version control tools.

;;; Code:

;; Love me some Magit.
(use-package magit
  :ensure t)

;; And also some Majutsu.
(use-package majutsu
  :ensure t
  :vc (:url "https://github.com/0WD0/majutsu")
  :bind (("C-x j" . majutsu)))

(provide 's3thi-vc)

;;; s3thi-vc.el ends here
