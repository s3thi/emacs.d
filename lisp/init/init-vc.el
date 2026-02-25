;;; init-vc.el --- Version control configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Magit and other version control tools.

;;; Code:

;; Magit needs a newer transient than the one built into Emacs.
(use-package transient
  :ensure t)

;; Love me some Magit.
(use-package magit
  :ensure t
  :after transient)

;; And also some Majutsu.
(use-package majutsu
  :ensure t
  :vc (:url "https://github.com/0WD0/majutsu")
  :bind (("C-x j" . majutsu)))

(provide 'init-vc)

;;; init-vc.el ends here
