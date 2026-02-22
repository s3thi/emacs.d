;;; init-completion.el --- Completion configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configure completion frameworks.

;;; Code:

;; Use vertico as the completion UI.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Use orderless to filter completions.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enable annotations in the minibuffer using marginalia.
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(provide 'init-completion)

;;; init-completion.el ends here
