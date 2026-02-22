;;; init-input-methods.el --- Input methd configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Input method configuration for typing symbols and non-English languages.

;;; Code:

;; Add a few things to the C-x 8 bindings.
(use-package iso-transl
  :config
  (define-key iso-transl-ctl-x-8-map "r" [?₹])
  (define-key iso-transl-ctl-x-8-map ".3" [?…])
  (define-key iso-transl-ctl-x-8-map "m" [?—]))

;; जब मैं toggle-input-method कॉल करूं तो कौनसा इन्पुट मैथड सेलेक्ट होना चाहिये.
(setq default-input-method "devanagari-itrans")

;; कभी चीटशीट देखने की ज़रूरत पढ़ जाये तो.
(global-set-key
 (kbd "C-c I")
 (lambda ()
   "Describe the devanagari-itrans input method."
   (interactive)
   (describe-input-method "devanagari-itrans")))

(provide 'init-input-methods)

;;; init-input-methods.el ends here
