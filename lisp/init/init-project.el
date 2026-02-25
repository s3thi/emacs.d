;;; init-project.el --- Project configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configure project.el.

;;; Code:

;; project-vc-extra-root-markers is a list of file names or glob patterns which
;; mark a project's root in addition to the default .git, .hg and other common
;; markers. This allows you to mark non-code directories as projects that the
;; built-in project.el recognizes.
(setq project-vc-extra-root-markers
      '(".project.el" ".jj" "package.json" "deno.json"))

;; Make sure the macOS .DS_Store files don't show up in project-find-file.
(setq project-vc-ignores '(".DS_Store"))

;; Open ibuffer with only project buffers visible.
(defun s3thi/project-ibuffer ()
  (interactive)
  (let* ((proj (project-current t))
         (root (project-root proj))
         (bufs (project-buffers proj)))
    (ibuffer nil (format "*ibuffer: %s*" root)
             `((predicate . (memq (current-buffer) ',bufs))))))

(keymap-set project-prefix-map "C-b" #'s3thi/project-ibuffer)

(provide 'init-project)

;;; init-project.el ends here
