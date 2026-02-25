;;; init-prog-web.el --- Web development configuration -*- lexical-binding: t -*-

;;; Commentary:
;; JavaScript, TypeScript, Astro, Node, and Deno support.

;;; Code:

(use-package astro-ts-mode
  :ensure t
  :mode "\\.astro\\'")

(let ((astro-recipe (make-treesit-auto-recipe
                     :lang 'astro
                     :ts-mode 'astro-ts-mode
                     :url "https://github.com/virchau13/tree-sitter-astro"
                     :revision "master"
                     :source-dir "src")))
  (add-to-list 'treesit-auto-recipe-list astro-recipe))

;; Configuration for the Astro LSP, used in eglot-server-programs and in
;; s3thi/eglot-server-for-ts below.
(defvar s3thi/astro-lsp-config
  '("astro-ls" "--stdio"
    :initializationOptions
    (:typescript (:tsdk "node_modules/typescript/lib"))))

;; Astro-specific Prettier formatter for apheleia.
(with-eval-after-load 'apheleia
  (setf (alist-get 'prettier-astro apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath
          "--plugin=prettier-plugin-astro" "--parser=astro"))
  (setf (alist-get 'astro-ts-mode apheleia-mode-alist)
        '(prettier-astro)))

;; This function finds node_modules/.bin directories from the current file's
;; directory up to the home directory and adds them to the buffer-local
;; exec-path. This allows Emacs to find project-local LSP servers and other
;; tools without installing them globally.
(defun s3thi/add-node-modules-to-exec-path ()
  "Add node_modules/.bin directories to exec-path for the current buffer.
Searches from the current file's directory up to and including the
user's home directory, adding any node_modules/.bin directories found
to a buffer-local exec-path."
  (interactive)
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name))
          (home (expand-file-name "~"))
          (found-paths '()))
      ;; Walk up the directory tree until we pass home
      (while (and dir (string-prefix-p home dir))
        (let ((node-bin (expand-file-name "node_modules/.bin" dir)))
          (when (file-directory-p node-bin)
            (push node-bin found-paths)))
        ;; Move to parent directory
        (let ((parent (file-name-directory (directory-file-name dir))))
          (setq dir (unless (string= parent dir) parent))))
      ;; Make exec-path buffer-local and add found paths
      (when found-paths
        (make-local-variable 'exec-path)
        (dolist (path found-paths)
          (add-to-list 'exec-path path))))))

;; Eglot must be loaded before we can subclass eglot-lsp-server.
(require 'eglot)

(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
  "Passes through required deno initialization options"
  (list
   :enable t
   :unstable t))

;; This function decides whether a project uses Node, Deno, or Astro and
;; picks the right TypeScript LSP server.
(defun s3thi/eglot-server-for-ts (&optional interactive)
  "Returns the correct LSP server for TypeScript based on project type."
  (let ((project-root (project-root (project-current))))
    (cond
     ((and project-root
           (or (file-exists-p (expand-file-name "deno.json" project-root))
               (file-exists-p (expand-file-name "deno.jsonc" project-root))))
      (list 'eglot-deno "deno" "lsp"))
     ((and project-root
           (file-exists-p (expand-file-name "astro.config.mjs" project-root)))
      s3thi/astro-lsp-config)
     (t
      (list 'eglot-lsp-server "typescript-language-server" "--stdio")))))

;; Register LSP servers for web modes.
(add-to-list 'eglot-server-programs
             `(astro-ts-mode . ,s3thi/astro-lsp-config))
(add-to-list 'eglot-server-programs
             '((js-mode js-ts-mode typescript-mode typescript-ts-mode) . s3thi/eglot-server-for-ts))

;; Unified setup for JS, TS, and Astro modes. This ensures node_modules is
;; added to exec-path before Eglot starts. Code formatting is handled globally
;; by apheleia.
(defun s3thi/setup-web-dev ()
  "Set up environment for JS/TS/Astro development."
  (s3thi/add-node-modules-to-exec-path)
  (eglot-ensure))

(add-hook 'js-ts-mode-hook #'s3thi/setup-web-dev)
(add-hook 'typescript-ts-mode-hook #'s3thi/setup-web-dev)
(add-hook 'tsx-ts-mode-hook #'s3thi/setup-web-dev)
(add-hook 'astro-ts-mode-hook #'s3thi/setup-web-dev)

(provide 'init-prog-web)

;;; init-prog-web.el ends here
