;;; ../codez/dotfiles/doom.d/doctor.el -*- lexical-binding: t; -*-

(require 'treesit)

(when (modulep! :tools tree-sitter)
  (when (modulep! :lang javascript)
    (unless (treesit-language-available-p 'tsx)
      (warn! "Tree-sitter parser for tsx is not installed.")))

  (when (modulep! :lang python)
    (unless (treesit-language-available-p 'python)
      (warn! "Tree-sitter parser for python is not installed."))))


(when (modulep! :lang python +conda)
  (unless (executable-find "conda")
    (warn! "conda is not installed.")))


(when (modulep! :lang javascript)
  (when (modulep! :tools lsp +eglot)
    (unless (executable-find "rass")
      (warn! "rassumfrassum is not installed."))
    (unless (executable-find "eslint-lsp")
      (warn! "eslint-lsp is not installed."))
    (unless (executable-find "tailwindcss-language-server")
      (warn! "tailwindcss-language-server is not installed."))
    (unless (executable-find "typescript-language-server")
      (warn! "typescript-language-server is not installed."))))


(when (featurep 'mermaid)
  (unless (executable-find "mmdc")
    (warn! "mermaid-cli is not installed.")))


;; TODO: this is giving a false positive.
;; (unless (find-font (font-spec :family "Fira Code"))
;;   (warn! "fira code font not found."))
