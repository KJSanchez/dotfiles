;;; ../codez/dotfiles/doom.d/doctor.el -*- lexical-binding: t; -*-

(when (modulep! :lang python +conda)
  (unless (executable-find "conda")
    (warn! "conda is not installed.")))

;; TODO: this is giving a false positive.
;; (unless (find-font (font-spec :family "Fira Code"))
;;   (warn! "fira code font not found."))


(when (and
       (modulep! :lang javascript)
       (modulep! :tools lsp +eglot))
  (unless (executable-find "rass")
    (warn! "rassumfrassum is not installed."))
  (unless (executable-find "eslint-lsp")
    (warn! "eslint-lsp is not installed."))
  (unless (executable-find "tailwindcss-language-server")
    (warn! "tailwindcss-language-server is not installed."))
  (unless (executable-find "typescript-language-server")
    (warn! "typescript-language-server is not installed.")))


