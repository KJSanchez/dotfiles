;;; ../codez/dotfiles/doom.d/doctor.el -*- lexical-binding: t; -*-

(if (modulep! :lang python +conda)
    (unless (executable-find "conda")
      (warn! "conda is not installed.")))

;; TODO: this is giving a false positive.
;; (unless (find-font (font-spec :family "Fira Code"))
;;   (warn! "fira code font not found."))
