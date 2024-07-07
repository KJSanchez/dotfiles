;;; ../codez/dotfiles/doom.d/doctor.el -*- lexical-binding: t; -*-


;; TODO: this is giving a false positive.
(unless (find-font (font-spec :family "Fira Code"))
  (warn! "fira code font not found."))
