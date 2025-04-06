;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el


(package! realgud-lldb :disable (unless (modulep! :lang cc)))
(package! hy-mode  :disable (unless (modulep! :lang python)))
(package! python-coverage :disable (unless (modulep! :lang python)))

(package! kubernetes :disable (unless (modulep! :tools docker)))

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! csv-mode :disable t)
(package! dash)
(package! f)
(package! ov :disable t)
(package! imenu-list)

;; TODO: find a sensible way to use perspectives workspaces, and tabs
(package! tabspaces :disable t)

;; (package! unique-dir-name
;;   :recipe (:host github :repo "abougouffa/unique-dir-name"))

;; (package! one-tab-per-project
;;   :recipe (:host github :repo "abougouffa/one-tab-per-project"))

(package! nix-mode :disable t)

(package! feature-mode)

(package! lsp-tailwindcss
  :disable (unless (modulep! :lang javascript))
  :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! rg
  :recipe (:host github :repo "dajva/rg.el"))
