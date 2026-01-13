;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! realgud-lldb :disable
  (unless (modulep! :lang cc)))

(package! python-coverage :disable
  (unless (modulep! :lang python)))

(package! jinja2-mode :disable
  (unless (modulep! :lang python)))

(package! kubernetes :disable
  (unless (modulep! :tools docker)))

(package! copilot
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el")))

(package! csv-mode :disable t)

(package! dash)

(package! f)

(package! ov :disable t)

(package! imenu-list)

(package! feature-mode
  :recipe (:host github
           :repo "freesteph/cucumber.el"))

(package! activities)

(package! lsp-tailwindcss
  :disable
  (unless (modulep! :lang javascript))
  :recipe (:host github
           :repo "merrickluo/lsp-tailwindcss"))

(package! rg
  :recipe (:host github
           :repo "dajva/rg.el"))

(package! rainbow-mode
  :disable
  (unless (modulep! :lang javascript)))

(package! aidermacs
  :recipe (:host github :repo "MatthewZMD/aidermacs"))

(package! treesit-auto
  :disable (unless (modulep! :tools tree-sitter)))
