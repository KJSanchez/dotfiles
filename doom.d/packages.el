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

(package! eldoc-box
  :disable (unless (modulep! :tools lsp +eglot)))

(package! f)

(package! ov :disable t)

(package! imenu-list)

(package! feature-mode
  :recipe (:host github
           :repo "freesteph/cucumber.el"))

(package! rainbow-mode
  :disable
  (unless (modulep! :lang javascript)))

(package! treesit-auto
  :disable (unless (modulep! :tools tree-sitter)))

(package! evil-textobj-tree-sitter
  :disable (unless (and (modulep! :tools tree-sitter)
                        (modulep! :editor evil))))

(package! combobulate :recipe (:host github :repo "mickeynp/combobulate" :nonrecursive t))
