;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el


(package! realgud-lldb :disable (unless (modulep! :lang cc)))
(package! hy-mode :disable (unless (modulep! :lang python)))
(package! python-coverage :disable (unless (modulep! :lang python)))
(package! jinja2-mode :disable (unless (modulep! :lang python)))

(package! kubernetes :disable (unless (modulep! :tools docker)))

(package! aidermacs :disable t)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

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

(package! feature-mode
  :recipe (:host github :repo "freesteph/cucumber.el"))

(package! activities
  :recipe (:host github :repo "alphapapa/activities.el"))

(package! lsp-tailwindcss
  :disable (unless (modulep! :lang javascript))
  :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! cobol-mode)

(package! rg
  :recipe (:host github :repo "dajva/rg.el"))

(package! rainbow-mode
  ;; Only need this for `tailwind.config.ts' so far.
  :disable (unless (modulep! :lang javascript)))

(package! aidermacs :disable t)

;; (package! combobulate :recipe (:host github :repo "mickeynp/combobulate" :files ("*.el")))
