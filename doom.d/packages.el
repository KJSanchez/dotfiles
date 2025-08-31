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

;; TODO: find a sensible way to use perspectives workspaces, and tabs
(package! tabspaces :disable t)

;; (package! unique-dir-name
;;   :recipe (:host github :repo "abougouffa/unique-dir-name"))

;; (package! one-tab-per-project
;;   :recipe (:host github :repo "abougouffa/one-tab-per-project"))

(package! nix-mode :disable t)

(package! feature-mode
  :recipe (:host github
           :repo "freesteph/cucumber.el"))

(package! activities)

(package! lsp-tailwindcss
  :disable
  (unless (modulep! :lang javascript))
  :recipe (:host github
           :repo "merrickluo/lsp-tailwindcss"))

(package! cobol-mode)

(package! rg
  :recipe (:host github
           :repo "dajva/rg.el"))

(package! rainbow-mode
  :disable
  (unless (modulep! :lang javascript)))

;; (package! aider)

;; NOTE: aidermacs doesn't work, but... aider-macs does.
;; (aidermacs-run)
(package! aider
  :recipe (:host github :repo "MatthewZMD/aidermacs"))

;; (package! combobulate :recipe (:host github :repo "mickeynp/combobulate" :files ("*.el")))


(package! prettier-elisp
  :recipe (:host github
           :repo "KarimAziev/prettier-elisp"))
