;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(load! "functions.el")

(setq
 user-full-name "Keenan Sanchez"
 user-mail-address "keenan@gainful.com"
 doom-theme 'doom-city-lights
 org-directory "~/org/"
 display-line-numbers-type t
 evil-escape-key-sequence "kj"
 initial-major-mode 'emacs-lisp-mode
 doom-modeline-github t
 which-key-idle-delay .01
 which-key-idle-secondary-delay .01)

;; git-commit-style-convention-checks (remove 'overlong-summary-line git-commit-style-convention-checks)

;; TODO:
;; (setq lsp-python-ms-python-executable-cmd "PYENV_VERSION=3.6.12 python")

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\Cask\\'" . emacs-lisp-mode))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 160))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;

(setq-default
 fill-column 88
 imenu-list-size .3 ;; TODO pretty sure this is broken.
 git-commit-summary-max-length 68)


(map! :leader
      :desc "Global Flycheck Mode"
      "t f" #'global-flycheck-mode

      :desc "imenu"
      "t i" #'imenu-list-minor-mode

      :desc "global visual line mode"
      "t w" #'global-visual-line-mode

      "TAB 0" nil
      "TAB 1" nil
      "TAB 2" nil
      "TAB 3" nil
      "TAB 4" nil
      "TAB 5" nil
      "TAB 6" nil
      "TAB 7" nil
      "TAB 8" nil
      "TAB 9" nil
      ;; "b i" nil  ;; ibuffer
      "`" nil
      "*" nil
      "f E" nil  ;; Browse emacs.d

      ;; :desc "Browse emacs.d"
      ;; "b E" (cmd! (let ((default-directory doom-emacs-dir))
      ;;               (projectile-find-file)))

      :desc "Eval expression"
      "RET" (cmd! (call-interactively #'execute-extended-command))

      ;; "TAB TAB" #'+vterm/here

      :desc "Install a package"
      "h i" #'package-install

      :desc "Switch to last buffer"
      "." #'evil-switch-to-windows-last-buffer

      ;; :after vterm-mode
      :map vterm-mode-map
      :i "kj" #'+evil-force-normal-state)


(set-popup-rule! "helpful function:" :height 25 :side 'bottom)
(set-popup-rule! "helpful macro:" :height 25 :side 'bottom)
(set-popup-rule! "helpful command:" :height 25 :side 'bottom)
(set-popup-rule! "*Ilist*" :side 'right :width 40 :select t)

(add-hook! emacs-lisp-mode
  (add-hook 'before-save-hook #'eval-buffer nil t)
  (when (string= (buffer-name) "*scratch*")
    (add-hook 'evil-insert-state-exit-hook #'eval-buffer nil t)))


(add-hook! vterm-mode
  (centaur-tabs-mode))


;; # TODO open vterm in seperate workspace
;; # TODO toggle debugger
;; # TODO breakpoint snippets
;; # TODO generic test function snippet
