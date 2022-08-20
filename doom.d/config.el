;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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

;; TODO set a smaller font in the imenu buffer.

(setq
 user-full-name "Keenan Sanchez"
 user-mail-address "keenan@gainful.com"
 doom-theme 'doom-city-lights
 org-directory "~/org/"
 display-line-numbers-type t
 evil-escape-key-sequence "kj"
 initial-major-mode 'emacs-lisp-mode
 doom-modeline-github t)
 ;; which-key-idle-delay .01
 ;; which-key-idle-secondary-delay .01)


(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 160))

(after! centaur-tabs
  (add-to-list 'centaur-tabs-excluded-prefixes "*Messages*")
  (add-to-list 'centaur-tabs-excluded-prefixes "*doom*"))

(setq-default
 fill-column 88
 git-commit-summary-max-length 68)
;; git-commit-style-convention-checks (remove 'overlong-summary-line git-commit-style-convention-checks)

(map! :leader
      :desc "Global Flycheck Mode"
      "t f" #'global-flycheck-mode

      :desc "imenu"
      "t i" #'imenu-list-minor-mode

      :desc "global visual line mode"
      "t w" #'global-visual-line-mode

      :desc "Centaur tabs"
      "t T" #'centaur-tabs-mode

      :desc "toggle transparency"
      "t t" (cmd!
             (let ((alpha (frame-parameter nil 'alpha)))
               (if (eq
                    (if (numberp alpha)
                        alpha
                      (cdr alpha)) ; may also be nil
                    100)
                   (set-frame-parameter nil 'alpha '(60 . 50))
                 (set-frame-parameter nil 'alpha '(100 . 100)))))

      ;; :desc "relative line numbers"
      ;; "t l" (cmd!
      ;;        (setq ))

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
      ;; "b i" nil        ;; ibuffer
      "`" nil
      "*" nil
      "f E" nil        ;; Browse emacs.d
      ;; "f f" (cmd! (error "Use SPC SPC instead."))
      ;; ":" nil       ;; M-x

      ;; TODO how do I map to SPC?
      ;; "SPC" (cmd! (error "use SPC p f instead."))

      ;; "TAB TAB" #'+vterm/here

      :desc "Install a package"
      "h i" #'package-install

      :desc "Vterm"
      "T" #'+vterm/here

      :desc "Switch to last buffer"
      "." #'evil-switch-to-windows-last-buffer

      :desc "edit snippet"
      "c s" #'+snippets/find-private

      ;; FIXME: Why doesn't evil escape key sequence work in vterm?
      ;; :after vterm-mode
      (:map vterm-mode-map
       :i "kj" #'+evil-force-normal-state)

      (:map emacs-lisp-mode-map
       :desc "run ert tests"
       :localleader
       "t" (cmd! (ert t))))

(map! :map python-mode-map
      :prefix "coverage"
      :localleader
      :desc "toggle coverage overlay"
      "c c" #'python-coverage-overlay-mode
      :desc "refresh coverage overlay"
      "c r" #'python-coverage-overlay-refresh)

(set-popup-rule! "helpful function:" :height 25 :side 'bottom)
(set-popup-rule! "helpful macro:" :height 25 :side 'bottom)
(set-popup-rule! "helpful command:" :height 25 :side 'bottom)
(set-popup-rule! "helpful variable:" :height 25 :side 'bottom)
;; FIXME why isn't :select option working?
(set-popup-rule! "*Ilist*" :side 'right :width 50 :select t)
;; TODO Add ENTER to ilist-mode-map
(set-popup-rule! "*ert*" :side 'right :width 60 :select t)

(add-hook! emacs-lisp-mode
  (add-hook 'before-save-hook #'eval-buffer nil t)
  (when (string= (buffer-name) "*doom:scratch*")
    (add-hook 'evil-insert-state-exit-hook #'eval-buffer nil t))
  (when (string= (buffer-name) "*scratch*")
    (add-hook 'evil-insert-state-exit-hook #'eval-buffer nil t)))

(add-hook! vterm-mode
  (centaur-tabs-local-mode nil))

(add-hook! imenu-list-minor-mode
  (centaur-tabs-local-mode nil))

;; # TODO How should debuggers integrate with emacs?
;; # TODO breakpoint snippets
;; # TODO exclude popup buffers from centaur-tabs


(require 'ov)
(require 'f)
(require 's)

(add-hook! python-mode
  (when (-> (f-this-file) f-filename (string= "urls.py"))
    (highlight-unused-routes)))

(defun highlight-unused-routes ()
  "(WIP) Highlight url routes that haven't been queried in the past 60 days."
  (interactive)

  (remove-overlays (point-min) (point-max))

  (->> "/Users/keenan/dotfiles/doom.d/unused_routes.txt"
       f-read-text
       (s-replace "/api/admin/" "")
       (s-replace "/api/" "")
       (s-replace "/blog/" "")
       (s-replace "/survey/admin/" "")
       (s-replace "/admin/" "")
       (s-replace "/api/quiz/" "")
       (s-replace "/quiz/" "")
       (s-replace "/users/" "")
       (replace-regexp-in-string "^/" "")
       s-lines
       (mapc (lambda (api-route)
               (ov-set (ov-regexp api-route) 'face 'error)))))
