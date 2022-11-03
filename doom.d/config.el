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
(add-to-list 'auto-mode-alist '("django.config\\'" . yaml-mode))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 160))

;; (after! centaur-tabs
;;   (add-to-list 'centaur-tabs-excluded-prefixes "*Messages*")
;;   (add-to-list 'centaur-tabs-excluded-prefixes "*doom*"))

(setq-default
 fill-column 88
 git-commit-summary-max-length 68)
;; git-commit-style-convention-checks (remove 'overlong-summary-line git-commit-style-convention-checks)

(map! :leader
      :desc "imenu"
      "t i" #'imenu-list-minor-mode)

(map! :leader
      :desc "global visual line mode"
      "t w" #'global-visual-line-mode)

(map! :leader
      :desc "tabs"
      "t T" #'tab-bar-mode)

(map! :leader
      :desc "transparency"
      "t t" (cmd! (let ((alpha (frame-parameter nil 'alpha)))
                    (if (eq
                         (if (numberp alpha)
                             alpha
                           (cdr alpha)) ; may also be nil
                         100)
                        (set-frame-parameter nil 'alpha '(60 . 50))
                      (set-frame-parameter nil 'alpha '(100 . 100))))))

(map! :leader
      :desc "Install a package"
      "h i" #'package-install)

(map! :leader
      :desc "Switch to last buffer"
      "l" #'evil-switch-to-windows-last-buffer)


(map!
 :map emacs-lisp-mode-map
 :desc "run ert tests"
 :localleader
 "t" (cmd! (ert t)))

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

(add-to-list '+format-on-save-enabled-modes 'mhtml-mode t)

;; # TODO How should debuggers integrate with emacs?


(require 'ov)
(require 'f)
(require 's)


(defun highlight-words-in-file ()
  "Highlight words in a file"
  (interactive)

  (remove-overlays (point-min) (point-max))

  (let ((file (read-string "File: ")))
    (->> file
         f-read-text
         s-lines
         (mapc (lambda (string-to-highlight)
                 (ov-set (ov-regexp string-to-highlight) 'face 'error))))))


(map! :after magit
      :map magit-mode-map
      :nv "]"
      #'+workspace:switch-previous)

(map! :leader
      "g s"
      (cmd! (+workspace-switch "magit" :auto-create-p)
            (magit-status-setup-buffer)))

(map! :leader "w o" #'delete-other-windows)

(map! :leader
      "c x"
      (cmd! (+default/diagnostics)
            (switch-to-buffer-other-window "*Flycheck errors*")))

(map! :leader
      "w v"
      (cmd! (switch-to-buffer-other-window
             (evil-switch-to-windows-last-buffer))))


(map! :leader
      :desc "eshell"
      "o t"
      (cmd! (+workspace-switch "eshell" :auto-create-p)
            (eshell)))
