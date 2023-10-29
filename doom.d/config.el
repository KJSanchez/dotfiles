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
;; doom-font (font-spec :family "Fira Code" :size 12)
 doom-theme 'doom-spacegrey
 doom-theme 'doom-wilmersdorf
 doom-modeline-vcs-max-length 12
 doom-modeline-buffer-encoding nil
 domm-modeline-workspace-name t
 doom-modeline-time-icon nil
 doom-modeline-modal nil
 doom-modeline-percent-position nil
 doom-modeline-github t
 org-directory "~/org/"
 display-line-numbers-type t
 evil-escape-key-sequence "kj"
 initial-major-mode 'emacs-lisp-mode
 confirm-kill-emacs nil
 ;; flycheck-disabled-checkers '(python-mypy python-pylint)
 ;; flycheck-disabled-checkers '(python-mypy)
 +workspaces-on-switch-project-behavior t)
 ;; which-key-idle-delay .01
 ;; which-key-idle-secondary-delay .01)

(setq-default
 fill-column 88
 git-commit-summary-max-length 100)
;; git-commit-style-convention-checks (remove 'overlong-summary-line git-commit-style-convention-checks)


(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("django.config\\'" . yaml-mode))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list '+format-on-save-enabled-modes 'mhtml-mode t)
(add-to-list '+format-on-save-enabled-modes 'sh-mode t)
(add-to-list '+format-on-save-enabled-modes 'json-mode :append)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (setq copilot-node-executable "node"))


(map! :leader
      ;; :after workspaces
      "TAB 0" nil
      "TAB 1" nil
      "TAB 2" nil
      "TAB 3" nil
      "TAB 4" nil
      "TAB 5" nil
      "TAB 6" nil
      "TAB 7" nil
      "TAB 8" nil
      "TAB 9" nil)

(map! :leader
      ;; :after workspaces
      :desc "Display tab bar"
      "Tab f" #'+workspace/display)

(map! :leader
      ;; :after workspaces
      :desc "Switch to workspace"
      "TAB TAB" (cmd!
                 (ivy-read "Switch to workspace: "
                           (+workspace-list-names)
                           :action #'+workspace-switch
                           :caller #'+workspace-switch)
                 (+workspace-list-names)))


(map! :leader
      :desc "Switch to last workspace"
      "TAB l" #'+workspace:switch-previous)

(map! :leader
      ;; :after workspaces
      :desc "Create workspace here"
      "TAB n" (cmd!
               (condition-case _
                   (progn
                     (+workspace/new (projectile-project-name) t)
                     (+workspace/display))
                 (error
                  (progn
                    (+workspace-switch (projectile-project-name) t)
                    (+workspace/display))))))

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

(map! :leader
      :desc "Start debugger"
      "o D" #'debugger/start
      ;; :desc "eshell"
      ;; "o t" #'eshell
      :desc "Docker"
      "o d" #'docker)

(map! :leader
      "c x"
      (cmd! (+default/diagnostics)
            (switch-to-buffer-other-window "*Flycheck errors*")))

;; Switch compile commands

(map! :leader
      :desc "compile"
      "c C" #'+ivy/compile)

(map! :leader
      :desc "recompile"
      "c c" #'recompile)

(map! :leader "w o" #'delete-other-windows)

(map! :map python-mode-map
      :prefix "coverage"
      :localleader
      :desc "toggle coverage overlay"
      "c c" #'python-coverage-overlay-mode
      :desc "refresh coverage overlay"
      "c r" #'python-coverage-overlay-refresh)

(map! :leader
      :desc "global toggle modeline"
      "t m" #'hide-mode-line-mode
      :desc "global toggle modeline"
      "t M" #'global-hide-mode-line-mode)

;; (map! :leader
;;       :desc "Switch to functions.sh"
;;       "b f"
;;       (cmd! ()))

(define-key evil-normal-state-map (kbd "RET") #'recompile)

(set-popup-rule! "helpful function:" :height 25 :side 'bottom)
(set-popup-rule! "helpful macro:" :height 25 :side 'bottom)
(set-popup-rule! "helpful command:" :height 25 :side 'bottom)
(set-popup-rule! "helpful variable:" :height 25 :side 'bottom)
(set-popup-rule! "*Ilist*" :side 'right :width 50 :select t)
(set-popup-rule! "*ert*" :side 'right :width 60 :select t)
(set-popup-rule! "*compilation*" :select t :height 50)


(add-hook! 'dired-mode-hook #'dired-hide-details-mode)

(add-hook! emacs-lisp-mode
  (add-hook 'before-save-hook #'eval-buffer nil t)
  (when (string= (buffer-name) "*doom:scratch*")
    (add-hook 'evil-insert-state-exit-hook #'eval-buffer nil t))
  (when (string= (buffer-name) "*scratch*")
    (add-hook 'evil-insert-state-exit-hook #'eval-buffer nil t)))

(add-hook! js-mode
  (setq js-indent-level 2))


(after! magit
  (magit-add-section-hook
   'magit-refs-sections-hook
   'magit-insert-branch-description))

(after! modeline
  (column-number-mode -1)
  (line-number-mode -1)
  (size-indication-mode -1))
