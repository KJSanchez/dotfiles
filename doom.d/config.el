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

(load! "functions.el")

(use-package! vterm
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)

  (setq +workspaces-on-switch-project-behavior t)

  (map! :leader
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

  (map! :map vterm-mode-map
        :e
        "<escape>" #'+workspace/other)

  (map! :leader
        "o t" (cmd!
               (if (++workspace/workspace-p "vterm")
                   (++workspace/switch-to-by-name "vterm")
                 (progn
                   (+workspace/new "vterm")
                   (+vterm/here nil))))))

(map! :leader
      :desc "Display tab bar"
      "TAB f" #'+workspace/display)

(map! :leader
      :desc "Switch to workspace"
      "TAB TAB" (cmd!
                 (ivy-read "Switch to workspace: "
                           (+workspace-list-names)
                           :action #'+workspace-switch
                           :caller #'+workspace-switch)
                 (+workspace-list-names)))

(map! :leader
      :desc "Switch to last workspace"
      "TAB l" #'+workspace/other)

(map! :leader
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


;; (use-package! evil
;;   :conifg
;;   (setq evil-escape-key-sequence "kj"))

;; TODO set a smaller font in the imenu buffer.
(setq
 org-directory "~/org/"
 display-line-numbers-type t
 evil-escape-key-sequence "kj"
 initial-major-mode 'emacs-lisp-mode
 confirm-kill-emacs nil
 ;; flycheck-disabled-checkers '(python-mypy)
 projectile-project-search-path '("~/codez/")
 )

(use-package! doom
  :config
  ;; (setq doom-theme 'doom-wilmersdorf)
  ;; (setq doom-font (font-spec :family "Fira Code" :size 12))
  (setq doom-theme 'doom-spacegrey))

;; (use-package! flycheck
;;   :config
;;   (setq flycheck-disabled-checkers '(python-mypy)))


(use-package! doom-modeline
  :config
  (add-hook 'imenu-list-minor-mode-hook #'hide-mode-line-mode)
  (setq
   doom-modeline-vcs-max-length 12
   doom-modeline-buffer-encoding nil
   domm-modeline-workspace-name t
   doom-modeline-time-icon nil
   doom-modeline-modal nil
   doom-modeline-percent-position nil
   doom-modeline-github t)

  (add-hook! doom-modeline-mode-hook
    (column-number-mode -1)
    (line-number-mode -1)
    (size-indication-mode -1)))

(setq-default
 fill-column 88
 git-commit-summary-max-length 100)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("django.config\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '(".clang-format" . conf-mode))
(setq initial-frame-alist '((top . 1) (left . 1) (width . 160) (height . 55)))

;; (add-to-list '+format-on-save-enabled-modes 'mhtml-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'sh-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'json-mode :append)

(map! :leader
      :desc "imenu"
      "t i" #'imenu-list-minor-mode)

(use-package! centaur-tabs
  :when (modulep! :ui tabs)
  :config
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker nil
        ;; centaur-tabs-close-button ""
        ;; centaur-tabs-modified-marker "x"
        centaur-tabs-style "bar"
        centaur-tabs-height 16
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'vterm-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'python-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'compilation-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'inferior-python-mode-hook 'centaur-tabs-local-mode))

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
      :when (modulep! :tools debugger)
      "o D" #'debugger/start)

(map! :leader
      :when (modulep! :tools docker)
      :desc "Docker"
      "o d" #'docker)

(map! :leader
      :when (modulep! :config default)
      "c x"
      (cmd! (+default/diagnostics)
            (switch-to-buffer-other-window "*Flycheck errors*")))

(map! :leader
      :when (modulep! :completion ivy)
      :desc "compile"
      "c C" #'+ivy/compile)

(map! :leader
      :desc "+make/run"
      "c r" #'+make/run)

(map! :leader
      :desc "+make/run-last"
      "c l" #'+make/run-last)

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

(map! :map python-mode-map
      :prefix "shell"
      :localleader
      "e B" #'run-python
      "e b" #'python-shell-send-buffer)

(map! :map python-mode-map
      :n ", f" (cmd! (evil-buffer-new) (inferior-python-mode))
      :n ", e" #'python-shell-send-buffer)

(map! :map compilation-mode-map
      :n "q" #'quit-window)

;; (map! :map python-mode-map
;;       ;; :prefix ("test" . "test code")
;;       :localleader
;;       "t" #'python-pytest-dispatch)

(map! :leader
      :desc "global flycheck mode"
      "t z" #'global-flycheck-mode
      :desc "global toggle modeline"
      "t m" #'hide-mode-line-mode
      :desc "global toggle modeline"
      "t M" #'global-hide-mode-line-mode)

(map! :map org-mode-map
      :localleader
      "a" #'org-show-all)

(set-popup-rule! "*helpful function:" :height 100)
(set-popup-rule! "*helpful macro:" :height 100)
(set-popup-rule! "*helpful command:" :height 25 :side 'bottom)
(set-popup-rule! "*helpful variable:" :height 25 :side 'bottom)
(set-popup-rule! "*Ilist*" :side 'right :width 50 :select t)
(set-popup-rule! "*ert*" :side 'right :width 60 :select t)
(set-popup-rule! "*compilation*" :select nil :width .5 :side 'right)
(set-popup-rule! "*Anaconda*" :height 25)
(set-popup-rule! "*pytest*" :height .25 :select t)

(after! dired
  (add-hook! 'dired-mode-hook #'dired-hide-details-mode))

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded))

(add-hook! emacs-lisp-mode
  (when (string= (buffer-name) "functions.el")
    (udd-hook 'evil-insert-state-exit-hook #'eval-buffer nil t))
  (when (string= (buffer-name) "*doom:scratch*")
    (add-hook 'evil-insert-state-exit-hook #'eval-buffer nil t))
  (when (string= (buffer-name) "*scratch*")
    (add-hook 'evil-insert-state-exit-hook #'eval-buffer nil t)))


(global-visual-line-mode t)

;; (setq native-comp-async-report-warnings-errors nil)


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

;; (add-hook! c++mode
;;   (setq c-basic-offset 2))
