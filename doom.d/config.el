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


(use-package! doom
  :config
  (setq doom-theme 'doom-spacegrey))

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



(use-package! vterm
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)

  ;; TODO: move this somewhere else.
  (setq +workspaces-on-switch-project-behavior t)

  (map! :map vterm-mode-map
        :e
        "M-<left>" (cmd! (vterm-send-key "\e")))
  ;; ;;       "M <right>" )


  (map! :when (modulep! :ui workspaces)
        :leader
        "o t" (cmd!
               (if (++workspace/workspace-p "vterm")
                   (++workspace/switch-to-by-name "vterm")
                 (progn
                   (+workspace/new "vterm")
                   (+vterm/here nil))))))

(use-package! imenu-list
  :config
  (map! :leader
        :desc "imenu"
        "t i" #'imenu-list-minor-mode))

;; (use-package! evil
;;   :conifg
;;   (setq evil-escape-key-sequence "kj"))

(map! :when (modulep! :editor evil)
      :leader
      :desc "Switch to last buffer"
      "l" #'evil-switch-to-windows-last-buffer)

(map! :when (modulep! :ui workspaces)
      :leader
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

(map! :when (modulep! :ui workspaces)
      :map vterm-mode-map
      :e
      "<escape>" #'+workspace/other)

(map! :when (modulep! :ui workspaces)
      :leader
      :desc "Display tab bar"
      "TAB f" #'+workspace/display)

(map! :when (and (modulep! :ui workspaces) (modulep! :completion ivy))
      :leader
      :desc "Switch to workspace"
      "TAB TAB" (cmd!
                 (ivy-read "Switch to workspace: "
                           (+workspace-list-names)
                           :action #'+workspace-switch
                           :caller #'+workspace-switch)
                 (+workspace-list-names)))

(map! :when (modulep! :ui workspaces)
      :leader
      :desc "Switch to last workspace"
      "TAB l" #'+workspace/other)

(map! :when (modulep! :ui workspaces)
      :leader
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

(map! :when (modulep! :tools debugger)
      :desc "Start debugger"
      :leader
      "o D" #'debugger/start)

(map! :when (modulep! :tools docker)
      :leader
      :desc "Docker"
      "o d" #'docker)

(map! :when (modulep! :config default)
      :leader
      "c x"
      (cmd! (+default/diagnostics)
            (switch-to-buffer-other-window "*Flycheck errors*")))

(map! :when (modulep! :checkers syntax)
      :leader
      :desc "global flycheck mode"
      "t z" #'global-flycheck-mode
      :desc "toggle modeline"
      "t m" #'hide-mode-line-mode
      :desc "global hide toggle modeline"
      "t M" #'global-hide-mode-line-mode)

(map! :when (modulep! :completion ivy)
      :leader
      :desc "compile"
      "c C" #'+ivy/compile)

(map! :leader
      :desc "recompile"
      "c c" #'recompile)

;; (map! :n "RET" (cmd!
;;                 (if ))

(map! :when (modulep! :tools make)
      :leader
      :desc "+make/run"
      "c r" #'+make/run
      :desc "+make/run-last"
      "c l" #'+make/run-last)

(map! :when (modulep! :lang python)
      :map python-mode-map
      :localleader
      (:prefix ("c" . "coverage")
       :desc "toggle coverage overlay"
       "c" #'python-coverage-overlay-mode
       :desc "refresh coverage overlay"
       "r" #'python-coverage-overlay-refresh))

(map! :map org-mode-map
      :localleader
      "a" #'org-show-all)

(map! :leader "w o" #'delete-other-windows)

(map! :map compilation-mode-map
      :n "q" #'quit-window)

(map! :leader
      :desc "global visual line mode"
      "t w" #'global-visual-line-mode)

(map! :leader
      :desc "tab-bar-mode"
      "t t" #'tab-bar-mode)

(map! :leader
      :desc "transparency"
      "t T" (cmd! (let ((alpha (frame-parameter nil 'alpha)))
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

(set-popup-rule! "*helpful function:" :height 100)
(set-popup-rule! "*helpful macro:" :height 100)
(set-popup-rule! "*helpful command:" :height 25 :side 'bottom)
(set-popup-rule! "*helpful variable:" :height 25 :side 'bottom)
(set-popup-rule! "*Ilist*" :side 'right :width 50 :select t)
(set-popup-rule! "*ert*" :side 'right :width 60 :select t)
(set-popup-rule! "*compilation*" :select nil :width .5 :side 'right)
(set-popup-rule! "*Anaconda*" :height 25)
(set-popup-rule! "*pytest*" :height .25 :select t)

(when (modulep! :ui doom-dashboard)
  (setq +doom-dashboard-functions
        '(doom-dashboard-widget-banner
          doom-dashboard-widget-shortmenu
          doom-dashboard-widget-loaded)))

(add-hook! 'dired-mode-hook #'dired-hide-details-mode)
(add-hook! emacs-lisp-mode
  (when (string= (buffer-name) "functions.el")
    (add-hook 'evil-insert-state-exit-hook #'eval-buffer nil t))
  (when (string= (buffer-name) "*doom:scratch*")
    (add-hook 'evil-insert-state-exit-hook #'eval-buffer nil t))
  (when (string= (buffer-name) "*scratch*")
    (add-hook 'evil-insert-state-exit-hook #'eval-buffer nil t)))


(global-visual-line-mode t)

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

(setq initial-frame-alist '((top . 1) (left . 1) (width . 160) (height . 55)))

(setq-default
 fill-column 88
 git-commit-summary-max-length 100)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("django.config\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '(".clang-format" . conf-mode))

;; (setq native-comp-async-report-warnings-errors nil)
