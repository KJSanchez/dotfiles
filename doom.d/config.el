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

;;; Code:
(load! "lib.el" nil t)
(load! "experimentals.el" nil t)

(toggle-frame-maximized)

(use-package! lsp-mode
  :defer t
  :config
  (setq lsp-signature-auto-activate nil))

(use-package! lsp-tailwindcss
  :defer t
  :after typescript-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(map! :leader
      :n
      "k" #'lsp-ui-doc-glance)

;; Something's wrong with the binary.
(use-package! parinfer
  :disabled t)

(use-package! tabspaces
  :defer t
  :config
  (map! :leader
        :n
        :desc "tab-bar-close-tab"
        "w g d" #'tab-bar-close-tab)
  (map! :leader
        :n
        :desc "tabspaces-mode"
        "t t" (cmd! (if tabspaces-mode
                        (progn
                          (tabspaces-save-session)
                          (tab-bar-mode -1)
                          (tabspaces-mode nil))
                      (tab-bar-mode)
                      (tabspaces-mode 1)
                      (tabspaces-restore-session)))))

(use-package! copilot
  :hook ((emacs-lisp-mode . copilot-mode)
         (json-mode . copilot-mode)
         (yaml-mode . copilot-mode)
         (conf-mode . copilot-mode)
         (python-mode . copilot-mode)
         (typescript-mode . copilot-mode)
         (markdown-mode . copilot-mode)
         (sh-mode . copilot-mode)
         (c++-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (setq copilot-max-char -1)
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-node-executable "node"))

(use-package! doom-modeline
  :config
  (map! :leader
        :desc "toggle modeline"
        "t m" #'hide-mode-line-mode
        :desc "global hide toggle modeline"
        "t M" #'global-hide-mode-line-mode)
  ;; (setq mode-line-modified "")
  ;; (setq vc-display-status nil)
  ;; (setq doom-modeline-vcs-max-length 0)
  ;; (setq doom-modeline-buffer-encoding nil)
  ;; (setq doom-modeline-workspace-name t)
  ;; (setq doom-modeline-time-icon nil)
  ;; (setq doom-modeline-vcs-icon nil)
  ;; (setq doom-modeline-modal nil)
  ;; (setq doom-modeline-major-mode-icon nil)
  ;; (setq doom-modeline-major-mode-color-icon nil)
  ;; (setq doom-modeline-position-column-line-format nil)
  ;; (setq doom-modeline-percent-position nil)
  )


(use-package! centaur-tabs
  :disabled t
  :config
  (centaur-tabs-group-by-projectile-project))

;; (setq centaur-tabs-set-icons t)
;; (setq centaur-tabs-gray-out-icons 'buffer)
;; (setq centaur-tabs-set-bar 'left)
;; (setq centaur-tabs-set-modified-marker nil)
;; ;; (setq centaur-tabs-close-button "")
;; ;; (setq centaur-tabs-modified-marker "x")
;; (setq centaur-tabs-style "bar")
;; (setq centaur-tabs-height 16)
;; ;; Scrolling (with the mouse wheel) past the end of the tab list
;; ;; replaces the tab list with that of another Doom workspace. This
;; ;; prevents that.
;; (setq centaur-tabs-cycle-scope 'tabs)
;; (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
;; (add-hook 'vterm-mode-hook 'centaur-tabs-local-mode)
;; (add-hook 'python-mode-hook 'centaur-tabs-local-mode)
;; (add-hook 'compilation-mode-hook 'centaur-tabs-local-mode)
;; (add-hook 'inferior-python-mode-hook 'centaur-tabs-local-mode))


(use-package! vterm
  :defer t
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)

  (map! :map vterm-mode-map
        :e
        "M-<right>" (cmd! (vterm-send-escape) (vterm-send-key "f"))
        "M-<left>" (cmd! (vterm-send-escape) (vterm-send-key "b")))

  (map! :when (modulep! :ui workspaces)
        :leader
        "o t" (cmd!
               (if (++workspace/workspace-p "vterm")
                   (progn
                     (++workspace/switch-to-by-name "vterm")
                     (unless (get-buffer "*vterm*")
                       (+vterm/here nil)))
                 (progn
                   (+workspace/new "vterm")
                   (+vterm/here nil))))))

(map! :leader
      :desc "imenu"
      "t i" #'imenu-list-minor-mode)

(use-package! evil
  :config
  (setq evil-escape-key-sequence "kj"))

;; (map! :after evil
;;       :leader
;;       :n
;;       :desc "++window-vsplit"
;;       "w v" (cmd!
;;              (split-window-right)
;;              (other-window 1)
;;              (switch-to-buffer (other-buffer))
;;              (other-window 1)))

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
      :desc "Display workspaces"
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

(map! :when (modulep! :config default)
      :leader
      "c x"
      (cmd! (+default/diagnostics)
            (switch-to-buffer-other-window "*Flycheck errors*")))

(map! :when (modulep! :checkers syntax)
      :leader
      :desc "global flycheck mode"
      "t z" #'global-flycheck-mode)

(map! :when (modulep! :completion ivy)
      :leader
      :desc "compile"
      "c C" #'+ivy/compile)

(map! :leader
      :desc "recompile"
      "c c" #'recompile)

(map! :n "RET" #'recompile)

(map! :when (modulep! :tools make)
      :leader
      :desc "+make/run"
      "c r" #'+make/run
      :desc "+make/run-last"
      "c l" #'+make/run-last)

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
      :desc "transparency"
      "t T" (cmd! (let ((alpha (frame-parameter nil 'alpha)))
                    (if (eq
                         (if (numberp alpha)
                             alpha
                           (cdr alpha)) ; may also be nil
                         100)
                        (set-frame-parameter nil 'alpha '(60 . 50))
                      (set-frame-parameter nil 'alpha '(100 . 100))))))

;; (map! :when (modulep! :ui popup)
;;       :n
;;       :map +popup-mode-map
;;       "Q" (cmd! (+popup-mode -q)))

(map! :map emacs-lisp-mode-map
      :n "RET" (cmd! (eval-buffer nil t)))

(set-popup-rule! "*helpful function:" :height 100)
(set-popup-rule! "*helpful macro:" :height 100)
(set-popup-rule! "*helpful command:" :height 25 :side 'bottom)
(set-popup-rule! "*helpful variable:" :height 25 :side 'bottom)
(set-popup-rule! "*Ilist*" :side 'right :width 50 :select t)
(set-popup-rule! "*ert*" :side 'right :width .5)
(set-popup-rule! "*compilation*" :select nil :width .5 :side 'right)
(set-popup-rule! "*cargo-test*" :select nil :width .5 :side 'right)
;; (set-popup-rule! "*compilation*" :select t :height 31 :side 'top)
(set-popup-rule! "*Anaconda*" :height 25)
(set-popup-rule! "*pytest*" :height .25 :select t)

(use-package! pixel-scroll
  ;; TODO: Kinda jumpy...
  :disabled t
  :hook (prog-mode . pixel-scroll-mode))

(use-package! doom-ui
  :config
  (setq doom-font (font-spec :family "Fira Code" :weight 'medium :size 13))
  ;; (setq doom-font (font-spec :family "iosevka" :size 15 :width 'normal))
  ;; (setq doom-font (font-spec :family "Menlo" :size 16))
  ;; (setq doom-font (font-spec :family "Monaco" :size 16))
  (setq org-directory "~/codez/obsidian")
  (setq doom-theme 'doom-spacegrey)

  ;; (add-hook! doom-big-font-mode-hook
  ;;   (set-popup-rule! "*compilation*" :select t :side 'left :width 124))
  (setq +doom-dashboard-ascii-banner-fn #'++doom-dashboard-draw-ascii-banner-fn)
  (setq +doom-dashboard-menu-sections
        '(("Browse project"
           :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
           :action projectile-switch-project)
          ("Browse .doom.d"
           :icon (nerd-icons-octicon "nf-oct-tools" :face 'doom-dashboard-menu-title)
           :when (file-directory-p doom-user-dir)
           :action doom/open-private-config)
          ("Recently opened files"
           :icon (nerd-icons-faicon "nf-fa-file_text" :face 'doom-dashboard-menu-title)
           :action recentf-open-files)
          ("Reload last session"
           :icon (nerd-icons-octicon "nf-oct-history" :face 'doom-dashboard-menu-title)
           :when (cond ((modulep! :ui workspaces)
                        (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                       ((require 'desktop nil t)
                        (file-exists-p (desktop-full-file-name))))
           :action doom/quickload-session)))
  (setq +doom-dashboard-functions
        '(doom-dashboard-widget-banner
          doom-dashboard-widget-shortmenu
          doom-dashboard-widget-loaded)))

(after! files
  (setq confirm-kill-emacs nil))

(use-package! yaml
  :defer t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package! conf-toml-mode
  :defer t
  :mode (("\\.aws/credentials\\'" . conf-toml-mode)
         ("\\.aws/config\\'" . conf-toml-mode)))

(use-package! sh-script
  :defer t
  :mode ("zshrc\\'" . sh-mode))

(use-package! cc-mode
  :defer t
  :mode (".clang-format" . conf-mode))

(use-package! flycheck
  :defer t
  :config
  (setq flycheck-disabled-checkers '(python-pylint)))

(use-package! projectile
  :defer t
  :config
  (setq projectile-project-search-path '("~/codez/" "~/codez/hello-world/" "~/ploomber/" "~/open-source/")))

(use-package! magit
  :defer t
  :config
  (setq-default git-commit-summary-max-length 100))

(use-package! neotree
  :defer t
  :config
  (setq neo-show-hidden-files nil))

(use-package! python-coverage
  :defer t
  :config
  (map! :map python-mode-map :localleader "c" nil)
  (map! :map python-mode-map
        :localleader
        :prefix ("t" . "test")
        :desc "toggle coverage overlay"
        "c" #'python-coverage-overlay-mode
        :desc "refresh coverage overlay"
        "r" #'python-coverage-overlay-refresh))

(featurep 'python)

(use-package! python
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("poetry.lock" . conf-toml-mode)))

(use-package! which-key
  :defer t
  :config
  (map! :leader
        :desc "doom/describe-active-minor-mode"
        "h m" #'doom/describe-active-minor-mode)
  (map! :leader
        :desc "doom/describe-active-minor-mode"
        "h M" #'describe-mode))

(use-package! conda
  :disabled t
  :defer t
  :config
  (map! :map python-mode-map
        :localleader
        :prefix ("c" . "conda")
        :desc "activate environment"
        "a" #'conda-env-activate
        :desc "deactivate environment"
        "d" #'conda-env-deactivate
        :desc "list environments"
        "l" #'conda-env-list))


;; sketchy
(map! :leader
      :desc "++doom/reload-dir-locals"
      "h r R" (cmd!
               (dir-locals-read-from-dir "/Users/keenansanchez/codez/cloud-backend")
               (print flycheck-disabled-checkers)
               ))

;; TODO set a smaller font in the imenu buffer.
;; Key mapping to
;; accept keystroke
;; take thing it's bound to
;; then switch to config.el in popup buffer
;; then populate a map! snippet with the keybinding
;; key switch to

(use-package! kubernetes
  :defer t
  :config nil)

;; This has been more annoying than not =(.
(use-package! persp-mode
  :defer t
  :config
  (setq +workspaces-on-switch-project-behavior t))

;; (use-package! projectile
;;   :config
;;   (defun project-tab-groups ()
;;     (interactive)
;;     (message "here"))
;;   (add-hook 'persp-frame-server-switch-hook #'project-tab-groups)
;;   ;; (remove-hook 'persp-frame-server-switch-hook #'project-tab-groups)
;;   (add-hook 'projectile-after-switch-project-hook #'project-tab-groups))

(add-hook! 'dired-mode-hook #'dired-hide-details-mode)


(defun ++search-notes ()
  (interactive)
  (let ((default-directory org-directory))
    (call-interactively
     (cond ((modulep! :completion ivy)     #'+ivy/project-search)
           ((modulep! :completion helm)    #'+helm/project-search)
           ((modulep! :completion vertico) #'+vertico/project-search)
           (#'projectile-ripgrep)))))


(map! :leader
      :n
      :desc "++search-notes"
      "n s" #'++search-notes)

(map! :leader
      :n
      :desc "Browse dotfiles"
      "f p" (cmd! (doom-project-find-file "~/codez/dotfiles")))

(map! :leader
      :n
      :desc "Switch buffer"
      ;; "," #'+ivy/switch-work-space-buffer)
      "," #'+ivy/switch-buffer)



(after! ivy
  (map!
   :map ivy-minibuffer-map
   "M-n" #'+ivy/woccur)

  (map!
   :map wgrep-mode-map
   "M-n" (cmd!
          (evil-ex (format "%%s/%s" (ivy-state-text ivy-last))))))
