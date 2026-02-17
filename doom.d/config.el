;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setopt x y))
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


(global-visual-line-mode 1)
(setq initial-major-mode 'emacs-lisp-mode)
(set-frame-parameter nil 'fullscreen 'maximized)
(load! "lib.el" doom-user-dir t)

(set-popup-rule! '(major-mode . helpful-mode) :height .5 :select t)
(set-popup-rule! '(major-mode . help-mode) :height .5 :select t)
(set-popup-rule! '(major-mode . imenu-list-major-mode) :side 'right :width 50 :select t)
(set-popup-rule! '(major-mode . compilation-mode) :select nil :width .5 :side 'right)

(map!
 :leader
 :desc "delete frame"
 "q f" #'delete-frame)

(map!
 :leader
 :n
 :desc "search notes"
 "n s" (cmd!
        (let ((default-directory org-directory))
          (call-interactively #'+ivy/project-search))))

(map!
 :leader
 :n
 :desc "Browse .local"
 "s E" (cmd!
        (let
            ((default-directory (f-join doom-emacs-dir ".local/straight/repos")))
          (call-interactively #'+ivy/project-search-from-cwd)))
 :desc "Browse dotfiles"
 "f p" (cmd! (doom-project-find-file "~/codez/dotfiles")))

(map!
 :after ivy
 :map ivy-minibuffer-map
 "M-n" #'+ivy/woccur
 "," #'ivy-next-line
 "<" #'ivy-previous-line)


(use-package! direnv
  :config
  (defun direnv-allow--onsave ()
    (add-hook! 'after-save-hook :local #'direnv-allow))
  (add-hook! 'direnv-mode-hook #'direnv-allow--onsave))

(use-package! treesit-auto
  :custom
  (treesit-auto-langs '(bash c cmake cpp css dockerfile html javascript json make markdown python rust sql toml tsx typescript yaml))
  (treesit-auto-install t))

(use-package! lsp-mode
  :defer t
  :custom
  (lsp-signature-auto-activate nil)
  :config
  (map! :leader
        :n
        "k" #'lsp-ui-doc-glance))

(use-package! lsp-tailwindcss
  :defer t
  :custom
  (lsp-eldoc-enable-hover nil)
  (lsp-tailwindcss-add-on-mode t))

(use-package! eglot
  :config
  (map! :leader
        :n
        "k" #'eldoc-box-help-at-point)

  (setf
   (alist-get '(tsx-ts-mode typescript-ts-mode) eglot-server-programs nil nil 'equal)
   '("rass"
     "--"
     "typescript-language-server" "--stdio"
     "--"
     "eslint-lsp" "--stdio"
     "--"
     "tailwindcss-language-server" "--stdio")))


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
  :custom
  (copilot-max-char -1)
  (copilot-indent-offset-warning-disable t)
  (copilot-node-executable "node"))

(use-package! doom-modeline
  :custom
  (doom-modeline-display-default-persp-name t)
  :config
  (doom-modeline-def-modeline 'main
    '(bar buffer-info)
    '(misc-info minor-modes check major-mode))
  (map! :leader
        :desc "toggle modeline"
        "t m" #'hide-mode-line-mode
        :desc "global hide toggle modeline"
        "t M" #'global-hide-mode-line-mode))

(use-package! centaur-tabs
  :disabled t
  :config
  (centaur-tabs-group-by-projectile-project))


(use-package! org
  :custom
  (org-startup-folded nil)
  (org-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents")
  :config
  (map!
   :map org-mode-map
   :localleader
   "a" #'org-fold-show-all)
  (add-hook 'org-mode-hook
            (cmd!
             (when (s-matches? "README.org" (buffer-name))
               (org-fold-show-all)))))

(use-package! evil
  :custom
  (evil-escape-key-sequence "kj"))

;; (map! :after evil
;;       :leader
;;       :n
;;       :desc "++window-vsplit"
;;       "w v" (cmd!
;;              (split-window-right)
;;              (other-window 1)
;;              (switch-to-buffer (other-buffer))
;;              (other-window 1)))

(map!
 :when (modulep! :editor evil)
 :leader
 :desc "Switch to last buffer"
 "l" #'evil-switch-to-windows-last-buffer)

(map! :leader
      :desc "imenu"
      "t i" #'imenu-list-minor-mode)

(map!
 :when (and (modulep! :ui workspaces)
            (modulep! :completion ivy))
 :leader
 :desc "Switch to workspace"
 "TAB TAB" (cmd!
            (ivy-read "Switch to workspace: "
                      (+workspace-list-names)
                      :action #'+workspace-switch
                      :caller #'+workspace-switch)
            (+workspace-list-names)))

(when (modulep! :config default)
  (map!
   :leader
   "f y" #'+default/yank-buffer-path-relative-to-project
   "f Y" #'+default/yank-buffer-path))

(map!
 :leader
 "c x"
 (cmd! (+default/diagnostics)
       (switch-to-buffer-other-window "*Flycheck errors*")))

(map!
 :when (modulep! :checkers syntax)
 :leader
 :desc "global flycheck mode"
 "t z" #'global-flycheck-mode)

(map!
 :when (modulep! :completion ivy)
 :leader
 :desc "compile"
 "c C" #'+ivy/compile)

(map! :leader
      :desc "recompile"
      "c c" #'recompile)

(map!
 :when (modulep! :tools make)
 :leader
 :desc "+make/run"
 "c r" #'+make/run
 :desc "+make/run-last"
 "c l" #'+make/run-last)

(map! :leader "w o" #'delete-other-windows)

(map!
 :map compilation-mode-map
 :n "q" #'quit-window)

(map! :leader
      :desc "global visual line mode"
      "t w" #'global-visual-line-mode)

(map! :leader
      :desc "transparency"
      "t t" (cmd!
             (let ((alpha (frame-parameter nil 'alpha)))
               (if (eq
                    (if (numberp alpha)
                        alpha
                      (cdr alpha))      ; may also be nil
                    100)
                   (set-frame-parameter nil 'alpha '(60 . 50))
                 (set-frame-parameter nil 'alpha '(100 . 100))))))

;; (map! :when (modulep! :ui popup)
;;       :n
;;       :map +popup-mode-map
;;       "Q" (cmd! (+popup-mode -q)))


(use-package! elisp-mode
  :config
  (map!
   :map emacs-lisp-mode-map
   :n "TAB" (cmd! (indent-rigidly (region-beginning) (region-end) 2))
   :n "RET" (cmd! (eval-defun nil))))


(use-package! typescript-ts-mode
  :defer t
  :config
  (map!
   :map tsx-ts-mode-map
   :desc "wrap in tag"
   :localleader
   "w" (cmd! 
        (defun ++treesit-node-region (node)
          (cons (copy-marker (treesit-node-start node))
                (copy-marker (treesit-node-end node))))

        (let ((node (treesit-node-at (point) 'tsx)))

          (while (not (member (treesit-node-type node)
                              '("jsx_self_closing_element" "jsx_element")))
            (setq node (treesit-node-parent node)))

          (let* ((jsx-region (++treesit-node-region node))
                 (new-jsx-identifier (read-from-minibuffer "New JSX tag: ")))
            (cl-destructuring-bind (start . end) jsx-region
              (save-excursion
                (goto-char start)
                (insert "<" new-jsx-identifier ">")
                (goto-char end)
                (insert "</" new-jsx-identifier ">")))))))

  (map!
   :map tsx-ts-mode-map
   :desc "delete tag"
   :localleader
   "d" (cmd! 
        (defun ++treesit-node-region (node)
          (cons (copy-marker (treesit-node-start node))
                (copy-marker (treesit-node-end node))))

        (let ((node (treesit-node-at (point) 'tsx)))
          (while (not (member (treesit-node-type node)
                              '("jsx_self_closing_element" "jsx_element")))
            (setq node (treesit-node-parent node)))

          (let* ((jsx-region (++treesit-node-region node)))
            (cl-destructuring-bind (start . end) jsx-region
              (save-excursion
                (delete-region start end)))))))

  ;; TODO: this should handle fragments.
  (map!
   :map tsx-ts-mode-map
   :desc "change tag"
   :localleader
   "c" (cmd!
        (defun ++treesit-node-region (node)
          (cons (copy-marker (treesit-node-start node))
                (copy-marker (treesit-node-end node))))

        (let ((node (treesit-node-at (point) 'tsx)))
          (while (not (member (treesit-node-type node)
                              '("jsx_self_closing_element" "jsx_element")))
            (setq node (treesit-node-parent node)))

          (pcase (treesit-node-type node)
            ("jsx_self_closing_element"
             (let* ((open-identifier-region (-> node
                                                (treesit-node-child-by-field-name "name")
                                                (++treesit-node-region)))
                    (new-jsx-identifier (read-from-minibuffer "New JSX tag: ")))

               (cl-destructuring-bind (start . end) open-identifier-region
                 (save-excursion
                   (delete-region start end)
                   (goto-char start)
                   (insert new-jsx-identifier)))

               (call-interactively #'+format/buffer)))

            ("jsx_element"
             (let* ((open-identifier-region (-> node
                                                (treesit-node-child-by-field-name "open_tag")
                                                (treesit-node-child-by-field-name "name")
                                                (++treesit-node-region)))
                    (close-identifier-region (-> node
                                                 (treesit-node-child-by-field-name "close_tag")
                                                 (treesit-node-child-by-field-name "name")
                                                 (++treesit-node-region)))
                    (new-jsx-identifier (read-from-minibuffer "New JSX tag: ")))

               (cl-destructuring-bind (start . end) open-identifier-region
                 (save-excursion
                   (delete-region start end)
                   (goto-char start)
                   (insert new-jsx-identifier)))
               
               (cl-destructuring-bind (start . end) close-identifier-region
                 (save-excursion
                   (delete-region start end)
                   (goto-char start)
                   (insert new-jsx-identifier)))

               (call-interactively #'+format/buffer))))))))


(use-package! pixel-scroll
  :custom
  ;; TODO: up scroll sensitivity. This *kinda* works?
  (pixel-scroll-precision-use-momentum t))
;; (pixel-scroll-precision-interpolate-page nil)
;; (pixel-scroll-precision-large-scroll-height nil)
;; (pixel-scroll-precision-interpolation-factor 2.0)
;; (pixel-scroll-precision-interpolate-page t)
;; (pixel-scroll-precision-large-scroll-height 27.0)
;; (pixel-scroll-precision-interpolation-factor 10.0)
;; :hook (prog-mode . pixel-scroll-precision-mode))


(use-package! doom-ui
  :custom
  (doom-font (font-spec :family "Fira Code" :weight 'medium :size 13))
  ;; (doom-font (font-spec :family "iosevka" :size 15 :width 'normal))
  ;; (doom-font (font-spec :family "Menlo" :size 16))
  ;; (doom-font (font-spec :family "Monaco" :size 16))
  (doom-theme 'doom-spacegrey)
  ;; (add-hook! doom-big-font-mode-hook
  ;;   (set-popup-rule! "*compilation*" :select t :side 'left :width 124))
  (+doom-dashboard-ascii-banner-fn
   #'++doom-dashboard-draw-ascii-banner-fn)
  (+doom-dashboard-menu-sections
   '(("Browse project"
      :icon (nerd-icons-octicon "nf-oct-briefcase" :face
                                'doom-dashboard-menu-title)
      :action projectile-switch-project)
     ("Browse .doom.d"
      :icon (nerd-icons-octicon "nf-oct-tools" :face
                                'doom-dashboard-menu-title)
      :when (file-directory-p doom-user-dir)
      :action doom/open-private-config)
     ("Recently opened files"
      :icon (nerd-icons-faicon "nf-fa-file_text" :face
                               'doom-dashboard-menu-title)
      :action recentf-open-files)
     ("Reload last session"
      :icon (nerd-icons-octicon "nf-oct-history" :face
                                'doom-dashboard-menu-title)
      :when
      (cond ((modulep! :ui workspaces))
            (file-exists-p (expand-file-name persp-auto-save-fname
                                             persp-save-dir))
            ((require 'desktop nil t)
             (file-exists-p (desktop-full-file-name))))
      :action doom/quickload-session)))
  (+doom-dashboard-functions
   '(doom-dashboard-widget-banner
     doom-dashboard-widget-shortmenu
     doom-dashboard-widget-loaded)))

(after! files
  (setopt confirm-kill-emacs nil))

(use-package! jinja2-mode
  :defer t
  :mode ("\\.html" . jinja2-mode))

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
  :custom
  (flycheck-disabled-checkers '(python-pylint)))

(use-package! projectile
  :defer t
  :custom
  (projectile-project-search-path '("~/codez/" "~/open-source/")))

(use-package! magit
  :defer t
  :custom
  (git-commit-summary-max-length 100))

(use-package! neotree
  :defer t
  :custom
  (neo-show-hidden-files nil))

(use-package! python-coverage
  :defer t
  :config
  (map!
   :map python-mode-map
   :localleader "c" nil)
  (map!
   :map python-mode-map
   :localleader
   :prefix ("t" . "test")
   :desc "toggle coverage overlay"
   "c" #'python-coverage-overlay-mode
   :desc "refresh coverage overlay"
   "r" #'python-coverage-overlay-refresh))

(use-package! python
  :defer t
  :mode ("poetry.lock" . conf-toml-mode))

(when (modulep! :lang javascript)
  (add-to-list 'auto-mode-alist '(".env.local" . shell-script-mode)))

(use-package! which-key
  :defer t
  :config
  (map!
   :leader
   :desc "doom/describe-active-minor-mode"
   "h m" #'doom/describe-active-minor-mode)
  (map!
   :leader
   :desc "doom/describe-active-minor-mode"
   "h M" #'describe-mode))

(use-package! conda
  :disabled t
  :defer t
  :config
  (map!
   :map python-mode-map
   :localleader
   :prefix ("c" . "conda")
   :desc "activate environment"
   "a" #'conda-env-activate
   :desc "deactivate environment"
   "d" #'conda-env-deactivate
   :desc "list environments"
   "l" #'conda-env-list))

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

(use-package! rainbow-mode
  :hook (typescript-ts-mode . rainbow-mode))

;; Pretty sure this did nothing to begin with.
;; (use-package! typescript-mode
;;   :mode ("tailwind.config.ts" . typescript-mode))

(use-package! typescript-ts-mode
  :hook
  (ts-tsx-mode . (lambda ()
                   (setq-local evil-shift-width 2))))

;; This has been more annoying than not =(.
(use-package! persp-mode
  :defer t
  :custom
  (+workspaces-on-switch-project-behavior t)
  :config
  (map!
   :when (modulep! :term vterm)
   ;; :when (modulep! :ui workspaces)
   :map vterm-mode-map
   :e
   "<escape>" #'+workspace/other)
  (map!
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
  (map!
   :leader
   :desc "Display workspaces"
   "TAB f" #'+workspace/display)
  (map!
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
  (map!
   :leader
   :desc "Switch to last workspace"
   "TAB l" #'+workspace/other))

;; (use-package! projectile
;;   :config
;;   (defun project-tab-groups ()
;;     (interactive)
;;     (message "here"))
;;   (add-hook 'persp-frame-server-switch-hook #'project-tab-groups)
;;   ;; (remove-hook 'persp-frame-server-switch-hook #'project-tab-groups)
;;   (add-hook 'projectile-after-switch-project-hook #'project-tab-groups))

(add-hook! 'dired-mode-hook #'dired-hide-details-mode)

(map!
 :leader
 :n
 "p l" (cmd!
        (find-file
         (completing-read
          "Find file: "
          (append
           (directory-files-recursively
            "~/codez/dp-warranty-backend/apps/local"
            ".*"
            t))))))

(map!
 :map wgrep-mode-map
 "M-n" (cmd!
        (evil-ex (format "%%s/%s" (ivy-state-text ivy-last)))))

(map!
 :leader
 "t F" #'toggle-frame-maximized)
;; TODO: add stuff for flycheck.
;; TODO: figure out how to have project-wide *problems* buffer, akin to vscode.
;; Create a work flow where I can have a dedicated frame for problems.
;; Have SPC-c-x switch the frame with the *problems* buffer if it exists,
;; otherwise use the standard popup.
;;
;; (after! flycheck
;;   (map! :mode flycheck-error-list-mode-map))

;; TODO: need a way to show all active workspaces. Maybe using tab-bar, centaur,
;; or in the minibuffer.
;;
;; Relevant functions
;; (+workspace/display)
