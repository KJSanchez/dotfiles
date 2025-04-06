;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       corfu
       ;; vertico
       (ivy +fuzzy)
       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       modeline
       neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +all)   ; tame sudden yet inevitable temporary windows
       ;; tabs              ; a tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       ;; parinfer          ; turn lisp into python, sort of
       ;; snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired
       electric          ; smarter, keyword-based electric-indent
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;; eshell
       vterm

       :checkers
       (syntax
        ;; +flycheck      ; TODO: try?
        +childframe)

       :tools
       tree-sitter
       (debugger +lsp)
       direnv           ; TODO https://github.com/doomemacs/doomemacs/issues/1666#issuecomment-853629887
       docker
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       lookup              ; navigate your code and its documentation
       lsp
       magit
       make              ; run make tasks from Emacs

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS

       :lang
       (rust +lsp)
       (cc +lsp)
       emacs-lisp
       json
       (javascript +lsp +tree-sitter)
       (markdown +grip)
       (python +pyenv +lsp +pyright)
       sh
       (yaml +lsp)

       :config
       (default +bindings +smartparens))
