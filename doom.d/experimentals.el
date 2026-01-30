;;; -*- lexical-binding: t; -*-

(require 'treesit)
(require 'dash)
(require 'evil)
(require 'evil-textobj-tree-sitter)
(require 'typescript-ts-mode)

treesit-explore-mode

(treesit-language-available-p 'tsx)
;; working!
(progn
  (setq query '((jsx_element) @jsx-tag))
  (setq query '((jsx_element
                 open_tag: (jsx_opening_element name: (identifier) @open-identifier) 
                 close_tag: (jsx_closing_element name: (identifier) @close-identifier))))
  
  (treesit-query-validate 'tsx query)
  (define-key evil-inner-text-objects-map "x" 
              (evil-textobj-tree-sitter-get-textobj
                "close-identifier"
                `((tsx-ts-mode . ,query)))
              (setq foo 'bar)))

(evil-define-operator evil-change--jsx-tag
  (orig-fn beg end type register yank-handler delete-func)
  (interactive "<R><x><y>")
  when
  (message "evil-change called with args: %s" (list orig-fn beg end type register yank-handler delete-func))
  (if nil
      nil
    (apply orig-fn (list beg end type register yank-handler delete-func))))



(advice-add 'evil-change :around #'evil-change--jsx-tag)
(advice-remove 'evil-change  #'evil-change--jsx-tag)


(unwind-protect
    (progn
      (other-window 1)
      ;; (bounds-of-thing-at-point)
      ;; thing-at-point-functions
      ;; thing-at-point-provider-alist
      ;; evil-define-operator
      

      (defun fn (opening-tag-name)
        (treesit-node-eq opening-tag-name (treesit-node-at (point))))
      
      
      (let* (
             (query '((jsx_element
                       open_tag: (jsx_opening_element name: (identifier) @opening-tag-name)
                       close_tag: (jsx_closing_element name: (identifier) @closing-tag-name)
                       (:pred fn @opening-tag-name))))
             
             (_ (treesit-query-validate 'tsx query))
             ;; (captures '()))
             (captures (treesit-query-capture (treesit-buffer-root-node) query)))
        (cl-loop for (key . node) in captures
                 do (message "%s: '%s'" key (treesit-node-text node))
                 when nil
                 return (treesit-node-text node))))
        
      
  (other-window -1))

