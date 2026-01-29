;;; -*- lexical-binding: t; -*-

(require 'treesit)
(require 'dash)
(require 'evil)
(require 'evil-textobj-tree-sitter)

evil-textobj-tree-sitter-get-textobj
evil-inner-text-objects-map
evil-change

(setq +evil-textobj-jsx-tag
      (evil-textobj-tree-sitter-get-textobj
        "jsx-tag"
        '((jsx_element))))

(define-key evil-inner-text-objects-map "x" '+evil-textobj-jsx-tag)



(progn
  (setq query
        '((jsx_element
           open_tag: (jsx_opening_element) @start
           close_tag: (jsx_closing_element) @end)))
  (define-key evil-inner-text-objects-map "x"
              (evil-textobj-tree-sitter-get-textobj
                "jsx-element"
                query))

  )

(defun my-evil-change (orig-fn beg end &optional type register yank-handler delete-func)
  (message "evil-change called with args: %s" (list beg end type register yank-handler delete-func))
  (apply orig-fn (list beg end type register yank-handler delete-func))
  ;; evil-inner-word
  )

(advice-add 'evil-change :around my-evil-change)


(unwind-protect
    (progn
      (other-window 1)
      ;; (bounds-of-thing-at-point)
      ;; thing-at-point-functions
      ;; thing-at-point-provider-alist
      ;; evil-define-operator
      

      (defun fn (opening-tag-name)
        (treesit-node-eq opening-tag-name (treesit-node-at (point)))
        )
      
      (let* (
             (query '((jsx_element
                       open_tag: (jsx_opening_element name: (identifier) @opening-tag-name)
                       close_tag: (jsx_closing_element name: (identifier) @closing-tag-name)
                       (:pred fn @opening-tag-name)
                       )))
             (_ (treesit-query-validate 'tsx query))
             ;; (captures '()))
             (captures (treesit-query-capture (treesit-buffer-root-node) query)))
        (cl-loop for (key . node) in captures
                 do (message "%s: '%s'" key (treesit-node-text node))
                 when nil
                 return (treesit-node-text node))
        )
      )
  (other-window -1))

