;;; -*- lexical-binding: t; -*-

(require 'treesit)

(map!
 :map tsx-ts-mode-map
 :desc "wrap in tag"
 :localleader
 "w" (cmd! (print "TODO")))


(map!
 :map tsx-ts-mode-map
 :desc "wrap in tag"
 :localleader
 "w" (cmd! 
      (defun ++treesit-node-region (node)
        (cons (copy-marker (treesit-node-start node))
              (copy-marker (treesit-node-end node))))

      (let ((node (treesit-node-at (point) 'tsx)))
        (while (not (string= (treesit-node-type node) "jsx_element"))
          (setq node (treesit-node-parent node)))

        (let* ((jsx-region (++treesit-node-region node)))
          (cl-destructuring-bind (start . end) jsx-region
            (save-excursion
              (goto-char start)
              (insert "<>")
              (goto-char end)
              (insert "</>"))
            (call-interactively #'+format/buffer))))))

(map!
 :map tsx-ts-mode-map
 :desc "delete tag"
 :localleader
 "d" (cmd! 
      (defun ++treesit-node-region (node)
        (cons (copy-marker (treesit-node-start node))
              (copy-marker (treesit-node-end node))))

      (let ((node (treesit-node-at (point) 'tsx)))
        (while (not (string= (treesit-node-type node) "jsx_element"))
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
        (while (not (string= (treesit-node-type node) "jsx_element"))
          (setq node (treesit-node-parent node)))

        (let* (
               (open-identifier-region (-> node
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

          (call-interactively #'+format/buffer)))))
