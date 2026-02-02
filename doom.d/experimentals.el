;;; -*- lexical-binding: t; -*-

(require 'treesit)


(map!
 :map tsx-ts-mode-map
 :desc "change-tag"
 :localleader
 "c" (cmd!
      (cl-flet ((fn (@open-identifier)
                  (treesit-node-eq @open-identifier (treesit-node-at (point))))
                (++treesit-node-region (node)
                  (cons (copy-marker (treesit-node-start node))
                        (copy-marker (treesit-node-end node)))))

        ;; (with-selected-window (next-window)
        (let* ((query '((jsx_element
                         open_tag: (jsx_opening_element name: (identifier) @open-identifier) 
                         close_tag: (jsx_closing_element name: (identifier) @close-identifier)
                         @jsx
                         (:pred fn @open-identifier))))
               (captures (treesit-query-capture (treesit-buffer-root-node) query))
               (open-identifier (alist-get 'open-identifier captures))
               (open-identifier-region (++treesit-node-region open-identifier))
               (close-identifier (alist-get 'close-identifier captures))
               (close-identifier-region (++treesit-node-region close-identifier))
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
              (insert new-jsx-identifier)))))))


;; open-identifier-region)))

