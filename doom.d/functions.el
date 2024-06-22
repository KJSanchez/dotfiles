


(defun ++workspace/switch-to-by-name (name &optional create)
  (let ((index (cl-position name (+workspace-list-names) :test 'string=)))
    (+workspace/switch-to index)))


(defun ++workspace/workspace-p (name)
  (not (null (-contains? (+workspace-list-names) name))))

;; (++workspace/workspace-p "vterm")

;; (++workspace/switch-to-by-name "vterm")
;; (-elem-index "vterm" (+workspace-list-names))
;; (-elem-index "main" (+workspace-list-names))
