
(defun ++workspace/switch-to-by-name (name)
  (let ((index (cl-position name (+workspace-list-names) :test 'string=)))
    (+workspace/switch-to index)))


(defun ++workspace/workspace-p (name)
  (not (null (-contains? (+workspace-list-names) name))))

;; (++workspace/workspace-p "vterm")

;; (++workspace/switch-to-by-name "vterm")
;; (-elem-index "vterm" (+workspace-list-names))
;; (-elem-index "main" (+workspace-list-names))


(defun ++doom/enabled-minor-modes ()
  (interactive)
  (ivy-read "Enabled minor modes: "
            (cl-remove-if-not 'identity minor-mode-list)
            :action (lambda (x)
                      (apply (intern x) '(-1)))
            :caller '++browse/enabled-minor-modes))


(defun ++doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("                                   E M A C S                                   "
            "                                                                             "
            "                                                                             "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))
