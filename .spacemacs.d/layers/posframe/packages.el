(defconst posframe-packages
  '((ivy-posframe
     :toggle posframe-ivy-enable)
    (transient-posframe
     :toggle posframe-transient-enable)))


(defun posframe/init-ivy-posframe ()
  (use-package ivy-posframe
    :if posframe-ivy-enable
    ;; :defer t
    :config (ivy-posframe-mode 1)))

(defun posframe/init-transient-posframe ()
  :if posframe-transient-enable
  :defer t
  :config (transient-posframe-mode 1))
