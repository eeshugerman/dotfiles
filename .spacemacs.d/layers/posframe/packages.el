(defconst posframe-packages
  '((ivy-posframe
     :toggle posframe-ivy-enable)
    (which-key-posframe
     :toggle posframe-which-key-enable
     :location (recipe
                :fetcher github
                :repo "eeshugerman/which-key-posframe"
                :branch "patch-1"))
    (transient-posframe
     :toggle posframe-transient-enable)))


(defun posframe/init-ivy-posframe ()
  (use-package ivy-posframe
    :if posframe-ivy-enable
    ;; :defer t
    :config (ivy-posframe-mode 1)))

(defun posframe/init-which-key-posframe ()
  :if posframe-which-key-enable
  :defer t
  :config (which-key-posframe-mode 1))

(defun posframe/init-transient-posframe ()
  :if posframe-transient-enable
  :defer t
  :config (transient-posframe-mode 1))
