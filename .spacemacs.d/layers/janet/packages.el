;; loosely based on the crystal layer (~/.emacs.d/layers/+lang/crystal)

(defconst janet-packages
  '(flycheck
    janet-mode
    (flycheck-janet
     :requires flycheck
     :location (recipe :fetcher gitlab :repo "sogaiu/flycheck-janet"))
    (inf-janet
     :location (recipe :fetcher github :repo "velkyel/inf-janet"))))

(defun janet/init-janet-mode ()
  (use-package janet-mode
    :defer t
    :init
    (spacemacs/declare-prefix-for-mode 'janet-mode "m=" "format")
    (spacemacs/set-leader-keys-for-major-mode 'janet-mode
      "==" #'spacemacs/janet-format-format-buffer)
    ;; TODO: enable for .jdn files
    ))

(defun janet/init-flycheck-janet ()
  (use-package flycheck-janet))

(defun janet/init-inf-janet ()
  (use-package inf-janet
    :defer t
    :init
    (spacemacs/register-repl 'inf-janet 'inf-janet "inf-janet")
    (spacemacs/declare-prefix-for-mode 'janet-mode "ms" "repl")
    (spacemacs/set-leader-keys-for-major-mode 'janet-mode
      "si" #'inf-janet
      "ss" #'inf-janet-switch-to-repl
      "sb" #'inf-janet-eval-buffer
      "se" #'inf-janet-eval-form-and-next
      "sf" #'inf-janet-eval-defun
      "sF" #'inf-janet-eval-defun-and-go
      "sr" #'inf-janet-eval-region
      "sR" #'inf-janet-eval-region-and-go)))

(defun janet/post-init-flycheck ()
  (spacemacs/enable-flycheck 'janet-mode))

(defun janet/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (sp-local-pair 'janet-mode "'" nil :actions nil)
    (sp-local-pair 'inf-janet-mode "'" nil :actions nil)))
