(defun spacemacs/janet-format-format-buffer ()
  (interactive)
  ;; adapted from nix-format.el
  (let ((jfmt-bin (executable-find janet-format-command))
        (code-buffer (current-buffer))
        (temp-buffer (get-buffer-create "*jfmt*")))
    (with-current-buffer temp-buffer
      (erase-buffer)
      (insert-buffer-substring code-buffer)
      (if (zerop (call-process-region (point-min) (point-max) jfmt-bin t t nil))
          (with-current-buffer code-buffer
            (replace-buffer-contents temp-buffer))
        (error "Command jfmt failed, see *jfmt* buffer for details")))))
