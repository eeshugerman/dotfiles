(defun spacemacs/janet-format-format-buffer ()
  (interactive)
  ;; adapted from nix-format.el
  (let ((janet-format-path (executable-find janet-format-command))
        (code-buffer (current-buffer))
        (temp-buffer (get-buffer-create "*janet-format*")))
    (with-current-buffer temp-buffer
      (erase-buffer)
      (insert-buffer-substring code-buffer)
      (let ((janet-format-config-path
             (f-join (projectile-acquire-root) ".janet-format.jdn")))
        (if (zerop (call-process-region
                    (point-min)
                    (point-max)
                    janet-format-path
                    t
                    t
                    nil
                    ;; TODO: only include if the file exists
                    ;;   or maybe bind default-directory
                    "-c" janet-format-config-path
                    ))
            (with-current-buffer code-buffer
              (replace-buffer-contents temp-buffer))
          (error "Command janet-format failed, see *janet-format* buffer for details"))))))
