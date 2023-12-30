(defun spacemacs/janet-format-format-buffer ()
  (interactive)
  ;; adapted from nix-format.el
  (let ((src-buffer (current-buffer))
        (temp-buffer (get-buffer-create "*janet-format*"))
        ;; it's import we grab these before binding current-buffer because
        ;; that will also change default-directory
        (bin-path (executable-find janet-format-command))
        (project-dir-path (projectile-project-root))
        (config-path janet-format-config-path))
    (with-current-buffer temp-buffer
      (erase-buffer)
      (insert-buffer-substring src-buffer)
      ;; it's important to bind default dir for finding config file
      ;; when `janet-format-config-path' is not set
      (projectile-with-default-dir project-dir-path
        (let* ((cpr-args (append (list
                                  (point-min)  ;; start
                                  (point-max)  ;; end
                                  bin-path     ;; program
                                  t            ;; delete
                                  t            ;; buffer (t for current)
                                  nil          ;; display
                                  )
                                 (if config-path
                                     (list "--config" config-path)
                                   '())))
               (rc (apply 'call-process-region cpr-args)))
          (if (zerop rc)
              (with-current-buffer src-buffer
                (replace-buffer-contents temp-buffer))
            (error "Command janet-format failed, see *janet-format* buffer for details"))
          )))))
