;; $ emacs -Q -l ~/.emacs-vanilla.el

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(setq viper-mode t)
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level 3)
(require 'viper)

(define-key viper-vi-global-user-map (kbd "SPC SPC") 'execute-extended-command)
(define-key viper-vi-global-user-map (kbd "SPC :") 'eval-expression)
