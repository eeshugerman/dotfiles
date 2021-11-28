;;; packages.el --- tree-sitter layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Elliott Shugerman <eeshugerman@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defconst tree-sitter-packages
  '(
    tree-sitter
    tree-sitter-langs
    (tree-sitter-indent
     :toggle tree-sitter-indent-enable)
    ;; (tree-sitter-fold
    ;;  :toggle tree-sitter-fold-enable
    ;;  :location (recipe
    ;;             :fetcher github
    ;;             :repo "junyi-hou/tree-sitter-fold"))
    (ts-fold
     :toggle tree-sitter-fold-enable
     :location (recipe
                :fetcher github
                :repo "jcs090218/ts-fold"))
    ))

(defun tree-sitter/init-tree-sitter ()
  (use-package tree-sitter
    :defer t
    :init
    (progn
      (global-tree-sitter-mode)
      (when tree-sitter-hl-enable
        (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))))

(defun tree-sitter/init-tree-sitter-langs ()
  (use-package tree-sitter-langs
    :defer t))

(defun tree-sitter/init-tree-sitter-indent ()
  (use-package tree-sitter-indent
    :if tree-sitter-indent-enable
    :defer t
    :init
    (progn
      (require 'tree-sitter-indent) ;; missing autoload
      (tree-sitter-require 'rust)
      (add-hook 'rust-mode-hook #'tree-sitter-indent-mode))))

;; (defun tree-sitter/init-tree-sitter-fold ()
  ;; TODO: This has only been tested with `dotspacemacs-editing-style' 'vim' and
  ;;       `dotspacemacs-folding-method' 'origami' and . Other combinations
  ;;       integrated and tested. On the other hand, should 'tree-sitter' should
  ;;       become a new `dotspacemacs-folding-method' of it's own? Once it's
  ;;       more battle-tested?

  ;; (use-package tree-sitter-fold
  ;;   :if tree-sitter-fold-enable
  ;;   :defer t
  ;;   :init
  ;;   (progn
  ;;     (require 'tree-sitter-fold) ;; missing autoload
  ;;     (dolist (mode-hook '(python-mode-hook
  ;;                          go-mode-hook
  ;;                          ess-r-mode-hook
  ;;                          nix-mode))
  ;;       (when (boundp mode-hook)
  ;;         (add-hook mode-hook #'tree-sitter-fold-mode)))))
  ;; )

;; TODO: check out https://github.com/jcs090218/ts-fold
(defun tree-sitter/init-ts-fold ()
  (use-package ts-fold
    :if tree-sitter-fold-enable
    :defer t
    :init
    (progn
      (when tree-sitter-fold-indicators
        ;; don't obscure lint and breakpoint indicators
        (setq ts-fold-indicators-priority 0))

      ;; is there a way to avoid listing these explicitly?
      (dolist (mode-hook '(agda-mode
                           sh-mode
                           c-mode
                           c++-mode
                           csharp-mode
                           css-mode
                           ess-r-mode
                           go-mode
                           html-mode
                           java-mode
                           javascript-mode
                           js-mode
                           js2-mode
                           js3-mode
                           json-mode
                           jsonc-mode
                           nix-mode
                           php-mode
                           python-mode
                           rjsx-mode
                           ruby-mode
                           rust-mode
                           rustic-mode
                           scala-mode
                           swift-mode
                           typescript-mode))
        (when (boundp mode-hook)
          (add-hook mode-hook #'ts-fold-mode)
          (when tree-sitter-fold-indicators
            (add-hook mode-hook #'ts-fold-indicators-mode)))))))
