;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Elliott Shugerman"
      user-mail-address "eeshugerman@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :leader
      :desc "M-x"
      "SPC" #'counsel-M-x)

(map! :leader
      :desc "Eval expression"
      ":" #'eval-expression)


;; buffer
(map! :leader
      :desc "Go to last buffer"
      "TAB" #'evil-switch-to-windows-last-buffer)

(defun my/switch-to-messages-buffer ()
  (interactive)
  (switch-to-buffer (messages-buffer)))

(map! :leader
      :desc "Go to *Message*"
      "b m" #'my/switch-to-messages-buffer)

;; window
(map! :leader
      :desc "Vertical split"
      "w /" #'+evil-window-vsplit-a)

(map! :leader
      :desc "Horizontal split"
      "w -" #'+evil-window-split-a)



;; help
(map! :leader
      :desc "Describe function"
      "h d f" #'describe-function)

(map! :leader
      :desc "Describe variable"
      "h d v" #'describe-variable)

(map! :leader
      :desc "Describe key"
      "h d k" #'describe-key)


;; evil

(map! :leader
      :desc "Clear evil-ex search"
      "s c" #'evil-ex-search-abort) ;; not working

(after! evil
  (setq evil-want-minibuffer t
        evil-want-fine-undo t
        evil-want-Y-yank-to-eol t)

  ;; todo: use map!
  (evil-define-key 'visual 'global (kbd "v") 'evil-visual-line)
  (evil-define-key 'motion 'global (kbd "V") (kbd "C-v $"))

  (evil-define-key 'normal 'global (kbd "C-,") #'evil-emacs-state)
  (evil-define-key 'insert 'global (kbd "C-,") #'evil-emacs-state)
  (evil-define-key 'emacs  'global (kbd "C-,") #'evil-normal-state)

  )


(after! vi-tilde-fringe
  (vi-tilde-fringe-mode -1))

(after! flycheck
  (setq flycheck-checker-error-threshold 500))

(after! tree-sitter
  (global-tree-sitter-mode))


(after! symex
  (evil-define-key 'normal symex-mode-map
    (kbd "<escape>") 'symex-mode-interface)

  (evil-define-key 'insert symex-mode-map
    (kbd "<escape>") 'symex-mode-interface)

  (setq symex--user-evil-keyspec
        '(("j" . symex-go-up)
          ("k" . symex-go-down)
          ("C-j" . symex-climb-branch)
          ("C-k" . symex-descend-branch)
          ("M-j" . symex-goto-highest)
          ("M-k" . symex-goto-lowest)))
  (symex-initialize))
