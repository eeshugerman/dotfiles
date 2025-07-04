;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; adapted from https://gist.github.com/ffevotte/9345586
;; see also: https://github.com/purcell/exec-path-from-shell
(defun source-shell-script (filename)
  "Update environment variables from a shell source file."
  (interactive "fSource file: ")

  (message "Sourcing environment from `%s'..." filename)
  (with-temp-buffer

    (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))

    ;; skip header
    (dotimes (_ 3)
      (delete-line))

    (let ((envvar-re "\\([^=]+\\)='?\\(.*\\)'?$"))
      ;; Remove environment variables
      (while (search-forward-regexp (concat "^-" envvar-re) nil t)
        (let ((var (match-string 1)))
          (message "%s" (prin1-to-string `(setenv ,var nil)))
          (setenv var nil)))

      ;; Update environment variables
      (goto-char (point-min))
      (while (search-forward-regexp (concat "^+" envvar-re) nil t)
        (let ((var (match-string 1))
              (value (match-string 2)))
          (message "%s" (prin1-to-string `(setenv ,var ,value)))
          (setenv var value)))))
  (message "Sourcing environment from `%s'... done." filename))

;; https://emacs.stackexchange.com/a/71898
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (shell-command-to-string "$SHELL --login -c 'echo -n $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))


(defconst my/macos-flag (eq system-type 'darwin))

(when my/macos-flag
  (toggle-debug-on-error)
  (source-shell-script (expand-file-name "~/.zshrc"))
  (set-exec-path-from-shell-PATH))

(defconst my/work-flag (thread-first "yadm config --get local.class"
                                     shell-command-to-string
                                     string-trim
                                     (string= "WORK")))

(defmacro my/with-no-messages (&rest body)
  ;; `inhibit-message' still logs to *Messages* and (apprently?) clears previous message
  ;; so instead...
  `(cl-letf (((symbol-function 'message) #'ignore))
     (progn ,@body)))

(defun my/suppress-messages-advice (func &rest args)
  (my/with-no-messages (apply func args)))

(defun my/suppress-messages-hook (func)
  (lambda () (my/with-no-messages (funcall func))))

(defun my/install-external-deps ()
  (interactive)
  (let* ((spacemacs-d-path (f-expand "~/.spacemacs.d"))
         (nix-path (f-join spacemacs-d-path "nix"))
         (flake-path (f-join nix-path "flake"))
         (profile-path (f-join nix-path "profile"))
         (out-buffer "*nix profile install*"))

    ;; --- install ---
    ;; avoid priority/conflict errors. is there a better way?
    (when (file-exists-p profile-path) (delete-file profile-path t))
    (async-shell-command
     (format "set -x; nix -v profile install --profile %s %s" profile-path flake-path)
     out-buffer)
    (pop-to-buffer out-buffer)
    (evil-normal-state)


    ;; --- config ---

    (let ((profile-bin-path (f-join profile-path "bin")))
      (add-to-list 'exec-path profile-bin-path)
      (setenv "PATH" (format "%s:%s" profile-bin-path (getenv "PATH"))))


    ;; eslint
    (setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))

    ;; angular
    (let* ((ng-extension-path (f-join profile-path "share/vscode/extensions/Angular.ng-template"))
           (ng-server-path (f-join ng-extension-path "server/bin/ngserver"))
           (ng-node-modules-path (f-join ng-extension-path "node_modules")))
      (setq lsp-clients-angular-language-server-command
            (list "node" ng-server-path
                  "--ngProbeLocations" ng-node-modules-path
                  "--tsProbeLocations" ng-node-modules-path
                  "--stdio")))
    ))

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   `(auto-completion
     c-c++
     csv
     dap
     debug
     docker
     emacs-lisp
     git
     go
     graphviz
     helpful
     html
     ibuffer
     ipython-notebook
     ivy
     java
     lua
     markdown
     multiple-cursors
     nav-flash
     org
     posframe
     python
     shell
     shell-scripts
     spacemacs-layouts
     spell-checking
     sql
     syntax-checking
     tree-sitter
     treemacs
     typescript
     unicode-fonts
     version-control
     yaml
     ,@(when my/work-flag
         '(groovy
           kubernetes
           terraform))
     ,@(unless my/work-flag
         '(ansible
           dhall
           epub
           haskell
           janet
           meson
           nginx
           nixos
           purescript
           rust
           scheme
           vagrant))
     ,@(when my/macos-flag
         '())
     ,@(unless my/macos-flag
         '(systemd)))

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   ;; for :location format, see https://github.com/melpa/melpa/#recipe-format
   ;; may need to delete package dir in ~/.emacs.d/elpa to replace
   `(
     apheleia ;; auto formatter
     caps-lock
     diredfl ;; dired font-lock
     eat ;; elisp terminal emulator
     envrc
     explain-pause-mode
     flycheck-posframe
     fold-this
     gcmh
     just-mode
     minimap
     mustache-mode ;; for the templating lang
     nerd-icons ;; for doom-modeline -- should be a dep?
     nix-ts-mode
     ox-pandoc ;; org pandoc exporter
     ox-reveal ;; org slidedeck exporter
     solaire-mode
     symex
     ;; mini-frame
     ;; undo-hl
     ;; coterm

     (ivy-nixos-options
      :location (recipe :fetcher github :repo "travisbhartwell/nix-emacs"))
     (dconf-dotfile
      :location (recipe :fetcher github :repo "eeshugerman/dconf-dotfile.el"))
     (explain-pause-mode
      :location (recipe :fetcher github :repo "lastquestion/explain-pause-mode"))

     ,@(when my/work-flag
         '((sql-snowflake :location "~/devel/sql-snowflake.el")
           (sql-databricks :location "~/devel/sql-databricks.el")
           sql-trino
           (prisma-mode :location (recipe :fetcher github :repo "pimeys/emacs-prisma-mode"))))
     ,@(unless my/work-flag
         '(guix
           (nushell-mode :location (recipe :fetcher github :repo "mrkkrp/nushell-mode")))))


   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(vim :variables
                                    vim-style-remap-Y-to-y$ t
                                    vim-style-visual-feedback t
                                    vim-style-visual-line-move-text t
                                    vim-style-ex-substitute-global t)

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 0

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "nerd-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent t

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable t

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light). A theme from external
   ;; package can be defined with `:package', or a theme can be defined with
   ;; `:location' to download the theme package, refer the themes section in
   ;; DOCUMENTATION.org for the full theme specifications.
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light

                         doom-nord
                         doom-nord-light

                         doom-solarized-dark
                         doom-solarized-light

                         ;; extra lights
                         doom-one-light
                         doom-opera-light
                         doom-tomorrow-day

                         ;; extra medium
                         doom-nova

                         ;; extra darks
                         doom-city-lights
                         doom-ephemeral
                         doom-material
                         doom-palenight
                         doom-one
                         doom-vibrant
                         doom-horizon
                         doom-snazzy
                         doom-spacegrey)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme 'doom

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font `(("JetBrains Mono"
                                :size ,(if my/macos-flag 12.0 10.0))
                               ("Fira Code"
                                :size ,(if my/macos-flag 12.0 10.0)))

   ;; Default icons font, it can be `all-the-icons' or `nerd-icons'.
   dotspacemacs-default-icons-font 'all-the-icons

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "M-<return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "M-<return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position '(posframe . center)

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup t ;; this breaks Rectangle stuff in OSX

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format nil

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace (not my/work-flag)

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  ;; (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (setq-default
   ;; misc -- TODO: organize these
   c-c++-lsp-enable-semantic-highlight t
   ;; c-c++-lsp-enable-semantic-highlight 'overlay
   doom-solarized-dark-brighter-modeline t
   haskell-completion-backend 'lsp
   evil-respect-visual-line-mode t

   html-enable-lsp t
   css-enable-lsp t
   scss-enable-lsp t

   dap-debug-restart-keep-session nil

   git-enable-magit-delta-plugin t
   git-enable-magit-todos-plugin nil ;; can this be configured to show todos in the diff from master only?

   ;; groovy-backend 'lsp
   ;; TODO: use nix
   groovy-lsp-jar-path "~/util/groovy-language-server/build/libs/groovy-language-server-all.jar"

   ivy-enable-icons t ;; also sets `ivy-enable-advanced-buffer-information'
   ivy-extra-directories nil
   ivy-initial-inputs-alist nil
   ivy-virtual-abbreviate 'full
   ivy-wrap t

   java-backend 'lsp

   javascript-import-tool (if my/work-flag 'import-js nil)
   javascript-repl 'nodejs

   lsp-auto-execute-action nil
   lsp-clients-typescript-max-ts-server-memory 4096
   lsp-copilot-enabled nil ;; still prompts to install server :(
   lsp-eldoc-enable-hover nil
   lsp-enable-dap-auto-configure nil ;; performance issues. we also do (dap-auto-configure-mode -1) below for good measure
   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil
   lsp-enable-symbol-highlighting t
   lsp-eslint-enable t
   lsp-eslint-warn-on-ignored-files t
   lsp-file-watch-threshold 3000
   lsp-headerline-breadcrumb-enable t
   lsp-headerline-breadcrumb-segments '(symbols)
   lsp-headerline-breadcrumb-icons-enable nil ;; icons can make the headerline height wobble when point moves
   lsp-inlay-hint-enable t
   lsp-modeline-code-actions-enable nil
   lsp-signature-render-documentation nil
   lsp-ui-doc-alignment 'window
   lsp-ui-doc-delay 0.5 ; seconds
   lsp-ui-doc-enable t ;; slow w/ large files?
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature t
   lsp-ui-doc-max-width (if my/work-flag 90 75)
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-show-with-mouse nil
   lsp-ui-doc-use-childframe t
   lsp-ui-imenu-auto-refresh 'after-save
   lsp-ui-imenu-enable nil
   lsp-ui-peek-always-show t
   lsp-ui-peek-enable t
   lsp-ui-peek-fontify 'always
   lsp-ui-peek-list-width 60
   lsp-ui-peek-show-directory t
   lsp-ui-sideline-diagnostic-max-line-length 90
   lsp-ui-sideline-diagnostic-max-lines 10
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-show-code-actions nil
   lsp-ui-sideline-show-diagnostics nil ;; use SPC e x instead
   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-show-symbol nil
   lsp-ui-sideline-update-mode 'line ;; more performant maybe?

   nix-backend 'lsp

   org-adapt-indentation t
   org-enable-jira-support t
   org-enable-verb-support t


   purescript-fmt-on-save t

   python-backend 'lsp
   python-fill-column 100
   python-fill-docstring-style 'django
   python-format-on-save nil
   python-formatter 'black
   python-poetry-activate t
   python-sort-imports-on-save nil
   python-tab-width 4

   scheme-implementations '(guile)

   shell-default-height 30
   shell-default-position 'bottom
   shell-default-shell 'shell

   spacemacs-layouts-restrict-spc-tab t
   spacemacs-layouts-restricted-functions '(ivy-switch-buffer ;; shouldn't be necessary ?
                                            spacemacs/window-split-double-columns
                                            spacemacs/window-split-triple-columns
                                            spacemacs/window-split-grid)
   persp-autokill-buffer-on-remove 'kill-weak

   spell-checking-enable-by-default nil

   tree-sitter-indent-enable nil
   tree-sitter-fold-enable t
   tree-sitter-fold-indicators-enable nil ;; would rather disable in dir-locals but have not had luck with that
   tree-sitter-hl-enable-query-region-extension t

   treemacs-sorting 'alphabetic-asc
   treemacs-use-filewatch-mode t
   treemacs-use-git-mode 'deferred
   treemacs-deferred-git-apply-delay 0.25
   treemacs-use-follow-mode nil
   ;; https://github.com/Alexander-Miller/cfrs/issues/4 is fixed but this is still buggy
   treemacs-read-string-input 'from-minibuffer

   unicode-fonts-enable-ligatures t
   unicode-fonts-less-feedback t
   unicode-fonts-ligature-modes '(typescript-mode
                                  javascript-mode
                                  js2-mode
                                  web-mode
                                  html-mode
                                  scss-mode
                                  css-mode)
   version-control-diff-tool 'diff-hl

   writeroom-maximize-window nil
   writeroom-mode-line t
   writeroom-global-effects nil)

  (setopt spacemacs-keep-legacy-current-buffer-delete-bindings nil))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (setq truncate-partial-width-windows nil) ;; respect `truncate-lines'
  (setq-default truncate-lines t)
  (spacemacs/toggle-debug-on-error-on)

  (my/install-external-deps)


  ;; temp ---------------------------------------------------------------------
  ;; fixes js org blocks -- why??
  (defface tree-sitter-hl-face:punctuation
    '((default :inherit unspecified))
    "Face for punctuations."
    :group 'tree-sitter-hl-faces)

  ;; init/configure standalone packages ----------------------------------------------------
  (use-package diredfl :config (diredfl-global-mode 1))
  (use-package gcmh
    :init (setq gcmh-verbose nil
                gcmh-low-cons-threshold (expt 10 3)
                gcmh-high-cons-threshold (expt 10 8)
                gcmh-idle-delay 15)
    :config (gcmh-mode 1))
  (use-package envrc
    :config (envrc-global-mode))
  ;; (use-package explain-pause-mode
  ;;   :config (explain-pause-mode 1))
  (use-package solaire-mode :config (solaire-global-mode 1))
  (use-package symex)

  (unless my/work-flag
    (use-package guix))

  (when my/work-flag
    (use-package sql-snowflake)
    (use-package sql-databricks)
    (use-package sql-trino
      :config (add-to-list 'sql-trino-options "--pager=cat"))
    (use-package prisma-mode))

  (unless my/work-flag
    (use-package ivy-nixos-options)
    (use-package dconf-dotfile))

  ;; misc/general --------------------------------------------------------------
  (server-start)

  (defun my/clear-shell-buffer ()
    (interactive)
    (cond ((derived-mode-p 'comint-mode) (comint-clear-buffer))
          ((eq major-mode 'eat-mode) (eat-reset))))

  (spacemacs/set-leader-keys
    ":"  'eval-expression
    "ow" 'eww
    "oc" 'my/clear-shell-buffer
    "og" 'revert-buffer
    "ou" 'my/unescape-newlines)

  (setq-default select-enable-clipboard nil
                create-lockfiles nil
                projectile-indexing-method 'hybrid
                bidi-inhibit-bpa t
                bidi-paragraph-direction 'left-to-right
                completion-ignore-case t
                diff-refine nil
                inhibit-compacting-font-caches t
                jit-lock-defer-time 0
                use-dialog-box nil
                use-file-dialog nil
                helpful-switch-buffer-function #'pop-to-buffer-same-window
                tab-always-indent t)

  (let ((custom-file-path (file-truename "~/.spacemacs.d/custom.el")))
    (unless (file-exists-p custom-file-path)
      (with-temp-buffer (write-file custom-file-path)))
    (customize-set-variable 'custom-file custom-file-path))
  (load custom-file)

  (remove-hook 'after-make-frame-functions 'persp-init-new-frame) ;; not working?
  (remove-hook 'diff-mode-hook 'whitespace-mode)

  (when my/macos-flag
    ;; can be slow, resulting in periodic pauses
    ;; maybe not just just a macos thing
    (savehist-mode -1)
    ;; seems to help with maximizing and moving between monitors
    (setq frame-resize-pixelwise t))

  (dolist (hook '(hack-local-variables-hook
                  special-mode-hook
                  shell-mode-hook))
    (add-hook hook (my/suppress-messages-hook #'spacemacs/toggle-truncate-lines-on)))

  (add-to-list 'auto-mode-alist
               ;; TODO: define custom mode extending markdown-mode with ",c" bound to save and kill
               `(,(rx "tmp_github.com_" (repeat 8 alphanumeric) ".txt" string-end) . markdown-mode))


  ;; default to t after three seconds for "open file literally?" prompt
  (advice-add 'spacemacs/check-large-file
              :around (lambda (func &rest args)
                        (cl-letf (((symbol-function 'y-or-n-p-orig) (symbol-function 'y-or-n-p))
                                  ((symbol-function 'y-or-n-p) (lambda (prompt)
                                                                 (with-timeout (3 t)
                                                                   (y-or-n-p-orig prompt)))) )
                          (apply func args))))

  ;; treesit  ----------------------------------------------------------------
  (setq treesit-language-source-alist
        '((yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.0")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0")
          (nix "https://github.com/nix-community/tree-sitter-nix")))

  (dolist (pair treesit-language-source-alist)
    (treesit-install-language-grammar (car pair)))

  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode))
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
  (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))

  ;; nix ----------------------------------------------------------------------
  ;; it appears spacemacs doesn't respect major-mode-remap-alist :(
  (add-to-list 'auto-mode-alist `(,(rx ".nix" string-end) . nix-mode))
  (add-hook 'nix-mode-hook (lambda () (lsp-mode +1)))



  ;; emacs lisp ----------------------------------------------------------------
  (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
    "gd" #'spacemacs/jump-to-definition
    "gr" #'xref-find-references)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (unless (string= buffer-file-name (f-expand "~/.spacemacs.d/init.el"))
                (flycheck-mode 1))))

  ;; autosave ------------------------------------------------------------------
  (auto-save-mode -1)
  (auto-save-visited-mode -1)
  (setq auto-save-timeout 5)

  (defun my/save-buffer-if-visiting-file (&rest _)
    (when buffer-file-name
      (save-buffer)))


  (add-hook 'window-selection-change-functions #'my/save-buffer-if-visiting-file)
  (add-hook 'focus-out-hook #'my/save-buffer-if-visiting-file)
  (let ((switch-buffer-funcs '(spacemacs/alternate-buffer ivy-switch-buffer)))
    (dolist (func switch-buffer-funcs)
      (advice-add func :before #'my/save-buffer-if-visiting-file)))

  ;; company --------------------------------------------------------------------
  (setq company-selection-wrap-around t)

  (when my/macos-flag
    ;; disable in sh-mode
    (setq company-shell-modes '(eshell-mode)))

  ;; flycheck ---------------------------------------------------------------------
  ;; TODO: move flycheck-posframe stuff to posframe layer
  (remove-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-posframe-mode)

  (setq flycheck-checker-error-threshold 3000
        flycheck-display-errors-function nil

        flycheck-pos-tip-max-width 75
        flycheck-posframe-border-use-error-face t
        flycheck-posframe-border-width 1)

  (flycheck-posframe-configure-pretty-defaults)

  ;; https://github.com/doomemacs/doomemacs/issues/6416#issuecomment-1156164346
  (progn
    (defun flycheck-posframe-monitor-post-command ()
      (when (not (flycheck-posframe-check-position))
        (posframe-hide flycheck-posframe-buffer)))

    (defun fix-flycheck-posframe-not-hide-immediately ()
      (cond (flycheck-posframe-mode
             (add-hook 'post-command-hook #'flycheck-posframe-monitor-post-command nil t))
            ((not flycheck-posframe-mode)
             (remove-hook 'post-command-hook #'flycheck-posframe-monitor-post-command t))))

    (add-hook 'flycheck-posframe-mode-hook #'fix-flycheck-posframe-not-hide-immediately))

  ;; make it easy to disable because it can get in the way of completions
  (progn
    (defun my/toggle-flycheck-posframe ()
      (interactive)
      (if flycheck-posframe-mode
          (progn
            (flycheck-posframe-mode -1)
            (setq flycheck-display-errors-function 'flycheck-display-error-messages))
        (flycheck-posframe-mode 1)))

    (spacemacs/set-leader-keys "oe" #'my/toggle-flycheck-posframe))

  ;; dired -----------------------------------------------------------------------
  (defun my/dired-up-directory ()
    (interactive)
    (find-alternate-file ".."))

  (evil-define-key 'normal dired-mode-map
    [return] 'dired-find-alternate-file
    "u" 'my/dired-up-directory)

  ;; undo --------------------------------------------------------------------
  ;; granular history ---
  (setq evil-want-fine-undo t)

  (advice-add #'undo-tree-load-history :around #'my/suppress-messages-advice)


  ;; comint --------------------------------------------------------------------

  ;; doesn't work, try advising spacemacs/kill-this-buffer instead
  (spacemacs/set-leader-keys-for-major-mode 'comint-mode
    "bd" #'comint-send-eof)

  (evil-define-key 'normal comint-mode-map
    [return] 'comint-send-input)

  (evil-define-key 'insert comint-mode-map
    (kbd "C-r") 'comint-history-isearch-backward
    (kbd "C-S-r") 'comint-history-isearch-backward-regexp)

  (evil-define-key 'normal ielm-map
    [return] 'ielm-return)

  (setq-default comint-scroll-to-bottom-on-input nil
                comint-scroll-to-bottom-on-output nil
                comint-input-ignoredups t)

  ;; spacemacs only binds this in shell-mode-map ;; TODO: upstream
  (evil-define-key 'normal comint-mode-map ",H" #'counsel-shell-history)

  ;; doesn't work because comint-input-ring-separator is only used for reading
  ;; from history files
  ;; (add-hook 'sql-interactive-mode-hook
  ;;           (lambda () (setq-local comint-input-ring-separator ";\n")))

  ;; shell ---

  ;; TODO: try adding a `shell-dynamic-complete-functions' for shell aliases

  (setq shell-pop-autocd-to-working-dir nil
        shell-completion-execonly nil)

  (add-hook 'comint-mode-hook (lambda () (hl-line-mode 1)))

  ;; TODO: upstream to shell.el (the separator part at least -- maybe not the string-replace hack)
  (defun my/fix-multiline-zsh-history-items (&rest args)
    (when (equal shell--start-prog "zsh")
      (setq-local comint-input-ring-separator (concat "\n" comint-input-ring-file-prefix))
      (comint-read-input-ring)
      (let* ((old-ring (ring-elements comint-input-ring))
             (new-ring (seq-map (lambda (item) (string-replace "\\\\\n" "\\\n" item)) old-ring)))
        (setq-local comint-input-ring (ring-convert-sequence-to-ring new-ring)))))
  ;; can't do this in shell-mode-hook because `comint-read-input-ring' runs before that
  (advice-add 'shell-mode :after #'my/fix-multiline-zsh-history-items)

  (defun my/pop-shell-at-project-root-or-home ()
    (interactive)
    (if (projectile-project-p)
        (spacemacs/projectile-shell-pop)
      (spacemacs/default-pop-shell)))
  (spacemacs/set-leader-keys "'" #'my/pop-shell-at-project-root-or-home)

  (add-hook 'shell-mode-hook (lambda () (setq-local comint-process-echoes t)))

  ;; enable colors in shell without breaking sql-interactive-mode. untested.
  ;; (add-hook 'shell-mode-hook
  ;;           (lambda () (setq-local comint-terminfo-terminal "dumb-emacs-ansi")))


  ;; TODO: try https://github.com/CeleritasCelery/emacs-native-shell-complete


  ;; transient --------------------------------------------------------------------
  (with-eval-after-load 'transient
    (define-key transient-map (kbd "<escape>") 'transient-quit-one))

  ;; xml ---------------------------------------------------------------------------
  (add-to-list 'auto-mode-alist `(,(rx ".xml" string-end) . nxml-mode))
  (add-hook 'nxml-mode-hook 'origami-mode)
  (setq nxml-child-indent 2
        nxml-attribute-indent 2)

  ;; info ---------------------------------------------------------------------------
  (evil-define-key 'motion Info-mode-map
    [return] 'Info-follow-nearest-node
    (kbd "[[") 'Info-last
    (kbd "]]") 'Info-next
    (kbd "C-m") 'Info-goto-node)
  (set-face-background 'info-isolated-quote 'unspecified)


  ;; python ------------------------------------------------------------------------
  (add-hook 'python-mode-hook #'spacemacs/toggle-fill-column-indicator-on)

  ;; ein ---
  (add-hook 'ein:notebook-mode-hook #'spacemacs/toggle-fill-column-indicator-off)
  (setq ein:output-area-inlined-images t)


  ;; interpreter and tooling ---
  (setq python-shell-interpreter "ipython3")
  (when my/macos-flag
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"
          python-shell-completion-native-enable nil))


  ;; [ma]git ----------------------------------------------------------------------
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; -- delta
  ;; slow, so leave off and toggle on as needed https://github.com/dandavison/magit-delta/issues/9#issuecomment-1610136282
  (remove-hook 'magit-mode-hook 'magit-delta-mode)

  (spacemacs/set-leader-keys "gB" 'magit-switch-to-repository-buffer)

  (defun my/toggle-magit-delta ()
    (interactive)
    (call-interactively 'magit-delta-mode)
    (magit-refresh))

  (setq magit-delta-hide-plus-minus-markers t)
  (with-eval-after-load 'magit-delta
    ;; syntax highlight removals, not just additions
    (add-to-list 'magit-delta-delta-args "--minus-style='syntax auto'")
    ;; don't refine hunks
    (add-to-list 'magit-delta-delta-args "--minus-emph-style=minus-style")
    (add-to-list 'magit-delta-delta-args "--plus-emph-style=plus-style"))

  ;; mismatched background issue
  ;; from https://github.com/dandavison/magit-delta/issues/6#issuecomment-808824398

  ;; for delta's github light (github is the default):
  (defun my/magit-delta-set-light ()
    (with-eval-after-load 'magit-delta
      (set-face-attribute 'magit-diff-added-highlight nil :background "#d0ffd0")
      (set-face-attribute 'magit-diff-added nil :background "#d0ffd0")
      (set-face-attribute 'magit-diff-removed-highlight nil :background "#ffe0e0")
      (set-face-attribute 'magit-diff-removed nil :background "#ffe0e0")))

  ;; for delta's github dark:
  (defun my/magit-delta-set-dark ()
    (with-eval-after-load 'magit-delta
      (set-face-attribute 'magit-diff-added-highlight nil :background "#002800")
      (set-face-attribute 'magit-diff-added nil :background "#002800")
      (set-face-attribute 'magit-diff-removed-highlight nil :background "#3f0001")
      (set-face-attribute 'magit-diff-removed nil :background  "#3f0001")))

  ;; wonder what this does ?
  (add-hook 'magit-delta-mode-hook
            (lambda ()
              (setq face-remapping-alist
                    (seq-difference face-remapping-alist
                                    '((magit-diff-removed . default)
                                      (magit-diff-removed-highlight . default)
                                      (magit-diff-added . default)
                                      (magit-diff-added-highlight . default))))))
  ;; end delta stuff

  (setq magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18))


  ;; ivy/ivy-rich --------------------------------------------------------------

  ;; debounce? experimenting
  ;; (setopt ivy-dynamic-exhibit-delay-ms 250)

  (evil-define-key 'normal ivy-minibuffer-map
    [return] #'exit-minibuffer ;; is this the same as #'ivy-done?
    [escape] #'minibuffer-keyboard-quit)

  (evil-define-key nil ivy-minibuffer-map
    (kbd "C-j") #'ivy-next-line
    (kbd "C-k") #'ivy-previous-line)

  ;; unsure why this is necessary, but C-j/C-k doesn't work without it
  (add-hook 'minibuffer-setup-hook 'evil-insert-state)

  (setq ivy-rich-parse-remote-buffer nil)

  (let* ((switch-buffer-configs
          (seq-map (lambda (func)
                     (plist-get ivy-rich-display-transformers-list func))
                   '(ivy-switch-buffer
                     ivy-switch-buffer-other-window
                     counsel-switch-buffer
                     counsel-switch-buffer-other-window
                     persp-switch-to-buffer)))
         (my-columns-config
          '((all-the-icons-ivy-rich-buffer-icon)
            (ivy-rich-candidate
             (:width 45))
            (ivy-rich-switch-buffer-indicators
             (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode
             (:width 20 :face warning))
            (ivy-rich-switch-buffer-project
             (:width 25 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x
                        (ivy-rich-minibuffer-width 0.3))))))))
    (dolist (config switch-buffer-configs)
      (plist-put config :columns my-columns-config)))

  ;; fix "error code 2" counsel/ripgrep issue
  ;; based on https://github.com/doomemacs/doomemacs/issues/3038#issuecomment-929996064 but without
  ;; the doom macros
  ;; TODO: upstream to spacemacs?
  (with-eval-after-load 'counsel
    (advice-add 'counsel-rg
                :around
                (lambda (func &rest args)
                  (cl-flet ((filter-func (code) (if (= code 2) 0 code)))
                    (unwind-protect
                        (progn (advice-add 'process-exit-status :filter-return #'filter-func)
                               (apply func args))
                      (advice-remove 'process-exit-status #'filter-func))))))

  (cl-destructuring-bind (rg-exe . existing-args) counsel-rg-base-command
    (let* ((wanted-args '("--hidden"
                          "--glob=!.git/"
                          "--glob=!.venv/"
                          "--glob=!node_modules/"))
           (new-args (seq-difference wanted-args existing-args)))
      (setq counsel-rg-base-command (append (cons rg-exe new-args) existing-args))))

  (defun my/toggle-counsel-rg-ignore-vcs ()
    (interactive)
    (let ((flag "--no-ignore-vcs"))
      (if (member flag counsel-rg-base-command)
          (progn
            (setq counsel-rg-base-command (remove flag counsel-rg-base-command ))
            (message "excluding .gitignore matches"))
        (setq counsel-rg-base-command
              (cl-destructuring-bind (rg-exe . args) counsel-rg-base-command
                (cons rg-exe (cons flag args))))
        (message "including .gitignore"))))

  ;; themeing -----------------------------------------------------------------
  (defconst my/border-width 10)

  ;; also called by gnome extension via emacsclient
  (defun my/load-theme (system-appearance)
    (mapc 'disable-theme custom-enabled-themes)
    (pcase system-appearance
      ('dark (progn
               (load-theme (first dotspacemacs-themes) t)
               (my/magit-delta-set-dark)))
      ('light (progn
                (load-theme (second dotspacemacs-themes) t)
                (my/magit-delta-set-light)))))

  ;; using emacs-plus integration for macos
  (when (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions #'my/load-theme)
    (my/load-theme ns-system-appearance))

  ;; for gnome, best we can do right now is check on startup
  (unless my/macos-flag
    (thread-first "gsettings get org.gnome.desktop.interface color-scheme"
                  shell-command-to-string
                  (string-trim "'" "'\n")
                  (pcase ("default" 'light) ("prefer-dark" 'dark))
                  (my/load-theme)))

  (toggle-menu-bar-mode-from-frame -1)

  ;; doom ---
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (load-library "lsp-treemacs-themes") ;; https://github.com/emacs-lsp/lsp-treemacs/issues/89
  (doom-themes-treemacs-config)

  ;; borders, etc ---
  (fringe-mode (cons my/border-width my/border-width))

  (setq window-divider-default-right-width 1
        window-divider-default-bottom-width 1)
  (menu-bar-bottom-and-right-window-divider)

  (setq ivy-posframe-border-width my/border-width
        which-key-posframe-border-width my/border-width)

  (spacemacs/toggle-vi-tilde-fringe-off)

  (defun my/do-theme-tweaks ()
    "misc tweaks that for some reason need a nudge after theme change"
    (let ((default-background (face-background 'solaire-default-face)))
      ;; for some reason both need to be set for which-key-posframe to look right
      (set-face-background 'child-frame-border default-background)
      (set-face-background 'which-key-posframe-border default-background)
      (set-face-background 'ivy-posframe-border default-background)
      (set-face-background 'fringe default-background)
      (set-face-attribute 'show-paren-match nil :underline t))
    (set-face-foreground 'all-the-icons-ivy-rich-doc-face (doom-color 'base7))
    (if my/macos-flag ;; fix current-line jiggle w/ doom themes
        (set-face-attribute 'line-number-current-line nil :weight 'normal))
    (window-divider-mode 1)
    (doom-modeline-invalidate-huds))

  (add-hook 'spacemacs-post-theme-change-hook #'my/do-theme-tweaks)
  (my/do-theme-tweaks)

  (add-hook
   'terraform-mode-hook
   (lambda () (set-face-foreground 'terraform--resource-name-face "hot pink")))

  ;; doom-modeline -------------------------------------------------------------
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-hud t
        doom-modeline-percent-position nil
        doom-modeline-buffer-encoding nil
        doom-modeline-bar-width my/border-width
        doom-modeline-irc t
        doom-modeline-persp-name nil
        ;; want this, but slows scrolling. todo: cake and eat it to?
        ;; doom-modeline-buffer-state-icon nil ;; testing without
        )

  (defun my/toggle-relative-path-in-modeline ()
    (interactive)
    (setq-local doom-modeline-buffer-file-name-style
                (if doom-modeline-buffer-file-name-style
                    nil
                  'relative-from-project)))
  (spacemacs/set-leader-keys "of" #'my/toggle-relative-path-in-modeline)

  (set-face-attribute 'doom-modeline-persp-name nil :inherit 'unspecified)
  (defun doom-modeline-segment--major-mode () nil)

  ;; evil ------------------------------------------------------------------------
  ;; vi ---
  (setq evil-want-Y-yank-to-eol t)
  (evil-define-key 'visual 'global (kbd "v") #'evil-visual-line)
  (evil-define-key 'motion 'global
    (kbd "V") (kbd "C-v $"))

  ;; evil in ivy/minibuffer
  (setq evil-want-minibuffer t)

  ;; https://emacs.stackexchange.com/a/66679
  (setq evil-kill-on-visual-paste nil)

  ;; ex stuff ---
  ;; what about evil-ex-map? what does it do?
  (dolist (my-keymap (list evil-ex-completion-map evil-ex-search-keymap))
    (evil-define-key* 'normal my-keymap
      [escape] #'minibuffer-keyboard-quit)

    (evil-define-key* '(normal insert) my-keymap
      (kbd "C-j") #'next-history-element
      (kbd "C-k") #'previous-history-element))


  ;; eval-expression, etc ---
  (evil-define-key 'normal minibuffer-local-map
    [return] #'exit-minibuffer
    [escape] #'minibuffer-keyboard-quit)

  (evil-define-key '(normal insert) minibuffer-local-map
    (kbd "C-j") #'next-history-element
    (kbd "C-k") #'previous-history-element)

  ;; misc ---
  (evil-define-key 'normal 'global (kbd "C-,") #'evil-emacs-state)
  (evil-define-key 'insert 'global (kbd "C-,") #'evil-emacs-state)
  (evil-define-key 'emacs  'global (kbd "C-,") #'evil-normal-state)

  ;; motion-state instead of evilified-state in special-mode buffers (eg *helpful, *Messages*)
  (delete 'special-mode evil-evilified-state-modes)
  ;; these special-mode-inheriting modes are exceptions
  (dolist (mode '(docker-container-mode
                  docker-volume-mode
                  docker-machine-mode
                  docker-network-mode
                  docker-image-mode))
    (add-to-list 'evil-evilified-state-modes mode))

  ;; default for special-mode derivatives is 'kill-window
  (evil-define-key nil helpful-mode-map "q" #'kill-buffer-and-window)
  (evil-define-key nil help-mode-map "q" #'kill-buffer-and-window)

  ;; make C-k work in ivy/insert (and elsewhere, probably)
  (evil-define-key 'insert 'global (kbd "C-k") nil)

  (defun my/sync-registers-system-to-emacs ()
    (interactive)
    (evil-set-register ?\" (evil-get-register ?\+)))

  (defun my/sync-registers-emacs-to-system ()
    (interactive)
    (evil-set-register ?\+ (evil-get-register ?\")))


  ;; vterm ---------------------------------------------------------------------
  (evil-define-key 'emacs vterm-mode-map
    (kbd "C-k") #'evil-previous-line
    (kbd "C-j") #'evil-next-line)
  (evil-define-key '(normal insert) vterm-mode-map
    (kbd "C-k") #'vterm-send-up
    (kbd "C-j") #'vterm-send-down)
  (evil-define-key 'emacs vterm-mode-map (kbd "C-,") #'evil-normal-state)

  (setq vterm-max-scrollback 100000     ; maximum size supported
        vterm-min-window-width 65535    ; no suppress-hard-newline :(
        vterm-always-compile-module t   ; doesn't work? still prompts at first install
        vterm-clear-scrollback-when-clearing t)

  ;; javascript/typescript ---------------------------------------------------

  ;; typescript mode seems to work better than js2, at least w/r/t performance
  (add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . typescript-mode))
  (add-to-list 'auto-mode-alist `(,(rx ".mjs" string-end) . typescript-mode))

  (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
    ;; TODO: ts-node repl?
    (kbd "si") #'nodejs-repl)

  ;; (setq dap-js-debug-program `("node" ,(expand-file-name "~/Downloads/dist/src/dapDebugServer.js")))
  ;; (add-hook 'typescript-mode-hook (lambda () (require 'dap-js)))

  ;; for testing https://github.com/emacs-lsp/dap-mode/pull/736 :
  ;; (add-hook 'typescript-mode-hook (lambda () (require 'dap-js-debug)))

  ;; TODO: upstream these to lsp-mode
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.angular\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.nx\\'"))


  ;; experimenting
  (setq lsp-javascript-completions-complete-function-calls nil)

  ;; apheleia ---------------------------------------------------------------------
  (apheleia-global-mode +1)

  ;; eslint -----------------------------------------------------------------------
  ;; reduce modeline clutter
  (add-hook 'lsp-before-initialize-hook
            (lambda () (defun lsp-eslint-status-handler (_ _) t)))

  ;; html -----------------------------------------------------------------------------
  (evil-define-key 'normal web-mode-map
    (kbd "zc") #'web-mode-fold-or-unfold
    (kbd "zo") #'web-mode-fold-or-unfold)

  ;; css/scss ------------------------------------------------------------------------
  (setq css-fontify-colors nil)

  ;; org --------------------------------------------------------------------------
  (with-eval-after-load 'org
    (require 'ox-jira)
    (org-babel-do-load-languages 'org-babel-load-languages '((scheme . t)))
    (setq org-confirm-babel-evaluate nil
          org-format-latex-options (plist-put org-format-latex-options :scale 1.2)))

  (evil-define-key 'normal 'org-mode-map (kbd "<S-return>") #'org-babel-execute-src-block)

  (add-hook 'org-mode-hook #'spacemacs/toggle-line-numbers-off) ;; doesn't work
  (add-hook 'org-mode-hook #'spacemacs/toggle-auto-fill-mode-on)
  (add-hook 'org-mode-hook (lambda () (setq show-trailing-whitespace nil)))

  ;; scheme -------------------------------------------------------------------------
  (spacemacs/set-leader-keys-for-major-mode 'scheme-mode
    "gd" #'spacemacs/jump-to-definition)
  (setq geiser-repl-history-no-dups-p nil)

  ;; yadm ------------------------------------------------------------------------
  (require 'tramp)

  ;; for debugging
  ;; (setq tramp-verbose 10)
  ;; (tramp-cleanup-all-connections)
  ;; (setq tramp-persistency-file-name nil)

  ;; for yadm method -- tell tramp where to find stuff in nixos box
  (add-to-list 'tramp-remote-path (format "/etc/profiles/per-user/%s/bin" (user-login-name)))

  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))))

  (defun my/magit-yadm ()
    (interactive)
    (magit-status "/yadm::"))
  (spacemacs/set-leader-keys "oy" #'my/magit-yadm)

  ;; c/c++ ----------------------------------------------------------------------
  (setq c-basic-offset 4)

  ;; shell-scripts -------------------------------------------------------------
  (if my/macos-flag
      (add-hook 'sh-mode-hook (lambda () (company-mode -1))))

  ;; proced -------------------------------------------------------------------
  ;; pcpu and pmem don't work on mac
  (setq-default proced-format '(pid user start pcpu pmem comm args)
                proced-filter 'all)
  (add-hook 'proced-mode-hook (lambda () (evil-motion-state +1)))
  (evil-define-key 'motion proced-mode-map (kbd "C-c k")  #'proced-send-signal)


  ;; sql -------------------------------------------------------------------
  (setq sqlfmt-executable "sql-formatter"
        sqlfmt-options nil)

  ;; docker -------------------------------------------------------------------
  (defun my/docker-tramp-find-file ()
    (interactive)
    (spacemacs/counsel-find-file "/docker:"))

  (spacemacs/declare-prefix "od" "docker")
  (spacemacs/set-leader-keys
    "odf" #'my/docker-tramp-find-file
    "odb" #'docker-container-shell
    "odB" #'docker-container-shell-env)

  (setq docker-container-columns '((:name "Id" :width 16 :template "{{ json .ID }}" :sort nil :format nil)
                                   (:name "Image" :width 15 :template "{{ json .Image }}" :sort nil :format nil)
                                   (:name "Names" :width 20 :template "{{ json .Names }}" :sort nil :format nil)
                                   (:name "Status" :width 20 :template "{{ json .Status }}" :sort nil :format nil)
                                   (:name "Created" :width 23 :template "{{ json .CreatedAt }}" :sort nil
                                          :format (lambda (x) (format-time-string "%F %T" (date-to-time x))))
                                   (:name "Ports" :width 10 :template "{{ json .Ports }}" :sort nil :format nil)
                                   (:name "Command" :width 30 :template "{{ json .Command }}" :sort nil :format nil)))

  (setq docker-pop-to-buffer-action '(display-buffer-same-window))

  ;; yaml ---------------------------------------------------------------------
  (add-hook 'yaml-ts-mode-hook (lambda () (origami-mode +1))) ;; doesn't work terribly well
  (add-hook 'yaml-ts-mode-hook #'spacemacs/toggle-spelling-checking-off)

  ;; symex --------------------------------------------------------------------
  (evil-define-key '(normal insert) symex-mode-map
    (kbd "S-<escape>") #'symex-mode-interface)

  (setq symex--user-evil-keyspec
        '(("j" . symex-go-up)
          ("k" . symex-go-down)
          ("C-j" . symex-climb-branch)
          ("C-k" . symex-descend-branch)
          ("M-j" . symex-goto-highest)
          ("M-k" . symex-goto-lowest)))

  (dolist (mode '(lisp-data-mode ielm-mode janet-mode))
    (add-to-list 'symex-elisp-modes mode))
  (symex-initialize)

  ;; highlight-indentation ----------------------------------------------------
  ;; this is off by default
  (setq highlight-indentation-blank-lines t)

  ;; smartparens --------------------------------------------------------------
  ;; https://github.com/Fuco1/smartparens/issues/1036
  (defun my/minibuffer-fix-sp ()
    (setq-local comment-start ";")
    (sp-local-pair 'minibuffer-pairs "`" nil :actions nil)
    (sp-update-local-pairs 'minibuffer-pairs))
  (add-hook 'eval-expression-minibuffer-setup-hook #'my/minibuffer-fix-sp)

  ;; treemacs -------------------------------------------------------------------
  (add-hook 'treemacs-post-buffer-init-hook #'hl-line-mode)

  ;; tree-sitter ----------------------------------------------------------------
  (setq tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t)

  ;; make more stuff foldable
  ;; TODO: switch/case
  ;; TODO: upstream these
  (defvar my/extra-javascript-folds
    '((object . ts-fold-range-seq)
      (array . ts-fold-range-seq)
      (arguments . ts-fold-range-seq)
      (formal_parameters . ts-fold-range-seq)
      (template_string . ts-fold-range-seq)
      (class_body . ts-fold-range-seq)))

  (defvar my/extra-typescript-folds
    (append my/extra-javascript-folds
            '((object_type . ts-fold-range-seq))))

  (dolist (mode '(javascript-mode js-mode js2-mode js3-mode))
    (dolist (item my/extra-javascript-folds)
      (cl-pushnew item (alist-get mode ts-fold-range-alist))))

  (dolist (item my/extra-typescript-folds)
    (cl-pushnew item (alist-get 'typescript-mode ts-fold-range-alist)))

  ;; purescript ----------------------------------------------------------------
  (setq purescript-indent-offset 2)

  ;; minimap ------------------------------------------------------------------
  ;; TODO: try out https://github.com/zk-phi/sublimity#sublimity-map-minimap-experimental
  (setq minimap-window-location 'right
        minimap-width-fraction 0.05
        minimap-minimum-width 10
        minimap-dedicated-window t
        minimap-hide-scroll-bar t
        minimap-always-recenter nil ;; idk
        minimap-recenter-type 'middle)

  ;; hide fringe/glyph junk. not really sure why this works
  (add-hook 'minimap-mode-hook (lambda () (set-window-fringes (minimap-get-window) 1 1)))


  ;; lsp/dap ---------------------------------------------------------------------
  ;; TODO: upstream these
  (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
    "hf" #'lsp-ui-doc-focus-frame)
  (evil-define-key 'normal 'lsp-ui-doc-frame-mode
    "q" #'lsp-ui-doc-unfocus-frame)

  (put 'lsp-treemacs-errors-list 'disabled "Performance issues.")
  ;; performance issues
  ;; maybe unnecessary on top of (setq lsp-enable-dap-auto-configure nil)
  (dap-auto-configure-mode -1)

  ;; verb ---------------------------------------------------------------------
  (setq verb-auto-kill-response-buffers t)

  ;; rust ---------------------------------------------------------------------
  ;; `rust--format-call' needs to kill the *rustfmt* buffer when it's done, but for
  ;; some reason it does that with `kill-buffer-and-window', killing the buffer to
  ;; be formatted as well. We don't want that.
  (advice-add 'rust--format-call
              :around (lambda (func &rest args)
                        (cl-letf (((symbol-function 'kill-buffer-and-window) #'kill-buffer))
                          (apply func args))))

  ;; text ---------------------------------------------------------------------
  (add-hook 'text-mode-hook #'spacemacs/toggle-spelling-checking-on)

  ;; nushell ------------------------------------------------------------------
  (with-eval-after-load 'nushell-mode
    (set-face-foreground 'nushell-pay-attention-face (doom-color 'base6)))

  ;; woman ---------------------------------------------------------------------
  ;; TODO: make woman less weird about windows

  ;; ffap ----------------------------------------------------------------------
  ;; TODO: advise to prefix relative paths with (projectile-project-root)

  ;; compilation-mode -------------------------------------------------------------
  ;; TODO: don't be weird about windows

  ;; janet -------------------------------------------------------------
  ;; see local/custom layer for the rest
  (advice-add 'spacemacs/janet-format-format-buffer :around #'envrc-propagate-environment)

  ;; eat -------------------------------------------------------------------
  (use-package eat
    :custom
    (eat-enable-auto-line-mode t)
    (eat-enable-directory-tracking t)
    (eat-enable-auto-line-mode t))

  (spacemacs/set-leader-keys "ot" #'eat-project)

  (evil-define-key '(normal insert) eat-mode-map
    (kbd "C-j") #'eat-line-next-input
    (kbd "C-k") #'eat-line-previous-input
    [return] #'eat-line-send-input)

  (add-hook 'eat-mode-hook (lambda () (toggle-truncate-lines +1)))


  ;; ==========================================================================
  (when my/work-flag
    (load (file-truename "~/.spacemacs.d/day-job.el") nil nil t))
  (spacemacs/toggle-debug-on-error-off))
;; end dotspacemacs/user-config
;; ==========================================================================

;; misc commands --------------------------------------------------------------
(defun my/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun my/browse-info ()
  (interactive)
  (info (buffer-file-name)))

(defun my/ansi-color/apply-on-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun my/tramp-ssh ()
  (interactive)
  (spacemacs/counsel-find-file "/ssh:"))

(defun my/kill-buffer-process ()
  (interactive)
  (kill-process (get-buffer-process (current-buffer))))

(defun my/unescape-newlines ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (search-forward "\\n" (line-end-position) t)
      (replace-match "\n"))))

;; TODO: implement as mode?
(defvar my/prosey nil)

(defun my/toggle-prosey-on ()
  (interactive)
  (toggle-word-wrap +1)
  (visual-line-mode +1)
  (spacemacs/toggle-line-numbers-off)
  (spacemacs/toggle-relative-line-numbers-off)
  (spacemacs/toggle-truncate-lines-off)
  (spacemacs/toggle-spelling-checking-on)
  (setq my/prosey t))

(defun my/toggle-prosey-off ()
  (interactive)
  (toggle-word-wrap -1)
  (visual-line-mode -1)
  (spacemacs/toggle-line-numbers-on)
  (spacemacs/toggle-relative-line-numbers-on)
  (spacemacs/toggle-truncate-lines-on)
  (spacemacs/toggle-spelling-checking-off)
  (setq my/prosey nil))

(defun my/toggle-prosey ()
  (interactive)
  (if my/prosey
      (my/toggle-prosey-off)
    (my/toggle-prosey-on)))

(defun my/create-new-project (dir)
  (interactive "sdirectory name? ")
  (let ((path (f-join "~/devel" dir)))
    (make-empty-file (f-join path "README.md") t)
    (let ((default-directory path))
      (shell-command "git init"))
    (projectile-add-known-project path)))

(defun my/clone-new-project (url)
  (interactive "surl? ")
  (require 's)
  (let ((default-directory "~/devel")
        (repo-name  (s-replace ".git" "" (car (last (split-string url "/"))))))
    (message "cloning...")
    ;; TODO: make it async
    ;; TODO: check return code
    (shell-command (format "git clone %s" url))
    (projectile-add-known-project (f-join default-directory repo-name)))
  (message "git clone complete"))


;; don't really need this now that we're using --with-poll on macos
(defun my/fix-too-many-open-files ()
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

(defun my/toggle-bool (var)
  (interactive "vvariable? ")
  (cond ((equal (eval var) t)
         (set var nil)
         (message "%s is now nil" var))
        ((equal (eval var) nil)
         (message "nil")
         (set var t)
         (message "%s is now t" var))
        (t (error "%s is not a bool" var))))

(defun my/advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;; WIP
(defun my/sync-project-layout-buffers ()
  (interactive)
  (let ((persp-names
         (seq-filter (lambda (dir-name) (not (string= dir-name dotspacemacs-default-layout-name)))
                     (seq-map #'f-filename (persp-names-current-frame-fast-ordered))))
        (project-name (projectile-project-name)))
    (projectile-process-current-project-buffers-current
     (lambda ()
       (when (seq-contains-p persp-names project-name #'string=)
         (message "adding buffer %s to layout %s" (buffer-name (current-buffer)) project-name)
         (persp-add-buffer (current-buffer) (persp-get-by-name project-name)))))))

;; TODO: annoying auth is needed on each call. what can we do about this?
;; seems it wouldn't happen if called in process somehow:
;; https://1password.community/discussion/138627/cli-keeps-prompting-for-authentication
;; or can turn off app integration and use `op signing', but then master password is required.
;; try https://github.com/xuchunyang/1password.el
(defun my/1password-read (url)
  (string-trim (shell-command-to-string (format "op read '%s'" url))))
