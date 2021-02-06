;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

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
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '((haskell :variables
              haskell-completion-backend 'lsp)
     scheme
     rust
     (html :variables
           css-enable-lsp t
           scss-enable-lsp t
           html-enable-lsp t)
     import-js
     (javascript :variables javascript-import-tool 'import-js)
     typescript
     (java :variables java-backend 'lsp)
     (unicode-fonts :variables
                    unicode-fonts-enable-ligatures t
                    unicode-fonts-ligature-modes '(typescript-mode  ;; breaks in python files
                                                   javascript-mode
                                                   web-mode
                                                   html-mode
                                                   scss-mode
                                                   css-mode))
     csv
     sql
     terraform
     nginx
     epub
     ansible
     yaml
     docker
     dap
     multiple-cursors

     (treemacs :variables
               treemacs-sorting 'alphabetic-asc
               treemacs-use-filewatch-mode t
               treemacs-use-git-mode 'extended)

     (python :variables
             python-backend 'lsp
             python-lsp-server 'mspyls
             python-tab-width 4
             python-fill-column 100
             python-formatter 'yapf
             python-format-on-save nil
             python-sort-imports-on-save nil
             python-fill-docstring-style 'django)
     ipython-notebook
     ivy
     auto-completion
     emacs-lisp
     git
     github
     markdown
     org
     epub
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'vterm)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     syntax-checking
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(doom-themes
     which-key-posframe
     editorconfig
     ivy-posframe
     ivy-rich
     all-the-icons-ivy-rich)


   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(doom-flatwhite-theme  ;; if these arent't listed here spacemacs
                                    doom-henna-theme)     ;; prints a harmless error on startup. why??

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

  ;; https://www.reddit.com/r/emacs/comments/cdf48c/failed_to_download_gnu_archive/
  ;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 10

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
   dotspacemacs-startup-buffer-show-version nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

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

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

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
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(; the gotos
                         doom-nord-light
                         doom-nord

                         ; extra lights
                         doom-one-light
                         doom-opera-light
                         doom-tomorrow-day
                         doom-flatwhite

                         ; extra medium
                         doom-nova

                         ; extra darks
                         doom-henna
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

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Fira Code"
                               :size 10.0
                               :weight normal
                               :width normal)

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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

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
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup (eq system-type 'darwin)

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

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

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
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

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

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
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "Emacs"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

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
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq byte-compile-warnings '(cl-functions))
  (if (eq system-type 'darwin)
      (setq insert-directory-program "/usr/local/bin/gls")))

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

  ;; init standalone modes ----------------------------------------------------
  (use-package all-the-icons-ivy-rich
    :ensure t
    :init (all-the-icons-ivy-rich-mode 1))
  (use-package ivy-rich
    :ensure t
    :init (ivy-rich-mode 1))
  (ivy-posframe-mode 1)
  (which-key-posframe-mode 1)
  (editorconfig-mode 1)


  ;; misc/general --------------------------------------------------------------
  (add-hook 'hack-local-variables-hook 'spacemacs/toggle-truncate-lines-on)
  (setq select-enable-clipboard nil)
  (setq create-lockfiles nil)
  (setq projectile-indexing-method 'hybrid)
  (customize-set-variable 'custom-file (file-truename "~/.emacs-custom.el"))
  (load custom-file)
  (evil-define-key 'normal 'global (kbd "zz") 'evil-toggle-fold)
  (setq bidi-inhibit-bpa t)
  (setq bidi-paragraph-direction 'left-to-right)
  (setq byte-compile-warnings '(cl-functions))
  (spacemacs/set-leader-keys "fE" 'custom/echo-file-path)  ;; TODO: how to make which-key reflect this?
  ;; (setq ansible-vault-password-file "foo")              ;; TODO: set this to 'projectile-project-root / .vault_pass
  ;; (evil-define-key nil 'global (kbd "<leader>-:") 'eval-expression)


  ;; python ------------------------------------------------------------------------
  ;; --- tweaks
  (setq dap-python-debugger 'debugpy) ; this should be the default at some point
  (add-hook 'python-mode-hook 'spacemacs/toggle-fill-column-indicator-on)
  ;; --- dependencies
  ;; pip install importmagic epc ipython debugpy flake8
  ;; (setq python-shell-interpreter (if (eq system-type 'darwin) "python3.8" "python3"))
  (setq python-shell-interpreter "python3")

  (setq flycheck-python-flake8-executable python-shell-interpreter)
  ;; (setq lsp-python-ms-python-executable-cmd python-shell-interpreter)  ;; overrides activated venv, no bueno
  ;; (setq poetry-tracking-strategy 'switch-buffer)
  ;; (poetry-tracking-mode)
  (add-hook 'ein:notebook-mode-hook 'spacemacs/toggle-fill-column-indicator-off)
  (setq ein:output-area-inlined-images t)


  ;; git ----------------------------------------------------------------------
  (setq browse-at-remote-remote-type-domains '(("git.loc.gov" . "gitlab")
                                               ("github.com" .  "github")))
  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  ;; (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)


  ;; writeroom -------------------------------------------------------------------------
  (add-hook 'writeroom-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
  (add-hook 'writeroom-mode-hook 'spacemacs/toggle-line-numbers-off)
  (add-hook 'writeroom-mode-hook 'spacemacs/toggle-spelling-checking-on)
  (add-hook 'writeroom-mode-hook 'spacemacs/toggle-fullscreen-frame-off)


  ;; ivy ---------------------------------------------------------------------
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-virtual-abbreviate 'full)  ; does this actually do anything?
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-wrap t)
  (setq counsel-rg-base-command
        (append (butlast counsel-rg-base-command) '("--hidden" "%s")))


  ;; autosave ------------------------------------------------------------------
  (defun save-buffer-if-needed ()
    (when (and (buffer-file-name) (buffer-modified-p))
      (save-buffer)))
  (add-hook 'focus-out-hook 'save-buffer-if-needed)
  ;; the following don't seem to work :(
  (defadvice switch-to-buffer (before set-buffer activate)
    (save-buffer-if-needed))
  (defadvice other-window (before other-window-now activate)
    (save-buffer-if-needed))


  ;; undo --------------------------------------------------------------------
  ;; --- persistent undo
  ;; https://github.com/syl20bnr/spacemacs/issues/774#issuecomment-77712618
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  ;; --- granular history
  (setq evil-want-fine-undo t)


  ;; themeing -----------------------------------------------------------------
  (spacemacs/toggle-vi-tilde-fringe-off)
  (fringe-mode '(0 . nil))  ; disable right "fringe"
  ;; hide arrows at window border for truncated lines
  (define-fringe-bitmap 'left-curly-arrow (make-vector 8 #b0))
  (define-fringe-bitmap 'right-curly-arrow (make-vector 8 #b0))
  (define-fringe-bitmap 'right-arrow (make-vector 8 #b0))

  (load-library "lsp-treemacs-themes")   ;; why does this fix icons?
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)

  (doom-themes-org-config)
  (doom-themes-visual-bell-config)

  (setq window-divider-default-right-width 10)

  (setq ivy-posframe-border-width 10)
  (setq which-key-posframe-border-width 10)

  (defun do-theme-tweaks ()
    "misc tweaks that for some reason need a nudge after theme change"
    ;; posframe color stuff
    (let ((face-color (face-background 'ivy-posframe)))
      (set-face-background 'which-key-posframe face-color)
      (set-face-background 'which-key-posframe-border face-color)
      (set-face-background 'ivy-posframe-border face-color))
    ;; lighter window divider
    (set-face-foreground 'window-divider (face-background 'mode-line-inactive))
    (window-divider-mode))

  (add-hook 'spacemacs-post-theme-change-hook 'do-theme-tweaks)
  (do-theme-tweaks)

  (add-hook
   'terraform-mode-hook
   (lambda () (set-face-foreground 'terraform--resource-name-face "hot pink")))


  ;; doom-modeline -------------------------------------------------------------
  (setq doom-modeline-window-width-limit 90)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-buffer-encoding nil)
  ; default: https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline.el#L92-L94
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding process vcs checker))


  ;; evil ------------------------------------------------------------------------
  ;; --- vi
  (define-key evil-visual-state-map (kbd "v") 'evil-visual-line)
  (define-key evil-motion-state-map (kbd "V") (kbd "C-v $"))
  (define-key evil-motion-state-map (kbd "RET") 'evil-ex-nohighlight)

  ;; --- ivy/minibuffer
  (setq evil-want-minibuffer t)
  (define-key evil-insert-state-map "\C-k" nil) ;; make C-k work in ivy/insert
  (evil-define-key 'normal minibuffer-local-map [return]    'exit-minibuffer)
  (evil-define-key 'normal minibuffer-local-map [escape]    'minibuffer-keyboard-quit)
  (evil-define-key 'normal ivy-minibuffer-map   [return]    'exit-minibuffer)
  (evil-define-key 'normal ivy-minibuffer-map   [escape]    'minibuffer-keyboard-quit)

  ;; only works in normal mode :/
  (evil-define-key '(normal insert) minibuffer-local-map (kbd "C-j") 'next-history-element)
  (evil-define-key '(normal insert) minibuffer-local-map (kbd "C-k") 'previous-history-element)

  ;; --- misc
  (delete 'special-mode evil-evilified-state-modes)
  (evil-define-key 'normal special-mode-map "q" 'quit-window)


  ;; LSP -----------------------------------------------------------------------
  (setq lsp-ui-doc-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (setq lsp-ui-sideline-enable t)


  ;; vterm ---------------------------------------------------------------------
  (defun pop-shell-at-project-root-or-home ()
    (interactive)
    (if (projectile-project-p)
        (spacemacs/projectile-shell-pop)
      (spacemacs/default-pop-shell)))
  (spacemacs/set-leader-keys "'" 'pop-shell-at-project-root-or-home)

  ;; (evil-define-key 'emacs vterm-mode-map (kbd "C-k") 'evil-previous-line)
  ;; (evil-define-key 'emacs vterm-mode-map (kbd "C-j") 'evil-next-line)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "C-k") 'vterm-previous-prompt)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "C-j") 'vterm-next-prompt)
  (evil-define-key 'emacs vterm-mode-map (kbd "C-,") 'evil-normal-state)
  (evil-define-key 'normal vterm-mode-map (kbd "C-,") 'evil-emacs-state)
  (evil-define-key 'insert vterm-mode-map (kbd "C-,") 'evil-emacs-state)

  (setq vterm-max-scrollback 100000)  ; maximum size supported
  (setq vterm-always-compile-module t)
  ;; (setq term-suppress-hard-newline t) ;; vterm equivalent?


  ;; haskell -------------------------------------------------------------------
  (evil-define-key 'normal haskell-interactive-mode-map
    (kbd "C-j") 'haskell-interactive-mode-history-next)
  (evil-define-key 'normal haskell-interactive-mode-map
    (kbd "C-k") 'haskell-interactive-mode-history-previous)


  ;; angular/web ---------------------------------------------------------------
  (setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
  (setenv "TSC_NONPOLLING_WATCHER" "true")

  (setq lsp-clients-angular-language-server-command
        (let ((node-modules "/usr/local/lib/node_modules"))
          `("node"
            ,(concat node-modules "/@angular/language-server")
            "--ngProbeLocations" ,node-modules
            "--tsProbeLocations" ,node-modules
            "--experimental-ivy"
            "--stdio")))

  ;; (setq-default js-indent-level 2
  ;;               javascript-indent-level 2
  ;;               typescript-indent-level 2
  ;;               web-mode-markup-indent-offset 2
  ;;               web-mode-css-indent-offset 2
  ;;               web-mode-code-indent-offset 2
  ;;               css-indent-offset 2)


  ;; org --------------------------------------------------------------------------
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((scheme . t)))
    (setq org-confirm-babel-evaluate nil))

  (setq org-adapt-indentation nil)
  (evil-define-key 'normal 'org-mode-map (kbd "<S-return>") 'org-babel-execute-src-block)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
)


;; functions for adhoc use ----------------------------------------------------
(defun custom/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun custom/macos-paste ()
  (interactive)
  (ignore-error 'end-of-buffer (forward-char))
  (insert (shell-command-to-string "pbpaste")))

(defun custom/kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "Kill buffers matching this regular expression: ")
  (cl-letf (((symbol-function 'kill-buffer-ask)
         (lambda (buffer) (kill-buffer buffer))))
    (kill-matching-buffers regexp)))

(defun custom/echo-file-path ()
  (interactive)
  (spacemacs/echo (spacemacs--projectile-file-path)))

(defun custom/magit-kill-all ()
     (interactive)
     (custom/kill-buffers "^magit"))

(defun custom/browse-info ()
  (interactive)
  (info (buffer-file-name)))
