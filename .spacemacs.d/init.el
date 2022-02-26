;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defconst my/macos-flag (eq system-type 'darwin))
(defconst my/work-flag nil)

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
   '(systemd
     graphviz
     ansible
     auto-completion
     c-c++
     csv
     dap
     debug
     docker
     emacs-lisp
     epub
     erc
     git
     github
     groovy
     haskell
     helpful
     html
     ibuffer
     ipython-notebook
     ivy
     java
     javascript
     markdown
     multiple-cursors
     nav-flash
     nginx
     org
     posframe
     prettier
     python
     ruby
     rust
     scheme
     shell
     shell-scripts
     slack
     spacemacs-layouts
     spell-checking
     sql
     syntax-checking
     terraform
     tree-sitter
     treemacs
     typescript
     unicode-fonts
     yaml)

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     dired-git-info
     diredfl
     direnv
     fold-this
     gcmh
     guix
     journalctl-mode
     pacfiles-mode
     solaire-mode
     symex
     ;; mini-frame
     (org-clock-reminder
      :location (recipe
                 :fetcher github
                 :repo "eeshugerman/org-clock-reminder"))
     (dconf-dotfile
      :location (recipe
                 :fetcher file
                 :path "~/devel/dconf-dotfile/dconf-dotfile.el"))
     (tree-sitter-langs
      :location local))

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
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
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
   dotspacemacs-startup-buffer-show-version nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 0

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
   dotspacemacs-startup-lists '((todos . 5)
                                (agenda . 5)
                                (recents . 5)
                                (projects . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

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
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light

                         doom-solarized-dark
                         doom-solarized-light

                         doom-nord
                         doom-nord-light

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
   dotspacemacs-default-font `(("JetBrains Mono"
                                :size ,(if my/macos-flag 12.0 10.0))
                               ("Fira Code"
                                :size ,(if my/macos-flag 12.0 10.0)))

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
   dotspacemacs-auto-save-file-location 'original

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
   dotspacemacs-loading-progress-bar nil

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
   dotspacemacs-undecorated-at-startup my/macos-flag

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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

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
  (spacemacs/load-spacemacs-env)
)

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (add-to-list 'warning-suppress-types '(comp))

  (if my/macos-flag
      (setq insert-directory-program "/usr/local/bin/gls"))


  (setq
   ;; misc -- TODO: organize these
   c-c++-lsp-enable-semantic-highlight t
   ;; c-c++-lsp-enable-semantic-highlight 'overlay
   doom-solarized-dark-brighter-modeline t
   haskell-completion-backend 'lsp
   evil-respect-visual-line-mode t

   html-enable-lsp t
   css-enable-lsp t
   scss-enable-lsp t

   groovy-backend 'lsp
   groovy-lsp-jar-path "~/util/groovy-language-server/build/libs/groovy-language-server-all.jar"

   ivy-enable-icons t ;; also sets `ivy-enable-advanced-buffer-information'
   ivy-extra-directories nil
   ivy-initial-inputs-alist nil
   ivy-virtual-abbreviate 'full
   ivy-wrap t

   java-backend 'lsp

   javascript-import-tool (if my/work-flag 'import-js nil)
   javascript-repl 'nodejs
   js2-include-node-externs t

   lsp-ui-doc-enable nil ;; slow w/ large files
   lsp-ui-doc-include-signature t
   lsp-ui-doc-header nil
   lsp-ui-doc-delay 1 ; seconds
   lsp-ui-doc-alignment 'window

   lsp-ui-sideline-enable t
   lsp-ui-sideline-diagnostic-max-line-length 70
   lsp-ui-sideline-diagnostic-max-lines 5
   lsp-ui-sideline-show-symbol nil
   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-show-diagnostics t

   lsp-ui-imenu-enable nil
   lsp-ui-imenu-auto-refresh 'after-save

   lsp-ui-peek-enable t
   lsp-ui-peek-fontify 'always
   lsp-ui-peek-show-directory t
   lsp-ui-peek-list-width 60
   lsp-ui-peek-always-show t

   ;; lsp-eslint-code-action-show-documentation nil
   lsp-eslint-warn-on-ignored-files t

   lsp-eldoc-enable-hover nil
   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil
   lsp-enable-symbol-highlighting t

   lsp-headerline-breadcrumb-enable t
   lsp-headerline-breadcrumb-segments '(symbols)


   org-adapt-indentation t
   org-enable-jira-support t


   python-backend 'lsp
   python-fill-column 100
   python-fill-docstring-style 'django
   python-format-on-save nil
   python-formatter 'black
   python-lsp-server 'pyright
   python-poetry-activate t
   python-sort-imports-on-save nil
   python-tab-width 4

   scheme-implementations '(guile)

   shell-default-height 30
   shell-default-position 'bottom
   shell-default-shell 'shell
   shell-pop-restore-window-configuration nil ;; not really sure what this does

   spacemacs-layouts-restrict-spc-tab t
   spacemacs-layouts-restricted-functions '(ivy-switch-buffer ;; doesn't work. also, shouldn't be necessary
                                            spacemacs/window-split-double-columns
                                            spacemacs/window-split-triple-columns
                                            spacemacs/window-split-grid)
   persp-autokill-buffer-on-remove 'kill-weak

   spell-checking-enable-by-default nil

   tree-sitter-indent-enable t
   tree-sitter-fold-enable t
   tree-sitter-fold-indicators-enable nil ;; cool but kind of slow

   treemacs-sorting 'alphabetic-asc
   treemacs-use-filewatch-mode t
   treemacs-use-git-mode 'extended
   treemacs-use-follow-mode nil
   ;; https://github.com/Alexander-Miller/cfrs/issues/4
   treemacs-read-string-input (if (not my/macos-flag)
                                  'from-minibuffer
                                'from-child-frame)

   unicode-fonts-enable-ligatures t
   unicode-fonts-less-feedback t
   unicode-fonts-ligature-modes '(typescript-mode
                                  javascript-mode
                                  js2-mode
                                  web-mode
                                  html-mode
                                  scss-mode
                                  css-mode))
)

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

  ;; temp ---------------------------------------------------------------------
  ;; remove once https://github.com/syl20bnr/evil-iedit-state/pull/37 is merged
  ;; (defun evil-iedit-state//goto-overlay-start ()
  ;;   "Return the position of the start of the current overlay."
  ;;   (let ((overlay (iedit-find-current-occurrence-overlay)))
  ;;     (if overlay
  ;;         (goto-char (overlay-start overlay))
  ;;       (call-interactively 'evil-beginning-of-line))))
  ;; (define-key evil-iedit-state-map "0"   'evil-iedit-state/evil-beginning-of-line)

  ;; init standalone modes ----------------------------------------------------
  (use-package diredfl
    :defer t
    :hook (dired-mode . diredfl-global-mode))

  (use-package gcmh :config (gcmh-mode 1))
  (use-package direnv :config (direnv-mode 1))
  (use-package guix)
  (use-package solaire-mode :config (solaire-global-mode 1))
  (use-package symex)

  ;; still needs lots of work
  ;; doesn't work with pgtk
  ;; (use-package mini-frame
  ;;   :config
  ;;   (ivy-posframe-mode -1)
  ;;   (which-key-posframe-mode -1)
  ;;   (setq which-key-popup-type 'minibuffer  ;; doesn't work
  ;;         mini-frame-show-parameters '((top . 10)
  ;;                                      (width . 0.7)
  ;;                                      (left . 0.5)))
  ;;   (mini-frame-mode 1))

  ;; spacing issues
  ;; (use-package dired-git-info
  ;;   :hook (dired-after-readin . dired-git-info-auto-enable))


  ;; misc/general --------------------------------------------------------------
  (server-start)

  (spacemacs/set-leader-keys
    ":"  'eval-expression
    "ofe" 'my/echo-file-path
    "oaw" 'eww)

  (setq select-enable-clipboard nil
        create-lockfiles nil
        projectile-indexing-method 'hybrid
        bidi-inhibit-bpa t
        bidi-paragraph-direction 'left-to-right
        completions-ignore-case t)

  (let ((custom-file-path (file-truename "~/.spacemacs.d/custom.el")))
    (unless (file-exists-p custom-file-path)
      (with-temp-buffer (write-file custom-file-path)))
    (customize-set-variable 'custom-file custom-file-path))
  (load custom-file)

  (when my/work-flag
    (load (file-truename "~/.spacemacs.d/day-job.el") nil nil t))

  (remove-hook 'after-make-frame-functions 'persp-init-new-frame)

  ;; (when my/macos-flag
  ;;   (savehist-mode -1)) ;; performance issues?

  (defmacro my/with-no-messages (&rest body)
    ;; `inhibit-message' still logs to *Messages* and (apprently?) clears previous message
    ;; so instead...
    `(cl-letf (((symbol-function 'message) (lambda (&rest args) nil)))
      (progn ,@body)))

  (defun my/suppress-messages-advice (func &rest args)
    (my/with-no-messages (apply func args)))

  (defun my/suppress-messages-hook (func)
    (lambda () (my/with-no-messages (funcall func))))

  (add-hook 'hack-local-variables-hook (my/suppress-messages-hook 'spacemacs/toggle-truncate-lines-on))
  (add-hook 'special-mode-hook (my/suppress-messages-hook 'spacemacs/toggle-truncate-lines-on))

  ;; emacs lisp ----------------------------------------------------------------
  ;; alternatively, switch to gg everywhere?
  (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
    "gd" 'spacemacs/jump-to-definition)


  ;; autosave ------------------------------------------------------------------
  (auto-save-mode -1) ;; only want auto-save-visited-mode
  (setq auto-save-timeout 5)

  ;; company --------------------------------------------------------------------
  (setq company-selection-wrap-around t)

  (when my/macos-flag
    ;; disable in sh-mode
    (setq company-shell-modes '(eshell-mode)))

  ;; gcmh ------------------------------------------------------------------------
  (setq gcmh-verbose nil
        gcmh-low-cons-threshold (* 500 (expt 10 3))
        gcmh-high-cons-threshold (* 500 (expt 10 6))
        gcmh-idle-delay 5)

  ;; dired -----------------------------------------------------------------------
  (defun my/dired-up-directory ()
    (interactive)
    (find-alternate-file ".."))

  (evil-define-key 'normal dired-mode-map
    [return] 'dired-find-alternate-file
    "u" 'my/dired-up-directory)

  ;; undo --------------------------------------------------------------------
  ;; persistent undo ---
  ;; https://github.com/syl20bnr/spacemacs/issues/774#issuecomment-77712618
  ;; is slow?
  (let ((undo-tree-cache-dir (concat spacemacs-cache-directory "undo")))
    (unless (file-exists-p undo-tree-cache-dir)
      (make-directory undo-tree-cache-dir))
    (setq undo-tree-auto-save-history t
          undo-tree-history-directory-alist `(("." . ,undo-tree-cache-dir))))
  ;; granular history ---
  (setq evil-want-fine-undo t)

  (advice-add #'undo-tree-load-history :around #'my/suppress-messages-advice)


  ;; comint --------------------------------------------------------------------
  (evil-define-key 'normal comint-mode-map
    (kbd (concat dotspacemacs-leader-key " b d")) 'comint-send-eof ;; doesn't work :(
    [return] 'comint-send-input)

  (evil-define-key 'normal ielm-map
    [return] 'ielm-return)

  (setq comint-move-point-for-output nil ;; does this do anything?
        comint-scroll-to-bottom-on-input t
        ;; enable colors in shell
        ;; see also `ansi-color-for-comint-*'
        ;; breaks sql-interactive-mode tho :(
        ;; TODO: how to enable for shell-mode but not sql-interactive-mode?
        ;; comint-terminfo-terminal "dumb-emacs-ansi"
        )

  ;; shell (comint) --------------------------------------------------------------------
  (setq shell-pop-autocd-to-working-dir nil
        shell-completion-execonly nil)

  (defun pop-shell-at-project-root-or-home ()
    (interactive)
    (if (projectile-project-p)
        (spacemacs/projectile-shell-pop)
      (spacemacs/default-pop-shell)))
  (spacemacs/set-leader-keys "'" 'pop-shell-at-project-root-or-home)


  ;; term -------------------------------------------------------------------------
  (add-hook 'term-mode-hook (lambda () (term-line-mode)))

  ;; transient --------------------------------------------------------------------
  (with-eval-after-load 'transient
    (define-key transient-map (kbd "<escape>") 'transient-quit-one))

  ;; xml ---------------------------------------------------------------------------
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
  (add-hook 'nxml-mode-hook 'origami-mode)
  (setq nxml-child-indent 2
        nxml-attribute-indent 2)

  ;; info ---------------------------------------------------------------------------
  (evil-define-key 'motion Info-mode-map
    [return] 'Info-follow-nearest-node
    (kbd "[[") 'Info-last
    (kbd "]]") 'Info-next
    (kbd "C-m") 'Info-goto-node)


  ;; python ------------------------------------------------------------------------
  (add-hook 'python-mode-hook 'spacemacs/toggle-fill-column-indicator-on)

  ;; ein ---
  (add-hook 'ein:notebook-mode-hook 'spacemacs/toggle-fill-column-indicator-off)
  (setq ein:output-area-inlined-images t)


  ;; interpreter and tooling ---
  (setq python-shell-interpreter "ipython3")
  (when my/macos-flag
    ;; TODO: experiment with a portable (Linux/MacOS) venv + exec-path
    ;; solution for python dependencies (flake8, importmagic, etc)
    (add-to-list 'exec-path "~/Library/Python/3.8/bin")
    (add-to-list 'exec-path "~/Library/Python/3.9/bin")
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"
          python-shell-completion-native-enable nil))
  (setq dap-python-debugger 'debugpy) ; this should be the default at some point


  ;; git ----------------------------------------------------------------------
  (setq browse-at-remote-remote-type-domains '(("github.com" .  "github")))
  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  (evil-define-key 'normal magit-diff-mode-map
    (kbd "RET") 'magit-diff-visit-worktree-file-other-window)

  ;; https://github.com/magit/magit/issues/2942#issuecomment-1026201640
  ;; (use-package magit-delta
  ;;   :hook (magit-mode . magit-delta-mode))

  ;; writeroom -----------------------------------------------------------------
  (defvar writeroom-global-effects '()) ;; not sure why this hack is necessary
  (use-package writeroom-mode
    ;; :defer t ;; `writeroom-mode' isn't autoloaded :(
    :custom
    (writeroom-maximize-window nil)
    (writeroom-mode-line t)
    (writeroom-global-effects (delq 'writeroom-set-fullscreen
                                    writeroom-global-effects)))

  ;; ivy/ivy-rich --------------------------------------------------------------
  (setq ivy-rich-parse-remote-buffer nil)

  (let* ((switch-buffer-configs
          (mapcar (lambda (func)
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
             (:width 40 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x
                        (ivy-rich-minibuffer-width 0.3))))))))
    (dolist (config switch-buffer-configs)
      (plist-put config :columns my-columns-config)))

  ;; themeing -----------------------------------------------------------------
  (defvar-local my/border-width 10)

  ;; also called by gnome extension via emacsclient
  (defun my/load-theme (system-appearance)
    (mapc 'disable-theme custom-enabled-themes)
    (pcase system-appearance
      ('dark (load-theme (first dotspacemacs-themes) t))
      ('light (load-theme (second dotspacemacs-themes) t))))

  (when (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions 'my/load-theme)
    (my/load-theme ns-system-appearance))

  (toggle-menu-bar-mode-from-frame -1)

  ;; fringe ---
  (spacemacs/toggle-vi-tilde-fringe-off)
  ;; hide arrows at window border for truncated lines -- not working
  ;; (define-fringe-bitmap 'left-curly-arrow (make-vector 8 #b0))
  ;; (define-fringe-bitmap 'right-curly-arrow (make-vector 8 #b0))
  ;; (define-fringe-bitmap 'right-arrow (make-vector 8 #b0))
  (fringe-mode (cons my/border-width my/border-width))

  ;; doom ---
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (load-library "lsp-treemacs-themes")  ;; https://github.com/emacs-lsp/lsp-treemacs/issues/89
  (doom-themes-treemacs-config)

  ;; borders, etc ---
  (setq window-divider-default-right-width 1
        window-divider-default-bottom-width 1)
  (menu-bar-bottom-and-right-window-divider)

  (setq ivy-posframe-border-width my/border-width
        which-key-posframe-border-width my/border-width)

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
    (if my/macos-flag  ;; fix current-line jiggle w/ doom themes
        (set-face-attribute 'line-number-current-line nil :weight 'normal))
    (window-divider-mode 1)
    (doom-modeline-invalidate-huds))

  (add-hook 'spacemacs-post-theme-change-hook 'my/do-theme-tweaks)
  (my/do-theme-tweaks)

  (add-hook
   'terraform-mode-hook
   (lambda () (set-face-foreground 'terraform--resource-name-face "hot pink")))

  (setq which-key-posframe-font "JetBrains Mono NL")

  (with-eval-after-load 'lsp-ui
    ;; (set-face-attribute 'lsp-ui-sideline-global nil :background (doom-color 'base1))
    (set-face-attribute 'lsp-ui-sideline-global nil :weight 'light)
    (set-face-attribute 'lsp-ui-sideline-global nil :slant 'italic))

  ;; doom-modeline -------------------------------------------------------------
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-hud t
        doom-modeline-percent-position nil
        doom-modeline-buffer-encoding nil
        doom-modeline-bar-width my/border-width
        doom-modeline-irc t
        doom-modeline-persp-name nil)

  (set-face-attribute 'doom-modeline-persp-name nil :inherit 'unspecified)
  (add-hook 'buffer-list-update-hook 'doom-modeline-redisplay)
  (defun doom-modeline-segment--major-mode () nil)

  ;; evil ------------------------------------------------------------------------
  ;; vi ---
  (setq evil-want-Y-yank-to-eol t)
  (evil-define-key 'visual 'global (kbd "v") 'evil-visual-line)
  (evil-define-key 'motion 'global
    (kbd "V") (kbd "C-v $"))

  ;; evil in ivy/minibuffer
  (setq evil-want-minibuffer t)

  ;; ivy ---
  (evil-define-key 'normal ivy-minibuffer-map
    [return] 'exit-minibuffer
    [escape] 'minibuffer-keyboard-quit)

  (evil-define-key '(normal insert) ivy-minibuffer-map
    (kbd "C-j") 'ivy-next-line
    (kbd "C-k") 'ivy-previous-line)

  ;; workaround for what seems to be a bug:
  ;; C-j/C-k binding doesn't work until state is changed
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (evil-normal-state)
                                     (evil-insert-state)
                                     (move-end-of-line nil)))

  ;; ex stuff ---
  ;; what about evil-ex-map? what does it do?
  (dolist (my-keymap `(,evil-ex-completion-map ,evil-ex-search-keymap))
    (evil-define-key* 'normal my-keymap
      [escape] 'minibuffer-keyboard-quit)

    (evil-define-key* '(normal insert) my-keymap
      (kbd "C-j") 'next-history-element
      (kbd "C-k") 'previous-history-element))


  ;; eval-expression, etc ---
  (evil-define-key 'normal minibuffer-local-map
    [return] 'exit-minibuffer
    [escape] 'minibuffer-keyboard-quit)

  (evil-define-key '(normal insert) minibuffer-local-map
    (kbd "C-j") 'next-history-element
    (kbd "C-k") 'previous-history-element)

  ;; misc ---
  (evil-define-key 'normal 'global (kbd "C-,") 'evil-emacs-state)
  (evil-define-key 'insert 'global (kbd "C-,") 'evil-emacs-state)
  (evil-define-key 'emacs  'global (kbd "C-,") 'evil-normal-state)

  ;; normal mode in help, warning, etc buffers
  ;; alternatively, could modify evil-evilified-state-map
  (delete 'special-mode evil-evilified-state-modes)
  ;; these special-mode-inheriting modes are exceptions
  (dolist (mode '(docker-container-mode
                  docker-volume-mode
                  docker-machine-mode
                  docker-network-mode
                  docker-image-mode))
    (add-to-list 'evil-evilified-state-modes mode))


  (evil-define-key 'normal special-mode-map "q" 'quit-window)
  (evil-define-key 'normal helpful-mode-map "q" 'kill-buffer-and-window) ;; doesn't kill buffer?

  ;; make C-k work in ivy/insert (and elsewhere, probably)
  (evil-define-key 'insert 'global (kbd "C-k") nil)

  ;; vterm ---------------------------------------------------------------------
  (evil-define-key 'emacs vterm-mode-map
    (kbd "C-k") 'evil-previous-line
    (kbd "C-j") 'evil-next-line)
  (evil-define-key '(normal insert) vterm-mode-map
    (kbd "C-k") 'vterm-send-up
    (kbd "C-j") 'vterm-send-down)
  (evil-define-key 'emacs vterm-mode-map (kbd "C-,") 'evil-normal-state)

  (setq vterm-max-scrollback 100000  ; maximum size supported
        vterm-min-window-width 65535 ; no suppress-hard-newline :(
        vterm-always-compile-module t
        vterm-clear-scrollback-when-clearing t)

  ;; haskell -------------------------------------------------------------------
  (evil-define-key '(normal insert) haskell-interactive-mode-map
    (kbd "C-j") 'haskell-interactive-mode-history-next
    (kbd "C-k") 'haskell-interactive-mode-history-previous)


  ;; ts/js ---------------------------------------------------------------
  (defun my/dap-node-enable ()
    (require 'dap-node))
  (add-hook 'js2-mode-hook 'my/dap-node-enable)
  (add-hook 'typescript-mode-hook 'my/dap-node-enable)

  (setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
  (setenv "TSC_NONPOLLING_WATCHER" "true")

  (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
    "si" 'nodejs-repl)

  ;; reduce modeline clutter
  (add-hook 'lsp-before-initialize-hook
            (lambda () (defun lsp-eslint-status-handler (foo bar) t)))


  ;; org --------------------------------------------------------------------------
  (with-eval-after-load 'org
    (require 'ox-jira)
    (org-babel-do-load-languages 'org-babel-load-languages '((scheme . t)))
    (setq org-confirm-babel-evaluate nil
          org-format-latex-options (plist-put org-format-latex-options :scale 1.2)
          org-clock-auto-clockout-timer (* 60 15)))

  (org-agenda-files (directory-files-recursively "~/org" "\.org$" nil))

  (evil-define-key 'normal 'org-mode-map (kbd "<S-return>") 'org-babel-execute-src-block)



  ;; scheme -------------------------------------------------------------------------
  (spacemacs/set-leader-keys-for-major-mode 'scheme-mode
    "gd" 'spacemacs/jump-to-definition)
  (add-hook 'scheme-mode-hook (lambda () (indent-guide-mode 1)))
  (setq geiser-repl-history-no-dups-p nil)

  ;; ;; yadm ------------------------------------------------------------------------
  ;; ;; only half works, sometimes breaks stuff
  (require 'tramp)
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
  (spacemacs/set-leader-keys "oy" 'my/magit-yadm)

  ;; c/c++ ----------------------------------------------------------------------
  (setq c-basic-offset 4)

  ;; slack ----------------------------------------------------------------------
  (require 'slack)
  (set-face-background 'slack-message-mention-face (doom-color 'base3))
  (set-face-background 'slack-message-mention-me-face (doom-color 'base3))
  (set-face-foreground 'slack-message-mention-me-face (doom-color 'magenta))
  (set-face-foreground 'slack-mrkdwn-code-face (doom-color 'violet))
  (set-face-background 'slack-mrkdwn-code-face (doom-color 'base5))
  (set-face-foreground 'slack-mrkdwn-code-block-face (doom-color 'violet))
  (set-face-background 'slack-mrkdwn-code-block-face (doom-color 'base5))

  (setq slack-render-image-p nil
        slack-prefer-current-team t
        slack-thread-also-send-to-room nil)

  (set-face-attribute 'slack-message-output-header nil
                      :underline nil
                      :weight 'bold
                      :height 1.0
                      :foreground (doom-color 'highlight))

  ;; shell-scripts -------------------------------------------------------------
  (if my/macos-flag
      (add-hook 'sh-mode-hook (lambda () (company-mode -1))))

  ;; proced -------------------------------------------------------------------
  ;; maybe should go in user-init?
  ;; pcpu and pmem don't work on mac
  (setq-default proced-format '(pid user start pcpu pmem comm args)
                proced-filter 'all)


  ;; sql -------------------------------------------------------------------
  (setq sqlfmt-executable "sql-formatter") ;; npm install sql-formatter
  (setq sqlfmt-options nil)


  ;; docker -------------------------------------------------------------------
  (defun my/docker-tramp-find-file ()
    (interactive)
    (spacemacs/counsel-find-file "/docker:"))

  (spacemacs/set-leader-keys "odf" 'my/docker-tramp-find-file)
  (spacemacs/set-leader-keys "odb" 'docker-container-shell)
  (spacemacs/set-leader-keys "odB" 'docker-container-shell-env)

  ;; yaml ---------------------------------------------------------------------
  (add-hook 'yaml-mode-hook (lambda ()
                              (spacemacs/toggle-indent-guide-on)
                              (origami-mode +1)))

  ;; symex --------------------------------------------------------------------
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
  (symex-initialize)

  ;; erc ----------------------------------------------------------------------
  (setq erc-autojoin-timing 'connect

        erc-enable-notifications t

        erc-fill-function 'erc-fill-static
        erc-fill-static-center 22

        erc-hide-list '("JOIN" "PART" "QUIT")

        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-lurker-threshold-time 43200

        erc-prompt-for-nickserv-password nil

        erc-server-reconnect-attempts 5
        erc-server-reconnect-timeout 3

        erc-status-sidebar-width 25

        erc-track-exclude-server-buffer t
        erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" "329" "332" "333" "353" "477")
        erc-track-position-in-mode-line nil
        erc-track-shorten-function nil
        erc-track-showcount t

        erc-server-list
        (if my/work-flag
            '()
          '(("irc.libera.chat"
             :nick "ees"
             :port "6697"
             :ssl t)
            ("irc.gitter.im"
             :nick "eeshugerman"
             :port "6697"
             :ssl t)))

        erc-autojoin-channels-alist
        (if my/work-flag
            '()
          '(("libera.chat" . ("#chickadee"
                              "#emacs"
                              "#guile"
                              "#guix"
                              "#haskell"
                              "#tridactyl"
                              "##politics"))
            ("gitter.im" . ("#syl20bnr/spacemacs")))))

  (add-hook 'erc-track-minor-mode-hook 'erc-status-sidebar-open)

  ;; highlight-indentation ----------------------------------------------------
  ;; this is off by default
  (setq highlight-indentation-blank-lines t)

  ;; smartparens --------------------------------------------------------------
  ;; https://github.com/Fuco1/smartparens/issues/1036
  (defun my/minibuffer-fix-sp ()
    (setq-local comment-start ";")
    (sp-local-pair 'minibuffer-pairs "'" nil :actions nil)
    (sp-local-pair 'minibuffer-pairs "`" nil :actions nil)
    (sp-update-local-pairs 'minibuffer-pairs))
  (add-hook 'eval-expression-minibuffer-setup-hook 'my/minibuffer-fix-sp)

  ;; tree-sitter ----------------------------------------------------------------
  ;; make objects foldable
  (defun my/add-javascript-folds (alist)
    (append '((object . ts-fold-range-seq)
              (template_string . ts-fold-range-seq)
              (class_body . ts-fold-range-seq))
            alist))

  (advice-add 'ts-fold-parsers-javascript :filter-return #'my/add-javascript-folds)


  ;; org clock ------------------------------------------------------------

  (require 'org-clock)
  (defvar my/org-clock-reminder-interval 300)
  (unless (boundp 'my/org-clock-reminder-timer)
    (defvar my/org-clock-reminder-timer nil))
  (defvar my/org-clock-reminder-disable nil)

  (defun my/org-clock-reminder-function ()
    (unless (or (org-clocking-p)
                (> (org-user-idle-seconds) my/org-clock-reminder-interval)
                my/org-clock-reminder-disable)
        (alert "No task clocked!"
               :never-persist t)))

  (unless (or my/org-clock-reminder-timer (not my/work-flag))
    (setq my/org-clock-reminder-timer
          (run-with-timer my/org-clock-reminder-interval
                          my/org-clock-reminder-interval
                          #'my/org-clock-reminder-function)))
)

;; misc commands --------------------------------------------------------------
(defun my/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun my/magit-kill-all ()
  (interactive)
  (kill-matching-buffers "^magit" nil t))

(defun my/echo-file-path ()
  (interactive)
  (spacemacs/echo (spacemacs--projectile-file-path)))

(defun my/browse-info ()
  (interactive)
  (info (buffer-file-name)))

(defun my/monitor-half-width ()
  (interactive)
  (set-frame-size (selected-frame) 945 1055 t))

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

;; TODO: implement as mode? deriving writeroom-mode?
(defvar my/prosey nil)

(defun my/toggle-prosey-on ()
  (interactive)
  (spacemacs/toggle-line-numbers-off) ;; doesn't always work?
  (spacemacs/toggle-relative-line-numbers-off)
  (spacemacs/toggle-truncate-lines-off)
  (spacemacs/toggle-spelling-checking-on)
  ;; (hl-line-mode -1)
  ;; (unless writeroom-mode
  ;;   (spacemacs/toggle-centered-buffer))
  (visual-line-mode +1)
  (setq my/prosey t))

(defun my/toggle-prosey-off ()
  (interactive)
  (spacemacs/toggle-line-numbers-on)
  (spacemacs/toggle-relative-line-numbers-on)
  (spacemacs/toggle-truncate-lines-on)
  (spacemacs/toggle-spelling-checking-off)
  ;; (hl-line-mode +1)
  ;; (when writeroom-mode
  ;;   (speacemacs/toggle-centered-buffer))
  (visual-line-mode -1)
  (setq my/prosey nil))

(defun my/toggle-prosey ()
  (interactive)
  (if my/prosey
    (my/toggle-prosey-off)
    (my/toggle-prosey-on)))

;; (add-hook 'text-mode-hook 'my/toggle-prosey-on)
;; (add-hook 'markdown-mode 'my/toggle-prosey-on)
;; (add-hook 'org-mode 'my/toggle-prosey-on)
