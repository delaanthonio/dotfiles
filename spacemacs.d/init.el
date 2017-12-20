;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '((auto-completion :variables
                      auto-completion-complete-with-key-sequence "fd"
                      auto-completion-complete-with-key-sequence-delay 0.2
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-private-snippets-directory nil
                      spacemacs-default-company-backends '(company-files company-capf))
     better-defaults
     (c-c++ :variables c-c++-enable-clang-support t :packages (not cmake-ide))
     emacs-lisp
     git
     (github :packages (not magithub))
     (gtags :variables
            helm-gtags-use-input-at-cursor t
            helm-gtags-display-style 'detail
            helm-gtags-auto-update nil
            helm-gtags-maximum-candidates 10
            :disabled-for emacs-lisp)
     helm
     html
     latex
     (markdown :variables markdown-live-preview-engine 'vmd)
     (python :variables
             python-test-runner 'pytest
             python-enable-yapf-format-on-save t
             python-sort-imports-on-save t
             python-fill-column 79
             python-indent-offset 4)
     org
     semantic
     (shell-scripts :packages (not fish-mode))
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     systemd
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(all-the-icons
                                      flycheck-mypy
                                      flycheck-package
                                      pkgbuild-mode
                                      python-docstring
                                      suggest
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Safterpacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 5))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro for Powerline"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source setftings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; if non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; this can be temporary disabled by pressing `c-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; select a scope to highlight delimiters. possible values are `any',
   ;; `current', `all' or `nil'. default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; if non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; list of search tool executable names. spacemacs uses the first installed
   ;; tool of the list. supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; the default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; delete whitespace while saving buffer. possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  "initialization function for user code.
it is called immediately after `dotspacemacs/init', before layer configuration
executes.
 this function is mostly useful for variables that need to be set
before packages are loaded. if you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq exec-path-from-shell-check-startup-files nil)

  ;; https://github.com/syl20bnr/spacemacs/issues/2705
  (setq tramp-ssh-controlmaster-options
        "-o controlmaster=auto -o controlpath='tramp.%%c' -o controlpersist=no")
  )

(defun dotspacemacs/user-config ()
  "configuration function for user code.
this function is called at the very end of spacemacs initialization after
layers configuration.
this is the place where most of your configurations should be done. unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; (define-aliases)

  (setq-default
   c-basic-offset 4
   compilation-ask-about-save nil

   ;; js2-basic-offset 2
   ;; js-indent-level 2

   ;; git-magit-status-fullscreen t

   indent-tabs-mode nil
   powerline-default-separator 'slant
   tab-width  4

   magit-repository-directories '("~/Projects/")

   helm-gtags-minor-mode-name 'HGT)

  (setq shell-file-name "/usr/bin/zsh")

  (setq neo-theme
        (if (display-graphic-p)
            'icons 'arrow))

  ;; compilation-mode
  (add-to-list 'auto-mode-alist '("\\.log\\'" . compilation-mode) t)

  ;; unix
  (add-to-list 'auto-mode-alist '("\\defconfig\\'" . conf-unix-mode) t)

  ;; init.*.rc files
  (add-to-list 'auto-mode-alist '("\\.rc\\'" . conf-unix-mode) t)

  ;; .cmd files
  (add-to-list 'auto-mode-alist '("\\.cmd\\'" . makefile-mode) t)

  ;; clang-format
  (add-to-list 'auto-mode-alist '("\\.clang-format\\'" . yaml-mode) t)

  ;; env
  (add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode) t)

  ;; arch linux package build mode
  (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)

  ;; company
  (setq-default
   company-idle-delay 0.4
   company-show-numbers t
   company-minimum-prefix-length 2
   company-candidates-length 10)

  ;; (add-hook 'after-init-hook 'company-global-mode)

  ;; evil escape
  (setq-default
   evil-escape-delay 0.2
   evil-escape-key-sequence "fd"
   evil-escape-unordered-key-sequence t)

  ;; c-mode

  (defun c-c++/clang-format-line-or-region (&optional style)
    "Format the current line or region with clang-format according to STYLE."
    (interactive)
    (save-excursion
      (progn
        (if (region-active-p)
            (clang-format (region-beginning) (region-end) style)
          (message "Formatted region"))
        (progn
          (clang-format (line-beginning-position) (line-end-position) style)
          (message "Formatted line %s" (line-number-at-pos))))))

  (defun c-c++/clang-format-function (&optional style)
    "Format the current function with clang-format according to STYLE."
    (interactive)
    (save-excursion
      (c-mark-function)
      (clang-format (region-beginning) (region-end) style)
      (deactivate-mark)
      (message "Formatted function %s" (c-defun-name))))

  (defun c-c++/clang-format-buffer (&optional style)
    "Format the current buffer with clang-format according to STYLE."
    (interactive)
    (clang-format (point-min) (point-max) style)
    (message "Formatted buffer %s" (buffer-name)))

  (dolist (mode '(c-mode c++-mode))
    ;; (spacemacs/declare-prefix-for-mode mode "m=" "format")
    ;; (spacemacs/set-leader-keys-for-major-mode mode "==" 'c-c++/clang-format-line-or-region)
    ;; (spacemacs/set-leader-keys-for-major-mode mode "=b" 'c-c++/clang-format-buffer)
    ;; (spacemacs/set-leader-keys-for-major-mode mode "=f" 'c-c++/clang-format-function)
    (spacemacs/set-leader-keys-for-major-mode mode "gb" 'c-beginning-of-defun)
    (spacemacs/set-leader-keys-for-major-mode mode "ge" 'c-end-of-defun)
    (spacemacs/set-leader-keys-for-major-mode mode "tn" 'c-toggle-auto-newline)
    (spacemacs/set-leader-keys-for-major-mode mode "ts" 'c-toggle-syntactic-indentation)
    (spacemacs/set-leader-keys-for-major-mode mode "te" 'c-toggle-electric-state)
    (spacemacs/set-leader-keys-for-major-mode mode "th"'c-toggle-hungry-state)
    (spacemacs/set-leader-keys-for-major-mode mode "tH" 'c-toggle-auto-hungry-state))

  ;; prettify

  (defun init-prog-mode-alists ()
    "Add pretty symbols to prog mode"
    (add-to-list 'prettify-symbols-alist '("<=" . ?≤))
    (add-to-list 'prettify-symbols-alist '(">=" . ?≥))
    (add-to-list 'prettify-symbols-alist '("<-" . ?←))
    (add-to-list 'prettify-symbols-alist '("->" . ?→))
    (add-to-list 'prettify-symbols-alist '("!=" . ?≠))
    (add-to-list 'prettify-symbols-alist '("::" . ?∷))
    (add-to-list 'prettify-symbols-alist '("..." . ?…))
    (add-to-list 'prettify-symbols-alist '("--" . ?╌))
    )

  (dolist (hook '(c-mode-hook emacs-lisp-mode-hook))
    (add-hook hook 'init-prog-mode-alists)
    )

  ;; evil
  (evil-ex-define-cmd "cl" 'spacemacs/comment-or-uncomment-lines)
  (evil-ex-define-cmd "dp" 'delete-pair)
  (evil-ex-define-cmd "ks" 'kill-sexp)
  (evil-ex-define-cmd "kp" 'kill-paragraph)
  (evil-ex-define-cmd "mp" 'mark-paragraph)

  ;; flycheck-package
  (eval-after-load 'flycheck
    '(flycheck-package-setup))

  ;; web
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq-local web-mode-markup-indent-offset 2)
    )
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  (add-hook 'compilation-mode-hook 'read-only-mode )

  ;; avy
  avy-all-windows 'all-frames

  ;; Elisp
  (use-package suggest
    :defer t)

  ;; (add-hook 'emacs-lisp-mode-hook 'user-init-emacs-lisp-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

  ;; python
  (use-package flycheck-mypy
    :defer nil)

  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-hook 'python-mode-hook 'python-docstring-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

  ;; markdown
  (add-hook 'text-mode
            (lambda()
              (set-fill-column 70)))
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook
            (lambda()
              (spacemacs/toggle-fill-column-indicator-on)
              (set-fill-column 70)))

  ;; smartparens
  (global-set-key (kbd "C-k") 'sp-kill-hybrid-sexp)
  (global-set-key (kbd "C-M-k") 'sp-kill-sexp)
  (global-set-key (kbd "M-d") 'sp-kill-word)
  (global-set-key (kbd "M-k") 'kill-whole-line)

  ;; anzu
  (global-set-key (kbd "C-M-r") 'anzu-query-replace-at-cursor)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)

  ;; helm-swoop
  (eval-after-load 'helm-swoop
    (lambda()
      (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
      (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
      (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
      (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)))

  ;; key bindings

  (global-set-key [f5] 'spacemacs/safe-revert-buffer)
  (global-set-key [f6] 'recompile)
  (global-set-key [M-down] 'move-text-down)
  (global-set-key (kbd "M-n") 'move-text-down)
  (global-set-key [M-up]   'move-text-up)
  (global-set-key (kbd "M-p")   'move-text-up)
  (global-set-key (kbd "<C-tab>")   'next-buffer)
  (global-set-key (kbd "<C-iso-lefttab>")   'previous-buffer)
  (global-set-key (kbd "C-x C-b")'helm-mini)
  (global-set-key (kbd "C-s") 'helm-swoop)
  (global-set-key (kbd "M-.") 'spacemacs/jump-to-definition)
  (global-set-key (kbd "M-RET") 'spacemacs/duplicate-line-or-region)
  (global-set-key (kbd "C-x C-k") 'kill-paragraph)
  (global-set-key (kbd "C-r") 'anzu-query-replace-at-cursor)
  (global-set-key (kbd "C-M-w") 'other-window)
  (global-set-key (kbd "C-;") 'spacemacs/comment-or-uncomment-lines)
  (global-set-key (kbd "C-M-;") 'spacemacs/comment-or-uncomment-paragraphs)
  )

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
