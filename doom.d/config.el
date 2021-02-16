;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Dela Anthonio"
      user-mail-address "dell.anthonio@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka" :size 22)
      doom-variable-pitch-font (font-spec :family "sans" :size 22))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; text
(use-package! visual-fill-column
  :hook ((text-mode . visual-fill-column-mode)
         (text-mode . visual-line-mode))
  :config (setq-default visual-fill-column-width 100
                        visual-fill-column-center-text t
                        line-spacing 3))

;; Set default browser for Windows
(when (and (eq system-type 'gnu/linux)
           (string-match ".*microsoft.*" (shell-command-to-string "uname -a")))
  (defun browse-url-wsl (url &optional ignored)
    "Open a URL using the default Windows Browser"
    (interactive (browse-url-interactive-arg "URL: "))
    (shell-command-to-string (concat "explorer.exe " url)))
  (setq browse-url-browser-function #'browse-url-wsl))

;; general settings
(setq projectile-project-search-path '("~/Git")
      recentf-auto-cleanup 120
      shell-command-switch "-ic")

(global-auto-revert-mode t)

;; direnv
(use-package direnv
  :config
  (direnv-mode))

;; evil
(setq evil-escape-unordered-key-sequence t)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; javascript
(setq flycheck-javascript-standard-executable "/usr/bin/standardx")

(use-package! js-react-redux-yasnippets)

(after! js2-mode
  (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet))

(use-package prettier
  :ensure t
  :hook (js-mode . prettier-mode))

;; python
(add-hook! 'python-mode-hook
           (lambda() (add-hook! 'before-save-hook #'py-isort-before-save)))

;; emacs lisp
(add-hook! 'emacs-lisp-mode-hook
  (add-hook 'before-save-hook #'+format/buffer))

;; sh
(add-to-list 'auto-mode-alist '("zshrc\\'" . sh-mode) t)
(add-to-list 'auto-mode-alist '("zshenv\\'" . sh-mode) t)
(add-to-list 'auto-mode-alist '("zprofile\\'" . sh-mode) t)
(add-to-list 'auto-mode-alist '("\\.zshenv_local\\'" . sh-mode) t)

;; org
(setq org-directory "~/Dropbox/Org/"
      org-log-into-drawer "LOGBOOK"
      org-projectile-projects-file (concat org-directory "para.org")
      org-id-track-globally t
      org-id-locations-file (concat user-emacs-directory "org-id-locations"))

(defcustom org-inbox-file (concat org-directory "inbox.org")
  "Org inbox file."
  :type 'string)

(defcustom org-para-file (concat org-directory "para.org")
  "Org PARA file."
  :type 'string)

(defcustom org-journal-file (concat org-directory "journal.org")
  "Org Journal file."
  :type 'string)

(setq org-refile-targets '((org-para-file . (:maxlevel . 2))
                           ((concat org-directory "archives.org") . (:maxlevel . 2))))
(setq org-global-properties '(("Effort_ALL" . "0 0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00 3:00")))
(setq org-columns-default-format "%3PRIORITY %TODO %25ITEM %20Effort{:} %CLOCKSUM %TAGS")

(use-package! org-brain
  :config
  (map! :map org-mode-map
        :leader
        (:prefix ("n" . "notes")
         :desc "Org Brain" "V" #'org-brain-visualize))
  (setq org-brain-path org-directory))

(use-package! doct
  :config
  (defun transform-brackets-to-parentheses(string)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat (mapcar (lambda (c) (cond ((equal c ?\[) ?\()
                                      ((equal c ?\]) ?\))
                                      (t c))) string)))
  (setq org-capture-templates
        (doct '(("Journal"
                 :keys "j"
                 :file  org-journal-file
                 :template ("* %^{Description}"
                            ":PROPERTIES:"
                            ":Created: %U"
                            ":END:"
                            "%?")
                 :datetree t)
                ("Note"
                 :keys "n"
                 :file  org-inbox-file
                 :template ("* %^{Description}"
                            ":PROPERTIES:"
                            ":Created: %U"
                            ":END:"
                            "%?"))
                ("Protocol"
                 :keys "p"
                 :file org-inbox-file
                 :template ("* [[%:link][%(transform-brackets-to-parentheses \"%:description\")]]"
                            ":PROPERTIES:"
                            ":Created: %U"
                            ":END:"
                            "%i"
                            "%?"))
                ("Protocol Link"
                 :keys "L"
                 :file org-inbox-file
                 :template ("* [[%:link][%(transform-brackets-to-parentheses \"%:description\")]]"
                            ":PROPERTIES:"
                            ":Created: %U"
                            ":END:"
                            "%?"))
                ("Todo"
                 :keys "t"
                 :file org-inbox-file
                 :template ("* TODO %^{Description}"
                            ":PROPERTIES:"
                            ":Created: %U"
                            ":END:"
                            "%?"))))))
