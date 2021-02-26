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
(setq doom-font (font-spec :family "Iosevka SS04" :size 22 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Roboto" :size 22 :weight 'normal))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

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

;; vterm
(setq vterm-buffer-name-string "vterm %s")

;; org
(add-hook! 'text-mode-hook
           #'mixed-pitch-mode
           (lambda ()
             (setq display-line-numbers nil)))

(setq org-directory "~/Dropbox/Org/"
      org-log-into-drawer "LOGBOOK"
      org-projectile-projects-file (concat org-directory "para.org")
      org-roam-directory (concat org-directory "roam/")
      org-superstar-headline-bullets-list '("●")
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
(use-package! org-brain
  :config
  (map! :map org-mode-map
        :leader
        (:prefix ("n" . "notes")
         :desc "Org Brain" "V" #'org-brain-visualize))
  (setq org-brain-path org-directory))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n")))

(use-package! doct
  :config
  (defun transform-brackets-to-parentheses(string)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat (mapcar (lambda (c) (cond ((equal c ?\[) ?\()
                                      ((equal c ?\]) ?\))
                                      (t c))) string)))
  (setq org-capture-templates
        (doct '(("Note"
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
                            "%?"))))))
