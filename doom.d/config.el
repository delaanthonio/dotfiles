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
(setq doom-font (font-spec :family "Iosevka SS04" :size 20 :weight 'normal)
      doom-variable-pitch-font
      (cond
       ((eq system-type 'gnu/linux) (font-spec :family "Roboto" :size 22 :weight 'normal))
       (t (font-spec :family "Sans Serif" :size 22 :weight 'normal))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-spacegrey)
(setq doom-spacegrey-brighter-comments t)
(setq doom-spacegrey-comment-bg nil)

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
  :config
  (add-hook! (text-mode prog-mode org-agenda-mode)
             '(visual-fill-column-mode visual-line-mode))
  (add-hook! text-mode  (setq visual-fill-column-width 150))
  (add-hook! (text-mode org-agenda-mode) (setq visual-fill-column-center-text t)))

(setq-default visual-fill-column-width 100)

(add-hook 'markdown-mode-hook #'mixed-pitch-mode)
(add-hook 'org-mode-hook #'mixed-pitch-mode)
(remove-hook 'markdown-mode-hook #'display-line-numbers-mode)
(remove-hook 'org-mode-hook #'display-line-numbers-mode)

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
      shell-command-switch "-ic")

(auto-save-visited-mode t)
(setq-default line-spacing 3)
(global-auto-revert-mode t)
(setq mac-command-modifier 'meta)
(setq confirm-kill-emacs nil)

;; evil
(setq evil-escape-unordered-key-sequence t)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; emacs lisp
(add-hook! 'emacs-lisp-mode-hook
  (add-hook 'before-save-hook #'+format/buffer))

;; sh
(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . java-mode) t)
(add-to-list 'auto-mode-alist '("poetry.lock" . conf-toml-mode) t)
(add-to-list 'auto-mode-alist '("zshrc\\'" . sh-mode) t)
(add-to-list 'auto-mode-alist '("zshenv\\'" . sh-mode) t)
(add-to-list 'auto-mode-alist '("zprofile\\'" . sh-mode) t)
(add-to-list 'auto-mode-alist '("\\.zshenv_local\\'" . sh-mode) t)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"
                                          ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; sql
(use-package! sqlformat
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("--keyword-case" "2" "--type-case" "1" "--spaces" "2" "-g"))
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

;; Enable plantuml-mode for PlantUML files
(after! plantuml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq plantuml-default-exec-mode 'executable)
  (after! org-mode
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (setq org-plantuml-exec-mode 'plantuml)))

(use-package! mermaid-mode
  :config
  (add-to-list 'org-src-lang-modes '("mermaid" . mermaid)))

(use-package! ob-mermaid
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc"))

;; org
(setq org-directory "~/Dropbox/Org/"
      org-log-into-drawer "LOGBOOK"
      org-roam-directory (concat org-directory "roam/")
      org-id-track-globally t)

(add-hook 'org-mode-hook #'+org-pretty-mode)

(setq org-refile-targets '((+org-capture-notes-file . (:maxlevel . 2))
                           (+org-capture-todo-file . (:maxlevel . 2))))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(defun transform-brackets-to-parentheses(string)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat (mapcar (lambda (c) (cond ((equal c ?\[) ?\()
                                    ((equal c ?\]) ?\))
                                    (t c))) string)))

(setq org-capture-templates
      '(("n" "Note" entry (file+headline +org-capture-notes-file "Inbox")
         "* %U \n%?")
        ("t" "To-do" entry (file+headline +org-capture-todo-file "Inbox")
         "* TODO %?")
        ("L" "Link" entry (file+headline +org-capture-notes-file "Inbox")
         "* [[%:link][%(transform-brackets-to-parentheses \"%:description\")]]\n:PROPERTIES:\n:Created: %U\n:END:\n%i\n%?")))
