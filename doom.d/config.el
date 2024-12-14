;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Dela Anthonio"
      user-mail-address "dela.anthonio@pm.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
(setq doom-font (cond
                 ((find-font (font-spec :family "Iosevka SS04"))
                  (font-spec :family "Iosevka SS04" :size 20))
                 (t (font-spec :family "Monospace" :size 20))))
(setq doom-symbol-font doom-font)
(setq doom-variable-pitch-font (cond
                                ((find-font (font-spec :family "SF Pro"))
                                 (font-spec :family "SF Pro" :size 20))
                                ((find-font (font-spec :family "Roboto"))
                                 (font-spec :family "Roboto" :size 20))
                                (t (font-spec :family "Sans Serif" :size 20))))

(setq doom-theme 'doom-spacegrey)
(setq doom-spacegrey-brighter-comments t)
(setq doom-spacegrey-comment-bg nil)

(auto-save-visited-mode t)
(setq auto-save-visited-interval 10)
(setq confirm-kill-emacs nil)
(setq display-line-numbers-type t)
(setq evil-escape-unordered-key-sequence t)
(setq-default explicit-shell-file-name "/bin/zsh")
(global-auto-revert-mode t)
(setq-default line-spacing 3)
(setq mac-command-modifier 'meta)
(setq magit-clone-default-directory "~/Git")
(setq projectile-project-search-path '("~/Git" . 0))
(setq shell-command-switch "-ic")
(setq which-key-add-column-padding 3)
(setq which-key-max-description-length 30)

(use-package! doom-modeline
  :config
  (setq doom-modeline-height 10
        doom-modeline-bar-width 0
        doom-modeline-buffer-state-icon nil
        doom-modeline-icon nil
        doom-modeline-percent-position nil
        doom-modeline-buffer-encoding nil
        doom-modeline-check-simple-format nil
        doom-modeline-vcs-max-length 20
        doom-modeline-window-width-limit 90
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-minor-modes nil)

  (doom-modeline-def-modeline 'minimal
    '(bar matches buffer-info-simple)
    '(misc-info major-mode))

  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'minimal 'default))

  (add-hook! 'doom-modeline-mode-hook 'setup-custom-doom-modeline))

(use-package! spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 8
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 10)
        spacious-padding-subtle-mode-line nil
        )
  (spacious-padding-mode 1))

(setq +format-on-save-disabled-modes
      '(git-commit-mode
        magit-mode
        text-mode
        ))

(add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode) t)
(add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . conf-toml-mode) t)
(add-to-list 'auto-mode-alist '("zprofile\\'" . sh-mode) t)
(add-to-list 'auto-mode-alist '("\\.zshenv_local\\'" . sh-mode) t)

(defun modi/lower-case-org-keywords ()
  "Lower case Org keywords and block identifiers.

Example: \"#+TITLE\" -> \"#+title\"
         \"#+BEGIN_EXAMPLE\" -> \"#+begin_example\"

Inspiration:
https://code.orgmode.org/bzg/org-mode/commit/13424336a6f30c50952d291e7a82906c1210daf0."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      ;; Match examples: "#+FOO bar", "#+FOO:", "=#+FOO=", "~#+FOO~",
      ;;                 "‘#+FOO’", "“#+FOO”", ",#+FOO bar",
      ;;                 "#+FOO_bar<eol>", "#+FOO<eol>".
      (while (re-search-forward "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)" nil :noerror)
        (setq count (1+ count))
        (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil 1))
      (message "Lower-cased %d matches" count))))

;;; Text
(use-package! visual-fill-column
  :config
  (add-hook! (text-mode org-agenda-mode) #'visual-fill-column-mode #'visual-line-mode)
  (add-hook! (markdown-mode org-mode org-agenda-mode) (setq visual-fill-column-center-text t))
  (add-hook! (markdown-mode ) (setq visual-fill-column-width 120))
  (add-hook! (org-mode org-agenda-mode) (setq visual-fill-column-width 80))
  (setq-default visual-fill-column-width 120))

(add-hook! (markdown-mode org-mode) #'mixed-pitch-mode)
(remove-hook! (markdown-mode org-mode) #'display-line-numbers-mode)

(define-key visual-line-mode-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key visual-line-mode-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;;; Emacs Lisp
(add-hook! 'emacs-lisp-mode-hook
  (add-hook! 'before-save-hook #'+format/buffer))

;;; Git Commit
(after! git-commit
  (setq-default git-commit-summary-max-length 70))

;;; Org
(after! org
  ;; Set org directory first since other org variables use it.
  (setq org-directory "~/Org")

  (setq org-agenda-files (directory-files org-directory t "\\.org$" t)
        org-archive-location "~/Org/archive.org::* From %s"
        +org-capture-projects-file (expand-file-name "projects.org" org-directory )
        +org-capture-todo-file (expand-file-name "todo.org" org-directory )
        +org-capture-notes-file (expand-file-name "inbox.org" org-directory)
        org-columns-default-format "%25ITEM(Headline) %DEADLINE(Deadline) %EFFORT(Effort){:}"
        org-ellipsis " ⯆ "
        org-id-track-globally t
        org-log-into-drawer "LOGBOOK")

  (setq org-refile-targets `((+org-capture-notes-file . (:maxlevel . 1))
                             (+org-capture-todo-file . (:maxlevel . 1))
                             (nil . (:maxlevel . 1))
                             (,(expand-file-name "areas.org" org-directory)
                              . (:maxlevel . 1))
                             (,(expand-file-name "resources.org" org-directory)
                              . (:maxlevel . 1))
                             (+org-capture-projects-file . (:maxlevel . 1))))

  (defun transform-brackets-to-parentheses(string)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat (mapcar (lambda (c) (cond ((equal c ?\[) ?\()
                                      ((equal c ?\]) ?\))
                                      (t c))) string)))

  (setq org-capture-templates
        `(("n" "Note" entry (file +org-capture-notes-file)
           "* %?\n:PROPERTIES:\n:Created: %U\n:ID: %(org-id-uuid)\n:END:\n")
          ("j" "Journal" entry (file+datetree ,(expand-file-name "journal.org" org-directory))
           "* %U\n%?")
          ("p" "Project" entry (file+headline +org-capture-projects-file "Later")
           ,(string-join '("* PROJ %?"
                           ":PROPERTIES:"
                           ":Created: %U"
                           ":ID: %(org-id-uuid)"
                           ":END:") "\n"))
          ("t" "To-do" entry (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n:PROPERTIES:\n:Created: %U\n:ID: %(org-id-uuid)\n:END:\n")
          ("L" "Link" entry (file +org-capture-notes-file)
           "* [[%:link][%(transform-brackets-to-parentheses \"%:description\")]]\n:PROPERTIES:\n:Created: %U\n:ID: %(org-id-uuid)\n:END:\n%i\n%?")))

  (add-hook! org-mode #'+org-pretty-mode))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉")
        org-superstar-special-todo-items t
        org-superstar-todo-bullet-alist '(("TODO" . 9675)
                                          ("STRT" . 9675)
                                          ("PROJ" . 9675)
                                          ("GOAL" . 9675)
                                          ("LOOP" . 9675)
                                          ("[ ]" . 9675)
                                          ("DONE" . 9679)
                                          ("KILL" . 9679)
                                          ("[X]" . 9679))))

;;; Themes
(after! (doom-themes org)
  (custom-set-faces!
    '(org-document-title :foreground "fg" :height 1.25 :background nil :weight semi-bold)
    '(font-lock-doc-face :italic nil)
    '(org-link :inherit font-lock-constant-face :underline t)
    '(org-level-1 :height 1.1 :weight semi-bold)
    '(org-level-2 :height 1.0 :weight normal)
    '(org-level-3 :height 1.0 :weight normal)
    '(org-ellipsis :height 1.0 :foreground "#65737E"  :background nil :weight normal))

  (custom-theme-set-faces! 'doom-one-light
    '(outline-1 :weight normal)
    '(outline-2 :weight normal)
    '(outline-3 :weight normal)
    '(outline-4 :weight normal)
    '(outline-5 :weight normal)
    '(outline-6 :weight normal)
    `(font-lock-builtin-face :foreground ,(doom-color 'orange))
    `(font-lock-constant-face :foreground ,(doom-color 'blue))
    `(font-lock-function-name-face :foreground ,(doom-color 'yellow))
    `(font-lock-variable-name-face :foreground ,(doom-color 'blue))
    '(default :background "white" :weight normal)
    `(region :background ,(doom-color 'base0) :weight normal)))

;;; Color
(use-package! ansi-color
  :config
  (defun colorize-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook! 'compilation-filter-hook #'colorize-buffer))
(message "Loaded config.el")
