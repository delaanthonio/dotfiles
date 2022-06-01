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
(setq doom-font (font-spec :family "Iosevka SS04" :size 20 :weight 'normal))
(setq doom-unicode-font doom-font)
(setq doom-variable-pitch-font (cond
                                ((find-font (font-spec :family "SF Pro Text"))
                                 (font-spec :family "SF Pro Text" :size 20))
                                ((find-font (font-spec :family "Roboto"))
                                 (font-spec :family "Roboto" :size 20))
                                (t (font-spec :family "Sans Serif" :size 20))))

(setq doom-theme 'doom-spacegrey)
(setq doom-spacegrey-brighter-comments t)
(setq doom-spacegrey-comment-bg nil)
(setq display-line-numbers-type t)

;; text
(use-package! visual-fill-column
  :config
  (add-hook! (text-mode prog-mode org-agenda-mode)
             '(visual-fill-column-mode visual-line-mode))
  (add-hook! (text-mode org-agenda-mode) (setq visual-fill-column-center-text t))
  (setq-default visual-fill-column-width 120))

(add-hook! (markdown-mode org-mode) #'mixed-pitch-mode)
(remove-hook! (markdown-mode org-mode) #'display-line-numbers-mode)

;; general settings
(setq projectile-project-search-path '("~/Git")
      shell-command-switch "-ic")

(auto-save-visited-mode t)
(setq auto-save-visited-interval 10)
(setq-default line-spacing 3)
(global-auto-revert-mode t)
(setq git-commit-summary-max-length 68)
(setq mac-command-modifier 'meta)
(setq confirm-kill-emacs nil)
(setq-default explicit-shell-file-name "/bin/zsh")
(setq which-key-add-column-padding 3)
(setq which-key-max-description-length 30)

;; evil
(setq evil-escape-unordered-key-sequence t)

(define-key visual-line-mode-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key visual-line-mode-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

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

;; org
(after! org
  (setq org-directory "~/Dropbox/Org/")

  (setq org-agenda-files (directory-files org-directory t "\\.org$" t)
        org-archive-location "::* Archive"
        +org-capture-projects-file (concat org-directory "projects.org")
        +org-capture-todo-file (concat org-directory "todo.org")
        +org-capture-notes-file (concat org-directory "inbox.org")
        org-columns-default-format "%25ITEM(Headline) %DEADLINE(Deadline) %EFFORT(Effort){:}"
        org-ellipsis " ⯆ "
        org-id-track-globally t
        org-log-into-drawer "LOGBOOK")

  (setq org-refile-targets '((+org-capture-notes-file . (:maxlevel . 1))
                             (+org-capture-todo-file . (:maxlevel . 1))
                             (+org-capture-projects-file . (:maxlevel . 1))))

  (defun transform-brackets-to-parentheses(string)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat (mapcar (lambda (c) (cond ((equal c ?\[) ?\()
                                      ((equal c ?\]) ?\))
                                      (t c))) string)))

  (setq org-capture-templates
        `(("n" "Note" entry (file +org-capture-notes-file)
           "* %?\n:PROPERTIES:\n:Created: %U\n:ID: %(org-id-uuid)\n:END:\n")
          ("p" "Project" entry (file+headline +org-capture-projects-file "Backlog")
           ,(string-join '("* PROJ %?"
                           ":PROPERTIES:"
                           ":Created: %U"
                           ":ID: %(org-id-uuid)"
                           ":END:"
                           ":Meta:"
                           "- Area ::"
                           "- Goal ::"
                           ":END:") "\n"))
          ("t" "To-do" entry (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n:PROPERTIES:\n:Created: %U\n:ID: %(org-id-uuid)\n:END:\n")
          ("L" "Link" entry (file +org-capture-notes-file)
           "* [[%:link][%(transform-brackets-to-parentheses \"%:description\")]]\n:PROPERTIES:\n:Created: %U\n:ID: %(org-id-uuid)\n:END:\n%i\n%?")))

  (add-hook! org-mode #'+org-pretty-mode))

(after! org-roam
  (setq
   org-roam-capture-templates '(("d" "default" plain "%?" :target
                                 (file+head "roam/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                 :unnarrowed t))
   org-roam-dailies-directory "daily/"
   org-roam-dailies-capture-templates '(("d" "default" entry
                                         "* %U\n%?"
                                         :target (file+head "%<%Y-%m-%d>.org"
                                                            "#+title: %<%Y-%m-%d>\n")))
   org-roam-directory org-directory
   org-roam-node-display-template (format "${doom-hierarchy:50} %s %s"
                                          (propertize "${doom-type:12}" 'face 'font-lock-keyword-face)
                                          (propertize "${doom-tags:42}" 'face 'org-tag))))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("■")
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


;; protobuf
(use-package! protobuf-mode)
