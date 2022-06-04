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

;;; Text
(use-package! visual-fill-column
  :config
  (add-hook! (text-mode prog-mode org-agenda-mode) (visual-fill-column-mode visual-line-mode))
  (add-hook! (markdown-mode org-mode org-agenda-mode) (setq visual-fill-column-center-text t
                                                            visual-fill-column-width 80))
  (setq-default visual-fill-column-width 100))

(add-hook! (markdown-mode org-mode) #'mixed-pitch-mode)
(remove-hook! (markdown-mode org-mode) #'display-line-numbers-mode)

(define-key visual-line-mode-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key visual-line-mode-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;;; General Settings
(setq projectile-project-search-path '("~/Git")
      shell-command-switch "-ic")

(auto-save-visited-mode t)
(setq auto-save-visited-interval 10)
(setq evil-escape-unordered-key-sequence t)
(setq-default line-spacing 3)
(global-auto-revert-mode t)
(setq git-commit-summary-max-length 68)
(setq mac-command-modifier 'meta)
(setq confirm-kill-emacs nil)
(setq-default explicit-shell-file-name "/bin/zsh")
(setq which-key-add-column-padding 3)
(setq which-key-max-description-length 30)


;;; Emacs Lisp
(add-hook! 'emacs-lisp-mode-hook
  (add-hook! 'before-save-hook #'+format/buffer))

;;; Shell
(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode) t)
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

;;; SQL
(use-package! sqlformat
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("--keyword-case" "2" "--type-case" "1" "--spaces" "2" "-g"))
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

;;; PlantUML files
(after! plantuml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq plantuml-default-exec-mode 'executable)
  (after! org-mode
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (setq org-plantuml-exec-mode 'plantuml)))

;;; Org
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
                                          (propertize "${doom-tags:42}" 'face 'org-tag)))
  (map! :leader
        (:prefix-map ("r" . "roam")
         (:when (featurep! :lang org +roam2)
          :desc "Open random node"           "a" #'org-roam-node-random
          :desc "Find node"                  "f" #'org-roam-node-find
          :desc "Find ref"                   "F" #'org-roam-ref-find
          :desc "Show graph"                 "g" #'org-roam-graph
          :desc "Insert node"                "i" #'org-roam-node-insert
          :desc "Capture to node"            "n" #'org-roam-capture
          :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
          :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
          :desc "Sync database"              "s" #'org-roam-db-sync
          :desc "Goto today"                 "t" #'org-roam-dailies-goto-today
          :desc "Capture today"              "T" #'org-roam-dailies-capture-today
          (:prefix ("d" . "by date")
           :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
           :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
           :desc "Capture date"              "D" #'org-roam-dailies-capture-date
           :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
           :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
           :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
           :desc "Capture today"             "n" #'org-roam-dailies-capture-today
           :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
           :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
           :desc "Find directory"            "-" #'org-roam-dailies-find-directory)))))

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


;;; Themes
(after! (doom-themes org)
  (custom-set-faces!
    '(org-document-title :foreground "fg" :height 1.25 :background nil :weight semi-bold)
    '(font-lock-doc-face :italic nil)
    '(org-link :inherit font-lock-constant-face :underline t)
    '(org-level-1 :height 1.1 :weight semi-bold)
    '(org-level-2 :height 1.0 :weight normal)
    '(org-level-3 :height 1.0 :weight normal)
    '(font-lock-constant-face :weight bold)
    '(org-ellipsis :height 1.0 :foreground "#65737E"  :background nil :weight normal)))

;; protobuf
(use-package! protobuf-mode)
