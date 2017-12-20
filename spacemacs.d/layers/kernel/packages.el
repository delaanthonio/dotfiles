;;; packages.el --- Kernel layer packages file for Spacemacs
;;
;; Copyright (c) 2017 Dela Anthonio & Contributors
;;
;; Author: Dela Anthonio <dell.anthonio@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst kernel-packages
  '(
    company
    flycheck
    (flycheck-checkpatch :location local :requires flycheck)
    (kconfig-mode :location local)
    ))

(defun kernel/post-init-flycheck()
  ;; (spacemacs/enable-flycheck 'kconfig-mode)
  (add-hook 'kconfig-mode-hook 'flycheck-mode)
  )

(defun kernel/init-kconfig-mode ()
  "Initialize kconfig-mode"
  (use-package kconfig-mode
    :defer t
    :commands kconfig-mode
    :mode "Kconfig.*"
    )
  )

(defun kernel/init-flycheck-checkpatch ()
  (use-package flycheck-checkpatch
    :defer t)
  )

(defun kernel/post-init-company ()
  (add-hook 'kconfig-mode-hook 'company-mode)
  )
