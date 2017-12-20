;;; flycheck-checkpatch.el --- Integrate checkpatch into flycheck

;; Copyright © 2017 Dela Anthonio

;; Author: Dela Anthonio <dell.anthonio@gmail.com>
;; Homepage: https://github.com/beta1440/flycheck-checkpatch
;; Keywords: kernel, linux, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (flucj))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; On-the-fly linting for Linux kernel source files.

;; Setup:

;; (require 'flycheck-checkpatch)

;;; Code:

(require 'flycheck)

(flycheck-def-option-var flycheck-checkpatch-strict t checkpatch
  "Whether to enable strict erros."
  :type 'boolean
  :safe 'booleanp)

(flycheck-def-config-file-var flycheck-checkpatchconf checkpatch ".checkpatch.conf")

  (flycheck-define-checker checkpatch
    "A checker using checkpatch.

See URL `https://github.com/torvalds/linux/blob/master/scripts/checkpatch.pl'"
    :command ("checkpatch.pl"
              "--file"
              "--terse"
              "--color=never"
              "--mailback"
              "--no-summary"
              (option-flag "--strict" flycheck-checkpatch-strict)
              source)
    :error-patterns
    ((error line-start  (file-name) ":" line ": ERROR: " (message) line-end)
     (warning line-start (file-name) ":" line ": WARNING: " (message) line-end))
    :modes (c-mode kconfig-mode)
    )

;;;###autoload
(add-to-list 'flycheck-checkers 'checkpatch)

(provide 'flycheck-checkpatch)

;;; flycheck-checkpatch.el ends here
