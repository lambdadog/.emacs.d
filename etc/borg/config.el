(when (native-comp-available-p)
  (setq borg-compile-function #'borg-byte+native-compile))

;; TODO: experiment with `git config --local core.excludesFile <path>`
;; for a path in-repo so we don't need to manually replicate this
;; everywhere and it's possible to update without rebuilding.
(defun config/borg-add-exclude-rules (clone)
  (let ((file (expand-file-name "info/exclude" (borg-gitdir clone))))
    (message "Populating %s..." file)
    (ignore-errors
      (make-directory (file-name-directory file)))
    (with-temp-file file
      (insert "*.elc\n")
      (insert "*-autoloads.el\n")
      (insert "*.texi\n")
      (insert "*.info\n")
      (insert "dir\n"))
    (message "Populating %s...done" file)))
