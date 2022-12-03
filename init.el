(when (and (boundp 'esup-child-parent-log-process)
	   esup-child-parent-log-process)
  (load-file (locate-user-emacs-file "early-init.el")))

(eval-and-compile ;; borg
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(with-eval-after-load 'magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))
(keymap-global-set "C-x g" #'magit-status)
