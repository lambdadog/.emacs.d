(when (bound-and-true-p esup-child-parent-log-process)
  (load-file (locate-user-emacs-file "early-init.el")))

(eval-and-compile ;; borg
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

;; utility stolen from use-package. allows using custom to set
;; variables without them being saved in your custom file
(deftheme config)
(enable-theme 'config)
(setq custom-enabled-themes (remq 'config custom-enabled-themes))

;; magit
(with-eval-after-load 'magit
  (declare-function magit-add-section-hook "magit")
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(autoload #'magit-status-setup-buffer "magit")
(declare-function magit-status-setup-buffer "magit")

(defun config:magit-status-for-emacsd ()
  (interactive)
  (magit-status-setup-buffer user-emacs-directory))

(keymap-global-set "C-x g" #'magit-status)
(keymap-global-set "C-x C-g" #'config:magit-status-for-emacsd)
