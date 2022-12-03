;;; -*- lexical-binding: t -*-
(when (bound-and-true-p esup-child-parent-log-process)
  (load-file (locate-user-emacs-file "early-init.el")))

(eval-and-compile ;; borg
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

;; idea stolen from use-package. allows using customize to set
;; variables without them being saved in your custom file
(deftheme config)
(enable-theme 'config)
(setq custom-enabled-themes (remq 'config custom-enabled-themes))

(defmacro setc (&rest args)
  "Set each SYM to the value of its VAL using the customize
system. Uses a custom theme to avoid saving to your
`custom-file'. Works as `setq', except the value returned is nil.

The benefit of this is that variables can be set before they are
defined with defcustom without issues.

\(fn [SYM VAL] ...)"
  (when (/= (logand (length args) 1) 0)
    (signal 'wrong-number-of-arguments (list 'setc (length args))))
  (let ((sets nil))
    (while args (push `(list ',(pop args) ,(pop args) nil nil "Customized by setc") sets))
    `(let ((custom--inhibit-theme-enable nil))
       ,(append
	 '(custom-theme-set-variables
	   'config)
	 (nreverse sets)))))

(progn ;; no-littering
  (require 'no-littering)
  (setc auto-save-file-name-transforms
     `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(progn ;; minibuffer performance tweaks 
  (let ((restore:gc-cons-threshold gc-cons-threshold))
    (defun config:-do-restore-gc ()
      (setq gc-cons-threshold restore:gc-cons-threshold)))

  (declare-function config:-do-restore-gc nil)
  (defun config:-defer-gc ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun config:-restore-gc ()
    (run-at-time 1 nil #'config:-do-restore-gc))

  (add-hook 'minibuffer-setup #'config:-defer-gc)
  (add-hook 'minibuffer-exit  #'config:-restore-gc))

(progn ;; customize
  ;; disable user-facing customize interface
  ;; https://github.com/doomemacs/doomemacs/blob/35865ef5e89442e3809b8095199977053dd4210f/core/core-ui.el#L628-L639
  (dolist (sym '(customize-option customize-browse customize-group customize-face
				  customize-rogue customize-saved customize-apropos
				  customize-changed customize-unsaved customize-variable
				  customize-set-value customize-customized customize-set-variable
				  customize-apropos-faces customize-save-variable
				  customize-apropos-groups customize-apropos-options
				  customize-changed-options customize-save-customized
				  customize-themes))
    (put sym 'disabled nil))

  ;; `custom-file' is still used internally by emacs to store some data,
  ;; even though we've disabled all of its user-facing functions.
  (setc custom-file (no-littering-expand-etc-file-name "custom.el"))
  (while (file-exists-p custom-file)
    (load custom-file)))

(progn ;; magit
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
  (keymap-global-set "C-x C-g" #'config:magit-status-for-emacsd))
