;;; -*- lexical-binding: t -*-
(when (bound-and-true-p esup-child-parent-log-process)
  (load-file (locate-user-emacs-file "early-init.el")))

(eval-and-compile
  (when (or (< emacs-major-version 29)
	    (and (= emacs-major-version 29)
		 (< emacs-minor-version 1)))
    (load (locate-user-emacs-file "shim/29.1.el"))))

(eval-and-compile ;; borg
  (add-to-list 'load-path (locate-user-emacs-file "lib/borg"))
  (require 'borg)
  (load-file (locate-user-emacs-file "etc/borg/config.el"))
  (borg-initialize))

;; idea stolen from use-package. allows using customize to set
;; variables without them being saved in your custom file
;;
;; if there's a speed difference to using this over setq I haven't
;; been able to observe it
(deftheme setc)
(defmacro setc (&rest args)
  "Set each SYM to the value of its VAL using the customize
system. Uses a custom theme to avoid saving to your
`custom-file'. Works as `setq', except the value returned is nil.

The benefit of this is twofold: variables can be set with `setc'
prior to being defined with `defcustom' and they will trigger
setters correctly as opposed to `setq'.

\(fn [SYM VAL] ...)"
  (when (/= (logand (length args) 1) 0)
    (signal 'wrong-number-of-arguments (list 'setc (length args))))
  (let ((sets nil))
    (while args (push `(quote (,(pop args) ,(pop args) nil nil "Customized by setc")) sets))
    `(let ((custom--inhibit-theme-enable nil))
       (custom-theme-set-variables
	'setc
	,@(nreverse sets)))))

(progn ;; startup config
  (setc inhibit-startup-buffer-menu t
	inhibit-startup-screen t
	inhibit-startup-echo-area-message user-login-name
	initial-buffer-choice t
	initial-scratch-message "")

  (defun config:-trim-windows-on-startup ()
    "Work around emacs' persistent desire to open exactly two windows
on startup if even conceivably possible."
    ;; `count-windows' apparently doesn't work yet when this hook is
    ;; run, so we just have to try and kill scratch and ignore the
    ;; error if it fails due to being the only open window.
    ;;
    ;; `initial-buffer-choice' being set to `t' is essentially what
    ;; makes this hack work. Since *scratch* will always be one of our
    ;; windows we can just hardcode closing it and get correct
    ;; behavior.
    (ignore-errors
      (delete-window (get-buffer-window "*scratch*"))))
  (add-hook 'emacs-startup-hook #'config:-trim-windows-on-startup)

  (defun config:-display-emacs-init-time ()
    (message (emacs-init-time "Emacs started in %f seconds")))
  (add-hook 'after-init-hook #'config:-display-emacs-init-time))

(progn ;; aesthetic
  (setc window-combination-resize t))

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
    (let ((timer (timer-create)))
      (timer-set-time timer 0 nil)
      (timer-set-function timer #'config:-do-restore-gc)
      (timer-activate-when-idle timer t)))

  (add-hook 'minibuffer-setup-hook #'config:-defer-gc)
  (add-hook 'minibuffer-exit-hook  #'config:-restore-gc))

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

(progn ;; recentf
  (with-eval-after-load 'recentf
    (defvar recentf-exclude)
    (add-to-list 'recentf-exclude no-littering-var-directory)))

(progn ;; display-line-numbers
  (setc display-line-numbers-type 'relative
	display-line-numbers-width 3
	display-line-numbers-current-absolute nil)

  (defvar display-line-numbers-mode)
  (defun config:-toggle-left-margin-w-line-numbers ()
    (setq-local left-margin-width (if display-line-numbers-mode +1 -1)))
  (add-hook 'display-line-numbers-mode-hook #'config:-toggle-left-margin-w-line-numbers)

  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(progn ;; mood-line
  (require 'mood-line)
  (mood-line-mode +1))

(progn ;; fira-code-mode
  (require 'fira-code-mode)
  (setc fira-code-mode-disabled-ligatures '("x"))
  ;; https://github.com/jming422/fira-code-mode/issues/27
  ;; https://codeberg.org/akib/emacs-eat/issues/26
  (add-hook 'prog-mode-hook #'fira-code-mode))

(progn ;; ctrlf
  (ctrlf-mode +1))

(progn ;; vertico
  (require 'vertico)
  (require 'marginalia)
  (vertico-mode +1)
  (marginalia-mode +1))

(progn ;; eat/eshell
  (setc eshell-visual-commands nil)
  (defun config:-load-eat-before-eshell (&rest _)
    (eat-eshell-mode +1)
    (advice-remove #'eshell #'config:-load-eat-before-eshell))
  (advice-add #'eshell :before #'config:-load-eat-before-eshell))

;; This isn't configured *well* yet, but it's nicer to have than not.
(progn ;; corfu
  (global-corfu-mode +1))

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
  (keymap-global-set "C-x G" #'config:magit-status-for-emacsd))
