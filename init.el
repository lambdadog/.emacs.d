;;; -*- lexical-binding: t -*-
(when (bound-and-true-p esup-child-parent-log-process)
  (load-file (locate-user-emacs-file "early-init.el")))

(eval-and-compile
  (when (< emacs-major-version 29)
    (load (locate-user-emacs-file "shim/29.1.el"))))

;; TODO: split WSL off into a wsl.el to be loaded on WSL
;; Not portable, but my WSL setup is my WSL setup
(defconst is-wsl (string= "alpine-wsl" (system-name)))
(when is-wsl
  (set-face-attribute 'default nil :height 100))

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

(progn ;; epkg
  (when (>= emacs-major-version 29)
    (setc epkg-database-connector 'sqlite-builtin)))

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
  (when (file-exists-p custom-file)
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

;; Enable ligatures in programming modes
(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(global-ligature-mode 't)

(progn ;; ctrlf
  (ctrlf-mode +1))

;; I have played around with icomplete-vertical some and it's not bad
;; with decent settings, but it has some minor issues that bug me
;; enough to stick to vertico
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

(progn ;; ef-themes util
  (defun config:-ef-themes-current-variant ()
    "Gets the current ef-theme's variant"
    (if (memq (ef-themes--current-theme) ef-themes-light-themes)
	'light
      'dark))

  (defun config:-ef-themes-toggle-random (_oldfun)
    "Overrides `ef-themes-toggle' to toggle to a random theme of the
opposite variant of the current theme.

Cursed, but one of those things you do in your own config and
never share."
    (let ((new-variant (if (eq (config:-ef-themes-current-variant) 'dark)
			     'light
			   'dark)))
      (ef-themes-load-random new-variant)))

  (defun config:-ef-themes-random-in-variant (args)
    "Advises `ef-themes-load-random' to always random within the same
variant the current theme already is. With a light ef-theme,
always loads another light ef-theme and the same for dark."
    (if (null (car args))
	`(,(config:-ef-themes-current-variant))
      args))

  (advice-add #'ef-themes-toggle :around
	      #'config:-ef-themes-toggle-random)
  (advice-add #'ef-themes-load-random :filter-args
	      #'config:-ef-themes-random-in-variant))

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

;; show-paren-mode errors for some reason on Alpine WSL
(when is-wsl
  (show-paren-mode -1)

  (defun shell-command-on-str (cmd &optional str)
    "Insert result of calling CMD with STR as input.

STR is current-kill if unspecified.
"
    (interactive (list (read-shell-command "Shell command on region: ")))
    (setq str (or str
                  (current-kill 0)))
    (insert (with-temp-buffer
              (insert str)
              (shell-command-on-region (point-min) (point-max) cmd nil 'replace)
              (buffer-string))))
  (declare-function shell-command-on-str nil)

  (defun wsl-update-clip (&rest _args)
    (interactive)
    (shell-command-on-str "clip.exe"))
  (declare-function wsl-update-clip nil)

  (advice-add 'kill-new :after #'wsl-update-clip))

(with-eval-after-load 'ox
  (require 'ox-sb))
