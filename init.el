;;; -*- lexical-binding: t -*-
(when (bound-and-true-p esup-child-parent-log-process)
  (load-file (locate-user-emacs-file "early-init.el")))

(eval-and-compile
  (when (< emacs-major-version 29)
    (load (locate-user-emacs-file "shim/29.1.el"))))

;; Sketchy since `operating-system-release' is deprecated, but it
;; seems to be the only good way to detect WSL, since it get the
;; kernel's info rather than the userspace's.
(defconst is-wsl
  (eval-when-compile
    (add-to-list 'byte-compile-not-obsolete-vars
		 'operating-system-release)
    (string-match "-[Mm]icrosoft" operating-system-release)))

(when is-wsl
  (set-face-attribute 'default nil :height 90))

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

;; epkg
(when (>= emacs-major-version 29)
  (setc epkg-database-connector 'sqlite-builtin))

;; startup config
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

(defun config:-display-emacs-init-time ()
  (message (emacs-init-time "Emacs started in %f seconds")))

(add-hook 'emacs-startup-hook #'config:-trim-windows-on-startup)
(add-hook 'after-init-hook #'config:-display-emacs-init-time)

;; aesthetic
(setc window-combination-resize t)

;; no-littering
(require 'no-littering)
(no-littering-theme-backups)

;; minibuffer performance tweaks 
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
(add-hook 'minibuffer-exit-hook  #'config:-restore-gc)

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
  (load custom-file))

;; recentf
(with-eval-after-load 'recentf
  (defvar recentf-exclude)
  (add-to-list 'recentf-exclude no-littering-var-directory))

;; line numbers
(setc display-line-numbers-type 'relative
      display-line-numbers-width 3
      display-line-numbers-current-absolute nil)

(defvar display-line-numbers-mode)
(defun config:-toggle-left-margin-w-line-numbers ()
  (setq-local left-margin-width (if display-line-numbers-mode +1 -1)))

(add-hook 'display-line-numbers-mode-hook #'config:-toggle-left-margin-w-line-numbers)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; modeline
(require 'mood-line)
(mood-line-mode +1)

;; ligatures
(require 'ligature)
(ligature-set-ligatures
   'prog-mode
   '(; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
     ; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
     ; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
     ; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
     ; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
     ; Group F
     "@_" "&&" "&&&" "&=" "~@" "++" "+++" "_|_" "||"
     ; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))

(global-ligature-mode 't)

;; ctrlf
(ctrlf-mode +1)

;; vertico
(require 'vertico)
(require 'marginalia)
(vertico-mode +1)
(marginalia-mode +1)

;; eat/eshell
(setc eshell-visual-commands nil)
(defun config:-load-eat-before-eshell (&rest _)
  (eat-eshell-mode +1)
  (advice-remove #'eshell #'config:-load-eat-before-eshell))
(advice-add #'eshell :before #'config:-load-eat-before-eshell)

;; corfu
(global-corfu-mode +1)

;; ef-themes
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

;; I have to admit, I'm not actually entirely happy with this config.
(advice-add #'ef-themes-toggle :around
	    #'config:-ef-themes-toggle-random)
(advice-add #'ef-themes-load-random :filter-args
	    #'config:-ef-themes-random-in-variant)

;; magit
(with-eval-after-load 'magit
  (declare-function magit-add-section-hook "magit")
  (setc magit-module-sections-hook '(magit-insert-modules-overview)
	magit-module-sections-nested nil)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

;; allegedly improves magit performance
(when is-wsl
  (setc magit-git-executable "/usr/bin/git"))

(autoload #'magit-status-setup-buffer "magit")
(declare-function magit-status-setup-buffer "magit")

(defun config:magit-status-for-emacsd ()
  (interactive)
  (magit-status-setup-buffer user-emacs-directory))

(keymap-global-set "C-x g" #'magit-status)
(keymap-global-set "C-x G" #'config:magit-status-for-emacsd)

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

(setc indent-tabs-mode nil)

(with-eval-after-load 'zig-mode
  (add-hook 'zig-mode-hook
	    (lambda () (zig-format-on-save-mode -1))))

;; (defvar elixir-mode-hook)
;; (add-hook 'elixir-mode-hook
;; 	  (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
