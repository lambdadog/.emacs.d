;;; -*- lexical-binding: t -*-
;; loading early-init.el changes some properties of my load process,
;; so make sure esup loads it if we're profiling startup
(when (or (and (boundp 'esup-child-parent-log-process)
	       esup-child-parent-log-process)
	  (< emacs-major-version 27))
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(eval-and-compile ;; borg
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(require 'cl-lib)
(require 's)

(eval-and-compile ;; use-package
  (require 'use-package)
  (setq use-package-verbose t)
  (setq use-package-always-defer t))

(use-package no-littering
  :demand t
  :custom
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package minibuffer
  :no-require t
  :hook
  (minibuffer-setup . lambdadog:defer-gc)
  (minibuffer-exit . lambdadog:restore-gc)
  :config
  (defun lambdadog:defer-gc ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun lambdadog:restore-gc ()
    (run-at-time
     1 nil
     (defun lambdadog:-do-restore-gc ()
       (setq gc-cons-threshold 16777216)))))

(use-package custom
  :after (no-littering)
  :no-require t
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  ;; disable customize
  ;; https://github.com/doomemacs/doomemacs/blob/35865ef5e89442e3809b8095199977053dd4210f/core/core-ui.el#L628-L639
  (dolist (sym '(customize-option customize-browse customize-group customize-face
				  customize-rogue customize-saved customize-apropos
				  customize-changed customize-unsaved customize-variable
				  customize-set-value customize-customized customize-set-variable
				  customize-apropos-faces customize-save-variable
				  customize-apropos-groups customize-apropos-options
				  customize-changed-options customize-save-customized))
    (put sym 'disabled nil))
  (put 'customize-themes 'disabled nil)
  ;; `custom-file' is still used internally by emacs to store some data,
  ;; even though we've disabled all of its user-facing functions.
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package recentf
  :after (no-littering)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package display-line-numbers
  :demand t
  :hook
  (prog-mode . lambdadog:display-line-numbers-hook)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  (display-line-numbers-current-absolute nil)
  ;; https://github.com/lambdadog/emacs-patches/blob/master/patches/display-line-numbers-pad.patch
  (display-line-numbers-pad t)
  :config
  (defun lambdadog:display-line-numbers-hook ()
    (setq-local left-margin-width 0)
    (display-line-numbers-mode +1))
  ;; TODO: inherit from `solaire-default-face'
  (set-face-background 'line-number-current-line "#eeeeee")
  (set-face-background 'line-number "#eeeeee"))

(use-package esup
  :custom
  ;; fixes error
  (esup-depth 0))

(use-package mood-line
  :demand t
  :config
  (mood-line-mode))

(use-package solaire-mode
  :demand t
  :custom
  (solaire-mode-real-buffer-fn #'lambdadog:real-buffer-fn)
  :config
  (defun lambdadog:real-buffer-fn ()
    (or (solaire-mode-real-buffer-p)
	(string= (buffer-name) "*dashboard*")))
  ;; TODO: inherit from `solaire-default-face'
  (set-face-foreground 'vertical-border "#eeeeee")
  (set-face-background 'vertical-border "#eeeeee")
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (setf (alist-get face solaire-mode-remap-alist) nil))
  (solaire-global-mode +1))

(use-package ligature
  :demand t
  :config
  (ligature-set-ligatures
   'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't))

(use-package dashboard
  :after (solaire-mode)
  :demand t
  :custom
  (dashboard-set-init-info nil)
  (dashboard-set-footer nil)
  (dashboard-page-separator "\n\n")
  (dashboard-center-content t)
  (dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (dashboard-item-names '(("Agenda for the coming week:" . "Agenda:")))
  (dashboard-items '((recents . 5) (agenda)))
  (initial-buffer-choice #'lambdadog:initial-buffer-choice)
  :config
  (defun lambdadog:initial-buffer-choice ()
    (get-buffer-create "*dashboard*"))
  (dashboard-setup-startup-hook))

;; CTRLF-mode has autoload configured in a way that doesn't actually
;; load the package upon ctrlf-mode being enabled
(use-package ctrlf)
(ctrlf-mode +1)

(use-package magit
  :commands (magit-status-setup-buffer)
  :bind
  ("C-x g" . #'magit-status)
  ("C-x G" . #'lambdadog:magit-status-for-emacsd))
(defun lambdadog:magit-status-for-emacsd ()
  (interactive)
  (magit-status-setup-buffer user-emacs-directory))

(use-package forge
  :after magit
  :custom
  (forge-topic-list-limit '(60 . -5))
  (forge-topic-list-order '(number . >)))

;;; TODO: setup selectrum how I like it
(use-package selectrum
  :after (solaire-mode)
  :demand t
  :custom
  (selectrum-display-action '(display-buffer-in-side-window
			      (side . bottom)
			      (slot . -1)))
  (selectrum-fix-vertical-window-height 8)
  :hook
  (selectrum-display-action . lambdadog:selectrum-display-action-hook)
  (buffer-list-update . lambdadog:selectrum-refocus-minibuffer)
  :config
  (defun lambdadog:selectrum-display-action-hook ()
    ;; (with-current-buffer (get-buffer-create " *selectrum-preview*")
    ;;   (setq-local mode-line-format nil)
    ;;   (setq-local cursor-type nil))
    (buffer-face-set 'solaire-default-face)
    (setq-local mode-line-format nil)
    (setq-local truncate-lines t))
  (defun lambdadog:selectrum-refocus-minibuffer ()
    (when (string= (buffer-name) selectrum--display-action-buffer)
      (select-window (active-minibuffer-window) nil)))
  (selectrum-mode 1))

;; (defun lambdadog:display-preview-window (&rest _)
;;   (when minibuffer-completing-file-name
;;     (display-buffer-in-side-window
;;      (get-buffer " *selectrum-preview*")
;;      '((side . bottom) (slot . +1)))
;;     (minimize-window (get-buffer-window " *selectrum-preview*"))))
;; (advice-add #'selectrum--get-display-window :after
;; 	    #'lambdadog:display-preview-window)

(use-package marginalia
  :after (selectrum)
  :demand t
  :custom
  (marginalia-margin-threshold 170)
  :config
  (marginalia-mode 1))

(use-package epkg-marginalia
  :after (marginalia)
  :commands (epkg-marginalia-annotate-package))
(cl-pushnew 'epkg-marginalia-annotate-package
	    (alist-get 'package marginalia-annotator-registry))

;; LSP Bridge is a fantastic package with abysmal configuration-flow.
(use-package lsp-bridge
  :commands (lsp-bridge-mode)
  :custom
  (lsp-bridge-python-command
   (let ((root-path (no-littering-expand-var-file-name "nix-gc-roots/lsp-bridge-python3/")))
     (unless (file-exists-p root-path)
       (let* ((default-directory user-emacs-directory)
	      (nix-build-cmd
	       (format "nix-build -E 'with (import ./.).pkgs; python3.withPackages (p: [ p.epc ])' -o %s"
		       (shell-quote-argument root-path))))
	 (unless (= 0 (shell-command nix-build-cmd))
	   (error "lsp-bridge: failed to get python3 shell"))))
     (expand-file-name "bin/python3" root-path)))
  (lsp-bridge-lang-server-extension-list '())
  (lsp-bridge-lang-server-mode-list '())
  (lsp-bridge-get-lang-server-by-project #'lambdadog:get-lang-server-by-project)
  :config
  ;; TODO handle loading by extension as well
  (defun lambdadog:get-lang-server-by-project (proj-path file-path)
    (let ((project-lsp-cfg-path (expand-file-name "lsp.json" proj-path))
	  (lang-lsp-path (no-littering-expand-etc-file-name "langserver")))
      (if (file-exists-p project-lsp-cfg-path)
	  project-lsp-cfg-path
	(let* ((mode (symbol-name
		      (save-excursion
			(with-current-buffer (find-file-noselect file-path)
			  major-mode))))
	       (json-name (s-replace "-mode" ".json" mode))
	       (mode-cfg-path (expand-file-name json-name lang-lsp-path)))
	  (when (file-exists-p mode-cfg-path)
	    mode-cfg-path)))))

  ;; lsp-bridge-mode won't actually start the LSP server without this
  (defun lambdadog:has-lsp-server ()
    (when buffer-file-name
      (let* ((file-path (ignore-errors (file-truename buffer-file-name)))
	     (proj-path (if lsp-bridge-get-project-path-by-filepath
			    (funcall lsp-bridge-get-project-path-by-filepath file-path)
			  (let ((default-directory (file-name-directory file-path)))
			    (when (string= "true"
					   (string-trim
					    (shell-command-to-string "git rev-parse --is-inside-work-tree")))
			      (string-trim
			       (shell-command-to-string "git rev-parse --show-toplevel")))))))
	(unless (file-exists-p (expand-file-name ".nolsp" proj-path))
	  (when (funcall lsp-bridge-get-lang-server-by-project proj-path file-path)
	    t)))))
  (advice-add #'lsp-bridge-has-lsp-server-p :override
	      #'lambdadog:has-lsp-server))

;; Setup hooks
(dolist (lsp-cfg (file-expand-wildcards (no-littering-expand-etc-file-name "langserver/*.json")))
  (let* ((mode-name (format "%s-mode" (file-name-base lsp-cfg)))
	 (mode-file (symbol-file (intern mode-name)))
	 (hook (intern (format "%s-hook" mode-name))))
    (message "hook %s after %s loads" (symbol-name hook) (file-name-base mode-file))
    (with-eval-after-load mode-file
      (add-hook hook #'lsp-bridge-mode))))

  ;; (global-lsp-bridge-mode))

(use-package haskell-mode)
(use-package rust-mode)
