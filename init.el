;;; -*- lexical-binding: t -*-
(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(eval-and-compile ;; borg
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(eval-and-compile ;; use-package
  (require 'use-package)
  (setq use-package-verbose t)
  (setq use-package-always-defer t))

(use-package no-littering
  :demand t
  :custom
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

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
  :config
  (defun lambdadog:display-line-numbers-hook ()
    (setq-local left-margin-width 0)
    (setq-local line-prefix " ")
    (set-window-buffer nil (current-buffer))
    (display-line-numbers-mode +1))
  ;; TODO: inherit from `solaire-default-face'
  (set-face-background 'line-number-current-line "#eeeeee")
  (set-face-background 'line-number "#eeeeee"))

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
  ;; I prefer this binding to #'magit
  :bind ("C-x g" . #'magit-status))

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
    (buffer-face-set 'solaire-default-face)
    (setq-local mode-line-format nil)
    (setq-local truncate-lines t))
  (defun lambdadog:selectrum-refocus-minibuffer ()
    (when (string= (buffer-name) selectrum--display-action-buffer)
      (select-window (active-minibuffer-window) nil)))
  (selectrum-mode 1))
