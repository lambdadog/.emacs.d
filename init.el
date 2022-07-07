;;; -*- lexical-binding: t -*-
(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(progn ;; disable customize
  ;; https://github.com/doomemacs/doomemacs/blob/35865ef5e89442e3809b8095199977053dd4210f/core/core-ui.el#L628-L639
  (dolist (sym '(customize-option customize-browse customize-group customize-face
				  customize-rogue customize-saved customize-apropos
				  customize-changed customize-unsaved customize-variable
				  customize-set-value customize-customized customize-set-variable
				  customize-apropos-faces customize-save-variable
				  customize-apropos-groups customize-apropos-options
				  customize-changed-options customize-save-customized))
    (put sym 'disabled nil))
  (put 'customize-themes 'disabled nil))

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

;; `custom-file' is still used internally by emacs to store some data,
;; even though we've disabled all of its user-facing functions.
(use-package custom
  :after (no-littering)
  :no-require t
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package recentf
  :after (no-littering)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package solaire-mode
  :demand t
  :custom
  (solaire-mode-real-buffer-fn
   (lambda ()
     (or (solaire-mode-real-buffer-p)
	 (string= (buffer-name) "*dashboard*"))))
  :config
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
  (initial-buffer-choice
   (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))
