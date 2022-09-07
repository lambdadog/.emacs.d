;;; -*- lexical-binding: t -*-
(setq user-init-file (or load-file-name buffer-file-name))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(defvar lambdadog:file-name-handler-alist-cache file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun lambdadog:restore-post-init-settings ()
  (setq gc-cons-threshold 16777216 ; 16mb
        gc-cons-percentage 0.1)
  (setq file-name-handler-alist lambdadog:file-name-handler-alist-cache))
(add-hook 'emacs-startup-hook #'lambdadog:restore-post-init-settings)

(progn ;; auto-compile
  (setq load-prefer-newer t)
  (add-to-list 'load-path (expand-file-name "lib/compat/"       user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lib/packed/"       user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lib/auto-compile/" user-emacs-directory))
  (require 'auto-compile)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(progn ;; package.el
  (setq package-enable-at-startup nil)

  (defun package--ensure-init-file/:override ())
  (advice-add #'package--ensure-init-file :override
              #'package--ensure-init-file/:override)
  (defun package--save-selected-packages/:override (&optional _))
  (advice-add #'package--save-selected-packages :override
              #'package--save-selected-packages/:override))

(progn ;; early customizations to avoid the emacs flash
  (push '(menu-bar-lines . 0)   default-frame-alist)
  (push '(tool-bar-lines . 0)   default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (setq menu-bar-mode   nil
	tool-bar-mode   nil
	scroll-bar-mode nil)

  (push '(left-fringe . 0)  default-frame-alist)
  (push '(right-fringe . 0) default-frame-alist)
  (setq-default left-margin-width 1
		right-margin-width 0)
  (setq-default truncate-lines t)

  (let ((display-table (make-display-table)))
    (set-display-table-slot display-table 'truncation 32)
    (set-display-table-slot display-table 'wrap 32)
    (set-display-table-slot display-table 'selective-display 32)
    (setq-default standard-display-table display-table))

  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message user-login-name)

  (add-to-list 'load-path (expand-file-name "lib/ef-themes/" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lib/doom-themes/themes/" user-emacs-directory))
  (require 'ef-themes)
  (setq ef-themes-to-toggle '(ef-summer ef-winter))
  (ef-themes-select 'ef-summer)

  ;; modeline padding
  (defun lambdadog:ef-themes-pad-modeline ()
    (ef-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :inherit mode-line
			:box (:line-width 4 :color ,bg-mode-line))))
       `(mode-line-inactive ((,c :inherit mode-line-inactive
				 :box (:line-width 4 :color ,bg-alt)))))))
  (add-hook 'ef-themes-post-load-hook #'lambdadog:ef-themes-pad-modeline)
  (lambdadog:ef-themes-pad-modeline)

  (push '(fullscreen . maximized) default-frame-alist)

  (setq-default line-spacing 1)
  (push '(font . "Fira Code") default-frame-alist))
