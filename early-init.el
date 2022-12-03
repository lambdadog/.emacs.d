;;; -*- lexical-binding: t -*-
(setq user-init-file (or load-file-name buffer-file-name))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Startup speedups
(let ((restore:gc-cons-threshold 16777216)
      (restore:gc-cons-percentage 0.1)
      (restore:file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold most-positive-fixnum
	gc-cons-percentage 0.6
	file-name-handler-alist nil)
  (defun config:-restore-post-init-settings ()
    (setq gc-cons-threshold restore:gc-cons-threshold
	  gc-cons-percentage restore:gc-cons-percentage
	  file-name-handler-alist restore:file-name-handler-alist)))
(add-hook 'emacs-startup-hook #'config:-restore-post-init-settings)

;; auto-compile
(setq load-prefer-newer t)
(add-to-list 'load-path (locate-user-emacs-file "lib/compat/"))
(add-to-list 'load-path (locate-user-emacs-file "lib/auto-compile/"))
(require 'auto-compile)
(auto-compile-on-load-mode +1)
(auto-compile-on-save-mode +1)

;; package.el
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)
(advice-add #'package--save-selected-packages :override #'ignore)

;; Early visual customizations to avoid the emacs flash
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq inhibit-startup-buffer-menu t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-buffer-choice t
      initial-scratch-message "")

(push '(fullscreen . maximized) default-frame-alist)

(setq-default line-spacing 1)
(push '(font . "Fira Code") default-frame-alist)
