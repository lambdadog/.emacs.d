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
  (defun config:-do-restore-post-init-settings ()
    (setq gc-cons-threshold restore:gc-cons-threshold
	  gc-cons-percentage restore:gc-cons-percentage
	  file-name-handler-alist restore:file-name-handler-alist))
  (declare-function config:-do-restore-post-init-settings nil)
  (defun config:-restore-post-init-settings ()
    "Restore settings changed in order to speed up init evaluation."
    (let ((timer (timer-create)))
      (timer-set-time timer 0 nil)
      (timer-set-function timer #'config:-do-restore-post-init-settings)
      (timer-activate-when-idle timer t))))

(add-hook 'emacs-startup-hook #'config:-restore-post-init-settings)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
	      cursor-in-non-selected-windows nil)
(setq bidi-inhibit-bpa t
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      ffap-machine-p-known 'reject
      idle-update-delay 1.0
      read-process-output-max (* 64 1024)
      redisplay-skip-fontification-on-input t)

;; auto-compile
(setq load-prefer-newer t)
(add-to-list 'load-path (locate-user-emacs-file "lib/compat/"))
(add-to-list 'load-path (locate-user-emacs-file "lib/auto-compile/"))
(require 'auto-compile)
(auto-compile-on-load-mode +1)
(auto-compile-on-save-mode +1)

;; package.el
(declare-function package--ensure-init-file "package")
(declare-function package--save-selected-packages "package")
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)
(advice-add #'package--save-selected-packages :override #'ignore)

;; Early visual customizations to avoid the emacs flash
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; On MacOS, disabling the menu bar makes MacOS treat Emacs as a
;; "non-application window", which causes a number of undesireable
;; behaviors.
(when (eq system-type 'darwin)
  (defun config:-restore-menu-bar-in-gui-frames (&optional frame)
    (when-let (frame (or frame (selected-frame)))
      (when (display-graphic-p frame)
	(set-frame-parameter frame 'menu-bar-lines 1))))

  (declare-function config:-restore-menu-bar-in-gui-frames nil)
  (add-hook 'window-setup-hook
	    #'config:-restore-menu-bar-in-gui-frames)
  (add-hook 'after-make-frame-functions
	    #'config:-restore-menu-bar-in-gui-frames))


(push '(left-fringe  . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)
(setq-default left-margin-width 1
	      truncate-lines t)

(let ((display-table (make-display-table)))
  (set-display-table-slot display-table 'truncation        32)
  (set-display-table-slot display-table 'wrap              32)
  (set-display-table-slot display-table 'selective-display 32)
  (setq-default standard-display-table display-table))

(add-to-list 'load-path (locate-user-emacs-file "lib/ef-themes/"))
(require 'ef-themes)
(setq ef-themes-to-toggle '(ef-summer ef-winter))
(ef-themes-select 'ef-summer)

(defun config:-ef-themes-pad-modeline ()
  "Add padding to the modeline of the currently active ef-theme"
  (ef-themes-with-colors
   (custom-set-faces
    `(mode-line ((,c :inherit mode-line
		     :box (:line-width 4 :color ,bg-mode-line))))
    `(mode-line-inactive ((,c :inherit mode-line-inactive
			      :box (:line-width 4 :color ,bg-alt)))))))
(add-hook 'ef-themes-post-load-hook #'config:-ef-themes-pad-modeline)

;; `ef-themes-select' must have already been called before
;; `ef-themes-with-colors' is used, so we must run this manually
;; rather than relying on our `ef-themes-select' invocation to call it
;; as a hook.
(config:-ef-themes-pad-modeline)

(push '(fullscreen . maximized) default-frame-alist)

(setq-default line-spacing 1)
(push '(font . "Fira Code") default-frame-alist)
