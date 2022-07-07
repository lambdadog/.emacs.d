;;; -*- lexical-binding: t -*-
(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(eval-and-compile ;; borg
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))
