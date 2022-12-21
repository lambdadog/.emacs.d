;;; -*- lexical-binding: t -*-
(defun keymap-global-set (key definition)
  (when (stringp definition)
    (setq definition (kbd definition)))
  (global-set-key (kbd key) definition))
