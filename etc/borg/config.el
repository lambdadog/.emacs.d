(when (native-comp-available-p)
  (setq borg-compile-function #'borg-byte+native-compile))
