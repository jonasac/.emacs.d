;;;; OSX
(when window-system
  (when (eq system-type 'darwin)
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)
    (setq mac-option-modifier nil
          mac-command-modifier 'meta)
    (set-face-attribute 'default nil :font "Monaco 12"))

  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :font "Inconsolata 10")))

(provide 'system-specific)
