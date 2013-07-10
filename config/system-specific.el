;;;; OSX
(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (setq mac-option-modifier nil
	mac-command-modifier 'meta)
  (set-face-attribute 'default nil :font "Monaco 12"))

(provide 'system-specific)
