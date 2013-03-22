;; Fix undo plz
(setq undo-tree-mode-lighter "")
(global-undo-tree-mode)

;; Enable smex for better M-x functionality
(setq smex-save-file (concat save-dir ".smex-items"))
(smex-initialize)

;; Auto-completions
(require 'auto-complete-config)
(ac-config-default)

;; Volatile Highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Yasnippet
(yas-global-mode 1)

(require 'setup-clojure)
(require 'setup-magit)
(provide 'plugins)
