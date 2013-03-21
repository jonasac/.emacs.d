;; Paredit is awesomesauce
(require 'paredit)

;; Fix undo plz
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Enable smex for better M-x functionality
(setq smex-save-file (concat save-dir ".smex-items"))
(smex-initialize)

(setq nrepl-popup-stacktraces nil)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'clojure-test-mode)
(setq inferior-lisp-program "/usr/local/bin/clisp")
(require 'slime)
(slime-setup)

;; Magit
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; Auto-completions
(require 'auto-complete-config)
(ac-config-default)

;; Volatile Highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(provide 'plugins)
