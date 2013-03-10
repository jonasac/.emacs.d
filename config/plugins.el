;; Make tab complete or indent according context
(require 'smart-tab)
(global-smart-tab-mode 1)

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
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime)
(slime-setup)

;; Magit
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(provide 'plugins)
