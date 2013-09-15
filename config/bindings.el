;; Make it harder to quit
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Start eshell or switch to it if it is active
(global-set-key (kbd "C-x m") 'eshell)

;; Jump to definitition in the current file
(global-set-key (kbd "C-x C-i") 'imenu)

;; Go M-x without pressing M
(global-set-key (kbd "C-x m") 'execute-extended-command)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x t") 'jac-toggle-eshell-visor)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'key-chord)

(key-chord-define-global "uu" 'undo-tree-visualize)

(key-chord-mode +1)
(provide 'bindings)
