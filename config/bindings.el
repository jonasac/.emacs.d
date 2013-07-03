;; Make it harder to quit
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Start eshell or switch to it if it is active
(global-set-key (kbd "C-x m") 'eshell)

;; Jump to definitition in the current file
(global-set-key (kbd "C-x C-i") 'imenu)

;; Go M-x without pressing M
(global-set-key (kbd "C-x m") 'execute-extended-command)

(provide 'bindings)
