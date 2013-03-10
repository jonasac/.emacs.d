;; Enter makes newline and indents it accordingly
(global-set-key (kbd "RET") 'newline-and-indent)

;; Better buffer handling
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; M-x works like ido-mode
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Find file in project (projectroot == folder with .git/)
(global-set-key (kbd "C-c f") 'find-file-in-project)

;; Make it harder to quit
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Buffer management
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

(provide 'bindings)