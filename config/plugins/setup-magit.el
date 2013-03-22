(defadvice magit-status (around magit-fullscreen activat)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(eval-after-load 'magit
  '(progn
     (set-face-background 'highlight nil)
     (set-face-foreground 'highlight nil)
     (set-face-underline 'highlight nil)
     (define-key magit-mode-map (kbd "q") 'magit-quit-session)))
(provide 'setup-magit)
