(defadvice magit-stats (around magit-fullscreen activat)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(provide 'setup-magit)
