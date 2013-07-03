(when (executable-find ispell-program-name)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(provide 'programming)
