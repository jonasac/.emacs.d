(defun prog-mode-defaults ()
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode)))

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'prog-mode-defaults)))

(provide 'programming)
