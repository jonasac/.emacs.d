(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)))

(defun lisp-defaults ()
  (paredit-mode +1))

(setq my-lisp-hook 'lisp-defaults)

(provide 'lisp)
