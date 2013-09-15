(require 'programming)
(jac-ensure-packages-installed '(rainbow-delimiters))

(defun jac-lisp-defaults ()
  (rainbow-delimiters-mode +1))

(setq jac-lisp-hook 'jac-lisp-defaults)

(provide 'lisp)
