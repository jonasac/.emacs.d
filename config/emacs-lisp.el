(require 'lisp)

(add-hook 'emacs-lisp-mode-hook (lambda () (run-hooks 'my-lisp-hook)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(provide 'emacs-lisp)
