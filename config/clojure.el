(require 'lisp)
(jac-ensure-packages-installed '(clojure-mode clojure-test-mode nrepl))

(eval-after-load 'clojure-mode
  '(progn
     (defun jac-clojure-mode-defaults ()
       (clojure-test-mode +1)
       (run-hooks 'jac-lisp-hook))

     (setq jac-clojure-mode-hook 'jac-clojure-mode-defaults)

     (add-hook 'clojure-mode-hook (lambda ()
				    (run-hooks 'jac-clojure-mode-hook)))))


(eval-after-load "nrepl"
  (add-hook 'nrepl-interaction-mode-hook
	    'nrepl-turn-on-eldoc-mode))


(provide 'clojure)
