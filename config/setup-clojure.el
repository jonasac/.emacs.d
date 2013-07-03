(eval-after-load 'clojure-mode
  '(progn
     (defun my-clojure-mode-defaults ()
       (clojure-test-mode +1))
     (add-hook 'clojure-mode-hook (lambda ()
				    (run-hooks 'my-clojure-mode-defaults)))))

(eval-after-load "nrepl"
  (add-hook 'nrepl-interaction-mode-hook
	    'nrepl-turn-on-eldoc-mode))


(provide 'setup-clojure)
