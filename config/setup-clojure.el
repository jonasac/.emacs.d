(eval-after-load "clojure-mode"
  (clojure-test-mode t))

(eval-after-load "nrepl"
  (add-hook 'nrepl-interaction-mode-hook
	    'nrepl-turn-on-eldoc-mode))


(provide 'setup-clojure)
