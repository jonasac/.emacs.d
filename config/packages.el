
(require 'cl)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar my-packages
 '(molokai-theme paredit rainbow-delimiters 
		 melpa clojure-mode nrepl 
		 magit exec-path-from-shell
		 rainbow-mode smex find-file-in-project
		 ido-ubiquitous undo-tree slime
		 auto-complete volatile-highlights yasnippet
		 clojure-test-mode))

(defun packages-installed-p ()
  (loop for p in my-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(defun install-packages ()
  (unless (packages-installed-p)
    (message "%s" "Refreshing packages...")
    (package-refresh-contents)
    (message "%s" "done.")
    (dolist (p my-packages)
      (unless (package-installed-p p)
	(package-install p)))))

(install-packages)
(provide 'packages)
