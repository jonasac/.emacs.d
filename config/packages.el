(require 'package)
(require 'cl)
(package-initialize)
(setq package-user-dir "~/.emacs.d/elpa")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar my-packages
  '(exec-path-from-shell
    ido-ubiquitous
    clojure-mode
    nrepl
    smex
    paredit
    clojure-test-mode
    solarized-theme))

(defun all-packages-installed-p ()
  "Check if all packages are already installed"
  (every #'package-installed-p my-packages))

(defun install-my-packages ()
  "Installs packages that are not already installed on system."
  (interactive)
  (unless (all-packages-installed-p)
    (package-refresh-contents)
    (mapc #'package-install
	  (remove-if #'package-installed-p my-packages))))
(install-my-packages)

(provide 'packages)
