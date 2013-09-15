(require 'package)
(require 'cl)
(package-initialize)
(setq package-user-dir "~/.emacs.d/elpa")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar jac-packages
  '(exec-path-from-shell
    ido-ubiquitous
    smex
    projectile
    flx-ido
    magit
    diminish
    key-chord
    undo-tree
    volatile-highlights
    expand-region))

(defun jac-all-packages-installed-p ()
  "Check if all packages are already installed"
  (every #'package-installed-p jac-packages))

(defun install-my-packages ()
  "Installs packages that are not already installed on system."
  (interactive)
  (unless (jac-all-packages-installed-p)
    (package-refresh-contents)
    (mapc #'package-install
	  (remove-if #'package-installed-p jac-packages))))

(defun jac-autoinstall-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun jac-ensure-packages-installed (packages)
  (mapc #'jac-autoinstall-package packages))

(install-my-packages)

(provide 'packages)
