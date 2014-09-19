;;;; init.el

;; Turn off mouse interface early to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable splash screen
(setq inhibit-splash-screen t)

(defvar root-dir (file-name-directory load-file-name))
(defvar save-dir (concat root-dir "savefile/"))
(defvar config-dir (concat root-dir "config/"))
(setq custom-file (concat root-dir "custom.el"))
(load custom-file 'noerror)
(add-to-list 'load-path config-dir)
;; Put backup stuff in /tmp so we dont keep it forever
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(unless (file-exists-p save-dir)
  (make-directory save-dir))

;;;; Packages
(require 'package)
(require 'cl)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun jac-install-packages ()
  "Install must have packages"
  (interactive)
  (let ((packages '(exec-path-from-shell
                   ido-ubiquitous
                   smex
		   flx-ido
                   projectile
                   magit
                   diminish
                   key-chord
                   undo-tree
                   volatile-highlights
                   expand-region
                   ace-jump-mode
                   smartparens
                   smart-tab
                   scala-mode2
                   ensime
                   solarized-theme)))
    (unless (every #'package-installed-p packages)
      (package-refresh-contents)
      (mapc '(lambda (package)
               (unless (package-installed-p package)
                 (package-install package)))
            packages))))
(jac-install-packages)

;;(require 'core)
;;(require 'bindings)
;;(require 'defuns)

;;;; System specific
(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (setq default-directory "~/")
  (setq mac-option-modifier nil
	mac-command-modifier 'meta)
  (set-face-attribute 'default nil :font "Monaco 12"))

(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "Inconsolata 10")))


(load-theme 'solarized-dark t)
