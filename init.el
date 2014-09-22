;;;; init.el
(require 'cl)
;;;; Variables
(setq inhibit-splash-screen t
      ring-bell-function 'ignore
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      package-user-dir "~/.emacs.d/elpa/")

;;;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defvar my-packages '(better-defaults
                      smex
                      magit
                      ujelly-theme
                      find-file-in-project
                      scala-mode2
                      sbt-mode
                      exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; Look and feel
(load-theme 'ujelly t)
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt
        mac-command-modifier 'meta))
  
(if window-system
    (when (eq system-type 'darwin)
      (progn 
        (exec-path-from-shell-initialize)
        (set-face-attribute 'default nil :font "Monaco 12")))
  
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :font "Inconsolata 10")))


(defalias 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(global-auto-revert-mode t) ; Change buffer if file changes on disk

;;;; Keybindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x t") 'my/toggle-eshell-visor)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;;;; Defuns
(defun my/rename-buffer-and-file ()
  "Rename the current buffer file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
	(cond
	 ((vc-backend filename) (vc-rename-file filename new-name))
	 (t
	  (rename-file filename new-name t)
	  (set-visited-file-name new-name t t)))))))

(defun my/toggle-eshell-visor ()
  "Brings up a visor like eshell buffer, filling the entire emacs frame"
  (interactive)
  (if (string= "eshell-mode" (eval 'major-mode))
      (jump-to-register :pre-eshell-visor-window-configuration)
    (window-configuration-to-register :pre-eshell-visor-window-configuration)
    (call-interactively 'eshell)
    (delete-other-windows)))


