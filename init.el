;;;; init.el
(require 'cl)
;;;; Variables
(setq-default indent-tabs-mode nil)
(setq inhibit-splash-screen t
      ring-bell-function 'ignore
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      package-user-dir "~/.emacs.d/elpa/"
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;;;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defvar my-packages '(smex
                      magit
                      solarized-theme
                      scala-mode2
                      sbt-mode
                      projectile
                      ido-vertical-mode
                      exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; Look and feel
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(load-theme 'solarized-dark t)
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt
        mac-command-modifier 'meta))

(if window-system
    (cond ((eq system-type 'darwin)
           (progn
             (exec-path-from-shell-initialize)
             (set-default-font "Monaco 12")))
          ((eq system-type 'gnu/linux)
           (set-default-font "Ubuntu Mono 11"))))

;;;; Settings
(defalias 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(setq tab-always-indent 'complete)

(global-auto-revert-mode t) ; Change buffer if file changes on disk

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(show-paren-mode 1)

(ido-mode t)

;;;; Plugins
(projectile-global-mode)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;;;; Keybindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x t") 'my/toggle-eshell-visor)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-S") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x C-b") 'ibuffer)
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


