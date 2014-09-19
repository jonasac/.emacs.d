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

;;;; System specific
(if (display-graphic-p)
    (load-theme 'solarized-dark t)
    (when (eq system-type 'darwin)
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize)
      (setq default-directory "~/")
      (setq mac-option-modifier nil
	    mac-command-modifier 'meta)
      (set-face-attribute 'default nil :font "Monaco 12"))

  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :font "Inconsolata 10")))

;;;; Keybindings

;; Make it harder to quit
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Start eshell or switch to it if it is active
(global-set-key (kbd "C-x m") 'eshell)

;; Jump to definitition in the current file
(global-set-key (kbd "C-x C-i") 'imenu)

;; Go M-x without pressing M
(global-set-key (kbd "C-x m") 'execute-extended-command)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x t") 'jac-toggle-eshell-visor)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'key-chord)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-mode +1)
;;;; Core config
;;; Init
(setq inhibit-startup-screen t)
;;(when (fboundp 'fringe-mode) (fringe-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Modeline info
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Better yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; Rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't rename special buffers

;; Imenu should always rescan the buffer
(setq imenu-auto-rescan t)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Enable ido-mode everywhere
(require 'flx-ido)
(ido-mode t)
(ido-ubiquitous-mode)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (concat save-dir "ido.hist")
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)

;; Better flexmatching for ido-mode
(flx-ido-mode +1)

;; Disable ido faces, so flx highlights are shown instead
(setq ido-use-faces nil)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Keep tabs 8 spaces wide
(setq-default tab-width 8)

;; Change buffers when the files they refer to are changed
(global-auto-revert-mode t)

;; Show matching parenthesis
(show-paren-mode +1)

;; Clean up obsolete buffers
(require 'midnight)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Lines should be 80 characters
(setq fill-column 80)

;; Trailing whitespace sucks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position)

;; Make hippie-expand nicer
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(require 'flyspell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" save-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; Cleanup modeline
(require 'diminish)

;; Awesome undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)


(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; Project management
(require 'projectile)
(setq projectile-cache-file (expand-file-name "projectile.cache" save-dir))
(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" save-dir))
(projectile-global-mode t)
(diminish 'projectile-mode "Prjl")
(projectile-load-known-projects)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" save-dir))

;; Paren matching and some paredit features
(require 'smartparens-config)
(smartparens-global-mode +1)

(require 'smart-tab)
(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode 1)

;;;; Defuns
(defun jac-rename-buffer-and-file ()
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

(defun jac-toggle-eshell-visor ()
  "Brings up a visor like eshell buffer, filling the entire emacs frame"
  (interactive)
  (if (string= "eshell-mode" (eval 'major-mode))
      (jump-to-register :pre-eshell-visor-window-configuration)
    (window-configuration-to-register :pre-eshell-visor-window-configuration)
    (call-interactively 'eshell)
    (delete-other-windows)))
