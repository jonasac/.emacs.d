(setq inhibit-startup-screen t)
(when (fboundp 'fringe-mode) (fringe-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (add-hook 'after-init-hook (lambda () (scroll-bar-mode -1))))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(defvar root-dir (file-name-directory load-file-name))
(defvar config-dir (concat root-dir "config/"))
(defvar plugins-dir (concat config-dir "plugins/"))
(defvar save-dir (concat root-dir "savefile/"))
(setq custom-file (concat config-dir "custom.el"))
(add-to-list 'load-path config-dir)
(add-to-list 'load-path plugins-dir)
;; Package section
(require 'cl)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar my-packages
 '(molokai-theme melpa
		 clojure-mode
		 nrepl 
		 magit
		 exec-path-from-shell
		 rainbow-mode smex 
		 ido-ubiquitous
		 undo-tree
		 auto-complete 
		 clojure-test-mode
		 virtualenv
		 noctilux-theme
		 evil))

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

;; Store backup and autosave files in the systems /tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(unless (file-exists-p save-dir)
  (make-directory save-dir))

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" save-dir))
(setq-default save-place t)
(require 'saveplace)

;; Setup path if we are on a mac
(when (eq system-type 'darwin)
(exec-path-from-shell-initialize)
(setq mac-option-modifier nil
      mac-command-modifier 'meta))

(require 'editor)
(require 'bindings)
(require 'plugins)
(require 'defuns)


(blink-cursor-mode -1)

;; Modeline info
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Show linenumbers on the side
(linum-mode t)
(setq linum-format " %4d ")

;; Better yes/no questions 
(fset 'yes-or-no-p 'y-or-n-p)

(load-theme 'noctilux t)

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Use shift + arrow for moving between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Imenu should always rescan the buffer
(setq imenu-auto-rescan t)

;; dired - resuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Enable ido-mode everywhere
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (concat save-dir "ido.hist")
      ido-default-file-method 'selected-window)

;; Esc quits everywhere
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; M-x works like ido-mode
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Find file in project (projectroot == folder with .git/)
(global-set-key (kbd "C-c f") 'find-file-in-project)

;; Make it harder to quit
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Buffer management
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

;; Start eshell or switch to it if it is active
(global-set-key (kbd "C-x m") 'eshell)

;; Alternative to M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Imenu is awesome
(global-set-key (kbd "C-x C-i") 'imenu)

;; Use spaces instead of tabs
(setq-default indent-with-tabs-mode nil)

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

;; If region is highlighted, delete it if characters are entered
(delete-selection-mode 1)

;; Lines should be 80 characters
(setq fill-column 80)

;; Paste things from emacs clipboard outside emacs
(setq x-select-enable-clipboard t)

;; Autopair parens etc
(electric-pair-mode t)

;; Dont break lines
(setq-default truncate-lines t)
