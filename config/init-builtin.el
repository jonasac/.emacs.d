;; These are settings that come with emacs, assuming version >= 24.3

;;; Init
(setq inhibit-startup-screen t)
;;(when (fboundp 'fringe-mode) (fringe-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;;; OSX specific settings
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil
	mac-command-modifier 'meta))

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

;; Imenu should always rescan the buffer
(setq imenu-auto-rescan t)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Enable ido-mode everywhere
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (concat save-dir "ido.hist")
      ido-default-file-method 'selected-window)

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

;; Lines should be 80 characters
(setq fill-column 80)

;; Paste things from emacs clipboard outside emacs
(setq x-select-enable-clipboard t)

; Dont break lines
(setq-default truncate-lines t)

;; Trailing whitespace sucks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position)

;; Make the fringe blend in
(set-face-background 'fringe "#242424")

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("erc" (mode . erc-mode))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")))))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(provide 'init-builtin)
