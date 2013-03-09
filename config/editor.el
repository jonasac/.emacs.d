;; Use spaces instead of tabs
(setq-default indent-with-tabs-mode nil)

;; Keep tabs 8 spaces wide
(setq-default tab-width 8)

;; Change buffers when the files they refer to are changed
(global-auto-revert-mode t)

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

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

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

;; Delete region inserting while region is marked
(delete-selection-mode 1)

;; Lines should be 80 characters
(setq fill-column 80)

;; Highlight the current line
(global-hl-line-mode +1)

;; Use shift + arrow for moving between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; Fix ediff insanity
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Enable smex for better M-x functionality
(setq smex-save-file (concat root-dir ".smex-items"))
(smex-initialize)

;; Paste things from emacs clipboard outside emacs
(setq x-select-enable-clipboard t)

;; Add pars of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Dont break lines
(setq-default truncate-lines t)

;; Better undo-mode
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

(provide 'editor)
