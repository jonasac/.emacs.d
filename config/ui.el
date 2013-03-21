(if (fboundp 'fringe-mode)
    (fringe-mode 0))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; Turn off scrollbar after gui is loaded to avoid bug on OSX
(when (fboundp 'scroll-bar-mode)
  (add-hook 'after-init-hook (lambda () (scroll-bar-mode -1))))
(when (fboundp 'menu-bar-mode) 
  (menu-bar-mode -1))

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Turn off the startup screen
(setq inhibit-startup-screen t)

;; Show linenumber in modeline
(line-number-mode t)

;; Show column numer in modeline
(column-number-mode t)

;; Indicate size in modeline
(size-indication-mode t)

;; Better yes/no questions 
(fset 'yes-or-no-p 'y-or-n-p)

;; Load theme if we are in gui
(when window-system
  (load-theme 'adwaita t))

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Use shift + arrow for moving between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Fix ediff insanity
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
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
(provide 'ui)
