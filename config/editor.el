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

;; Show trailing spaces
(setq show-trailing-whitespace t)

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; Smart indentation
(electric-indent-mode t)
(provide 'editor)


