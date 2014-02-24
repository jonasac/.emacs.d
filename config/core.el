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

;; Paste things from emacs clipboard outside emacs
;(setq x-select-enable-clipboard t
 ;     x-select-enable-primary t
  ;    save-interprogram-paste-before-kill t
   ;   apropos-do-all t
    ;  mouse-yank-at-point t)

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
                                         try-complete-lisp-symbol
                                         yas/hippie-try-expand))
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

;; Shorter aliases for ack-and-a-half commands
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Log completion of TODO items
(setq org-log-done t)

;; Paren matching and some paredit features
(require 'smartparens-config)
(smartparens-global-mode +1)
(require 'smart-tab)
(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode 1)
(require 'yasnippet)
(yas-global-mode 1)
(provide 'core)
