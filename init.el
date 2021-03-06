;;; package --- init.el

(setq gc-cons-threshold 100000000)
(load-file "~/.emacs.d/jonasac.el")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;; Variables
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t
      use-package-always-ensure t
      ring-bell-function 'ignore
      save-interprogram-paste-before-kill t
      require-final-newline t
      apropos-do-all t
      custom-file "~/.emacs.d/custom.el"
      compilation-scroll-output t
      initial-scratch-message ""
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; UTF-8
(set-charset-priority         'unicode)
(prefer-coding-system         'utf-8)
(set-terminal-coding-system   'utf-8)
(set-keyboard-coding-system   'utf-8)
(set-selection-coding-system  'utf-8)
(setq locale-coding-system    'utf-8)

;;;; Look and feel
(my/disable-gui-features)

(when (my/osx-p)
  (progn
    (setq mac-option-modifier nil
          mac-command-modifier 'meta)
    (set-face-attribute 'default nil
			:family "Source Code Pro"
			:height 110
			:weight 'normal
			:width 'normal)))

(when (my/linux-p)
  (set-frame-font "Ubuntu Mono 11"))

;;;; Packages
(when (my/osx-p)
  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize)
    :ensure t))

(use-package saveplace
  :init (save-place-mode 1)
  :config
  (setq-default save-place t)
  (setq save-place-limit nil))

(use-package paren
  :init (show-paren-mode t))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (setq auto-revert-check-vc-info t)
  (global-auto-revert-mode t))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode))

(use-package magit
  :commands (magit-status))

(use-package ivy
  :commands (ivy-mode)
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (use-package flx))

(use-package counsel
  :commands (counsel-M-x counsel-find-file))

(use-package groovy-mode :mode "\\.gradle\\'")

(use-package flycheck
  :diminish flycheck-mode)

(use-package projectile
  :config
  (projectile-mode t)
  (use-package counsel-projectile
    :config
    (counsel-projectile-on)))

(use-package anti-zenburn-theme
  :config (load-theme 'anti-zenburn t))

(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-rules
	`(("*Apropos*" :align below :size 16 :select t)
	  ("*Help*" :align below :size 16 :select t))))

(use-package disable-mouse
  :diminish global-disable-mouse-mode
  :config (global-disable-mouse-mode))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))



