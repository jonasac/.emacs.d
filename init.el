;;;; Packages

(load-file "~/.emacs.d/jonasac.el")

(when (osx-p)
  (when (file-exists-p "~/.mu4e_settings.el")
    (load-file "~/.mu4e_settings.el")))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)

;;;; Variables
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(delete-selection-mode t)
(setq inhibit-splash-screen t
      ring-bell-function 'ignore
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      package-user-dir "~/.emacs.d/elpa/"
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      require-final-newline t
      visible-bell t
      tab-always-indent 'complete
      initial-scratch-message ""
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;;;; Look and feel
(disable-gui-features)

(when (osx-p)
  (progn
    (setq mac-option-modifier 'alt
          mac-command-modifier 'meta)
    (set-default-font "Monaco 11")))

(when (linux-p)
  (set-default-font "Ubuntu Mono 11"))

;;;; Packages
(when (osx-p)
  (use-package exec-path-from-shell
    :idle (exec-path-from-shell-initialize)
    :ensure t))

(use-package paren
  :init (show-paren-mode t))

(use-package autorevert
  :commands auto-revert-mode
  :init
  (add-hook 'find-file-hook
            #'(lambda ()
                (auto-revert-mode t))))

(use-package guide-key
  :ensure t
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h")
          guide-key/popup-window-position 'bottom)
    (guide-key-mode t)))

(use-package projectile
  :init (add-hook 'prog-mode-hook 'projectile-mode)
  :defer t
  :ensure t)

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :ensure t)

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure t)

(use-package ido
  :idle (require 'ido)
  :config (progn
            (ido-everywhere t)
            (ido-mode t))
  :defer t)

(use-package ido-vertical-mode
  :defer t
  :init (ido-vertical-mode t)
  :ensure t)

(use-package flx-ido
  :defer t
  :init (flx-ido-mode t)
  :ensure t)

(use-package git-gutter
  :defer t
  :init (add-hook 'prog-mode-hook 'git-gutter-mode)
  :ensure t)

(use-package zenburn-theme
  :ensure t
  :init (load-theme 'zenburn t))

(use-package volatile-highlights
  :ensure t
  :defer t
  :commands volatile-highlights-mode
  :init (volatile-highlights-mode t))

(use-package jabber
  :ensure t
  :init (progn
          (setq jabber-account-list
                '(("jonasacl@chat.uio.no"
                   (:network-server . "chat.uio.no")
                   (:connection-type . ssl))))
          (setq jabber-show-offline-contacts nil
                jabber-roster-line-format "%c %-25n %u %-8s  %S")))
(use-package company
  :ensure t
  :init (global-company-mode))

;;;; Keybindings
(global-set-key (kbd "C-x t") 'my/toggle-eshell-visor)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-S") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
