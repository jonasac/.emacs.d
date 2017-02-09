;;;; Packages

(load-file "~/.emacs.d/jonasac.el")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/") t)

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
(my/disable-gui-features)

(when (my/osx-p)
  (progn
    (setq mac-option-modifier nil
          mac-command-modifier 'meta)
    (set-default-font "Monaco 11")))

(when (my/linux-p)
  (set-default-font "Ubuntu Mono 11"))

;;;; Packages
(when (my/osx-p)
  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize)
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

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure t)

(use-package solarized-theme
  :init (load-theme 'solarized-dark t)
  :ensure t)

(use-package volatile-highlights
  :ensure t
  :defer t
  :commands volatile-highlights-mode
  :init (volatile-highlights-mode t))

(use-package ivy
  :config (ivy-mode 1)
  :ensure t)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :ensure t)

(use-package groovy-mode :mode "\\.gradle\\'")

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package meghanada :config (add-hook 'java-mode-hook
                                         (lambda ()
                                           (meghanada-mode t)
                                           (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))))
  

;;;; Keybindings
(global-set-key (kbd "C-x t") 'my/toggle-eshell-visor)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-S") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
