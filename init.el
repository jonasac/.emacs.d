;;;; Packages

(load-file "~/.emacs.d/jonasac.el")

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;; Variables
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(delete-selection-mode t)
(setq inhibit-splash-screen t
      use-package-always-ensure t
      ring-bell-function 'ignore
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      package-user-dir "~/.emacs.d/elpa/"
      save-interprogram-paste-before-kill t
      require-final-newline t
      apropos-do-all t
      visible-bell t
      tab-always-indent 'complete
      initial-scratch-message ""
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;;;; Look and feel
(my/disable-gui-features)

(when (my/osx-p)
  (progn
    (setq mac-option-modifier nil
          mac-command-modifier 'meta)
    (set-frame-font "Source Code Pro 11")))

(when (my/linux-p)
  (set-frame-font "Ubuntu Mono 11"))

;;;; Packages
(when (my/osx-p)
  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize)
    :ensure t))

(use-package paren
  :init (show-paren-mode t))

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook
            #'(lambda ()
                (auto-revert-mode t))))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package material-theme
  :init (load-theme 'material t))

(use-package ivy
  :diminish ivy-mode
  :ensure t
  :config (progn
            (ivy-mode 1)
            (use-package flx)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

(use-package groovy-mode :mode "\\.gradle\\'")

(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  :config (use-package flycheck-pos-tip
            :ensure t
            :init (with-eval-after-load 'flycheck
                    (flycheck-pos-tip-mode))))

(use-package projectile
  :init (progn
            (projectile-global-mode t)
            (use-package counsel-projectile
              :ensure t
              :config (counsel-projectile-on))))

(use-package meghanada
  :config (progn
            (add-hook 'java-mode-hook
                    (lambda ()
                      (meghanada-mode t)
                      (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))))

(use-package evil
  :init (progn
          (evil-mode t)
          (define-key evil-ex-map "e" 'counsel-find-file)
          (define-key evil-ex-map "b" 'ivy-switch-buffer)

          (use-package evil-leader
            :init (global-evil-leader-mode)
            :config (progn
                      (evil-leader/set-leader "SPC")
                      (evil-leader/set-key "b" 'switch-buffer)))))
;;;; Keybindings
(global-set-key (kbd "C-x t") 'my/toggle-eshell-visor)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-S") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
