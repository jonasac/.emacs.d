;;; package --- init.el

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
      initial-scratch-message ""
      backup-directory-alist `(("" . ,(concat user-emacs-directory "backups"))))

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
    (set-frame-font "Source Code Pro 11")))

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
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook
            #'(lambda ()
                (auto-revert-mode t))))

(use-package which-key
  :diminish which-key-mode
  :config (progn
            (setq which-key-sort-order 'which-key-key-order-alpha)
            (which-key-add-key-based-replacements
              "SPC g" "git"
              "SPC f" "files"
              "SPC p" "project"
              "SPC b" "buffers"
              "SPC h" "help")
            (which-key-mode)))

(use-package magit
  :config (progn
            (evil-leader/set-key "gs" 'magit-status)))

(use-package ivy
  :diminish ivy-mode
  :config (progn
            (ivy-mode 1)
            (evil-leader/set-key "bb" 'ivy-switch-buffer)
            (use-package flx)))

(use-package counsel
  :config (progn
            (evil-leader/set-key "ff" 'find-file)))

(use-package groovy-mode :mode "\\.gradle\\'")

(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode))

(use-package projectile
  :init (progn
            (projectile-mode t)
            (use-package counsel-projectile
              :config (progn
                        (counsel-projectile-on)
                        (evil-leader/set-key "pp" 'counsel-projectile-switch-project)
                        (evil-leader/set-key "pb" 'counsel-projectile-switch-to-buffer)
                        (evil-leader/set-key "pf" 'counsel-projectile-find-file)))))

(use-package evil
  :init (progn
          (evil-mode t)
          (use-package evil-leader
            :init (global-evil-leader-mode)
            :config (progn
                      (evil-leader/set-leader "SPC")
                      (evil-leader/set-key "bk" 'kill-buffer)
                      (evil-leader/set-key "hk" 'describe-key)
                      (evil-leader/set-key "hv" 'describe-variable)
                      (evil-leader/set-key "hf" 'describe-function)
                      (evil-leader/set-key "hm" 'describe-mode)
                      (evil-leader/set-key "hi" 'info)
                      (evil-leader/set-key "ha" 'apropos-command)))))

(use-package neotree
  :init (progn
          (evil-leader/set-key "ft" 'neotree-toggle)
          (evil-leader/set-key "pt" 'neotree-find-project-root)
          (setq neo-theme 'arrow)
          (add-hook 'neotree-mode-hook
                    (lambda ()
                      (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-stretch-toggle)
                      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                      (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
                      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
                      (define-key evil-normal-state-local-map (kbd "gr") 'neotree-refresh)
                      (define-key evil-normal-state-local-map (kbd "j") 'neotree-next-line)
                      (define-key evil-normal-state-local-map (kbd "k") 'neotree-previous-line)
                      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)))))

(use-package tao-theme
  :init (load-theme 'tao-yang t))

(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-rules
        `(
          ("*Help*" :align below :size 16 :select t))))
  

(use-package company
  :init
  (setq company-idle-delay 0.2)
  (global-company-mode))

(use-package disable-mouse
  :diminish global-disable-mouse-mode
  :init (global-disable-mouse-mode))
