(if (fboundp 'fringe-mode)
    (fringe-mode 0))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (add-hook 'after-init-hook (lambda () (scroll-bar-mode -1))))
(when (fboundp 'menu-bar-mode) 
  (menu-bar-mode -1))
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(load-theme 'snorkelfrosk t)

;; Don't ring the bell
(setq ring-bell-function 'ignore)
(provide 'gui)
