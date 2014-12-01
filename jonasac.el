;;;; A hodgepodge of useful macros and functions
(defmacro osx-p ()
  `(equal system-type 'darwin))

(defmacro linux-p ()
  `(equal system-type 'gnu/linux))

(defun my/rename-buffer-and-file ()
  "Rename the current buffer file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
	(cond
	 ((vc-backend filename) (vc-rename-file filename new-name))
	 (t
	  (rename-file filename new-name t)
	  (set-visited-file-name new-name t t)))))))

(defun my/toggle-eshell-visor ()
  "Brings up a visor like eshell buffer, filling the entire emacs frame"
  (interactive)
  (if (string= "eshell-mode" (eval 'major-mode))
      (jump-to-register :pre-eshell-visor-window-configuration)
    (window-configuration-to-register :pre-eshell-visor-window-configuration)
    (call-interactively 'eshell)
    (delete-other-windows)))

(defun disable-gui-features ()
  (progn
    (menu-bar-mode -1)
    (when (fboundp 'tool-bar-mode)
      (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1))))
