(defvar root-dir (file-name-directory load-file-name))
(defvar save-dir (concat root-dir "savefile/"))
(defvar config-dir (concat root-dir "config/"))
(setq custom-file (concat root-dir "custom.el"))
(load custom-file 'noerror)
(add-to-list 'load-path config-dir)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(unless (file-exists-p save-dir)
  (make-directory save-dir))

;; Setup verious things to load after the init file is done loading
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'init-builtin)
	    (require 'package)
	    (require 'init-evil)
	    (require 'init-autocomplete)
	    (load-theme 'anti-zenburn)))

;; When opening a file put there cursor where it were when we closed the file.
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" save-dir))
(setq-default save-place t)
