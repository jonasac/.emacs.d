(defvar root-dir (file-name-directory load-file-name))
(defvar save-dir (concat root-dir "savefile/"))
(defvar config-dir (concat root-dir "config/"))
(setq custom-file (concat root-dir "custom.el"))
(load custom-file 'noerror)
(add-to-list 'load-path config-dir)
(add-to-list 'custom-theme-load-path (concat root-dir "themes/"))

;; Put backup stuff in /tmp so we dont keep it forever
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(unless (file-exists-p save-dir)
  (make-directory save-dir))

;; Setup various things to load after the init file is done loading
;; If a filename begins with init, it is a file that we want loaded during init
;; If a filename begins with setup, it is probably loaded and configured via autoload mechanisms
(require 'packages)
(require 'core)
(require 'system-specific)
(require 'bindings)
(require 'defuns)
(require 'programming)
(require 'lisp)
(require 'emacs-lisp)
(require 'clojure)
(require 'sml)


; When opening a file put there cursor where it were when we closed the file.
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" save-dir))
(setq-default save-place t)
(put 'dired-find-alternate-file 'disabled nil)

(load-theme 'wombat t)
