(defvar root-dir (file-name-directory load-file-name))
(defvar config-dir (concat root-dir "config/"))
(defvar plugins-dir (concat config-dir "plugins/"))
(defvar save-dir (concat root-dir "savefile/"))
(setq custom-file (concat config-dir "custom.el"))
(add-to-list 'load-path config-dir)
(add-to-list 'load-path plugins-dir)
(add-to-list 'custom-theme-load-path "~/.emacs.d/config/themes")
(require 'packages)

;; Store backup and autosave files in the systems /tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(unless (file-exists-p save-dir)
  (make-directory save-dir))

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" save-dir))
(setq-default save-place t)
(require 'saveplace)

(when (eq system-type 'darwin)
  (require 'osx))
(require 'ui)
(require 'editor)
(require 'bindings)
(require 'plugins)
(require 'defuns)
