(defvar root-dir (file-name-directory load-file-name))
(defvar config-dir (concat root-dir "config/"))
(defvar save-dir (concat root-dir "savefile/"))
(setq custom-file (concat config-dir "custom.el"))
(add-to-list 'load-path config-dir)
(add-to-list 'custom-theme-load-path "~/.emacs.d/config/themes")
(require 'packages)

;; Store backup and autosave files in tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(unless (file-exists-p save-dir)
  (make-directory save-dir))

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" root-dir))
(setq-default save-place t)
(require 'saveplace)
(when (eq system-type 'darwin)
  (require 'osx))
(require 'gui)
(require 'editor)
(require 'bindings)
(require 'plugins)
(require 'defuns)

