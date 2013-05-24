(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)
(setq ac-delay 0.1
      ac-auto-show-menu 0.2
      ac-quick-help-delay 2.5
      ac-ignore-case nil
      ac-limit 20)

(setq-default ac-sources
	      '(ac-source-imenu
		ac-source-words-in-buffer
		ac-source-words-in-same-mode-buffers
		ac-source-dictionary
		ac-source-filename))
(provide 'init-autocomplete)
