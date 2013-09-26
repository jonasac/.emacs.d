;; If mplayer is installed on the machine, we'd like to listen to the radio
;; M-x emms-streams gives you a list of avaliable streams, adding your own bookmakrk file is done via ~/.emacs.d/emms/streams file
(when (executable-find "mplayer")
    (jac-ensure-packages-installed '(emms))
    (require 'emms-streams)
    (require 'emms-player-mplayer)

    ;; We only need streaming capabilities, so just the playlist player
    (setq emms-player-list '(emms-player-mplayer-playlist))

    ;; Pressing enter in the emms-streams interface starts streaming instead of adding stream to playlist
    (setq emms-stream-default-action "play"))

(provide 'jac-emms)
