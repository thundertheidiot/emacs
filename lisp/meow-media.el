;; -*- lexical-binding: t; -*-

(use-package empv
  :demand t
  :general-config
  (:keymaps 'empv-youtube-results-mode :states '(normal visual insert)
	    "RET" 'empv-youtube-results-play-current)
  (:keymaps 'embark-file-map
	    "p" #'empv-play-file)
  :custom
  (empv-invidious-instance "https://yewtu.be/api/v1")
  (empv-volume-step 3)
  :config
  (defun eshell/yt (&rest args)
    (empv-youtube (mapconcat (lambda (s) (format "%s " s)) args))))

(use-package transient)

(transient-define-prefix empv-menu ()
  "Transient menu for empv."
  [["Menu"
    ("q" "Quit" transient-quit-one)]
   ["Playback"
    ("p" "Toggle" empv-toggle :transient t)
    ("v" "Toggle Video" empv-toggle-video :transient t)

    ("j" "Previous" empv-playlist-prev :transient t)
    ("k" "Next" empv-playlist-next :transient t)

    ("x" "Close MPV" empv-exit :transient t)]
   ["Playlist"
    ("Y" "Search Youtube" empv-youtube)
    ("f" "Play File" empv-play-file)
    ("s" "Select From Playlist" empv-playlist-select)]
   ["Settings"
    ("y" "Toggle Single" empv-toggle-file-loop :transient t)
    ("r" "Toggle Repeat" empv-toggle-playlist-loop :transient t)
    
    ("-" "Volume Down" empv-volume-down :transient t)
    ("=" "Volume Up" empv-volume-up :transient t)]])

(defmacro media-menu--mpv-or-mpd-action (mpv mpd &optional mpv-args mpd-args)
  "If the mpv playlist is not empty, call MPV, else call MPD.
MPV is called with MPV-ARGS and MPD is called with MPD-ARGS."
  `(empv--send-command
    '("get_property_string" "playlist")
    (lambda (result)
      (if (> (length (json-parse-string result)) 0)
	  (apply ,mpv ,mpv-args)
	(apply ,mpd ,mpd-args)))))

(defun media-menu--toggle ()
  (interactive)
  (media-menu--mpv-or-mpd-action #'empv-toggle #'libmpdel-playback-play-pause))

(defun media-menu--volume-increase()
  (interactive)
  (media-menu--mpv-or-mpd-action #'empv-volume-up #'meow/mpd-volume-up))

(defun media-menu--volume-decrease()
  (interactive)
  (media-menu--mpv-or-mpd-action #'empv-volume-down #'meow/mpd-volume-down))

(transient-define-prefix media-menu ()
  "Transient menu for mpd and empv."
  [["Menu"
    ("q" "Quit" transient-quit-one)
    ("e" "Open MPV menu" empv-menu)
    ("m" "Open MPD menu" meow/mpd-transient-menu)]
   ["Playback"
    ("p" "toggle playback" media-menu--toggle :transient t)
    ]
   ["Settings"
    ("-" "Volume down" media-menu--volume-decrease :transient t)
    ("=" "Volume up" media-menu--volume-increase :transient t)]])

(meow/leader 
  "m" '("media menu" . media-menu))

(provide 'meow-media)
;;; meow-media.el ends here
