;; -*- lexical-binding: t; -*-
(use-package libmpdel)
(use-package transient)

(require 'consult)
(require 'embark)
(require 'libmpdel)
(require 'transient)

(defun meow/--libmpdel-guard ()
  (unless (libmpdel-connected-p)
    (libmpdel--connect)))

(defmacro meow/mpd-wrapper (name &rest forms)
  "Create a wrapper for a libmpdel function, supporting a string entity from consult.
The function is named `meow/mpd-NAME', FORMS are executed with entity bound."
  `(defun ,(intern (format "meow/mpd-%s" name)) (entity)
     (when-let ((entity
		 (if (stringp entity)
		     (get-text-property 0 'consult--candidate entity)
		   entity)))
       ,@forms)))

(meow/mpd-wrapper
 "add-song"
 (libmpdel-current-playlist-add entity))

(meow/mpd-wrapper
 "play-song"
 (libmpdel-play-song entity))

(meow/mpd-wrapper
 "delete-song"
 (libmpdel-playlist-delete (list entity) 'current-playlist))

(meow/mpd-wrapper
 "replace-playlist"
 (libmpdel-current-playlist-replace entity))

(meow/mpd-wrapper
 "delete-playlist"
 (libmpdel-stored-playlists-delete (list entity)))

(meow/mpd-wrapper
 "add-playlist"
 (libmpdel-current-playlist-add entity))

(meow/mpd-wrapper
 "save-playlist"
 (let ((name (libmpdel--stored-playlist-name entity)))
   (libmpdel-stored-playlists-delete (list entity))
   (libmpdel-playlist-save name)))

(defvar meow/embark-mpd-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'meow/mpd-add-song)
    (define-key map (kbd "A") #'embark-act-all)
    map))

(defvar meow/embark-mpd-queue-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'meow/mpd-play-song)
    (define-key map (kbd "d") #'meow/mpd-delete-song)
    (define-key map (kbd "A") #'embark-act-all)
    map))

(defvar meow/embark-mpd-playlist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'meow/mpd-replace-playlist)
    (define-key map (kbd "a") #'meow/mpd-add-playlist)
    (define-key map (kbd "d") #'meow/mpd-delete-playlist)
    (define-key map (kbd "s") #'meow/mpd-save-playlist)
    (define-key map (kbd "A") #'embark-act-all)
    map))

(add-to-list 'embark-keymap-alist '(mpd . meow/embark-mpd-map))
(add-to-list 'embark-keymap-alist '(mpd-queue . meow/embark-mpd-queue-map))
(add-to-list 'embark-keymap-alist '(mpd-playlist . meow/embark-mpd-playlist-map))

(defvar meow/mpd-song-fields '(title artist album file)
  "Fields to include in the searchable text of a formatted MPD song.
Don't override this, it's let bound in every relevant case.")

(defun meow/--format-mpd-song (song &optional cur-id)
  "Format libmpdel song SONG for consult.
Highlight the song with CUR-ID."
  (let* ((file (or (libmpdel--song-file song) ""))
	 (title (libmpdel--song-name song))
	 (artist (or (libmpdel-artist-name song) ""))
	 (album (or (libmpdel-album-name song) ""))
	 (is-cur-song (and cur-id
			   (string= (libmpdel--song-id song) cur-id)))
	 (search-strings
	  (mapcar (lambda (i)
		    (pcase i
		      ('title (or title ""))
		      ('artist artist)
		      ('album album)
		      ('file file)))
		  meow/mpd-song-fields)))
    (apply
     #'propertize
     (string-join search-strings " ")
     'display (string-join (list
			    (when is-cur-song
			      "Now Playing - ")
			    (or title file)))
     'consult--candidate song
     (when is-cur-song
       '(face success)))))

(defun meow/--mpd-annotate (song)
  "Annotate SONG for good marginalia integration."
  (let ((song (get-text-property 0 'consult--candidate song)))
    (format "   %s - %s"
	    (or (ignore-errors (libmpdel-artist-name song)) "Unknown Artist")
	    (or (ignore-errors (libmpdel-album-name song)) "Unknown Album"))))

(defun meow/--async-mpd-search (type)
  "Create asynchoronus consult search for mpd.
TYPE is a `libmpdel-search-criteria' type."
  (lambda (sink)
    (lambda (action)
      (pcase action
	((pred stringp)

	 (funcall sink 'flush)
	 (libmpdel-list-songs
	  (libmpdel-search-criteria-create :type type :what action)
	  (lambda (songs)
	    (funcall sink (mapcar #'meow/--format-mpd-song songs))
	    (funcall sink 'refresh))))
	(_ (funcall sink action))))))

(defun meow/mpd-search ()
  "Search through songs with consult."
  (interactive)
  (meow/--libmpdel-guard)
  ;; TODO is there a better way?
  (libmpdel-send-command
   "listallinfo"
   (lambda (data)
     (let ((consult-async-split-style 'none)
	   (completion-ignore-case t))
       (consult--multi
	(list `(:name "Any"
		      :category mpd
		      :enabled ,(lambda () (not consult--narrow))
		      :annotate ,#'meow/--mpd-annotate
		      :action ,#'meow/mpd-add-song
		      :items ,(mapcar #'meow/--format-mpd-song
				      (libmpdel--create-songs-from-data data))
		      :sort nil)
	      `(:name "Album"
		      :category mpd
		      :narrow ?a
		      :hidden t
		      :annotate ,#'meow/--mpd-annotate
		      :action ,#'meow/mpd-add-song
		      :async ,(consult--async-pipeline
			       (consult--async-throttle)
			       (meow/--async-mpd-search "album")))
	      `(:name "Artist"
		      :category mpd
		      :narrow ?A
		      :hidden t
		      :annotate ,#'meow/--mpd-annotate
		      :action ,#'meow/mpd-add-song
		      :async ,(consult--async-pipeline
			       (consult--async-throttle)
			       (meow/--async-mpd-search "artist")))
	      `(:name "Filename"
		      :category mpd
		      :narrow ?f
		      :hidden t
		      :annotate ,#'meow/--mpd-annotate
		      :action ,#'meow/mpd-add-song
		      :async ,(consult--async-pipeline
			       (consult--async-throttle)
			       (meow/--async-mpd-search "filename"))))
	:prompt "Search MPD (a/A/f): "
	:require-match t)))))

(defun meow/mpd-queue ()
  "MPD Playlist view with consult."
  (interactive)
  (meow/--libmpdel-guard)
  (libmpdel-list-songs
   'current-playlist
   (lambda (songs)
     (let ((candidate
	    (consult--read (if-let* ((cur (libmpdel-current-song))
				     (id (libmpdel--song-id cur))
				     (meow/mpd-song-fields '(title file)))
			       (mapcar (lambda (s)
					 (meow/--format-mpd-song s id))
				       songs)
			     (mapcar #'meow/--format-mpd-song songs))
			   :annotate #'meow/--mpd-annotate
			   :category 'mpd-queue
			   :sort nil
			   :lookup #'consult--lookup-candidate
			   :require-match t)))
       (meow/mpd-play-song candidate)))))

(defun meow/mpd-load-playlist ()
  "Load a saved MPD playlist with consult.
Doubles up as a generic playlist selector, which you can embark with."
  (interactive)
  (meow/--libmpdel-guard)
  (libmpdel-list
   'stored-playlists
   (lambda (playlists)
     (let ((candidate
	    (consult--read (mapcar (lambda (p)
				     (propertize
				      (libmpdel--stored-playlist-name p)
				      'consult--candidate p))
				   playlists)
			   :category 'mpd-playlist
			   :lookup #'consult--lookup-candidate
			   :require-match t)))
       (meow/mpd-replace-playlist candidate)
       (libmpdel-play)))))

(defun meow/mpd-save-current-playlist ()
  "Save the current playlist."
  (interactive)
  (meow/--libmpdel-guard)
  (libmpdel-list
   'stored-playlists
   (lambda (playlists)
     (let ((playlist (consult--read
		      (mapcar (lambda (p)
				(propertize (libmpdel--stored-playlist-name p)
					    'consult--candidate p))
			      playlists)
		      :prompt "Name: "
		      :category 'mpd-playlist
		      :lookup (lambda (selected candidates &rest _)
				(or (consult--lookup-candidate selected candidates)
				    selected)))))
       (if (stringp playlist)
	   (libmpdel-playlist-save playlist)
	 (let ((name (libmpdel--stored-playlist-name playlist)))
	   (libmpdel-stored-playlists-delete (list playlist))
	   (libmpdel-playlist-save name)))))))

(defun meow/mpd-toggle-single ()
  "Toggle single mode."
  (interactive)
  (if (string= (libmpdel-single) "forever")
      (progn
	(libmpdel-playback-set-single-never)
	(message "Single off"))
    (progn
      (libmpdel-playback-set-single-forever)
      (message "Single on"))))

(defun meow/mpd-toggle-repeat ()
  "Toggle repeat mode."
  (interactive)
  (if (libmpdel-repeat)
      (progn
	(libmpdel-playback-unset-repeat)
	(message "Repeat off"))
    (progn
      (libmpdel-playback-set-repeat)
      (message "Repeat on"))))

(defvar meow/mpd-volume-step 3)
(defun meow/mpd-volume-down ()
  "Move volume down by volume step."
  (interactive)
  (let ((volume (- (string-to-number (libmpdel-volume)) meow/mpd-volume-step)))
    (libmpdel-playback-set-volume volume)
    (message "Volume %d" volume)))

(defun meow/mpd-volume-up ()
  "Move volume up by volume step."
  (interactive)
  (let ((volume (+ (string-to-number (libmpdel-volume)) meow/mpd-volume-step)))
    (libmpdel-playback-set-volume volume)
    (message "Volume %d" volume)))

(transient-define-prefix meow/mpd-transient-menu ()
  [["Menu"
    ("q" "Quit" transient-quit-one)]
   ["Playback"
    ("p" "Toggle" libmpdel-playback-play-pause :transient t)
    ("j" "Previous" libmpdel-playback-previous :transient t)
    ("k" "Next" libmpdel-playback-next :transient t)]
   ["Playlist"
    ("l" "Load" meow/mpd-load-playlist)
    ("s" "Search" meow/mpd-search)

    ("S" "Save" meow/mpd-save-current-playlist)

    ("c" "View playlist" meow/mpd-queue)
    ("C" "Clear playlist" (lambda () (interactive)
			    (libmpdel-playlist-clear 'current-playlist)
			    (message "Playlist cleared"))
     :transient t)]
   ["Settings"
    ("y" "Toggle single" meow/mpd-toggle-single :transient t)
    ("r" "Toggle repeat" meow/mpd-toggle-single :transient t)

    ("-" "Volume down" meow/mpd-volume-down :transient t)
    ("=" "Volume up" meow/mpd-volume-up :transient t)]])

(provide 'meow-mpd)
;;; meow-mpd.el ends here
