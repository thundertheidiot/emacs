;; -*- lexical-binding: t; -*-
(use-package libmpdel)
(use-package transient)

(require 'consult)
(require 'embark)
(require 'libmpdel)
(require 'transient)
(require 'dash)

;; https://mpd.readthedocs.io/en/latest/protocol.html
;; the mpd documentation specifically recommends against this
;; this seems like the best way to get good search though

(defvar meow/mpd-song-cache '()
  "Cached list of the MPD library, preformatted for consult.")

(defun meow/--libmpdel-guard (&optional skip-cache)
  (unless (libmpdel-connected-p)
    (libmpdel--connect))
  (unless (or skip-cache meow/mpd-song-cache)
	(meow/mpd-cache-all)))

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
 (let ((length-before (libmpdel-playlist-length)))
   (libmpdel-current-playlist-add entity)
   (when (= length-before 0)
	 (libmpdel-play))))

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

;; claudeslop but it makes sense
(defun meow/--group-mpd-song-data (data)
  "Group flat MPD DATA into one alist per song, splitting on `file' keys.
Unlike `libmpdel-group-data', this ignores `directory' entries
entirely instead of using them (or whatever key happens to come
first) as the group boundary."
  (let (groups current)
    (dolist (kv data)
      (when (eq (car kv) 'file)
        (when current (push (nreverse current) groups))
        (setq current nil))
      (push kv current))
    (when current (push (nreverse current) groups))
    ;; drop any leading/trailing chunk that never got a 'file (e.g. a
    ;; directory's own Last-Modified line before the first song)
    (nreverse (seq-filter (lambda (g) (assq 'file g)) groups))))

(defun meow/--create-songs-from-data (data)
  "Like `libmpdel--create-songs-from-data' but robust to `listallinfo'output containing interleaved `directory:' entries.
Create a list of `libpdel-song' from DATA."
  (mapcar #'libmpdel--create-song-from-data
          (meow/--group-mpd-song-data data)))

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
				  meow/mpd-song-fields))
		 ;; entries with the same string get deduped, this prevents that
		 (unique-suffix (format " %s" (or (libmpdel--song-id song) (random)))))
    (apply
     #'propertize
	 (concat (string-join search-strings " ") unique-suffix)
     'display (concat
			   (when is-cur-song
				 "Now Playing - ")
			   (or title file))
     'consult--candidate song
     (when is-cur-song
       '(face success)))))

(defun meow/--mpd-annotate (song)
  "Annotate SONG for good marginalia integration."
  (when-let* ((song (ignore-errors (get-text-property 0 'consult--candidate song))))
    (format "   %s - %s"
			(or (ignore-errors (libmpdel-artist-name song)) "Unknown Artist")
			(or (ignore-errors (libmpdel-album-name song)) "Unknown Album"))))


(defun meow/mpd-populate-cache (&optional types callback)
  "Populate a singular cache entry in the song cache.
TYPES is a cons cell with the key as the car and the list of entries for `meow/--format-mpd-song'.
CALLBACK is called when done."
  (libmpdel-send-command
   "listallinfo"
   (lambda (data)
	 (let* ((meow/mpd-song-fields (or (cdr types) meow/mpd-song-fields))
			(songs (mapcar #'meow/--format-mpd-song (meow/--create-songs-from-data data))))
	   (if (assq (car types) meow/mpd-song-cache)
		   (setf (alist-get (car types) meow/mpd-song-cache) songs)
		 (push (cons (or (car types) 'all) songs) meow/mpd-song-cache)))
	 (when callback (funcall callback)))))

(defun meow/mpd-cache-all (&optional callback)
  "Cache all query types.  Call CALLBACK when done."
  (libmpdel-send-command
   "listallinfo"
   (lambda (data)
	 (let ((songdata (meow/--create-songs-from-data data)))
	   (dolist (types '((all . nil) (name . (title file)) (album . (album)) (artist . (artist)) (file . file)))
		 (let* ((meow/mpd-song-fields (or (cdr types) meow/mpd-song-fields))
				(songs (mapcar #'meow/--format-mpd-song songdata)))
		   (if (assq (car types) meow/mpd-song-cache)
			   (setf (alist-get (car types) meow/mpd-song-cache) songs)
			 (push (cons (car types) songs) meow/mpd-song-cache)))))
	 (when callback (funcall callback)))))

;; WIP progress bar
;; (defvar meow/mpd-progress-timer nil)
;; (defvar meow/mpd-progress-bar-active nil)
;; (defvar meow/mpd-progress-bar-string "")

;; (defun meow/mpd-progress-start ()
;;   (unless meow/mpd-progress-timer
;; 	(setq meow/mpd-progress-timer
;; 		  (run-with-timer 0 1 #'meow/--mpd-draw-progress-bar))))

;; (defun meow/mpd-progress-stop ()
;;   (when meow/mpd-progress-timer
;; 	(cancel-timer meow/mpd-progress-timer)
;; 	(setq meow/mpd-progress-timer nil)))

;; (defun meow/--mpd-draw-progress-bar ()
;;   (libmpdel-send-command
;;    "status"
;;    (lambda (data)
;; 	 (let ((elapsed (string-to-number (cdr (assoc 'elapsed data))))
;; 		   (duration (string-to-number (cdr (assoc 'duration data)))))
;; 	   (when (and elapsed duration)
;; 		 (let ((fraction (/ elapsed duration))
;; 			   (len 64))
;; 		   (setq meow/mpd-progress-bar-string
;; 				 (message "[%s%s]"
;; 						  (propertize
;; 						   (concat (make-string (1- (floor (* len fraction))) ?=)
;; 								   ">")
;; 						   'face `(:foreground ,(batppuccin-get-color "bat-green")))
;; 						  (make-string (floor (* len (- 1 fraction))) ?-)))))))))

(defun meow/mpd-search ()
  "Search through songs with consult."
  (interactive)
  (meow/--libmpdel-guard)
  ;; TODO is there a better way?
  (let ((consult-async-split-style 'none)
		(vertico-sort-override-function #'identity)
		(completion-ignore-case t))
    (consult--multi
	 (list
	  `(:name "All"
			  :category mpd
			  :narrow ?q
			  :sort nil
			  :annotate ,#'meow/--mpd-annotate
			  :action ,#'meow/mpd-add-song
			  :items ,(cdr (assoc 'all meow/mpd-song-cache)))
	  `(:name "Name"
			  :category mpd
			  :narrow ?n
			  :hidden t
			  :sort nil
			  :annotate ,#'meow/--mpd-annotate
			  :action ,#'meow/mpd-add-song
			  :items ,(cdr (assoc 'name meow/mpd-song-cache)))
	  `(:name "Album"
			  :category mpd
			  :narrow ?a
			  :hidden t
			  :sort nil
			  :annotate ,#'meow/--mpd-annotate
			  :action ,#'meow/mpd-add-song
			  :items ,(cdr (assoc 'album meow/mpd-song-cache)))
	  `(:name "Artist"
			  :category mpd
			  :narrow ?A
			  :hidden t
			  :sort nil
			  :annotate ,#'meow/--mpd-annotate
			  :action ,#'meow/mpd-add-song
			  :items ,(cdr (assoc 'artist meow/mpd-song-cache)))
	  `(:name "Filename"
			  :category mpd
			  :narrow ?f
			  :hidden t
			  :sort nil
			  :annotate ,#'meow/--mpd-annotate
			  :action ,#'meow/mpd-add-song
			  :items ,(cdr (assoc 'file meow/mpd-song-cache))))
	 :prompt "Search MPD (q/n/a/A/f): "
	 :require-match t)))

(defun meow/mpd-queue ()
  "MPD Playlist view with consult."
  (interactive)
  (meow/--libmpdel-guard)
  (libmpdel-list-songs
   'current-playlist
   (lambda (songs)
     (let* ((vertico-sort-override-function #'identity)
			(meow/mpd-progress-bar-active t)
			(candidate
			 (consult--read
			  (if-let* ((cur (libmpdel-current-song))
						(id (libmpdel--song-id cur)))
				  (--map (meow/--format-mpd-song it id)
						 songs)
				(-map #'meow/--format-mpd-song songs))
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
     (let* ((playlist (consult--read
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

(defun meow/mpd-database-update ()
  "Update database."
  (interactive)
  (libmpdel-database-update)
  (message "Updating database...")
  (meow/mpd-cache-all))

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
    ("r" "Toggle repeat" meow/mpd-toggle-repeat :transient t)
    ("u" "update" meow/mpd-database-update :transient t)

    ("-" "Volume down" meow/mpd-volume-down :transient t)
    ("=" "Volume up" meow/mpd-volume-up :transient t)]])

(provide 'meow-mpd)
;;; meow-mpd.el ends here
