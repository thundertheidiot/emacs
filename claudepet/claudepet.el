;;; claudepet.el --- An animated, activity-aware Claude pet -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable `claudepet-mode' to display synchronized pets on graphical frames.
;; `claudepet-toggle' controls one frame; `claudepet-close' removes every pet.
;; Artwork and regression tests live alongside this library.
;; Register sprites and animations with `claudepet-put-sprite' and
;; `claudepet-define-animation'.  The animation wrappers accept hook arguments.
;; For optional notifications:
;; (add-hook 'org-wild-notifier-notification-hook #'claudepet-animation-notify)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'posframe)

(declare-function evil-local-mode "evil-core" (&optional arg))
(declare-function claudepet--register-animations "claudepet-animations" ())
(defvar claudepet--idle-bob-at)

;; Retire live callbacks before replacing either implementation on reload.
(when (featurep 'claudepet) (claudepet-close))

(defgroup claudepet nil "A pixel-art Claude companion." :group 'multimedia)
(defvar claudepet--image-cache nil "Images keyed by animation and pixel scale.")
(defcustom claudepet-scale 2
  "Image pixels per sprite pixel." :type 'integer :group 'claudepet)
(defcustom claudepet-frame-delay 0.12
  "Default seconds per animation frame." :type 'number :group 'claudepet)
(defcustom claudepet-sleep-after 300
  "Seconds of Emacs inactivity before sleeping." :type 'number :group 'claudepet)
(defcustom claudepet-palette
  '((?. "transparent" nil) (?o "claude-orange" "#D77757")
    (?b "black" "#000000") (?w "white" "#FFFFFF")
    (?p "pink" "#F19BA9") (?y "yellow" "#F5C542"))
  "Entries (CHAR NAME COLOR); nil COLOR means transparent.
Only colors actually used by a sprite appear in its XPM table.
Sprite characters must be printable ASCII other than quote and backslash."
  :type '(repeat (list character string (choice (const nil) string)))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq claudepet--image-cache nil))
  :group 'claudepet)
(defcustom claudepet-automatic-hooks nil
  "Hook/function pairs installed while the mode is enabled."
  :type '(repeat (cons symbol function)) :group 'claudepet)

(defvar claudepet-sprites (make-hash-table :test #'eq)
  "Sprite names mapped to lists of equally wide row strings.")
(defvar claudepet-animations (make-hash-table :test #'eq)
  "Animation names mapped to playback property lists.")
(defvar claudepet-mode nil "Non-nil when the global pet mode is enabled.")
(defvar claudepet--buffers nil "Buffers of displayed pet instances.")
(defvar claudepet--timer nil "Shared animation timer.")
(defvar claudepet--revert-timer nil "Timer ending the current transient animation.")
(defvar claudepet--animation nil "Current animation name.")
(defvar claudepet--click-callback nil "One-shot action for the current animation's click.")
(defvar claudepet--source nil "Source of the current animation.")
(defvar claudepet--event-source nil "Dynamically bound source of a playback request.")
(defvar claudepet-start-hook nil "Hook run after global mascot mode starts.")
(defvar claudepet-stop-hook nil "Hook run when global mascot mode stops.
Every handler is called even if an earlier handler fails.")
(defvar claudepet-resume-functions nil
  "Functions returning nil or (SOURCE . ANIMATION) for desired source activity.
Functions take no arguments.  Highest animation priority wins; hook order
breaks ties.  Unknown animations and failing providers are ignored.")
(defvar claudepet--poses nil "Current animation's resolved frame list.")
(defvar claudepet--pose-index 0 "Shared current frame index.")
(defvar claudepet--installed-hooks nil "Exact automatic hooks installed by the mode.")
(defvar claudepet--idle-blink-at 0 "Deadline for the next ambient blink.")
(defvar claudepet--idle-look-at 0 "Deadline for the next occasional idle look left.")
(defvar claudepet--wake-at 0 "Last explicit wake time, preventing immediate re-sleep.")
(defvar-local claudepet--parent-frame nil "This instance's parent frame.")
(defvar-local claudepet--posframe nil "This instance's child frame.")
(defvar-local claudepet--scale nil "This instance's pixel scale.")
(defvar-local claudepet--images nil "This instance's current image vector.")
(defvar-local claudepet--size nil "Last shown image size in pixels.")

(defun claudepet--validate-sprite (rows)
  "Validate rectangular ROWS and return them."
  (unless (and (consp rows) (stringp (car rows)) (> (length (car rows)) 0)
               (cl-every (lambda (row) (and (stringp row)
											(= (length row) (length (car rows))))) rows))
    (error "Sprite must contain equally wide, nonempty strings"))
  (dolist (row rows)
    (cl-loop for char across row do
             (unless (and (<= 32 char 126) (not (memq char '(?\" ?\\))))
               (error "Unsupported XPM sprite character: %S" char))))
  rows)

(defun claudepet-put-sprite (name rows)
  "Register a private copy of ROWS as sprite NAME."
  (puthash name (mapcar #'copy-sequence (claudepet--validate-sprite rows))
           claudepet-sprites))

(cl-defun claudepet-pose (&key (base 'claude) rects)
  "Return BASE with declarative RECTS (LEFT TOP RIGHT BOTTOM CHAR) applied.
BASE is a registered name or a sprite; its rows are never mutated."
  (let ((rows (mapcar #'copy-sequence
                      (claudepet--validate-sprite
                       (if (symbolp base) (gethash base claudepet-sprites) base)))))
    (dolist (rect rects)
      (cl-destructuring-bind (left top right bottom color) rect
        (unless (and (<= 0 left right (length (car rows)))
                     (<= 0 top bottom (length rows)))
          (error "Rectangle outside sprite: %S" rect))
        (cl-loop for y from top below bottom do
                 (cl-loop for x from left below right do
                          (aset (nth y rows) x color)))))
    rows))

(defun claudepet--stamp (sprite x y patch)
  "Return SPRITE with pixel PATCH placed at its top-left coordinate X, Y.
SPRITE is a registered name or a row list.  In PATCH, `.' leaves the
underlying pixel unchanged, `_' erases it to transparency, and other
characters paint it.  PATCH must fit entirely inside SPRITE.
Neither input is mutated; every returned row is a private copy."
  (let ((rows (mapcar #'copy-sequence
                      (claudepet--validate-sprite
                       (if (symbolp sprite) (gethash sprite claudepet-sprites) sprite))))
        (patch (claudepet--validate-sprite patch)))
    (unless (and (integerp x) (integerp y) (<= 0 x) (<= 0 y)
                 (<= (+ x (length (car patch))) (length (car rows)))
                 (<= (+ y (length patch)) (length rows)))
      (error "Pixel patch outside sprite at (%S, %S)" x y))
    (cl-loop for line in patch for target in (nthcdr y rows) do
             (cl-loop for char across line for column from x
                      unless (= char ?.) do
                      (aset target column (if (= char ?_) ?. char))))
    rows))

(defun claudepet-shift (sprite dy)
  "Return SPRITE shifted vertically by DY, keeping its dimensions."
  (let ((height (length sprite)) (width (length (car sprite))))
    (cl-loop for y below height collect
             (if (<= 0 (- y dy) (1- height))
                 (copy-sequence (nth (- y dy) sprite))
               (make-string width ?.)))))

(defun claudepet-xpm (&optional scale pixels)
  "Return an XPM for PIXELS at integer SCALE, defaulting to the base sprite."
  (setq scale (or scale claudepet-scale)
        pixels (claudepet--validate-sprite (or pixels (gethash 'claude claudepet-sprites))))
  (unless (and (integerp scale) (> scale 0))
    (error "Pixel scale must be a positive integer"))
  (let ((colors (delete-dups (apply #'append (mapcar #'string-to-list pixels)))))
    (concat "/* XPM */\nstatic char *claude[] = {\n"
            (format "\"%d %d %d 1\",\n" (* scale (length (car pixels)))
                    (* scale (length pixels)) (length colors))
            (mapconcat
             (lambda (char)
               (let ((entry (assq char claudepet-palette)))
                 (unless entry (error "No palette entry for %c" char))
                 (format "\"%c c %s\",\n" char (or (nth 2 entry) "None"))))
             colors "")
            (mapconcat
             (lambda (row)
               (let ((line (format "\"%s\""
                                   (mapconcat (lambda (char) (make-string scale char)) row ""))))
                 (mapconcat #'identity (make-list scale line) ",\n")))
             pixels ",\n") "\n};\n")))

(defun claudepet-image (&optional scale pixels)
  "Create an XPM image for PIXELS at SCALE."
  (create-image (claudepet-xpm scale pixels) 'xpm t :ascent 'center :scale 1))

(defun claudepet-insert (&optional scale &rest _)
  "Insert the base sprite at point; a numeric prefix selects SCALE."
  (interactive (list (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (condition-case nil (insert-image (claudepet-image scale) "Claude") (error nil)))

(defun claudepet-define-animation (name &rest properties)
  "Register NAME with PROPERTIES, including :frames, :delay and :priority.
:frames is a sprite list or a zero-argument function evaluated per cycle.
:duration ends playback after seconds; :next is a name or a function returning
a name or (SOURCE . ANIMATION).
:frame-delays optionally overrides :delay with seconds for individual frames.
:interruptible defaults to t; higher priorities always win."
  (unless (plist-member properties :interruptible)
    (setq properties (plist-put properties :interruptible t)))
  (puthash name properties claudepet-animations)
  (setq claudepet--image-cache nil)
  name)

(defun claudepet--resume ()
  "Return the highest-priority desired (SOURCE . ANIMATION), or (nil . idle)."
  (let (best priority)
    (dolist (provider claudepet-resume-functions)
      (let* ((request (ignore-errors (funcall provider)))
             (spec (and (consp request) (gethash (cdr request) claudepet-animations)))
             (value (and spec (or (plist-get spec :priority) 0))))
        (when (and spec (or (null best) (> value priority)))
          (setq best request priority value))))
    (or best '(nil . idle))))

(defun claudepet--play-next (next)
  "Force NEXT's animation, retaining source ownership in a sourced successor."
  (let* ((target (if (functionp next) (funcall next) next))
         (claudepet--event-source (and (consp target) (car target))))
    (claudepet-play (if (consp target) (cdr target) target) t)))

;; Read the sibling artwork again on reload, rather than reusing its feature.
(load (expand-file-name "claudepet-animations"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)
(claudepet--register-animations)

(defun claudepet-preview (name)
  "Show NAME's frames as a labeled sprite sheet without changing live playback.
Idle previews its blink, bob and look poses together, not long stationary holds."
  (interactive
   (list (intern (completing-read "Animation: "
                                  (hash-table-keys claudepet-animations) nil t nil nil "pet"))))
  (let* ((claudepet--idle-blink-at 0)
         (claudepet--idle-bob-at 0)
         (claudepet--idle-look-at 0)
         (claudepet-animations
          (let ((copy (make-hash-table :test #'eq)))
            (maphash (lambda (key value) (puthash key (copy-sequence value) copy))
                     claudepet-animations)
            copy))
         (frames (claudepet--frames name))
         (spec (gethash name claudepet-animations))
         (buffer (get-buffer-create "*ClaudePet Preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s: %d frames\n\n" name (length frames)))
        (cl-loop for frame in frames for index from 0 do
                 (let ((start (point))
                       (delay (or (nth index (plist-get spec :frame-delays))
                                  (plist-get spec :delay) claudepet-frame-delay)))
                   (insert (format "%d " (1+ index)))
                   (insert-image (claudepet-image claudepet-scale frame) "Claude")
                   (add-text-properties start (point)
                                        (list 'help-echo (format "Frame %d: %gs" (1+ index) delay)))
                   (insert (if (zerop (mod (1+ index) 3)) "\n\n" "    "))))
        (insert "\n\nFrames are in playback order.  Hover for frame timing.\n"))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun claudepet--frames (name)
  "Resolve and validate the frames of animation NAME."
  (let* ((spec (gethash name claudepet-animations))
         (value (plist-get spec :frames))
         (frames (if (functionp value) (funcall value) value)))
    (unless frames (error "Animation %s has no frames" name))
    (mapc #'claudepet--validate-sprite frames)
    (unless (cl-every (lambda (frame)
						(and (= (length frame) (length (car frames)))
                             (= (length (car frame)) (length (caar frames))))) frames)
      (error "Animation frames have different sizes"))
    frames))

(defun claudepet--images-for (scale)
  "Return the current animation's cached images at SCALE."
  (let* ((key (list claudepet--animation scale))
         (entry (assoc key claudepet--image-cache)))
    (or (cdr entry)
        (let ((images (vconcat (mapcar (lambda (pose) (claudepet-image scale pose))
                                       claudepet--poses))))
          (push (cons key images) claudepet--image-cache)
          images))))

(defun claudepet--start-animation (name &optional callback)
  "Start NAME unconditionally; the public API performs arbitration."
  (let* ((spec (gethash name claudepet-animations))
         (frames (claudepet--frames name))
         (delay (or (plist-get spec :delay) claudepet-frame-delay)))
    (unless (and (numberp delay) (> delay 0)) (error "Invalid animation delay"))
    (dolist (timer (list claudepet--timer claudepet--revert-timer))
      (when (timerp timer) (cancel-timer timer)))
    (remove-hook 'pre-command-hook #'claudepet-wake)
    (setq claudepet--timer nil claudepet--revert-timer nil
          claudepet--animation name claudepet--poses frames claudepet--pose-index 0
          claudepet--source claudepet--event-source
          claudepet--click-callback callback)
    (setq claudepet--image-cache
          (cl-remove-if (lambda (entry) (eq (caar entry) name)) claudepet--image-cache))
    (when (eq name 'sleep) (add-hook 'pre-command-hook #'claudepet-wake))
    (when-let* ((duration (plist-get spec :duration)))
      (setq claudepet--revert-timer (run-at-time duration nil #'claudepet--revert name)))
    (claudepet--tick)))

(defun claudepet-play (name &optional force callback &rest _)
  "Request animation NAME, optionally FORCE it, ignoring extra hook arguments.
CALLBACK, when a function, is called once with no arguments on a pet click.
Accepted requests replace any pending callback, even for the same animation.
Higher priority wins; equal priority wins if the current state is interruptible.
Requests are harmless when the mode is off or no mascot is displayed."
  (condition-case nil
      (when (and claudepet-mode claudepet--buffers)
        (when (eq claudepet--animation 'sleep) (claudepet-wake))
        (let* ((current (gethash claudepet--animation claudepet-animations))
               (new (gethash name claudepet-animations))
               (old-priority (or (plist-get current :priority) 0))
               (priority (or (plist-get new :priority) 0)))
          (when (and new
                     (or force (null current) (> priority old-priority)
                         (and (= priority old-priority) (plist-get current :interruptible))))
            ;; Streaming events must not keep restarting the same pose or timer.
            (if (or force (not (eq name claudepet--animation)))
                (claudepet--start-animation name (and (functionp callback) callback))
              (setq claudepet--click-callback (and (functionp callback) callback)))
            (setq claudepet--source claudepet--event-source)
            name)))
    (error nil)))

(defun claudepet-source-request (source name &optional callback)
  "Request NAME on behalf of SOURCE, optionally with click CALLBACK.
Normal animation priorities apply.  Return the accepted name, or nil.
Desired activity should be supplied independently via
`claudepet-resume-functions'."
  (let ((claudepet--event-source source))
    (claudepet-play name nil callback)))

(defun claudepet-source-release (source animations)
  "Retire non-nil SOURCE's playback only when it owns a member of ANIMATIONS.
Withdraw desired activity before calling this function.  Other sources, manual
playback and animations outside ANIMATIONS are untouched."
  (when (and source claudepet-mode claudepet--buffers
             (eq claudepet--source source) (memq claudepet--animation animations))
    (claudepet--play-next #'claudepet--resume)))

(defun claudepet--revert (name)
  "End transient NAME, bypassing priority arbitration for its successor."
  (when (eq name claudepet--animation)
    (setq claudepet--revert-timer nil)
    (condition-case nil
        (claudepet--play-next (or (plist-get (gethash name claudepet-animations) :next) 'idle))
      (error (claudepet--play-next 'idle)))))

(defun claudepet-wake (&rest _)
  "Wake with a wave and three quick blinks before idle, ignoring hook arguments."
  (interactive)
  (condition-case nil
      (when (and claudepet-mode claudepet--buffers (eq claudepet--animation 'sleep))
        (setq claudepet--wake-at (float-time))
        (remove-hook 'pre-command-hook #'claudepet-wake)
        (claudepet--start-animation 'wake))
    (error nil)))

(defun claudepet-animation-alert (&optional callback &rest _)
  "Loop an alert until acknowledged, optionally invoking CALLBACK on click."
  (interactive) (claudepet-play 'alert nil callback))
(defun claudepet-animation-thinking (&rest _) "Request thinking, safely from any hook." (interactive) (claudepet-play 'thinking))
(defun claudepet-animation-working (&rest _) "Request working, safely from any hook." (interactive) (claudepet-play 'working))
(defun claudepet-animation-tooling (&rest _) "Request tooling, safely from any hook." (interactive) (claudepet-play 'tooling))
(defun claudepet-animation-fly (&rest _) "Request flight, safely from any hook." (interactive) (claudepet-play 'fly))
(defun claudepet-animation-bike (&rest _) "Request cycling, safely from any hook." (interactive) (claudepet-play 'bike))
(defun claudepet-animation-binoculars (&rest _) "Request binocular scanning, safely from any hook." (interactive) (claudepet-play 'binoculars))
(defun claudepet-animation-look-left (&rest _)
  "Request a brief look left, safely from any hook."
  (interactive) (claudepet-play 'look-left))
(defun claudepet-animation-done (&optional callback &rest _)
  "Loop completion until acknowledged, optionally invoking CALLBACK on click."
  (interactive) (claudepet-play 'done nil callback))
(defun claudepet-animation-error (&optional callback &rest _)
  "Loop an error until acknowledged, optionally invoking CALLBACK on click."
  (interactive) (claudepet-play 'error nil callback))
(defun claudepet-animation-notify (&optional callback &rest _)
  "Loop notification until acknowledged, optionally invoking CALLBACK on click."
  (interactive) (claudepet-play 'notify nil callback))
(defun claudepet-animation-pet (&rest _) "Request happy hearts, safely from any hook." (interactive) (claudepet-play 'pet))

(defun claudepet--pet (&optional event &rest _)
  "Run playback's one-shot click action, or pet Claude when none is pending."
  (interactive "e")
  (let ((window (and (consp event) (posn-window (event-start event)))))
    (when (or (null event)
              (and (windowp window) (memq (window-buffer window) claudepet--buffers)))
      ;; Do not select the child window; only the callback may change focus.
      (let ((callback claudepet--click-callback))
        (setq claudepet--click-callback nil)
        (if callback (claudepet--play-next #'claudepet--resume) (claudepet-play 'pet t))
        (when callback
          (condition-case err
              (funcall callback)
            (error (message "Claude click callback failed: %s"
                            (error-message-string err)))))))))

(defvar claudepet--map
  (make-sparse-keymap)
  "Mouse bindings for mascot buffers.")
(dolist (event '(down-mouse-1 double-down-mouse-1 triple-down-mouse-1
							  drag-mouse-1 double-drag-mouse-1 triple-drag-mouse-1))
  (define-key claudepet--map (vector event) #'ignore))
(dolist (event '(mouse-1 double-mouse-1 triple-mouse-1))
  (define-key claudepet--map (vector event) #'claudepet--pet))

(defun claudepet--render (buffer)
  "Display the shared current pose in BUFFER, resizing if necessary."
  (with-current-buffer buffer
    (setq claudepet--images (claudepet--images-for claudepet--scale))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-image (aref claudepet--images claudepet--pose-index) "Claude")
      (add-text-properties (point-min) (point-max)
                           (list 'keymap claudepet--map 'pointer 'hand
                                 ))
      (goto-char (point-min)))
    (when (or (not (frame-live-p claudepet--posframe))
              (not (equal claudepet--size
                          (image-size (aref claudepet--images claudepet--pose-index)
                                      t claudepet--parent-frame))))
      (claudepet--show buffer))))

(defun claudepet--position (info)
  "Keep the requested child extent inside the parent using nonnegative coordinates."
  ;; Negative coordinates on PWAYL can resolve against stale child dimensions.
  (cons (max 0 (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width) 1))
        (max 0 (- (plist-get info :parent-frame-height)
                  (plist-get info :posframe-height)
                  (plist-get info :minibuffer-height)
                  (plist-get info :mode-line-height)))))

(defun claudepet--show (buffer)
  "Show or reposition BUFFER's mascot on its original parent frame."
  (with-selected-frame (buffer-local-value 'claudepet--parent-frame buffer)
    (with-current-buffer buffer
      (let ((posframe-mouse-banish-function #'ignore)
            (posframe-text-scale-factor-function (lambda (_) 0)))
        (text-scale-set 0)
        (let ((size (image-size (aref claudepet--images claudepet--pose-index) t)))
          (setq claudepet--posframe
                (posframe-show buffer :position 1
                               :width (ceiling (car size) (default-font-width))
                               :height (ceiling (cdr size) (default-line-height))
                               :override-parameters '((alpha-background . 0))
                               :poshandler #'claudepet--position
                               :left-fringe 0 :right-fringe 0 :border-width 0
                               :lines-truncate t :accept-focus nil)
                claudepet--size size))
        ;; Displaying a new buffer can initialize globalized modes again.
        (when (fboundp 'evil-local-mode) (evil-local-mode -1))))))

(defun claudepet--resize (frame)
  "Reanchor the mascot when parent FRAME's window layout changes."
  (let ((buffer (frame-parameter frame 'claudepet-buffer)))
    (when (and (buffer-live-p buffer)
               (frame-live-p (buffer-local-value 'claudepet--parent-frame buffer)))
      (claudepet--show buffer))))

(defun claudepet--frame-deleted (frame)
  "Remove only the mascot belonging to deleted FRAME."
  (when-let* ((buffer (frame-parameter frame 'claudepet-buffer))) (claudepet--stop buffer)))

(defun claudepet--buffer-killed ()
  "Remove this mascot without recursively killing its buffer."
  (claudepet--stop (current-buffer) t))

(defun claudepet--reset-playback ()
  "Cancel playback and remove frame and sleep callbacks."
  (dolist (timer (list claudepet--timer claudepet--revert-timer))
    (when (timerp timer) (cancel-timer timer)))
  (setq claudepet--timer nil claudepet--revert-timer nil claudepet--poses nil
        claudepet--animation nil claudepet--source nil claudepet--image-cache nil
        claudepet--click-callback nil
        claudepet--pose-index 0)
  (remove-hook 'pre-command-hook #'claudepet-wake)
  (remove-hook 'window-size-change-functions #'claudepet--resize)
  (remove-hook 'delete-frame-functions #'claudepet--frame-deleted))

(defun claudepet--stop (buffer &optional keep-buffer)
  "Remove BUFFER's mascot, retaining its buffer if KEEP-BUFFER."
  (setq claudepet--buffers (delq buffer claudepet--buffers))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (remove-hook 'kill-buffer-hook #'claudepet--buffer-killed t)
      (when (and (frame-live-p claudepet--parent-frame)
                 (eq (frame-parameter claudepet--parent-frame 'claudepet-buffer) buffer))
        (set-frame-parameter claudepet--parent-frame 'claudepet-buffer nil)))
    (condition-case err
        (if keep-buffer (posframe-delete-frame buffer) (posframe-delete buffer))
      (error (message "Cannot delete Claude instance: %s" (error-message-string err)))))
  (unless claudepet--buffers (claudepet--reset-playback)))

(defun claudepet--tick ()
  "Render one synchronized step and schedule the next shared tick."
  (setq claudepet--timer nil)
  (condition-case err
      (let* ((spec (gethash claudepet--animation claudepet-animations))
             (idle (current-idle-time)))
        (if (and idle (not (eq claudepet--animation 'sleep))
                 (>= (float-time idle) claudepet-sleep-after)
                 (>= (- (float-time) claudepet--wake-at) claudepet-sleep-after)
                 (< (or (plist-get spec :priority) 0) 10))
            (claudepet--start-animation 'sleep)
          (dolist (buffer (copy-sequence claudepet--buffers))
            (condition-case render-error
                (if (and (buffer-live-p buffer)
                         (frame-live-p (buffer-local-value 'claudepet--parent-frame buffer)))
                    (claudepet--render buffer) (claudepet--stop buffer))
              (error (claudepet--stop buffer)
                     (message "Claude instance stopped: %s" (error-message-string render-error)))))
          (when claudepet--buffers
            (setq claudepet--timer
                  (run-at-time (or (nth claudepet--pose-index (plist-get spec :frame-delays))
                                   (plist-get spec :delay) claudepet-frame-delay)
                               nil #'claudepet--advance)))))
    (error (claudepet-close)
           (message "Claude animation stopped: %s" (error-message-string err)))))

(defun claudepet--advance ()
  "Advance playback, rebuilding functional frames once per cycle."
  (setq claudepet--timer nil)
  (condition-case err
      (progn
        (setq claudepet--pose-index (mod (1+ claudepet--pose-index) (length claudepet--poses)))
        (when (and (zerop claudepet--pose-index)
                   (functionp (plist-get (gethash claudepet--animation claudepet-animations) :frames)))
          (setq claudepet--poses (claudepet--frames claudepet--animation)
                claudepet--image-cache
                (cl-remove-if (lambda (entry) (eq (caar entry) claudepet--animation))
                              claudepet--image-cache)))
        (claudepet--tick))
    (error (claudepet-close)
           (message "Claude animation stopped: %s" (error-message-string err)))))

(defun claudepet--eligible-p (frame)
  "Return non-nil if FRAME can host a mascot, excluding child frames."
  (and (frame-live-p frame) (display-graphic-p frame) (not (frame-parent frame))
       (not (frame-parameter frame 'tooltip))
       (not (eq (frame-parameter frame 'minibuffer) 'only))))

(defun claudepet--add (frame &optional scale)
  "Show a mascot in FRAME at SCALE, joining the current animation phase."
  (let ((existing (frame-parameter frame 'claudepet-buffer)))
    (if (and (buffer-live-p existing)
             (frame-live-p (buffer-local-value 'claudepet--posframe existing))) existing
      (when existing (claudepet--stop existing))
      (unless (claudepet--eligible-p frame) (user-error "Claude needs a graphical parent frame"))
      (with-selected-frame frame
        (unless (posframe-workable-p) (user-error "Posframes are unavailable on this frame")))
      (setq scale (or scale claudepet-scale))
      (unless (and (integerp scale) (> scale 0)) (user-error "Pixel scale must be positive"))
      (let ((buffer (generate-new-buffer " *claude*")))
        (condition-case err
            (progn
              (unless claudepet--poses
                (setq claudepet--animation 'idle claudepet--poses (claudepet--frames 'idle)))
              (with-current-buffer buffer
                (setq claudepet--parent-frame frame claudepet--scale scale)
                (setq-local buffer-read-only t line-spacing 0)
                (use-local-map claudepet--map)
                (when (fboundp 'evil-local-mode) (evil-local-mode -1))
                (add-hook 'kill-buffer-hook #'claudepet--buffer-killed nil t))
              (push buffer claudepet--buffers)
              (set-frame-parameter frame 'claudepet-buffer buffer)
              (claudepet--render buffer)
              (add-hook 'window-size-change-functions #'claudepet--resize)
              (add-hook 'delete-frame-functions #'claudepet--frame-deleted)
              (unless claudepet--timer (claudepet--tick))
              buffer)
          (error (claudepet--stop buffer) (signal (car err) (cdr err))))))))

(defun claudepet-toggle (&optional scale &rest _)
  "Toggle a mascot on the selected frame only, with optional pixel SCALE.
A hidden instance stays hidden until toggled or the mode is enabled again.
The global mode must be enabled before creating an instance."
  (interactive (list (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let* ((frame (selected-frame)) (buffer (frame-parameter frame 'claudepet-buffer)))
    (if (buffer-live-p buffer) (claudepet--stop buffer)
      (unless claudepet-mode (user-error "Enable claudepet-mode before toggling a mascot"))
      (claudepet--add frame scale))))

(defun claudepet--new-frame (frame)
  "Show a mascot in eligible new FRAME without restarting playback."
  (when (and claudepet-mode (claudepet--eligible-p frame))
    (condition-case err (claudepet--add frame)
      (error (message "Cannot show Claude: %s" (error-message-string err))))))

(define-minor-mode claudepet-mode
  "Show synchronized pets on all existing and future graphical frames.
Disabling removes all instances, automatic hooks, sources, and timers."
  :global t :group 'claudepet
  (if claudepet-mode
      (condition-case err
          (progn
            (add-hook 'after-make-frame-functions #'claudepet--new-frame)
            (dolist (frame (frame-list))
              (when (claudepet--eligible-p frame) (claudepet--add frame)))
            (dolist (entry claudepet-automatic-hooks)
              (unless (member (cdr entry) (and (boundp (car entry))
                                               (default-value (car entry))))
                (add-hook (car entry) (cdr entry))
                (cl-pushnew entry claudepet--installed-hooks :test #'equal)))
            (run-hooks 'claudepet-start-hook))
        (error (claudepet-close) (signal (car err) (cdr err))))
    (remove-hook 'after-make-frame-functions #'claudepet--new-frame)
    (dolist (entry claudepet--installed-hooks) (remove-hook (car entry) (cdr entry)))
    (setq claudepet--installed-hooks nil)
    (run-hook-wrapped 'claudepet-stop-hook
                      (lambda (function) (ignore-errors (funcall function)) nil))
    (dolist (buffer (copy-sequence claudepet--buffers)) (claudepet--stop buffer))
    (claudepet--reset-playback)))

(defun claudepet-close (&rest _)
  "Disable the mode, removing every mascot, watcher, hook and timer."
  (interactive) (claudepet-mode -1))

(provide 'claudepet)
;;; claudepet.el ends here
