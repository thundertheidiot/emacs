;;; claudepet-animations.el --- Literal Claude pixel poses -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by the sibling claudepet.el after its registration APIs are defined.  Call
;; `claudepet--register-animations' after each load, including core reloads.
;; Standing poses use 40 by 32 canvases with a 28 by 18 resting torso.
;; Their authored 2 by 2 body blocks, eyes, palms and feet are not rescaled.
;; Flight uses its own 76 by 40 canvas for the plane and propeller.
;; Cycling uses a 56 by 40 canvas; binoculars pads standing poses to 52 by 42.
;; Front views have square corners; only side profiles have stepped corners.
;; Headroom variants reuse a named body; flight variants flutter, bank and bob.
;; Pixel patches use `.' to keep the base pixel and `_' to erase it.
;; Other palette characters paint pixels; stamp coordinates place the top-left.
;; Loading this module alone does not register assets or require the core recursively.

;;; Code:

(require 'cl-lib)

(declare-function claudepet-put-sprite "claudepet" (name rows))
(declare-function claudepet-define-animation "claudepet" (name &rest properties))
(declare-function claudepet--resume "claudepet" ())
(declare-function claudepet--stamp "claudepet" (sprite x y patch))
(declare-function claudepet-shift "claudepet" (sprite dy))
(defvar claudepet-sprites)
(defvar claudepet-animations)
(defvar claudepet-palette)
(defvar claudepet--idle-blink-at)
(defvar claudepet--idle-bob-at 0 "Deadline for the next occasional idle bob.")
(defvar claudepet--idle-look-at)

(defconst claudepet--empty-headroom
  '("...................................."
    "...................................."
    "...................................."
    "...................................."
    "...................................."
    "....................................")
  "Six literal transparent rows above the body.")

(defun claudepet--put-pose (name body &optional headroom)
  "Register NAME with coarse BODY and full-resolution HEADROOM.
Literal BODY is an 18 by 12 grid expanded twofold in both directions.
A registered pose name reuses its full-size body without expanding again.
HEADROOM defaults to six transparent rows.  Longer glyphs may extend over
the body's empty upper rows, as with the outlined sleeping Zs.
Registration then redraws torso spacing without scaling the small features."
  (let ((headroom (or headroom claudepet--empty-headroom))
        (rows (if (symbolp body)
                  (nthcdr 6 (or (gethash body claudepet-sprites)
                                (error "Unknown body pose: %s" body)))
                (unless (and (= (length body) 12)
                             (cl-every (lambda (row) (and (stringp row) (= (length row) 18)))
                                       body))
                  (error "Body pose must be an 18 by 12 grid: %s" name))
                (cl-loop for row in body append
                         (make-list 2 (mapconcat (lambda (char) (make-string 2 char)) row ""))))))
    (claudepet-put-sprite name (append headroom (nthcdr (- (length headroom) 6) rows)))))

(defun claudepet--idle-frames ()
  "Return a quiet idle cycle, updating its per-frame delays every time.
Blink every 3-8 seconds, bob every 15-30 and look left every 30-60.
When gestures coincide, play blink, bob, then look.  Idle never waves."
  (let ((now (float-time)) frames delays)
    (when (>= now claudepet--idle-blink-at)
      (setq frames (list (gethash 'blink claudepet-sprites)
                         (gethash 'claude claudepet-sprites))
            delays '(0.12 0.88)
            claudepet--idle-blink-at (+ now 3 (random 6))))
    (when (>= now claudepet--idle-bob-at)
      (setq frames (append frames (list (gethash 'bob claudepet-sprites)
										(gethash 'claude claudepet-sprites)))
            delays (append delays '(0.35 0.65))
            claudepet--idle-bob-at (+ now 15 (random 16))))
    (when (>= now claudepet--idle-look-at)
      (setq frames (append frames (plist-get (gethash 'look-left claudepet-animations) :frames))
            delays (append delays (plist-get (gethash 'look-left claudepet-animations) :frame-delays))
            claudepet--idle-look-at (+ now 30 (random 31))))
    (unless frames
      (setq frames (list (gethash 'claude claudepet-sprites)) delays '(1.0)))
    (setf (cl-getf (gethash 'idle claudepet-animations) :frame-delays) delays)
    frames))

(defun claudepet--register-animations ()
  "Register all sprites and built-in animations.
Replace built-in entries without clearing user assets.  Reset independent
idle deadlines into the future so registration never causes an immediate
blink, bob or look.  The core retains responsibility for stopping live playback
before a reload and for implementing `claudepet--resume'."
  ;; Extend old/custom palettes on reload without replacing their colors.
  (dolist (entry '((?r "leather" "#61462F") (?t "leather-light" "#9C7447")
                   (?g "goggle-shadow" "#A6A19B") (?s "goggle-glass" "#E2DED7")
                   (?a "claude-shadow" "#BB6649") (?v "bike-purple" "#8B80B8")
                   (?k "bike-tire" "#302E28")
                   (?u "binoculars-blue" "#164A70") (?l "binoculars-glint" "#347FAB")))
    (unless (assq (car entry) claudepet-palette)
      (setq claudepet-palette (append claudepet-palette (list (copy-sequence entry))))))
  (claudepet--put-pose
   'claude
   '("...oooooooooooo..."
     "...oooooooooooo..."
     "...oobboooobboo..."
     "...oobboooobboo..."
     "oooooooooooooooooo"
     "oooooooooooooooooo"
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     ".................."))
  (claudepet--put-pose
   'blink
   '("...oooooooooooo..."
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oobboooobboo..."
     "oooooooooooooooooo"
     "oooooooooooooooooo"
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     ".................."))
  (claudepet--put-pose
   'look-left-turn
   '("...oooooooooooo..."
     "...oooooooooooo..."
     "...obboooobbooo..."
     "...obboooobbooo..."
     "ooooooooooooooooo."
     "ooooooooooooooooo."
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     ".................."))
  ;; Keep both eyes square; only the back edge recedes as the gaze turns left.
  (claudepet--put-pose
   'look-left
   '("...ooooooooooo...."
     "...oooooooooooo..."
     "...obbooobboooo..."
     "...obbooobboooo..."
     "oooooooooooooooo.."
     "oooooooooooooooo.."
     "...ooooooooooo...."
     "...ooooooooooo...."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     ".................."))
  ;; The head compresses and widens while all four feet stay planted.
  (claudepet--put-pose
   'bob
   '(".................."
     "..oooooooooooooo.."
     "..oooooooooooooo.."
     "..ooobboooobbooo.."
     "..ooobboooobbooo.."
     "oooooooooooooooooo"
     "oooooooooooooooooo"
     "..oooooooooooooo.."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     ".................."))
  (claudepet--put-pose
   'wave-raised
   '("..oooooooooooo...."
     "..oooooooooooo.oo."
     "..oobboooobboo.oo."
     "..oobboooobbooooo."
     "oooooooooooooo...."
     "oooooooooooooo...."
     "..oooooooooooo...."
     "..oooooooooooo...."
     "..oo.oo..oo.oo...."
     "..oo.oo..oo.oo...."
     ".................."
     ".................."))
  (claudepet--put-pose
   'wave-upright
   '("..oooooooooooo.oo."
     "..oooooooooooo.oo."
     "..oobboooobboo.oo."
     "..oobboooobbooooo."
     "oooooooooooooo...."
     "oooooooooooooo...."
     "..oooooooooooo...."
     "..oooooooooooo...."
     "..oo.oo..oo.oo...."
     ".........oo.oo...."
     ".................."
     ".................."))
  (claudepet--put-pose
   'wave-outward
   '("..oooooooooooo..oo"
     "..oooooooooooo..oo"
     "..oobboooobboo.oo."
     "..oobboooobbooooo."
     "oooooooooooooo...."
     "oooooooooooooo...."
     "..oooooooooooo...."
     "..oooooooooooo...."
     "..oo.oo..oo.oo...."
     ".oo......oo.oo...."
     ".................."
     ".................."))
  (claudepet--put-pose
   'alert-raised
   '("...oooooooooooo..."
     "oo.oooooooooooo.oo"
     "oo.oobboooobboo.oo"
     "ooooobboooobbooooo"
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     ".................."
     ".................."))
  ;; A shorter stem leaves room for a complete outline inside six headroom rows.
  (claudepet--put-pose
   'alert 'alert-raised
   '("................bbb................."
     "................bwb................."
     "................bwb................."
     "................bbb................."
     "................bwb................."
     "................bbb................."))
  (claudepet--put-pose
   'notify
   '("...oooooooooooo..."
     "...obbboooobbbo..."
     "...obbboooobbbo..."
     "...oooooooooooo..."
     "oooooooooooooooooo"
     "oooooooooooooooooo"
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo......"
     ".................."
     ".................."))
  (claudepet--put-pose
   'error
   '(".................."
     "..oooooooooooooo.."
     "..oooooooooooooo.."
     "..oobobooooboboo.."
     "..oooboooooobooo.."
     "oooobobooooboboooo"
     "oooooooooooooooooo"
     "..oooooooooooooo.."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     ".................."))
  (claudepet--put-pose
   'error-warning 'error
   '("................bbb................."
     "................byb................."
     "................byb................."
     "................bbb................."
     "................byb................."
     "................bbb................."))
  (claudepet--put-pose
   'thinking-one
   '("...oooooooooooo..."
     "...oooooooooooo..."
     "...obboooobbooo..."
     "...obboooobbooooo."
     "oooooooooooooooooo"
     "oooooooooooooooooo"
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     "..................")
   '("...................................."
     "...................................."
     ".............ww....................."
     ".............ww....................."
     "...................................."
     "...................................."))
  (claudepet--put-pose
   'thinking-two 'thinking-one
   '("...................................."
     "...................................."
     ".............ww..ww................."
     ".............ww..ww................."
     "...................................."
     "...................................."))
  (claudepet--put-pose
   'thinking-three
   '("...oooooooooooo..."
     "...oooooooooooo..."
     "...ooobboooobbo..."
     "...ooobboooobbooo."
     "oooooooooooooooooo"
     "oooooooooooooooooo"
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     "..................")
   '("...................................."
     "...................................."
     ".............ww..ww..ww............."
     ".............ww..ww..ww............."
     "...................................."
     "...................................."))
  ;; Contented eyes are small arches; the higher heart accompanies a squish.
  (claudepet--put-pose
   'pet
   '("...oooooooooooo..."
     "...oooooooooooo..."
     "...oobooooooboo..."
     "...oboboooobobo..."
     "oooooooooooooooooo"
     "oooooooooooooooooo"
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     "..................")
   '("...................................."
     "...............pp.pp................"
     "..............ppppppp..............."
     "...............ppppp................"
     "................ppp................."
     ".................p.................."))
  (claudepet--put-pose
   'pet-high
   '(".................."
     "..oooooooooooooo.."
     "..oooooooooooooo.."
     "..oooboooooobooo.."
     "..oobobooooboboo.."
     "oooooooooooooooooo"
     "oooooooooooooooooo"
     "..oooooooooooooo.."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     "..................")
   '("...............pp.pp................"
     "..............ppppppp..............."
     "...............ppppp................"
     "................ppp................."
     ".................p.................."
     "...................................."))
  ;; The lifted head occupies two headroom rows; its heart floats to one side.
  (claudepet--put-pose
   'done-hop
   '("oo.oooooooooooo.oo"
     "oo.oobooooooboo.oo"
     "oooobobooooboboooo"
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oooooooooooo..."
     "...oo.oo..oo.oo..."
     "...oo.oo..oo.oo..."
     ".................."
     ".................."
     "..................")
   '("..pp.pp............................."
     ".ppppppp............................"
     "..ppppp............................."
     "...ppp.............................."
     "....p.oooooooooooooooooooooooo......"
     "......oooooooooooooooooooooooo......"))
  ;; Tucked feet keep the sleeping silhouette low; breathing lifts only the torso.
  ;; The Z outlines start in the last headroom row and stay clear of the body.
  (claudepet--put-pose
   'sleep
   '(".................."
     ".................."
     ".................."
     ".................."
     "..oooooooooooooo.."
     "..ooobboooobbooo.."
     "oooooooooooooooooo"
     "oooooooooooooooooo"
     "..oooooooooooooo.."
     "...oo.oo..oo.oo..."
     ".................."
     "..................")
   '("...................................."
     "...................................."
     "...................................."
     "...................................."
     "...................................."
     "........................bbbbb......."
     "........................bwwwb......."
     "........................bbbwb......."
     "........................bbwbb......."
     "........................bwbbb......."
     "........................bwwwb......."
     "........................bbbbb......."))
  (claudepet--put-pose
   'sleep-zzz
   '(".................."
     ".................."
     ".................."
     "..oooooooooooooo.."
     "..ooobboooobbooo.."
     "..oooooooooooooo.."
     "oooooooooooooooooo"
     "oooooooooooooooooo"
     "..oooooooooooooo.."
     "...oo.oo..oo.oo..."
     ".................."
     "..................")
   '("...................................."
     "...................................."
     "...................................."
     "...................................."
     "...................................."
     "........................bbbbbbbbbb.."
     "........................bwwwbbwwwb.."
     "........................bbbwbbbbwb.."
     "........................bbwbbbbwbb.."
     "........................bwbbbbwbbb.."
     "........................bwwwbbwwwb.."
     "........................bbbbbbbbbb.."))
  ;; Add one sprite-pixel to the leg stems without widening or flattening the feet.
  (dolist (spec '((22 claude blink bob wave-raised wave-upright wave-outward
                      notify error error-warning
                      thinking-one thinking-two thinking-three pet pet-high
                     look-left-turn look-left)
                 (20 alert-raised alert done-hop) (24 sleep sleep-zzz)))
    (dolist (name (cdr spec))
      (let* ((sprite (gethash name claudepet-sprites))
             (cut (+ (car spec) 2)))
        (claudepet-put-sprite
         name (append (cl-subseq sprite 0 cut) (list (nth (car spec) sprite))
                      (cl-subseq sprite cut (1- (length sprite))))))))
  ;; Add orange torso space between the eyes and below the body, not pixel scale.
  ;; Side profiles insert before the eyes; feet retain their spacing and width.
  (dolist (spec '((22 18 6 center claude blink look-left-turn look-left bob
                     wave-raised wave-upright wave-outward notify error error-warning
                     thinking-one thinking-two thinking-three pet pet-high)
                 (20 18 6 center alert-raised alert)
                 (20 18 0 center done-hop)
                 (24 18 12 center sleep sleep-zzz)))
    (cl-destructuring-bind (feet split headroom align &rest names) spec
      (dolist (name names)
        (let ((rows
               (cl-loop for row in (gethash name claudepet-sprites) for y from 0 collect
                        (cond
                         ((>= y feet) (concat ".." row ".."))
                         ((< y headroom)
                          (if (eq align 'right) (concat "...." row)
                            (concat ".." row "..")))
                         (t (concat (substring row 0 split)
                                    (make-string 4 (if (= (aref row (1- split)) ?o) ?o ?.))
                                    (substring row split)))))))
          (claudepet-put-sprite
           name (append (cl-subseq rows 0 feet) (make-list 2 (nth (1- feet) rows))
                        (nthcdr feet rows)))))))
  ;; Activity poses share the resting anatomy.  Hands and tools move through
  ;; anticipation, contact and recovery rather than replacing the whole silhouette.
  (cl-labels
      ((pixels (rows)
         (cl-loop for row in rows append
                  (make-list 2 (mapconcat (lambda (char) (make-string 2 char)) row ""))))
       (no-palms (base)
         (claudepet--stamp
          base 0 (if (eq base 'bob) 16 14)
          (pixels (if (eq base 'bob)
                      '("__................__" "__................__")
                    '("___..............___" "___..............___"))))))
    (let* ((focus (pixels '("oo......oo" "bb......bb" "bb......bb")))
           (blink (pixels '("oo......oo" "oo......oo" "bb......bb")))
           (focus-right (pixels '("ooooooooooo" "obb.....obb" "obb.....obb")))
           (squint-right (pixels '("ooooooooooo" "ooooooooooo" "obb.....obb")))
           (elbows (pixels '("oo............oo" "ooaa........aaoo")))
           (keyboard (pixels '("kuuuuuuuuuuk" "kgwgwgwgwgwk" "kkkkkkkkkkkk")))
           (hands (pixels '("oo......oo" "oo......oo" "aa......aa")))
           (left-press (pixels '("aa......oo" "oo......oo" "oo......aa")))
           (right-press (pixels '("oo......aa" "oo......oo" "aa......oo")))
           (brace (pixels '("oo" "oo")))
           (held-arm (pixels '("oooo" "..oo")))
           (raised-arm (pixels '("..oo" "..oo" ".ooo" "oo..")))
           (hammer (pixels '("kgsk" "kggk")))
           (angled-hammer (pixels '("kk." "gsk" ".gk")))
           (shaft (pixels '("t" "t" "r" "r")))
           (angled-shaft (pixels '("t." ".t" ".r")))
           (spark (pixels '(".y." "yyy" ".y."))))
      (dolist (spec (list (list 'working 'claude focus hands)
                         (list 'working-left 'claude focus left-press)
                         (list 'working-right 'bob focus right-press)
                         (list 'working-blink 'claude blink hands)))
        (cl-destructuring-bind (name base face palms) spec
          (claudepet-put-sprite
           name (claudepet--stamp
                 (claudepet--stamp
                  (claudepet--stamp
                   (claudepet--stamp (no-palms base) 10 10 face) 4 16 elbows)
                  8 18 keyboard)
                 10 18 palms))))
      (dolist (spec (list (list 'tooling 'claude focus-right held-arm 16
                               hammer 32 8 shaft 36 12 nil)
                         (list 'tooling-windup 'claude focus-right raised-arm 10
                               hammer 30 0 shaft 34 4 nil)
                         (list 'tooling-swing 'claude focus-right held-arm 16
                               angled-hammer 32 8 angled-shaft 34 14 nil)
                         (list 'tooling-spark 'bob squint-right held-arm 16
                               hammer 32 20 shaft 36 12 t)
                         (list 'tooling-recoil 'bob focus-right held-arm 16
                               hammer 32 12 shaft 36 16 nil)))
        (cl-destructuring-bind
            (name base face arm arm-y head head-x head-y handle handle-x handle-y impact) spec
          (let ((pose (claudepet--stamp
                       (claudepet--stamp
                        (claudepet--stamp
                         (claudepet--stamp
                          (claudepet--stamp (no-palms base) 10 10 face)
                          4 18 brace)
                         head-x head-y head)
                        handle-x handle-y handle)
                       30 arm-y arm)))
            (claudepet-put-sprite name (if impact (claudepet--stamp pose 34 24 spark) pose)))))))
  ;; The reference lifts the binoculars, scans both ways and blinks through the
  ;; lenses.  Pad the standing body without enlarging Claude to fit the props.
  (let* ((rest (cl-loop for y below 42 collect
                        (concat "......"
                                (if (<= 8 y 39) (nth (- y 8) (gethash 'claude claudepet-sprites))
                                  (make-string 40 ?.))
                                "......")))
         (raised-arm
          '("oooo......"
            "oooo......"
            "oooo......"
            "oooo......"
            "oooo......"
            "oooo......"
            "oooo......"
            "oooo......"
            "oooo......"
            "oooo......"
            "oooo......"
            "oooo......"
            "oooo______"
            "oooo______"
            "oooo______"
            "oooo______"))
         (raised-strap
          '("....kkkkkk"
            "....kkkkkk"
            "..rrkkkkkk"
            "..rrkkkkkk"
            "kkrrkkkkkk"
            "kkrrkkkkkk"
            "kkrrkkkkkk"
            "kkrrkkkkkk"))
         (raised-barrels
          '("kkggggggggrrgggggggg"
            "kkggggggggrrgggggggg"
            "kkgglluuuurrgglluuuu"
            "kkgglluuuurrgglluuuu"
            "kkgguuuuuurrgguuuuuu"
            "kkgguuuuuurrgguuuuuu"
            "kkgguuuuuurrgguuuuuu"
            "kkgguuuuuurrgguuuuuu"
            "kkggggggggrrgggggggg"
            "kkggggggggrrgggggggg"))
         (scan-body
          '("......__aaaaoooooooooooooooooooooo......"
            "......__aaaaoooooooooooooooooooooo......"
            "......aaoooooooooooooooooooooooooo......"
            "......aaoooooooooooooooooooooooooo......"
            "......aaoooooooooooooooooooooooooo......"
            "......aaoooooooooooooooooooooooooo......"
            "......aaoooooooooooooooooooooooooo......"
            "......aaoooooooooooooooooooooooooo......"
            "______aaooooooooooooaaoooooooooooooooo__"
            "______aaooooooooooooaaoooooooooooooooo__"
            "______aaooooooooooooooaaoooooooooo______"
            "______aaooooooooooooooaaoooooooooo______"
            "......aaoooooooooooooooooooooooooo......"
            "......aaoooooooooooooooooooooooooo......"
            "......aaoooooooooooooooooooooooooo......"
            "......aaoooooooooooooooooooooooooo......"
            "......oooooooooooooooooooooooooooo......"
            "......oooooooooooooooooooooooooooo......"))
         (scan-strap
          '("......kkkkkkkkkkkkkkkkkkkkkkkk"
            "......kkkkkkkkkkkkkkkkkkkkkkkk"
            "....rrrrkkkkkkkkkkkkkkkkkkkkkk"
            "....rrrrkkkkkkkkkkkkkkkkkkkkkk"
            "..kkrrrrkkkkkkkkkkkkkkkkkkkkkk"
            "..kkrrrrkkkkkkkkkkkkkkkkkkkkkk"
            "kk..rrrrkkkkkkkkkkkkkkkkkkkkkk"
            "kk..rrrrkkkkkkkkkkkkkkkkkkkkkk"
            "......kkkkkkkkkkkkkkkkkkkkkkkk"
            "......kkkkkkkkkkkkkkkkkkkkkkkk"))
         (lens
          '("gggggggggg"
            "gggggggggg"
            "ggwwkkkkgg"
            "ggwwkkkkgg"
            "gguukkkkgg"
            "gguukkkkgg"
            "gglluuuugg"
            "gglluuuugg"
            "gggggggggg"
            "gggggggggg"))
         (bridge
          '("rr"
            "rr"
            "rr"
            "rr"
            "rr"
            "rr"
            "rr"
            "rr"
            "rr"
            "rr"))
         (closed-lens
          '("oooooo"
            "oooooo"
            "kkkkkk"
            "kkkkkk"
            "oooooo"
            "oooooo"))
         (raised (claudepet--stamp rest 36 10 raised-arm))
         (right (claudepet--stamp rest 6 14 scan-body)))
    (setq raised (claudepet--stamp raised 22 4 raised-strap)
          raised (claudepet--stamp raised 30 2 raised-barrels)
          right (claudepet--stamp right 20 12 scan-strap)
          right (claudepet--stamp right 28 12 lens)
          right (claudepet--stamp right 40 12 lens)
          right (claudepet--stamp right 38 12 bridge))
    (let ((blink (claudepet--stamp (claudepet--stamp right 30 14 closed-lens)
                                  42 14 closed-lens)))
      (dolist (entry (list (cons 'binoculars-rest rest) (cons 'binoculars-raised raised)
                           (cons 'binoculars right) (cons 'binoculars-blink blink)
                           (cons 'binoculars-left
                                 (mapcar (lambda (row) (concat (reverse (string-to-list row)))) right))
                           (cons 'binoculars-left-blink
                                 (mapcar (lambda (row) (concat (reverse (string-to-list row)))) blink))))
        (claudepet-put-sprite (car entry) (cdr entry)))))
  ;; The reference plane has a long white fuselage, leather goggles and trailing feet.
  (claudepet-put-sprite
   'fly
   '("............................................................................"
     "............................................................................"
     "............................................................................"
     "............................................................................"
     "............................................................................"
     "............................................................................"
     "......................................rrrrrrrrrrrrrrrr......................"
     "......................................rrrrrrrrrrrrrrrr......................"
     "..................................ttttrrssssssssrrrrssssrr.................."
     "..................................ttttrrssssssssrrrrssssrr.................."
     "..................................ttttrrssssggggrrrrssggrr.................."
     "..................................ttttrrssssggggrrrrssggrr.................."
     "................................ttttrrrrrrrrrrrrrrrrrrrrrrrr................"
     "................................ttttrrrrrrrrrrrrrrrrrrrrrrrr................"
     "..............................ttttrrrroobbbboooooooooooorr.................."
     "..............................ttttrrrroobbbboooooooooooorr.................."
     "....wwww......................ttttttoooobbbboooooooooooo...................."
     "....wwww......................ttttttoooobbbboooooooooooo...................."
     "....wwggww..................oooooottoooooooooooooooooooo...................."
     "....wwggww..................oooooottoooooooooooooooooooo...................."
     "....wwwwwwoo..............oooooooooooooooooooooooooooooowwwwwwwwwwtt..pp...."
     "....wwwwwwoo..............oooooooooooooooooooooooooooooowwwwwwwwwwtt..pp...."
     "....wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwbbwwbbwwwwppoo...."
     "....wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwbbwwbbwwwwppoo...."
     "....ppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppwwppoo...."
     "....ppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppwwppoo...."
     "....oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooowwppoo...."
     "....oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooowwppoo...."
     "........................oooo..oooo......ggggwwwwwwwwwwww..........ttppoo...."
     "........................oooo..oooo......ggggwwwwwwwwwwww..........ttppoo...."
     "......................oooo..oooo..wwwwwwwwwwwwwwwwww................ppoo...."
     "......................oooo..oooo..wwwwwwwwwwwwwwwwww................ppoo...."
     "..................oooo..oooo..........................................oo...."
     "..................oooo..oooo..........................................oo...."
     "............................................................................"
     "............................................................................"
     "............................................................................"
     "............................................................................"
     "............................................................................"
     "............................................................................"))
  (let ((level (claudepet--stamp 'fly 32 8 '("rr" "rr")))
         (goggle-flap-up
          '("rrrr.."
            "rrrr.."
            "..tttt"
            "..tttt"))
         (goggle-flap-down
          '("..ttttrr"
            "..ttttrr"
            "rrrr...."
            "rrrr...."))
        ;; The near wing reaches down-left as the side rolls into view.
        ;; Keep the longitudinal axis level rather than pitching the nose.
        (bank-wing
         '("..________wwwwwwwwwwwwwwww"
           "..________wwwwwwwwwwwwwwww"
           "..____wwggggwwwwwwwwww____"
           "..____wwggggwwwwwwwwww____"
           "..wwwwwwwwwwwwwwww________"
           "..wwwwwwwwwwwwwwww________"
           "wwwwwwwwwwwwww____________"
           "wwwwwwwwwwwwww____________"))
        (full-bank-fuselage
         '("..............................oooooooooooooooooooooo.........."
           "..............................oooooooooooooooooooooo.........."
           "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwoooooooooooooooooooooowwbbwwbbww"
           "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwoooooooooooooooooooooowwbbwwbbww"
           "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww"
           "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww"
           "pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp"
           "pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp"
           "oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"
           "oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"))
        ;; The fuselage covers the old wing's top rows; redraw the feet below it.
        (full-bank-wing
         '("______oooo__oooo______wwwwwwwwwwwwwwwwww"
           "______oooo__oooo______wwwwwwwwwwwwwwwwww"
           "____oooo__oooo____wwggggwwwwwwwwwwww____"
           "____oooo__oooo____wwggggwwwwwwwwwwww____"
           "oooo__oooo____wwwwggggwwwwwwwwww________"
           "oooo__oooo____wwwwggggwwwwwwwwww________"
           "____________wwwwwwwwwwwwwwww____________"
           "____________wwwwwwwwwwwwwwww____________"))
         (edge-propeller
          '("____.."
            "____.."
            "____.."
            "____.."
            "__oo.."
            "__oo.."
            "ppooww"
            "ppooww"
            "__oo.."
            "__oo.."
            "____.."
            "____.."
            "____.."
            "____..")))
    (cl-labels
        ((flight-pose (edge flap-up bank bob)
           (let ((pose (if flap-up
                           (claudepet--stamp level 26 6 goggle-flap-up)
                         (claudepet--stamp level 26 10 goggle-flap-down))))
             (pcase bank
               (1 (setq pose (claudepet--stamp pose 32 28 bank-wing)))
               (2 (setq pose (claudepet--stamp pose 4 20 full-bank-fuselage)
                        pose (claudepet--stamp pose 18 30 full-bank-wing))))
             (when edge
               (setq pose (claudepet--stamp pose 68 20 edge-propeller)))
             (claudepet-shift pose bob))))
      (claudepet-put-sprite 'fly (flight-pose nil nil 0 0))
      (claudepet-put-sprite 'fly-prop-edge (flight-pose t t 0 0))
      (claudepet-put-sprite 'fly-bank (flight-pose nil t 1 -1))
      (claudepet-put-sprite 'fly-bank-edge (flight-pose t nil 1 -1))
      (claudepet-put-sprite 'fly-bank-full (flight-pose nil nil 2 0))
      (claudepet-put-sprite 'fly-bank-full-edge (flight-pose t t 2 0))))
  ;; Recreate the bike capture without its scrolling scenery.  Wheel rims and
  ;; handlebars stay planted while the torso rocks and the feet turn the pedals.
  (let ((bicycle
         '("............................"
           "............................"
           "....aaooooooooooooooo......."
           "....aaooooooooooooooo......."
           "....aaooooooooooooooo......."
           "....aaooooooooooaaaaoooooo.."
           "....aaoooooooooooooovvoovv.."
           "....aaoooooooooooo.ggggg...."
           "....aaoooooooooaaa.....g...."
           "...aaoooooooooooooooo..g...."
           "...aaoooooooooooooooo..g...."
           "...oaaooooooooooooooo..g...."
           "...bbbvvvvvvvvvvvvvvvvv....."
           "..kkkk.vv.........vvvvkkkk.."
           ".k....k..vv.....vvv..kvv..k."
           ".k....k....vv.vvv....k.v..k."
           ".k....kvvvvvvvvvvvvvvk....k."
           ".k....k..............k....k."
           "..kkkk................kkkk.."
           "............................"))
        (level-head
         '("oooooooooo......"
           "................"
           ".....bb........."
           ".....bb........b"
           "...............b"))
        (forward-head
         '("oooooooooooooooo"
           "................"
           "....bb.........b"
           "....bb.........b"))
        (back-head
         '("oooooooooooo"
           "............"
           "...........b"
           "bb.........b"
           "bb.........."))
        (level-feet
         '("oo..oo....oo"
           "oo..oo....oo"
           "oo..oo....oo"
           "bb..bb....oo"
           "..........oo"
           "..........bb"))
        (down-feet
         '("oo..oo....oo"
           "oo..oo....oo"
           "oo..oo....oo"
           "oo..bb....oo"
           "bb........bb"))
        (back-feet
         '("oo..oo.....oo"
           "oo..oo.....oo"
           "oo..oo.....oo"
           "oo..oo.....bb"
           "oo..oo......."
           "bb..bb......."))
        (up-feet
         '("oo..oo.....oo"
           "oo..oo.....oo"
           "oo..oo.....bb"
           "oo..oo......."
           "oo..bb......."
           "bb..........."))
         (level-spokes
          '("g...................g."
            "gg..................gg"))
        (down-spokes
         '("g...................g..."
           ".g...................g.."
           "..g...................g."
           "...g...................g"))
         (back-spokes
          '("gg..................gg"
            ".g...................g"))
        (up-spokes
         '("...g...................g"
           "..g...................g."
           ".g...................g.."
           "g...................g...")))
    (dolist (entry
             (list
              (cons 'bike
                    (claudepet--stamp
                     (claudepet--stamp (claudepet--stamp bicycle 5 1 level-head)
                                      9 12 level-feet)
                     3 14 level-spokes))
              (cons 'bike-pedal-down
                    (claudepet--stamp
                     (claudepet--stamp (claudepet--stamp bicycle 5 1 forward-head)
                                      9 12 down-feet)
                     2 14 down-spokes))
              (cons 'bike-pedal-back
                    (claudepet--stamp
                     (claudepet--stamp (claudepet--stamp bicycle 9 1 back-head)
                                      7 12 back-feet)
                     3 16 back-spokes))
              (cons 'bike-pedal-up
                    (claudepet--stamp
                     (claudepet--stamp (claudepet--stamp bicycle 5 1 forward-head)
                                      7 12 up-feet)
                     2 14 up-spokes))))
      (claudepet-put-sprite
       (car entry)
       (cl-loop for row in (cdr entry) append
                (make-list 2 (mapconcat (lambda (char) (make-string 2 char)) row ""))))))
  (cl-labels ((poses (&rest names)
                (mapcar (lambda (name) (gethash name claudepet-sprites)) names)))
    (claudepet-define-animation
     'idle :frames #'claudepet--idle-frames :delay 1.0 :frame-delays '(1.0) :priority 0)
    (claudepet-define-animation
     'look-left :frames (poses 'look-left-turn 'look-left 'look-left-turn 'claude)
     :delay 0.12 :frame-delays '(0.12 0.75 0.12 1.01)
     :duration 2 :priority 20 :next #'claudepet--resume)
    (claudepet-define-animation
     'wave :frames (poses 'claude 'bob 'wave-raised 'wave-upright 'wave-outward
                          'wave-upright 'wave-outward 'wave-raised 'bob 'claude)
     :delay 0.12 :frame-delays '(0.06 0.08 0.08 0.10 0.12 0.10 0.12 0.10 0.08 0.16)
      :duration 1 :priority 5 :next 'idle)
    (let ((wave (gethash 'wave claudepet-animations)))
      (claudepet-define-animation
       'wake :frames (append (plist-get wave :frames)
                             (poses 'blink 'claude 'blink 'claude 'blink 'claude))
       :delay 0.12 :frame-delays (append (plist-get wave :frame-delays)
                                       '(0.08 0.12 0.08 0.12 0.08 0.12))
       :duration 1.6 :priority 5 :next 'idle))
    (claudepet-define-animation
     'alert :frames (poses 'bob 'alert-raised 'alert 'alert-raised 'claude)
     :delay 0.4 :frame-delays '(0.10 0.12 0.52 0.16 1.10)
     :priority 30)
    (claudepet-define-animation
     'thinking :frames (poses 'thinking-one 'thinking-two 'thinking-three 'thinking-two)
     :delay 0.6 :frame-delays '(0.55 0.65 0.70 0.50) :priority 20)
    (claudepet-define-animation
     'working :frames (poses 'working 'working-left 'working 'working-right
                             'working-left 'working-right 'working-blink 'working)
     :delay 0.3 :frame-delays '(0.18 0.12 0.10 0.12 0.12 0.12 0.08 0.32) :priority 20)
    (claudepet-define-animation
     'tooling :frames (poses 'tooling 'tooling-windup 'tooling-swing 'tooling-spark
                            'tooling-recoil 'tooling)
     :delay 0.3 :frame-delays '(0.28 0.26 0.09 0.08 0.16 0.28) :priority 20)
    (let ((frames (cl-loop for (normal edge repetitions)
                           in '((fly fly-prop-edge 4) (fly-bank fly-bank-edge 2)
								(fly-bank-full fly-bank-full-edge 4) (fly-bank fly-bank-edge 2))
                           append (cl-loop repeat repetitions append (poses normal edge)))))
      ;; Hold each bank while the propeller and cap keep fluttering.
      (claudepet-define-animation
       'fly :frames frames :delay 0.13 :priority 20
       :frame-delays (cl-loop repeat (/ (length frames) 2) append '(0.12 0.14))))
    (claudepet-define-animation
     'binoculars :frames (poses 'binoculars-raised 'binoculars 'binoculars-blink 'binoculars
                               'binoculars-left 'binoculars-left-blink 'binoculars-left
                               'binoculars 'binoculars-raised 'binoculars-rest)
     :delay (/ 2.0 12) :priority 20
     :frame-delays (mapcar (lambda (frames) (/ frames 12.0)) '(10 14 2 26 6 2 40 5 5 10)))
    ;; Four source poses, each held for two frames in the 12 fps capture.
    (claudepet-define-animation
     'bike :frames (poses 'bike 'bike-pedal-down 'bike-pedal-back 'bike-pedal-up)
     :delay (/ 2.0 12) :frame-delays (make-list 4 (/ 2.0 12)) :priority 20)
    (claudepet-define-animation
     'done :frames (poses 'bob 'done-hop 'bob 'claude)
     :delay 0.3 :frame-delays '(0.12 0.44 0.12 1.82)
     :priority 25)
    (claudepet-define-animation
     'notify :frames (poses 'notify 'alert 'notify 'bob 'notify)
     :delay 0.3 :frame-delays '(0.12 0.18 0.20 0.12 1.88)
     :priority 35)
    (claudepet-define-animation
     'error :frames (poses 'error 'error-warning 'error 'blink 'error)
     :delay 0.4 :frame-delays '(0.12 0.36 0.20 0.14 2.18)
     :priority 35)
    (claudepet-define-animation
     'pet :frames (poses 'pet 'pet-high 'pet-high 'pet)
     :delay 0.3 :frame-delays '(0.14 0.26 0.66 0.44)
     :duration 1.5 :priority 40 :next #'claudepet--resume)
    (claudepet-define-animation
     'sleep :frames (poses 'sleep 'sleep-zzz 'sleep)
     :delay 1.0 :frame-delays '(1.20 0.90 0.90) :priority 10))
  (let ((now (float-time)))
    (setq claudepet--idle-blink-at (+ now 3 (random 6))
          claudepet--idle-bob-at (+ now 15 (random 16))
          claudepet--idle-look-at (+ now 30 (random 31)))))

(provide 'claudepet-animations)
;;; claudepet-animations.el ends here
