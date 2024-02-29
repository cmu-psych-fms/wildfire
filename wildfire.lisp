;;;; Copyright 2023-2024 Carnegie Mellon University

#-(and cl-ppcre hunchentoot cl-json parenscript)
(ql:quickload '(:cl-interpol :alexandria :iterate :cl-ppcre
                :spinneret :hunchentoot :smackjack :cl-json :css-lite :parenscript
                :uuid :cl-geometry :vom))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(geometry::make-point geometry::construct-bounding-box
            geometry::x-min geometry::y-min
            geometry::x-max geometry::y-max) :geometry))

(defpackage :wildfire
  (:use :common-lisp :alexandria :iterate :ppcre
        :spinneret :hunchentoot :smackjack :json :uuid)
  (:local-nicknames (:css :css-lite) (:v :vom) (:g :geometry))
  (:import-from :ps ps:@)
  (:export #:start-server #:stop-server #:run-standalone #:defgame))

(in-package :wildfire)

(interpol:enable-interpol-syntax :modify-*readtable* t)



;;; Configuration

(defparameter *debug* nil)
(defparameter *data-directory* *default-pathname-defaults*)
(defparameter *access-log* "wildfire.log")
(defparameter *default-game* 'test-game)

(define-constant +default-port+ 8978)
(define-constant +cell-size+ 20)                      ; in pixels, cells are always square
(define-constant +view-size+ (* 39 +cell-size+))      ; in pixels, view is always square
(define-constant +plane-axis+ '(47 47) :test #'equal) ; in pixels, point about which to spin plane
(define-constant +default-map-size+ 100)              ; in cells, default for both width and height
(define-constant +polling-interval+ 333)              ; milliseconds
(define-constant +flame-flicker-interval+ 150)        ; milliseconds

(define-constant +cell-type-names+
    '((grass t) (ash nil) (water nil) (tree t) (road nil) (rock nil) (house t))
  :test #'equal)
(define-constant +default-cell-type+ (position 'grass +cell-type-names+ :key #'first))
(define-constant +image-template+ "images/~(~A~).png" :test #'string=)
(define-constant +region-types+
    '((grass) (lake water) (river water t) (forest tree) (road road t) (outcrop rock)
      (houses house))
  :test #'equal)

;;; TODO generate stuff like the following automatically from a simple radius, probably
;;;      configured on a per-game basis
(define-constant +extinguish-area+
  (iter (for x :from -3 :to 3)
        (nconcing (iter (for y :from -3 :to 3)
                        (unless (= (abs x) (abs y) 3)
                          (collect (list x y))))))
  :test #'equal)


(define-constant +tolerance+ 1.0d-12)

(defun =~ (n &rest more)
  (every (lambda (x) (< (abs (- x n)) +tolerance+)) more))



(defmacro define-object-printer (type (&optional (var type)) fmt &rest args)
  `(defmethod print-object ((,var ,type) #0=#:stream)
     (print-unreadable-object (,var #0# :type t :identity t)
       (format #0# ,fmt ,@args))))

(defstruct (cell-type (:conc-name ct-) (:print-object))
  name
  (flamablep nil)
  image-path
  index)

(define-object-printer cell-type (ct) "~S [~D]"
  (ct-name ct)
  (ct-index ct))

(defparameter +cell-types+
  (iter (for (name flamablep) :in +cell-type-names+)
        (for i :from 0)
        (collect (make-cell-type :name name
                                 :flamablep flamablep
                                 :image-path (format nil +image-template+ name)
                                 :index i)
          :into result)
        (finally (return (coerce result 'vector)))))

(defun get-ct (name)
  (find name +cell-types+ :key #'ct-name))



(defstruct (cell (:constructor %make-cell (x y type)) (:print-object))
  x
  y
  type
  (burningp nil))

(defun make-cell (x y index)
  (%make-cell x y (aref +cell-types+ index)))

(define-object-printer cell () "~D ~D ~S~:[~;!~]"
                       (cell-x cell)
                       (cell-y cell)
                       (ct-name (cell-type cell))
                       (cell-burningp cell))

(defun cells-to-pixels (n &optional centerp)
  (let ((raw (* n +cell-size+)))
    (if centerp
        (+ raw (round +cell-size+ 2))
        raw)))

(defun pixels-to-cells (x)
  (floor x +cell-size+))

(defstruct (game (:print-object))
  name
  map
  width
  height
  start-x
  start-y
  regions
  (fire-exhaustion-probability 0.005)
  (fire-propagation-probability 0.01)
  ignitions
  (model nil))

(define-object-printer game () "~A (~D×~D)"
                       (game-name game)
                       (game-width game)
                       (game-height game))

(defstruct (region (:constructor make-region (name cells index))
                   (:print-object))
  name
  cells
  index)

(define-object-printer region () "~S [~D]"
                       (region-name region)
                       (region-index region))



(defmacro defgame (name (&rest keys &key &allow-other-keys) &rest regions)
  `(%defgame ',name ',regions ,@(iter (for (k v) :on keys :by #'cddr)
                                      (nconcing `(,k ',v)))))

(defparameter *games* (make-hash-table :test 'equalp))

(defun get-game (name)
  (gethash (string name) *games*))

(defun polygon-cells (points bounds)
  (declare (ignore bounds))
  (iter (with poly := (g:make-polygon-from-point-list points))
        (with pbounds := (g:construct-bounding-box poly))
        (for y :from (g:y-min pbounds) :below (g:y-max pbounds))
        (nconcing (iter (for x :from (g:x-min pbounds) :below (g:x-max pbounds))
                        (when (g:point-in-polygon-crossing-p (g:make-point x y) poly)
                          (collect (list x y)))))))

(defun path-cells (points bounds)
  (labels ((neighbor (p direction)
             (ecase direction
               (:left (g:make-point (- (g:x p) 1) (g:y p)))
               (:right (g:make-point (+ (g:x p) 1) (g:y p)))
               (:up (g:make-point (g:x p) (- (g:y p) 1)))
               (:down (g:make-point (g:x p) (+ (g:y p) 1))))))
    (iter (with result := (make-hash-table :test 'equal))
          (for (start end) :on points)
          (while end)
          (if-first-time (setf (gethash start result) t))
          (iter (with best := nil)
                (with shortest := most-positive-single-float)
                (while (not (g:point-equal-p start end)))
                (iter (for candidate :in (mapcar (curry #'neighbor start)
                                                 '(:left :right :up :down)))
                      (unless (g:point-in-polygon-crossing-p candidate bounds)
                        (next-iteration))
                      (for d := (g:distance (g:x candidate) (g:y candidate)
                                            (g:x end) (g:y end)))
                      (when (<= d shortest)
                        (setf best candidate)
                        (setf shortest d))
                      (until (zerop d))
                      (finally (setf (gethash (setf start best) result) t)))
                (finally (setf (gethash end result) t)))
          (finally (return (mapcar (lambda (p) (list (g:x p) (g:y p)))
                                   (hash-table-keys result)))))))

(defun %defgame (name regions &rest keys &key (width +default-map-size+)
                                           (height +default-map-size+)
                                           (start-x (round width 2))
                                           (start-y (round height 2))
                                           (ignitions nil)
                 &allow-other-keys)
  (let ((m (make-array (list width height)
                       :element-type '(unsigned-byte 8)
                       :initial-element +default-cell-type+))
        (ign (stable-sort (iter (with last-time)
                                (for ig :in ignitions)
                                (nconcing (iter (for k :in '(:t :x :y))
                                                (for v := (getf ig k))
                                                (when (eq k :t)
                                                  (if v
                                                      (setf last-time v)
                                                      (setf v last-time))
                                                  (when v
                                                    (setf v (* v 1000)))) ; sec to ms
                                                (unless v
                                                  (v:error "Missing ~S in ignition ~S"
                                                           k ig)
                                                  (return nil))
                                                (collect v :into result)
                                                (finally (return (list result))))))
                          #'<
                          :key #'first)))
    (iter (for r :in regions)
          (for i :from 0)
          (collect (destructuring-bind (kind (&optional name) &rest coords) r
                     (destructuring-bind (&optional ct-name pathp)
                         (cdr (or (assoc kind +region-types+)
                                  (error "unknown region type ~S" kind)))
                       (let* ((ct (get-ct ct-name))
                              (bounds (g:make-polygon-from-point-list
                                       (list (g:make-point 0 0)
                                             (g:make-point (1+ width) 0)
                                             (g:make-point (1+ width) (1+ height))
                                             (g:make-point 0 (1+ height)))))
                              (points (apply #'g:coords-to-points coords))
                              (cells (progn
                                       (assert (every (rcurry #'g:point-in-polygon-crossing-p bounds)
                                                      points))
                                       (funcall (if pathp #'path-cells #'polygon-cells)
                                                points bounds))))
                         (iter (for (x y) :in cells)
                               (setf (aref m x y) (ct-index ct)))
                         (make-region name cells i))))
            :into reg)
          (finally (setf (gethash (string name) *games*)
                         (apply #'make-game (list* :name name
                                                   :width width
                                                   :height height
                                                   :map (copy-array m)
                                                   :start-x start-x
                                                   :start-y start-y
                                                   :regions reg
                                                   :ignitions ign
                                                   keys))))))
  name)



(defstruct (mission (:constructor %make-mission)
                    (:print-object))
  id
  (players nil)
  game
  map
  ignitions
  (fires (make-hash-table))
  (last-click nil))

(define-object-printer mission (s) "~A, ~A (~D))"
                       (mission-id s)
                       (game-name (mission-game s))
                       (length (mission-players s)))

(defparameter *missions* (make-hash-table :test 'equalp))

(defun make-mission (game &optional id)
  (unless id
    (setf id (format nil "mission-~A" (make-v1-uuid))))
  (iter (with map := (make-array (list (game-width game) (game-height game))))
        (with result := (%make-mission :id id
                                       :game game
                                       :ignitions (game-ignitions game)))
        (for y :from 0 :below (game-height game))
        (iter (for x :from 0 :below (game-width game))
              (setf (aref map x y) (make-cell x y (aref (game-map game) x y))))
        (finally (setf (mission-map result) map)
                 (setf (gethash id *missions*) result)
                 (return result))))

(defun get-mission (id)
  (gethash id *missions*))



(defstruct (player (:constructor %make-player)
                   (:print-object))
  id
  name
  mission
  (speed 30)
  (motion nil))

(define-object-printer player (p) "~A~:[ (~A)~;~*~] ~A"
                       (player-name p)
                       (equalp (player-name p) (player-id p))
                       (player-id p)
                       (player-mission p))

(defparameter *players* (make-hash-table :test 'equalp))

(defun make-player (mission &optional name)
  (let* ((id (format nil "player-~A" (make-v1-uuid)))
         (result (%make-player :id id :name (or name id) :mission mission)))
    (push result (mission-players mission))
    (setf (gethash id *players*) result)))

(defun get-player (id)
  (gethash id *players*))

(defun get-mission-player (name mission)
  (find name (mission-players mission) :key #'player-name :test #'equalp))



(defun join-mission (mission &optional player-name game)
  "Returns a player named PLAYER-NAME playing a mission with id MISSION.
If PLAYER-NAME is not supplied or is null a new uuid is allocated and used for the name.
If such a mission already exists it is joined, and otherwise one is created, playing the
game named GAME. Signals a SIMPLE-ERROR if the mission already exists and is not playing
the game name, if the game does not exist, or if a player of the same name has already
joined the mission."
  (unless game
    (setf game *default-game*))
  (if-let ((g (get-game game)))
    (let ((m (or (get-mission mission) (make-mission g mission))))
      (unless (eq (mission-game m) g)
        (error "Mission ~A is not playing game ~A." mission game))
      (when (get-mission-player player-name m)
        (error "A player named ~A is already in mission ~A" player-name mission))
      (make-player m player-name))
    (error "No game named ~A is available." game)))



;;; TODO figure out how tidily to hook models into missions
;;; TODO so far this is just a kludge to prove it can be done

;; (defparameter *next-model-move* nil)

;; (defparameter *locs* '#0=((200 400) (400 200) (600 600) . #0#))

;; (declaim (ftype (function (t t t) t) queue-motion))

;; (defun wildfire-model (player-id state)
;;   (when (equalp (cdr (assoc :speed state)) '(0 0))
;;     (cond ((null *next-model-move*)
;;            (setf *next-model-move* (+ 5000 (cdr (assoc :time state))))
;;            nil)
;;           ((>= (cdr (assoc :time state)) *next-model-move*)
;;            (setf *next-model-move* nil)
;;            (let ((pos (cdr (assoc :position state))))
;;              (queue-motion player-id (pop *locs*) pos))))))



(defparameter *js* nil)

(defun js (&rest forms)
  (let ((front (eq (first forms) :front)))
    (when front
      (pop forms))
    (setf forms (mapcar #'ps:ps* forms))
    (if front
        (setf *js* (append forms *js*))
        (appendf *js* forms))))

(defparameter *ajax* (make-instance 'ajax-processor :server-uri "/ajax"))

(defmacro define-remote-call (name (&rest args) &body body)
  ;; Note that because cl-json is so idiosyncratic at its alist detection, we have to
  ;; make it explicit, so any non-nil return value from this must be an alist.
  `(defun-ajax ,name (,@args) (*ajax* :method :post :callback-data :json)
     (v:debug "~A from client ~@{~S~^, ~}" ',name ,@args)
     (let ((result (encode-json-alist-to-string (progn ,@body))))
       (v:debug "~A to client ~A" ',name result)
       result)))

(ps:defpsmacro call (name (&optional jvar) (&rest args) &body callback-body)
  `(let ((vals (list ,@args)))
     (dlog "to server" ',name vals)
     ,(and jvar `((@ vals push) (lambda (,jvar)
                                  (dlog "from server" ',name ((@ +json+ stringify) ,jvar))
                                  ,@callback-body)))
     (apply (@ smackjack ,name) vals)))

(defun not-found ()
  (acceptor-status-message *acceptor* +http-not-found+))

(defmacro with-page ((title) &body body)
  `(%with-page ,title (lambda () ,@body)))

(defun %with-page (title thunk)
  (when (boundp '*reply*)
    (setf (header-out :X-Clacks-Overhead) "GNU Terry Pratchett"))
  (with-html-string
    (:doctype)
    (:html :style "font-family: 'Merriweather Sans', sans-serif; overflow-x: auto;"
           (:head (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                  (:link  :rel "stylesheet"
                          :href "https://fonts.googleapis.com/css?family=Merriweather+Sans")
                  (:raw (generate-prologue *ajax*))
                  (:script (:raw (format nil "~%~A~4%// ** Wildfire **~2%~{~A~%~}"
                                         (ps:ps* ps:*ps-lisp-library*) *js*)))
                  (:title title))
           (:body :style "margin-left: 4em; margin-top: 4ex;"
                  (funcall thunk)))))

(defun failure (fmt &rest args)
  (with-page ("Error")
    (with-html
      (:div :style "text-align:center;font-size:larger;color:red;margin-top:6ex"
            (apply #'format nil fmt args)))))

(js `(defun clog (&rest args)
       (apply (@ console log) args))
    `(defun dlog (&rest args)
       (when debug
         (apply clog args))))

(js `(ps:var load-count ,(+ (length +cell-types+) 4 1)) ; number of images + 1 document

    `(defun load-image (path)
       (let ((img (ps:new (-image))))
         (setf (@ img onload) load-test)
         (setf (@ img src) path)
         img))

    `(ps:var images (map load-image ',(map 'list #'ct-image-path +cell-types+)))
    `(ps:var dragons (load-image "images/dragons.jpg"))
    `(ps:var plane (load-image "images/plane.png"))
    `(ps:var flames (map load-image '("images/flame.png" "images/flame2.png")))
    `(ps:var flame-index 0)
    `(ps:var speed '(0 0))
    `(ps:var angle ,(- (/ pi 2)))

    `(defun flame ()
       ;; returns one of the two flame images selected pseudo-randomly
       (aref flames (mod (setf flame-index (logand (1+ (* 257 flame-index)) #x1ffff)) 2))))

(define-easy-handler (mission :uri "/") (game mission player)
  (let* ((p (handler-case (join-mission mission player game)
              (simple-error (e)
                (return-from mission (failure #?"${e}")))))
         (m (player-mission p))
         (g (mission-game m))
         (*js* *js*))           ; all parenscript added here is only local to this mission
    (js :front `(ps:var debug ,(if *debug* t 'false)))
    (js `(ps:var player ,(player-id p))
        `(ps:var position '(,(cells-to-pixels (game-start-x g) t)
                            ,(cells-to-pixels (game-start-x g) t)))
        `(ps:var target position)
        `(defun load-test ()
           (when (eql (decf load-count) 0)
             (render ',(iter (for x :below (game-width g))
                             (collect (iter (for y :below (game-height g))
                                            (collect (ct-index (cell-type
                                                                (aref (mission-map m)
                                                                      x y)))))))
                     ,(game-width g) ,(game-height g)))))
    (with-page ("Mission")
      (with-html
        (:div :style "text-align: center;"
              (:canvas :id "view"
                       :height +view-size+ :width +view-size+
                       :onclick (ps:ps (clicked-map (list (@ event offset-x)
                                                          (@ event offset-y))))
                       "Not supported in this browser"))
        (:canvas :id "map" :style #?'display: ${(if *debug* "block" "none")}'
                 :height (cells-to-pixels (game-height g))
                 :width (cells-to-pixels (game-width g)))))))

(ps:defpsmacro with-point ((x y) value &body body)
  `(let (,x ,y)
     (setf (list ,x ,y) ,value)
     ,@body))

(js (destructuring-bind (xp yp) +plane-axis+
      (let* ((view-center (/ +view-size+ 2.0)))
        `(defun display-map ()
           (let ((ctx (ps:chain document (get-element-by-id "view") (get-context "2d"))))
             ((@ ctx draw-image) dragons 0 0 ,+view-size+ ,+view-size+)
             (with-point (x y) position
               ((@ ctx draw-image) (ps:chain document (get-element-by-id "map"))
                (- x ,view-center) (- y ,view-center)
                ,+view-size+ ,+view-size+
                0 0
                ,+view-size+ ,+view-size+)
               ((@ ctx save))
               ((@ ctx translate) ,view-center ,view-center)
               ((@ ctx rotate) angle)
               ((@ ctx draw-image) plane ,(- xp) ,(- yp))
               ((@ ctx restore)))))))

    `(defun animation-update () ((@ window request-animation-frame) update-position))

    `(ps:var last-update nil)
    `(ps:var fires (ps:new (-Set)))
    `(ps:var last-flame-time 0)

    `(defun update-position (&optional ms)
       (when (eq ms undefined)
         (animation-update)
         (return-from update-position))
       (when (null last-update)
         (setf last-update ms))
       (when (and fires (>= (- ms last-flame-time) ,+flame-flicker-interval+))
         ((@ fires for-each) (lambda (v)
                               (with-point (x y) ((@ ((@ v split) ",") map) -Number)
                                 (modify-map (flame) x y))))
         (setf last-flame-time ms))
       (with-point (x y) position
         (with-point (tx ty) target
           (with-point (sx sy) speed
             (unless (= sx sy 0)
               (let ((d (/ (- ms last-update) 1000))) ; speed is pixels per second
                 (labels ((change (spd cur lim)
                            (cond ((and (> spd 0) (> lim cur))
                                   (min (+ cur (* spd d)) lim))
                                  ((and (< spd 0) (< lim cur))
                                   (max (+ cur (* spd d)) lim))
                                  (t cur))))
                   (setf x (change sx x tx))
                   (setf y (change sy y ty))))))
           (setf position (list x y))
         (display-map)
           (setf last-update ms)
           (when (and (= x tx) (= y ty))
             (setf speed '(0 0) last-update nil))
           (animation-update))))

    `(defun render (map-data w h)
       (loop :with ctx := (ps:chain document (get-element-by-id "map") (get-context "2d"))
             :for y :from 0 :below h
             :do (loop :for x :from 0 :below w
                       :do ((@ ctx draw-image)
                            (aref images (aref map-data x y))
                            (* x ,+cell-size+) (* y ,+cell-size+)
                            ,+cell-size+ ,+cell-size+))
             :finally (progn
                        (dlog "map rendered")
                        (animation-update))))

    `(defun modify-map (image x y)
       ;; x and y are indices into the map
       (let ((ctx (ps:chain document (get-element-by-id "map") (get-context "2d"))))
         ((@ ctx draw-image) image
          (* x ,+cell-size+) (* y ,+cell-size+)
          ,+cell-size+ ,+cell-size+)))

    `(setf (@ document onmousemove)
           (lambda () (setf (ps:chain document body style cursor) "default"))))

;;; TODO should be able to extract current from player state
;;; TODO queue-motion should probably be changed to take the target in the map's coordinate system

(defun queue-motion (player-id target current)
  ;; target is in pixels, in the visible region's coordinate system
  ;; current is in pixels, in the underlying map's coordinate system
  (when-let* ((p (get-player player-id))
              (mission (player-mission p))
              (map (mission-map mission))
              (d (mapcar #'- target `(,#0=(/ +view-size+ 2.0) ,#0#)))
              (new-pos (mapcar #'+ current d))) ; pixels, map's coordinate system
    (when (every #'<
                 `(,least-negative-single-float ,least-negative-single-float)
                 new-pos
                 (mapcar #'* (array-dimensions map) `(,+cell-size+ ,+cell-size+)))
      (let ((cell (apply #'aref map (mapcar #'pixels-to-cells new-pos))))
        (setf (mission-last-click mission)
              (and (cell-burningp cell) cell)))
      (unless (apply #'=~ 0 d)
        (let ((angle (- (/ pi 2) (apply #'atan d))))
          (setf (player-motion p)
                `((:target . ,new-pos)
                  (:angle . ,angle)
                  (:speed . ,(mapcar (lambda (x) (* (player-speed p) x))
                                     `(,(cos angle) ,(sin angle)))))))))))

(define-remote-call clicked-map (where player-id current)
  ;; where is in pixels, in the visible region's coordinate system
  ;; current is in pixels, in the underlying map's coordinate system
  (queue-motion player-id where current))

(js `(defun clicked-map (location)
       (call clicked-map () (location player position))))



;;; TODO make fire scale fire probabilities by update speed (probably well above
;;;      here somewhere)

;;; TODO factor out various geometry things, like test for a cell being in bounds

(defun propagate-fires (mission state)
  (let* ((g (mission-game mission))
         (m (mission-map mission))
         (exhaustion-probability (game-fire-exhaustion-probability g))
         (propagation-probability (game-fire-propagation-probability g))
         (ash-type (get-ct 'ash))
         (births nil)
         (deaths nil))
    (labels ((coords (cell)
               (list (cell-x cell) (cell-y cell)))
             (ignite (cell)
               (setf (cell-burningp cell) t)
               (setf (gethash cell (mission-fires mission)) t)
               (push (coords cell) births)))
      (when-let ((last-click (mission-last-click mission)))
        (when (and (every #'zerop (cdr (assoc :speed state)))
                   (equal (mapcar #'pixels-to-cells (cdr (assoc :position state)))
                          (coords last-click)))
          ;; refactor for commonality with what follows
          (iter (for (xo yo) :in +extinguish-area+)
                (for x := (+ (cell-x last-click) xo))
                (for y := (+ (cell-y last-click) yo))
                (for candidate := (and (< -1 x (game-width g))
                                       (< -1 y (game-height g))
                                       (aref m x y)))
                (when (and candidate (cell-burningp candidate))
                  (setf (cell-burningp candidate) nil)
                  (setf (cell-type candidate) ash-type)
                  (remhash candidate (mission-fires mission))
                  (push (coords candidate) deaths)))))
      (iter (for c :in (hash-table-keys (mission-fires mission)))
            (cond ((<= (random 1.0) exhaustion-probability)
                   (setf (cell-burningp c) nil)
                   (setf (cell-type c) ash-type)
                   (remhash c (mission-fires mission))
                   (push (coords c) deaths))
                  (t (iter (for (xo yo) :in '((-1 0) (0 -1) (1 0) (0 1)))
                           (for x := (+ (cell-x c) xo))
                           (for y := (+ (cell-y c) yo))
                           (for candidate := (and (< -1 x (game-width g))
                                                  (< -1 y (game-height g))
                                                  (aref m x y)))
                           (when (and candidate
                                      (ct-flamablep (cell-type candidate))
                                      (not (cell-burningp candidate))
                                      (<= (random 1.0) propagation-probability))
                             (ignite candidate))))))
      (iter (for start := (caar (mission-ignitions mission)))
            (while (and start (>= (cdr (assoc :time state)) start)))
            (for (nil x y) := (pop (mission-ignitions mission)))
            (ignite (aref m x y))))
    (values births deaths)))

(define-remote-call server-update (player-id state)
  ;; TODO following is a temporary hack until I figure out how to do it more tidily
  (when-let* ((p (get-player player-id))
              (m (player-mission p))
              (g (mission-game m)))
    (when-let ((mod (game-model g)))
      (funcall mod state))
    (multiple-value-bind (births deaths) (propagate-fires m state)
      `((:motion . ,(shiftf (player-motion p) nil))
        (:ignite . ,births)
        (:extinguish . ,deaths)))))

(js `(defun motion (json)
       (when json
         (setf target (@ json target))
         (setf angle (@ json angle))
         (setf speed (@ json speed))
         (setf (ps:chain document body style cursor) "none")
         (update-position)))

    `(ps:var ash (aref images ,(position 'ash +cell-type-names+ :key #'car)))

    `(defun ignite (locs)
       (when locs
         (dolist (loc locs)
           (with-point (x y) loc
             (modify-map (flame) x y))
           ((@ fires add) ((@ loc join))))))

    `(defun extinguish (locs)
       (when locs
         (dolist (loc locs)
           ((@ fires delete) ((@ loc join)))
           (with-point (x y) loc
             (modify-map ash x y)))))

    `(defun update-server ()
       (let ((state (ps:create :time (@ document timeline current-time)
                               :position position
                               :target target
                               :speed speed
                               :angle angle)))
         (call server-update (json) (player state)
               (progn
                 (extinguish (@ json extinguish))
                 (ignite (@ json ignite))
                 (motion (@ json motion)))))
       (set-timeout update-server ,+polling-interval+))

    `(set-timeout update-server))



(js '(setf (@ window onload) load-test))



(defparameter *port* +default-port+)

(defvar *server* nil)

(push (create-ajax-dispatcher *ajax*) *dispatch-table*)

(push (create-folder-dispatcher-and-handler "/images/" "images/") *dispatch-table*)

(defun stop-server (&optional (soft t))
  (cond (*server*
         (v:info "Stopping ~A" *server*)
         (stop *server* :soft soft)
         (v:info "~A stopped" *server*)
         (setf *server* nil))
        (t (v:warn "No server was running"))))

(defun enable-debug (&optional (debug t))
  (cond ((null debug) (setf *debug* nil))
        ((not (realp debug)) (setf *debug* t))
        (t (setf debug (clamp (round debug) 0 4))
           (setf *debug* (if (zerop debug) t debug))))
  (v:config :wildfire (cond ((null *debug*) :info)
                            ((integerp *debug*) (make-keyword #?"DEBUG${*debug*}"))
                            (t :debug))))

(defun start-server (&key (port *port*) debug)
  (enable-debug debug)
  (setf *port* port)
  (when *server*
    (v:warn "Server ~S already running, restarting it" *server*)
    (stop-server))
  (setf *server* (start (make-instance 'easy-acceptor
                                       :document-root *data-directory*
                                       :access-log-destination *access-log*
                                       :port port)))
  (v:info "Started ~A" *server*)
  *server*)

(defun run-standalone ()
  (start-server)
  (loop (sleep 100000000)))



(defgame test-game (:ignitions ((:x 55 :y 50 :t 4)
                                (:x 90 :y 90 :t 7)
                                (:x 91 :y 98)
                                (:x 25 :y 20)
                                (:x 64 :y 47 :t 14)))
         (forest (sherwood-forest) 0 0  30 0  45 45  10 40  0 25)
         (lake (loch-ness) 55 55  80 65  70 75  60 68  45 60)
         (river (nile) 70 0  60 20  64 60)
         (road (lincoln-highway) 40 0  4 99)
         (outcrop (bear-rocks) 45 44  49 46  47 51  46 49)
         (houses (levittown) 61 45  69 45  69 49  61 49))

(defgame model-game (:model test-model
                     :ignitions ((:x 55 :y 50 :t 4)
                                (:x 90 :y 90 :t 7)
                                (:x 91 :y 98)
                                (:x 25 :y 20)
                                (:x 64 :y 47 :t 14)))
         (forest (sherwood-forest) 0 0  30 0  45 45  10 40  0 25)
         (lake (loch-ness) 55 55  80 65  70 75  60 68  45 60)
         (river (nile) 70 0  60 20  64 60)
         (road (lincoln-highway) 40 0  4 99)
         (outcrop (bear-rocks) 45 44  49 46  47 51  46 49)
         (houses (levittown) 61 45  69 45  69 49  61 49))

(assert (get-game *default-game*))

(defun test-model (state)
  (format t "~&test-model: ~:W~%" state))



#|

(progn
  (swank:set-default-directory "/home/dfm/w/wildfire/")
  (load "wildfire")
  (swank:set-package "WILDFIRE")
  (funcall (find-symbol "START-SERVER" (find-package "WILDFIRE")) :debug nil))

|#
