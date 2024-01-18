;;; Copyright 2023-2024 Carnegie Mellon University

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
  (:import-from :ps ps:*ps-lisp-library* ps:ps ps:ps* ps:defpsmacro
                ps:@ ps:chain ps:var ps:new)
  (:export #:start-server #:stop-server #:run-standalone #:defgame))

(in-package :wildfire)

(interpol:enable-interpol-syntax :modify-*readtable* t)



;; Configuration

(defparameter *debug* nil)
(defparameter *data-directory* *default-pathname-defaults*)
(defparameter *access-log* "wildfire.log")
(defparameter *default-game* 'test-game)

(define-constant +default-port+ 8978)
(define-constant +view-size+ 800)                     ; in pixels, view is always square
(define-constant +plane-axis+ '(47 47) :test #'equal) ; in pixels, point about which to spin plane
(define-constant +cell-size+ 20)                      ; in pixels, cells are always square
(define-constant +default-map-size+ 100) ; in cells, default for both width and height

(define-constant +cell-type-names+
    '((grass 1) (ash 0) (water 0) (tree 1) (road 0) (rock 0) (house 1))
  :test #'equal)
(define-constant +default-cell-type+ (position 'grass +cell-type-names+ :key #'first))
(define-constant +image-template+ "images/~(~A~).png" :test #'string=)
(define-constant +region-types+
    '((grass) (lake water) (river water t) (forest tree) (road road t) (outcrop rock)
      (houses house))
  :test #'equal)

(define-constant +tolerance+ 1.0d-12)

(defun ~= (n &rest more)
  (every (lambda (x) (< (abs (- x n)) +tolerance+)) more))



(defmacro define-object-printer (type (&optional (var type)) fmt &rest args)
  `(defmethod print-object ((,var ,type) #0=#:stream)
     (print-unreadable-object (,var #0# :type t :identity t)
       (format #0# ,fmt ,@args))))

(defstruct (cell-type (:conc-name ct-) (:print-object))
  name
  flamability                           ; TODO need more structure to fire propagation
  image-path
  index)

(define-object-printer cell-type (ct) "~S [~D]"
  (ct-name ct)
  (ct-index ct))

(defparameter +cell-types+
  (iter (for (name flamability) :in +cell-type-names+)
        (for i :from 0)
        (collect (make-cell-type :name name
                                 :flamability flamability
                                 :image-path (format nil +image-template+ name)
                                 :index i)
          :into result)
        (finally (return (coerce result 'vector)))))

(defun get-ct (name)
  (find name +cell-types+ :key #'ct-name))



(defstruct (cell (:constructor %make-cell (x y type)) (:print-object))
  x
  y
  type)

(defun make-cell (x y index)
  (%make-cell x y (aref +cell-types+ index)))

(define-object-printer cell () "~D ~D ~S"
                       (cell-x cell)
                       (cell-y cell)
                       (ct-name (cell-type cell)))

(defstruct (game (:print-object))
  name
  map
  width
  height
  start-x
  start-y
  regions)

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
  `(%defgame ',name ',regions ,@keys))

(defparameter *games* (make-hash-table :test 'equalp))

(defun get-game (name)
  (gethash name *games*))

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
                 &allow-other-keys)
  (let ((m (make-array (list width height)
                       :element-type '(unsigned-byte 8)
                       :initial-element +default-cell-type+)))
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
          (finally (setf (gethash name *games*)
                         (apply #'make-game (list* :name name
                                                   :width width
                                                   :height height
                                                   :map m
                                                   :start-x start-x
                                                   :start-y start-y
                                                   :regions reg
                                                   keys))))))
  name)



(defstruct (mission (:constructor %make-mission)
                    (:print-object))
  id
  (players nil)
  game
  map)

(define-object-printer mission (s) "~A, ~A (~D))"
                       (mission-id s)
                       (game-name (mission-game s))
                       (length (mission-players s)))

(defparameter *missions* (make-hash-table :test 'equalp))

(defun make-mission (game &optional id)
  (unless id
    (setf id (format nil "mission-~A" (make-v1-uuid))))
  (iter (with map := (make-array (list (game-width game) (game-height game))))
        (with result := (%make-mission :id id :game game))
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
  (speed 100)) ; 30

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



(defparameter *js* nil)

(defun push-js (form)
  (first (push (ps* form) *js*)))

(defun append-js (form)
  (let ((s (ps* form)))
    (appendf *js* (list s))
    s))

(defparameter *ajax* (make-instance 'ajax-processor :server-uri "/ajax"))

(defmacro defajax (name (&rest bindings) ((var) &body js-code) &body body)
  (multiple-value-bind (args values)
      (iter (for (v . b) :in bindings)
            (unless (and (symbolp v) (not (keywordp v)))
              (error "Variable name ~S is not a non-keyword symbol" v))
            (collect v :into vars)
            (collect `(progn ,@b) :into bodies)
            (finally (return (values vars bodies))))
    `(progn
       (append-js '(defun ,name (,@args)
                    ((@ smackjack ,name) ,@values (lambda (,var) ,@js-code))))
       (defun-ajax ,name (,@args) (*ajax* :method :post :callback-data :json)
         (encode-json-to-string (progn ,@body))))))

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
                  (:script (:raw (format nil "~%~A~%~{~A~%~}"
                                         (ps* *ps-lisp-library*) *js*)))
                  (:title title))
           (:body :style "margin-left: 4em; margin-top: 4ex;"
                  (funcall thunk)))))

(defun failure (fmt &rest args)
  (with-page ("Error")
    (with-html
      (:div :style "text-align:center;font-size:larger;color:red;margin-top:6ex"
            (apply #'format nil fmt args)))))

(append-js `(defun clog (&rest args)
              (apply (@ console log) args)))

(append-js `(defun dlog (&rest args)
              (when debug
                (apply clog args))))

(push-js `(var load-count ,(+ (length +cell-types+) 2 1))) ; number of images + 1 document

(append-js '(defun load-image (path)
             (incf load-count)
             (let ((img (new (-image))))
               (setf (@ img onload) load-test)
               (setf (@ img src) path)
               img)))

(push-js `(progn
            (var images (map load-image ',(map 'list #'ct-image-path +cell-types+)))
            (var dragons (load-image "images/dragons.jpg"))
            (var plane (load-image "images/plane.png"))
            (var speed '(0 0))
            (var angle ,(- (/ pi 2)))))

(define-easy-handler (mission :uri "/") (game mission player)
  (let* ((p (handler-case (join-mission mission player game)
              (simple-error (e)
                (return-from mission (failure #?"${e}")))))
         (m (player-mission p))
         (g (mission-game m))
         (*js* *js*))           ; all parenscript added here is only local to this mission
    (push-js `(progn
                (var debug ,(if *debug* t 'false))
                (var player ,(player-id p))
                (var position '(,(* (game-start-x g) +cell-size+)
                                ,(* (game-start-x g) +cell-size+)))
                (var target position)))
    (append-js `(defun load-test ()
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
                       :onclick (ps (clicked-map event))
                       "Not supported in this browser"))
        (:canvas :id "map" :style #?'display: ${(if *debug* "block" "none")}'
                 :height (* (game-height g) +cell-size+)
                 :width (* (game-width g) +cell-size+))))))

(defpsmacro with-point ((x y) value &body body)
  `(let (,x ,y)
     (setf (list ,x ,y) ,value)
     ,@body))

(append-js (destructuring-bind (xp yp) +plane-axis+
             (let* ((view-center (/ +view-size+ 2.0)))
               `(defun display-map ()
                  (let ((ctx (chain document (get-element-by-id "view") (get-context "2d"))))
                    ((@ ctx draw-image) dragons 0 0 ,+view-size+ ,+view-size+)
                    (with-point (x y) position
                      ((@ ctx draw-image) (chain document (get-element-by-id "map"))
                       (- x ,view-center) (- y ,view-center)
                       ,+view-size+ ,+view-size+
                       0 0
                       ,+view-size+ ,+view-size+)
                      ((@ ctx save))
                      ((@ ctx translate) ,view-center ,view-center)
                      ((@ ctx rotate) angle)
                      ((@ ctx draw-image) plane ,(- xp) ,(- yp))
                      ((@ ctx restore))))))))

(append-js `(defun animation-update () ((@ window request-animation-frame) update)))

(append-js '(var last-update nil))

(append-js `(defun update (&optional ms)
              (when (eq ms undefined)
                (animation-update)
                (return-from update))
              (when (null last-update)
                (setf last-update ms))
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
                  (if (and (= x tx) (= y ty))
                      (setf speed '(0 0) last-update nil)
                      (animation-update))))))

(append-js `(defun render (map-data w h)
              (loop :with ctx := (chain document (get-element-by-id "map") (get-context "2d"))
                    :for y :from 0 :below h
                    :do (loop :for x :from 0 :below w
                              :do ((@ ctx draw-image)
                                   (aref images (aref map-data x y))
                                   (* x ,+cell-size+) (* y ,+cell-size+)
                                   ,+cell-size+ ,+cell-size+))
                    :finally (progn
                               (dlog "map rendered")
                               (animation-update)))))

(append-js `(setf (@ document onmousemove)
                  (lambda () (setf (chain document body style cursor) "default"))))

(defajax clicked-map ((where (list (@ event offset-x) (@ event offset-y)))
                      (player-id player)
                      (current position))
    ((json)
      (dlog "clicked" ((@ +json+ stringify) json))
      (when json
        (setf target (or (@ json target) target))
        (setf angle (or (@ json angle) angle))
        (setf speed (@ json speed))
        (setf (chain document body style cursor) "none")
        (update)))
  (v:debug "clicked ~S ~S ~S" where current player-id)
  (let* ((p (get-player player-id))
         (map (mission-map (player-mission p)))
         (d (mapcar #'- where `(,#0=(/ +view-size+ 2.0) ,#0#)))
         (new-pos (mapcar #'+ current d)))
    (when (every #'<
                 `(,least-negative-single-float ,least-negative-single-float)
                 new-pos
                 (mapcar #'* (array-dimensions map) `(,+cell-size+ ,+cell-size+)))
      (let* ((movep (not (apply #'~= 0 d)))
             (angle (and movep (- (/ pi 2) (apply #'atan d)))))
        `((:target . ,(and movep new-pos))
          (:angle . ,angle)
          (:speed . ,(if movep
                         (mapcar (lambda (x) (* (player-speed p) x))
                                 `(,(cos angle) ,(sin angle)))
                         '(0 0))))))))

(append-js '(setf (@ window onload) load-test))



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



(defgame test-game ()
         (forest (sherwood-forest) 0 0  30 0  45 45  10 40  0 25)
         (lake (loch-ness) 55 55  80 65  70 75  60 68  45 60)
         (river (nile) 70 0  60 20  64 60)
         (road (lincoln-highway) 40 0  4 99)
         (outcrop (bear-rocks) 45 44  49 46  47 51  46 49)
         (houses (levittown) 61 45  69 45  69 49  61 49))

(assert (get-game *default-game*))
