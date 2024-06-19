;;;; Copyright 2023-2024 Carnegie Mellon University

;;; TODO markers should be placed by cell coordinates, not raw pixels
;;; TODO generate Extinguishment area automatically from a simple radius, probably
;;;      configured on a per-game basis
;;; TODO add animation showing extent of extinguishment area
;;; TODO marker placement should be spring loaded instead of pervasive
;;; TODO update modeling API to reflect all the UI stuff
;;; TODO include more metadata in the log, such as the contents of the game object and map
;;; TODO in queue-motion it should be possible  to extract current position from player
;;;      state instead of having to pass it as another paremeter
;;; TODO queue-motion should probably be changed to take the target in the map's
;;;      coordinate system
;;; TODO make fire scale fire probabilities by update speed
;;; TODO factor out various geometry things, like testing for a cell being in bounds
;;; TODO implement an end to game
;;; TODO auto-compress log files
;;; TODO make debug output on Lisp side less voluminous
;;; TODO note and report when connection to server is lost
;;; TODO why does it sometimes not extinguish fires?
;;; TODO figure out why this doesn't always work right on Safari
;;; TODO figure out what's going wrong with logging near the map boundary
;;; TODO make use of JS foreach and more Lisp parenscript stuff consistent

#-(and cl-ppcre hunchentoot cl-json parenscript)
(ql:quickload '(:cl-interpol :alexandria :iterate :cl-ppcre
                :spinneret :hunchentoot :smackjack :cl-json :parenscript
                :uuid :cl-geometry :uiop :local-time :vom))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(geometry::make-point geometry::construct-bounding-box
            geometry::x-min geometry::y-min
            geometry::x-max geometry::y-max) :geometry))

(defpackage :wildfire
  (:nicknames :wf)
  (:use :common-lisp :alexandria :iterate :ppcre
        :spinneret :hunchentoot :smackjack :json :uuid)
  (:local-nicknames (:v :vom) (:g :geometry) (:lt :local-time))
  (:import-from :ps ps:@)
  (:export #:start-server #:stop-server #:run-standalone #:defgame
           #:grass #:ash #:water #:tree #:road #:rock #:house))

(in-package :wildfire)

(interpol:enable-interpol-syntax :modify-*readtable* t)



;;; Configuration

(define-constant +version+ "1.0" :test #'equal)
(define-constant +mission-log-format-version+ "1.0" :test #'equal)

(defparameter *debug* nil)
(defparameter *data-directory* *default-pathname-defaults*)
(defparameter *access-log* "wildfire.log")
(defparameter *default-game* 'test-game)

(define-constant +source-file-type+ "lisp" :test #'string=)
(define-constant +default-port+ 8978)
(define-constant +view-side+ 39)                      ; number of cells on one side of the view
(assert (oddp +view-side+))                           ; must be odd
(define-constant +cell-size+ 20)                      ; in pixels, cells are always square
(define-constant +view-size+ (* +view-side+ +cell-size+)) ; in pixels, view is always square
(define-constant +plane-axis+ '(47 47) :test #'equal) ; in pixels, point within plane about which to spin
(define-constant +default-map-size+ 100)              ; in cells, default for both width and height
(define-constant +polling-interval+ 1000)             ; milliseconds
(define-constant +flame-flicker-interval+ 150)        ; milliseconds
(define-constant +marker-color+ "#c00" :test #'string-equal)
(define-constant +marker-label-radius+ 4)

(define-constant +cell-type-names+
    '((grass t) (ash nil) (water nil) (tree t) (road nil) (rock nil) (house t))
  :test #'equal)
(define-constant +default-cell-type+ (position 'grass +cell-type-names+ :key #'first))
(define-constant +image-template+ "images/~(~A~).png" :test #'string=)
(define-constant +region-types+
    '((grass) (lake water) (river water t) (forest tree) (road road t) (outcrop rock)
      (houses house))
  :test #'equal)

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
  (fire-exhaustion-probability 0.002)
  (fire-propagation-probability 0.02)
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
  (last-extinguish-click nil)
  log-file-path)

(define-object-printer mission (s) "~A, ~A (~D))"
                       (mission-id s)
                       (game-name (mission-game s))
                       (length (mission-players s)))

(defparameter *missions* (make-hash-table :test 'equalp))

(defun write-to-mission-log (mission kind plist)
  (check-type kind keyword)
  (with-open-file (stream (mission-log-file-path mission)
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~:[~%~;~](~S ~S~%~{ ~S ~:W~^~%~})~%"
            (eq kind :metadata)
            kind
            (lt:format-timestring nil (lt:now))
            plist)))

(defun create-mission-log (mission)
  (write-to-mission-log mission :metadata
                        `(:log-file-version ,+mission-log-format-version+ :wildfire-version ,+version+
                          :original-log-file-path ,(namestring (mission-log-file-path mission))
                          :mission ,(mission-id mission)
                          :game ,(symbol-name (game-name (mission-game mission))))))

(defun make-mission (game &optional id)
  (unless id
    (setf id (lt:format-timestring nil (lt:now)
                                   :format `("mission-" (:year 4) (:month 2) (:day 2)
                                                        (:hour 2) (:min 2) (:sec 2)
                                                        (:msec 3) "-"
                                                        ,(princ-to-string (make-v1-uuid))))))
  (iter (with map := (make-array (list (game-width game) (game-height game))))
        (with result :=
              (%make-mission :id id
                             :game game
                             :ignitions (game-ignitions game)
                             :log-file-path (merge-pathnames (format nil "~A-log.lisp" id)
                                                             (uiop:subpathname *data-directory*
                                                                               "mission-logs/"))))
        (for y :from 0 :below (game-height game))
        (iter (for x :from 0 :below (game-width game))
              (setf (aref map x y) (make-cell x y (aref (game-map game) x y))))
        (finally (setf (mission-map result) map)
                 (setf (gethash id *missions*) result)
                 (create-mission-log result)
                 (return result))))

(defun get-mission (id)
  (gethash id *missions*))

(defun mission-cell (mission x y)
  (let ((m (mission-map mission))
        (g (mission-game mission)))
    (and (< -1 x (game-width g))
         (< -1 y (game-height g))
         (aref m x y))))



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



(defparameter *css* (make-array 1 :element-type 'character
                                  :initial-element #\Newline
                                  :adjustable t
                                  :fill-pointer 1))

(defun add-css (&rest strings)
  (format *css* "~{~A~%~}" strings))

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
    (:html :style "font-family: 'Merriweather Sans', sans-serif; overflow-x: auto"
           (:head (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                  (:link :href "https://fonts.googleapis.com/css?family=Merriweather+Sans"
                         :rel "stylesheet")
                  (:link :href "https://fonts.googleapis.com/icon?family=Material+Icons"
                         :rel "stylesheet")
                  (:style *css*)
                  (:raw (generate-prologue *ajax*))
                  (:script (:raw (format nil "~%~A~4%// ** Wildfire **~2%~{~A~%~}"
                                         (ps:ps* ps:*ps-lisp-library*) *js*)))
                  (:title title))
           (:body :style "margin-left: 4rem; margin-top: 4rex"
                  (funcall thunk)))))

(defun failure (fmt &rest args)
  (with-page ("Error")
    (with-html
      (:div :style "text-align: center; font-size: larger; color: red; margin-top: 6rex"
            (apply #'format nil fmt args)))))

(js `(defun clog (&rest args)
       (apply (@ console log) args))
    `(defun dlog (&rest args)
       (when debug
         (apply clog args))))

(js `(ps:var load-count ,(+ (length +cell-types+) 4 1)) ; number of images + 1 document

    `(defun load-image (path)
       (let ((img (ps:new (-image))))
         (setf (@ img onload) load-image-test-and-render)
         (setf (@ img src) path)
         img))

    `(ps:var images (map load-image ',(map 'list #'ct-image-path +cell-types+)))
    `(ps:var dragons (load-image "images/dragons.jpg"))
    `(ps:var plane (load-image "images/plane.png"))
    `(ps:var flames (map load-image '("images/flame.png" "images/flame2.png")))
    `(ps:var flame-index 0)
    `(ps:var velocity '(0 0))
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
        `(defun load-image-test-and-render ()
           (when (eql (decf load-count) 0)
             (render ',(iter (for x :below (game-width g))
                             (collect (iter (for y :below (game-height g))
                                            (collect (ct-index (cell-type
                                                                (aref (mission-map m)
                                                                      x y)))))))
                     ,(game-width g) ,(game-height g)))))
    (with-page ("Mission")
      (with-html
        (:div :style "display: flex; justify-content: center"
              (:div.uicol (:fieldset.component :style "width: 10rem"
                           (:legend "Clicking on map")
                           (:div
                            (:input#move :type "radio" :name "click-action" :checked t)
                            (:label :for "move" "moves airplane"))
                           (:div
                            (:input#mark :type "radio" :name "click-action")
                            (:label :for "mark" "places marker")))
                          (:div.component
                           (:input#extinguish :type "checkbox" :checked t)
                           (:label :for "extinguish"
                                   :style "width: 80%; margin-left: 0.5rem; display: inline-block; vertical-align: middle"
                                   "Extinguish fires at move destination"))
                          (:fieldset.component
                           (:legend "Markers")
                           ;; TODO debugging hack
                           (:div (:ul.markers (:li "Lake")
                                              (:li "Distant forest")
                                              (:li "Plains")))))
              (:canvas#view :height +view-size+ :width +view-size+
                            :onclick (ps:ps (clicked-map (list (@ event offset-x)
                                                               (@ event offset-y))))
                            "Not supported in this browser")
              (:div.uicol (:div.component (:input#speed :type "range" :min 1 :max 10)
                                          (:label :for "speed" "Speed"))))
        (:canvas#map :style #?'display: ${(if *debug* "block" "none")}'
                     :height (cells-to-pixels (game-height g))
         :width (cells-to-pixels (game-width g)))
        ;; the following foolishness is to ensure the Material Icons font is loaded
        (:div (:span.material-icons :style "visibility:hidden" "location_on"))))))

(add-css ".uicol { width:12rem; margin:0 0.7rem }"
         ".component { margin-bottom: 0.8rex }"
         ".component legend { padding: 0 0.3rem }"
         ".markers { list-style-type: none; margin-block-start: 0; margin-block-end: 0;
                     margin-inline-start: -1.5rem; margin-inline-end: 0.5rem }"
         ".markers li { border: solid 1px gray; margin: 0.1rem; padding: 0.1rex 0.5rem }")

(ps:defpsmacro with-point ((x y) value &body body)
  `(let (,x ,y)
     (setf (list ,x ,y) ,value)
     ,@body))

(js `(defun map-context ()
       (ps:chain document (get-element-by-id "map") (get-context "2d")))

    `(defun view-context ()
       (ps:chain document (get-element-by-id "view") (get-context "2d")))

    (destructuring-bind (xp yp) +plane-axis+
      (let* ((view-center (/ +view-size+ 2.0)))
        `(defun display-map ()
           (let ((ctx (view-context)))
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
         (draw-all-markers (map-context))
         (setf last-flame-time ms))
       (with-point (x y) position
         (with-point (tx ty) target
           (with-point (sx sy) velocity
             (unless (= sx sy 0)
               (let ((d (/ (- ms last-update) 1000))) ; velocity is pixels per second
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
             (setf velocity '(0 0) last-update nil))
           (animation-update))))

    ;; TODO temporary debugging hack
    `(ps:var markers (list (ps:create name "Lake" x 1290 y 1311)
                           (ps:create name "Distant forest" x 593 y 590)
                           (ps:create name "Plains" x 1100 y 1030)))

    `(defun render (map-data w h)
       (loop :with ctx := (map-context)
             :for y :from 0 :below h
             :do (loop :for x :from 0 :below w
                       :do ((@ ctx draw-image)
                            (aref images (aref map-data x y))
                            (* x ,+cell-size+) (* y ,+cell-size+)
                            ,+cell-size+ ,+cell-size+))
             :finally (progn
                        (draw-all-markers ctx)
                        (dlog "map rendered")
                        (animation-update))))

    `(defun draw-marker (ctx marker)
       ((@ ctx save))
       ((@ ctx translate) (@ marker x) (@ marker y))
       (setf (@ ctx fill-style) ,+marker-color+)
       (setf (@ ctx font) "40px Material Icons")
       ((@ ctx fill-text) "location_on" 0 0)
       (setf (@ ctx font) "10pt Merriweather Sans")
       (let* ((name (@ marker name))
              (width (+ (@ ((@ ctx measure-text) name) width) 8)))
         (setf (@ ctx fill-style) "white")
         ((@ ctx begin-path))
         ((@ ctx round-rect) 29 -53 width 18 ,+marker-label-radius+)
         ((@ ctx fill))
         (setf (@ ctx fill-style) "black")
         ((@ ctx fill-text) name 33 -38))
       ((@ ctx restore)))

    `(defun draw-all-markers (ctx)
       (dolist (m markers)
         (draw-marker ctx m)))

    `(defun modify-map (image x y)
       ;; x and y are indices into the map
       (let ((ctx (map-context)))
         ((@ ctx draw-image) image
          (* x ,+cell-size+) (* y ,+cell-size+)
          ,+cell-size+ ,+cell-size+)))

    `(setf (@ document onmousemove)
           (lambda () (setf (ps:chain document body style cursor) "default"))))

(defun queue-motion (player-id target current extinguish-p)
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
        (setf (mission-last-extinguish-click mission)
              (and extinguish-p (cell-burningp cell) cell)))
      (unless (apply #'=~ 0 d)
        (let ((angle (- (/ pi 2) (apply #'atan d))))
          (setf (player-motion p)
                `((:target . ,new-pos)
                  (:angle . ,angle)
                  (:velocity . ,(mapcar (lambda (x) (* (player-speed p) x))
                                     `(,(cos angle) ,(sin angle)))))))))))

(defun new-maker (where)
  `((:makrer . ,where)))

(define-remote-call clicked-map (where player-id current action extinguish-p)
  ;; where is in pixels, in the visible region's coordinate system
  ;; current is in pixels, in the underlying map's coordinate system
  (eswitch (action :test #'string-equal)
    ("move" (queue-motion player-id where current extinguish-p))
    ("mark" nil)))

(js `(defun click-action ()
       (dolist (action '("move" "mark"))
             (when (@ ((@ document get-element-by-id) action) checked)
               (return action))))

    `(defun extinguish-p ()
       (@ ((@ document get-element-by-id) "extinguish") checked))

    `(defun clicked-map (location)
       (call clicked-map () (location player position (click-action) (extinguish-p)))
       (update-server)))



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
      (when-let ((last-click (mission-last-extinguish-click mission)))
        (when (and (every #'zerop (cdr (assoc :velocity state)))
                   (equal (mapcar #'pixels-to-cells (cdr (assoc :position state)))
                          (coords last-click)))
          ;; refactor for commonality with what follows
          (iter (for (xo yo) :in +extinguish-area+)
                (for x := (+ (cell-x last-click) xo))
                (for y := (+ (cell-y last-click) yo))
                (for candidate := (mission-cell mission x y))
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
                           (for candidate := (mission-cell mission x y))
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

(define-constant +center+ (make-list 2 :initial-element (floor +view-side+ 2))
  :test #'equal)

(defun make-local-regions (by-type cell-data)
  (let ((used (make-hash-table :test 'equal)))
    (labels ((gather-region (coord type)
               (iter (with pending := (list coord))
                     (with result := (list coord))
                     (initially (setf (gethash coord used) t))
                     (while pending)
                     (for (x y) := (pop pending))
                     (iter (for (x0 y0) :in '((-1 0) (0 -1) (1 0) (0 1)))
                           (for new-x := (+ x x0))
                           (for new-y := (+ y y0))
                           (unless (and (< -1 new-x +view-side+) (< -1 new-y +view-side+))
                             (next-iteration))
                           (for new-coord := (list new-x new-y))
                           (when (and (not (gethash new-coord used))
                                      (eq (first (aref cell-data new-x new-y)) type))
                             (push new-coord pending)
                             (push new-coord result)
                             (setf (gethash new-coord used) t)))
                     (finally (return result)))))
      (iter (with result := nil)
            (for (type cells) :on by-type :by #'cddr)
            (iter (for coord :in cells)
                  (unless (gethash coord used)
                    (push (cons type (gather-region coord type)) result)))
            (finally (return result))))))

(defun make-model-state (player client-state)
  (let ((m (player-mission player))
        (result (iter (for pass-through :in '(:time :angle))
                      (nconcing `(,pass-through ,(cdr (assoc  pass-through client-state)))
                        :into passed-through)
                      (finally (return `(,@passed-through
                                         :center ,+center+
                                         :speed ,(sqrt (apply #'+ (mapcar (rcurry #'expt 2)
                                                                          (cdr (assoc :velocity client-state)))))
                                         :view ,(make-array `(,+view-side+ ,+view-side+))))))))
    (iter (with v := (getf result :view))
          (with type-cells := nil)
          (with off := (mapcar #'-
                               (mapcar #'pixels-to-cells
                                           (cdr (assoc :position client-state)))
                               +center+))
          (for x :from 0 :below +view-side+)
          (iter (for y :from 0 :below +view-side+)
                (for c := (mission-cell m (+ x (first off)) (+ y (second off))))
                (unless c
                  (next-iteration))
                (for nm := (ct-name (cell-type c)))
                (setf (aref v x y) (list nm (cell-burningp c)))
                (push (list x y) (getf type-cells nm)))
          (finally (return `(:regions ,(make-local-regions type-cells v) ,@result))))))

(define-remote-call server-update (player-id state)
  (when-let* ((p (get-player player-id))
              (m (player-mission p))
              (g (mission-game m)))
    (write-to-mission-log m :update-request-from-client (alist-plist state))
    (when-let ((mod (game-model g))
               (model-state (make-model-state p state)))
      (write-to-mission-log m :call-model model-state)
      (when-let ((model-response (funcall mod model-state)))
        (write-to-mission-log m :model-response model-response)
        (queue-motion player-id
                      (mapcar #'cells-to-pixels (getf model-response :target))
                      (cdr (assoc :position state))
                      t))) ; TODO figure out how to deal with this stuff from the model
    (let ((response (multiple-value-bind (births deaths) (propagate-fires m state)
                      `((:motion . ,(shiftf (player-motion p) nil))
                        (:ignite . ,births)
                        (:extinguish . ,deaths)))))
      (write-to-mission-log m :update-from-server (alist-plist response))
      response)))

(js `(defun motion (json)
       (when json
         (setf target (@ json target))
         (setf angle (@ json angle))
         (setf velocity (@ json velocity))
         (setf (ps:chain document body style cursor) "none")
         (update-position)))

    `(ps:var ash (aref images ,(position 'ash +cell-type-names+ :key #'car)))

    `(defun ignite (locs)
       (when locs
         (dolist (loc locs)
           (with-point (x y) loc
             (modify-map (flame) x y))
           ((@ fires add) ((@ loc join))))
         (draw-all-markers (map-context))))

    `(defun extinguish (locs)
       (when locs
         (dolist (loc locs)
           ((@ fires delete) ((@ loc join)))
           (with-point (x y) loc
             (modify-map ash x y)))
         (draw-all-markers (map-context))))

    `(ps:var pending-server-update nil)

    `(defun set-server-update ()
       (setf pending-server-update (set-timeout update-server ,+polling-interval+)))

    `(defun update-server ()
       (when pending-server-update
         (clear-timeout pending-server-update)
         (setf pending-server-update nil))
       (let ((state (ps:create :time (@ document timeline current-time)
                               :position position
                               :target target
                               :velocity velocity
                               :angle angle)))
         (call server-update (json) (player state)
               (progn
                 (extinguish (@ json extinguish))
                 (ignite (@ json ignite))
                 (motion (@ json motion)))))
       (set-server-update))

    `(set-server-update))



(js '(setf (@ window onload) load-image-test-and-render))



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

(defun load-from-subdirectory (subdir)
  (dolist (f (mapcar (lambda (p) (make-pathname :defaults p :type nil))
                     (directory (merge-pathnames (make-pathname :name :wild
                                                                :type +source-file-type+)
                                                 (uiop:subpathname *data-directory*
                                                                   subdir)))))
    (v:info "Loading ~A" (namestring f))
    (load f)))

(defun start-server (&key (port *port*) debug)
  (enable-debug debug)
  (setf *port* port)
  (when *server*
    (v:warn "Server ~S already running, restarting it" *server*)
    (stop-server))
  (load-from-subdirectory "games/")
  (assert (get-game *default-game*))
  (load-from-subdirectory "models/")
  (setf *server* (start (make-instance 'easy-acceptor
                                       :document-root *data-directory*
                                       :access-log-destination *access-log*
                                       :port port)))
  (v:info "Started ~A" *server*)
  *server*)

(defun run-standalone ()
  (start-server)
  (loop (sleep 100000000)))



#|

(progn
  (swank:set-default-directory "/home/dfm/w/wildfire/")
  (load "wildfire")
  (swank:set-package "WILDFIRE")
  (funcall (find-symbol "START-SERVER" (find-package "WILDFIRE")) :debug nil))

|#
