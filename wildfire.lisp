 ;;;; Copyright 2024 Carnegie Mellon University

;;; TODO double check that markers are being tidily positioned
;;; TODO tweak marker positioning
;;; TODO improve the macro wrapping the SmackJack stuff
;;; TODO why isn't the model extinguishing?
;;; TODO markers should be placed by cell coordinates, not raw pixels
;;; TODO generate Extinguishment area automatically from a simple radius, probably
;;;      configured on a per-game basis
;;; TODO add animation showing extent of extinguishment area
;;; TODO marker placement should be spring loaded instead of pervasive
;;; TODO update modeling API to reflect all the UI stuff
;;; TODO include more metadata in the log, such as the contents of the game object and map
;;; TODO in ?? (was older form of queue-motion) it should be possible to extract current position from player
;;;      state instead of having to pass it as another parameter
;;; TODO ?? (was older form of queue-motion) should probably be changed to take the target in the map's
;;;      coordinate system
;;; TODO make fire scale fire probabilities by update speed
;;; TODO factor out various geometry things, like testing for a cell being in bounds
;;; TODO debug end of game
;;; TODO auto-compress log files
;;; TODO make debug output on Lisp side less voluminous
;;; TODO note and report when connection to server is lost
;;; TODO why does it sometimes not extinguish fires?
;;; TODO figure out why this doesn't always work right on Safari
;;; TODO figure out what's going wrong with logging near the map boundary
;;; TODO why does movement clicking not work once the boundary shows?
;;; TODO make use of JS foreach and more Lisp parenscript stuff consistent
;;; TODO figure out how to deal with the angle when moving to a marker, etc.
;;; TODO get the destination right when moving to a marker
;;; TODO seems to be something wrong with :angle reporting
;;; TODO display elapsed time, and maybe remaining time, in UI
;;; TODO finish headless missions
;;; TODO try to implement HTTP version of remote modeling
;;; TODO finish obliterating old attempts at remote models
;;; TODO consider using sasht or jzon instead of cl-json
;;; TODO consider if some alists should be plists
;;; TODO maybe use multiple levels to make debugging output more useful?

#-(and cl-ppcre bordeaux-threads hunchentoot cl-json parenscript)
(ql:quickload '(:cl-interpol :alexandria :cl-strings :iterate :cl-ppcre :bordeaux-threads
                :usocket-server :spinneret :hunchentoot :smackjack :cl-json :parenscript
                :uuid :cl-geometry :uiop :local-time :vom))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(geometry::make-point geometry::construct-bounding-box
            geometry::x-min geometry::y-min
            geometry::x-max geometry::y-max) :geometry))

(defpackage :wildfire
  (:nicknames :wf)
  (:use :common-lisp :alexandria :iterate :ppcre :bt :usocket
        :spinneret :hunchentoot :smackjack :json :uuid)
  (:local-nicknames (:v :vom) (:g :geometry) (:lt :local-time) (:s :cl-strings))
  (:import-from :ps ps:@)
  (:export #:start-server #:stop-server #:run-standalone #:defgame #:request-mission
           #:grass #:ash #:water #:tree #:road #:rock #:house))

(in-package :wildfire)

(interpol:enable-interpol-syntax :modify-*readtable* t)



;;; Configuration

(defparameter *debug* nil)
(defparameter *data-directory* *default-pathname-defaults*)
(define-constant +version+ "1.0" :test #'equal)
(define-constant +mission-log-format-version+ "1.0" :test #'equal)

(defparameter *access-log* "wildfire.log")
(defparameter *default-game* 'test-game)

(define-constant +source-file-type+ "lisp" :test #'string=)
(define-constant +default-port+ 8978)
(define-constant +default-model-port+ 8979)
(define-constant +view-side+ 39)                          ; number of cells on one side of the view
(assert (oddp +view-side+))                               ; must be odd
(define-constant +cell-size+ 20)                          ; in pixels, cells are always square
(define-constant +view-size+ (* +view-side+ +cell-size+)) ; in pixels, view is always square
(define-constant +view-margin+ 24)                        ; in pixels
(define-constant +plane-axis+ '(47 47) :test #'equal)     ; in pixels, point within plane about which to spin
(define-constant +default-map-size+ 100)                  ; in cells, default for both width and height
(define-constant +default-duration+ (* 10 60))            ; in seconds
(define-constant +polling-interval+ 1000)                 ; milliseconds
(define-constant +flame-flicker-interval+ 150)            ; milliseconds
(define-constant +marker-color+ "#c00" :test #'string-equal)
(define-constant +marker-label-radius+ 4)
(define-constant +marker-characters+ "①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱" :test #'string-equal)
(define-constant +default-extinguish-radius+ 3)           ; cells
(define-constant +minimum-spread+ 2)
(define-constant +maximum-spread+ 5)
(define-constant +extinguishment-color+ "rgb(0 150 255 / 40%)" :test #'string-equal)
(define-constant +tolerance+ 1.0d-12)                     ; equality tolerance when compare floats
(define-constant +minimum-speed+ (floor +cell-size+ 4))  ; pixels per second
(define-constant +maximum-speed+ (* 6 +cell-size+))
(define-constant +initial-speed+ (floor +cell-size+ 0.66))
(define-constant +default-exhaustion-probability+ 0.002)
(define-constant +default-progagation-probability+ 0.01)

(defparameter *port* +default-port+)

(defun =~ (n &rest more)
  (every (lambda (x) (< (abs (- x n)) +tolerance+)) more))

(define-constant +cell-type-names+
    '((:grass t) (:ash nil) (:water nil) (:tree t) (:road nil) (:rock nil) (:house t))
  :test #'equal)
(define-constant +default-cell-type+ (position :grass +cell-type-names+ :key #'first))
(define-constant +image-template+ "images/~(~A~).png" :test #'string=)
(define-constant +region-types+
    '((:grass) (:lake :water) (:river :water t) (:forest :tree) (:road :road t)
      (:outcrop :rock) (:houses :house))
  :test #'equal)



(defmacro define-object-printer (type (&optional (var type)) fmt &rest args)
  `(defmethod print-object ((,var ,type) #0=#:stream)
     (print-unreadable-object (,var #0# :type t :identity t)
       (format #0# ,fmt ,@args))))

(defstruct (cell-type (:conc-name ct-) (:print-object))
  name
  (flammablep nil)
  image-path
  index)

(define-object-printer cell-type (ct) "~S [~D]"
  (ct-name ct)
  (ct-index ct))

(defparameter +cell-types+
  (iter (for (name flammablep) :in +cell-type-names+)
        (for i :from 0)
        (collect (make-cell-type :name name
                                 :flammablep flammablep
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

(defun cells-to-pixels (n &optional (centerp t))
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
  duration
  regions
  (fire-exhaustion-probability +default-exhaustion-probability+)
  (fire-propagation-probability +default-progagation-probability+)
  ignitions
  (costs nil)
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
                                           (duration +default-duration+)
                                           (ignitions nil)
                                           (costs nil)
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
    (iter (for (ctn cost) :on costs :by #'cddr)
          (for ct := (or (get-ct ctn) (v:warn "Unknown cell type ~S in game ~S" ctn name)))
          (unless (and ct (ct-flammablep ct))
            (v:warn "Cell type ~S is not flammable in game ~S" ctn name))
          (unless (and (integerp cost) (> cost 0))
            (v:warn "Burning cost of cell type ~S, ~S, is not a positive integer in game ~S"
                    ctn cost name)))
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
                                                   :duration duration
                                                   :start-y start-y
                                                   :regions reg
                                                   :ignitions ign
                                                   :costs costs
                                                   keys))))))
  name)



(defun compute-extinguish-area (radius)
  ;; doesn't really make it a circle, just a square without the corners,
  ;; but empirically that seems to be more pleasing
  (iter (for x :from (- radius) :to radius)
        (nconcing (iter (for y :from (- radius) :to radius)
                        (unless (= (abs x) (abs y) 3)
                          (collect (list x y)))))))

(defstruct (mission (:constructor %make-mission)
                    (:print-object))
  id
  (players nil)
  game
  map
  ignitions
  (damage 0)
  (click-places-marker-p nil)
  (move-extinguishes-p nil)
  (fires (make-hash-table))
  (last-extinguish-click nil)
  (show-extinguishment-p nil)
  (extinguish-radius +default-extinguish-radius+)
  (extinguish-area (compute-extinguish-area +default-extinguish-radius+))
  (markers nil)
  (marker-name-index 0)
  (concluded-p nil)
  (model nil)
  (remote-model-stream nil)
  log-file-path)

(define-object-printer mission (s) "~A, ~A (~D))"
                       (mission-id s)
                       (game-name (mission-game s))
                       (length (mission-players s)))

(defparameter *missions* (make-hash-table :test 'equalp))

;; (define-constant +extinguish-area+
  ;; (iter (for x :from -3 :to 3)
  ;;       (nconcing (iter (for y :from -3 :to 3)
  ;;                       (unless (= (abs x) (abs y) 3)
  ;;                         (collect (list x y))))))
;;   :test #'equal)

(defun write-to-mission-log (mission kind plist)
  (check-type kind keyword)
  (with-open-file (stream (mission-log-file-path mission)
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (when (oddp (length plist))
      (v:warn "Malformed plist ~S ~S" plist kind))
    (handler-case
        (format stream "~:[~%~;~](~S ~S~%~{ ~S ~:W~^~%~})~%"
                (eq kind :metadata)
                kind
                (lt:format-timestring nil (lt:now))
                plist)
      (error () (v:error "Error writing to mission log (~S, ~S)" kind plist)))))

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
                             :model (game-model game)
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



(defstruct (marker (:print-object))
  name
  location)                             ; pixels, map coordinates

(define-object-printer marker (m) "~A ~S"
                       (marker-name m)
                       (coerce (marker-location m) 'list))

(defun location-region (mission location)
  ;; location is in pixels in the map's coordinate system
  ;; With some effort this could be made faster, but it doesn't seem worth it for
  ;; current use case; maybe someday it will become worthwhile.
  (iter (with map := (mission-map mission))
        (with type := (cell-type (apply #'aref map location)))
        (for r :in (game-regions (mission-game mission)))
        (for c := (region-cells r))
        (unless (eq (cell-type (apply #'aref map (first c))) type)
          (next-iteration))
        (when (member location c :test #'equal)
          (return r))))

;; temporary, make this configurable per game
(defparameter *markers-use-region-names-p* nil)

(defun add-marker (mission location &optional name)
  ;; location is in pixels in the map's coordinate systems
  (unless name
    (setf name (format nil "~D ~A"
                       (incf (mission-marker-name-index mission))
                       (s:shorten (iter (with map := (mission-map mission))
                                        (with coords := (mapcar #'pixels-to-cells location))
                                        (with type := (cell-type (apply #'aref map coords)))
                                        (unless *markers-use-region-names-p*
                                          (finish))
                                        (for r :in (game-regions (mission-game mission)))
                                        (for c := (region-cells r))
                                        (unless (eq (cell-type (apply #'aref map (first c))) type)
                                          (next-iteration))
                                        (when (member coords c :test #'equal)
                                          (return (region-name r)))
                                        (finally (return (string-capitalize (ct-name type)))))
                                  10))))
  (nconcf (mission-markers mission) (list (make-marker :name name :location location))))



(defstruct (player (:constructor %make-player)
                   (:print-object))
  id
  name
  mission
  (speed +initial-speed+)
  (motion nil)
  (map-sent-to-model-p nil))

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



(defun join-mission (&optional mission player-name game)
  "Returns a player named PLAYER-NAME playing a mission with id MISSION.
If PLAYER-NAME is not supplied or is null a new uuid is allocated and used for the name.
If GAME is not supplied or is null the *default-game* is used. If such a mission already
exists it is joined, and otherwise one is created, playing the game named GAME. MISSION
should be a mission ID; if it is not supplied or is mull a new one with a generated ID is
created and used. Signals a SIMPLE-ERROR if the mission already exists and is not playing
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



#|

variables:
  fuel
  water
  speed

parameters:
  dead weight
  min speed
  max speed
  max fuel
  max water
  fuel consumption rate
  water per extinquishment

at each time step reduce the fuel level by
  fuel consumption rate * speed * (dead weight + fuel + water)


|#



;;; Remote models

(defparameter *pending-remote-models* (make-hash-table :test 'equal))
(defparameter *remote-model-client-host* "mneme.lan.cmu.edu")
(defparameter *remote-model-client-port* +default-model-port+)
(defparameter +rmmote-model-timeout+ 10)              ; seconds
(defparameter *previous-pending-remote-model* 0)

(defun remote-model (stream)
  (let ((key (format nil "~D" (incf *previous-pending-remote-model*))))
    (unwind-protect
         (progn
           (setf (gethash key *pending-remote-models*) stream)
           (format stream "~S~%" `(:key ,key :port ,*port*))
           (finish-output stream)
           (v:info "Waiting for remotely modeled game ~D to start" key)
           (iter (sleep 60)
                 (while (open-stream-p stream))))
      (close stream)                    ; in case of a non-local transfer of control
      (remhash key *pending-remote-models*))))

;;; Calling read on a TCP socket-stream is courting disaster if things stall, so we
;;; use line delimited strings. This means we really don't want linefeeds in any Lisp
;;; strings we serialize, so we replace them with spaces; this shouldn't cause any trouble
;;; with the Wildfire use cases.

(defmacro with-standard-io-syntax* (&body forms)
  ;; Deal with the nasty interaction between SBCL's over-finicky *print-readably*
  ;; semantics and prin1-to-string results.
  `(with-standard-io-syntax
     (let ((*print-readably* nil))
       ,@forms)))

(defun tcp-write (object stream)
  (with-standard-io-syntax*
    (handler-case
        (let ((s (prin1-to-string object)))
          (when (find #\Newline s)
            (v:warn "Replacing unexpected newline(s) by space(s) in serialized object ~S"
                    object)
            (setf s (nsubstitute #\Space #\Newline s)))
          (format stream "~A~%" s)
          (finish-output stream)
          t)
      (error () nil))))

(defun tcp-read (stream)
  (with-standard-io-syntax*
    (if-let ((s (read-line stream nil)))
      (read-from-string s)
      :eof)))

(defun conclude-mission (mission)
  (v:debug "Concluding mission ~S" mission)
  (setf (mission-concluded-p mission) t)
  `((:concluded . t)))                  ; value to return to the web client

(defun remote-model-wrapper (mission public private)
  (let ((stream (mission-remote-model-stream mission))
        response)
    (if (and (tcp-write public stream)
             (tcp-write private stream)
             (not (eq (setf response (tcp-read stream)) :eof)))
        response
        (alist-plist (conclude-mission mission)))))

(defun request-mission (model-function &key (host *remote-model-client-host*)
                                         (port *remote-model-client-port*) game trace)
  (declare (ignorable model-function))
  (labels ((tr* (fmt &rest args)
             (format *trace-output* ";;; ~?~%" fmt args))
           (tr (fmt &rest args)
             (when trace
               (apply #'tr* fmt args))))
    (tr "Note that trace output from REQUEST-MISSION is voluminous")
    (setf *remote-model-client-host* host)
    (setf *remote-model-client-port* port)
    (tr "Requesting mission ~A:~D~@[ (~A)~]" host port game)
    (let ((stream nil) (key nil) game-port)
      (unwind-protect
           (setf stream (socket-stream (socket-connect host port :timeout +rmmote-model-timeout+)))
        (unless stream
          (tr* "Failed to connect to server ~A:~D" host port)))
      (unwind-protect
           (let ((response (tcp-read stream)))
             (when (eq response :eof)
               (return-from request-mission))
             (setf game-port (getf response :port))
             (setf key (and game-port (getf response :key))))
        (unless key
          (tr* "Indecipherable response from server ~A:~D" host port)))
      (format t "~%;;; To start the mission point a web browser at http://~A:~D/?remote-model=~D~@[&game=~A~]~2%"
              host game-port key game)
      ;; (iter (for public := (tcp-read stream))
      ;;       (until (or (eq public :eof) (getf public :concluded)))
      ;;       (tr "Model called")
      ;;       (tr "Public:~%~:W" public)
      ;;       (for private := (tcp-read stream))
      ;;       (until (or (eq private :eof) (getf private :concluded)))
      ;;       (tr "Private:~%~:W" private)
      ;;       (for response := (funcall model-function public private))
      ;;       (tr "Response:~%~:W~%" response)
      ;;       (while (tcp-write response stream)))
      )
    (tr "Mission finished")))



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
     (unless mission-over
       (apply (@ smackjack ,name) vals))))



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

    `(ps:var mission-over false)

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
    `(ps:var mission-over-image (load-image "images/mission-concluded.png"))
    `(ps:var velocity '(0 0))
    `(ps:var angle ,(- (/ pi 2)))

    `(defun flame ()
       ;; returns one of the two flame images selected pseudo-randomly
       (aref flames (mod (setf flame-index (logand (1+ (* 257 flame-index)) #x1ffff)) 2))))

(define-easy-handler (mission :uri "/") (game mission player remote-model)
  (let* ((p (handler-case (join-mission mission player game)
              (simple-error (e)
                (return-from mission (failure #?"${e}")))))
         (m (player-mission p))
         (g (mission-game m))
         (*js* *js*))           ; all parenscript added here is only local to this mission
    (when-let ((remote-model-stream (gethash remote-model *pending-remote-models*)))
      (v:info "Remotely modeled game ~D starting" remote-model)
      (remhash remote-model *pending-remote-models*)
      (setf (mission-remote-model-stream m) remote-model-stream)
      (setf (mission-model m) (curry #'remote-model-wrapper m)))
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
                            (:input#move :type "radio" :name "click-action"
                                         :checked (not (mission-click-places-marker-p m))
                                         :onclick (ps:ps (clicked-click-places-marker)))
                            (:label :for "move" "moves airplane"))
                           (:div
                            (:input#mark :type "radio" :name "click-action"
                                         :checked (mission-click-places-marker-p m)
                                         :onclick (ps:ps (clicked-click-places-marker)))
                            (:label :for "mark" "places marker")))
                          (:div.component
                           (:input#extinguish :type "checkbox"
                                              :checked (mission-move-extinguishes-p m)
                                              :onclick (ps:ps (clicked-move-extinguishes)))
                           (:label :for "extinguish"
                                   :style "width: 80%; margin-left: 0.5rem; display: inline-block; vertical-align: middle"
                                   "Extinguish fires at move destination"))
                          (:fieldset.component
                           (:legend "Markers")
                           (:div (:ul.markers#marker-names))))
              (:canvas#view :height (+ +view-size+ (* 2 +view-margin+))
                            :width  (+ +view-size+ (* 2 +view-margin+))
                            :onclick (ps:ps (clicked-map (list (@ event offset-x)
                                                               (@ event offset-y))))
                            "Not supported in this browser")
              (:div.uicol (:div.fillable
                           (:div (:input#speed :type "range" ; range defaults to 0 to 100
                                               :value 55
                                               :onchange (ps:ps (speed-changed)))
                                 (:label :for "speed" "Speed"))
                           (:table
                            (:tr (:td (:label :for "fuel" "Fuel"))
                                 (:td (:meter.fill-control#fuel :low 0.1 :value 0.8)))
                            (:tr (:td.fill-label (:label :for "fuel-fill" "Fill level"))
                                 (:td (:input.fill-control#fuel-fill :type "range"
                                                                     :value 100))))) ; range defaults to 0 to 100
                          (:div.fillable
                           (:div (:input#spread :type "range" ; range defaults to 0 to 100
                                                :value 30
                                                :onchange (ps:ps (spread-changed)))
                                 (:label :for "spread" "Spread"))
                           (:table
                            (:tr (:td (:label :for "water" "Water"))
                                 (:td (:meter.fill-control#water :low 0.1 :value 0.8)))
                            (:tr (:td.fill-label (:label :for "water-fill" "Fill level"))
                                 (:td (:input.fill-control#water-fill :type "range"
                                                                      :value 100))))) ; range defaults to 0 to 100
                          (:table.component#data :style "margin-top: 3rex"
                           (:tr (:td "Time left")
                                (:td (:output#time-remaining "10:15")))
                           (:tr (:td "Damage")
                                (:td (:output#damage "0"))))))
        (:canvas#map :style #?'display: ${(if *debug* "block" "none")}; margin-top: 3rex;'
                     :height (cells-to-pixels (game-height g) nil)
         :width (cells-to-pixels (game-width g) nil))
        ;; the following foolishness is to ensure the Material Icons font is loaded
        (:div (:span.material-icons :style "visibility:hidden" "location_on"))))))

(add-css ".uicol { width: 12rem; margin: 0 0.7rem }"
         ".component { margin-bottom: 0.8rex }"
         ".component legend { padding: 0 0.3rem }"
         ".fillable { margin-bottom: 2.5rex }"
         ".fill-label { font-size: 75%; padding-left: 0.5em }"
         ".fill-control { width: 6rem }"
         "#data td { padding: 0 0.3rem 0.3rem 0 }"
         ".red { color: red }"
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

    `(ps:var extinguish-start 0)
    `(ps:var extinguish-radius 0)

    (destructuring-bind (xp yp) +plane-axis+
      (let* ((view-center (/ +view-size+ 2.0)))
        `(defun display-map ()
           (let ((ctx (view-context)))
             ((@ ctx clear-rect) 0 0
              (+ ,+view-size+ (* 2 ,+view-margin+)) (+ ,+view-size+ (* 2 ,+view-margin+)))
             ((@ ctx draw-image) dragons 0 0
              ,+view-size+ ,+view-size+
              ,+view-margin+ ,+view-margin+
              ,+view-size+ ,+view-size+)
             (with-point (x y) position
               ((@ ctx draw-image) (ps:chain document (get-element-by-id "map"))
                (- x ,view-center) (- y ,view-center)
                ,+view-size+ ,+view-size+
                ,+view-margin+ ,+view-margin+
                ,+view-size+ ,+view-size+)
               ((@ ctx save))
               ((@ ctx translate) (+ ,view-center ,+view-margin+) (+ ,view-center ,+view-margin+))
               ((@ ctx save))

               (when (> extinguish-start 0)
                 (let* ((frac (/ (- (@ document timeline current-time) extinguish-start)
                                ,+polling-interval+
                                0.3))
                        (r (* extinguish-radius frac)))
                   (clog "what?" frac r)
                   ((@ ctx begin-path))
                   (setf (@ ctx fill-style) ,+extinguishment-color+)
                   ((@ ctx arc) 0 0 (if (< r extinguish-radius) r extinguish-radius) 0 ,(* 2 pi))
                   ((@ ctx fill))

                   ))

               ((@ ctx rotate) angle)
               ((@ ctx draw-image) plane ,(- xp) ,(- yp))
               ((@ ctx restore))
               (when mission-over
                 ((@ ctx draw-image) mission-over-image ,(- xp 200) ,(- yp 180)))
               ((@ ctx restore)))
             (draw-margin-markers ctx)))))

    `(defun draw-margin-markers (ctx)
       (let ((hv (/ ,+view-size+ 2.0))
             (n 0))
         (with-point (xp yp) position
           (dolist (m markers)
             (with-point (x y) (@ m location)
               (let* ((xd (- x xp))
                      (xa (abs xd))
                      (yd (- y yp))
                      (ya (abs yd)))
                 (incf n)
                 (cond ((and (< xa  hv) (< ya hv))) ; marker is visible, do nothing
                       ((<= ya xa)
                        (if (< x xp)
                            ;; on left side
                            (draw-marker-character ctx n 0 (* hv (- 1 (/ yd xd))))
                            ;; on right side
                            (draw-marker-character ctx n (+ ,+view-margin+ ,+view-size+) (* hv (+ 1 (/ yd xd))))))
                       (t (if (< y yp)
                              ;; on top
                              (draw-marker-character ctx n (* hv (- 1 (/ xd yd))) ,+view-margin+)
                              ;; on bottom
                              (draw-marker-character ctx n (* hv (+ 1 (/ xd yd))) (+ ,+view-size+ (* 2 ,+view-margin+))))))))))))

    `(defun draw-marker-character (ctx n x y)
       ((@ ctx save))
       (setf (@ ctx fill-style) ,+marker-color+)
       (setf (@ ctx font) "24pt Merriweather Sans")
       ((@ ctx fill-text) ((@ ,+marker-characters+ substring) (- n 1) n) x y ,+view-margin+)
       ((@ ctx restore)))

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
           (with-point (sx sy) (if mission-over '(0 0) velocity)
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
           (when mission-over
             (clear-timeout pending-server-update)
             (return-from update-position))
           (setf last-update ms)
           (when (and (= x tx) (= y ty))
             (setf velocity '(0 0) last-update nil))
           (animation-update))))

    `(ps:var markers (list))

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
       (with-point (x y) (@ marker location)
         ((@ ctx save))
         ((@ ctx translate) x y)
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
         ((@ ctx restore))))

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

(defun queue-motion (player target angle)
  (setf (player-motion player) `((:target . ,target)
                                 (:angle . ,angle)
                                 (:velocity . ,(mapcar (lambda (x) (* (player-speed player) x))
                                                       `(,(cos angle) ,(sin angle)))))))

(defun %clicked-map (player-id target current &optional allow-marker-placement)
  ;; target is in pixels, in the visible region's coordinate system
  ;; current is in pixels, in the underlying map's coordinate system
  (when-let* ((p (get-player player-id))
              (mission (player-mission p))
              (map (mission-map mission))
              (d (mapcar #'- target `(,#0=(+ (/ +view-size+ 2.0) +view-margin+) ,#0#)))
              (new-pos (mapcar #'+ current d))) ; pixels, map's coordinate system
    (when (every #'<
                 `(,least-negative-single-float ,least-negative-single-float)
                 new-pos
                 (mapcar #'* (array-dimensions map) `(,+cell-size+ ,+cell-size+)))
      (when (and allow-marker-placement (mission-click-places-marker-p mission))
        (add-marker mission new-pos)
        (return-from %clicked-map
          `((:markers . ,(iter (for m :in (mission-markers mission))
                               (collect `((:name . ,(marker-name m))
                                          (:location . ,(marker-location m)))))))))
      (let ((cell (apply #'aref map (mapcar #'pixels-to-cells new-pos))))
        (setf (mission-last-extinguish-click mission)
              (and (mission-move-extinguishes-p mission) cell)))
      (unless (apply #'=~ 0 d)
        (let ((angle (- (/ pi 2) (apply #'atan d))))
          (queue-motion p new-pos angle)))))
  nil)

(define-remote-call clicked-map (where player-id current)
  ;; where is in pixels, in the visible region's coordinate system
  ;; current is in pixels, in the underlying map's coordinate system
  (%clicked-map player-id where current t))

(define-remote-call clicked-marker-name (name player-id position)
  (let* ((p (get-player player-id))
         (loc (marker-location (find name (mission-markers (player-mission p))
                                    :key #'marker-name
                                    :test #'string-equal))))
    (unless (equal loc position)
      (let ((angle (apply #'atan (mapcar #'- position loc))))
        (queue-motion p loc angle)))))      ; TODO compute the real angle somehow
;; I think the computation in %click-map is using view coordinates, but markers are
;; necessarily in map coordinates, so we need something a bit different to compute
;; the angle, very confusing.

(js `(defun marker-name-clicked (evt)
       (let ((name (@ evt src-element first-child node-value)))
         (call clicked-marker-name (json) (name player position))))

    `(defun clicked-map (location)
       (call clicked-map (json) (location player position)
             (let ((mkrs (@ json markers)))
               (when mkrs
                 (setf markers mkrs)
                 (let ((ul (ps:chain document (get-element-by-id "marker-names"))))
                   (setf (@ ul inner-h-t-m-l) "")
                   (dolist (m mkrs)
                     (let ((li (ps:chain document (create-element "li")))
                           (txt (ps:chain document (create-text-node (@ m name)))))
                       (ps:chain li (add-event-listener "click" marker-name-clicked))
                       (ps:chain li (append-child txt))
                       (ps:chain ul (append-child li))))))
               (update-server)))))

(define-remote-call clicked-click-places-marker (value player-id)
  (setf (mission-click-places-marker-p (player-mission (get-player player-id))) value)
  nil)

(js `(defun clicked-click-places-marker ()
       (call clicked-click-places-marker ()
             ((@ ((@ document get-element-by-id) "mark") checked) player))))

(define-remote-call clicked-move-extinguishes (value player-id)
  (setf (mission-move-extinguishes-p (player-mission (get-player player-id))) value)
  nil)

(js `(defun clicked-move-extinguishes ()
       (call clicked-move-extinguishes ()
             ((@ ((@ document get-element-by-id) "extinguish") checked) player))))

(define-remote-call speed-changed (value player-id position target angle)
  (let ((p (get-player player-id)))
    ;; the 0.01 below reflects an HTML range defaulting to 0 to 100
    (setf (player-speed p) (+ (floor (* (parse-integer value) 0.01
                                        (- +maximum-speed+ +minimum-speed+)))
                              +minimum-speed+))
    (unless (equal target position)
      (queue-motion p target angle))))

(js `(defun speed-changed ()
       (call speed-changed () ((@ ((@ document get-element-by-id) "speed") value)
                               player position target angle))
       (update-server)))

(define-remote-call spread-changed (value player-id)
  ;; TODO the spread should be per player, not per mission, FIX this
  (let* ((p (get-player player-id))
         (m (player-mission p)))
    ;; the 0.01 below reflects an HTML range defaulting to 0 to 100
    (setf (mission-extinguish-radius m) (+ (floor (* (parse-integer value) 0.01
                                                     (- +maximum-spread+ +minimum-spread+)))
                                           +minimum-spread+))))

(js `(defun spread-changed () (value player-id)
       (call spread-changed () ((@ ((@ document get-element-by-id) "spread") value)
                                player))
       (update-server)))



(defun propagate-fires (mission state)
  (let* ((g (mission-game mission))
         (m (mission-map mission))
         (exhaustion-probability (game-fire-exhaustion-probability g))
         (propagation-probability (game-fire-propagation-probability g))
         (ash-type (get-ct :ash))
         (births nil)
         (deaths nil))
    (assert ash-type)
    (labels ((coords (cell)
               (list (cell-x cell) (cell-y cell)))
             (ignite (cell)
               (setf (cell-burningp cell) t)
               (setf (gethash cell (mission-fires mission)) t)
               (push (coords cell) births)
               (incf (mission-damage mission)
                     (getf (game-costs g) (ct-name (cell-type cell)) 1))))
      (when-let ((last-click (mission-last-extinguish-click mission)))
        (when (and (every #'zerop (cdr (assoc :velocity state)))
                   (equal (mapcar #'pixels-to-cells (cdr (assoc :position state)))
                          (coords last-click)))
          (setf (mission-show-extinguishment-p mission) t)
          (setf (mission-last-extinguish-click mission) nil)
          ;; refactor for commonality with what follows
          (iter (for (xo yo) :in (mission-extinguish-area mission))
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
                                      (ct-flammablep (cell-type candidate))
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

(defun make-model-private-state (player client-state)
  (let ((result nil))
    (labels ((push-item (key value)
               (push (cons key value) result)))
      (unless (player-map-sent-to-model-p player)
        (let* ((m (player-mission player))
               (g (mission-game m))
               (gmap (game-map g))
               (map (make-array (array-dimensions gmap))))
          (iter (for x :from 0 :below (array-dimension gmap 0))
                (iter (for y :from 0 :below (array-dimension gmap 1))
                      (setf (aref map x y) (ct-name (aref +cell-types+ (aref gmap x y))))))
          (push-item :map map)
          (push-item :regions (iter (for r :in (game-regions g))
                                    (collect `(,(region-name r) . ,(region-cells r)))))
          (push-item :ignitions (game-ignitions g))
          (push-item :fire-propagation-probability (game-fire-propagation-probability g))
          (push-item :fire-exhaustion-probability (game-fire-exhaustion-probability g))
          (push-item :duration (game-duration g))
          (push-item :start-y (game-start-y g))
          (push-item :start-x (game-start-x g))
          (push-item :height (game-height g))
          (push-item :width (game-width g))
          (push-item :mission-id (mission-id m))
          (push-item :game (string (game-name g))))
      (setf (player-map-sent-to-model-p player) t)))
    (alist-plist (append client-state result))))

(defun make-model-public-state (player client-state)
  (let ((m (player-mission player))
        (result (iter (for pass-through :in '(:time :angle))
                      (nconcing `(,pass-through ,(cdr (assoc  pass-through client-state)))
                        :into passed-through)
                      (finally (return `(,@passed-through
                                         :center ,+center+
                                         :speed ,(/ (sqrt (apply #'+ (mapcar (rcurry #'expt 2)
                                                                             (cdr (assoc :velocity client-state)))))
                                                    +maximum-speed+)
                                         :regions nil
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
          (finally (setf (getf result :regions) (make-local-regions type-cells v))
                   (return result)))))

(defun format-time (sec)
  (multiple-value-bind (min s) (floor sec 60)
    (if (<= min 60)
        (format nil "~D:~2,'0D" min s)
        (multiple-value-bind (h m) (floor min 60)
          (format nil "~D:~2,'0D:~2,'0D" h m s)))))

(define-remote-call server-update (player-id state)
  (when-let* ((p (get-player player-id))
              (m (player-mission p))
              (g (mission-game m))
              (tm (round (- (game-duration g) (/ (cdr (assoc :time state)) 1000)))))
    (write-to-mission-log m :update-request-from-client (alist-plist state))
    (let (response)
      (cond ((<= tm 0)
             (setf response (conclude-mission m)))
            (t (when-let ((mod (mission-model m))
                          (private (make-model-private-state p state))
                          (public (make-model-public-state p state)))
                 (write-to-mission-log m :call-model `(:public ,public :private ,private))
                 (when-let ((model-response (funcall mod public private)))
                   (when (and model-response (atom model-response))
                     (v:error "Horrible disaster reading model response!! ~S" model-response))
                   (write-to-mission-log m :model-response model-response)
                   (%clicked-map player-id
                                 (mapcar #'cells-to-pixels (getf model-response :target))
                                 (cdr (assoc :position state)))))
               (multiple-value-bind (births deaths) (propagate-fires m state)
                 (setf response `((:motion . ,(shiftf (player-motion p) nil))
                                  (:ignite . ,births)
                                  (:extinguish . ,deaths)
                                  (:damage . ,(format nil "~:D" (mission-damage m)))
                                  (:time-remaining . ,(format-time tm))))
                 (when (mission-show-extinguishment-p m)
                   (push `(:show-extinguishment
                           . ,(* (+ (mission-extinguish-radius m) 0.8) +cell-size+))
                         response)
                   (setf (mission-show-extinguishment-p m) nil)))))
      (write-to-mission-log m :update-from-server (alist-plist response))
      response)))

(js `(defun motion (json)
       (when json
         (setf target (@ json target))
         (setf angle (@ json angle))
         (setf velocity (@ json velocity))
         (setf (ps:chain document body style cursor) "none")
         (update-position)))

    `(ps:var ash (aref images ,(position :ash +cell-type-names+ :key #'car)))

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

    `(defun damage (n)
       (let ((e (ps:chain document (get-element-by-id "damage"))))
         (when (> n 0)
           (setf (@ e class-name) "red"))
         (when n
           (setf (@ e value) n))))

    `(ps:var pending-server-update nil)

    `(defun set-server-update ()
       (setf pending-server-update (set-timeout update-server ,+polling-interval+)))

    `(defun update-server ()
       (when pending-server-update
         (clear-timeout pending-server-update)
         (setf pending-server-update nil))
       (setf extinguish-start 0)
       (let* ((time (@ document timeline current-time))
              (state (ps:create :time time
                                :position position
                                :target target
                                :velocity velocity
                                :angle angle)))
         (call server-update (json) (player state)
               (cond ((@ json concluded)
                      (setf mission-over true)
                      (set-time-display))
                     (t (set-time-display (@ json time-remaining))
                        (extinguish (@ json extinguish))
                        (let ((r (@ json show-extinguishment)))
                          (if (and r (> r 0))
                              (setf extinguish-start time extinguish-radius r)
                              (setf extinguish-start 0))
                        (ignite (@ json ignite))
                        (damage (@ json damage))
                        (motion (@ json motion))))))
       (set-server-update)))

    `(defun set-time-display (&optional val)
       (setf (ps:chain document (get-element-by-id "time-remaining") value)
             (or val "0:00")))

    `(set-server-update))



(js '(setf (@ window onload) load-image-test-and-render))



(defparameter *model-port* +default-model-port+)
(defvar *server* nil)
(defvar *remote-model-thread* nil)
(defvar *remote-model-socket* nil)

(push (create-ajax-dispatcher *ajax*) *dispatch-table*)

(push (create-folder-dispatcher-and-handler "/images/" "images/") *dispatch-table*)

(defun stop-server (&optional (soft t))
  (let ((result *server*))
    (cond (*server*
           (v:info "Stopping ~A" *server*)
           (stop *server* :soft soft)
           (v:info "~A stopped" *server*)
           (setf *server* nil))
          (t (v:warn "No server was running")))
    (when *remote-model-socket*
      (socket-close *remote-model-socket*))
    (sleep 0.2)
    (when (and *remote-model-thread* (thread-alive-p *remote-model-thread*))
      (destroy-thread *remote-model-thread*))
    result))

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

(defun start-server (&key (port *port*) (model-port *model-port*) debug)
  (enable-debug debug)
  (setf *port* port)
  (setf *model-port* model-port)
  (when *server*
    (v:warn "Server ~S already running, restarting it" *server*)
    (stop-server))
  (load-from-subdirectory "games/")
  (assert (get-game *default-game*))
  (load-from-subdirectory "models/")
  ;; (multiple-value-setq (*remote-model-thread* *remote-model-socket*)
  ;;   (socket-server nil model-port 'remote-model () :in-new-thread t)) ; :multi-threading t))
  ;; (v:info "Model port ~D" model-port)
  (setf *server* (start (make-instance 'easy-acceptor
                                       :document-root *data-directory*
                                       :access-log-destination *access-log*
                                       :port port)))
  (v:info "Started ~A" *server*)
  *server*)



(defun run-standalone ()
  (start-server)
  (loop (sleep 100000000)))

;;; TODO This will need to keep trace of where the current location is, which
;;;      is currently done by the browser; note that the browser is doing this in pixels.
;;;      This will also have to use the velocity and target to update the position, and
;;;      have some sort of magic for landing on the target: maybe the way to do that is to
;;;      compute the motion in a ~ms time granularity loop? And, of course, it must send a
;;;      state update every +polling-interval+ milliseconds.
(defun headless-mission (&key model game mission player)
  (let* ((p (join-mission mission player game))
         (m (player-mission p))
         (g (mission-game m)))
    (when model
      (setf (mission-model m) model))
    (unless (mission-model m)
      (error "A headless mission cannot be run without a model, either one supplied ~
              explicitly in the call to HEADLESS-MISSION or implicitly in ~A."
             g))
    (iter (for i from 1 to 100)
          (server-update p nil))))



;; ;; TODO need to figure out what to do so this doesn't get in the way on Mneme
;; (when (and (not (find-package "SWANK")) (y-or-n-p "Start Wildfire server?"))
;;   (start-server))

#|

(progn
  (swank:set-default-directory "/home/dfm/w/wildfire/")
  (load "wildfire")
  (swank:set-package "WILDFIRE")
  (funcall (find-symbol "START-SERVER" (find-package "WILDFIRE")) :debug nil))

|#
