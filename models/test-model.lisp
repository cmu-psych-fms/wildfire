;;;; Copyright 2024 Carnegie Mellon University

; (in-package :wildfire)

;;; This simple example waits five seconds after the mission starts, and then
;;;
;;; a) if the player is moving, does nothing
;;;
;;; b) if the player is stopped it looks for cells in its view that are on fire, and if
;;;    it finds any that it is not already directly on top of it picks one at random and
;;;    moves to it
;;;
;;; c) if the player is stopped and no such fires are found it selects a cell at random
;;;    that is at least three cells away from the current position and moves to it.
;;;
;;; If *model-trace-output* is non-nil it should be a string, a pathname or be a text
;;; stream open for writing, in which case this example writes the data supplied on each
;;; call to a file of the given name or to the supplied stream.

(defvar *model-trace-output* nil)
(defvar *model-trace-print-length* 4)

(defun %test-model (public private)
  (declare (ignorable private))
  (unless (and (zerop (getf public :speed)) (> (getf public :time) 5000))
    (return-from %test-model))
  (let ((v (getf public :view))
        (ctr (getf public :center)))
    (labels ((matching-locs (pred)
               (iter (for x :from 0 :below (array-dimension v 0))
                     (nconcing (iter (for y :from 0 :below (array-dimension v 1))
                                     (for loc := (list x y))
                                     (when (funcall pred loc)
                                       (collect loc))))))
             (ref (loc)
               (apply #'aref v loc)))
      `(:target ,(random-elt (or (matching-locs (lambda (loc)
                                                  (and (second (ref loc))
                                                       (not (equal loc ctr)))))
                                 (matching-locs (lambda (loc)
                                                  (and (ref loc)
                                                       (>= (sqrt (apply #'+ (mapcar (rcurry #'expt 2)
                                                                                    (mapcar #'- loc))))
                                                           3))))))))))

(defun test-model (public private)
  (labels ((trace-test-model (stream public private)
             (labels ((fmt (&rest args)
                        (when stream
                          (let ((*print-length* *model-trace-print-length*))
                            (apply #'format stream args)))))
               (fmt "~&;;; test-model called:~%~@{(~{~(~S~) ~:W~^~% ~})~%~}"
                    public private)
               (let ((result (%test-model public private)))
                 (fmt ";;; returning:~%~:W~2%" result)
                 result))))
    (typecase *model-trace-output*
      (stream (trace-test-model *model-trace-output* public private))
      ((or pathname string) (with-open-file (s *model-trace-output*
                                               :direction :output
                                               :if-does-not-exist :create
                                               :if-exists :append)
                              (trace-test-model s public private)))
      (t (trace-test-model nil public private)))))
