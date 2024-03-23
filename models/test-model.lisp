;;;; Copyright 2024 Carnegie Mellon University

(in-package :wildfire)

;;; This simple example waits five seconds after the mission starts, and then
;;;
;;; a) if the player is moving, does nothing
;;;
;;; b) if the player is stopped it looks for cells in its view that are on fire, and if it
;;;    finds any that it is not already directly on top of it picks one at random and moves to
;;;    it
;;;
;;; c) if the player is topped and no such fires are found it selects a cell at random that is
;;;    at least three cells away from the current position and moves to it.

(defun test-model (state)
  (unless (and (zerop (getf state :speed)) (> (getf state :time) 5000))
    (return-from test-model))
  (let ((v (getf state :view))
        (ctr (getf state :center)))
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
