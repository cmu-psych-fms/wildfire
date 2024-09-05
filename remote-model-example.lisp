(load "wildfire")

(defparameter *moved-once* nil)

(defun model (public private)
  (declare (ignorable private))
  (unless *moved-once*
    (when (>= (getf public :time) 5000)
      (setf *moved-once* t)
      '(:target (25 10)))))

(defun run (&optional host)
  (apply #'wf:request-mission #'model :trace t (when host `(:host ,host))))
