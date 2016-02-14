(in-package iteratorex)

(defclass iteratorex (iterator:iterator)
  ((info :initform nil :accessor info)))

(defmethod print-object ((obj iteratorex) out)
  (print-unreadable-object (obj out :type nil)
    (format out "~A ~@<~A ~:_INFO: ~:_~A~:>"
            (iterator:status obj)
            (iterator:value obj)
            (info obj))))

(defmethod info ((obj iterator:iterator))
  nil)

(defmethod change-class :after
    ((instance iterator::iterator) (new-class-name (eql 'iteratorex)) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp instance 'info)
   (setf (slot-value instance 'info) nil)))


(defun upgrade (iterator &optional info)
  (let ((iteratorex (change-class iterator 'iteratorex)))
    (setf (info iteratorex) info)
    iteratorex))


