(in-package iterator)

(defclass iterator ()
  ((status :initarg :status
           :accessor status
           :documentation "Iterator status: :continue :finished or :failed")
   (value :initarg :value
          :accessor value
          :documentation "Iterator value"))
  (:documentation "Computation flow status"))

(defmethod print-object ((obj iterator) out)
  (print-unreadable-object (obj out)
    (with-slots (status value) obj
      (format out "~A ~A" status value))))

(defun iterator-p (obj)
  "Returns T if a given OBJ is ITERATOR"
  (typep obj 'iterator))

(defun iterator (status value)
  "Constructs ITERATOR with given STATUS and VALUE"
  (make-instance 'iterator :status status :value value))

(defun continue (val)
  "Make contine iterator"
  (iterator :continue val))

(defun failed (val)
  "Make failed iterator"
  (iterator :failed val))

(defun finished (val)
  "Make finished iterator"
  (iterator :finished val))

(declaim (inline status-p))
(defun status-p (obj status)
  "If OBJ is ITERATOR checks its STATUS"
  (and (iterator-p obj) (eq (slot-value obj 'status) status)))

(defun continue-p (iterator)
  "Is ITERATOR :CONTINUE?"
  (status-p iterator :continue))

(defun failed-p (iterator)
  "Is ITERATOR :FAILED?"
  (status-p iterator :failed))

(defun finished-p (iterator)
  "Is ITERATOR :FINISHED?"
  (status-p iterator :finished))


(defun replace-value (iterator new-value)
  "Destrcutively change ITERATOR's value"
  (setf (value iterator) new-value)
  iterator)


(defun update-value (iterator update-function &rest args)
  "Destructively update ITERATOR's value"
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type iterator iterator)
           (type function update-function))
  (setf (slot-value iterator 'value)
        (apply update-function (slot-value iterator 'value) args))
  iterator)


(defun change-status (iterator new-status &optional update-value-function)
  "Destructively change iterator's status"
  (setf (status iterator) new-status)
  (if update-value-function
      (update-value iterator update-value-function)
      iterator))

(defun ->continue (iterator &optional (update-value-function #'identity))
  "Change status of the ITERATOR to :CONTINUE"
  (change-status iterator :continue update-value-function))

(defun ->failed (iterator &optional (update-value-function #'identity))
  "Change status of the ITERATOR to :FAILED"
  (change-status iterator :failed update-value-function))

(defun ->finished (iterator &optional (update-value-function #'identity))
  "Change status of the ITERATOR to :FINISHED"
  (change-status iterator :finished update-value-function))

(defun bind (iterator &rest functions)
  "Propogates ITERATOR's value through FUNCTIONS iff its status is
:CONTINUE. Each function must accept iterator's value and return a new
iterator."
  (cond ((or (null functions) (not (continue-p iterator))) iterator)
        (t (bind (funcall (first functions) (value iterator))
                 (rest functions)))))

(defun fmap (iterator &rest functions)
  "Update iterators value iff it is :CONTINUE"
  (if (continue-p iterator)
      (update-value iterator
                    (lambda (x)
                      (reduce (lambda (r f) (funcall f r))
                              functions
                              :initial-value x)))
      iterator))
