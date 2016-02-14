(in-package cl-iterative-ex)

;; * Extension of iterative method
;; ** Upgrade the result indicator
(defmethod fixed-point-result-indicator ((iterator iteratorex:iteratorex))
  (iteratorex:info iterator))

;; ** Add info to iterations
(defclass add-info ()
  ((init-info :initarg :init-info :reader init-info))
  (:documentation "Control that adds info to an iterator"))

(defun add-info (&optional init-info)
  "Constructs ADD-INFO control"
  (make-instance 'add-info :init-info init-info))

(defmethod apply-control ((control add-info) (val iterator:iterator))
  (iteratorex:upgrade val (init-info control)))

(defmethod apply-control ((control add-info) (val iteratorex::iteratorex))
  val)

;; ** Extend existing controls to add info
;; *** Super-class for all controls adding info 
(defclass control-with-id ()
  ((id :initarg :id :reader control-id)
   (push-info-p :initarg :push-info-p :reader push-info-p))
  (:documentation "Control mixin that adds ID to other controls"))

(defmethod apply-control :after ((control control-with-id) (value iteratorex:iteratorex))
  (when (funcall (push-info-p control) value)
    (push (control-id control) (iteratorex:info value))))

;; Trivial implementation for
;; controls (or anything else) that is not CONTROL-WITH-ID
(defmethod control-id (obj) nil)

(defmethod push-info-p (obj) (constantly nil))

;; *** Finished value
(defclass finished-value-with-id (finished-value control-with-id)
  ()
  (:documentation "Extension of FINISHED-VALUE with ID info"))

(defun finished-value-with-id (predicate &optional id (final #'identity))
  "Constructs FINIED-VALUE-WITH-ID control"
  (make-instance 'finished-value-with-id
    :id (cons :finished-value id)
    :push-info-p #'iterator:finished-p
    :final final
    :predicate predicate))

;; *** Failed value
(defclass failed-value-with-id (failed-value control-with-id)
  ()
  (:documentation "Extension of FAILED-VALUE with ID info"))

(defun failed-value-with-id (predicate &optional id (final #'identity))
  "Constructs FAILED-VALUE-WITH-ID control"
  (make-instance 'failed-value-with-id
    :id (cons :fialed-value id)
    :push-info-p #'iterator:failed-p
    :final final
    :predicate predicate))

;; *** Limit iterations
(defclass limit-iterations-with-id (limit-iterations control-with-id)
  ()
  (:documentation "Extension of LIMIT-ITERATIONS with ID info"))

(defun limit-iterations-with-id (max &optional id (final #'identity))
  "Constructs LIMIT-ITERATIONS-WITH-ID control"
  (make-instance 'limit-iterations-with-id
    :id (cons :limit-iterations (or id max))
    :push-info-p #'iterator:failed-p
    :max max
    :final final))

;; *** Converged value
(defclass converged-value-with-id (converged-value control-with-id)
  ()
  (:documentation "Extension of CONVERGED-VALUE with ID info"))

(defun converged-value-with-id (close-p copy &optional id (final #'identity))
  "Constructs CONVERGED-VALUE-WITH-ID control"
  (make-instance 'converged-value-with-id
    :id (cons :converged-value id)
    :push-info-p #'iterator:finished-p
    :close-p close-p
    :copy copy
    :final final))

;; *** Converged number
(defclass converged-number-with-id (converged-number control-with-id)
  ()
  (:documentation "Extension of CONVERGED-NUMBER with ID info"))

(defun converged-number-with-id (&optional id (tolerance (sqrt double-float-epsilon)))
  "Constructs CONVERGED-NUMBER-WITH-ID control"
  (make-instance 'converged-number-with-id
    :id (cons :converged-number id)
    :push-info-p #'iterator:finished-p
    :tolerance tolerance))

;; ** Example
;; (time
;;  (flet ((log-function (indicator val)
;;           (if (eq indicator :init)
;;               (format t "~&INIT: ~A~%" val)
;;               (format t "~&x = ~A~%" val))))
;;    (fixed-point #'sqrt 2d0
;;                 :pre-treat (add-info '((:upgraded)))
;;                 :controls (combine-controls (log-computation #'log-function)
;;                                             (finished-value-with-id (lambda (x)
;;                                                                       (< (abs (- x 1d0))
;;                                                                          1d-7))
;;                                                                     'finished)
;;                                             (limit-iterations-with-id 20)))))
