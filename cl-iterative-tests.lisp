(defpackage :cl-iterative-tests
  (:use #:cl #:cl-iterative #:cl-iterative-ex :fiveam))

(in-package cl-iterative-tests)


(def-suite cl-iterative-tests)

(in-suite cl-iterative-tests)

(test sqrt-converges-to-1
  "Tests the convergence of

           ---
    x = \\/ x

to 1"
  (multiple-value-bind (solution successful-p info)
      (fixed-point #'sqrt 2d0
                   :pre-treat (add-info '((:upgraded)))
                   :controls (combine-controls
                              (converged-number-with-id 'converged)
                              (limit-iterations-with-id 100)))
    (is-true (< (abs (- solution 1d0)) (sqrt double-float-epsilon))
             "~&Solution ~A does not have a desired precision~%" solution)
    (is (eq t successful-p) "~&Solution was not successful~%")
    (is-true (assoc :upgraded info)
             "~&ADD-INFO argument was not added to the INFO list~%")
    (is-true (assoc :converged-number info)
             "~&CONVERGED-NUMBER-WITH-ID should have added entry into INFO list~%")))

(test heron-method-for-sqrt
  "Find the sqrt of a number using Heron's method:

           1 /      S   \\
    x    = - |x  + ---  |
     n+1   2 \\ n    x   /
                     n
 "
  (for-all ((s (gen-float :bound 1d5 :type 'double-float)))
    (let ((s (abs s)))
      (flet ((improve (x)
               (* 0.5d0 (+ x (/ s x)))))
        (multiple-value-bind (final-x successful-p)
            (fixed-point #'improve 1d0
                         :pre-treat (add-info)
                         :controls (combine-controls
                                    (converged-number-with-id)
                                    (limit-iterations-with-id 20)))
          (is-true (< (abs (- (expt final-x 2) s)) (sqrt double-float-epsilon))
                   "~&X = ~,6F is not a sqrt of S = ~,6F~%" final-x s)
          (is (eq t successful-p)
              "~&Final solution for S = ~,6F was not reached~%" s))))))

(test iterative-vector-problem
  "Solves the vector problem

    /   \\   /    ------- \\
    | x |    |  \| x + y  |
    | y | =  |    0.2 y   |
    \\   /    \\            /

using iterative algorithm. Solution should
converge to (1 0). Tests the in-place modification
of the vector.
"
  (flet ((equation (vector)
           (setf (aref vector 0) (sqrt (+ (aref vector 0) (aref vector 1))))
           (setf (aref vector 1) (* 0.2 (aref vector 1)))
           vector)
         (eq-vectors (vector1 vector2)
           (let ((result 0d0))
             (dotimes (i (length vector1) (< result double-float-epsilon))
               (incf result (expt (- (aref vector1 i) (aref vector2 i)) 2))))))
    (multiple-value-bind (solution successful-p)
        (fixed-point #'equation
                     (vector 10d0 7d0)
                     :controls (combine-controls
                                (converged-value #'eq-vectors #'copy-seq)
                                (limit-iterations 100)))
      (is-true (eq-vectors solution (vector 1d0 0d0)))
      (is (eq t successful-p)))))

(run! 'cl-iterative-tests)
