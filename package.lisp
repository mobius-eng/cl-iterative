(in-package cl-user)

(defpackage #:iterator
  (:use #:cl #:optima)
  (:shadow #:continue)
  (:export #:iterator #:status #:value
           #:continue #:failed #:finished
           #:continue-p #:finished-p #:failed-p
           #:change-status #:->continue #:->failed #:->finished
           #:replace-value #:update-value
           #:bind #:fmap)
  (:documentation
   "Computation flow"))

(defpackage #:cl-iterative
  (:use #:cl #:optima)
  (:import-from #:alexandria #:make-keyword)
  (:export #:apply-control #:init-control
           #:define-simple-constructor
           #:finished-value
           #:failed-value
           #:limit-iterations
           #:log-computation
           #:converged-value
           #:converged-number
           #:alter-value
           #:control #:control-init-function #:control-apply-function
           #:combine-controls
           #:fixed-point-result-indicator
           #:fixed-point #:iterate)
  (:documentation "Control of computation flow and iterative algorithm"))

(defpackage :iteratorex
  (:use #:cl #:cl-iterative)
  (:export #:info #:upgrade #:iteratorex)
  (:documentation "Extension of ITERATOR to annotate it with INFO"))

(defpackage #:cl-iterative-ex
  (:use #:cl #:cl-iterative)
  (:export #:add-info #:init-info
           #:control-with-id #:control-id #:push-info-p
           #:finished-value-with-id
           #:failed-value-with-id
           #:limit-iterations-with-id
           #:converged-value-with-id
           #:converged-number-with-id)
  (:documentation "Upgrades controls from CL-ITERATIVE to work with ITERATOREX"))
