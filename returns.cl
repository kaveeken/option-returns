(ql:quickload :eazy-gnuplot)
(use-package :eazy-gnuplot)

(defun option-return (strike-price option-cost current-price option-type)
  "Calculate the return of an option contract"
  (case option-type
    (:call (max (- option-cost) (- current-price strike-price option-cost)))
    (:put (max (- option-cost) (- strike-price current-price option-cost)))))

(defun histogram-like-plot (output)
  (with-plots (s :debug nil)
    (gp-setup :terminal '(pngcairo) :output output)
    (gp :set :xzeroaxis 'nil :linetype 3 :lc "black")
    (gp :set :grid 'nil)
    (plot
     (lambda ()
       (loop for i from 120 upto 200 by 0.1
             do (format s "~&~a ~a" i (option-return 150.0 20.0 i :call))))
     :with '(:histeps)))
  output)
(histogram-like-plot "images/function-plot.png")

(defclass contract ()
  ((price :reader contract-price
          :initarg :price
          :type long-float) ; change to integer (millis)?
   (date :reader contract-date
         :initarg :date
         :type integer))) ; days?

;; claude
(defclass contract ()
  ((price :accessor price :initarg :price :type number)
   (date :accessor date :initarg :date)))

(defclass option-contract (contract)
  ((strike-price :accessor strike-price :initarg :strike-price :type number)))

(defclass put (option-contract) ())

(defclass call (option-contract) ())

(defparameter *my-call* (make-instance 'call :price 5.25 :date '(2023 12 15) :strike-price 100.0))

(defgeneric exercise-value (option underlying-price)
  (:documentation "Compute the exercise value of an option at the given underlying price"))

(defmethod exercise-value ((option put) underlying-price)
  (max 0 (- (strike-price option) underlying-price)))

(defmethod exercise-value ((option call) underlying-price)
  (max 0 (- underlying-price (strike-price option))))

;; Returns positive value when underlying < strike for puts
(exercise-value *my-call* 110.0) ;; => 10.0
(exercise-value *my-call* 90.0) ;; => 0.0

(defgeneric writer-return (option expiration-price)
  (:documentation "Compute the return for an option writer when the underlying expires at the given price"))

(defmethod writer-return ((option option-contract) expiration-price)
  (let ((premium (price option))
        (exercise-cost (exercise-value option expiration-price)))
    (- premium exercise-cost)))

;; Usage examples:
;; For a call with strike 100, premium 5.25:
;; - If expires at 90: writer keeps full premium (5.25)
;; - If expires at 110: writer pays 10, keeps premium, net return = 5.25 - 10 = -4.75

(defclass option-position ()
  ((bought-contracts :accessor bought-contracts :initarg :bought-contracts 
                    :initform nil :type list
                    :documentation "List of contracts bought/long")
   (written-contracts :accessor written-contracts :initarg :written-contracts 
                     :initform nil :type list
                     :documentation "List of contracts written/short")
   (underlying-lots :accessor underlying-lots :initarg :underlying-lots
                   :initform 0 :type number
                   :documentation "Number of underlying lots held")
   (underlying-cost :accessor underlying-cost :initarg :underlying-cost
                   :initform 0 :type number
                   :documentation "Average cost of held underlying lots")))

;; Example usage:
(defparameter *my-option-position* 
  (make-instance 'option-position
                :bought-contracts (list (make-instance 'call :price 5.25 :date '(2023 12 15) :strike-price 100.0))
                :written-contracts (list (make-instance 'put :price 4.50 :date '(2023 12 15) :strike-price 95.0))
                :underlying-lots 1
                :underlying-cost 100.0))

(defun option-position-return (option-position underlying-price)
  "Compute the total return of a option-position at the given underlying price"
  (let ((bought-return (reduce #'+ (mapcar (lambda (contract)
                                             (exercise-value contract underlying-price))
                                           (bought-contracts option-position))))
        (written-return (reduce #'+ (mapcar (lambda (contract)
                                              (writer-return contract underlying-price))
                                            (written-contracts option-position))))
        (underlying-return (* 100 (underlying-lots option-position)
                              (- underlying-price (underlying-cost option-position)))))
    (+ bought-return written-return underlying-return)))

;; Example usage:
;; For a option-position with:
;; - Long 1 call at strike 100, premium 5.25
;; - Short 1 put at strike 95, premium 4.50
;; - 100 lots of underlying
;; 
;; At price 105:
;; - Long call return: max(0, 105-100) = 5
;; - Short put return: 4.50 - max(0, 95-105) = 4.50
;; - Underlying: 100 * 105 = 10500
;; - Total: 5 + 4.50 + 10500 = 10509.50

;; Create a sample option-position with multiple contracts and underlying
(defparameter *example-option-position*
  (make-instance 'option-position
    :bought-contracts 
      (list (make-instance 'call :price 5.25 :date '(2023 12 15) :strike-price 100.0)
            (make-instance 'put :price 4.00 :date '(2023 12 15) :strike-price 90.0))
    :written-contracts 
      (list (make-instance 'call :price 2.50 :date '(2023 12 15) :strike-price 120.0)
            (make-instance 'put :price 3.75 :date '(2023 12 15) :strike-price 80.0))
    :underlying-lots 1
    :underlying-cost 100.0))

;; Test the option-position return at different prices
(format t "Option-Position return at $95: $~,2f~%" (option-position-return *example-option-position* 95))
(format t "Option-Position return at $105: $~,2f~%" (option-position-return *example-option-position* 105))
(format t "Option-Position return at $85: $~,2f~%" (option-position-return *example-option-position* 85))
(format t "Option-Position return at $125: $~,2f~%" (option-position-return *example-option-position* 125))

;; ;; Generic functions with standard method combination
;; (defgeneric exercise-value (contract underlying-price)
;;   (:documentation "Calculate the exercise value of a contract"))
;; 
;; (defgeneric writer-return (contract underlying-price)
;;   (:documentation "Calculate the return for writing a contract"))
;; 
;; ;; Base method for contract class: account for premium
;; (defmethod exercise-value ((contract contract) underlying-price)
;;   (- 0 (price contract)))  ;; By default, lose the premium
;; 
;; ;; Put and Call add their specific intrinsic values
;; (defmethod exercise-value ((option put) underlying-price)
;;   (+ (call-next-method)  ;; Get the premium effect from parent method
;;      (max 0 (- (strike-price option) underlying-price))))  ;; Add put intrinsic value
;; 
;; (defmethod exercise-value ((option call) underlying-price)
;;   (+ (call-next-method)  ;; Get the premium effect from parent method
;;      (max 0 (- underlying-price (strike-price option)))))  ;; Add call intrinsic value
;; 
;; ;; Writer return is similar but inverted
;; (defmethod writer-return ((contract contract) underlying-price)
;;   (price contract))  ;; By default, keep the premium
;; 
;; (defmethod writer-return ((option put) underlying-price)
;;   (- (call-next-method)  ;; Get premium effect
;;      (max 0 (- (strike-price option) underlying-price))))  ;; Subtract potential obligation
;; 
;; (defmethod writer-return ((option call) underlying-price)
;;   (- (call-next-method)  ;; Get premium effect
;;      (max 0 (- underlying-price (strike-price option)))))  ;; Subtract potential obligation
