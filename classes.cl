(defgeneric generate-return-function (object)
  (:documentation "Generate a function that computes the excercise value of the object for a given price."))

(defclass stock ()
  ((price :reader stock-price
          :initarg :price
          :type number)
   (conid :reader conid
          :initarg :conid
          :type string) ;; string of number?
   (months :reader available-months
           :initarg :months
           :type list)))

(defmethod generate-return-function ((object stock))
  (lambda (price) (- price (stock-price object))))

(defclass option-chain ()
  ((underlying :reader option-chain-underlying
               :initarg :underlying
               :type stock)
   (held-lots :accessor held-lots
              :initarg :lots
              :initform 0
              :type integer)
   (month :reader option-chain-month
          :initarg :month
          :type string)
   (rows :reader option-chain-rows
         :initarg :rows
         :type list)))

(defmethod generate-return-function ((object option-chain))
  (lambda (price)
    (+ (* (held-lots object)
          (funcall (generate-return-function (option-chain-underlying object)) price))
       (reduce (lambda (acc row)
                 (+ acc (funcall (generate-return-function row) price)))
               (option-chain-rows object)
               :initial-value 0.0))))

(defclass option-chain-row ()
  ((strike-price :reader strike-price ; held twice
                 :initarg :strike
                 :type number)
   (call :reader option-row-call
         :initarg :call
         :type option-contract)
   (call-position :accessor call-position
                  :initarg :call-position
                  :initform 0
                  :type integer)
   (put :reader option-row-put
        :initarg :put
        :type option-contract)
   (put-position :accessor put-position
                  :initarg :put-position
                 :initform 0
                 :type integer)))

(defmethod generate-return-function ((object option-chain-row))
  (let* ((call (option-row-call object))
         (put (option-row-put object))
         (call-function (generate-return-function call))
         (put-function (generate-return-function put))
         (calls (call-position object))
         (call-premium (if (< 0 calls)
                           (* (- calls) (option-bid call))
                           (* (- calls) (option-ask call))))
         (puts (put-position object))
         (put-premium (if (< 0 puts)
                           (* (- puts) (option-bid put))
                           (* (- puts) (option-ask put)))))
  (lambda (price)
    (+ call-premium
       put-premium
       (* calls (funcall call-function price)) ;; funcall happens if position is 0
       (* puts (funcall put-function price))))))

(defclass option-chain-row-mutator-functions ()
  ((set-call-position :reader call-setter
                      :initarg :call
                      :type function)
   (set-put-position :reader put-setter
                     :initarg :put
                     :type function)))

(defun make-option-chain-row-mutator-functions (option-chain-row)
  ;; (assert (and (option-bid (option-row-call option-chain-row))
  ;;              (option-ask (option-row-call option-chain-row))
  ;;              (option-bid (option-row-put option-chain-row))
  ;;              (option-ask (option-row-put option-chain-row))))
  (make-instance 'option-chain-row-mutator-functions
                 :call (lambda (number) (setf (call-position option-chain-row) number))
                 :put (lambda (number) (setf (put-position option-chain-row) number))))

(defclass option-contract ()
  ((conid :reader option-conid
          :initarg :conid
          :type string) ; string of number or number?
   (strike :reader strike-price
           :initarg :strike
           :type number)
   (right :reader option-right
          :initarg :right
          :type string)
   (bid :reader option-bid
        :initarg :bid
        :type number)
   (ask :reader option-ask
        :initarg :ask
        :type number)))

(defmethod generate-return-function ((object option-contract))
  ;; excercise value
  (if (string-equal (option-right object) "C")
      (lambda (price) (max 0 (- price (strike-price object))))
      (lambda (price) (max 0 (- (strike-price object) price)))))

;; (setq *test-row*
;;   (make-instance
;;    'option-chain-row
;;    :strike 40.0
;;    :call (make-instance 'option-contract
;;                         :conid "1234"
;;                         :strike 40.0
;;                         :right "C"
;;                         :bid 9.60
;;                         :ask 10.50)
;;    :call-position -1
;;    :put (make-instance 'option-contract
;;                        :conid "1235"
;;                        :strike 40.0
;;                        :right "P"
;;                        :bid 0.25
;;                        :ask 0.35)
;;    :put-position 1))
