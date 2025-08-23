(ql:quickload :drakma)
(ql:quickload :flexi-streams)
(ql:quickload :st-json)

(load "classes.cl")

(defun read-body (body)
  (st-json:read-json
   (flexi-streams:octets-to-string body)))

(defun get-request (endpoint)
  (let ((base-url "https://localhost:5000/v1/api/iserver/"))
    (read-body (drakma:http-request (concatenate 'string base-url endpoint)))))

(defun find-conid (symbol)
  (let ((contracts (get-request (format nil "secdef/search?symbol=~a&name=false&secType=STK" symbol))))
    (st-json:getjso "conid" (car contracts))))
    ;; (loop for contract in contracts 
    ;;       if (and (string-equal (st-json:getjso "symbol" contract) symbol)
    ;;               (search *exchange* (st-json:getjso "companyHeader" contract)))
    ;;         return (st-json:getjso "conid" contract))))

(defun get-history (conid period bar)
  (get-request
   (format nil "marketdata/history?conid=~a&exchange=SMART&period=~a&bar=~a" conid period bar)))

(defun get-closes (history)
  (let ((data (st-json:getjso "data" history)))
    (mapcar (lambda (ohlc) (st-json:getjso "c" ohlc)) data)))

(defun get-contract-by-symbol (symbol)
  (car (get-request (format nil "secdef/search?symbol=~a" symbol))))

(defun get-stock (symbol)
  (let* ((contract (get-contract-by-symbol symbol))
         (conid (st-json:getjso "conid" contract))
         (last (st-json:getjso "31" (get-market-snapshot conid))))
    (make-instance
     'stock :conid conid :price last
            :months (loop for section in (st-json:getjso "sections" contract)
                          if (string-equal (st-json:getjso "secType" section) "OPT")
                            return (st-json:getjso "months" section)))))

(defun get-option-chain-strikes (conid month)
  (let* ((all (get-request (format nil "secdef/strikes?conid=~a&sectype=OPT&month=~a" conid month)))
         (call-strike-prices (st-json:getjso "call" all))
         (put-strike-prices (st-json:getjso "put" all)))
    (cons call-strike-prices put-strike-prices)))

(defun get-option-conid (underlying-conid month strike right)
  (let* ((contract-info (car (get-request
                 (format nil "secdef/info?conid=~a&secType=OPT&month=~a&strike=~a&right=~a"
                         underlying-conid month strike right))))
         (contract-conid (st-json:getjso "conid" contract-info)))
    contract-conid))

(defun get-option (underlying-conid month strike right)
  (let* ((option-conid (get-option-conid underlying-conid month strike right))
         (snapshot (car (get-request (format nil "marketdata/snapshot?conids=~a&fields=84,86" option-conid))))
         (bid (st-json:getjso "84" snapshot))
         (ask (st-json:getjso "86" snapshot)))
    (make-instance 'option-contract :conid option-conid :strike strike :right right :bid bid :ask ask)))

(defun get-option-chain-row (underlying-conid month strike call-strikes put-strikes)
  (let* ((call (if (member strike call-strikes)
                   (get-option underlying-conid month strike "C")
                   nil))
         (put (if (member strike put-strikes)
                   (get-option underlying-conid month strike "P")
                   nil)))
    (make-instance 'option-chain-row :strike strike :call call :put put)))

(defun get-option-chain (stock month)
  (let* ((strike-prices (get-option-chain-strikes (conid stock) month))
         (call-strikes (car strike-prices))
         (put-strikes (cdr strike-prices))
         (rows (loop for strike in (union call-strikes put-strikes)
                     collect (get-option-chain-row
                              (conid stock) month strike call-strikes put-strikes))))
    (make-instance 'option-chain :underlying stock :month month :rows rows)))

(defun option-chain-instantiate-get-months (symbol)
  (let* ((contracts (get-request (format nil "secdef/search?symbol=~a" symbol)))
         (head (car contracts))
         (conid (st-json:getjso "conid" head))
         (sections (st-json:getjso "sections"  head))
         (months (loop for section in sections
                       if (string-equal (st-json:getjso "secType" section) "OPT")
                         return (st-json:getjso "months" section))))
    (list conid months)))

(print (option-chain-instantiate-get-months "GIS"))


(print (option-chain-find-strikes "7616" "NOV25"))

(defun option-chain-get-contract-conid (underlying-conid month strike right)
  (let* ((contract-info (car (get-request
                 (format nil "secdef/info?conid=~a&secType=OPT&month=~a&strike=~a&right=~a"
                         underlying-conid month strike right))))
         (contract-conid (st-json:getjso "conid" contract-info)))
    contract-conid))

(print (option-chain-get-contract-conid "7616" "NOV25" "40.0" "C"))

(defun get-market-snapshot (conid)
  (let* ((snapshot (get-request (format nil "marketdata/snapshot?conids=~a&fields=31,7308,76,84,88,85,86" conid))))
    snapshot))

(print (get-market-snapshot "806058060"))

(defclass option-chain ()
  ((underlying-conid :reader option-chain-conid
                     :initarg :conid
                     :type string)
   (month :reader option-chain-month
          :initarg :month
          :type string)
   (strike-table :reader option-strike-table
                 :initarg :strikes
                 :type option-strike)))

(defclass option-contract ()
  ((conid :reader option-conid
          :initarg :conid
          :type string) ; string of number or number?
   (right :reader option-right
          :initarg :right
          :type string)
   (bid :reader option-bid
        :initarg :bid
        :type number)
   (ask :reader option-ask
        :initarg :ask
        :type number)))

(defclass option-strike ()
  ((strike-price :reader option-strike-price
                 :initarg :price
                 :type number)
   (call :reader option-strike-call
         :initarg :call
         :type option-contract)
   (put :reader option-strike-put
        :initarg :put
        :type option-contract)))

(defun generate-option-strike (conid month strike-price right)
  (let* ((option-conid (option-chain-get-contract-conid conid month strike-price right))
          ;; fields: 84-bid price 86-ask price
          (snapshot (car (get-request (format nil "marketdata/snapshot?conids=~a&fields=84,86" option-conid))))
          (bid (st-json:getjso "84" snapshot))
          (ask (st-json:getjso "86" snapshot)))
     (make-instance 'option-contract
                    :conid option-conid
                    :right "C"
                    :bid bid
                    :ask ask)))

(defun build-option-chain (conid month)
  (let* ((strike-prices (option-chain-find-strikes conid month))
         (call-strike-prices (st-json:getjso "call" strike-prices))
         (put-strike-prices (st-json:getjso "put" strike-prices))
         (calls (mapcar (lambda (strike-price)
                          (cons strike-price (generate-option-strike conid month strike-price "C")))
                        call-strike-prices))
         (puts (mapcar (lambda (strike-price)
                          (cons strike-price (generate-option-strike conid month strike-price "P")))
                       put-strike-prices))
         (merged-strikes (union call-strike-prices put-strike-prices)))
    (make-instance
     'option-chain
     :conid conid
     :month month
     :strikes
     (loop for strike in merged-strikes
           collect (make-instance 'option-strike :price strike :call (cdr (assoc strike calls)) :put (cdr (assoc strike puts)))))))

(print (option-bid (generate-option-strike "7616" "NOV25" "50.0" "P")))

(defvar *example-strikes* (build-option-chain "7616" "NOV25"))
(setq *example-strikes* (build-option-chain "7616" "NOV25"))
(print (build-option-chain "7616" "NOV25"))
(print *example-strikes*)
(print (option-strike-price (car *example-strikes*)))
