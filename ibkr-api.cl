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

(defun get-market-snapshot (conid)
  (let* ((snapshot (get-request (format nil "marketdata/snapshot?conids=~a&fields=31,7308,76,84,88,85,86" conid))))
    snapshot))

(defun get-history (conid period bar)
  (get-request
   (format nil "marketdata/history?conid=~a&exchange=SMART&period=~a&bar=~a" conid period bar)))

(defun get-closes (history)
  (let ((data (st-json:getjso "data" history)))
    (mapcar (lambda (ohlc) (st-json:getjso "c" ohlc)) data)))

(defun get-contract-by-symbol (symbol)
  (car (get-request (format nil "secdef/search?symbol=~a" symbol))))
(get-contract-by-symbol "GIS")
(get-stock "GIS")

(defun get-stock (symbol)
  (let* ((contract (get-contract-by-symbol symbol))
         (conid (st-json:getjso "conid" contract))
         (last (read-from-string (st-json:getjso "31" (car (get-market-snapshot conid))))))
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
    (make-instance 'option-contract :conid option-conid :strike strike :right right
                                    :bid (if bid (read-from-string bid) nil)
                                    :ask (if ask (read-from-string ask) nil))))

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

