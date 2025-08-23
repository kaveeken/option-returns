(load "classes.cl")
(load "ibkr-api.cl")
(load "bins.cl")
(load "plot.cl")
(load "returns.cl")
(load "site-generator.cl")

(defparameter *example-stock* (get-stock "GIS"))
(defparameter *example-chain* (get-option-chain *example-stock* "NOV25"))

(get-stock "GIS")

(print (strike-price (option-row-call (car (last (option-chain-rows *example-chain*))))))
(funcall (generate-return-function *example-chain*) 20)
(stock-price (option-chain-underlying *example-chain*))

(defparameter *example-mutators* (make-option-chain-row-mutator-functions (car (last (option-chain-rows *example-chain*)))))
(put-position (car (last (option-chain-rows *example-chain*))))

(funcall (call-setter *example-mutators*) -1)
(funcall (put-setter *example-mutators*) 0)

