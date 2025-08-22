(defun option-strike-to-row (option-strike)
  (let* ((strike-price (option-strike-price option-strike))
         (call (option-strike-call option-strike))
         (put (option-strike-put option-strike)))
  (format
   nil
   "<tr> <td>~a</td> <td>~a</td> <td><center>~a</center></td> <td>~a</td> <td>~a</td> </tr>~%"
   (option-bid call) (option-ask call) strike-price (option-bid put) (option-ask put))))

(defun option-chain-to-table (option-chain)
  (let ((header
          (format nil "<caption>~a: ~a<\caption>~%<tr> <th>Call bid</th> <th>Call ask</th> <th>Strike price</th> <th>Put bid</th> <th>Put ask</th> </tr>~%" (option-chain-conid option-chain) (option-chain-month option-chain))))
    (format nil "<table>~%~a~{~a~%~}</table>" header (mapcar #'option-strike-to-row (option-strike-table option-chain)))))

(setq *page-header*
"<!DOCTYPE html>
<html>
<head>
<style>
table, th, td {
  border: 1px solid black;
}
</style>
</head>
<body>
")

(defvar *page-footer* "</body></html>")

(defun generate-page ()
  (with-open-file (page "html/page.html"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format page "~a~a~a" *page-header* (option-chain-to-table *example-strikes*) *page-footer*)))

(generate-page)

(print (option-strike-price (car *example-strikes*)))
(print (cdr (option-strike-call (car *example-strikes*))))
(print (option-chain-to-table *example-strikes*))

(print (option-bid (option-strike-call (car (option-strike-table *example-strikes*)))))
(print (option-ask (option-strike-call (car (option-strike-table *example-strikes*)))))
(print (option-ask (option-strike-call (cadr (option-strike-table *example-strikes*)))))
