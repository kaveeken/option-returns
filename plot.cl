(ql:quickload :eazy-gnuplot)
(use-package :eazy-gnuplot)

(defun generate-histogram-plot-function (lst bin-count)
  (let ((bins (bin-list lst bin-count)))
    (lambda ()
      (plot
       (lambda ()
         (loop for bin in bins
               do (format t "~&~a ~a 0" (bin-lower bin) (bin-size bin))))
       :using '(1 |($2)| 3) :with :boxerrorbars)))) ; idk why this is and the 0 above are required

(defun generate-hline-function (xmin xmax y)
  (lambda ()
    (plot (lambda () (loop for x in (list xmin xmax) do (format t "~a ~a~%" x y)))
        :with :lines :axes :x1y2 :lw 2 :lc "black" :title "")))

(defun generate-vline-function (x ymin ymax)
  (lambda ()
    (plot (lambda () (loop for y in (list ymin ymax) do (format t "~a ~a~%" x y)))
        :with :lines :axes :x1y2 :lw 2 :lc "black" :title "")))


(defun plot-histogram-and-lines (output plot-functions)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :terminal '(pngcairo) :output output
              :style '(fill pattern 2 border lt -1))
    (gp :set :y2tics)
    (gp :set :ylabel "Count")
    (gp :set :y2label "Return")
    (gp :set :y2range '(-20 40))
    (loop for fn in plot-functions do (funcall fn))))
  

;; (plot-histogram-overlaid-line "plot.png"
;;                               (generate-histogram-plot-function-from-closes
;;                                (get-closes (get-history (find-conid "GIS") "12y" "1w"))
;;                                50 3 0 10000 500)
;;                               (lambda () nil))
;; 
;; (let* ((weeks-to-expiry 12) ; tau
;;        (closes (get-closes (get-history (find-conid "GIS") "12y" "1w")))
;;        (diffs (generate-bootstrap closes weeks-to-expiry 1000))
;;        (scaled-diffs (mapcar (lambda (diff) (* (car (last closes)) diff)) diffs)))
;;   (plot-histogram-and-lines
;;    "plot.png"
;;    (list
;;     (generate-histogram-plot-function scaled-diffs 50))))
;; 
;; (let* ((weeks-to-expiry 12) ; steps
;;        (bin-count 50)
;;        (closes (get-closes (get-history (find-conid "GIS") "12y" "1w")))
;;        (diffs (geometric-diffs closes weeks-to-expiry))
;;        (scaled-diffs (mapcar (lambda (diff) (* (car (last closes)) diff)) diffs))
;;        (xmin (reduce #'min scaled-diffs))
;;        (xmax (reduce #'max scaled-diffs))
;;        (dx (/ (- xmax xmin) bin-count))
;;        (returns (loop for x from xmin to xmax by dx
;;                       collect (format nil "~a ~a~%" x (funcall (generate-return-function *example-chain*) x)))))
;;   (plot-histogram-and-lines
;;    "plot.png"
;;    (list
;;     (generate-histogram-plot-function scaled-diffs 50)
;;     (generate-hline-function xmin xmax 0)
;;     (generate-vline-function 50 -10 10)
;;     (lambda ()
;;       (plot (lambda () (loop for return in returns do (format t "~a" return)))
;;         :with :lines :axes :x1y2 :lw 4 :title "Return")))))
;; 
;; (setq *example-option-position*
;;   (make-instance 'option-position
;;     :bought-contracts 
;;       (list (make-instance 'call :price 0.25 :date '(2025 11 15) :strike-price 65.0)
;;             (make-instance 'put :price 0.55 :date '(2025 11 15) :strike-price 42.5))
;;     :written-contracts 
;;       (list (make-instance 'call :price 1.45 :date '(2025 11 15) :strike-price 52.5))
;;             ;(make-instance 'put :price 3.75 :date '(2025 11 15) :strike-price 80.0))
;;     :underlying-lots 0
;;     :underlying-cost 50.0))
