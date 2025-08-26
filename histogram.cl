(ql:quickload :eazy-gnuplot)
(use-package :eazy-gnuplot)

(defclass bin ()
  ((lower-bound :accessor bin-lower
                :initarg :lower
                :type long-float)
   (upper-bound :accessor bin-upper
                :initarg :upper
                :type long-float)
   (size :accessor bin-size
         :initarg :size
         :type integer)))

(defun bin-list (lst bin-count)
  (let* ((minimum (reduce #'min lst))
         (maximum (reduce #'max lst))
         (width (- maximum minimum))
         (bin-width (/ width bin-count))
         (sorted-lst (sort lst #'<)))
    (loop for i below bin-count
          for lower-bound = (+ minimum (* i bin-width))
          for upper-bound = (if (= i (1- bin-count))
                              (+ maximum 1) ; ensure maximum is included
                              (+ minimum (* (1+ i) bin-width)))
          collect (make-instance 'bin
                                 :lower lower-bound
                                 :upper upper-bound
                                 :size (count-if (lambda (x) 
                                                   (and (>= x lower-bound) 
                                                        (< x upper-bound)))
                                                 sorted-lst)))))

(defun geometric-diffs (closes &optional (period 1))
  (mapcar (lambda (first second) (/ second first)) (butlast closes period) (last closes (- (length closes) period))))

(defun geometric-walk (diffs steps)
  (let* ((len (length diffs))
         (indices (loop for i from 0 to steps collect (random len))) ;; wasteful but no real problem. maybe look at streams
         (samples (mapcar #'(lambda (n) (nth n diffs)) indices)))
  (reduce #'* samples)))

(defun bootstrap (diffs steps n)
  (loop for i from 0 to n collect (geometric-walk diffs steps)))
  
(defun generate-bootstrap (closes steps n)
  (bootstrap (geometric-diffs closes) steps n))

(defun make-histogram-data-from-bootstrap (closes bin-count steps n) ;; scale?
  (bin-list (generate-bootstrap closes steps n) bin-count))

(defun make-histogram-data-directly (closes bin-count &optional (period 1))
  (bin-list (geometric-diffs closes period) bin-count))

(defun generate-histogram-plot-function-from-closes
    (closes bin-count &optional (period 1) (bootstrap-tau nil) (bootstrap-n nil) (price 1))
  (let* ((diffs (mapcar (lambda (diff) (* diff price))
                        (if (and bootstrap-tau bootstrap-n)
                            (generate-bootstrap closes bootstrap-tau bootstrap-n)
                            (geometric-diffs closes period))))
         (bins (bin-list diffs bin-count)))
    (lambda ()
      (plot
       (lambda ()
         (loop for bin in bins
               do (format t "~&~a ~a 0" (bin-lower bin) (bin-size bin))))
       :using '(1 |($2)| 3) :with :boxerrorbars)))) ; idk why this is and the 0 above are required

(defun generate-histogram-plot-function (diffs bin-count)
  (let ((bins (bin-list diffs bin-count)))
    (lambda ()
    (plot
     (lambda ()
       (loop for bin in bins
             do (format t "~&~a ~a 0" (bin-lower bin) (bin-size bin))))
     :using '(1 |($2)| 3) :with :boxerrorbars))))) ; idk why this is and the 0 above are required


;; old: ignore
;; (defun plot-histogram (lst bin-count output)
;;   (with-plots (s :debug nil)
;;     (gp-setup :terminal '(pngcairo) :output output
;;               :style '(fill pattern 2 border lt -1))
;;     (let ((bins (bin-list lst bin-count)))
;;       (plot
;;        (lambda ()
;;          (loop for bin in bins
;;                do (format s "~&~a ~a 0" (bin-lower bin) (bin-size bin))))
;;        :using '(1 |($2)| 3) :with :boxerrorbars))) ; idk why this is and the 0 above are required
;;     output)
;; 
;; (plot-histogram (geometric-diffs (get-closes (get-history (find-conid "AAPL") "12y" "1m")) 3) 50 "images/histogram-stacked-plot.png")
;; (plot-histogram (geometric-diffs (get-closes (get-history (find-conid "GIS") "12y" "1m")) 3) 50 "images/histogram-stacked-plot.png")
;; (plot-histogram (mapcar (lambda (diff) (* diff 50.0))
;;                         (geometric-diffs
;;                          (get-closes (get-history (find-conid "GIS") "12y" "1m")) 3))
;;                 20 "images/histogram-stacked-plot.png")
;; (plot-histogram '(1.1 2.0 5.5 2.89 1.4 3.2 4 9 2) 5 "images/test.png")
;; 
;; (print (geometric-walk '(1.1 2.0 5.5 2.89 1.4 3.2 4 9 2) 5))
;; (print (bootstrap '(1.1 2.0 5.5 2.89 1.4 3.2 4 9 2) 3 10))
;; (plot-histogram (bootstrap '(1.1 2.0 5.5 2.89 1.4 3.2 4 9 2) 3 100) 50 "images/histogram-stacked-plot.png")
;; (plot-histogram (bootstrap (geometric-diffs (get-closes (get-history (find-conid "GIS") "12y" "1m"))) 12 10000) 50 "images/histogram-stacked-plot.png")
;; 
;; 
;; (plot-histogram
;;  (generate-bootstrap
;;   (get-closes (get-history (find-conid "GIS") "12y" "1m"))
;;   1 10000)
;;  50 "images/histogram-stacked-plot.png")
