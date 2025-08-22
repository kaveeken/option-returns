
(defun geometric-diffs (closes &optional (period 1))
  (mapcar (lambda (first second) (/ second first)) (butlast closes period) (last closes (- (length closes) period))))

(defun geometric-walk (diffs count)
  (let* ((len (length diffs))
         (indices (loop for i from 0 to count collect (random len))) ;; wasteful, look at streams
         (samples (mapcar #'(lambda (n) (nth n diffs)) indices)))
  (reduce #'* samples)))

(defun bootstrap (diffs tau n)
  (loop for i from 0 to n collect (geometric-walk diffs tau)))
  
(defun generate-bootstrap (closes tau n)
  (bootstrap (geometric-diffs closes) tau n))
