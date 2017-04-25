


(defun tak(x y z)
  (if (< y x)
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))
      z))


(loop for x from 1 to 12 do
     (loop for y from 1 to 8 do
	  (loop for z from 1 to 8 do
	       (format t "TAK ~a ~a ~a = ~a~%" x y z (tak x y z)))))













