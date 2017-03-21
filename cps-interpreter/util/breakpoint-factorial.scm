


(define factorial (lambda (n)
                    (if (< n 2) 1
                        (begin
                          (breakpoint n)
                          (* n (factorial (- n 1)))))))

(display "breakpoint-factorial defined")
(newline)
