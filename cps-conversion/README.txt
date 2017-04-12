
cps-minimal.scm
code from matt might website rewritten for scheme , without match and
quasiquote

cps.scm

think that its entirely possible to write continuation passing style by hand ,
as in passing multiple values forward to next computation is a great example
of a useful application of this technique.
also cps evaluator also lets me understand breakpoints , inspection and tracing

TRACER in cps

(define (cps-trace exp env cont)
    (base-eval exp env (lambda (result)
			 (display "TRACE : ")
			 (display exp)
			 (display " => ")
			 (display result)
			 (newline)
			 (cont result))))

where base-eval is a continuation passing evaluator




