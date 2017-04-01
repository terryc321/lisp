

;; pretty-print 

(pp '((env) (val) ((assign val (op make-compiled-procedure) (label #[uninterned-symbol 11 entry127]) (reg env)) (goto (label #[uninterned-symbol 12 after-lambda128])) #[uninterned-symbol 11 entry127] (assign env (op compiled-procedure-env) (reg proc)) (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) (save continue) (save env) (assign proc (op lookup-variable-value) (const =) (reg env)) (assign val (const 1)) (assign argl (op list) (reg val)) (assign val (op lookup-variable-value) (const n) (reg env)) (assign argl (op cons) (reg val) (reg argl)) (test (op primitive-procedure?) (reg proc)) (branch (label #[uninterned-symbol 15 primitive-branch132])) #[uninterned-symbol 13 compiled-branch133] (assign continue (label #[uninterned-symbol 14 after-call134])) (assign val (op compiled-procedure-entry) (reg proc)) (goto (reg val)) #[uninterned-symbol 15 primitive-branch132] (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) #[uninterned-symbol 14 after-call134] (restore env) (restore continue) (test (op false?) (reg val)) (branch (label #[uninterned-symbol 26 false-branch130])) #[uninterned-symbol 25 true-branch129] (assign val (const 1)) (goto (reg continue)) #[uninterned-symbol 26 false-branch130] (assign proc (op lookup-variable-value) (const *) (reg env)) (save continue) (save proc) (assign val (op lookup-variable-value) (const n) (reg env)) (assign argl (op list) (reg val)) (save argl) (assign proc (op lookup-variable-value) (const factorial) (reg env)) (save proc) (assign proc (op lookup-variable-value) (const -) (reg env)) (assign val (const 1)) (assign argl (op list) (reg val)) (assign val (op lookup-variable-value) (const n) (reg env)) (assign argl (op cons) (reg val) (reg argl)) (test (op primitive-procedure?) (reg proc)) (branch (label #[uninterned-symbol 18 primitive-branch135])) #[uninterned-symbol 16 compiled-branch136] (assign continue (label #[uninterned-symbol 17 after-call137])) (assign val (op compiled-procedure-entry) (reg proc)) (goto (reg val)) #[uninterned-symbol 18 primitive-branch135] (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) #[uninterned-symbol 17 after-call137] (assign argl (op list) (reg val)) (restore proc) (test (op primitive-procedure?) (reg proc)) (branch (label #[uninterned-symbol 21 primitive-branch138])) #[uninterned-symbol 19 compiled-branch139] (assign continue (label #[uninterned-symbol 20 after-call140])) (assign val (op compiled-procedure-entry) (reg proc)) (goto (reg val)) #[uninterned-symbol 21 primitive-branch138] (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) #[uninterned-symbol 20 after-call140] (restore argl) (assign argl (op cons) (reg val) (reg argl)) (restore proc) (restore continue) (test (op primitive-procedure?) (reg proc)) (branch (label #[uninterned-symbol 23 primitive-branch141])) #[uninterned-symbol 22 compiled-branch142] (assign val (op compiled-procedure-entry) (reg proc)) (goto (reg val)) #[uninterned-symbol 23 primitive-branch141] (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) (goto (reg continue)) #[uninterned-symbol 24 after-call143] #[uninterned-symbol 27 after-if131] #[uninterned-symbol 12 after-lambda128] (perform (op define-variable!) (const factorial) (reg val) (reg env)) (assign val (const ok)))))

((env)
 (val)
 ((assign val
	  (op make-compiled-procedure)
	  (label
	   #[uninterned-symbol 11 entry127])
	  (reg env))
  (goto
   (label
    #[uninterned-symbol 12 after-lambda128]))
  #[uninterned-symbol 11 entry127]
  (assign env
	  (op compiled-procedure-env)
	  (reg proc))
  (assign env
	  (op extend-environment)
	  (const
	   (n))
	  (reg argl)
	  (reg env))
  (save continue)
  (save env)
  (assign proc
	  (op lookup-variable-value)
	  (const =)
	  (reg env))
  (assign val
	  (const 1))
  (assign argl
	  (op list)
	  (reg val))
  (assign val
	  (op lookup-variable-value)
	  (const n)
	  (reg env))
  (assign argl
	  (op cons)
	  (reg val)
	  (reg argl))
  (test
   (op primitive-procedure\?)
   (reg proc))
  (branch
   (label
    #[uninterned-symbol 15 primitive-branch132]))
  #[uninterned-symbol 13 compiled-branch133]
  (assign continue
	  (label
	   #[uninterned-symbol 14 after-call134]))
  (assign val
	  (op compiled-procedure-entry)
	  (reg proc))
  (goto
   (reg val))
  #[uninterned-symbol 15 primitive-branch132]
  (assign val
	  (op apply-primitive-procedure)
	  (reg proc)
	  (reg argl))
  #[uninterned-symbol 14 after-call134]
  (restore env)
  (restore continue)
  (test
   (op false\?)
   (reg val))
  (branch
   (label
    #[uninterned-symbol 26 false-branch130]))
  #[uninterned-symbol 25 true-branch129]
  (assign val
	  (const 1))
  (goto
   (reg continue))
  #[uninterned-symbol 26 false-branch130]
  (assign proc
	  (op lookup-variable-value)
	  (const *)
	  (reg env))
  (save continue)
  (save proc)
  (assign val
	  (op lookup-variable-value)
	  (const n)
	  (reg env))
  (assign argl
	  (op list)
	  (reg val))
  (save argl)
  (assign proc
	  (op lookup-variable-value)
	  (const factorial)
	  (reg env))
  (save proc)
  (assign proc
	  (op lookup-variable-value)
	  (const -)
	  (reg env))
  (assign val
	  (const 1))
  (assign argl
	  (op list)
	  (reg val))
  (assign val
	  (op lookup-variable-value)
	  (const n)
	  (reg env))
  (assign argl
	  (op cons)
	  (reg val)
	  (reg argl))
  (test
   (op primitive-procedure\?)
   (reg proc))
  (branch
   (label
    #[uninterned-symbol 18 primitive-branch135]))
  #[uninterned-symbol 16 compiled-branch136]
  (assign continue
	  (label
	   #[uninterned-symbol 17 after-call137]))
  (assign val
	  (op compiled-procedure-entry)
	  (reg proc))
  (goto
   (reg val))
  #[uninterned-symbol 18 primitive-branch135]
  (assign val
	  (op apply-primitive-procedure)
	  (reg proc)
	  (reg argl))
  #[uninterned-symbol 17 after-call137]
  (assign argl
	  (op list)
	  (reg val))
  (restore proc)
  (test
   (op primitive-procedure\?)
   (reg proc))
  (branch
   (label
    #[uninterned-symbol 21 primitive-branch138]))
  #[uninterned-symbol 19 compiled-branch139]
  (assign continue
	  (label
	   #[uninterned-symbol 20 after-call140]))
  (assign val
	  (op compiled-procedure-entry)
	  (reg proc))
  (goto
   (reg val))
  #[uninterned-symbol 21 primitive-branch138]
  (assign val
	  (op apply-primitive-procedure)
	  (reg proc)
	  (reg argl))
  #[uninterned-symbol 20 after-call140]
  (restore argl)
  (assign argl
	  (op cons)
	  (reg val)
	  (reg argl))
  (restore proc)
  (restore continue)
  (test
   (op primitive-procedure\?)
   (reg proc))
  (branch
   (label
    #[uninterned-symbol 23 primitive-branch141]))
  #[uninterned-symbol 22 compiled-branch142]
  (assign val
	  (op compiled-procedure-entry)
	  (reg proc))
  (goto
   (reg val))
  #[uninterned-symbol 23 primitive-branch141]
  (assign val
	  (op apply-primitive-procedure)
	  (reg proc)
	  (reg argl))
  (goto
   (reg continue))
  #[uninterned-symbol 24 after-call143]
  #[uninterned-symbol 27 after-if131]
  #[uninterned-symbol 12 after-lambda128]
  (perform
   (op define-variable!)
   (const factorial)
   (reg val)
   (reg env))
  (assign val
	  (const ok))))
