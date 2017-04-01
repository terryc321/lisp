

******************************************************************************

map is a multi -argument routine

apply seems to work now.

(apply map list '((1 2 3)(10 20 30)))
 => ((1) (2) (3))
 because MAP is loaded from util/map.scm , only takes 1 argument






******************************************************************************

calling length on circular lists -- leads to infiinite 

******************************************************************************

ev-operands
 builds up arguments in wrong order

user-apply
primitive-apply
 both then have to reverse the arguments built by ev-operands

EVAL-APPLY
   PROC =
   ARGL =

(apply fac '(10))  ;; works ok

on next application to
(+ 1 2)

;; stack looks like it got environment stuck on it
;; clearly mismatch on 
;; 



******************************************************************************

fixed macro expander DEFINE bug , didnt expand ... in this example
 (define f ...)

 (define f (letrec ((x 1)) x))
 f -> 1
 

******************************************************************************

fixed repl environment bug

required environment be mutated in place , at exact memory location initial
environment is pointing at.



******************************************************************************

in trying to run compile.scm in the repl.scm interpreter , run into really nasty bug
. seems proc register is assigned 

split cps repl into another file cps-repl.scm

at the moment it really does not contain much useful stuff as it is not wired together.
want to investigate the cps interpreter further as it has more useful properties.

narrowed it down to environment problem that is not yet fixed.

(define f 3)
f -> 3
(define (g f) 2)
(g 10)
f -> 10

environment in place is still the one that got constructed when called the procedure.



******************************************************************************

worked out a way to translate CPS to register machine architecture

trying to see if we can get microcode included into the system.

think the link between CPS evaluator and Register Machine code should be examined more
closely.
in particular want to see callcc in the register machine , along with trace and inspect.


******************************************************************************

didnt read SICP too well , because not saving registers at all in the right places
return to SICP ways of doing things

primitives use ARGL register , not EXP now .

ev-if	        :       conditionals
ev-begin	:	sequencing
ev-application  :	application
ev-define	:	define
ev-assignment	:	set!

******************************************************************************

to do primitives
 >
 <
 >=
 <=

******************************************************************************

 * the number primitives *
+	: add
*	: multiply
-	:
/	:
=	: num =

  * the list primitives *
cons	: 
car	: 
cdr	: 
reverse : 
list	:
length  :

eq?	:
eqv?	:

 * type predicates *
procedure?	:  
pair?		:
boolean?	:
symbol?		:
number?		:



*****************************************************************************

** some of this section is clearly not useful now , as got right evaluator from SICP **

half way deciding on e.g eval-cons
eval-cons
CONS : exp = (x y)
where x y are as yet un-evaluated

here is observation -- this observation is quite important
-- highlights distinction between syntactic and functions

these are functions , meaning they evaluate all their arguments
CONS CAR CDR LIST PAIR? REVERSE PROCEDURE? BOOLEAN?


these are macros , Not functions so do not exist at runtime
OR AND WHEN

e.g in scheme we might want to write this ...
(map or '(#t #t #t) '(#f #f #f))
(map and '(#t #t #t) '(#f #f #f))
but OR is a MACRO , so does not exist as a function , hence cannot be mapped like a function.

the work-around might be something like this ...
(map (lambda (x y)(or x y)) '(#t #t #t) '(#f #f #f))
(map (lambda (x y)(and x y)) '(#t #t #t) '(#f #f #f))

********************************************************************************

by the time SICP evaluator gets involved there are NO MACROS whatever ,
all completely function procedure-y code to evaluate

between reader and base-eval , macro-expand is called
this also handles quasiquote splicing and nesting , but thats another thing.

***************************************************************************
base-eval

SICP explicit control evaluator has a stack and 7 registers

1 EXP  : expression register  - a memory address
2 ENV  : environment register - naturally an environment
3 CONT : continuation register - a memory address , where go to next
4 VAL  : value register - where THE value of result lives
5 PROC : procedure register .
6 ARGL : argument list - 
7 UNEV : unevaluated arguments 

number = number
boolean = boolean
string = string
vector = vector
procedure = procedure
symbol = lookup symbol in environment
*****************************************************************

pretty-printing circular list is a real pain.

*****************************************************************

(trace fib)	;; trace fibonacci routine
(fib 10)

(trace)		;; untraces everything

*****************************************************************
(begin)
(begin 1)
(begin 1 2 3)

value of begin is last evaluated expression , or #f otherwise


*****************************************************************
did a rewrite on cps-function +
other routines still using fexpr like approach

weird interaction with apply and map , its really weird
... apply fixed now !
... split apply into operator operands
slurps up operands , expects last element to be a list
*****************************************************************


base-apply does not now call base-eval

base-eval evaluates expression
if base-apply calls base-eval again , then it will throw evaluation out of whack.

*****************************************************************

how can i trace compiled routine ??

what if we want to extend + operator to handle rectangles ??


*****************************************************************
*****************************************************************
          (load "repl.scm")

file paths are absolute

*****************************************************************
*****************************************************************

core/quasiquote.scm		bawden system

core/macro-expander.scm		defmacro like macro expander
				fine as long as not trying to redefine primitives


macros/				directory contains common macros
macros/and
macros/or


util/				directory contains common utility functions 
util/not			negation 
util/list			list
util/append
util/map			map



********************************************************************

1 : printing circular linked lists

2 : unique datatypes that are seperate to primitives

3 : hygienic macros

4 : 


***** Outline ********
CPS evaluator

what if error occurs ?
example 
  (car 5)
this will crash ordinary procedure , since 5 is not a pair.
cause us to end up in the debugger.

eval-car
(car X) , expects X is a pair .

 
(procedure? X) : is X a procedure ? 

(eof-object? X) : is X an end of file object ?




********************************************************************



fexprs
-----------------------
fexprs do not evaluate their arguments on entry to the body of fexpr

(define myquote (fexpr (n) n))

(myquote non-existent-symbol)
> non-existent-symbol

fexprs are like a runtime macro
fexprs are unreliable so far... something do with wrong environment ...


types and car cdr
-------------------
if make new type say APOLLO type
i dont want to be able to go in and alter 


types
----------
ability to construct new types that are distinct from all other types is
very important.
this is achieved using a simple mechanism.

every object in the system has a header - currently this is just a CONS
every object is assigned a type by attaching a type-unique label
the main idea is that the LABEL tells the system , this THING is of this TYPE
and not any other type.
a value has a TYPE , and that TYPE is unique.
a variable can be bound to a value , and then it is said to have type TYPE.

(environment? (current-environment))



environment
-----------
environment is represented as a flat list
for example
env = (a 1 b 2 c 3 d 4)

search environment
otherwise look at next binding (cdr (cdr env))
to move to next binding we need to take two steps.

the advantage of the flat environment is that set-car! can easily change a binding.

printing the environment is problematic due to circular lists , printing incurs infinite loops.


define
------
define allows us to define bindings of unlimited extent.

(define f 5)
(define (f x) (+ x x))

if environment does not yet contain a binding , then the environment needs to be altered.

case 1 : toplevel define
here we want to 

case 2 : nested define -- this is really letrec , as common nested defines want to see
each other in scope.
here we do not want the nested define to pollute outer environment when nested scope is exited.
this is important.

extended destructively but only at the head of the environment.

this allows for nested defines to act like a letrec
nested defines act like set! instead of letrecs , this is not correct.

f1: (define f 1)
f2: (define f 2)
does f1 clobber f2 define ?

parameter lists
---------------
flat parameter list
 > (define (f x y z) (list 'x x 'y y 'z z))
 > (f 1 2 3)
 ==>  (x 1 y 2 z 3)

dotted pair parameter list
::> (define (f x y . z) (list 'x x 'y y 'z z))
==> <lambda>
::> (f 1 2 3 4 5)
 ==> (x 1 y 2 z (3 4 5))

symbol parameter list
   : (define f (lambda args (list 'args args)))
   : (f 1 2 3 4 5)
   > (args 1 2 3 4 5)

lambda
-------
save typing by calling it fn instead
     (define twice (fn (x) (+ x x)))

if we let code is data , i see twice function as (lambda (x) (+ x x))
but then i should be able to just put it together and get the same result      
   (list 'lambda '(x) '(+ x x))
in this sense , there is nothing magical about lambda

 (lambda? '(lambda (x) (+ x x)))
> #t

if we allow this it becomes hard to detect when using programming in the large.
so build mechanism on top of this.

dynamic variables
-----------------
dynamic variables denoted by ears *..var..*
dynamic variables are looked up in the dynamic environment rather than in the environment ?

letrecs are macros let and set!
let* are macros let
let can be just lambda , but less efficient like that

trying to see how can get flexible system

the sicp way of describing execution is nice

figured out callcc eventually.

going to bed.



