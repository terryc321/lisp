#!/usr/bin/python3


import subprocess

global results
global fails
global passes

results = []
fails = []
passes = []

def test_section(title):
    print("TEST SECTION :")
    print(title)
    print("\n")
    

def test_case(expr,expected):
    global results
    global passes
    global fails
    
    # encode as bytes
    expected = bytes(expected,'utf-8')
    expected = expected.decode()
    #expected = expected.decode()

    subprocess.run(['rm', '-f' , "work.temp" ])
    
    f= open('work.temp' , 'w')
    f.write(expr)
    f.close()
    
    subprocess.run(['rm', '-f' , "driver" ])
    subprocess.run(['rm', '-f' , "entry.asm" ])
    subprocess.run(['rm', '-f' , "entry.o" ])    
    subprocess.run(['rm', '-f' , "c-help.o" ])    
    
    subprocess.run(['guile', '-c' , "(begin (load \"lisp.scm\") (stage-4 \"work.temp\" \"entry.asm\"))" ])
    subprocess.run(['make'])
    result = subprocess.run(['./driver'] , stdout=subprocess.PIPE)
    result = result.stdout
    result = result.decode()
    
    print("RESULT = ",)
    print(result)
    print("\n")
    print("EXPECTED = ",)
    print(expected)
    
    if result == expected :
         passes = passes + [["PASS",expr,expected,result]]
         print("PASS\n")        
    else:
         fails = fails + [["FAIL",expr,expected,result]]
         print("FAIL\n")

def test_reset():
    global results
    global fails
    fails = []
    results = []
    passes = []
    
    
        
# simple testing infrastructure
test_reset()


test_section("fixnums")
test_case("(+ 1 (+ 2 (+ 3 4)))","10")

test_section("addition by 1")
test_case("(add1 3)","4")
test_case("(add1 255)","256")

test_section("subtraction by 1")
test_case("(sub1 3)","2")
test_case("(sub1 0)","-1")

test_section("integer->char ch")
test_case("(integer->char 65)","#\\A")
test_case("(integer->char 97)","#\\a")

test_section("char->integer ")
test_case("(char->integer #\\A)","65")
test_case("(char->integer #\\a)","97")


test_case("(add1 (char->integer #\\a))","98")
test_case("(sub1 (char->integer #\\a))","96")


test_case("(integer->char (char->integer #\\a))","#\\a")
test_case("(integer->char (char->integer #\\b))","#\\b")
test_case("(integer->char (char->integer #\\c))","#\\c")

test_case("(char->integer (integer->char 97))","97")
test_case("(char->integer (integer->char 65))","65")


test_section(" booleans ")
test_case("#t","#t")
test_case("#f","#f")


test_section(" the empty list ")
test_case("()","()")

test_section(" zero? ")
test_case("(zero? 0)","#t")
test_case("(zero? 1)","#f")
test_case("(zero? -1)","#f")
test_case("(zero? #\\a)","#f")
test_case("(zero? ())","#f")

test_section(" null? ")
test_case("(null? ())","#t")
test_case("(null? 0)","#f")
test_case("(null? 1)","#f")
test_case("(null? -1)","#f")
test_case("(null? #\\a)","#f")



test_section(" boolean? ")
test_case("(boolean? #f)","#t")
test_case("(boolean? #t)","#t")
test_case("(boolean? 32)","#f")
test_case("(boolean? #\\a)","#f")
test_case("(boolean? #\\A)","#f")
test_case("(boolean? ())","#f")


test_section(" integer? ")
test_case("(integer? 32)","#t")
test_case("(integer? #f)","#f")
test_case("(integer? #t)","#f")
test_case("(integer? #\\a)","#f")
test_case("(integer? #\\A)","#f")
test_case("(integer? ())","#f")

test_section(" add ")
test_case("(+ 1 2)","3")
test_case("(+ (+ 1 2) (+ 3 4))","10")
test_case("(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 (+ 10 0))))))))))","55")
test_case("(+ 1 -1)","0")
test_case("(+ 10 -10)","0")

test_section(" not ")
test_case("(not #f)","#t")
test_case("(not #t)","#f")
test_case("(not 32)","#f")
test_case("(not #\\a)","#f")
test_case("(not #\\A)","#f")
test_case("(not ())","#f")


test_section(" IF ")
test_case("(if #t 1 2)" , "1")
test_case("(if #f 1 2)" , "2")
test_case("(if #t 1)" , "1")
test_case("(if #f 1)" , "#f")
test_case("(if () 1 2)" , "1")
test_case("(if 32 1 2)" , "1")
test_case("(if #\\a #\\b #\\c)" , "#\\b")


test_section(" CONS ")
test_case("(cons #t #f)" , "(#t . #f)")
test_case("(cons (cons 1 2) (cons 3 4))" , "((1 . 2) . (3 . 4))")


test_section(" CAR and CDR ")
test_case("(car (cons 1 2))" , "1")
test_case("(cdr (cons 1 2))" , "2")

test_case("(car (cons #t #f))", "#t")
test_case("(cdr (cons #t #f))", "#f")

test_case("(car (cons (cons 1 2) (cons 3 4)))" , "(1 . 2)")
test_case("(cdr (cons (cons 1 2) (cons 3 4)))" , "(3 . 4)")


test_section(" check negative number prints correctly ")
test_case("-32","-32")

test_section(" vector ")
test_case("(make-vector 12)", "#[#f #f #f #f #f #f #f #f #f #f #f #f]")
test_case("(make-vector 5)", "#[#f #f #f #f #f]")
test_case("(make-vector 3)", "#[#f #f #f]")
test_case("(make-vector 1)", "#[#f]")
test_case("(make-vector (+ 7 8))", "#[#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f]")

test_section(" divide by 2 ")
test_case("(div2 512)" ,"256")
test_case("(div2 256)" ,"128")
test_case("(div2 12)" ,"6")
test_case("(div2 3)" ,"1")
test_case("(div2 4)" ,"2")


test_section(" multiply by 3 and add 1 ")
test_case("(mul3+1 512)" ,"1537")
test_case("(mul3+1 256)" ,"769") 
test_case("(mul3+1 12)" ,"37")
test_case("(mul3+1 3)" ,"10")
test_case("(mul3+1 4)" ,"13")

test_section(" even? ")
test_case("(even? 5)","#f")
test_case("(even? 4)","#t")
test_case("(even? -5)","#f")
test_case("(even? 5)","#f")

test_section(" odd? ")
test_case("(odd? 5)","#t")
test_case("(odd? 4)","#f")
test_case("(odd? -4)","#f")
test_case("(odd? -5)","#t")

test_section(" fixnum = tests ")
test_case("(= 1 1)","#t")
test_case("(= 0 1)","#f")
test_case("(= 0 0)","#t")
test_case("(= 1 0)","#f")
test_case("(= -1 1)","#f")
test_case("(= -1 -1)","#t")
test_case("(= -256 -256)","#t")

test_section(" fixnum > tests ")
test_case("(> 1 1)","#f")
test_case("(> 0 1)","#f")
test_case("(> 0 0)","#f")
test_case("(> 1 0)","#t")
test_case("(> 1 -3)","#t")
test_case("(> -1 1)","#f")
test_case("(> -1 -1)","#f")
test_case("(> -256 -256)","#f")
test_case("(> -1 -5)","#t")
test_case("(> -5 -1)","#f")


test_section(" fixnum < tests ")
test_case("(< 1 1)","#f")
test_case("(< 0 1)","#t")
test_case("(< 0 0)","#f")
test_case("(< 1 0)","#f")
test_case("(< 1 -3)","#f")
test_case("(< -1 1)","#t")
test_case("(< -1 -1)","#f")
test_case("(< -256 -256)","#f")
test_case("(< -1 -5)","#f")
test_case("(< -5 -3)","#t")

#test_case("(div2 -256)" ,"-128")

test_section(" fixnum <= tests ")
test_case("(<= 1 1)","#t")
test_case("(<= 0 1)","#t")
test_case("(<= 0 0)","#t")
test_case("(<= 1 0)","#f")
test_case("(<= 1 -3)","#f")
test_case("(<= -1 1)","#t")
test_case("(<= -1 -1)","#t")
test_case("(<= -256 -256)","#t")
test_case("(<= -1 -5)","#f")
test_case("(<= -5 -3)","#t")


test_section(" fixnum >= tests ")
test_case("(>= 1 1)","#t")
test_case("(>= 0 1)","#f")
test_case("(>= 0 0)","#t")
test_case("(>= 1 0)","#t")
test_case("(>= 1 -3)","#t")
test_case("(>= -1 1)","#f")
test_case("(>= -1 -1)","#t")
test_case("(>= -256 -256)","#t")
test_case("(>= -1 -5)","#t")
test_case("(>= -5 -3)","#f")


test_section(" MULTIPLY tests ")
test_case("(* 2 3)" , "6")
test_case("(* -1 5)" , "-5")


# test_section(" F3 tests ")
# test_case("(f3x 1 2 3)","1")
# test_case("(f3y 1 2 3)","2")
# test_case("(f3z 1 2 3)","3")

# test_case("(f3x (f3x 1 2 3) 4 5)","1")
# test_case("(f3y (f3y 1 2 3) 4 5)","4")
# test_case("(f3z (f3z 1 2 3) 4 5)","5")

# test_case("(f3x (f3x 1 2 3) 4 5)","1")
# test_case("(f3x (f3y 1 2 3) 4 5)","2")
# test_case("(f3x (f3z 1 2 3) 4 5)","3")

# test_case("(f3x (let ((dummy 5)) (f3x 1 2 3)) 4 5)","1")
# test_case("(f3x (f3y 1 2 3) (let ((dummy 4)) dummy) 5)","2")
# test_case("(f3x (f3z 1 2 3) 4 (let ((dummy 5)) dummy))","3")

test_section(" LET ")
test_case("(let ((a 5)) 3)" , "3") # didnt use the bindings
test_case("(let ((a 5)) a)" , "5") 
test_case("(let ((a (+ 1 1))) a)" , "2")
test_case("(let ((a 1 2 3)) a)" , "3")
test_case("(let ((a 123)) (let ((b (+ a a))) b))" , "246")



test_section(" TAK tests ")


def tak_test(a,b):
    global test_case
    test_case(
        """
        (define tak (lambda (x y z) 
	       (if (< y x) 
		   (tak (tak (- x 1) y  z) 
			(tak (- y 1) z x) 
			(tak (- z 1) x y)) 
		   z)))
        """ + a , b)


tak_test("(tak 5 3 1)","2")
tak_test("(tak 7 3 1)","2")
tak_test("(tak 9 3 1)","2")
tak_test("(tak 11 3 1)","2")
tak_test("(tak 6 4 1)","2")
tak_test("(tak 8 4 1)","2")
tak_test("(tak 10 4 1)","2")
tak_test("(tak 12 4 1)","2")
tak_test("(tak 7 5 1)","2")
tak_test("(tak 9 5 1)","2")
tak_test("(tak 11 5 1)","2")
tak_test("(tak 8 6 1)","2")
tak_test("(tak 10 6 1)","2")
tak_test("(tak 12 6 1)","2")
tak_test("(tak 9 7 1)","2")
tak_test("(tak 11 7 1)","2")
tak_test("(tak 10 8 1)","2")
tak_test("(tak 12 8 1)","2")
tak_test("(tak 11 9 1)","2")
tak_test("(tak 12 10 1)","2")
tak_test("(tak 6 4 2)","3")
tak_test("(tak 8 4 2)","3")
tak_test("(tak 10 4 2)","3")
tak_test("(tak 12 4 2)","3")
tak_test("(tak 7 5 2)","3")
tak_test("(tak 9 5 2)","3")
tak_test("(tak 11 5 2)","3")
tak_test("(tak 8 6 2)","3")
tak_test("(tak 10 6 2)","3")
tak_test("(tak 12 6 2)","3")
tak_test("(tak 9 7 2)","3")
tak_test("(tak 11 7 2)","3")
tak_test("(tak 10 8 2)","3")
tak_test("(tak 12 8 2)","3")
tak_test("(tak 11 9 2)","3")
tak_test("(tak 12 10 2)","3")
tak_test("(tak 7 5 3)","4")
tak_test("(tak 9 5 3)","4")
tak_test("(tak 11 5 3)","4")
tak_test("(tak 8 6 3)","4")
tak_test("(tak 10 6 3)","4")
tak_test("(tak 12 6 3)","4")
tak_test("(tak 9 7 3)","4")
tak_test("(tak 11 7 3)","4")
tak_test("(tak 10 8 3)","4")
tak_test("(tak 12 8 3)","4")
tak_test("(tak 11 9 3)","4")
tak_test("(tak 12 10 3)","4")
tak_test("(tak 8 6 4)","5")
tak_test("(tak 10 6 4)","5")
tak_test("(tak 12 6 4)","5")
tak_test("(tak 9 7 4)","5")
tak_test("(tak 11 7 4)","5")
tak_test("(tak 10 8 4)","5")
tak_test("(tak 12 8 4)","5")
tak_test("(tak 11 9 4)","5")
tak_test("(tak 12 10 4)","5")
tak_test("(tak 9 7 5)","6")
tak_test("(tak 11 7 5)","6")
tak_test("(tak 10 8 5)","6")
tak_test("(tak 12 8 5)","6")
tak_test("(tak 11 9 5)","6")
tak_test("(tak 12 10 5)","6")
tak_test("(tak 3 1 6)","2")
tak_test("(tak 10 8 6)","7")
tak_test("(tak 12 8 6)","7")
tak_test("(tak 11 9 6)","7")
tak_test("(tak 12 10 6)","7")
tak_test("(tak 4 1 7)","2")
tak_test("(tak 4 2 7)","3")
tak_test("(tak 11 9 7)","8")
tak_test("(tak 12 10 7)","8")
tak_test("(tak 3 1 8)","2")
tak_test("(tak 5 1 8)","2")
tak_test("(tak 5 2 8)","3")
tak_test("(tak 5 3 8)","4")
tak_test("(tak 12 10 8)","9")
tak_test("(tak 4 1 9)","2")
tak_test("(tak 6 1 9)","2")
tak_test("(tak 4 2 9)","3")
tak_test("(tak 6 2 9)","3")
tak_test("(tak 6 3 9)","4")
tak_test("(tak 6 4 9)","5")
tak_test("(tak 3 1 10)","2")
tak_test("(tak 5 1 10)","2")
tak_test("(tak 7 1 10)","2")
tak_test("(tak 5 2 10)","3")
tak_test("(tak 7 2 10)","3")
tak_test("(tak 5 3 10)","4")
tak_test("(tak 7 3 10)","4")
tak_test("(tak 7 4 10)","5")
tak_test("(tak 7 5 10)","6")
tak_test("(tak 4 1 11)","2")
tak_test("(tak 6 1 11)","2")
tak_test("(tak 8 1 11)","2")
tak_test("(tak 4 2 11)","3")
tak_test("(tak 6 2 11)","3")
tak_test("(tak 8 2 11)","3")
tak_test("(tak 6 3 11)","4")
tak_test("(tak 8 3 11)","4")
tak_test("(tak 6 4 11)","5")
tak_test("(tak 8 4 11)","5")
tak_test("(tak 8 5 11)","6")
tak_test("(tak 8 6 11)","7")
tak_test("(tak 3 1 12)","2")
tak_test("(tak 5 1 12)","2")
tak_test("(tak 7 1 12)","2")
tak_test("(tak 9 1 12)","2")
tak_test("(tak 5 2 12)","3")
tak_test("(tak 7 2 12)","3")
tak_test("(tak 9 2 12)","3")
tak_test("(tak 5 3 12)","4")
tak_test("(tak 7 3 12)","4")
tak_test("(tak 9 3 12)","4")
tak_test("(tak 7 4 12)","5")
tak_test("(tak 9 4 12)","5")
tak_test("(tak 7 5 12)","6")
tak_test("(tak 9 5 12)","6")
tak_test("(tak 9 6 12)","7")
tak_test("(tak 9 7 12)","8")
tak_test("(tak 18 12 6)","7")


test_case("(< 10 2)","#f")
test_case("(if (< 10 2) 1 2)","2")

test_section(" subtract ")
test_case("(- 10 7)","3")
test_case("(- 1 2)","-1")
test_case("(+ (- 1 2) (- 3 4))","-2")
test_case("(- 1 -1)","2")
test_case("(- 10 -10)","20")


test_section(" FIB tests ")
def fib_test(a,b):
    global test_case
    test_case(
        """

(define (fib n)
  (if (= n 0) 0
      (if (= n 1) 1
          (if (= n 2) 1
              (+ (fib (- n 1)) (fib (- n 2)))))))

        """ + a , b)

fib_test("(fib 1)" , "1")
fib_test("(fib 2)" , "1")
fib_test("(fib 3)" , "2")
fib_test("(fib 4)" , "3")
fib_test("(fib 5)" , "5")
fib_test("(fib 6)" , "8")
fib_test("(fib 7)" , "13")
fib_test("(fib 8)" , "21")
fib_test("(fib 9)" ,  "34")
fib_test("(fib 10)" , "55")
fib_test("(fib 11)" , "89")
fib_test("(fib 12)" , "144")
fib_test("(fib 13)" , "233")
fib_test("(fib 14)" , "377")
fib_test("(fib 15)" , "610")
fib_test("(let ((x 1)) (fib 6))" , "8")



test_section(" FAC tests ")

# def fac_test(a,b):
#     global test_case
#     test_case(
#         """
# (define (fac n)
#   (if (= n 1) 1
#       (* n (fac (- n 1)))))
#         """ + a , b)



def fac_test(a,b):
    global test_case
    test_case(
        """
(define (fac n)
  (if (= n 1) 1
      (* n (fac (- n 1)))))

        """ + a , b)

fac_test("(fac 1)" , "1")
fac_test("(fac 2)" , "2")
fac_test("(fac 3)" , "6")
fac_test("(fac 4)" , "24")
fac_test("(fac 5)" , "120")
fac_test("(fac 6)" , "720")
fac_test("(fac 10)" , "3628800")
fac_test("(fac 11)" , "39916800")
fac_test("(+ (fac 1) (fac 2))" , "3")
fac_test("(+ (fac (fac 1)) (fac 2))" , "3")
fac_test("(+ (fac (fac 1)) (fac (fac 2)))" , "3")
fac_test("(+ (fac (fac 1)) (fac (fac 3)))" , "721")
fac_test("(fac (+ (fac (fac 1)) (fac (fac 2))))" , "6")


test_section(" begin section ")
test_case("(begin)","#f")
test_case("(begin 1)","1")
test_case("(begin #t)","#t")
test_case("(begin #f)","#f")
test_case("(begin 1 2 3)","3")
test_case("(begin (cons 1 2)(cons 3 4)(cons 5 6)(cons 7 8)(cons 9 10))","(9 . 10)")

test_case("(lambda () (+ 1 2))","#<closure>")
test_case("((lambda () (+ 1 2)))","3")

test_case("(let ((x 5)) (+ x x))","10")

test_case("((let ((x 5)) (lambda () (+ x x))))","10")
test_case("((let ((x 5)(y 6)) (lambda () (+ x x))))","10")

test_case("((let ((a 1)(b 2)(c 3)(d 4)) (lambda () a)))","1")
test_case("((let ((a 1)(b 2)(c 3)(d 4)) (lambda () b)))","2")
test_case("((let ((a 1)(b 2)(c 3)(d 4)) (lambda () c)))","3")
test_case("((let ((a 1)(b 2)(c 3)(d 4)) (lambda () d)))","4")



test_case("((let ((a 1)(b 2)(c 3)(d 4)(e 5)(f 6)(g 7)) (lambda () e)))","5")
test_case("((let ((a 1)(b 2)(c 3)(d 4)(e 5)(f 6)(g 7)) (lambda () f)))","6")
test_case("((let ((a 1)(b 2)(c 3)(d 4)(e 5)(f 6)(g 7)) (lambda () g)))","7")
test_case("((lambda (a b c) (+ a (+ b c))) 1 2 3)","6")
test_case("(define f 1)(define g 2) ((lambda (a b c) (+ a (+ b c))) 1 2 3)","6")

test_case("(define f 1) f" , "1")
test_case("(define f 1) (define g 2) f " , "1")

test_case("(define square (lambda (x) (* x x))) (define g 2) (square 5) " , "25")



 

#(* 18 4)
#(* 12 4)
#(* 6 4)
# TAK 72 48 24



# show results 
if fails == []:
    for r in passes:
        print(r)
    print ("\nCONGRATULATIONS - * * * ALL TESTS PASS * * *\n")

else:
    print("\n***************** ERROR ******************\n")
    print("\n***************** ERROR ******************\n")
    print("\n***************** ERROR ******************\n")
    for f in fails:
        print(f)

        

        


    
