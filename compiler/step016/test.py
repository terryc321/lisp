#!/usr/bin/python3


import subprocess

global results

results = []
fails = []


def test_section(title):
    print("TEST SECTION :")
    print(title)
    print("\n")

    

def test_case(expr,expected):
    global results
    
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
    
    subprocess.run(['guile', '-c' , "(begin (load \"comp.scm\") (compile-program \"work.temp\" \"entry.asm\"))" ])
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
         print("PASS\n")        
    else:
         fails = fail + [["FAIL",expr,expected,result]]
         print("FAIL\n")

def test_reset():
    global results
    global fails
    fails = []
    results = []
    
   
        
# simple testing infrastructure
test_reset()

test_section("fixnums")
test_case("32","32")

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

test_section(" subtract ")
test_case("(- 10 7)","3")
test_case("(- 1 2)","-1")
test_case("(+ (- 1 2) (- 3 4))","-2")
test_case("(- 1 -1)","2")
test_case("(- 10 -10)","20")


test_section(" not ")
test_case("(not #f)","#t")
test_case("(not #t)","#f")
test_case("(not 32)","#f")
test_case("(not #\\a)","#f")
test_case("(not #\\A)","#f")
test_case("(not ())","#f")


test_section(" LET ")
test_case("(let ((a 5)) 3)" , "3") # didnt use the bindings
test_case("(let ((a 5)) a)" , "5") 
test_case("(let ((a (+ 1 1))) a)" , "2")
test_case("(let ((a 1 2 3)) a)" , "3")
test_case("(let ((a 123)) (let ((b (+ a a))) b))" , "246")


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




#test_case("(div2 -256)" ,"-128")




# show results 
for r in results:
    print(r)
if fails == []:
    print ("\nCONGRATULATIONS - * * * ALL TESTS PASS * * *\n")
else:
    print("\n***************** ERROR ******************\n")
    print("\n***************** ERROR ******************\n")
    print("\n***************** ERROR ******************\n")
    for f in fails:
        print(f)

        


    
