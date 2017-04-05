#!/usr/bin/python3

import subprocess

global results

results = []


def test_section(title):
    print("TEST SECTION :")
    print(title)
    print("\n")

    

def test_case(expr,expected):
    global results
    
    # encode as bytes
    expected = bytes(expected + "\n",'utf-8')
    expected = expected.decode()

    subprocess.run(['rm', '-f' , "work.temp" ])
    
    f= open('work.temp' , 'w')
    f.write(expr)
    f.close()
    
    subprocess.run(['rm', '-f' , "driver" ])
    subprocess.run(['rm', '-f' , "entry.asm" ])
    subprocess.run(['rm', '-f' , "entry.o" ])    
    
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
         results = results + [["PASS",expr,expected,result]]
         print("PASS\n")        
    else:
         results = results + [["FAIL",expr,expected,result]]
         print("FAIL\n")

def test_reset():
    global results
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






# show results 
for r in results:
    print(r)


