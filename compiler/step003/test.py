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


for r in results:
    print(r)


