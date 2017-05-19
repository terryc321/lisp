#!/usr/bin/python3

import random

def randStack(a,b,size):
    r = []
    while len(r) < size:
        n = random.randrange(a,b)
        r = r + [n]
    return r 

def showStack(stack):
    print("-------- show stack -------- ")
    print("stack   = ", stack)

print("----------- random stack generator ---------")

s1 = randStack(100,200,10)
showStack(s1)












