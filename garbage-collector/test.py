#!/usr/bin/python3


# top of stack
# bottom of stack
# size of stack

# size of heap A
# size of heap B 

# let heap be 100 bytes long
heapSize = 100

heapA = ["this is heap A",123]
heapB = ["this is heap B",456]

fromSpace = heapA
toSpace = heapB

def swapHeap():
    global fromSpace , toSpace , heapA , heapB
    if fromSpace == heapA and toSpace == heapB:
        fromSpace = heapB
        toSpace = heapA
    else:
        fromSpace = heapA
        toSpace = heapB


def showHeap():
    print("-------- show heap -------- ")
    print("fromSpace = ", fromSpace)
    print("toSpace   = ", toSpace)



print("hello world")

showHeap()

swapHeap()
showHeap()

swapHeap()
showHeap()

swapHeap()
showHeap()

swapHeap()
showHeap()

swapHeap()
showHeap()

swapHeap()
showHeap()


    
    


