#!/usr/bin/python3


## get subprocess module 
import subprocess

# get regular expressions
import re

register_mapping = []
register_hash = {}

## call date command ##
#proc = subprocess.Popen(['gdb' ,'-mi', './driver'], stdin=subprocess.PIPE , stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=False)
proc = subprocess.Popen(['gdb' ,'--interpreter=mi', './driver'], stdin=subprocess.PIPE , stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=False ,  close_fds=True)


def wait_gdb_prompt():
    global proc
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()        
        print(output)
        if output == b'(gdb)':
            break
        else:
            pass

def set_gdb_breakpoint(routine):
    global proc
    proc.stdin.write(b'-break-insert ' + routine.encode() + b'\r\n')
    proc.stdin.flush()
    wait_gdb_prompt()


def send_gdb(command):
    global proc
    proc.stdin.write(command.encode() + b'\n')
    proc.stdin.flush()
    wait_gdb_prompt()

def send_gdb_no_wait(command):
    global proc
    proc.stdin.write(command.encode() + b'\n')
    proc.stdin.flush()

    
def gdb_run_until_breakpoint():
    global proc
    proc.stdin.write("-exec-run".encode() + b'\n')
    proc.stdin.flush()
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        print("[" + output + "]")
        m = re.search("^\*stopped",output)
        if m :
            if m.group(0):
                print("AHA ! ")
                break
            else:
                pass
        else:
            pass

def gdb_continue_until_breakpoint():
    global proc
    proc.stdin.write("-exec-continue".encode() + b'\n')
    proc.stdin.flush()
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        print("[" + output + "]")
        m = re.search("^\*stopped",output)
        if m :
            if m.group(0):
                print("AHA ! ")
                break
            else:
                pass
        else:
            pass



        
        
def wait_gdb_done():
    global proc
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        print("[" + output + "]")
        m = re.search("^\^done",output)
        if m :
            if m.group(0):
                print("we-re done - AHA ! ")
                break
            else:
                pass
        else:
            pass


def gdb_reg_hex_value(register):
    global proc
    send_gdb_no_wait("-data-list-register-values x " + register_hash[register])
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        print("[" + output + "]")
        m = re.match('.*done.*register-values.*value="([0-9]x[0-9a-f]+)"',output)
        if m :
            if m.group(1):
                print("Register VALUE = [" + m.group(1) + "]")
                # return the register value
                return m.group(1)
            else:
                pass
        else:
            pass

#---------------------------------------------------
def gdb_get_register_name_mapping():
    global proc
    send_gdb_no_wait("-data-list-register-names")
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        print("[" + output + "]")
        m = re.match(".*done.*register-names=(\[.*\]).*",output)
        if m :
            if m.group(1):
                print("Register Mapping = [" + m.group(1) + "]")
                # return the register value
                return m.group(1)
            else:
                pass
        else:
            pass
      
        

#---------------------------------------------------
# get the register names to register number mapping
print("########## setup starting ##########")

register_mapping = eval(gdb_get_register_name_mapping())
register_hash = {}
i = 0
for reg in register_mapping:
    register_hash[reg] = str(i)
    i = i + 1

print("the register mapping is as follows ")
print(register_hash)

print("########## setup completed ##########")


        
#wait_gdb_prompt()
#set_gdb_breakpoint("main")
#set_gdb_breakpoint("scheme_entry")

#send_gdb("-break-insert main")
#send_gdb("-break-insert scheme_entry")
send_gdb("-break-insert scheme_heap_in_esi")
gdb_run_until_breakpoint()
wait_gdb_prompt()

# send_gdb("-exec-step-instruction")
# wait_gdb_prompt()

# send_gdb("-exec-step-instruction")
# wait_gdb_prompt()


#send_gdb_no_wait("-data-list-register-names")
#wait_gdb_done()
#wait_gdb_prompt()

# send_gdb_no_wait("-data-list-register-values x eax")
# wait_gdb_done()
# wait_gdb_prompt()

# send_gdb_no_wait("-data-list-register-values x eax ebx ecx edx esi ebp esp")
# wait_gdb_done()
# wait_gdb_prompt()


#send_gdb_no_wait('-data-list-register-values x 6')
#wait_gdb_done()

heap = gdb_reg_hex_value("esi");
wait_gdb_prompt()
print("the heap address is ", heap)

#wait_gdb_prompt()


send_gdb("-break-insert --source \"/home/terry/lisp/cps-interpreter/step023/entry.asm\" --line 28")
send_gdb("-break-insert --source \"/home/terry/lisp/cps-interpreter/step023/entry.asm\" --line 43")
send_gdb("-break-insert --source \"/home/terry/lisp/cps-interpreter/step023/entry.asm\" --line 58")
send_gdb("-break-insert --source \"/home/terry/lisp/cps-interpreter/step023/entry.asm\" --line 62")
send_gdb("-break-insert --source \"/home/terry/lisp/cps-interpreter/step023/entry.asm\" --line 171")
gdb_continue_until_breakpoint()
wait_gdb_prompt()

#gdb_continue_until_breakpoint()
#wait_gdb_prompt()

stack = gdb_reg_hex_value("esp");
wait_gdb_prompt()
print("the initial stack ESP address is ", stack)

## prints as signed decimal ??
#send_gdb_no_wait("-data-evaluate-expression $esi")
#wait_gdb_done()
#wait_gdb_prompt()


# read memory at esp , 4 bytes per word , 8 words , 1 row
send_gdb("-data-read-memory ($esp) x 4 1 8")
#wait_gdb_prompt()

#send_gdb("-data-read-memory ($esp-4) x 4 1 8")
#send_gdb("-data-read-memory ($esp-8) x 4 1 8")
#send_gdb("-data-read-memory ($esp-12) x 4 1 8")
# READ STACK from ESP-16 ESP-12 ESP-8 ESP-4 ESP ESP+4 ESP+8 ESP+12
send_gdb("-data-read-memory ($esp-16) x 4 1 8")
#send_gdb("-data-read-memory ($esp-16) u 4 1 8")
send_gdb("-data-read-memory ($esp-16) d 4 1 8")


send_gdb("-break-insert tak")
gdb_continue_until_breakpoint()
wait_gdb_prompt()


send_gdb("-data-read-memory ($esp-12) x 4 1 8")
send_gdb("-data-read-memory ($esp-12) d 4 1 8")
#wait_gdb_prompt()







# proc.stdin.write(b'\n')
# print("1",output.rstrip())
# output = proc.stdout.readline()
# print("2",output.rstrip())
# output = proc.stdout.readline()
# print("3",output.rstrip())
# output = proc.stdout.readline()
# print("4",output.rstrip())
# output = proc.stdout.readline()
# print("5",output.rstrip())
# output = proc.stdout.readline()
# print("6",output.rstrip())
# output = proc.stdout.readline()
# print("7",output.rstrip())
# output = proc.stdout.readline()
# print("8",output.rstrip())
# output = proc.stdout.readline()
# print("9",output.rstrip())
# output = proc.stdout.readline()
# print("10",output.rstrip())
# output = proc.stdout.readline()
# print("11",output.rstrip())
# output = proc.stdout.readline()
# print("12",output.rstrip())
# output = proc.stdout.readline()
# print("13",output.rstrip())
# output = proc.stdout.readline()
# print("14",output.rstrip())
# output = proc.stdout.readline()
# print("15",output.rstrip())
# output = proc.stdout.readline()

# proc.stdin.write(b'break scheme_entry\r\n')
# proc.stdin.flush()



# print("16",output.rstrip())
# output = proc.stdout.readline()
# print("17",output.rstrip())
# output = proc.stdout.readline()
# print("18",output.rstrip())
# output = proc.stdout.readline()
# print("19",output.rstrip())
# output = proc.stdout.readline()
# print("20",output.rstrip())




## Talk with date command i.e. read data from stdout and stderr. Store this info in tuple ##
#Interact with process: Send data to stdin. Read data from stdout and stderr, until end-of-file is reached. Wait for process to terminate. The optional input argument should be a string to be sent to the child process, or None, if no data should be sent to the child.

#(output, err) = proc.communicate()
#print("output = ",repr(output))

#(output, err) = proc.communicate(b'--break-insert scheme_entry')
#print("output2 = ",repr(output))












