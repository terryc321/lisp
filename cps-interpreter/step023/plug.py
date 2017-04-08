#!/usr/bin/python3

#
import sys

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
        #print(output)
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
                #print("AHA ! ")
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
        #print("[until_breakpoint_::" + output + "]")
        m = re.search("^\*stopped",output)
        if m :
            if m.group(0):
                #print("AHA ! ")
                return output
            else:
                pass
        else:
            pass



def gdb_tak_continue_until_breakpoint():
    global proc
    proc.stdin.write("-exec-continue".encode() + b'\n')
    proc.stdin.flush()
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')        
        #print("[until_breakpoint_::" + output + "]")
        m = re.search('.*?Breakpoint ([0-9]+), ([a-zA-Z_][a-zA-Z_0-9]+)',output)
        if m :
            if m.group(1):
                rval = [m.group(1) , m.group(2)]
                #print("GDB_TAK_found breakpoint " , rval)
                return rval
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
        #print("[" + output + "]")
        m = re.search("^\^done",output)
        if m :
            if m.group(0):
                #print("we-re done - AHA ! ")
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
        #print("[" + output + "]")
        m = re.match('.*done.*register-values.*value="([0-9]x[0-9a-f]+)"',output)
        if m :
            if m.group(1):
                #print("Register VALUE = [" + m.group(1) + "]")
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
        #print("[" + output + "]")
        m = re.match(".*done.*register-names=(\[.*\]).*",output)
        if m :
            if m.group(1):
                #print("Register Mapping = [" + m.group(1) + "]")
                # return the register value
                return m.group(1)
            else:
                pass
        else:
            pass


        

def gdb_read_stack(offset,count):
    global proc
    command = "-data-read-memory -o " + str(offset) + " $esp x 4 1 " + str(count)    
    #print("sending command " + command)
    proc.stdin.write(command.encode() + b'\n')
    proc.stdin.flush()
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        print("[" + output + "]")
        m = re.match(".*done.*",output)
        if m :
            if m.group(0):
                #print("Got match = [" + m.group(0) + "]")
                break
            else:
                pass
        else:
            pass


def gdb_TAK_read_stack(offset,count):
    global proc
    command = "-data-read-memory -o " + str(offset) + ' "$esp" d 4 1 ' + str(count)    
    #print("sending command " + command)
    proc.stdin.write(command.encode() + b'\n')
    proc.stdin.flush()
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        #print("[" + output + "]")
        m = re.match('.*done.*,data=(\[.*?\])',output)
        if m :
            if m.group(1):
                #print("Got match = [" + m.group(0) + "]")                
                dat = eval(m.group(1))
                dat[0] = round(int(dat[0]) / 4)
                dat[1] = round(int(dat[1]) / 4)
                dat[2] = round(int(dat[2]) / 4)
                #print("DATA = " , dat)                
                return dat
                break
            else:
                pass
        else:
            pass


        

        
        
def gdb_has_exited_normally():
    global proc
    output = proc.stdout.readline()    
    output = output.rstrip()
    output = output.decode('utf-8')
    #print("[" + output + "]")
    m = re.match(".*stopped.*exited.*",output)
    if m :
        return True
    else:
        return False
        


def gdb_stepi():
    global proc
    command = "-exec-step-instruction"
    #print("sending command " + command)
    proc.stdin.write(command.encode() + b'\n')
    proc.stdin.flush()
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        #print("[" + output + "]")
        m = re.match(".*stopped.*",output)
        if m :
            if m.group(0):
                #print("Got match = [" + m.group(0) + "]")
                break
            else:
                pass
        else:
            pass


# pc = program counter        
def gdb_disassemble_pc():
    global proc
    # try mode 0 just disassembly only
    command = '-data-disassemble -s $pc -e "$pc + 20" -- 0 '
    #print("sending command " + command)
    proc.stdin.write(command.encode() + b'\n')
    proc.stdin.flush()
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        #print("[" + output + "]")
        m = re.match('.done.asm_insns=\[{(address="(.*?)".*?,inst="(.*?)".*?)}.*"',output)
        if m :
            if m.group(0):                
                #print("\nGot match = [" + m.group(2) + "]\n")
                addr = m.group(2)
                instr = m.group(3)
                return [addr , instr.split()]
            else:
                pass
        else:
            pass


        
        
# which breakpoint did we hit ??
def gdb_which_tak_breakpoint():
    global proc
    # try mode 0 just disassembly only
    command = '-data-disassemble -s $pc -e "$pc + 20" -- 0 '
    print("sending command " + command)
    proc.stdin.write(command.encode() + b'\n')
    proc.stdin.flush()
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        print("[which_tak::" + output + "]")
        m = re.match('.*?,bkptno="(.*?)",.*',output)
        if m:
            if m.group(1):
                print("WE hit breakpoint " + m.group(1) + "\n")
                return m.group(1)
            else:
                pass
        else:
            pass
        # ^done
        m = re.match('^\^done',output)
        if m:
            if m.group(0):
                print("were done with something " + m.group(0) + "\n")
                return m.group(0)
            else:
                pass
        else:
            pass


        
        


        

# pc = program counter        
def gdb_disassemble_pc():
    global proc
    # try mode 0 just disassembly only
    command = '-data-disassemble -s $pc -e "$pc + 20" -- 0 '
    #print("sending command " + command)
    proc.stdin.write(command.encode() + b'\n')
    proc.stdin.flush()
    while True:
        output = proc.stdout.readline()    
        output = output.rstrip()
        output = output.decode('utf-8')
        #print("[" + output + "]")
        
        m = re.match('.done.asm_insns=\[{(address="(.*?)".*?,inst="(.*?)".*?)}.*"',output)
        if m :
            if m.group(0):                
                #print("\nGot match = [" + m.group(2) + "]\n")
                addr = m.group(2)
                instr = m.group(3)
                return [addr , instr.split()]
            else:
                pass
        else:
            pass



        
        

#---------------------------------------------------
# get the register names to register number mapping
#print("########## setup starting ##########")

register_mapping = eval(gdb_get_register_name_mapping())
register_hash = {}
i = 0
for reg in register_mapping:
    register_hash[reg] = str(i)
    i = i + 1

#print("the register mapping is as follows ")
#print(register_hash)

#print("########## setup completed ##########")


        
#wait_gdb_prompt()
#set_gdb_breakpoint("main")
#set_gdb_breakpoint("scheme_entry")

#send_gdb("-break-insert main")



send_gdb("-break-insert tak1")
#wait_gdb_prompt()

send_gdb("-break-insert tak2")
#wait_gdb_prompt()

send_gdb("-break-insert tak3")
#wait_gdb_prompt()

send_gdb("-break-insert tak4")
#wait_gdb_prompt()

#send_gdb("-break-insert tak")
#wait_gdb_prompt()

#send_gdb("-break-insert scheme_entry")
send_gdb("-break-insert scheme_heap_in_esi")
#send_gdb("-break-insert ")
gdb_run_until_breakpoint()
#wait_gdb_prompt()

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

#heap = gdb_reg_hex_value("esi");
#wait_gdb_prompt()
#print("the heap address is ", heap)

#wait_gdb_prompt()

#send_gdb("-break-insert --source \"/home/terry/lisp/cps-interpreter/step023/entry.asm\" --line 26")
#send_gdb("-break-insert --source \"/home/terry/lisp/cps-interpreter/step023/entry.asm\" --line 40")
#send_gdb("-break-insert --source \"/home/terry/lisp/cps-interpreter/step023/entry.asm\" --line 54")
#send_gdb("-break-insert --source \"/home/terry/lisp/cps-interpreter/step023/entry.asm\" --line 58")
#send_gdb("-break-insert --source \"/home/terry/lisp/cps-interpreter/step023/entry.asm\" --line 75")


#gdb_continue_until_breakpoint()
#gdb_continue_until_breakpoint()
#wait_gdb_prompt()


#gdb_continue_until_breakpoint()

#print(" we got line 388 in plug.py \n")
ncalls = 0
while True:
    # step instruction is SLOW 
    #gdb_stepi()
    
    #[addr,instr] = gdb_disassemble_pc()
    #bkpt = gdb_which_tak_breakpoint()
    #wait_gdb_prompt()
    
    #print("\ninstruction = ",instr , "\n")
    
    #if instr[0] == 'call':
    #    ncalls = ncalls + 1
    #    print(addr , " : CALL instruction found." , ncalls , " : " , instr , "\n")        
    #if gdb_has_exited_normally():
    #    print("\nPROGRAM has FINISHED\n")
    #    break

    # carry on mr GDB
    [bkpt , name] = gdb_tak_continue_until_breakpoint()
    #
    # tak1
    # tak2
    # tak3
    # tak4

    # TAK X  Y  Z
    #    -4 -8 -12
    #print("found breakpoint [" + bkpt + "] , [" + name  + "]")
    dat = []
    if name == "tak1":
        ncalls = ncalls + 1
        dat = gdb_TAK_read_stack(-12 , 3)
        print("(tak ", dat[0] , dat[1] , dat[2] , ") ; " , ncalls)        
        pass
    elif name == "tak2":
        ncalls = ncalls + 1
        dat = gdb_TAK_read_stack(-12 , 3)
        print("(tak ", dat[0] , dat[1] , dat[2] , ") ; " , ncalls)        
        pass
    elif name == "tak3":
        ncalls = ncalls + 1
        #print("ooo TAK 3 special ooo")
        dat = gdb_TAK_read_stack(-12 , 3)
        print("(tak ", dat[0] , dat[1] , dat[2] , ") ; " , ncalls)        
        pass
    elif name == "tak4":
        ncalls = ncalls + 1
        dat = gdb_TAK_read_stack(-12 , 3)
        print("(tak ", dat[0] , dat[1] , dat[2] , ") ; " , ncalls)        
        pass
    sys.stdout.flush()
    
    
    #
    

    

    

    

    
    
    

    

    
    
    
        

        

# #gdb_continue_until_breakpoint()
# #wait_gdb_prompt()

# #stack = gdb_reg_hex_value("esp");
# #wait_gdb_prompt()
# #print("the initial stack ESP address is ", stack)

# ## prints as signed decimal ??
# #send_gdb_no_wait("-data-evaluate-expression $esi")
# #wait_gdb_done()
# #wait_gdb_prompt()


# # read memory at esp , 4 bytes per word , 8 words , 1 row
# send_gdb("-data-read-memory ($esp) x 4 1 8")
# #wait_gdb_prompt()

# #send_gdb("-data-read-memory ($esp-4) x 4 1 8")
# #send_gdb("-data-read-memory ($esp-8) x 4 1 8")
# #send_gdb("-data-read-memory ($esp-12) x 4 1 8")
# # READ STACK from ESP-16 ESP-12 ESP-8 ESP-4 ESP ESP+4 ESP+8 ESP+12
# send_gdb("-data-read-memory ($esp-16) x 4 1 8")
# #send_gdb("-data-read-memory ($esp-16) u 4 1 8")
# send_gdb("-data-read-memory ($esp-16) d 4 1 8")


# send_gdb("-break-insert tak")
# gdb_continue_until_breakpoint()
# wait_gdb_prompt()


# #send_gdb("-data-read-memory ($esp-12) x 4 1 8")
# #send_gdb("-data-read-memory ($esp-12) d 4 1 8")
# #wait_gdb_prompt()

# # read esp 12 bytes below , for 8 words of 4 bytes each = 32 bytes
# stack_contents = gdb_read_stack(-12,8)
# wait_gdb_prompt()





# # proc.stdin.write(b'\n')
# # print("1",output.rstrip())
# # output = proc.stdout.readline()
# # print("2",output.rstrip())
# # output = proc.stdout.readline()
# # print("3",output.rstrip())
# # output = proc.stdout.readline()
# # print("4",output.rstrip())
# # output = proc.stdout.readline()
# # print("5",output.rstrip())
# # output = proc.stdout.readline()
# # print("6",output.rstrip())
# # output = proc.stdout.readline()
# # print("7",output.rstrip())
# # output = proc.stdout.readline()
# # print("8",output.rstrip())
# # output = proc.stdout.readline()
# # print("9",output.rstrip())
# # output = proc.stdout.readline()
# # print("10",output.rstrip())
# # output = proc.stdout.readline()
# # print("11",output.rstrip())
# # output = proc.stdout.readline()
# # print("12",output.rstrip())
# # output = proc.stdout.readline()
# # print("13",output.rstrip())
# # output = proc.stdout.readline()
# # print("14",output.rstrip())
# # output = proc.stdout.readline()
# # print("15",output.rstrip())
# # output = proc.stdout.readline()

# # proc.stdin.write(b'break scheme_entry\r\n')
# # proc.stdin.flush()



# # print("16",output.rstrip())
# # output = proc.stdout.readline()
# # print("17",output.rstrip())
# # output = proc.stdout.readline()
# # print("18",output.rstrip())
# # output = proc.stdout.readline()
# # print("19",output.rstrip())
# # output = proc.stdout.readline()
# # print("20",output.rstrip())




# ## Talk with date command i.e. read data from stdout and stderr. Store this info in tuple ##
# #Interact with process: Send data to stdin. Read data from stdout and stderr, until end-of-file is reached. Wait for process to terminate. The optional input argument should be a string to be sent to the child process, or None, if no data should be sent to the child.

# #(output, err) = proc.communicate()
# #print("output = ",repr(output))

# #(output, err) = proc.communicate(b'--break-insert scheme_entry')
# #print("output2 = ",repr(output))












