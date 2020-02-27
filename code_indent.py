import re
import sys

iname = ""
oname = ""

if len(sys.argv)<2:
    print("ERROR! Please specify a file to process!")
    sys.exit()
elif len(sys.argv)==2:
    iname = str(sys.argv[1])
    oname = iname
    print("INPUT&OUTPUT:   ",iname)
elif len(sys.argv)==3:
    iname = str(sys.argv[1])
    oname = str(sys.argv[2])
    print("INPUT:   ",iname)
    print("OUTPUT:  ",oname)
str_data = """  

        if(a) b=c  
       if(a&
       b&
       c) then
       a=b
       endif
       
       if(a&
       b&
       c) a=b
       if(a) then
    b=c
    endif
"""

f90_str=""

with open(iname,"r") as f:
    f90_str = f.read()

print(len(f90_str))


m=re.findall(r'.*',str(f90_str))
line_intent = 0
post_str=[]
res_str=[]
# print(str_data)
# print("___________________________________")

for item in m:
    post_item = re.sub('^\s*','',item)
    post_item = re.sub('^\s*\n', '', post_item)

    post_str.append(post_item)
    #print(item)

#print(post_str[-3])
#print(post_str)
    #print(post_item)
bra_stack=[]
fbra=0
for s in post_str:

    #print(s)
    if re.search("&", s) and re.search("(?i)^\s*if\s*\(", s):
        fbra = 1

    if len(bra_stack) != 0:
        pass
    if fbra==1:
        #print(s)
        for s_ in s:
            #print(s_)
            if s_ =='(':
                bra_stack.append(s_)
            if s_==')':
                bra_stack.pop()
        #directly call
        if len(bra_stack)==0 and not re.search("(?i)then", s):
            s = (line_intent+1) * '\t' + s
            fbra   = 0
            #print("======",s)
            res_str.append(s)
            continue
        # elif len(bra_stack) == 0 and not re.search("then|THEN", s):
        #     s = (line_intent + 1) * '\t' + s
        #     print("=====",s)
        #     # fbra = 0
        #     # print(s)
        #     res_str.append(s)
        #     continue
        elif len(bra_stack)!=0 and re.search("&", s) and not re.search("(?i)^\s*if\s*\(", s):
            s = (line_intent+1) * '\t' + s
            #fbra = 0
            # print(s)
            res_str.append(s)
            continue
        elif len(bra_stack)==0 and re.search("(?i)then", s):
            line_intent = line_intent+1
            s = (line_intent) * '\t' + s
            fbra = 0
            # print(s)
            res_str.append(s)
            continue
        elif len(bra_stack)!=0 and re.search("&", s) and re.search("(?i)^\s*if\s*\(", s):
            #line_intent = line_intent+1
            s = (line_intent) * '\t' + s
            #fbra = 0
            # print(s)
            res_str.append(s)
            continue




    if re.search("(?i)^\s*end\s*$",s):
        #print(s)
        line_intent = line_intent - 1
        res_str.append(s)
        continue


    if re.search("(?i)^\s*subroutine|^\s*function",s):
        line_intent = 1
        res_str.append(s)
        continue
    if re.search("&",s) and not re.search("(?i)then",s) and not re.search("&",s) :
        s = line_intent * '\t' + s
        #print(s)
        res_str.append(s)
        continue

    if re.search("(?i)^\s*end\s*do",s) or\
            re.search("(?i)^\s*end\s*if",s) or\
            re.search("(?i)^\s*else",s):
        line_intent = line_intent - 1
        #print(s)

    s = line_intent*'\t'+s
    #print(line_intent)
    #print(line_intent)
    res_str.append(s)
    #print(s)

    if re.search("(?i)^\s*[Dd][Oo]\s*",s) or \
        (re.search("(?i)^\s*if\s*\(",s) and (re.search("then",s)))or \
            re.search("(?i)^\s*else\s*(if)?",s) or \
            (re.search("(?i)^\s*.*:\s*do\s", s) and not re.search("^\s*!", s)) or \
            (re.search("(?i)^\s*[0-9]*:*\s*if\s*\(", s) and (re.search("(?i)then", s))):
        # if (re.search("^\s*.*:\s*do\s|^\s*.*:\s*DO\s", s) and not re.search("^\s*!", s)):
        #     print(s)
        #if (re.search("^\s*[1-9]*\s*if\s*\(|^\s*[1-9]*\s*IF\s*\(", s) and (re.search("then|THEN", s))):
            #print(s)
        line_intent=line_intent + 1
        #print(s)
    #print(s)
c_str= ""
for s in res_str:
    c_str = c_str+s+"\n"
    #print(s)
    pass
g_str = re.sub("(\n\t*\n){1}","\n",c_str)
"\n".join(c_str.split())
#print(g_str)
    #s = line_intent*'\t'+s

with open(oname,"w") as f1:
    f1.write(g_str)
#post_str = re.sub(r'^\s*',r'',str_data)
#print(m)

