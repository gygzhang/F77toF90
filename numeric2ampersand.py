"""
Statement label                   1 through 5
Continuation indicator            6
Statement                         7 to the end of the line or to the start
                                    of the comment field
Comment (optional)                73 through end of line

"""


import re
import os
import c2mark

con="""
C     TI1 = T1 * AF1                                                    FLW02340
C     TI2 = T2 * AF2                                                    FLW02350
C     SUM = TI0 + TI1 + TI2                                             FLW02360
      SUM = ZERO                                                        FLW02370
C                                                                       FLW02380
C *** COMPUTE THE QUANTITY IDXX.  ....EQN. A-31                         FLW02390
      IDXX = -TWO * LXI2 * (ONE + TWO * XMDLX2) * I  -                  FLW02400
     X       FOUR * XMDLX * LXI * IDX  +  TZFAC * HHXYM * SUM           FLW02410
C                                                                       FLW02420
C                                                                       FLW02430
C                                                                       FLW02440
C *** COMPUTE THE VARIOUS INTEGRAL TERMS FOR IDY.                       FLW02450
C     T0 = ZERO                                                         FLW02460
C     T1 = ZERO                                                         FLW02470
      """


def get_sfun(con):
    subr=re.findall(r"\s*subroutine\s+[A-z]",con)
    print(subr.gourp(0))

def process_sf(fn):
    with open(fn,"r") as f:
        con = f.read()
        con = c2mark.c2m(con)
        (n2a1(con,fn))

def n2a1(str1,fn):
    str1 = str1.replace("\r\n","\n")
    lines = re.findall(r".*", str1)
    lit = []
    for idx,line in enumerate(lines):
        #lines[idx] = lines[idx].replace("\r\n","\n")
        if line == "":
            continue
        if line[0]=="*":
            line="!"+line[1:]
        # print(line)
        if re.search(r"include\s*'.*\.for'",line):
            line = line.strip()
            s = re.search(r"include\s*",line)
            s1= re.search(r"'.*.for'",line)
            #print(s.start(),s.end())
            #print(line[s1.start():s1.end()-4])
            line = re.sub(r"include\s*'.*\.for'"," "*8+"include '"+line[s1.start()+1:s1.end()-4]+"f90'",line)
            #print(lines[idx])
        lit.append(line.expandtabs(6))
    for idx, li in enumerate(lit):
        # print(li)
        #

        if re.search(r"\.\s*((LT)|(GT)|(EQ)|(OR)|(AND)|(NOT))\s*\.",li):
            s = re.search(r"\.\s*LT\s*\.",li)
            #print(li)
            #print(li)
            if re.search(r"\.\s*LT\s*\.",li):
                #print(s.start(),s.end())
                #print(li[s.start():s.end()])
                lit[idx] = re.sub(r"\.\s*LT\s*\.",".LT.",lit[idx])
                #print(lit[idx])
            if re.search(r"\.\s*GT\s*\.",li):
                lit[idx] = re.sub(r"\.\s*GT\s*\.",".GT.",lit[idx])
            if re.search(r"\.\s*EQ\s*\.",li):
                lit[idx] = re.sub(r"\.\s*EQ\s*\.",".EQ.",lit[idx])
            if re.search(r"\.\s*OR\s*\.",li):
                lit[idx] = re.sub(r"\.\s*OR\s*\.",".OR.",lit[idx])
            if re.search(r"\.\s*AND\s*\.",li):
                lit[idx] = re.sub(r"\.\s*AND\s*\.",".AND.",lit[idx])
            if re.search(r"\.\s*NOT\s*\.",li):
                lit[idx] = re.sub(r"\.\s*NOT\s*\.",".NOT.",lit[idx])




        if re.search(r"^!", li) or re.search(r"^\s*\s$",li) or len(li) < 6 :
            if re.search(r"^\s*\s$", li):
                #print(li)
                pass
            continue
        #print((li[6].isspace()),li[6])
        #print("{0:x}".format(li[6]))
        #print(idx)
        try:
            #find a continuation indicator
            #print(li)
            if not li[5].isspace():
                #print(li)
                i=1
                #when last line is a comment, increase i untill find "real" last line
                while re.search(r"^!",lit[idx-i]) or re.search(r"^\s*\s$",lit[idx-i]):
                    i=i+1
                pm=re.search(r"!",lit[idx-i])
                #
                #print(li)
                if pm is not None and pm.start()!=0 and len(li)<73:
                    ss=pm.start()
                    #print(ss)
                    lit[idx - i] = lit[idx - i][0:ss] + " & " + lit[idx-i][ss:len(lit[idx - i])]
                    lit[idx] = " " * 6 + lit[idx][6:len(lit[idx])]
                    #print(lit[idx - 1], pm.start())
                    continue
                end=len(lit[idx-i])
                if end>72:
                    end=71
                #print(i,end,lit[idx-i],lit[idx])
                lit[idx - i] = lit[idx - i][0:end] + "&"+lit[idx-i][end:]
                lit[idx] = " " * 6 + lit[idx][6:len(lit[idx])]
                #print(lit[idx-i])

                #print(len(lit[idx]),lit[idx-i])
            if not re.search(r"!",li) and len(li)>72:
                #if not li[72].isspace():
                lit[idx] = lit[idx][0:71]+"!"+lit[idx][71:]
                #print(lit[idx])


        except IndexError:
            #print(li,fn)
            pass

    c2w = ""
    for l in lit:
        c2w = c2w + l + "\n"

    return(c2w)

def n2a(str):
    lines = re.findall(r".*", str)
    lit = []
    for line in lines:
        if line == "":
            continue
        # print(line)
        lit.append(line)
    for idx, li in enumerate(lit):
        # print(li)
        if re.search(r"^!", li):
            continue
        if re.search(r"\s*[0-9]+\s+", li):
            # can use for to enumerate the se to get idices of all occurances of that pattern
            # se=re.finditer(r"[0-9]+\s+",li)
            se = re.search(r"[0-9A-z]+\s+", li)
            if se.start()>6:
                continue
            # i=li.find(r"[0-9]+\s+")
            # for s in se:
            # print(se.start())
            # print(lit[idx-1])
            # print(len(lit[idx-1]))
            # lit[idx-1]=lit[idx-1]+" &"
            lit[idx - 1] = lit[idx - 1][0:len(lit[idx - 1])] + " &"
            lit[idx] = " " * 6 + lit[idx][se.start() + 1:-1]
            #print(li)
            # print("------------------")

        # print(lit[idx-1])

    c2w = ""
    for l in lit:
        c2w = c2w + l + "\n"

    return(c2w)

def process_continue(srcdir,dstdir,*ftype):
    # pp=os.getcwd()
    # print(pp)
    # pp=os.path.join(os.curdir)
    # print(pp)
    savedir=dstdir
    savepath=os.path.join(os.getcwd(),savedir)
    files = os.listdir(os.path.join(os.getcwd(),srcdir))
    if not os.path.exists(savepath):
        os.makedirs(savepath)
    fns = []
    for fn in files:
        if fn.split('.')[-1] in ftype:
            fns.append(fn)
    print(fns)
    for fn in fns:
        with open(os.path.join(os.getcwd(),srcdir,fn), 'r') as f:
            con = f.read()
            con=c2mark.c2m(con)
            c2w=n2a1(con,fn)
            if re.search(r".for",fn):
                fn=re.sub(r".for",".f90",fn)
            np = os.path.join(savepath,fn)
            # if fn=="api_chem.for":
            #     #print(c2w)
            #     with open(np, "w") as f:
            #         f.write(c2w)
            #
            #     rr=""
            #     with open(np, "r") as f:
            #         rr=f.read()
            #         print(rr)

            print(np)
            with open(np, "w") as f:
                f.write(c2w)
#print(__name__)



if __name__=="__main__":
    con = c2mark.c2m(con)

    #print(n2a(con))
    #(n2a1(con,"no"))
    process_continue("textfiles","savedir6","puf","for","inc","cal","crd")
    #process_sf(os.path.join(os.getcwd(),"textfiles","calpuff.for"))

    #print(n2a1(con,"nofile"))
