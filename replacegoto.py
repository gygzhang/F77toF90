from __future__ import division
import numeric2ampersand as n2a
import os
import sys
import re
from collections import OrderedDict
import datetime
import copy
import shutil
import random

con = """

"""

class file_mannager:
    def __init__(self):
        pass

    def add_subprogram_manager(self, names, cspms):
        for name, cspm in zip(names, cspms):
            print(name, cspm)
            setattr(self, name, cspm)
        #self.class_subprogram_manager_list
    
    def sub_subprogram_text_by_name(self,sp_name, sub_sm, be_sub_sm):
        ssm_sp_idx = self.sub_sm.get_idx_by_name(sp_name)
        sub_text = self.sub_sm.class_subprogram[ssm_sp_idx].ftext
        bessm_sp_idx = self.be_sub_sm.get_idx_by_name(sp_name)
        self.be_sub_sm.class_subprogram[bessm_sp_idx].ftext = sub_text
        
    def get_name_list(self, sub_sm, start_sp_name, end_sp_name ):
        self.subprogram_name_list = []
        ssm_start_idx = self.sub_sm.get_idx_by_name(start_sp_name)
        ssm_end_idx = self.sub_sm.get_idx_by_name(end_sp_name)
        for i in range(ssm_start_idx, ssm_end_idx+1):
            name = self.sub_sm.get_name_by_idx(i)
            self.subprogram_name_list.append(name)

    def subprogram_manager_range(self, sub_sm, be_sub_sm):
        for name in self.subprogram_name_list:
            self.sub_subprogram_text_by_name(name, sub_sm, be_sub_sm)
    


        


    def sub_by_se_fun_name(self, sub_sm, be_sub_sm, start_sp_name, end_sp_name):
        ssm_start_idx = self.sub_sm.get_idx_by_name(start_sp_name)
        ssm_end_idx = self.sub_sm.get_idx_by_name(end_sp_name)

        ssm_sln = self.sub_sm[ssm_start_idx].get_start_line_number()
        ssm_eln = self.sub_sm[ssm_end_idx].get_end_line_number()

        bessm_start_idx = self.be_sub_sm.get_idx_by_name(start_sp_name)
        bessm_end_idx = self.be_sub_sm.get_idx_by_name(end_sp_name)

        bessm_sln = self.sub_sm[bessm_start_idx].get_start_line_number()
        bessm_eln = self.sub_sm[bessm_end_idx].get_end_line_number()

    def write_to_file(self,be_sub_sm):
        self.be_sub_sm.write_to_file()

        # for i in range(ssm_start_idx,ssm_start_idx):

class subprogram_manager:


    def __init__(self,fn):
        self.functions=[[]for i in range(1000)]
        #the get content of given file name as a string
        self.file_text = self._get_content_f2l(fn)
        self.sp_start_line_number = []
        self.sp_end_line_number = []
        self.ubprograms = []
        self.class_subprogram = []
        self.num_g2s = 0
        self.total_comments = 0
        self.total_lines,self.total_functions = self._get_subroutine(self.file_text)
        self.total_statements = self.total_lines - self.total_comments
        self.odict = OrderedDict()
        self.function_names = self._get_ng2()
        self.start_idx = 0
        self.end_idx = self.total_functions - 1
        self.num_goto_modified = 0
        self.text_before_subprogram = self.get_text_to_subprogram()

    def __iter__(self):
        self.iter_num = self.start_idx
        return self

    def next(self):
        if self.iter_num <= self.end_idx:
            #print(self.function_names[self.iter_num])
            num = self.iter_num
            cr = self.class_subprogram[self.iter_num]
            self.iter_num = self.iter_num + 1
            return cr
        else:
            raise StopIteration

    def get_text_to_subprogram(self):
        p1 = r"(?i)^\s*subroutine\s+(\w+\(.*\)){0,1}"
        p2 = r"(?i)^\s*\w*\s*function\s+(\w+\(.*\)){0,1}"
        spat = p1+"|"+p2
        pat = re.compile(p1+"|"+p2)
        t_list = []
        #print(len(self.file_text))
        for i, line in enumerate(self.file_text):
            
            if not pat.search(line):
                line = re.sub("\n","",line)
                t_list.append(line)
            else:
                break
        #print(len(t_list))
        #exit()
        return t_list

    def get_num_goto_modified(self):
        return self.num_goto_modified
    
    def set_num_goto_modified(self,num_goto_modified):
        self.num_goto_modified = num_goto_modified

    def set_se_idx(self,s,e):
        self.start_idx = s
        self.end_idx = e

    def _get_sps(self,con):
        for f in fs:
            #self.functions
            pass

    def _get_subroutine(self,con):
        lines = con
        p1 = r"(?i)^\s*subroutine\s+(\w+\(.*\)){0,1}"
        p2 = r"(?i)^\s*\w*\s*function\s+(\w+\(.*\)){0,1}"
        spat = p1+"|"+p2
        pat = re.compile(p1+"|"+p2)
        sp_cnt=0
        sp_idx = 0
        first_sp = 0
        for idx,line in enumerate(lines):
            if re.search(r"^\s*!",line):
                self.total_comments = self.total_comments + 1
            if pat.search(line):
                
                sgl=[]
                #subr_ls.extend(sgl)
                self.functions.extend(sgl)
                self.sp_start_line_number.append(idx+1)
                if sp_cnt != 0:
                    self.sp_end_line_number.append(idx-1)
                sp_cnt=sp_cnt+1
            #subr_ls[sp_cnt-1].append(re.sub(r"\n$","",line))
            self.functions[sp_cnt-1].append(re.sub(r"\n$","",line))
        self.sp_end_line_number.append(idx)
        #self.functions.append(subr_ls[sp_cnt-1])

        return len(lines),sp_cnt
    def _get_ng2(self):
        #len(sbur_ls)
        p1 = r"(?i)go\s*to"
        p2 = r"^\s*!"
        pat1 = re.compile(p1)
        pat2 = re.compile(p2)
        gn_g2=0
        rn_g2 = 0
        csps = []
        #rn_g2=OrderedDict()
        ft = ""
        prt = r"(?i)((?i)(?<=subroutine))\s*\w*"
        pfc = r"(?i)((?i)(?<=function))\s*\w*"
        patrt = re.compile(prt)
        patfc = re.compile(pfc)
        nst = 0
        function_names = []
        for idx,fun in enumerate(self.functions):
            if idx < self.total_functions:
                if patrt.search(fun[0]):
                    ft = "subroutine"
                if patfc.search(fun[0]):
                    ft = "function"
                nst = 0
                nl = len(fun)
                g = re.search(r"(?i)((?i)(?<=subroutine)|(?<=function))\s*\w*",fun[0])
                n_g2=0
                for line in self.functions[idx]:
                    if pat2.search(line):
                        nst = nst + 1
                        continue
                    if pat1.search(line):
                        ksp = line
                        n_g2 = n_g2 + 1
                        self.num_g2s = self.num_g2s + 1
                        #print(n_g2)

                if g is not None:
                    #print(g.group(0))
                    #rn_g2[g.group(0).strip()] = n_g2
                    pass
                else:
                    print("err",fun[0])
                function_names.append(g.group(0).strip())
                csp = subprogram(idx,g.group(0).strip(),ft,fun,nl,nst,nl-nst,n_g2,self.sp_start_line_number[idx],self.sp_end_line_number[idx])
                #csp._get_dfsln()
                self.class_subprogram.append(csp)
        #return rn_g2
        return function_names


    def add_subprogram(self,sp):
        self.subprogram.append(sp)
    def _get_content_f2l(self,fn):
        if os.path.exists(fn):
            with open(fn,"r") as f:
                return f.readlines()
    
    def get_goto_number(self,sidx,eidx):
        ng2 = 0
        for sp_idx in range(sidx,eidx+1):
            csp = self.class_subprogram[sp_idx]
            csp.update_goto_statement()
            len_goto = len(csp.class_goto_list)
            ng2 += len_goto
            for cg2 in self.class_subprogram[sp_idx].class_goto_list:
                print(self.class_subprogram[sp_idx].ftext[cg2.get_start_line_number()])
        return ng2
    def show_ifu_ng2(self,sidx,eidx):
        print("--------------------function list---------------------")
        bli = "|----+------------------------------------------------|"
        ng2 = 0
        for idx,fn in enumerate(self.function_names):
            if idx in range(sidx,eidx+1):
                ng2 = ng2 + self.class_subprogram[idx].ng2
                l = "|"+"{0}".format(idx) +" | "+"{0}".format(fn).ljust(47)+"|" +" {0}".format(self.class_subprogram[idx].ng2)
                print(l)
                print(bli)
        print("total goto: {0}".format(ng2))

    def show_file_text():
        pass
    def show_status(self):
        print("+----------------+----------------------+")
        print("|lines:          |      {0}           |".format(self.total_lines))
        print("|----------------|----------------------|")
        print("|statements:     |      %d           |"%self.total_statements)
        print("|----------------|----------------------|")
        print("|comments:       |      %d           |"%self.total_comments)
        print("|----------------|----------------------|")
        print("|comment ratio:  |      {0:.2f}%          |".format(float(100*self.total_comments/self.total_lines)))
        print("|----------------|----------------------|")
        print("|statement ratio:|      {0:.2f}%          |".format(100*self.total_statements/self.total_lines))
        print("|----------------|----------------------|")
        print("|num goto :      |      %d             |"%self.num_g2s)
        print("+----------------+----------------------+")

    def show_subprogram(self):
        for sp in self.class_subprogram:
            print(sp.order,sp.fname,sp.fn_ng2)
    
    #get the order of function by the name given
    def get_order(self,fn):
        for idx, tfn in enumerate(self.function_names):
            if fn.lower() == tfn.lower():
                return idx
    
    def update_text(self):
        text = ""
        for sp in self.class_subprogram:
            for line in sp.ftext:
                text = text + "\n"+ line
        self.file_text = ""
        self.file_text = text

    def get_idx_by_name(self,fn):
        for sp_idx,sp_cls in enumerate(self.class_subprogram):
            if sp_cls.fname ==fn:
                #print(sp_cls.get_idx_id())
                return sp_cls.get_idx_id()
        return None

    def get_name_by_idx(self,i):
        return self.class_subprogram[i].fname

    def is_subprogram_declaration(self,line):
        prt = r"(?i)((?i)(?<=subroutine))\s*\w*"
        pfc = r"(?i)((?i)(?<=function))\s*\w*"
        patrt = re.compile(prt)
        patfc = re.compile(pfc)
        if patrt.search(line) or patfc.search(line):
            return True
        else:
            return False

    def sub_with_se_subprogram(self, sp_start_idx, sp_end_idx):

        for sp_idx in range(sp_start_idx, sp_end_idx + 1):
            csp = self.class_subprogram[sp_idx]
            ssi_sln = csp

    def string_to_list(self):
        #r = re.findall(r".*",self.file_text)
        r = re.split(r"\n+",self.file_text)
        file_text_list = []
        for line in r:
            if re.search(r"\n",line):
                continue
            else:
                file_text_list.append(line)
        #print(len(file_text_list))
        return file_text_list

    def get_line_number_by_name(self,fn):
        text_list = self.string_to_list()
        for idx, line in enumerate(text_list):
            if self.is_subprogram_declaration(line):
                if re.search(fn,line):
                    return idx
        
        return None

    def get_subprogram_length(self,subprogram_name):
        sp_idx = self.get_idx_by_name(subprogram_name)
        start_line_number = self.class_subprogram[sp_idx].start_line_number
        end_line_number = self.class_subprogram[sp_idx].end_line_number
        return end_line_number - start_line_number

    #get a file by the content of functions that in the class
    def write_to_file(self,fn,start_name=None,end_name=None,start_idx = -1, end_idx = -1):
        # with open(fn,"w") as f:
        #     f.write(self.file_text)
        self.update_text()
        start_line_number = -1
        end_line_number = -1

        text_list = self.string_to_list()
        

        if start_name != None and end_name!=None:
            start_idx = self.get_idx_by_name(start_name)
            end_idx = self.get_idx_by_name(end_name)
            start_line_number = self.get_line_number_by_name(start_name)
            end_line_number = self.get_line_number_by_name(end_name)
            end_subprogram_length = self.get_subprogram_length(end_name)
        if start_line_number >=0 and end_line_number >= 0:
            if os.path.exists(fn):
                os.remove(fn)
            
            with open(fn,"a") as f:
                for i in range(start_line_number,end_line_number + 1 + end_subprogram_length):
                    f.write(text_list[i]+"\n")

    def write2file_full(self, fn):
        text_list = self.string_to_list()
        print(len(text_list))
        self.text_before_subprogram.extend(text_list)
        print(len(self.text_before_subprogram))
        if os.path.exists(fn):
                os.remove(fn)
        with open(fn,"a") as f:
            for i in range(0, len(self.text_before_subprogram)):
                f.write(self.text_before_subprogram[i]+"\n")

    #subsitute goto "end label" with return clause
    def modify_goto_end_label(self):
        print("in function modify_goto_end_label...")
        for sp_idx in range(self.start_idx,self.end_idx+1):
            #print("------------{0}-----------------".format(self.class_subprogram[idx].fname))
            self.class_subprogram[sp_idx].update_exit_label()
            self.class_subprogram[sp_idx].update_label()
            self.class_subprogram[sp_idx].update_goto_statement_start()
            self.class_subprogram[sp_idx].update_goto_statement_end()
            self.class_subprogram[sp_idx].update_do_label_statement()
            if self.class_subprogram[sp_idx].exit_label != 0:
                #print(self.class_subprogram[idx].fname,self.class_subprogram[idx].exit_label)
                pass

            for cg2 in self.class_subprogram[sp_idx].class_goto_list:
                if cg2.label==self.class_subprogram[sp_idx].exit_label:
                    #s = re.sub(r"(?i)go\s*to\d+","stop",self.class_subprogram[idx].ftext[cg2.line_number]
                    #print(self.class_subprogram[idx].ftext[cg2.line_number])
                    s = re.sub(r"(?i)go\s*to\s*\d+","return !add by @creaqi to replace goto end of subprogram",\
                        self.class_subprogram[sp_idx].ftext[cg2.line_number])
                    self.class_subprogram[sp_idx].ftext[cg2.line_number] = s
                    #print(self.class_subprogram[idx].ftext[cg2.line_number])
                    ng2 = self.get_num_goto_modified()
                    self.set_num_goto_modified(ng2+1)
        #self.update_text()

    def get_label_line_number_id(self,cls_sp,id):
        for lb_idx,cls_lb in enumerate(cls_sp.class_label_list):
            if cls_lb.get_label_id() == id:
                return cls_lb.line_number

    def search_be4_mark(self, parttern , line):
        if re.search(parttern, line):
            rres = re.search(parttern, line)
            r_p_s = rres.start()
            if re.search(r"!", line) is not None:
                mres = re.search(r"!", line)
                m_p_s = mres.start()
                if m_p_s < r_p_s:
                    return None
                elif m_p_s > r_p_s:
                    return rres
                elif m_p_s ==r_p_s:
                    print("debug147","error in search_be4_mark",line)
                    exit(-1)
            else:
                return rres
        else:
            return None
            print("debug149","error in search_be4_mark",line)
            exit(-1)

    #eliminate the do label
    def modify_do_label(self):
        pdo = r"(?<=do)\s+\d+\s*"
        pdol = r"(?<=)"
        patdo = re.compile(pdo)
        print("in function modify_do_label...")

        #self.update_text()
        for sp_idx in range(self.start_idx,self.end_idx + 1):
            self.class_subprogram[sp_idx].update_exit_label()
            self.class_subprogram[sp_idx].update_label()
            self.class_subprogram[sp_idx].update_goto_statement_start()
            self.class_subprogram[sp_idx].update_goto_statement_end()
            self.class_subprogram[sp_idx].update_do_label_statement()
            #print("-------------------{0}-----------".format(self.function_names[sp_idx]))
            lb_idx = -1
            for idx, cdo in enumerate(self.class_subprogram[sp_idx].class_do_list):
                
                cdo = self.class_subprogram[sp_idx].class_do_list[idx]
                if self.class_subprogram[sp_idx].fname == "SNSQ":
                    #print(self.class_subprogram[sp_idx].ftext[cdo.get_start_line_number()])
                    pass
                #print("do_list",cdo,cdo.do_label,cdo.start_line_number,cdo.end_line_number)
                if cdo.do_label <= 0:
                    #print("return from : "+str(cdo.do_label))
                    continue
                
                offset = 0
                have_goto = False
                lb_idx = lb_idx + 1
                do_ln = cdo.get_start_line_number()
                enddo_ln = cdo.get_end_line_number()
                search_start = cdo.get_start_line_number() #- self.class_subprogram[sp_idx].start_line_number
                search_end   = cdo.get_end_line_number() #- self.class_subprogram[sp_idx].start_line_number
                #print("zgy: "+str(search_start),str(search_end))
                #print("zgy:"+self.class_subprogram[sp_idx].ftext[search_start]+" | "+self.class_subprogram[sp_idx].ftext[search_end])
                #print()
                # if lb_idx != len(self.class_subprogram[sp_idx].class_do_list)-1:
                #     self.class_subprogram[sp_idx].class_do_list[idx+1].end_line_number += 1 + lb_idx
                #     self.class_subprogram[sp_idx].class_do_list[idx+1].start_line_number -= 1 + lb_idx

                # if lb_idx != 0:
                #     self.class_subprogram[sp_idx].class_do_list[idx].end_line_number +=  lb_idx  
                #     self.class_subprogram[sp_idx].class_do_list[idx].start_line_number -=  lb_idx 
                # search_start = self.class_subprogram[sp_idx].class_do_list[idx].start_line_number
                # search_end = self.class_subprogram[sp_idx].class_do_list[idx].end_line_number

                if self.class_subprogram[sp_idx].fname == "wrout1":
                    #print("zgy: "+str(search_start),str(search_end))
                    #print("zgy:"+self.class_subprogram[sp_idx].ftext[search_start]+" | "+self.class_subprogram[sp_idx].ftext[search_end])
                    pass
                #
                #print("zgy:"+self.class_subprogram[sp_idx].ftext[search_start])
                for i in range(search_start,search_end + 1 ):
                    #search downward to find the right start_line_number
                    if re.search(r"(?i)do\s+\d+",self.class_subprogram[sp_idx].ftext[i]):
                        if re.search(str(cdo.do_label),self.class_subprogram[sp_idx].ftext[i]):
                            #search_start = i
                            pass
                    
                    #must perform this , or the result is not correct !!!
                    if re.search(r"(?i)go\s*to\s+",self.class_subprogram[sp_idx].ftext[i]):
                        have_goto = True
                        #print(self.class_subprogram[sp_idx].ftext[i])
                        pass
                # if have_goto == True:
                #     #print("have goto in do statement")
                #     continue
                if self.class_subprogram[sp_idx].not_goto_this_label(cdo.get_label_id()) is False:
                    continue
                else:                   
                    self.class_subprogram[sp_idx].ftext.insert(enddo_ln,"      enddo !add by @creaqi {0}".format(cdo.do_label))
                    self.class_subprogram[sp_idx].update_line_number(enddo_ln)
                    self.class_subprogram[sp_idx].ftext[search_start] = \
                            re.sub(r"(?i)(?<=do)\s*\d+","",self.class_subprogram[sp_idx].ftext[search_start])+\
                                "! add by @creaqi do label {0}".format(cdo.do_label)
                    
                    # #print(search_start,search_end)
                    # for i in range(search_start,search_end + 1 ):
                    #     #print(self.ftext[i])
                    #     if re.search(r"(?i)\s*\d+\s*continue",self.class_subprogram[sp_idx].ftext[i]):
                    #         #print(self.class_subprogram[sp_idx].ftext[i])
                    #         #self.ftext.pop(i)
                    #         pass
                    #         if re.search(cdo.do_label,self.class_subprogram[sp_idx].ftext[i][0:6]):
                    #             self.class_subprogram[sp_idx].ftext.insert(i,"      enddo !add by @creaqi {0}".format(cdo.do_label))
                    #         self.class_subprogram[sp_idx].update_line_number(i)
                    #         #     break
                    

                    # self.class_subprogram[sp_idx].ftext[search_start] = \
                    #         re.sub(r"(?i)(?<=do)\s*\d+","",self.class_subprogram[sp_idx].ftext[search_start])+\
                    #             "! add by @creaqi do label {0}".format(cdo.do_label)

                    #self.ftext[search_end]   = re.sub(r"(?i)")
                    #r = re.sub(r"^\s{0,5}\d{0,5}\s*continue\s*","")
                    #print("sss",self.ftext[search_end+idx])
                    #print(self.ftext[search_start],self.ftext[search_end])

                

        #self.update_text()

    def modify_goto_statement(self):
        #self.update_text()
        print("in function modify_goto_statement...")
        for sp_idx in range(self.start_idx,self.end_idx + 1):
            print(self.function_names[sp_idx])
            self.class_subprogram[sp_idx].update_exit_label()
            self.class_subprogram[sp_idx].update_label()
            self.class_subprogram[sp_idx].update_goto_statement_start()
            self.class_subprogram[sp_idx].update_goto_statement_end()
            self.class_subprogram[sp_idx].update_do_label_statement()
            #self.class_subprogram[sp_idx].update_if_statement()
            
            if self.class_subprogram[sp_idx].fname == "XERPRN":
                for cg2 in self.class_subprogram[sp_idx].class_goto_list:
                    #print("zgy: ",len(cg2.get_label_id()),cg2.get_start_line_number(),cg2.get_end_line_number())
                    #print("zgy: "+str(len(self.class_subprogram[sp_idx].class_goto_list)))
                    pass
            
            
            for g2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
                cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                
                for clb_idx,clb in enumerate(self.class_subprogram[sp_idx].class_label_list):
                    clb = self.class_subprogram[sp_idx].class_label_list[clb_idx]
                    clb_lb_id = clb.get_label_id()
                    cg2_lb_id = cg2.get_label_id()

                    if clb_lb_id == cg2_lb_id:
                        
                        clb_ln = clb.get_line_number()
                        cg2_sl = cg2.get_start_line_number()
                        cg2_el = cg2.get_end_line_number()
                        
                        if clb_ln != cg2_el:
                            print("error in function {0} goto {1}".format(self.class_subprogram[sp_idx].fname,clb_lb_id))
                            exit()

                        
                        #global insert_bias_enddo_ln
                        #print(self.class_subprogram[sp_idx].ftext[cg2_sl])
                        # only process the do-enddo of goto
                        if clb_ln < cg2_sl:
                            # if self.class_subprogram[sp_idx].fname == "XERPRN":
                            #     print("zgy",cg2_lb_id)
                            #print("clb_ln,cg2_sl,cg2_el: ",clb_ln,cg2_sl,cg2_el)
                            # print("line se:",self.class_subprogram[sp_idx].ftext[cg2_sl],self.class_subprogram[sp_idx].ftext[cg2_el])

                            l2return = False
                            s2return = []

                            for j in range(clb_ln, len(self.class_subprogram[sp_idx].ftext)):
                                line = self.class_subprogram[sp_idx].ftext[j]
                                s2return.append(line)

                                if self.search_be4_mark(r"(?i)go\s*to\s*\d+",line):
                                    break

                                if self.search_be4_mark(r"(?i)return",line):
                                    l2return = True
                                    break

                            if l2return == True:
                                print("debug150",self.class_subprogram[sp_idx].ftext[cg2_sl])
                                print("debug151", s2return)

                                g2_sl_text = self.class_subprogram[sp_idx].ftext[cg2_sl]
                                self.class_subprogram[sp_idx].ftext[cg2_sl] = "!"+g2_sl_text+"comment by @creaqi"
                                for i, line in enumerate(s2return):
                                    
                                    self.class_subprogram[sp_idx].ftext.insert(cg2_sl + i, line)
                                    self.class_subprogram[sp_idx].update_line_number(cg2_sl + i)
                                continue


                            insert_bias_enddo_ln = cg2_sl
                            g2_content = self.class_subprogram[sp_idx].ftext[cg2_sl]
                            
                            if re.search(r"(?i)\s*go\s*to\s*\d+", g2_content):
                                
                                if self.class_subprogram[sp_idx].fname == "SNSQ":
                                    for i in range(clb_ln,cg2_sl+1):
                                        #print("debug8",self.class_subprogram[sp_idx].ftext[i])
                                        pass
                                        #print("debug8",cg2_lb_id)
                                # should judge if the do-enddo statement contains unmatch if
                                #print("a")
                                if self.class_subprogram[sp_idx].is_doenddo_contains_um_if(clb_ln,cg2_sl):
                                    #print(g2_content)
                                    if self.class_subprogram[sp_idx].fname == "SNSQ":
                                        print("debug9",clb_lb_id)
                                    endif_line_number = self.class_subprogram[sp_idx].get_endif_line_number(clb_ln,cg2_sl)
                                    insert_bias_enddo_ln = endif_line_number
                                    
                                    #print("zgy",self.class_subprogram[sp_idx].ftext[endif_line_number])
                                    #print("{1} True in function {0}...".format(self.class_subprogram[sp_idx].fname,clb_lb_id))
                                    #continue
                                
                                    

                                #print("unconditional goto: ",self.class_subprogram[sp_idx].ftext[cg2_sl],self.class_subprogram[sp_idx].ftext[cg2_el])
                                current_date = datetime.datetime.now()
                                skeleton_do = self.get_space(6) + "do ! insert do to replace label {0}, add by @creaqi {1}"
                                #print("insert_bias_enddo_ln",insert_bias_enddo_ln,cg2_sl)
                                if insert_bias_enddo_ln == cg2_sl:
                                    #print("yesssssssssssssss")
                                    skeleton_enddo = self.get_space(6) + "enddo ! insert enddo to replace goto [{0}, add by @creaqi {1}"
                                    insert_enddo_statement = skeleton_enddo.format(clb_lb_id,current_date)
                                else:
                                    #print("nonnnnnnnnnnnnnnnnnn")
                                    skeleton_enddo = self.get_space(6) + \
                                        "enddo ! insert enddo to replace goto [{0} (shift down {1}), add by @creaqi {2}"
                                    insert_enddo_statement = skeleton_enddo.format(clb_lb_id,insert_bias_enddo_ln-cg2_sl,current_date)
                                
                                insert_do_statement = skeleton_do.format(clb_lb_id,current_date)
                                
                                #print(insert_do_statement)
                                g2_sub = self.get_space(6)+r"! goto {0} add by @creaqi {1}".format(clb_lb_id,current_date)
                                self.class_subprogram[sp_idx].ftext[cg2_sl] = g2_sub 
                                self.class_subprogram[sp_idx].ftext.insert(insert_bias_enddo_ln,insert_enddo_statement)
                                self.class_subprogram[sp_idx].update_line_number(insert_bias_enddo_ln)                             
                                self.class_subprogram[sp_idx].ftext.insert(clb_ln,insert_do_statement)
                                self.class_subprogram[sp_idx].update_line_number(clb_ln)
                                
                                cg2_sl = self.class_subprogram[sp_idx].class_goto_list[g2idx].get_start_line_number()
                                ng2 = self.get_num_goto_modified()
                                self.set_num_goto_modified(ng2+1)
                                #print(self.class_subprogram[sp_idx].ftext[cg2_sl])
                                #TODO should add the do statement to class_do_list
                                cdo_idx = len(self.class_subprogram[sp_idx].class_do_list)
                                cdo_start_ln = clb_ln
                                cdo_end_ln = insert_bias_enddo_ln
                                #if self.class_subprogram[sp_idx].fname == "tiblset":
                                #print("debug36",self.class_subprogram[sp_idx].ftext[cdo_end_ln + 1])
                                # -998 add in modify_goto_statement
                                cdo = class_do(cdo_idx,cdo_start_ln,cdo_end_ln + 1,-998,0)
                                self.class_subprogram[sp_idx].class_do_list.append(cdo)
                                
                                
                            else:
                                #print("conditional goto: ",self.class_subprogram[sp_idx].ftext[cg2_sl],self.class_subprogram[sp_idx].ftext[cg2_el])
                                pass
            self.modify_goto_cycle_exit(sp_idx)
            self.modify_goto_return(sp_idx)
            self.modify_goto_pure(sp_idx)
            
            self.modify_goto_related_if(sp_idx)
            self.modify_goto_with_mix_if_do(sp_idx)
            self.class_subprogram[sp_idx].modify_computed_goto()
            
            #self.modify_goto_sl_lt_el(sp_idx)

            
        #self.update_text()  

    def modify_goto_cycle_exit(self,sp_idx):
        #print(self.class_subprogram[sp_idx].class_do_list)
        self.class_subprogram[sp_idx].update_do_label_statement()
        self.class_subprogram[sp_idx].update_goto_statement()
        self.class_subprogram[sp_idx].update_if_statement()
        sp_name = self.class_subprogram[sp_idx].fname

        for g2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
            cg2_lb_id = cg2.get_label_id()
            for cd_idx, cdo in enumerate(self.class_subprogram[sp_idx].class_do_list):
                #print(cdo.start_line_number,cdo.end_line_number,cg2.start_line_number)
                # if self.class_subprogram[sp_idx].fname == "SNSQ":
                #     cg2id = cg2.get_label_id()
                #     print("SNSQ: ",cg2id,self.class_subprogram[sp_idx].ftext[cg2.get_start_line_number()])

                cdo_do_ln    = cdo.get_start_line_number()
                cdo_enddo_ln = cdo.get_end_line_number()

                cdo_sl = cdo.get_start_line_number()
                cdo_el = cdo.get_end_line_number()
                cg2_sl = cg2.get_start_line_number()
                # judge if the goto statement in do label statement
                # do gs ge enddo        ------> untouched
                # do gs ed ge           ------> exit
                # do1 do2 gs ed1 ed2 ge ------> exit label
                # do1 gs1 enddo         ------> cycle
                if self.is_in_range(cdo_sl, cdo_el, cg2_sl):

                    cdo_sl = cdo.get_start_line_number()
                    cdo_el = cdo.get_end_line_number()
                    cg2_sl = cg2.get_start_line_number()
                    cg2_el = cg2.get_end_line_number()
                    if self.is_in_range(cdo_sl, cdo_el, cg2_el):
                        #continue
                        pass
                        #continue
                    
                    #should cycle
                    #print("debug14",cg2.label , cdo.do_label)
                    # do1 gs1 enddo ------> cycle
                    if int(cg2.label) == int(cdo.do_label):
                        if self.class_subprogram[sp_idx].has_um_do_in_range(cdo_sl, cg2_sl) is True:
                            print("need to flag cycle!")
                            continue
                        else:
                            #print("debug1: ",self.class_subprogram[sp_idx].fname)
                            self.class_subprogram[sp_idx].ftext[cg2.start_line_number]=\
                                re.sub(r"(?i)go\s*to\s*\d+",\
                                    r"cycle ! add by @creaqi replace goto do label with cycle goto {0} in {1}".format(cg2.label,sp_name),\
                                        self.class_subprogram[sp_idx].ftext[cg2.start_line_number])
                    #print(self.class_subprogram[sp_idx].ftext[cg2.start_line_number])

                    # do do gotos gotoe enddo enddo
                    # do gs ge enddo ------> untouched
                    elif is_in_range(cdo_sl, cdo_el, cg2_el):
                        # if self.class_subprogram[sp_idx].has_um_do_in_range(cdo_sl, cg2_sl) is True:
                        #     pass
                        continue
                    
                    # do gs ed ge           ------> exit
                    # do1 do2 gs ed1 ed2 ge ------> exit label
                    elif is_in_range(cg2_sl, cg2_el, cdo_el) is True:
                        cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                        cdo = self.class_subprogram[sp_idx].class_do_list[cd_idx]
                        #print("debug35",self.class_subprogram[sp_idx].ftext[cg2.get_end_line_number()])
                        len_do, dostk = self.class_subprogram[sp_idx].has_um_enddo_in_range(cg2, cdo)
                        if len_do == 0:
                            print("match well!")
                            continue
                        elif len_do == 1:
                            #print("one match ")
                            cdo_top = dostk[0]
                            cdo_sl = cdo.get_start_line_number()
                            if self.has_statement(cdo_el,cg2_el,self.class_subprogram[sp_idx]):
                                #print("debug31",self.class_subprogram[sp_idx].ftext[cg2_sl])
                                continue
                            else:
                                self.class_subprogram[sp_idx].ftext[cg2_sl]= \
                                    re.sub(r"(?i)go\s*to\s*\d+","exit ! add by @creaqi break the loop goto {0} in {1} with len equal {2}".format(cg2.label,sp_name,len_do),\
                                        self.class_subprogram[sp_idx].ftext[cg2_sl])
                                #print("debug30",self.class_subprogram[sp_idx].ftext[cdo_sl],self.class_subprogram[sp_idx].ftext[cg2_sl])
                        elif len_do > 1:
                            continue
                            min_idx = 99999
                            min_ln = 999999
                            obj = None
                            for i, t_cdo in enumerate(dostk):
                                td_sl = t_cdo.get_start_line_number()
                                #print("debug34",td_sl,self.class_subprogram[sp_idx].ftext[td_sl])
                                if td_sl < min_ln:
                                    min_ln = td_sl
                                    min_idx = i
                                    obj = t_cdo

                            #print("debug37",obj.get_start_line_number())
                            cdo_top = obj
                            cdo_top_sl = cdo_top.get_start_line_number()
                            cdo_top_el = cdo_top.get_end_line_number()
                            
                            
                            do_line = self.class_subprogram[sp_idx].ftext[cdo_top_sl]
                            enddo_line = self.class_subprogram[sp_idx].ftext[cdo_top_el]

                            do_label_name = "do_label{0}".format(cg2_lb_id)
                            enddo_label_name = "enddo {0}".format(do_label_name)
                            if re.search(r"(?i)do_label{0}: ".format(cg2_lb_id),do_line) is None:
                                self.class_subprogram[sp_idx].ftext[cdo_top_sl] = "    do_label{0}: ".format(cg2_lb_id) + do_line

                            if re.search(r"(?i)do_label{0}: ".format(cg2_lb_id),enddo_line) is None:
                                self.class_subprogram[sp_idx].ftext[cdo_top_el] = "    enddo do_label{0} ! add by @creaqi".format(cg2_lb_id)
                            
                            
                            #print("debug32",self.class_subprogram[sp_idx].ftext[cdo_top_sl])
                            vda_sln = self.class_subprogram[sp_idx].get_vda_eln()
                            varible_name = "lgoto{0}_{1}".format(cg2_lb_id, cg2.get_idx())
                            self.class_subprogram[sp_idx].ftext.insert(cdo_top_el + 1, "      if(.not.{0}) then ".format(varible_name))
                            self.class_subprogram[sp_idx].update_line_number(cdo_top_el + 1)
                            
                            cg2_el = self.class_subprogram[sp_idx].class_goto_list[g2idx].get_end_line_number()
                            self.class_subprogram[sp_idx].ftext.insert(cg2_el, "      endif ! exit to here, add by @creaqi".format(varible_name))
                            self.class_subprogram[sp_idx].update_line_number(cg2_el)

                            self.class_subprogram[sp_idx].ftext.insert(vda_sln + 1,\
                                 "logical :: lgoto{0}_{1}= .false. ! varible add by @creaqi".format(cg2_lb_id, cg2.get_idx()))
                            self.class_subprogram[sp_idx].update_line_number(vda_sln + 1)
                            
                            cg2_sl = self.class_subprogram[sp_idx].class_goto_list[g2idx].get_start_line_number()
                            cg2_el = self.class_subprogram[sp_idx].class_goto_list[g2idx].get_end_line_number()
                            cdo_sl = self.class_subprogram[sp_idx].class_do_list[cd_idx].get_start_line_number()
                            cdo_el = self.class_subprogram[sp_idx].class_do_list[cd_idx].get_end_line_number()
                            #print(self.class_subprogram[sp_idx].ftext[cg2_sl])
                            res = re.search(r"(?i)go\s*to",self.class_subprogram[sp_idx].ftext[cg2_sl])
                            start_pos = res.start()
                            line = self.class_subprogram[sp_idx].ftext[cg2_sl]
                            #print("debug33",re.sub(r"(?i)go\s*to\s*\d+","then",line))
                            self.class_subprogram[sp_idx].ftext[cg2_sl] = re.sub(r"(?i)go\s*to\s*\d+","then",line)
                            self.class_subprogram[sp_idx].ftext.insert(cg2_sl+1,"      {0} = .true. ".format(varible_name))
                            self.class_subprogram[sp_idx].update_line_number(cg2_sl+1)
                            self.class_subprogram[sp_idx].ftext.insert(cg2_sl+2,"      exit {0}".format(do_label_name))
                            self.class_subprogram[sp_idx].update_line_number(cg2_sl+2)
                            self.class_subprogram[sp_idx].ftext.insert(cg2_sl+3,"      endif")
                            self.class_subprogram[sp_idx].update_line_number(cg2_sl+3)
                            cite_idx = len(self.class_subprogram[sp_idx].class_if_then_endif_list)
                            # -664 means add in function modify_goto_cycle_exit
                            cite = class_if(cite_idx,cg2_sl,cg2_sl+3,1,True,-664)
                            self.class_subprogram[sp_idx].class_if_then_endif_list.append(cite)
                            


                            
                            #self.class_subprogram[sp_idx].ftext[cg2_sl] = re.sub(r"(?i)go\s*to\s*\d+","{0} = .true. ".format(varible_name),line)
                            #self.class_subprogram[sp_idx].ftext[cg2_sl].insert(cg2_sl,"{0} = .true. ".format(varible_name))
                            
                            #self.class_subprogram[sp_idx].update_line_number(cg2_sl)
                            break
                    else:
                        print("debug39","???")

                            #print("debug29",self.class_subprogram[sp_idx].ftext[cdo_sl])
                        # # do gs ed ge           ------> exit
                        # if self.class_subprogram[sp_idx].has_um_enddo_in_range(cg2, cdo) is True:
                        #     print("debug24",self.class_subprogram[sp_idx].ftext[cg2_sl])
                        # else:
                        #     print("debug25",self.class_subprogram[sp_idx].ftext[cg2_sl])
                    # do1 do2 gs ed1 ed2 ge ------> exit label
                    #elif is_in_range()
    
                        #TODO exit bug exist!!!
                        # if self.class_subprogram[sp_idx].fname == "SNSQ":
                        #     cg2id = cg2.get_label_id()
                        #     if cg2id == "32":
                        #         #print("debug5: ",cg2.end_line_number,cdo.end_line_number+1)
                        #         for i in range(cg2.end_line_number, cdo.end_line_number + 1):
                        #             #print("debug4: "+self.class_subprogram[sp_idx].ftext[i])
                        #             pass
                        #         #print("debug3: ",cg2id,self.class_subprogram[sp_idx].ftext[cg2.get_start_line_number()]) 
                        
                        # clb_ln = self.class_subprogram[sp_idx].get_label_line_number_by_label_id(cg2_lb_id)
                        # #if should_add_exit(cdo_enddo_ln,clb_ln)
                        # if self.has_statement(cdo.end_line_number,cg2.end_line_number,self.class_subprogram[sp_idx]):
                        #     #print("")
                        #     #print("need flag label {0} in {1} ".format(cg2.get_label_id(), self.class_subprogram[sp_idx].fname))
                        #     continue
                        # else:
                        #     # if self.class_subprogram[sp_idx].fname == "SNSQ":
                        #     #     cg2id = cg2.get_label_id()
                        #     #     print("SNSQ: ",cg2id,self.class_subprogram[sp_idx].ftext[cg2.get_start_line_number()])
                        #     self.class_subprogram[sp_idx].ftext[cg2.start_line_number]= \
                        #         re.sub(r"(?i)go\s*to\s*\d+","exit ! add by @creaqi break the loop goto {0} in {1}".format(cg2.label,sp_name),\
                        #             self.class_subprogram[sp_idx].ftext[cg2.start_line_number])
                        #     #print(self.class_subprogram[sp_idx].ftext[cg2.start_line_number])
                        #     ng2 = self.get_num_goto_modified()
                        #     self.set_num_goto_modified(ng2+1)


    def modify_goto_sl_lt_el(self,sp_idx):
        self.class_subprogram[sp_idx].update_do_label_statement()
        self.class_subprogram[sp_idx].update_goto_statement()
        self.class_subprogram[sp_idx].update_if_statement()
        # self.class_subprogram[sp_idx].get_class_if_list()
        # self.class_subprogram[sp_idx].get_class_endif_list()
        # self.class_subprogram[sp_idx].update_if_statement()
        cnt_insert_endif = 0
        len_cg2_list = len(self.class_subprogram[sp_idx].class_goto_list)
        len_clb_list = len(self.class_subprogram[sp_idx].class_label_list)
        for g2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
            if g2idx > len(self.class_subprogram[sp_idx].class_goto_list):
                break

            t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
            cg2_sl = t_cg2.get_start_line_number()
            cg2_el = t_cg2.get_end_line_number()
            line = self.class_subprogram[sp_idx].ftext[cg2_sl]
            if self.class_subprogram[sp_idx].goto_has_modified(line) is True:
                continue
            #print("debug48", len(self.class_subprogram[sp_idx].class_goto_list))
            cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
            cg2_sl = cg2.get_start_line_number()
            cg2_el = cg2.get_end_line_number()
            #if the goto statement is in a do statement, just not process
            if self.class_subprogram[sp_idx].is_goto_in_do_statement(cg2) is True:
                if self.class_subprogram[sp_idx].fname == "SNSQ":
                    #print(cg2.label)
                    pass
                #print("is_goto_in_do_statement(cg2) is True ")
                continue
                pass
            
            
            

            #if self.class_subprogram[sp_idx].is_goto

            if self.class_subprogram[sp_idx].fname == "output":
                temp_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                #print("---------   "+self.class_subprogram[sp_idx].ftext[cg2.get_start_line_number()])
                # print("---------"+g2_condition)

            #TODO 1.should make if-then-endif a ite class  2.add method to insert the class to proper postion of list
            for clb_idx,clb in enumerate(self.class_subprogram[sp_idx].class_label_list):
                if clb.get_label_id() == cg2.get_label_id():

                    if self.class_subprogram[sp_idx].goto_in_um_if(cg2_sl,cg2_el) is True:
                        um_eif_ln = self.class_subprogram[sp_idx].get_um_endif_line_number(cg2_sl,cg2_el)
                        #print("zzz")
                        #continue
                    else:
                        #print("yyy")
                        pass

                    if self.class_subprogram[sp_idx].is_goto_in_do_statement(cg2) is True:
                        #print("is_goto_in_do_statement(cg2) is True ")
                        #continue
                        pass

                    # if self.class_subprogram[sp_idx].fname == "SNSQ":
                    #     cg2id = cg2.get_label_id()
                    #     print("debug6: ",cg2id)
                    if self.class_subprogram[sp_idx].is_do_enddo_match(cg2_sl,cg2_el) is True:
                        if self.class_subprogram[sp_idx].fname == "SNSQ":
                            cg2id = cg2.get_label_id()
                            #print("debug6: ",cg2id)
                        pass

                    if self.class_subprogram[sp_idx].is_clb_in_do(clb) is True:
                        #print("is_clb_in_do")
                        pass
                        #continue


                    # print("zzz",clb.get_label_id(),cg2.get_label_id())
                    #self.class_subprogram[sp_idx].update_label()
                    clb_ln = clb.get_line_number()
                    cg2_sl = cg2.get_start_line_number()
                    cg2_el = cg2.get_end_line_number()
                    t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                    cg2_sl = t_cg2.get_start_line_number()
                    cg2_el = t_cg2.get_end_line_number()
                    

                    # if clb_idx != len_clb_list:
                    #     self.class_subprogram[sp_idx].class_label_list[clb_idx].set_line_number(clb_ln+cnt_insert_endif+1)
                    #     self.class_subprogram[sp_idx].class_goto_list[g2idx].set_start_line_number(cg2_sl+cnt_insert_endif+1)
                    #     self.class_subprogram[sp_idx].class_goto_list[g2idx].set_end_line_number(cg2_el+cnt_insert_endif+1)
                    #clb_ln = self.class_subprogram[sp_idx].class_label_list[clb_idx].get_line_number()
                    #cg2_sl = self.class_subprogram[sp_idx].class_goto_list[g2idx].get_start_line_number()
                    # print("--------goto and label b--------{0}----{1}---{2}---".format(clb.get_label_id(),g2idx,clb_idx))
                    # print(self.class_subprogram[sp_idx].ftext[clb_ln])
                    # print(self.class_subprogram[sp_idx].ftext[cg2_sl])
                    # print("-------------------goto and label e------------------")
                    
                    if cg2_sl<clb_ln:

                        

                        # #
                        # #  ^     ^
                        # len_if, stk_if = self.class_subprogram[sp_idx].jump_out_if(cg2_sl,cg2_el)
                        # # if gs ge ed
                        # #    ^  ^ 
                        # if len_if == 0:
                        #     pass
                        
                        # # if gs ed ge
                        # #    ^     ^
                        # #if if gs xxx ed ed ge
                        # if len_if > 0 :
                        #     # for i in range(cg2_sl, cg2_el+1):
                        #     #     line = self.class_subprogram[sp_idx].ftext[i]
                        #     #     print("debug51",line)
                        #     # for i in range(cg2_sl, cg2_el + 1):
                        #     #     line = self.class_subprogram[sp_idx].ftext[i]
                        #     #     print("debug51",line)
                        #     #print("debug52",self.class_subprogram[sp_idx].ftext[cg2_sl],self.class_subprogram[sp_idx].ftext[cg2_el])
                        #     min_if_el = 99999
                        #     max_if_el = -99999
                        #     for cif in stk_if:
                        #         cif_sl = cif.get_start_line_number()
                        #         cif_el = cif.get_end_line_number()
                        #         if cif_el < min_if_el:
                        #             #print("debug51",self.class_subprogram[sp_idx].ftext[cif_el])
                        #             min_if_el = cif_el
                        #         if cif_el > max_if_el:
                        #             max_if_el = cif_el

                        #     line = self.class_subprogram[sp_idx].ftext[cg2_sl]
                        #     vda_el = self.class_subprogram[sp_idx].get_vda_eln()
                        #     cg2id = cg2.get_label_id()
                        #     variable_name = "lgoto_{0}_{1}".format(cg2id,cg2.get_idx())
                        #     insert_variable_content = "      logical :: {0} = .false.".format(variable_name)
                        #     self.class_subprogram[sp_idx].ftext.insert(vda_el + 1, insert_variable_content)
                        #     self.class_subprogram[sp_idx].update_line_number(vda_el + 1)
                        #     if re.search(r"(?i)^\s*if",line):
                        #         pass
                        #     elif re.search(r"(?i)^\s*go\s*to\s*\d+",line):
                        #         #print("debug41",line)
                        #         t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                        #         cg2_sl = t_cg2.get_start_line_number()
                        #         cg2_el = t_cg2.get_end_line_number()
                        #         # if if gs ed ed xxx ge
                        #         #print("debug50",self.class_subprogram[sp_idx].ftext[cg2_sl], min_if_el)
                        #         if self.class_subprogram[sp_idx].has_statement(max_if_el,cg2_el):
                        #             #cg2_sl = cg2_sl if 
                        #             #print("debug40",self.class_subprogram[sp_idx].ftext[cg2_sl])
                        #             # continue
                        #             #cg2_order = self.class_subprogram[sp_idx].get_same_goto_order(cg2id, g2idx)
                        #             #cg2_sl = self.class_subprogram[sp_idx].get_cg2_sl_by_id(cg2id, cg2_order)
                        #             t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                        #             cg2_sl = t_cg2.get_start_line_number()
                        #             cg2_el = t_cg2.get_end_line_number()
                        #             #print("debug43",self.class_subprogram[sp_idx].ftext[t_cg2_sl])

                        #             self.class_subprogram[sp_idx].ftext[cg2_sl] = "      {0} = .true. !".format(variable_name) + line
                        #             #min_if_el = min_if_el if min_if_el < vda_el + 1 else min_if_el + 1
                                    
                        #             # if if gs xxx ed ed xxx ge
                                    
                        #             if self.class_subprogram[sp_idx].has_statement(cg2_sl + 1, min_if_el):
                        #                 insert_if_text = "      if({0}) then ! add by @creaqi start of goto {1}".format(variable_name,cg2id)
                        #                 insert_endif_text = "      endif ! add by @creaqi end of goto {0}".format(cg2id)
                        #                 self.class_subprogram[sp_idx].ftext.insert(cg2_sl + 1, insert_if_text)
                        #                 self.class_subprogram[sp_idx].update_line_number(cg2_sl + 1)
                        #                 self.class_subprogram[sp_idx].ftext.insert(min_if_el, insert_endif_text)
                        #                 self.class_subprogram[sp_idx].update_line_number(min_if_el)
                        #                 #max_if_el = max_if_el if max_if_el > min_if_el else max_if_el + 1
                        #                 #print("debug49",max_if_el , min_if_el)
                        #             #     if max_if_el > min_if_el:
                        #             #         #print("xxxxxxxxxxxxxxxxx")
                        #             #         max_if_el += 1

                        #             # if max_if_el > vda_el + 1:
                        #             #         #print("xxxxxxxxxxxxxxxxx")
                        #             #         max_if_el += 1
                        #             insert_m_text = "      if(.not.{0}) then".format(variable_name)
                        #             self.class_subprogram[sp_idx].ftext.insert(max_if_el + 1, insert_m_text)
                        #             self.class_subprogram[sp_idx].update_line_number(max_if_el + 1)
                        #             t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                        #             cg2_sl = t_cg2.get_start_line_number()
                        #             cg2_el = t_cg2.get_end_line_number()
                        #             self.class_subprogram[sp_idx].ftext.insert(cg2_el, "      endif ! add by @creaqi max {0}".format(variable_name))
                        #             self.class_subprogram[sp_idx].update_line_number(cg2_el)
                        #             #print("debug42", self.class_subprogram[sp_idx].ftext[cg2_el])
                        #             t_cif = class_if(len_if,max_if_el + 1,cg2_el,1,True,-663)
                        #             continue


                        #         else:
                        #             print("nonono")
                        #             self.class_subprogram[sp_idx].ftext[cg2_sl] = "      {0} = .true. !".format(variable_name) + line
                        #             min_if_el = min_if_el if min_if_el < vda_el + 1 else min_if_el + 1
                        #             insert_text = "      endif ! add by @creaqi end of goto {0}".format(cg2id)
                        #             self.class_subprogram[sp_idx].ftext.insert(min_if_el, insert_text)
                        #             self.class_subprogram[sp_idx].update_line_number(min_if_el)
                        #     else:
                        #         pass
                            
                             

                        
                        # # gs if ge ed
                        # # ^     ^
                        # len_if, stk_if = self.class_subprogram[sp_idx].jump_into_if(cg2_sl,cg2_el)
                        # if len_if != 0:
                        #     continue
                        
                        t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                        cg2_sl = t_cg2.get_start_line_number()
                        cg2_el = t_cg2.get_end_line_number()

                        g2_content = self.class_subprogram[sp_idx].ftext[cg2_sl]
                        lb_content = self.class_subprogram[sp_idx].ftext[clb_ln]
                        #print(g2_content)
                        # clb_ln = clb.get_line_number()
                        # cg2_sl = cg2.get_start_line_number()
                        
                        # need search back to find the beginning of the condition line
                        if re.search(r"&",g2_content):
                            continue
                            print("find continuation: "+g2_content)
                        if re.search(r"(?i)^\s*if",g2_content) is None:
                            #print(g2_content)
                            continue
                        
                        g2_condition_sobj = re.search(r"(?i)\(.*\)",g2_content)
                        if g2_condition_sobj is not None :
                            
                            g2_condition = g2_condition_sobj.group(0)
                            
                            
                            inv_g2_condition = "(.not. "+g2_condition+")"
                            if re.search(r"(?i)if",g2_content):
                                
                                complete_g2_statement = "if"+inv_g2_condition+\
                                    " then ! add by @creaqi goto {0} in fun modify_goto_sl_lt_el".format(clb.get_label_id())
                            else:
                                continue
                                complete_g2_statement = inv_g2_condition+" then ! add by @creaqi goto {0}".format(clb.get_label_id())
                            if re.search(r"ITER",g2_content):
                                
                                #print("======: ",g2_content,cg2.get_label_id(),clb.get_label_id(),complete_g2_statement)
                                pass
                            #clb.set_line_number( clb.get_line_number() + cnt_insert_endif )

                            #cg2.set_start_line_number( cg2.get_start_line_number() + cnt_insert_endif )

                            t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                            cg2_sl = t_cg2.get_start_line_number()
                            cg2_el = t_cg2.get_end_line_number()

                            # print("-------------------goto and label b------------------")
                            # print(cnt_insert_endif)
                            # print(self.class_subprogram[sp_idx].ftext[clb_ln+cnt_insert_endif])
                            # print(self.class_subprogram[sp_idx].ftext[cg2_sl+0])
                            # print(self.class_subprogram[sp_idx].ftext[cg2_sl+cnt_insert_endif])
                            # print("-------------------goto and label e------------------")
                            
                            self.class_subprogram[sp_idx].ftext[cg2_sl] = complete_g2_statement
                            self.class_subprogram[sp_idx].ftext.insert(clb_ln," "*6+"endif !add by @creaqi label {0} modify_goto_sl_lt_el".format(clb.get_label_id()))
                            self.class_subprogram[sp_idx].update_line_number(clb_ln)
                            cnt_insert_endif = cnt_insert_endif+1
                            ng2 = self.get_num_goto_modified()
                            self.set_num_goto_modified(ng2+1)
                            len_cite_list = len(self.class_subprogram[sp_idx].class_if_then_endif_list)
                            # identity equal to -665 means should keep it
                            cite = class_if(len_cite_list,cg2_sl, clb_ln,1,True,identity = -665)
                            self.class_subprogram[sp_idx].class_if_then_endif_list.append(cite)
                            #self.class_subprogram[sp_idx].update_goto_statement()
                            #print(complete_g2_statement,self.class_subprogram[sp_idx].ftext[clb_ln])
                        #print(self.class_subprogram[sp_idx].ftext[cg2_sl],self.class_subprogram[sp_idx].ftext[clb_ln])
                    #print(clb.get_line_number(),cg2.get_start_line_number())

    def modify_goto_return(self,sp_idx):
        for g2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
            cg2_sl = cg2.get_start_line_number()
            cg2_el = cg2.get_end_line_number()

            t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
            cg2_sl = t_cg2.get_start_line_number()
            cg2_el = t_cg2.get_end_line_number()
            line = self.class_subprogram[sp_idx].ftext[cg2_sl]
            if self.class_subprogram[sp_idx].goto_has_modified(line) is True:
                continue

            if cg2_sl > cg2_el:
                print("loop occur! ")
                continue
            cg2_lb = cg2.get_label_id()
            clb = self.class_subprogram[sp_idx].get_lb_obj_by_id(cg2_lb)
            
            clb_ln = -99999
            if clb == -1:
                print("error in modify_goto_pure")
                exit(-1)
            else:
                clb_ln = clb.get_line_number()
            
            is_return_label = clb.is_return

            if is_return_label is True:
                
                cg2_sl_text = self.class_subprogram[sp_idx].ftext[cg2_sl]
                self.class_subprogram[sp_idx].ftext[cg2_sl] = re.sub(r"(?i)go\s*to\s*\d+","return ! add by @creaqi goto return clb is_return",cg2_sl_text)
                if self.class_subprogram[sp_idx].fname == "ERFDIF":
                    print("debug148",self.class_subprogram[sp_idx].ftext[cg2_sl])
                    #print("debug145")
                    #exit()

            #goto_state = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)
            state_pr, state_do, state_if = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)
            # if self.class_subprogram[sp_idx].fname == "VCOUP":
            #     print("debug60",self.class_subprogram[sp_idx].exit_label)
            if self.class_subprogram[sp_idx].fname == "slugsnp":
                #print("debug62",self.class_subprogram[sp_idx].exit_label)
                pass
            if (state_do is False and state_if is True) or state_pr is True:
                if cg2_lb == int(self.class_subprogram[sp_idx].exit_label):
                    #print("debug59",self.class_subprogram[sp_idx].ftext[cg2_sl])
                    rep_pat = r"(?i)go\s*to\s*\d+"
                    rep_text =  "      return ! add by @creaqi replace goto {0} with return"
                    rep_src = self.class_subprogram[sp_idx].ftext[cg2_sl]
                    self.class_subprogram[sp_idx].ftext[cg2_sl] = re.sub(rep_pat, rep_text, rep_src )

    def modify_goto_out_if(self, sp_idx):
        self.class_subprogram[sp_idx].update_goto_statement()
        self.class_subprogram[sp_idx].update_if_statement()
        self.class_subprogram[sp_idx].update_do_label_statement()
        for g2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
            cg2_sl = cg2.get_start_line_number()
            cg2_el = cg2.get_end_line_number()

            t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
            cg2_sl = t_cg2.get_start_line_number()
            cg2_el = t_cg2.get_end_line_number()
            line = self.class_subprogram[sp_idx].ftext[cg2_sl]
            if self.class_subprogram[sp_idx].goto_has_modified(line) is True:
                continue

            if cg2_sl > cg2_el:
                print("loop occur! ")
                continue
            cg2_lb = cg2.get_label_id()
            clb = self.class_subprogram[sp_idx].get_lb_obj_by_id(cg2_lb)
            clb_ln = -99999
            if clb == -1:
                print("error in modify_goto_pure")
                exit(-1)
            else:
                clb_ln = clb.get_line_number()
            
            #goto_state = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)
            state_pr, state_do, state_if = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)

            #print(state_pr, state_do, state_if)

            if state_pr is True:
                #print("")
                continue

            # do
            if state_do is True and state_if is False:
                #print("debug57",self.class_subprogram[sp_idx].ftext[cg2_sl])
                continue
            
            # if
            if state_do is False and state_if is True:
                #continue
                #
                #  ^     ^
                len_eif, stk_if = self.class_subprogram[sp_idx].jump_into_if(cg2_sl,cg2_el)
                # if gs ge ed
                #    ^  ^ 
                if len_eif == 0:
                    continue
                
                # if gs ed ge
                #    ^     ^
                #if if gs xxx ed ed xxx ge
                #      ^                ^
                if len_eif > 0 :
                    print("debug61",self.class_subprogram[sp_idx].ftext[cg2_sl])

    def modify_goto_with_mix_if_do(self,sp_idx):
        self.class_subprogram[sp_idx].update_goto_statement()
        self.class_subprogram[sp_idx].update_if_statement()
        self.class_subprogram[sp_idx].update_do_label_statement()
        for g2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
            sl = cg2.get_start_line_number()
            line = self.class_subprogram[sp_idx].ftext[sl]
            if self.class_subprogram[sp_idx].fname == "SNSQ":
                #print("debug111", line)
                pass
        for g2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):

            t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
            cg2_sl = t_cg2.get_start_line_number()
            cg2_el = t_cg2.get_end_line_number()
            line = self.class_subprogram[sp_idx].ftext[cg2_sl]
            if self.class_subprogram[sp_idx].goto_has_modified(line) is True:
                continue
            
            t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
            cg2_sl = t_cg2.get_start_line_number()
            cg2_el = t_cg2.get_end_line_number()
            #if self.class_subprogram[sp_idx].fname == "SNSQ":
                #print("debug110",self.class_subprogram[sp_idx].ftext[cg2_sl])
            if cg2_sl > cg2_el:
                print("loop occur! ")
                continue
            cg2_lb = cg2.get_label_id()
            clb = self.class_subprogram[sp_idx].get_lb_obj_by_id(cg2_lb)
            clb_ln = -99999
            clb_end = clb.get_line_to_end()
            
            if clb == -1:
                print("error in modify_goto_pure")
                exit(-1)
            else:
                clb_ln = clb.get_line_number()
            
            if clb_end != clb_end:
                #print("debug78",clb_ln, clb_end)
                
                t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                cg2_sl = t_cg2.get_start_line_number()
                cg2_el = t_cg2.get_end_line_number()
                cg2_sl_line = self.class_subprogram[sp_idx].ftext[cg2_sl]
                if re.search(r"(?i)^\s*if",cg2_sl_line):
                    rpt = r"(?i)\s*go\s*to\s*\d+"
                    spt = " then ! add by @creaqi goto {0}".format(cg2_lb)
                    robj = re.search(rpt, cg2_sl_line )
                    if robj:
                        gps = robj.start()
                        mps = re.search(r"!", cg2_sl_line)
                        if mps is not None:
                            if mps.start() < gps:
                                print("! before goto")
                                continue
                        
                    self.class_subprogram[sp_idx].ftext[cg2_sl] = re.sub(rpt, spt, cg2_sl_line )
                    #print("debug81", self.class_subprogram[sp_idx].ftext[cg2_sl])
                    continue
                    for i in range(0, clb_end):
                        #print()
                        self.class_subprogram[sp_idx].ftext.insert(cg2_sl + i + 1,clb.get_text(i))
                        self.class_subprogram[sp_idx].update_line_number(cg2_sl + i + 1)
                    #self.class_subprogram[sp_idx].ftext[clb_ln + clb_end + 1] 
                    self.class_subprogram[sp_idx].ftext.insert(cg2_sl + clb_end + 1,"endif ! add by @creaqi end of {0}".format(cg2_lb))
                    self.class_subprogram[sp_idx].update_line_number(cg2_sl + clb_end + 1)
                    #print("debug80",self.class_subprogram[sp_idx].ftext[clb_ln + clb_end])
                    continue


                else:
                    self.class_subprogram[sp_idx].ftext[clb_ln] = '!' + self.class_subprogram[sp_idx].ftext[clb_ln]
                    for i in range(0, clb_end):
                        
                        self.class_subprogram[sp_idx].ftext.insert(cg2_sl + i + 1,self.class_subprogram[sp_idx].ftext[clb_ln + 1 + i])
                        self.class_subprogram[sp_idx].update_line_number(cg2_sl + i + 1)
            
            #goto_state = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)
            
            state_pr, state_do, state_if = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)

            #print(state_pr, state_do, state_if)

            if state_pr is True:
                print("debug108",self.class_subprogram[sp_idx].ftext[cg2_sl])
                #print("")
                continue

            if state_do is True and state_if is True:
                
                gres = re.search(r"(?i)go\s*to\s*\d+",self.class_subprogram[sp_idx].ftext[cg2_sl])
                if gres:
                    gps = gres.start()
                    mres = re.search(r"!",self.class_subprogram[sp_idx].ftext[cg2_sl][0:gps])
                    if mres is None:
                        offset_line = 0
                        di_obj_list = self.class_subprogram[sp_idx].goto_if_type(cg2_sl, cg2_el)
                        len_di_obj_list = len(di_obj_list)
                        if len_di_obj_list < 2:
                            print("error in modify_goto_in_if")
                            exit()
                        #print("debug85",len_di_obj_list)
                        if len_di_obj_list == 2:
                            
                            sif_0 = isinstance(di_obj_list[0], class_if)
                            sif_1 = isinstance(di_obj_list[1], class_if)
                            #print("debug89",sif_0,sif_1)
                            if sif_0 and not sif_1:
                                t_if_el = di_obj_list[0].get_end_line_number()
                                t_do_el = di_obj_list[1].get_end_line_number()
                                #print("debug89","should exit directly")
                                #if self.class_subprogram[sp_idx].has_statement(t_if_el)
                            if not sif_0 and sif_1:
                                t_do_el = di_obj_list[0].get_end_line_number()
                                t_if_el = di_obj_list[1].get_end_line_number()
                                t_if_idx = di_obj_list[1].get_idx() 
                                if self.class_subprogram[sp_idx].has_statement(t_do_el,t_if_el):
                                    pass
                                    #print("debug88", self.class_subprogram[sp_idx].ftext[cg2_sl])
                                else:
                                    #print("debug90", self.class_subprogram[sp_idx].ftext[cg2_sl])
                                    cg2_sl_text = self.class_subprogram[sp_idx].ftext[cg2_sl]
                                    if re.search(r"(?i)go\s*to\s*\d+", cg2_sl_text):
                                        res = re.search(r"(?i)go\s*to\s*\d+", cg2_sl_text)
                                        g2_s_p = res.start()
                                        vda_el = self.class_subprogram[sp_idx].get_vda_eln()
                                        variable_name = "lgoto{0}_{1}".format(cg2_lb,t_if_idx)
                                        self.class_subprogram[sp_idx].ftext.insert(vda_el, "logical :: {0}=.false.".format(variable_name))
                                        self.class_subprogram[sp_idx].update_line_number(vda_el)
                                        t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                        cg2_sl = t_cg2.get_start_line_number()
                                        cg2_el = t_cg2.get_end_line_number()
                                        rep_sl_text = cg2_sl_text[0:g2_s_p]+"then ! " + cg2_sl_text[g2_s_p:-1]+"add by @creaqi with len_di_obj_list == 2"
                                        self.class_subprogram[sp_idx].ftext[cg2_sl] = rep_sl_text
                                        self.class_subprogram[sp_idx].ftext.insert(cg2_sl+1, "      {0}=.true.".format(variable_name))
                                        self.class_subprogram[sp_idx].update_line_number(cg2_sl+1)
                                        self.class_subprogram[sp_idx].ftext.insert(cg2_sl+2, "      exit ! add by @creaqi ")
                                        self.class_subprogram[sp_idx].update_line_number(cg2_sl+2)
                                        self.class_subprogram[sp_idx].ftext.insert(cg2_sl+3, "      endif ! add by @creaqi ")
                                        self.class_subprogram[sp_idx].update_line_number(cg2_sl+3)
                                        insert_text = "      if(.not.{0}) then ! start of {1} add by @creaqi ".format(variable_name,variable_name)
                                        self.class_subprogram[sp_idx].ftext.insert(t_if_el+4, insert_text)
                                        self.class_subprogram[sp_idx].update_line_number(t_if_el+4)
                                        t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                        cg2_sl = t_cg2.get_start_line_number()
                                        cg2_el = t_cg2.get_end_line_number()
                                        insert_text = "      endif ! end of {0} add by @creaqi ".format(variable_name)
                                        self.class_subprogram[sp_idx].ftext.insert(cg2_el, insert_text)
                                        self.class_subprogram[sp_idx].update_line_number(cg2_el)
                                        

                                        #rep_sl_text = re.sub(r"(?i)go\sto\d")

                                    else:
                                        print("content not contain a goto cluase !", cg2_sl_text)
                                        continue

                            # for i in range(0, len_di_obj_list):
                            #     di_obj = di_obj_list[i]
                            #     di_obj_el = di_obj.get_end_line_number()
                            #     di_obj_sl = di_obj.get_start_line_number()
                            #     cg2_text = self.class_subprogram[sp_idx].ftext[cg2_sl]
                            #     #self.class_subprogram[sp_idx].has_statement(d)
                            #     print("debug84",cg2_text,di_obj_sl, di_obj_el, isinstance(di_obj,class_if))

                        # gs do do if ge
                        # gs do if do ge
                        # gs if do do ge
                        # gs do if if ge
                        # gs if do if ge
                        # gs if if do ge
                        # if only have one do, just exit it, and search 
                        
                        if len_di_obj_list > 2:
                            
                            line = self.class_subprogram[sp_idx].ftext[cg2_sl]
                            
                            if self.class_subprogram[sp_idx].line_contain_goto(line) is False:
                                continue
                            min_do_el = 99999
                            min_if_el = 99999
                            max_do_el = -99999
                            max_if_el = -99999
                            min_do_obj = None
                            min_if_obj = None
                            max_do_obj = None
                            max_if_obj = None
                            find_max_do = False
                            do_next_if_idx = -1
                            for i, di_obj in enumerate(di_obj_list):
                                if isinstance(di_obj, class_if):
                                    tt_cif_el = di_obj.get_end_line_number()
                                    if tt_cif_el < min_if_el:
                                        min_if_el = tt_cif_el
                                        min_if_obj = di_obj
                                    if tt_cif_el > max_if_el:
                                        max_if_el = tt_cif_el
                                        max_if_obj = di_obj
                                    if find_max_do == True:
                                        do_next_if_idx = i
                                        find_max_do = False
                                if isinstance(di_obj, class_do):
                                    #cnt_enddo += 1
                                    #print("")
                                    tt_cdo_el = di_obj.get_end_line_number() 
                                    #print("debug97",tt_cdo_el , min_if_el)
                                    if tt_cdo_el < min_do_el:
                                        min_do_el = tt_cdo_el
                                        min_do_obj = di_obj
                                    if tt_cdo_el > max_do_el:
                                        max_do_el = tt_cdo_el
                                        max_do_obj = di_obj
                                        find_max_do = True
                                    
                                    
                            
                            #print("debug98",do_next_if_idx)

                            # exit the last enddo, and add if 

                            #print(max_do_obj , min_do_obj)
                            #

                            vda_sl = self.class_subprogram[sp_idx].get_vda_sln()
                            vda_el = self.class_subprogram[sp_idx].get_vda_eln()

                            max_do_sl = max_do_obj.get_start_line_number()
                            max_do_el = max_do_obj.get_end_line_number()
                            min_do_sl = min_do_obj.get_start_line_number()
                            min_do_el = min_do_obj.get_end_line_number()
                            max_do_lb = max_do_obj.get_label_id()

                            max_if_sl = max_if_obj.get_start_line_number()
                            max_if_el = max_if_obj.get_end_line_number()
                            min_if_sl = min_if_obj.get_start_line_number()
                            min_if_el = min_if_obj.get_end_line_number()

                            v_name = "lgoto{0}_{1}".format(cg2_lb,g2idx)

                            max_do_sl_text = self.class_subprogram[sp_idx].ftext[max_do_sl]
                            
                            max_do_lb_id = max_do_obj.get_label_id()
                            if max_do_lb_id < 0:
                                max_do_lb_id = 44#random.randint(1,100)
                            if re.search(r"label{0}".format(max_do_lb_id), max_do_sl_text) is None:
                                self.class_subprogram[sp_idx].ftext[max_do_sl] = "   label{0}:".format(max_do_lb_id) + max_do_sl_text
                            mdlb = max_do_obj.get_label_id()
                            if self.class_subprogram[sp_idx].safe_do_label(mdlb) is True:
                                mdl = self.class_subprogram[sp_idx].ftext[max_do_sl]
                                self.class_subprogram[sp_idx].ftext[max_do_sl] = re.sub(r"(?i)do\s*\d+", "do ", mdl)
                                self.class_subprogram[sp_idx].ftext[max_do_el] = "      enddo label{1}! {0} continue flag 2".format(max_do_lb_id, max_do_lb_id)
                            else:
                                print("need handle this situation")
                                exit(-1)
                            
                            max_do_el_text = self.class_subprogram[sp_idx].ftext[max_do_el]
                            self.class_subprogram[sp_idx].ftext[max_do_el] = max_do_el_text + "   label{0}".format(max_do_lb_id)

                            cg2_sl_text = self.class_subprogram[sp_idx].ftext[cg2_sl]

                            

                            #print("debug100", cg2_sl_text )
                            if re.search(r"(?i)\Wif\W", cg2_sl_text):
                                #print("debug99", cg2_sl_text)
                                condition_obj = re.search(r"(?i)\(.*\)",cg2_sl_text)
                                if condition_obj is not None:
                                    condition = condition_obj.group(0)
                                    #print("debug100",condition)
                                else:
                                    print("error, if exists but without condition!")
                                    exit(-1)
                                dn_if_el = di_obj_list[do_next_if_idx].get_end_line_number()
                                #print("debug101",self.class_subprogram[sp_idx].ftext[max_do_el])
                                #print("debug101_1",self.class_subprogram[sp_idx].ftext[dn_if_el])
                                
                                #print("debug102",self.class_subprogram[sp_idx].ftext[dn_if_el])
                                
                                insert_cg2_sl_text = "      if(.not.{0}) then ! add by @creaqi flag 3".format(condition)
                                self.class_subprogram[sp_idx].ftext[cg2_sl] = insert_cg2_sl_text
                                cg2_sl_p1 = "      {0}=.true. ! add by @creaqi flag 3".format(v_name)
                                self.class_subprogram[sp_idx].ftext.insert(cg2_sl + 1, cg2_sl_p1)
                                self.class_subprogram[sp_idx].update_line_number(cg2_sl + 1)

                                self.class_subprogram[sp_idx].ftext.insert(cg2_sl + 2, "      exit ! add by @creaqi flag 3")
                                self.class_subprogram[sp_idx].update_line_number(cg2_sl + 2)

                                self.class_subprogram[sp_idx].ftext.insert(cg2_sl + 3, "      endif ! add by @creaqi flag 3")
                                self.class_subprogram[sp_idx].update_line_number(cg2_sl + 3)

                                dn_if_el = di_obj_list[do_next_if_idx].get_end_line_number() 

                                #continue
                                if self.class_subprogram[sp_idx].has_statement(max_do_el, dn_if_el):
                                    #print("debug101")
                                    temp_text =  "      if(.not.{0}) then ! flag 3.5 max_do_el, dn_if_el".format(v_name)
                                    self.class_subprogram[sp_idx].ftext.insert(max_do_el + 4, temp_text)
                                    self.class_subprogram[sp_idx].update_line_number(max_do_el + 4)
                                    temp_text = "      endif !add by @creaqi {0}! flag 3.5 max_do_el, dn_if_el".format(v_name)
                                    self.class_subprogram[sp_idx].ftext.insert(dn_if_el + 5, temp_text)
                                    self.class_subprogram[sp_idx].update_line_number(dn_if_el + 5)
                                else:
                                    #print("debug102")
                                    pass

                                if do_next_if_idx != len_di_obj_list:
                                    print(do_next_if_idx , len_di_obj_list)
                                    #raise NotImplementedError
                                    for i in range(do_next_if_idx, len_di_obj_list):
                                        if i != len_di_obj_list - 1:
                                            offset = i - do_next_if_idx
                                            i_obj = di_obj_list[i]
                                            i_next_obj = di_obj_list[i + 1]

                                            i_sl = i_obj.get_start_line_number() + 5 + offset
                                            i_el = i_obj.get_end_line_number() + 6 + offset

                                            in_sl = i_next_obj.get_start_line_number() + 5 + offset
                                            in_el = i_next_obj.get_end_line_number() + 6 + offset
                                            
                                            if self.class_subprogram[sp_idx].has_statement(i_el, in_el):
                                                temp_text =  "      if(.not.{0}) then !flag 4 i_el, in_el".format(v_name)
                                                self.class_subprogram[sp_idx].ftext.insert(i_el + 5 + offset, temp_text)
                                                self.class_subprogram[sp_idx].update_line_number(i_el + 5 + offset)

                                                temp_text =  "      endif ! add by @creaqi flag 4 i_el, in_el"
                                                self.class_subprogram[sp_idx].ftext.insert(i_el + 6 + offset, temp_text)
                                                self.class_subprogram[sp_idx].update_line_number(i_el + 6 + offset)
                                            else:
                                                pass

                                

                            else:
                                self.class_subprogram[sp_idx].ftext[cg2_sl] = "!" + self.class_subprogram[sp_idx].ftext[cg2_sl]
                                self.class_subprogram[sp_idx].ftext.insert( cg2_sl + 1, "      {0}=.true. !add by @creaqi flag 2".format(v_name))
                                self.class_subprogram[sp_idx].update_line_number(cg2_sl + 1)
                                self.class_subprogram[sp_idx].ftext.insert( cg2_sl + 2, "      exit label{0}! add by @creaqi flag 2".format(max_do_lb_id))
                                self.class_subprogram[sp_idx].update_line_number(cg2_sl + 2)

                                dn_if_el = di_obj_list[do_next_if_idx].get_end_line_number()
                                if self.class_subprogram[sp_idx].has_statement(max_do_el, dn_if_el):
                                    self.class_subprogram[sp_idx].ftext.insert(max_do_el + 3, "      if(.not.{0}) ! flag 1".format(v_name))
                                    self.class_subprogram[sp_idx].update_line_number(max_do_el + 3)
                                    self.class_subprogram[sp_idx].ftext.insert(dn_if_el + 4, "      endif ! add by @creaqi flag 1 ")
                                    self.class_subprogram[sp_idx].update_line_number(dn_if_el + 4)
                                else:
                                    #print("2")
                                    pass
                                
                                if do_next_if_idx == len_di_obj_list:
                                    print("3")
                                    continue







                            continue

                            if max_do_obj == min_do_obj:
                                max_do_sl = max_do_obj.get_start_line_number()
                                max_do_el = max_do_obj.get_end_line_number()
                                min_do_sl = min_do_obj.get_start_line_number()
                                min_do_el = min_do_obj.get_end_line_number()

                                max_if_sl = max_if_obj.get_start_line_number()
                                max_if_el = max_if_obj.get_end_line_number()
                                min_if_sl = min_if_obj.get_start_line_number()
                                min_if_el = min_if_obj.get_end_line_number()

                                max_do_sl_text = self.class_subprogram[sp_idx].ftext[max_do_sl]
                                self.class_subprogram[sp_idx].ftext[max_do_sl] = "   label{0}:".format(cg2_lb) + max_do_sl_text
                                max_do_el_text = self.class_subprogram[sp_idx].ftext[max_do_el]
                                self.class_subprogram[sp_idx].ftext[max_do_el] = max_do_el_text + "   label{0}:".format(cg2_lb)


                                # one enddo should find the following endif
                                print("debug94","one enddo",self.class_subprogram[sp_idx].ftext[cg2_sl])
                            
                            # 
                            elif max_if_obj == min_if_obj:
                                max_do_sl = max_do_obj.get_start_line_number()
                                max_do_el = max_do_obj.get_end_line_number()
                                min_do_sl = min_do_obj.get_start_line_number()
                                min_do_sl = min_do_obj.get_end_line_number()
                                
                                max_if_sl = max_if_obj.get_start_line_number()
                                max_if_el = max_if_obj.get_end_line_number()
                                min_if_sl = min_if_obj.get_start_line_number()
                                min_if_el = min_if_obj.get_end_line_number()
                                print("debug95","one if",self.class_subprogram[sp_idx].ftext[cg2_sl])
                            
                            else:
                                print("debug96","other",self.class_subprogram[sp_idx].ftext[cg2_sl])
                        continue
                        if len_di_obj_list == 3:
                            s1 = isinstance(di_obj_list[0], class_if)
                            s2 = isinstance(di_obj_list[1], class_if)
                            s3 = isinstance(di_obj_list[2], class_if)
                            cnt_enddo = 0
                            cnt_endif = 0
                            # print("debug90",s1,s2,s3)
                            for di_obj in di_obj_list:
                                if isinstance(di_obj, class_if):
                                    cnt_endif += 1
                                if isinstance(di_obj, class_do):
                                    cnt_enddo += 1
                            if cnt_enddo == 0 or cnt_endif == 0:
                                print("error in modify_goto_in_if")
                                exit(-1)

                            # gs do if if ge
                            # gs if do if ge
                            # gs if if do ge
                            if cnt_enddo == 1:
                                print("debug91",s1,s2,s3)
                            if cnt_enddo ==2:
                                print("debug92",s1,s2,s3)
                        #print("debug64",self.class_subprogram[sp_idx].ftext[cg2_sl])
                        if len_di_obj_list == 4 :
                            #print("debug93",self.class_subprogram[sp_idx].ftext[cg2_sl])
                            pass

            if state_do is False and state_if is True:
                print("debug106",self.class_subprogram[sp_idx].ftext[cg2_sl])

            if state_do is True and state_if is False:
                print("debug107",self.class_subprogram[sp_idx].ftext[cg2_sl])

    def modify_goto_with_multiple_if(self,sp_idx):
        for g2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
            t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
            cg2_sl = t_cg2.get_start_line_number()
            cg2_el = t_cg2.get_end_line_number()
            line = self.class_subprogram[sp_idx].ftext[cg2_sl]
            if self.class_subprogram[sp_idx].goto_has_modified(line) is True:
                continue

            if cg2_sl > cg2_el:
                print("loop occur! ")
                continue
            cg2_lb = cg2.get_label_id()
            clb = self.class_subprogram[sp_idx].get_lb_obj_by_id(cg2_lb)
            clb_ln = -99999
            clb_end = clb.get_line_to_end()
            
            if clb == -1:
                print("error in modify_goto_pure")
                exit(-1)
            else:
                clb_ln = clb.get_line_number()

            #goto_state = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)
            state_pr, state_do, state_if = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)

            #print(state_pr, state_do, state_if)
            

            if state_pr is True:
                #print("")
                continue

            if state_do is True and state_if is True:
                continue
           
            # do
            if state_do is True and state_if is False:
                gres = re.search(r"(?i)go\s*to\s*\d+",self.class_subprogram[sp_idx].ftext[cg2_sl])
                if gres:
                    gps = gres.start()
                    mres = re.search(r"!",self.class_subprogram[sp_idx].ftext[cg2_sl][0:gps])
                    if mres is None:
                        #print("debug57",self.class_subprogram[sp_idx].ftext[cg2_sl])
                        pass
                continue
            
            # if
            if state_do is False and state_if is True:
                
                #continue
                #
                #  ^     ^
                len_eif, stk_if = self.class_subprogram[sp_idx].jump_out_if(cg2_sl,cg2_el)

                for i, cif in enumerate(stk_if):
                    sl = cif.get_start_line_number()
                    el = cif.get_end_line_number()
                    sl_text = self.class_subprogram[sp_idx].ftext[sl]
                    el_text = self.class_subprogram[sp_idx].ftext[el]
                    print("debug122",sl, sl_text, el, el_text)

                if re.search(r"(?i)^\s*if",line):

                        if self.class_subprogram[sp_idx].goto_has_modified(line) is True:
                            #print("debug127", line)
                            continue
                        

                        vda_el = self.class_subprogram[sp_idx].get_vda_eln()
                        v_name = "lgoto{0}_{1}".format(cg2_lb,g2idx)

                        dv_name = "      logical :: {0} = .false.".format(v_name)

                        for i, cif in enumerate(stk_if):
                            sl = cif.get_start_line_number()
                            el = cif.get_end_line_number()
                            sl_text = self.class_subprogram[sp_idx].ftext[sl]
                            el_text = self.class_subprogram[sp_idx].ftext[el]
                            #print("debug133",sl, sl_text, el, el_text)
                        # ??????
                        #print("debug135", vda_el + 1 )

                        self.class_subprogram[sp_idx].ftext.insert(vda_el + 1, dv_name)
                        self.class_subprogram[sp_idx].update_line_number(vda_el + 1)
                        #self.class_subprogram[sp_idx].set_vda_eln(vda_el + 1)
                        self.class_subprogram[sp_idx].update_line_number_list(vda_el + 1, stk_if)

                        for i, cif in enumerate(stk_if):
                            sl = cif.get_start_line_number()
                            el = cif.get_end_line_number()
                            sl_text = self.class_subprogram[sp_idx].ftext[sl]
                            el_text = self.class_subprogram[sp_idx].ftext[el]
                            #print("debug134",sl, sl_text, el, el_text)
                        
                        # this update is not necessary, because the stk_if store the reference of class_if
                        #self.class_subprogram[sp_idx].update_line_number_list(vda_el + 1, stk_if)

                        # for i in range(0,len(stk_if)):
                        #     insert_ln = vda_el + 1
                        #     tcif_sl = stk_if[i].get_start_line_number()
                        #     tcif_el = stk_if[i].get_end_line_number()
                        #     if insert_ln < tcif_sl:
                        #         stk_if[i].set_start_line_number(tcif_sl + 1)
                        #     if insert_ln < tcif_el:
                        #         stk_if[i].set_end_line_number(tcif_el + 1)
                        
                        t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                        cg2_sl = t_cg2.get_start_line_number()
                        cg2_el = t_cg2.get_end_line_number()
                        nsub_g2_sl_text = " {0}=.true. ! add by @creaqi".format(v_name)

                        
                        
                        self.class_subprogram[sp_idx].ftext[cg2_sl] = re.sub(r"(?i)go\s*to\s*\d+", nsub_g2_sl_text, line)
                        
                        
                        # cg2's end line number 
                        len_stk_if = len(stk_if)
                        #print(len_stk_if)
                        
                        offset = 0
                        for i, cif in enumerate(stk_if):
                            
                            if i == 0:
                                
                                cif_sl = stk_if[i].get_start_line_number() 
                                cif_el = stk_if[i].get_end_line_number() 
                                cif_sl_text = self.class_subprogram[sp_idx].ftext[cif_sl]
                                cif_el_text = self.class_subprogram[sp_idx].ftext[cif_el]
                                #print("debug132", cif_el_text)
                                t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                cg2_sl = t_cg2.get_start_line_number()
                                cg2_el = t_cg2.get_end_line_number()
                                if self.class_subprogram[sp_idx].has_statement_pure(cg2_sl, cif_sl) is True:
                                    cg2_sl_p1_text = "      if(.not.{0}) then ! add by @creaqi i == 0".format(v_name)
                                    
                                    self.class_subprogram[sp_idx].ftext.insert(cg2_sl+1, cg2_sl_p1_text)
                                    self.class_subprogram[sp_idx].update_line_number(cg2_sl + 1)
                                    self.class_subprogram[sp_idx].update_line_number_list(cg2_sl + 1, stk_if)
                                    # for i in range(0,len(stk_if)):
                                    #     insert_ln = cg2_sl
                                    #     tcif_sl = stk_if[i].get_start_line_number()
                                    #     tcif_el = stk_if[i].get_end_line_number()
                                    #     if insert_ln < tcif_sl:
                                    #         stk_if[i].set_start_line_number(tcif_sl + 1)
                                    #     if insert_ln < tcif_el:
                                    #         stk_if[i].set_end_line_number(tcif_el + 1)

                                    # cif_el + 1 because insert after cg2_sl, the cg2_sl is precced cif_el
                                    # if gs if endif endif  ge 
                                    cif_sl = stk_if[i].get_start_line_number() 
                                    cif_el = stk_if[i].get_end_line_number()
                                    
                                    #print("debug131", cif_el, self.class_subprogram[sp_idx].ftext[cif_el],len_stk_if)
                                    self.class_subprogram[sp_idx].ftext.insert(cif_el , "      endif ! add by @creaqi i == 0")
                                    self.class_subprogram[sp_idx].update_line_number(cif_el )
                                    self.class_subprogram[sp_idx].update_line_number_list(cif_el, stk_if)

                                    lif = len(self.class_subprogram[sp_idx].class_if_then_endif_list)
                                    tcif = class_if(lif, cg2_sl+1, cif_el,1,True,-661)
                                    self.class_subprogram[sp_idx].class_if_then_endif_list.append(tcif)
                                    
                                    #self.class_subprogram[sp_idx].update_line_number_list(cif_el, stk_if)
                                    

                                    # for i in range(0,len(stk_if)):
                                    #     insert_ln = cif_el
                                    #     tcif_sl = stk_if[i].get_start_line_number()
                                    #     tcif_el = stk_if[i].get_end_line_number()
                                    #     if insert_ln < tcif_sl:
                                    #         stk_if[i].set_start_line_number(tcif_sl + 1)
                                    #     if insert_ln < tcif_el:
                                    #         stk_if[i].set_end_line_number(tcif_el + 1)
                                    offset = 2

                            #continue 
                            if i != len_stk_if - 1:
                                nxt_cif = stk_if[i + 1]
                                cif_sl = stk_if[i].get_start_line_number() 
                                cif_el = stk_if[i].get_end_line_number() 
                                nxt_cif_sl = nxt_cif.get_start_line_number() 
                                nxt_cif_el = nxt_cif.get_end_line_number() 

                                cif_sl_text = self.class_subprogram[sp_idx].ftext[cif_sl]
                                cif_el_text = self.class_subprogram[sp_idx].ftext[cif_el]
                                nxt_cif_sl_text = self.class_subprogram[sp_idx].ftext[nxt_cif_sl]
                                nxt_cif_el_text = self.class_subprogram[sp_idx].ftext[nxt_cif_el]
                                
                                if re.search(r"&",cif_sl_text) or re.search(r"&",cif_el_text):
                                    print("debug117","need handle this")
                                    exit(-1)
                                if re.search(r"&",nxt_cif_sl_text) or re.search(r"&",nxt_cif_el_text):
                                    print("debug117","need handle this")
                                    exit(-1)
                                
                                cst_condtion_obj = re.search(r"(?i)\(.*\)",cif_sl_text)
                                #print()
                                if cst_condtion_obj is not None:
                                    cst_cd = cst_condtion_obj.group(0)
                                else:
                                    print(r"debug118","error",cif_sl_text)
                                    exit(-1)

                                t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                cg2_sl = t_cg2.get_start_line_number()
                                cg2_el = t_cg2.get_end_line_number()
                                
                                if self.class_subprogram[sp_idx].has_statement_pure(cg2_sl, cif_sl) is True:
                                    if self.class_subprogram[sp_idx].has_statement_endif_only(cif_el, cg2_el) is True:
                                        print("debug139", i,cif_el, cg2_el)
                                        continue
                                        #exit(-1)
                                        #break
                                    else:
                                        print("debug140",i, cif_el, cg2_el)
                                        for j in range(cif_el, cg2_el):
                                            line = self.class_subprogram[sp_idx].ftext[j]
                                            #print("debug141", line)
                                    #print("debug120", cif_el, nxt_cif_el)
                                    cg2_sl_p1_text = "      if(.not.{0}) then ! add by @creaqi i != len_stk_if - 1".format(v_name)

                                    nxt_cif = stk_if[i + 1]
                                    cif_sl = stk_if[i].get_start_line_number() 
                                    cif_el = stk_if[i].get_end_line_number() 
                                    nxt_cif_sl = nxt_cif.get_start_line_number() 
                                    nxt_cif_el = nxt_cif.get_end_line_number() 
                                    
                                    self.class_subprogram[sp_idx].ftext.insert(cif_el, cg2_sl_p1_text)
                                    self.class_subprogram[sp_idx].update_line_number(cif_el)
                                    self.class_subprogram[sp_idx].update_line_number_list(cif_el, stk_if)

                                    

                                    # for i in range(0,len(stk_if)):
                                    #     insert_ln = cif_el
                                    #     tcif_sl = stk_if[i].get_start_line_number()
                                    #     tcif_el = stk_if[i].get_end_line_number()
                                    #     if insert_ln < tcif_sl:
                                    #         stk_if[i].set_start_line_number(tcif_sl + 1)
                                    #     if insert_ln < tcif_el:
                                    #         stk_if[i].set_end_line_number(tcif_el + 1)

                                    nxt_cif = stk_if[i + 1]
                                    cif_sl = stk_if[i].get_start_line_number() 
                                    cif_el = stk_if[i].get_end_line_number() 
                                    nxt_cif_sl = nxt_cif.get_start_line_number() 
                                    nxt_cif_el = nxt_cif.get_end_line_number() 


                                    self.class_subprogram[sp_idx].ftext.insert(nxt_cif_el, "      endif ! i != len_stk_if - 1")
                                    self.class_subprogram[sp_idx].update_line_number(nxt_cif_el)
                                    self.class_subprogram[sp_idx].update_line_number_list(nxt_cif_el, stk_if)

                                    lif = len(self.class_subprogram[sp_idx].class_if_then_endif_list)
                                    tcif = class_if(lif, cif_el, nxt_cif_el,1,True,-661)
                                    self.class_subprogram[sp_idx].class_if_then_endif_list.append(tcif)

                                    

                                    # for i in range(0,len(stk_if)):
                                    #     insert_ln = nxt_cif_el
                                    #     tcif_sl = stk_if[i].get_start_line_number()
                                    #     tcif_el = stk_if[i].get_end_line_number()
                                    #     if insert_ln < tcif_sl:
                                    #         stk_if[i].set_start_line_number(tcif_sl + 1)
                                    #     if insert_ln < tcif_el:
                                    #         stk_if[i].set_end_line_number(tcif_el + 1)


                                # cet_condtion_obj = re.search(r"(?i)\(.*\)",cif_el_text)
                                # if cet_condtion_obj is not None:
                                #     cet_cd = cet_condtion_obj.group(0)
                                # else:
                                #     print(r"debug118","error")
                                #     exit(-1)

                                # ncst_condtion_obj = re.search(r"(?i)\(.*\)",nxt_cif_sl_text)
                                # if ncst_condtion_obj is not None:
                                #     ncst_cd = ncst_condtion_obj.group(0)
                                # else:
                                #     print(r"debug123","error", nxt_cif_sl_text)
                                #     exit(-1)

                                # ncet_condtion_obj = re.search(r"(?i)\(.*\)",nxt_cif_el_text)
                                # if ncet_condtion_obj is not None:
                                #     ncet_cd = ncet_condtion_obj.group(0)
                                # else:
                                #     print(r"debug118","error")
                                #     exit(-1)
                            
                            
                            if i == len_stk_if - 1 :
                                cif_sl = stk_if[i].get_start_line_number() 
                                cif_el = stk_if[i].get_end_line_number() 
                                #continue
                                #print("debug121",self.class_subprogram[sp_idx].ftext[cif_el])
                                cif_sl_text = self.class_subprogram[sp_idx].ftext[cif_sl]
                                cif_el_text = self.class_subprogram[sp_idx].ftext[cif_el]
                                t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                cg2_sl = t_cg2.get_start_line_number()
                                cg2_el = t_cg2.get_end_line_number()
                                if self.class_subprogram[sp_idx].has_statement_pure(cif_el, cg2_el) is True:
                                    if self.class_subprogram[sp_idx].has_statement_endif_only(cif_el, cg2_el) is True:
                                        print("debug142", cif_el, cg2_el)
                                        for i in range(cif_el+1, cg2_el):
                                            line = self.class_subprogram[sp_idx].ftext[i]
                                            print("debug144", line)
                                        #exit(-1)
                                        continue
                                    else:
                                        print("debug143",cif_el, cg2_el)
                                    cg2_sl_p1_text = "      if(.not.{0}) then ! add by @creaqi i == len_stk_if - 1".format(v_name)

                                    cif_sl = stk_if[i].get_start_line_number() 
                                    cif_el = stk_if[i].get_end_line_number() 

                                    self.class_subprogram[sp_idx].ftext.insert(cif_el + 1, cg2_sl_p1_text)
                                    self.class_subprogram[sp_idx].update_line_number(cif_el + 1)
                                    self.class_subprogram[sp_idx].update_line_number_list(cif_el + 1, stk_if)
                                    

                                    # for i in range(0,len(stk_if)):
                                    #     insert_ln = cif_el
                                    #     tcif_sl = stk_if[i].get_start_line_number()
                                    #     tcif_el = stk_if[i].get_end_line_number()
                                    #     if insert_ln < tcif_sl:
                                    #         stk_if[i].set_start_line_number(tcif_sl + 1)
                                    #     if insert_ln < tcif_el:
                                    #         stk_if[i].set_end_line_number(tcif_el + 1)

                                    # cif_el + 1 because insert after cg2_sl, the cg2_sl is precced cif_el

                                    t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                    cg2_sl = t_cg2.get_start_line_number()
                                    cg2_el = t_cg2.get_end_line_number()

                                    cif_sl = stk_if[i].get_start_line_number() 
                                    cif_el = stk_if[i].get_end_line_number() 

                                    self.class_subprogram[sp_idx].ftext.insert(cg2_el, "      endif ! add by @creaqi i == len_stk_if - 1")
                                    self.class_subprogram[sp_idx].update_line_number(cg2_el)
                                    self.class_subprogram[sp_idx].update_line_number_list(cg2_el, stk_if)

                                    lif = len(self.class_subprogram[sp_idx].class_if_then_endif_list)
                                    tcif = class_if(lif, cif_el + 1,cg2_el,1,True,-661)
                                    self.class_subprogram[sp_idx].class_if_then_endif_list.append(tcif)

                                    

                                    # for i in range(0,len(stk_if)):
                                    #     insert_ln = cg2_el
                                    #     tcif_sl = stk_if[i].get_start_line_number()
                                    #     tcif_el = stk_if[i].get_end_line_number()
                                    #     if insert_ln < tcif_sl:
                                    #         stk_if[i].set_start_line_number(tcif_sl + 1)
                                    #     if insert_ln < tcif_el:
                                    #         stk_if[i].set_end_line_number(tcif_el + 1)
                                
                        continue

                        if self.class_subprogram[sp_idx].fname == "ERFDIF":
                            print("debug116", line)
                        #print("debug77",line)
                        continue
                    

    def modify_goto_in_if(self, sp_idx):
        # self.class_subprogram[sp_idx].update_goto_statement()
        # self.class_subprogram[sp_idx].update_if_statement()
        # self.class_subprogram[sp_idx].update_do_label_statement()
        for g2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
            t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
            cg2_sl = t_cg2.get_start_line_number()
            cg2_el = t_cg2.get_end_line_number()
            line = self.class_subprogram[sp_idx].ftext[cg2_sl]
            if self.class_subprogram[sp_idx].goto_has_modified(line) is True:
                continue

            if cg2_sl > cg2_el:
                print("loop occur! ")
                continue
            cg2_lb = cg2.get_label_id()
            clb = self.class_subprogram[sp_idx].get_lb_obj_by_id(cg2_lb)
            clb_ln = -99999
            clb_end = clb.get_line_to_end()
            
            if clb == -1:
                print("error in modify_goto_pure")
                exit(-1)
            else:
                clb_ln = clb.get_line_number()

            if clb_end != 0:
                #print("debug78",clb_ln, clb_end)
                #print("debug79",self.class_subprogram[sp_idx].ftext[cg2_sl],clb.get_label_id())
                t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                cg2_sl = t_cg2.get_start_line_number()
                cg2_el = t_cg2.get_end_line_number()
                cg2_sl_line = self.class_subprogram[sp_idx].ftext[cg2_sl]
                if re.search(r"(?i)^\s*if",cg2_sl_line):
                    rpt = r"(?i)\s*go\s*to\s*\d+"
                    spt = " then ! add by @creaqi goto {0}".format(cg2_lb)
                    robj = re.search(rpt, cg2_sl_line )
                    if robj:
                        gps = robj.start()
                        mps = re.search(r"!", cg2_sl_line)
                        if mps is not None:
                            if mps.start() < gps:
                                continue
                        
                    self.class_subprogram[sp_idx].ftext[cg2_sl] = re.sub(rpt, spt, cg2_sl_line )
                    #print("debug81", self.class_subprogram[sp_idx].ftext[cg2_sl])
                    for i in range(0, clb_end):
                        #print()
                        self.class_subprogram[sp_idx].ftext.insert(cg2_sl + i + 1,clb.get_text(i))
                        self.class_subprogram[sp_idx].update_line_number(cg2_sl + i + 1)
                    #self.class_subprogram[sp_idx].ftext[clb_ln + clb_end + 1] 
                    self.class_subprogram[sp_idx].ftext.insert(cg2_sl + clb_end + 1,"      endif ! add by @creaqi end of {0}".format(cg2_lb))
                    self.class_subprogram[sp_idx].update_line_number(cg2_sl + clb_end + 1)
                    #print("debug80",self.class_subprogram[sp_idx].ftext[clb_ln + clb_end])
                    continue


                else:
                    self.class_subprogram[sp_idx].ftext[clb_ln] = '!' + self.class_subprogram[sp_idx].ftext[clb_ln]
                    for i in range(0, clb_end):
                        
                        self.class_subprogram[sp_idx].ftext.insert(cg2_sl + i + 1,self.class_subprogram[sp_idx].ftext[clb_ln + 1 + i])
                        self.class_subprogram[sp_idx].update_line_number(cg2_sl + i + 1)
            
            #goto_state = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)
            state_pr, state_do, state_if = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)

            #print(state_pr, state_do, state_if)

            if state_pr is True:
                #print("")
                continue

            if state_do is True and state_if is True:
                continue
           
            # do
            if state_do is True and state_if is False:
                gres = re.search(r"(?i)go\s*to\s*\d+",self.class_subprogram[sp_idx].ftext[cg2_sl])
                if gres:
                    gps = gres.start()
                    mres = re.search(r"!",self.class_subprogram[sp_idx].ftext[cg2_sl][0:gps])
                    if mres is None:
                        #print("debug57",self.class_subprogram[sp_idx].ftext[cg2_sl])
                        pass
                continue
            
            # if
            if state_do is False and state_if is True:
                
                #continue
                #
                #  ^     ^
                len_eif, stk_if = self.class_subprogram[sp_idx].jump_out_if(cg2_sl,cg2_el)
                # if gs ge ed
                #    ^  ^ 
                if len_eif == 0:
                    
                    continue
                
                # if gs ed ge
                #    ^     ^
                #if if gs xxx ed ed xxx ge
                #      ^                ^
                if len_eif > 0 :
                    # for i in range(cg2_sl, cg2_el+1):
                    #     line = self.class_subprogram[sp_idx].ftext[i]
                    #     print("debug51",line)
                    # for i in range(cg2_sl, cg2_el + 1):
                    #     line = self.class_subprogram[sp_idx].ftext[i]
                    #     print("debug51",line)
                    #print("debug52",self.class_subprogram[sp_idx].ftext[cg2_sl],self.class_subprogram[sp_idx].ftext[cg2_el])

                    state_same = False
                    for cif in stk_if:
                        cif_sl = cif.get_start_line_number()
                        cif_el = cif.get_end_line_number()
                        cif_sl_text = self.class_subprogram[sp_idx].ftext[cif_el]
                        res = re.search(r"\d+",cif_sl_text)
                        # if self.class_subprogram[sp_idx].fname == "output":
                        #     print("debug63",self.class_subprogram[sp_idx].ftext[cg2_sl])
                        if res is not None:
                            cif_lb = int(res.group(0).strip())
                            if cif_lb == cg2_lb:
                                state_same = True
                            else:
                                state_same = False

                    

                    min_if_el = 99999
                    max_if_el = -99999
                    for cif in stk_if:
                        cif_sl = cif.get_start_line_number()
                        cif_el = cif.get_end_line_number()
                        if cif_el < min_if_el:
                            #print("debug51",self.class_subprogram[sp_idx].ftext[cif_el])
                            min_if_el = cif_el
                        if cif_el > max_if_el:
                            max_if_el = cif_el

                    if state_same == True:

                        line = self.class_subprogram[sp_idx].ftext[cg2_sl]
                        if re.search(r"(?i)if",line):
                            g2_condition_sobj = re.search(r"(?i)\(.*\)",line)
                            if g2_condition_sobj is not None :
                                
                                g2_condition = g2_condition_sobj.group(0)
                                
                                
                                inv_g2_condition = "(.not. "+g2_condition+")"
                                re_pat =r"(?i)go\s*to\s*\d+"
                                insert_if_text = " if {0} then ! add by @creaqi {1} state_same == True".format(inv_g2_condition,cg2_lb)
                                #insert_if_text = re.sub(,,line)
                                self.class_subprogram[sp_idx].ftext[cg2_sl] = insert_if_text
                                self.class_subprogram[sp_idx].ftext.insert(min_if_el, "       endif ! add by @creaqi {0} state_same == True".format(cg2_lb))
                                self.class_subprogram[sp_idx].update_line_number(min_if_el)

                                # for i in range(0,len(stk_if)):
                                #     insert_ln =  min_if_el
                                #     tcif_sl = stk_if[i].get_start_line_number()
                                #     tcif_el = stk_if[i].get_end_line_number()
                                #     if insert_ln < tcif_sl:
                                #         stk_if[i].set_start_line_number(tcif_sl + 1)
                                #     if insert_ln < tcif_el:
                                #         stk_if[i].set_end_line_number(tcif_el + 1)
                                
                                lif = len(self.class_subprogram[sp_idx].class_if_then_endif_list)
                                cif = class_if(lif, cg2_sl, min_if_el, 1,True, -662)
                                self.class_subprogram[sp_idx].class_if_then_endif_list.append(cif)
                        continue


                    

                    # if if gs endif endif ge
                    
                    if re.search(r"(?i)^\s*go\s*to\s*\d+",line):
                        #print("debug41",line)

                        line = self.class_subprogram[sp_idx].ftext[cg2_sl]
                        vda_el = self.class_subprogram[sp_idx].get_vda_eln()
                        cg2id = cg2.get_label_id()
                        variable_name = "lgoto_{0}_{1}".format(cg2id,cg2.get_idx())
                        fun_name = self.class_subprogram[sp_idx].fname
                        insert_variable_content = "      logical :: {0} = .false. ! add by @creaqi {1} modify_goto_in_if".format(variable_name,fun_name)
                        self.class_subprogram[sp_idx].ftext.insert(vda_el + 1, insert_variable_content)
                        self.class_subprogram[sp_idx].update_line_number(vda_el + 1)

                        # for i in range(0,len(stk_if)):
                        #     insert_ln =  vda_el + 1
                        #     tcif_sl = stk_if[i].get_start_line_number()
                        #     tcif_el = stk_if[i].get_end_line_number()
                        #     if insert_ln < tcif_sl:
                        #         stk_if[i].set_start_line_number(tcif_sl + 1)
                        #     if insert_ln < tcif_el:
                        #         stk_if[i].set_end_line_number(tcif_el + 1)

                        min_if_el += 1
                        max_if_el += 1

                        t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                        cg2_sl = t_cg2.get_start_line_number()
                        cg2_el = t_cg2.get_end_line_number()
                        
                        # if if gs ed ed xxx ge
                        #print("debug50",self.class_subprogram[sp_idx].ftext[cg2_sl], min_if_el)
                        if self.class_subprogram[sp_idx].has_statement(max_if_el,cg2_el):
                            #cg2_sl = cg2_sl if 
                            #print("debug40",self.class_subprogram[sp_idx].ftext[cg2_sl])
                            # continue
                            #cg2_order = self.class_subprogram[sp_idx].get_same_goto_order(cg2id, g2idx)
                            #cg2_sl = self.class_subprogram[sp_idx].get_cg2_sl_by_id(cg2id, cg2_order)
                            
                            #print("debug43",self.class_subprogram[sp_idx].ftext[t_cg2_sl])

                            self.class_subprogram[sp_idx].ftext[cg2_sl] = "      {0} = .true. !".format(variable_name) + line
                            #min_if_el = min_if_el if min_if_el < vda_el + 1 else min_if_el + 1
                            
                            # if if gs xxx ed ed xxx ge
                            #print("debug56",cg2_sl, min_if_el)
                            if self.class_subprogram[sp_idx].has_statement(cg2_sl, min_if_el):
                                insert_if_text = "      if({0}) then ! add by @creaqi start of goto {1} modify_goto_related_if".format(variable_name,cg2id)
                                insert_endif_text = "      endif ! add by @creaqi end of goto {0} modify_goto_related_if".format(cg2id)
                                self.class_subprogram[sp_idx].ftext.insert(cg2_sl + 1, insert_if_text)
                                self.class_subprogram[sp_idx].update_line_number(cg2_sl + 1)
                                t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                cg2_sl = t_cg2.get_start_line_number()
                                cg2_el = t_cg2.get_end_line_number()
                                self.class_subprogram[sp_idx].ftext.insert(min_if_el, insert_endif_text)
                                self.class_subprogram[sp_idx].update_line_number(min_if_el)
                                #max_if_el = max_if_el if max_if_el > min_if_el else max_if_el + 1
                                #print("debug49",max_if_el , min_if_el)
                            #     if max_if_el > min_if_el:
                            #         #print("xxxxxxxxxxxxxxxxx")
                            #         max_if_el += 1

                            # if max_if_el > vda_el + 1:
                            #         #print("xxxxxxxxxxxxxxxxx")
                            #         max_if_el += 1
                            if self.class_subprogram[sp_idx].has_statement(max_if_el, cg2_el):
                                insert_m_text = "      if(.not.{0}) then ! start second start of goto {1} modify_goto_related_if".format(variable_name,cg2_lb)
                                self.class_subprogram[sp_idx].ftext.insert(max_if_el + 1, insert_m_text)
                                self.class_subprogram[sp_idx].update_line_number(max_if_el + 1)
                                t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                cg2_sl = t_cg2.get_start_line_number()
                                cg2_el = t_cg2.get_end_line_number()
                                self.class_subprogram[sp_idx].ftext.insert(cg2_el,\
                                     "      endif ! add by @creaqi max {0} end of goto {1} modify_goto_related_if".format(variable_name,cg2_lb))
                                self.class_subprogram[sp_idx].update_line_number(cg2_el)
                                #print("debug42", self.class_subprogram[sp_idx].ftext[cg2_el])
                                t_cif = class_if(len_eif,max_if_el + 1,cg2_el,1,True,-663)
                                continue


                        else:
                            print("nonono",line)
                            # self.class_subprogram[sp_idx].ftext[cg2_sl] = "      {0} = .true. !".format(variable_name) + line
                            # insert_text = "      endif ! add by @creaqi end of goto {0}".format(cg2id)
                            # self.class_subprogram[sp_idx].ftext.insert(min_if_el, insert_text)
                            # self.class_subprogram[sp_idx].update_line_number(min_if_el)
                    else:
                        pass

    def modify_goto_related_if(self,sp_idx):
        
        self.modify_goto_in_if(sp_idx)
        self.modify_goto_with_multiple_if(sp_idx)
        #self.modify_goto_out_if(sp_idx)
        # gs if ge ed
        # ^     ^
        # len_eif, stk_if = self.class_subprogram[sp_idx].jump_into_if(cg2_sl,cg2_el)
        # if len_eif != 0:
        #     continue
        # if len_eif > 0:
        #     print("debug61",self.class_subprogram[sp_idx].ftext[cg2_sl])

        # if goto_state is True:
        #     pass

    # gs x ge  
    # x not equal to endif/enddo
    def modify_goto_pure(self,sp_idx):
        for i, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
            cg2_sl = cg2.get_start_line_number()
            cg2_el = cg2.get_end_line_number()

            t_cg2 = self.class_subprogram[sp_idx].class_goto_list[i]
            cg2_sl = t_cg2.get_start_line_number()
            cg2_el = t_cg2.get_end_line_number()
            line = self.class_subprogram[sp_idx].ftext[cg2_sl]
            if self.class_subprogram[sp_idx].goto_has_modified(line) is True:
                continue

            if cg2_sl > cg2_el:
                print("loop occur! ")
                continue
            cg2_lb = cg2.get_label_id()
            clb = self.class_subprogram[sp_idx].get_lb_obj_by_id(cg2_lb)
            clb_ln = -99999
            if clb == -1:
                print("error in modify_goto_pure")
                exit(-1)
            else:
                clb_ln = clb.get_line_number()
                
            state_pr, state_do, state_if = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)
            #goto_state = self.class_subprogram[sp_idx].goto_is_pure(cg2_sl, cg2_el)

            if state_pr is False:
                # if self.class_subprogram[sp_idx].fname == "wrout1":
                #     print("debug65",self.class_subprogram[sp_idx].ftext[clb_ln])
                #     print("debug66",state_pr, state_do, state_if)
                #print(goto_state)
                continue
            
            # if
            if state_pr is False:
                #print(goto_state)
                if self.class_subprogram[sp_idx].fname == "tiblon":
                    print("debug114",self.class_subprogram[sp_idx].ftext[cg2_sl])
                continue

            if state_pr is True:
                print("debug55",self.class_subprogram[sp_idx].ftext[cg2_sl])
                g2_content = self.class_subprogram[sp_idx].ftext[cg2_sl]
                lb_content = self.class_subprogram[sp_idx].ftext[clb_ln]
                if re.search(r"&",g2_content):
                    print("find continuation: "+g2_content)
                    continue
                    
                if re.search(r"(?i)^\s*if",g2_content) is None and self.class_subprogram[sp_idx].bracket_mismatch(g2_content):
                    #print("debug70_0",self.class_subprogram[sp_idx].ftext[cg2_sl])
                    # print("debug70_1",self.class_subprogram[sp_idx].ftext[cg2_sl-1])
                    # print("debug70_2",self.class_subprogram[sp_idx].ftext[cg2_sl-2])

                    ssl = 0
                    len_ftext = len(self.class_subprogram[sp_idx].ftext)
                    #print("debug73",cg2_sl ,len_ftext)
                    for i in range(1, len_ftext-cg2_sl):
                        
                        search_line = self.class_subprogram[sp_idx].ftext[cg2_sl - i]
                        
                        if re.search(r"^\s*!",search_line):
                            continue
                        #print(search_line)
                        if re.search(r"(?i)^\s*if.*&", search_line):
                            ssl = cg2_sl - i
                            break
                    if ssl == 0:
                        print("error in ssl",g2_content)
                        exit(-1)
                    ssl_text = self.class_subprogram[sp_idx].ftext[ssl]
                    #print("debug74",ssl_text)
                    sl_text = self.class_subprogram[sp_idx].ftext[cg2_sl]
                    #if((IVIS.ne.1 .AND. IT2D.ne.1 .AND. IRHO.ne.1)&
                    #             .OR. isrcmode.NE.0)then ! add by @creaqi continuation
                    if re.search(r"(?i)\s*if\s*\(",ssl_text):
                        #print("debug75","yes")
                        pass
                    self.class_subprogram[sp_idx].ftext[ssl] = re.sub(r"(?i)\s*if\s*\(","      if(.not.(",ssl_text)
                    #print("debug76",self.class_subprogram[sp_idx].ftext[ssl])
                    self.class_subprogram[sp_idx].ftext[cg2_sl] = re.sub(r"(?i)\s*go\s*to\s*\d+", ")then ! add by @creaqi continuation",sl_text)
                    self.class_subprogram[sp_idx].ftext.insert(cg2_el,"      endif ! add by @creaqi continuation")
                    len_li = len(self.class_subprogram[sp_idx].class_if_then_endif_list)
                    tcif = class_if(len_li, ssl, cg2_el , cg2_sl - ssl,True,-663)
                    #self.class_subprogram[sp_idx].class_if_then_endif_list.append(tcif)
                    self.class_subprogram[sp_idx].update_line_number(cg2_el)

                    continue
                
                g2_condition_sobj = re.search(r"(?i)\(.*\)",g2_content)
                if g2_condition_sobj is not None :
                    
                    g2_condition = g2_condition_sobj.group(0)
                    
                    
                    inv_g2_condition = "(.not. "+g2_condition+")"
                    if re.search(r"(?i)if",g2_content):
                        
                        complete_g2_statement = "      if"+inv_g2_condition+\
                            " then ! add by @creaqi goto {0} in fun modify_goto_pure".format(clb.get_label_id())
                    else:
                        continue
                        complete_g2_statement = inv_g2_condition+" then ! add by @creaqi goto {0} modify_goto_pure".format(clb.get_label_id())
                    if re.search(r"ITER",g2_content):
                        
                        #print("======: ",g2_content,cg2.get_label_id(),clb.get_label_id(),complete_g2_statement)
                        pass
                    #clb.set_line_number( clb.get_line_number() + cnt_insert_endif )

                    #cg2.set_start_line_number( cg2.get_start_line_number() + cnt_insert_endif )

                    # t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                    # cg2_sl = t_cg2.get_start_line_number()
                    # cg2_el = t_cg2.get_end_line_number()

                    # print("-------------------goto and label b------------------")
                    # print(cnt_insert_endif)
                    # print(self.class_subprogram[sp_idx].ftext[clb_ln+cnt_insert_endif])
                    # print(self.class_subprogram[sp_idx].ftext[cg2_sl+0])
                    # print(self.class_subprogram[sp_idx].ftext[cg2_sl+cnt_insert_endif])
                    # print("-------------------goto and label e------------------")
                    if self.class_subprogram[sp_idx].fname == "sigset":
                        print(complete_g2_statement)
                    self.class_subprogram[sp_idx].ftext[cg2_sl] = complete_g2_statement
                    self.class_subprogram[sp_idx].ftext.insert(clb_ln," "*6+"endif !add by @creaqi label {0} modify_goto_pure".format(clb.get_label_id()))
                    self.class_subprogram[sp_idx].update_line_number(clb_ln)
                    #cnt_insert_endif = cnt_insert_endif+1
                    ng2 = self.get_num_goto_modified()
                    self.set_num_goto_modified(ng2+1)
                    len_cite_list = len(self.class_subprogram[sp_idx].class_if_then_endif_list)
                    # identity equal to -665 means should keep it
                    cite = class_if(len_cite_list,cg2_sl, clb_ln,1,True,identity = -665)
                    self.class_subprogram[sp_idx].class_if_then_endif_list.append(cite)

    def modify_goto_with_if(self):
        self.class_subprogram[sp_idx].update_do_label_statement()
        self.class_subprogram[sp_idx].update_goto_statement()
        self.class_subprogram[sp_idx].update_if_statement()
        # self.class_subprogram[sp_idx].get_class_if_list()
        # self.class_subprogram[sp_idx].get_class_endif_list()
        # self.class_subprogram[sp_idx].update_if_statement()
        cnt_insert_endif = 0
        len_cg2_list = len(self.class_subprogram[sp_idx].class_goto_list)
        len_clb_list = len(self.class_subprogram[sp_idx].class_label_list)
        for g2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
            t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
            cg2_sl = t_cg2.get_start_line_number()
            cg2_el = t_cg2.get_end_line_number()
            line = self.class_subprogram[sp_idx].ftext[cg2_sl]
            if self.class_subprogram[sp_idx].goto_has_modified(line) is True:
                continue
            if g2idx > len(self.class_subprogram[sp_idx].class_goto_list):
                break
            #print("debug48", len(self.class_subprogram[sp_idx].class_goto_list))
            cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
            cg2_sl = cg2.get_start_line_number()
            cg2_el = cg2.get_end_line_number()
            #if the goto statement is in a do statement, just not process
            if self.class_subprogram[sp_idx].is_goto_in_do_statement(cg2) is True:
                if self.class_subprogram[sp_idx].fname == "SNSQ":
                    #print(cg2.label)
                    pass
                #print("is_goto_in_do_statement(cg2) is True ")
                continue
                pass
            
            
            

            #if self.class_subprogram[sp_idx].is_goto

            if self.class_subprogram[sp_idx].fname == "output":
                temp_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                #print("---------   "+self.class_subprogram[sp_idx].ftext[cg2.get_start_line_number()])
                # print("---------"+g2_condition)

            #TODO 1.should make if-then-endif a ite class  2.add method to insert the class to proper postion of list
            for clb_idx,clb in enumerate(self.class_subprogram[sp_idx].class_label_list):
                if clb.get_label_id() == cg2.get_label_id():

                    if self.class_subprogram[sp_idx].goto_in_um_if(cg2_sl,cg2_el) is True:
                        um_eif_ln = self.class_subprogram[sp_idx].get_um_endif_line_number(cg2_sl,cg2_el)
                        #print("zzz")
                        #continue
                    else:
                        #print("yyy")
                        pass

                    if self.class_subprogram[sp_idx].is_goto_in_do_statement(cg2) is True:
                        #print("is_goto_in_do_statement(cg2) is True ")
                        #continue
                        pass

                    # if self.class_subprogram[sp_idx].fname == "SNSQ":
                    #     cg2id = cg2.get_label_id()
                    #     print("debug6: ",cg2id)
                    if self.class_subprogram[sp_idx].is_do_enddo_match(cg2_sl,cg2_el) is True:
                        if self.class_subprogram[sp_idx].fname == "SNSQ":
                            cg2id = cg2.get_label_id()
                            #print("debug6: ",cg2id)
                        pass

                    if self.class_subprogram[sp_idx].is_clb_in_do(clb) is True:
                        #print("is_clb_in_do")
                        pass
                        #continue


                    # print("zzz",clb.get_label_id(),cg2.get_label_id())
                    #self.class_subprogram[sp_idx].update_label()
                    clb_ln = clb.get_line_number()
                    cg2_sl = cg2.get_start_line_number()
                    cg2_el = cg2.get_end_line_number()
                    t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                    cg2_sl = t_cg2.get_start_line_number()
                    cg2_el = t_cg2.get_end_line_number()
                    

                    # if clb_idx != len_clb_list:
                    #     self.class_subprogram[sp_idx].class_label_list[clb_idx].set_line_number(clb_ln+cnt_insert_endif+1)
                    #     self.class_subprogram[sp_idx].class_goto_list[g2idx].set_start_line_number(cg2_sl+cnt_insert_endif+1)
                    #     self.class_subprogram[sp_idx].class_goto_list[g2idx].set_end_line_number(cg2_el+cnt_insert_endif+1)
                    #clb_ln = self.class_subprogram[sp_idx].class_label_list[clb_idx].get_line_number()
                    #cg2_sl = self.class_subprogram[sp_idx].class_goto_list[g2idx].get_start_line_number()
                    # print("--------goto and label b--------{0}----{1}---{2}---".format(clb.get_label_id(),g2idx,clb_idx))
                    # print(self.class_subprogram[sp_idx].ftext[clb_ln])
                    # print(self.class_subprogram[sp_idx].ftext[cg2_sl])
                    # print("-------------------goto and label e------------------")
                    
                    if cg2_sl<clb_ln:

                        

                        #
                        #  ^     ^
                        len_if, stk_if = self.class_subprogram[sp_idx].jump_out_if(cg2_sl,cg2_el)
                        # if gs ge ed
                        #    ^  ^ 
                        if len_if == 0:
                            pass
                        
                        # if gs ed ge
                        #    ^     ^
                        #if if gs xxx ed ed ge
                        if len_if > 0 :
                            # for i in range(cg2_sl, cg2_el+1):
                            #     line = self.class_subprogram[sp_idx].ftext[i]
                            #     print("debug51",line)
                            # for i in range(cg2_sl, cg2_el + 1):
                            #     line = self.class_subprogram[sp_idx].ftext[i]
                            #     print("debug51",line)
                            #print("debug52",self.class_subprogram[sp_idx].ftext[cg2_sl],self.class_subprogram[sp_idx].ftext[cg2_el])
                            min_if_el = 99999
                            max_if_el = -99999
                            for cif in stk_if:
                                cif_sl = cif.get_start_line_number()
                                cif_el = cif.get_end_line_number()
                                if cif_el < min_if_el:
                                    #print("debug51",self.class_subprogram[sp_idx].ftext[cif_el])
                                    min_if_el = cif_el
                                if cif_el > max_if_el:
                                    max_if_el = cif_el

                            
                            if re.search(r"(?i)^\s*if",line):
                                pass
                            elif re.search(r"(?i)^\s*go\s*to\s*\d+",line):
                                line = self.class_subprogram[sp_idx].ftext[cg2_sl]
                                vda_el = self.class_subprogram[sp_idx].get_vda_eln()
                                cg2id = cg2.get_label_id()
                                variable_name = "lgoto_{0}_{1}".format(cg2id,cg2.get_idx())
                                insert_variable_content = "      logical :: {0} = .false. ! add by @creaqi modify_goto_with_if".format(variable_name)
                                self.class_subprogram[sp_idx].ftext.insert(vda_el + 1, insert_variable_content)
                                self.class_subprogram[sp_idx].update_line_number(vda_el + 1)
                                #print("debug41",line)
                                t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                cg2_sl = t_cg2.get_start_line_number()
                                cg2_el = t_cg2.get_end_line_number()
                                # if if gs ed ed xxx ge
                                #print("debug50",self.class_subprogram[sp_idx].ftext[cg2_sl], min_if_el)
                                if self.class_subprogram[sp_idx].has_statement(max_if_el,cg2_el):
                                    #cg2_sl = cg2_sl if 
                                    #print("debug40",self.class_subprogram[sp_idx].ftext[cg2_sl])
                                    # continue
                                    #cg2_order = self.class_subprogram[sp_idx].get_same_goto_order(cg2id, g2idx)
                                    #cg2_sl = self.class_subprogram[sp_idx].get_cg2_sl_by_id(cg2id, cg2_order)
                                    t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                    cg2_sl = t_cg2.get_start_line_number()
                                    cg2_el = t_cg2.get_end_line_number()
                                    #print("debug43",self.class_subprogram[sp_idx].ftext[t_cg2_sl])

                                    self.class_subprogram[sp_idx].ftext[cg2_sl] = "      {0} = .true. !".format(variable_name) + line
                                    #min_if_el = min_if_el if min_if_el < vda_el + 1 else min_if_el + 1
                                    
                                    # if if gs xxx ed ed xxx ge
                                    
                                    if self.class_subprogram[sp_idx].has_statement(cg2_sl + 1, min_if_el):
                                        insert_if_text = "      if({0}) then ! add by @creaqi start of goto {1}".format(variable_name,cg2id)
                                        insert_endif_text = "      endif ! add by @creaqi end of goto {0}".format(cg2id)
                                        self.class_subprogram[sp_idx].ftext.insert(cg2_sl + 1, insert_if_text)
                                        self.class_subprogram[sp_idx].update_line_number(cg2_sl + 1)
                                        self.class_subprogram[sp_idx].ftext.insert(min_if_el, insert_endif_text)
                                        self.class_subprogram[sp_idx].update_line_number(min_if_el)
                                        #max_if_el = max_if_el if max_if_el > min_if_el else max_if_el + 1
                                        #print("debug49",max_if_el , min_if_el)
                                    #     if max_if_el > min_if_el:
                                    #         #print("xxxxxxxxxxxxxxxxx")
                                    #         max_if_el += 1

                                    # if max_if_el > vda_el + 1:
                                    #         #print("xxxxxxxxxxxxxxxxx")
                                    #         max_if_el += 1
                                    insert_m_text = "      if(.not.{0}) then".format(variable_name)
                                    self.class_subprogram[sp_idx].ftext.insert(max_if_el + 1, insert_m_text)
                                    self.class_subprogram[sp_idx].update_line_number(max_if_el + 1)
                                    t_cg2 = self.class_subprogram[sp_idx].class_goto_list[g2idx]
                                    cg2_sl = t_cg2.get_start_line_number()
                                    cg2_el = t_cg2.get_end_line_number()
                                    self.class_subprogram[sp_idx].ftext.insert(cg2_el, "      endif ! add by @creaqi max {0}".format(variable_name))
                                    self.class_subprogram[sp_idx].update_line_number(cg2_el)
                                    #print("debug42", self.class_subprogram[sp_idx].ftext[cg2_el])
                                    t_cif = class_if(len_if,max_if_el + 1,cg2_el,1,True,-663)
                                    #self.class_subprogram[sp_idx].class_if_then_endif_list.appe
                                    continue


                                else:
                                    print("nonono")
                                    # self.class_subprogram[sp_idx].ftext[cg2_sl] = "      {0} = .true. !".format(variable_name) + line
                                    # min_if_el = min_if_el if min_if_el < vda_el + 1 else min_if_el + 1
                                    # insert_text = "      endif ! add by @creaqi end of goto {0}".format(cg2id)
                                    # self.class_subprogram[sp_idx].ftext.insert(min_if_el, insert_text)
                                    # self.class_subprogram[sp_idx].update_line_number(min_if_el)
                            else:
                                pass

                        # gs if ge ed
                        # ^     ^
                        len_if, stk_if = self.class_subprogram[sp_idx].jump_into_if(cg2_sl,cg2_el)
                        if len_if != 0:
                            continue

    


    def get_space(self,n):
        return " "*n

    def update_line_number(self,sp_idx,inserted_ln):
        for clbidx,clb in enumerate(self.class_subprogram[sp_idx].class_label_list):
            t_clb_ln = clb.get_line_number()
            if t_clb_ln >= inserted_ln:
                 self.class_subprogram[sp_idx].class_label_list[clbidx].set_line_number(t_clb_ln+1)
            
        for cg2idx, cg2 in enumerate(self.class_subprogram[sp_idx].class_goto_list):
            t_cg2_sl = cg2.get_start_line_number()
            t_cg2_el = cg2.get_end_line_number()
            if t_cg2_sl >= inserted_ln:
                #print('update_line_number: ' + self.class_subprogram[sp_idx].ftext[t_cg2_sl + 1])
                self.class_subprogram[sp_idx].class_goto_list[cg2idx].set_start_line_number(t_cg2_sl + 1)
            if t_cg2_el >= inserted_ln:
                self.class_subprogram[sp_idx].class_goto_list[cg2idx].set_end_line_number(t_cg2_el + 1)
        

        
        

    def is_in_range(self,start,end,test_num):
        if test_num<=end and test_num>=start:
            return  True

    def is_comment(self,ln,cls_sp):
        if re.search(r"^\s*!",cls_sp.ftext[ln]):
            return True
        else:
            return False

    def has_statement(self,start,end,cls_sp):
        #indicate if there is a statement between start and end line number
        #False means there is statements between start and end line number
        status = False
        for idx in range(start+1,end):
            if cls_sp.fname == "SNSQ":
                #print("debug2: "+cls_sp.ftext[idx])
                pass
            if not self.is_comment(idx,cls_sp):
                status = True
                return True
        return status

class subprogram:

    def __init__(self,order, fname,ftype,ftext,nl,ns,nc,ng2,start_line_number,end_line_number):
        self.order = order
        self.fname=fname
        self.ftype=ftype
        #list of text
        self.ftext=ftext
        self.nlines=nl
        self.nstatements = ns
        self.ncomments = nc
        self.ng2 = ng2
        self.statements = self._get_statements()
        self.statements2insert = OrderedDict()
        self.insert_num = []
        self.class_goto_list = []
        self.class_do_list = []
        self.class_if_list = []
        self.class_endif_list = []
        self.class_if_then_endif_list = []
        self.exit_label = 0
        self.start_line_number = start_line_number
        self.end_line_number = end_line_number
        self.class_label_list = []
        self.declaration_field_start_line_number = 0
        self.declaration_field_end_line_number = 0
        self.include_field_start_line_number = 0
        self.include_field_end_line_number = 0
        self.data_filed_start_line_number = 0
        self.data_filed_end_line_number = 0
        self._get_dfsln()
        #self.get_computed_goto()

    def __iter__(self):
        return self

    def __next__(self):
        return 1
    
    def get_end_line_number(self):
        return self.end_line_number
    
    def set_end_line_number(self,end_line_number):
        self.end_line_number = end_line_number

    def get_start_line_number(self):
        return self.start_line_number
    
    def set_start_line_number(self,start_line_number):
        self.start_line_number = start_line_number

    def get_vda_sln(self):
        return self.declaration_field_start_line_number
    
    def set_vda_sln(self,vda_sln):
        self.declaration_field_start_line_number = vda_sln

    def get_vda_eln(self):
        return self.declaration_field_end_line_number

    def set_vda_eln(self,vda_eln):
        self.declaration_field_end_line_number = vda_eln
    
    def get_ifa_sln(self):
        return self.include_field_start_line_number

    def set_ifa_sln(self, ifa_sln):
        self.include_field_start_line_number = ifa_sln

    def get_ifa_eln(self):
        return self.include_field_end_line_number

    def set_ifa_eln(self, ifa_eln):
        self.include_field_end_line_number = ifa_eln 

    def get_statement_length(self):
        return self.get_end_line_number() - self.get_start_line_number()

    
    def get_idx_id(self):
        return self.order

    def _get_dfsln(self):
        datatypes_list = ["(int)","(logical)","(double)","(float)","(real)"]
        pat_dt = "(?i)"+"^\s*("+"|".join(datatypes_list)+")"
        pat_inc = "(?i)^\s*include\s*[\'\"].*[\'\"]"
        pat_dat = "(?i)^\s*data\s+"
        #print(pat_dt)
        first_find_dec = False
        first_find_inc = False
        first_find_dat = False
        for idx,line in enumerate(self.ftext):
            if re.search(r"\s*!",line):
                continue
            if re.search(pat_dt,line):
                if first_find_dec == False:
                    self.declaration_field_start_line_number = idx
                    self.declaration_field_end_line_number = idx
                    first_find_dec = True
                else:
                    self.declaration_field_end_line_number += 1
            
            if re.search(pat_inc,line):
                if first_find_inc == False:
                    self.include_field_start_line_number = idx
                    self.include_field_end_line_number = idx
                    first_find_inc = True
                else:
                    self.include_field_end_line_number += 1

            if re.search(pat_dat,line):
                #print(line)
                if first_find_dat == False:
                    self.data_filed_start_line_number = idx
                    self.data_filed_end_line_number = idx
                    first_find_dat = True
                else:
                    self.data_filed_end_line_number += 1

        #print(self.ftext[self.declaration_field_start_line_number],self.ftext[self.declaration_field_end_line_number])

    def search_be4_mark(self, parttern , line):
        if re.search(parttern, line):
            rres = re.search(parttern, line)
            r_p_s = rres.start()
            if re.search(r"!", line) is not None:
                mres = re.search(r"!", line)
                m_p_s = mres.start()
                if m_p_s < r_p_s:
                    return None
                elif m_p_s > r_p_s:
                    return rres
                elif m_p_s ==r_p_s:
                    print("debug147","error in search_be4_mark",line)
                    exit(-1)
            else:
                return rres
        else:
            return None
            print("debug149","error in search_be4_mark",line)
            exit(-1)

    def modify_computed_goto(self):
        computed_label_list = []
        label_set_list = []
        select_var = None
        is_find = False
        rstart = 0
        rend = 0
        for i, line in enumerate(self.ftext):
            if self.search_be4_mark(r"(?i)go\s*to\s*",line):
                gres = self.search_be4_mark(r"(?i)go\s*to\s*",line)
                g_p_s = gres.start()
                dres = self.search_be4_mark(r"(\d+,)+\d+",line[g_p_s:])
                if dres is not None:
                    rstart = i
                    is_find = True
                    start_p = dres.start() + g_p_s
                    end_p = dres.end() + g_p_s

                    computed_label_list = re.findall(r"\d+",line[start_p:end_p + 1])

                    label_set_list = list(set(computed_label_list))
                    list.sort(label_set_list)
                    print(label_set_list) 

                    select_var_obj = re.search(r"\w+", line[end_p:])
                    select_var = select_var_obj.group(0)

                    print("debug154", select_var)

                    # if lres is not None:
                    #     print(lres)
                    #     print(len(lres))

                    print("debug153", line[start_p:end_p + 1])

                    

                    # ddres = re.search("\d+", line)
                    # if ddres is not None:
                    #     print("debug152",len(ddres.groupdict))
                    # print(dres.group(0))
                    
                    # len_d = dres
                    
                    # if len_d > 1:
                    #     print(line,len_d)
                    #     exit()
        if is_find:
            label_start_ln = []
            label_statements_list = [ [] for i in range(0,len(label_set_list)) ]
            
            j = 0
            lf = False
            for i, line in enumerate(self.ftext):
                if lf is True:
                    cl = copy.copy(line)
                    if re.search(r"\d+", line[0:6]):
                        print(line)
                        tline = re.sub(r"\d+","   ", line[0:6]) + cl[6:]
                        print(line)
                        label_statements_list[j-1].append(tline)
                        #exit()
                    else:
                        label_statements_list[j-1].append(line)
                    if self.search_be4_mark(r"(?i)return",self.ftext[i]) and j == len(label_set_list) :
                        print("debug158",line)
                       
                        rend = i
                        break
                lres = self.search_be4_mark(r"\d+", line[0:6])
                if lres is not None:
                    if int(lres.group(0).strip()) == int(label_set_list[j]):
                        if re.search(r"\d+", line[0:6]):
                            print(line)
                            line = re.sub(r"\d+","   ", line[0:6]) + line[6:]
                            print(line)
                            #exit()
                        label_statements_list[j].append(line)
                        label_start_ln.append(i)
                        j += 1
                        lf = True

            

            #for i, line in enumerate(self.ftext):
            case_statements_list = [ [] for i in range(0,len(computed_label_list)) ]
            for i, ib in enumerate(computed_label_list):
                for j, jb in enumerate(label_set_list):
                    if ib == jb:
                        case_statements_list[i] = label_statements_list[j]

            for i, ib in enumerate(case_statements_list):
                for j, line in enumerate(ib):
                    print(j,line)
                    pass

            
            cnt_select_label = len(computed_label_list)

            select_clause = "      select case({0})".format(select_var)
            case_list = []
            for i, label in enumerate(computed_label_list):
                t_case_list = []
                case_clause = "      case({0})".format(i+1)
                t_case_list.append(case_clause)
                t_case_statements = case_statements_list[i][0:-1]
                print("debug156",len(t_case_statements))
                for i, line in enumerate(t_case_statements):
                    t_case_list.append(line)

                case_list.extend(t_case_list)

            # for line in case_list:
            #     print(line)

            case_list.insert(0, select_clause)
            case_list.append("end select")

            print("debug157",rstart, rend)
            print(self.ftext[rstart], self.ftext[rend])
            print(len(case_list))
            for i in range(rstart, rend + 1):
                line = self.ftext[i]
                self.ftext[i] = "!" + line

            for i in range(0, len(case_list)):
                self.ftext.insert(rstart + i, case_list[i])

           
    def bracket_mismatch(self, line):
        cnt = 0
        for c in line:
            #print(c)
            if c == '(':
                cnt += 1
            if c == ')':
                cnt -= 1

        if cnt==0:
            return False
        if cnt < 0:
            return True
        if cnt >0:
            print("debug159","error in bracket_mismatch")
            exit(-1)
                 

    def get_clb_idx_by_label_id(self,lb_id):
        for clbidx,clb in enumerate(self.class_label_list):
            if lb_id == clb.get_label_id():
                return clbidx
            
        return -1

    def get_label_line_number_by_label_id(self,lb_id):
        for clbidx,clb in enumerate(self.class_label_list):
            if lb_id == clb.get_label_id():
                return clb.get_line_number()
            
        return -1

    def should_add_exit(self,enddo_ln,clb_ln,clb_id):
        should_exit = False
        for i in range(enddo_ln, clb_ln + 1):
            line = self.ftext[i]
            if re.search(r"^\s*!",line):
                continue
            lb = re.search(r"\d+",line[0:6])
            if lb is not None:
                lb_id == lb.group(0).strip()
                
    def is_do_enddo_match(self,cg2_ln, clb_ln):
        ded_stack = []
        lm = 0
        for i in range(cg2_ln, clb_ln + 1):
            line = self.ftext[i]
            
            
            if re.search(r"\s*!",line):
                continue
            #print(line)
            if re.search(r"^\s*do\s+",line):
                lm +=1
                ded_stack.append(line)
                #print("append: ",line,len(ded_stack))
            
            if re.search(r"^\s*enddo",line):
                #print("pop: ",line,len(ded_stack))
                lm -= 1
                ded_stack.append(line)
        
        if lm != 0:
            return False
        else:
            return True



    def update_line_number_list(self,insert_ln, update_list):
        #print("debug126", insert_ln, self.ftext[insert_ln])
        for i in range(0, len(update_list)):
            i_sl = update_list[i].get_start_line_number()
            i_el = update_list[i].get_end_line_number()
            sl_text = self.ftext[i_sl]
            el_text = self.ftext[i_el]
            #print("debug124",i_sl, i_el, sl_text,el_text)
            if insert_ln <= i_sl:
                update_list[i].set_start_line_number(i_sl + 1)
            if insert_ln <= i_el :
                update_list[i].set_end_line_number(i_el + 1)
            
            i_sl = update_list[i].get_start_line_number()
            i_el = update_list[i].get_end_line_number()
            #print("debug138", i_sl, i_el)
            sl_text = self.ftext[i_sl]
            el_text = self.ftext[i_el]
            #print("debug125",i_sl, i_el,sl_text,el_text)




    #TODO should consider the following condtions:
    # firstly, do--if-then--enddo, in this case, should search downward to find endif
    # secondly, do--endif--enddo, in this case, should search upward to find if-then

    def get_endif_line_number(self,do_ln,enddo_ln):
        if_stack = []
        endif_ln = 0
        first_find = True
        #print('end line number',do_ln,self.get_statement_length())
        for idx in range( do_ln,self.get_statement_length() + 1 ):
            line = self.ftext[idx]
            #print(idx,line)
            if re.search(r"(?i)^\s*!",line):
                continue
            if first_find == False:
                #print("if_stack length: " , len(if_stack))
                if len(if_stack)==0:
                    return idx
            if re.search(r"(?i)^\d*\s*if\s*\(.*\)\s*then\s*",line):
                #print("line if-then: "+line)
                if_stack.append(line)
                first_find = False
            
            if re.search(r"(?i)\d*\s*if.*&",line):
                for j in range(idx+1, idx + 10):
                    line_n = self.ftext[j]
                    if re.search(r"(?i)then",line_n):

                        #print(line)
                        #lm += 1
                        if_stack.append(line)
                        first_find = False

            if re.search(r"(?i)^\s*end\s*if",line):
                #print("line endif: " + line)
                endif_ln = idx
                if_stack.pop()
        
        # cnt_um_endif = len(if_stack)

        # for idx in range(enddo_ln ,self.get_end_line_number()+1):
        #     print("self.ftext[idx]"+self.ftext[idx])
        #     if re.search(r"(?i)^\s*end\s*if",self.ftext[idx]):
        #         cnt_um_endif -= 1
            
        #     if cnt_um_endif == 0:
        #         print("self.ftext[idx]: ",self.ftext[idx])
        #         return idx
        
        # return -1


               

    def is_doenddo_contains_um_if(self,do_ln,enddo_ln):
        if_stack = []
        lm = 0
        for idx in range(do_ln,enddo_ln + 1):
            line = self.ftext[idx]
            if re.search(r"(?i)^\s*!",line):
                continue
            if self.fname=="comp":
               # print(line)
                pass
            #if re.search(r"(?i)^\s*if\s*\(.*\)\s*then\s*",line):
            if re.search(r"(?i)^\d*\s*if.*then",line):
                #print(line)
                if self.fname=="SNSQ":
                    #print(line)
                    pass
                #print(line)
                lm += 1
                if_stack.append(line)
            
            if re.search(r"(?i)\d*\s*if.*&",line):
                for j in range(idx+1, idx + 10):
                    line_n = self.ftext[j]
                    if re.search(r"(?i)then",line_n):

                        #print(line)
                        lm += 1
                        if_stack.append(line)

            if re.search(r"(?i)^\s*end\s*if",line):
                #print(line)
                lm -= 1
                if_stack.pop()
        
        if len(if_stack) != 0:
            return True
        else:
            return False
        pass  


    def line_contain_goto(self, text):
        if re.search(r"(?i)go\s*to\W",text):
            res = re.search(r"(?i)go\s*to\W",text)
            g2_s_p = res.start()
            if re.search(r"!",text[0:g2_s_p]):
                return False
            else:
                return True


    def get_lb_obj_by_id(self, cg2_lb):
        for clb in self.class_label_list:
            lb_id = clb.get_label_id()
            if lb_id == cg2_lb:
                return clb
        # failed
        return -1
    # do if gs edo ed ge
    def goto_is_pure(self, cg2_sl, cg2_el):
        state_do = False
        state_if = False
        state_pr = False
        for cdo in self.class_do_list:
            cdo_sl = cdo.get_start_line_number()
            cdo_el = cdo.get_end_line_number()
            s1 = is_in_range(cg2_sl, cg2_el, cdo_sl)
            s2 = is_in_range(cg2_sl, cg2_el, cdo_el)
            
            if ( s1==True and s2==False ) or ( s1==False and s2==True ):
                state_do = True
                break
            

        for cif in self.class_if_then_endif_list:
            cif_sl = cif.get_start_line_number()
            cif_el = cif.get_end_line_number()
            
            s1 = is_in_range(cg2_sl, cg2_el, cif_sl)
            s2 = is_in_range(cg2_sl, cg2_el, cif_el)

            if ( s1 and not s2 ) or ( not s1 and s2 ):
                state_if = True
                break

        if state_do is False and state_if is False:
            state_pr = True
        return state_pr, state_do, state_if

    # do if gs enddo endif ge
    # if do gs endif enddo 
    #
    #
    def goto_um_state(self, cg2_sl, cg2_el):
        state_do = False
        state_if = False
        state_pr = False
        for cdo in self.class_do_list:
            cdo_sl = cdo.get_start_line_number()
            cdo_el = cdo.get_end_line_number()
            # gs if ge endif
            s1 = is_in_range(cg2_sl, cg2_el, cdo_sl)
            # if gs endif ge
            s2 = is_in_range(cg2_sl, cg2_el, cdo_el)
            
            # 
            if ( s1==True and s2==False ) or ( s1==False and s2==True ):
                state_do = True
                break
            

        for cif in self.class_if_then_endif_list:
            cif_sl = cif.get_start_line_number()
            cif_el = cif.get_end_line_number()
            
            s1 = is_in_range(cg2_sl, cg2_el, cif_sl)
            s2 = is_in_range(cg2_sl, cg2_el, cif_el)

            if ( s1 and not s2 ) or ( not s1 and s2 ):
                state_if = True
                break

        if state_do is False and state_if is False:
            state_pr = True
        return state_pr, state_do, state_if

    def goto_if_type(self, cg2_sl, cg2_el):
        state_do = False
        state_if = False
        state_pr = False
        di_obj_list = []
        for cdo in self.class_do_list:
            cdo_sl = cdo.get_start_line_number()
            cdo_el = cdo.get_end_line_number()
            s1 = is_in_range(cg2_sl, cg2_el, cdo_sl)
            s2 = is_in_range(cg2_sl, cg2_el, cdo_el)
            
            #if ( s1 and not s2 ) or ( not s1 and s2 ):
            # do gs enddo ge
            # gs do ge enddo
            # only care about goto jump out do
            if not s1 and s2 :
                #print("debug86","dodododoodo")
                di_obj_list.append(cdo)
                state_do = True
                #break
            

        for cif in self.class_if_then_endif_list:
            cif_sl = cif.get_start_line_number()
            cif_el = cif.get_end_line_number()
            s1 = is_in_range(cg2_sl, cg2_el, cif_sl)
            s2 = is_in_range(cg2_sl, cg2_el, cif_el)
            # if gs endif ge
            # gs if ge endif
            # only care about jump out if
            # if ( s1 and not s2 ) or ( not s1 and s2 ):
            if not s1 and s2:
                state_if = True
                di_obj_list.append(cif)
                #break
        
        len_di_obj = len(di_obj_list)

        for i in range(0, len_di_obj):
            di_obj_el = di_obj_list[i].get_end_line_number()
            di_obj_sl = di_obj_list[i].get_start_line_number()
            #print("debug83",di_obj_sl, di_obj_el)

        for i in range(0, len_di_obj):
          
            di_obj = di_obj_list[i]
            #print("debug87",isinstance(di_obj,class_if))
            if isinstance(di_obj, class_if):
                i_sl = di_obj.get_start_line_number()
                i_el = di_obj.get_end_line_number()
            if isinstance(di_obj, class_do):
                i_sl = di_obj.get_start_line_number()
                i_el = di_obj.get_end_line_number()
            
            for j in range(i+1,len_di_obj):
                j_sl = di_obj_list[j].get_start_line_number()
                j_el = di_obj_list[j].get_end_line_number()
                if i_el >= j_el:
                    i_obj_copy = copy.copy(di_obj_list[i])
                    di_obj_list[i] = copy.copy(di_obj_list[j])
                    di_obj_list[j] = i_obj_copy
        
        for i in range(0, len_di_obj):
            di_obj_el = di_obj_list[i].get_end_line_number()
            di_obj_sl = di_obj_list[i].get_start_line_number()
            #print("debug82",self.ftext[di_obj_sl], self.ftext[di_obj_el])
        if state_do is False and state_if is False:
            state_pr = True
        if state_pr is False:
            return di_obj_list



    # can be improved by use blur search
    def get_cg2_sl_by_id(self, cg2id, g2_order) :
        t_g2_order = 0
        for ln, line in enumerate(self.ftext):
            if re.search(r"^\s*!",line):
                continue
            res = re.search(r"(?i)go\s*to\s*\d+",line)
            if res is not None:
                res_s_pos = res.start()
                m_res = re.search(r"!",line)
                if m_res is not None:
                    mark_s_pos = m_res.start()
                    if mark_s_pos < res_s_pos:
                        continue
                res_content = res.group(0)
                lb_obj = re.search(r"\d+",res_content)
                lb_id = int(lb_obj.group(0))
                if lb_id == cg2id:
                    t_g2_order += 1
                    print("debug45",t_g2_order ,g2_order)
                    if t_g2_order == g2_order:
                        print("debug44",self.ftext[ln])
                        return ln
        # -1 means failed to find the label
        return -1

    def get_same_goto_order(self, cg2_lb, g_g2idx):
        #print("debug47",len(self.class_goto_list))
        order = 0
        for i, cg2 in enumerate(self.class_goto_list):
            t_cg_lb = cg2.get_label_id()
            if t_cg_lb == cg2_lb:
                order += 1
                print("debug46",i , g_g2idx)
            if i == g_g2idx:
                return order
        # -1 means failed
        return -1 

    def is_doenddo_contains_um_if1(self,do_ln,enddo_ln):
        if_stack = []
        lm = 0
        for idx in range(do_ln,enddo_ln + 1):
            line = self.ftext[idx]
            if re.search(r"(?i)^\s*!",line):
                continue
            if re.search(r"(?i)^\s*if\s*\(.*\)\s*then\s*",line):
                #print(line)
                lm += 1
                if_stack.append(line)
            if re.search(r"(?i)^\s*end\s*if",line):
                #print(line)
                lm -= 1
                if_stack.append(line)
        
        if lm != 0:
            return True
        else:
            return False
        pass  

    def goto_has_modified(self,line):
        if re.search(r"(?i)go\s*to",line):
            gres = re.search(r"(?i)go\s*to",line)
            gps = gres.start()
            if re.search(r"!",line):
                mres = re.search(r"!",line)
                mps = mres.start()
                if mps < gps:
                    return True
                else:
                    return False
            else:
                return False
        else:
            return True
        print(line)
        raise NotImplementedError
        


    def is_goto_in_do_statement(self,goto):
        self.update_do_label_statement()
        #print(len(self.class_do_list))
        for do_idx, do_cls in enumerate(self.class_do_list):
            
            do_sl = do_cls.get_start_line_number()
            do_el = do_cls.end_line_number
            goto_sl = goto.start_line_number
            goto_el = goto.end_line_number
            # if self.fname =="SNSQ":
            #     print("debug10",self.ftext[do_sl])
            #print(do_sl,goto_ln,do_el)
            if goto_sl<= do_el and goto_sl>= do_sl:
                return True
            if goto_el<= do_el and goto_el>= do_sl:
                return True
        
        return False

    def safe_do_label(self,mdlb):
        #self.update_goto_statement()
        for cg2 in self.class_goto_list:
            cg2_label = cg2.get_label_id()
            if cg2_label == mdlb:
                return False
        return True

    def has_statement(self, start, end):
        state = True
        for line in self.ftext[start+1:end]:
            if re.search(r"^\s*!",line) :
                state = False
            else:
                state = True
                return True
        
        return False

    def has_statement_pure(self, start, end):
        state = True
        for line in self.ftext[start+1:end]:
            
                    
            if re.search(r"(?i)^\s*(endif)",line[5:]):
                state = False
            if re.search(r"^\s*!",line) :
                state = False
            else:
                state = True
                return True
        
        return state

    def has_statement_endif_only(self, start, end):
        endif_only = False
        cnt_ie = 0
        for line in self.ftext[start+1:end]:
            #print("debug130", line)
            #exit(-1)
            if re.search(r"(?i)^\s*if",line):
                cnt_ie += 1
                continue
                    
            if re.search(r"(?i)^\s*endif",line):
                #print("debug129",line)
                cnt_ie -= 1
                endif_only = True
                continue
            if re.search(r"^\s*!",line) :
                #endif_only = False
                continue
            else:
                endif_only = False
                
        if endif_only == True and cnt_ie < 0:
            return True
        else:
            return False
        

    
    def _get_statements(self):
        #print(self.ftext)
        td = []
        for ln,line in enumerate(self.ftext):
            if not re.search(r"^\s*[!\*]",line):
                td.append(line)
        return td
    def show_statements(self):
        for k,v in self.statements.items():
            print(k,v)

    def update_exit_label(self):
        self.exit_label = 0
        for l,s in enumerate(self.ftext):
            #find exit label of a subprogram
            if re.search(r"(?i)\s*\d+\s*return",s) and re.search(r"(?i)^\s*end",self.ftext[l+1]):
                #print(s)
                detect_return = True
                rs = re.search(r"(?i)\d+",s)
                #print(rs)
                if rs is not None:
                    exit_label = rs.group(0).strip()
                    #print("el:"+exit_label)
                    self.exit_label = exit_label
            #if detect_return and re.search(r"\s*end[ ]*$",s):
                #print(s)
            #    detect_return = False

    def update_do_label_statement_complete(self):
        pass

    def get_special_do(self):
        special_do_list = []
        for cdo in self.class_do_list:
            if int(cdo.get_label_id()) == -998 :
                special_do_list.append(cdo)
        return special_do_list


    def clear_class_do_list(self):
        for i in range(0, len(self.class_do_list)+1):
            pass


    def static_vars(**kwargs):
        def decorate(func):
            for k in kwargs:
                setattr(func, k, kwargs[k])
            return func
        return decorate
    
    #
    # if gs ed ge
    #    ^     ^
    # if if gs ed ed ge
    #       ^        ^
    # if gs if  ge  endif  endif ge
    #    ^      ^            ^
    @self.static_vars(counter=0)
    def jump_out_if(self, cg2_sl, cg2_el):
        self.counter += 1
        
        cnt_eif = 0
        stk_if = []
        #print("#########jump_out_if start###########")
        for cif in self.class_if_then_endif_list:
            cif_sl = cif.get_start_line_number()
            cif_el = cif.get_end_line_number()
            if self.fname == "rdmet":
                print("debug53", self.ftext[cif_sl],self.ftext[cif_el])
                if re.search(r"(?i)^\s*endif", self.ftext[cif_sl]):
                    print("debug160","error in jump_out_if",jump_out_if.counter)
                    exit(-1)
            if is_in_range(cg2_sl,cg2_el,cif_el) and not is_in_range(cg2_sl,cg2_el,cif_sl):
                stk_if.append(copy.copy(cif))
                cnt_eif += 1
        #print("#########jump_out_if end###########")

        #should sort the list by the end line number in increasing order
        max_eln = -99999
        for i in range(0, cnt_eif):
            for j in range(i + 1, cnt_eif):
                if stk_if[i].get_end_line_number() > stk_if[j].get_end_line_number():
                    temp = copy.copy(stk_if[i])
                    stk_if[i] = copy.copy(stk_if[j])
                    stk_if[j] = temp
                
        
        return cnt_eif, stk_if

    # gs if ge ed
    # ^     ^
    # gs if if ge ed ed
    # ^        ^
    def jump_into_if(self, cg2_sl, cg2_el):
        cnt_eif = 0
        stk_if = []
        for cif in self.class_if_then_endif_list:
            cif_sl = cif.get_start_line_number()
            cif_el = cif.get_end_line_number()
            if is_in_range(cg2_sl,cg2_el,cif_sl) and not is_in_range(cg2_sl,cg2_el,cif_el):
                stk_if.append(cif)
                cnt_eif += 1
        
        return cnt_eif, stk_if


    """
    True: if if goto_start endif goto_end endif
    """
    def goto_in_um_if(self,g2_sl, g2_el):
        um_state = True
        for cif in self.class_if_then_endif_list:
            cif_sl = cif.get_start_line_number()
            cif_el = cif.get_end_line_number()
            #print("asdasdasd")
            # print(g2_sl,g2_el, cif_sl, cif_el)
            # print(is_in_range(cif_sl,cif_el,g2_sl))
            if is_in_range(cif_sl,cif_el,g2_sl):               
                if is_in_range(cif_sl,cif_el,g2_el):
                    #print("prove match !")
                    um_state = False
                else:
                    #print("")
                    um_state = True
            if is_in_range(cif_sl,cif_el,g2_sl) is False:
                um_state = False
            
        return um_state

    def get_um_endif_line_number(self, g2_sl, g2_el):
        eif_ln_list = []
        #print("debug21",self.ftext[g2_sl])
        for cif in self.class_if_then_endif_list:
            cif_sl = cif.get_start_line_number()
            cif_el = cif.get_end_line_number()
            if is_in_range(g2_sl,g2_el,cif_el):
                eif_ln_list.append(cif.get_end_line_number())
                #return cif.get_end_line_number()
            
            if is_in_range(g2_sl,g2_el,cif_sl):
                #print("debug18","format of loop occur !!!")
                continue
        
        len_ell = len(eif_ln_list)
        #print(len_ell)
        if len_ell == 0:
            #print("debug19","not find um_if")
            pass

        if len_ell == 1:
            #print("debug20", "one um if find")
            return eif_ln_list.pop()
        
        max_eif_eln = -999

        if len_ell > 1:
            
            for cif_eln in eif_ln_list:
                #cif_eln = cif.get_end_line_number()
                if cif_eln > max_eif_eln:
                    max_eif_eln = cif_eln
            return max_eif_eln



    def get_ie_obj_by_idx(self,idx):
        for cendif in self.class_endif_list:
            if cendif.get_idx() == idx:
                return cendif
            
        for cif in self.class_if_list:
            if cif.get_idx() == idx:
                return cif


    def get_class_ifthen_endif_list(self):
        #print("###############start######################")
        find_if = False
        order = 0
        cnt_ie = 0
        # fix bug here, IndexError occur when without INITIALIZE the following two list
        self.class_if_list = []
        self.class_endif_list = []
        for idx, line in enumerate(self.ftext):
            if re.search(r"^\s*!", line):
                continue
            

            if re.search(r"(?i)^[\d]*\s*if\s*\(.*",line):
                #print("sss")
                # print(line)
                
                t_c_if = class_if(order,idx,-899, 1, True)
                find_if = True
                
                

            if re.search(r"(?i)else\s*if",line):
                continue

            if re.search(r"&",line) and find_if:
                di = t_c_if.get_deep_if()
                t_c_if.set_deep_if(di + 1)


            if re.search(r"(?i)then",line) and find_if:
                #print(line)
                #if self.fname == 'ERFDIF':
                    #print("debug104", order, line)
                self.class_if_list.append(t_c_if)
                order += 1
                cnt_ie += 1
                find_if = False

            if re.search(r"(?i)^\s*end\s*if",line):
                #print(line)
                # if self.fname == 'ERFDIF':
                #     print("debug104", order, line)
                t_cendif = class_endif(order,idx)
                self.class_endif_list.append(t_cendif)
                order += 1
                cnt_ie += 1

        #print("###############end######################")  
        return cnt_ie
                

        for cif in self.class_if_list:
            cif_sl = cif.get_start_line_number()
            #print("debug15",cif_sl,cif.get_deep_if(),self.ftext[cif_sl])

    def get_class_endif_list(self):
        order = 0
        for idx, line in enumerate(self.ftext):
            if re.search(r"^\s*!", line):
                continue

            if re.search(r"(?i)^\s*end\s*if",line):
                #print(line)
                t_cendif = class_endif(order,idx)
                self.class_endif_list.append(t_cendif)
                order += 1

        len_if = len(self.class_if_list)
        len_endif = len(self.class_endif_list)

        if len_if != len_endif:
            #print("debug16","error in {0}".format(self.fname),len_if , len_endif)
            pass

    def sort_list(self,sort_list):
        if len(sort_list) == 0:
             return 
        #print(isinstance(sort_list[0],class_if))
        if isinstance(sort_list[0],class_if):
            len_cite_list = len(sort_list)
            #print(len_cite_list)
            for i in range(0,len(sort_list)):
                cite_i = sort_list[i]
                cite_i_idx = cite_i.get_idx()
                for j in range(i+1, len(sort_list)):
                    cite_j = sort_list[j]
                    # if cite_i == cite_j:
                    #     pass
                    cite_j_idx = cite_j.get_idx()                   
                    if cite_i_idx > cite_j_idx:
                        #print(cite_i_idx , cite_j_idx)
                        t = copy.copy(sort_list[i])
                        sort_list[i] = copy.copy(sort_list[j])
                        sort_list[j] = t
            for i in range(0,len(sort_list)):
                sort_list[i].set_idx(i)
                pass

    def get_special_if(self):
        t_cite_list = []
        for cite in self.class_if_then_endif_list:
            if cite.get_identity() == -665:
                t_cite_list.append(cite)
        
        return t_cite_list

    def not_goto_this_label(self, do_lb_id):
        for cg2 in self.class_goto_list:
            cg2_lb_id = cg2.get_label_id()
            if cg2_lb_id == do_lb_id:
                return False
        
        return True


    """
    if if if endif endif endif  if if endif if endif endif
    """
    def update_if_statement(self):
        cnt_ie = self.get_class_ifthen_endif_list()
        #TODO when modify goto to if-endif, should add it to the list
        self.class_if_then_endif_list = []
        t_cite_list = []
        #deep of stack
        dstk = 0
        cnt_if = 0
        cnt_endif = 0
        base = 0
        #print("cnt_ie",cnt_ie, self.fname)

        # for cif


        # for i in range(0, cnt_ie):
        #     ie_obj = self.get_ie_obj_by_idx(i)
        #     if isinstance(ie_obj,class_if):
        #         print(ie_obj.get_idx(),"IF   ",self.ftext[ie_obj.get_start_line_number()])
            
        #     if isinstance(ie_obj, class_endif):
        #         print(ie_obj.get_idx(),"ENDIF", self.ftext[ie_obj.get_line_number()])
       
        for i in range(0, cnt_ie):
            ie_obj = self.get_ie_obj_by_idx(i)
            # if i < cnt_ie -1:
            #     ie_obj_next = self.get_ie_obj_by_idx(i + 1)
            #print(ie_obj)
            if isinstance(ie_obj,class_if):
                #t_cite_list.append(ie_obj)
                
                self.class_if_then_endif_list.append((ie_obj))
                cnt_if += 1
                
                ieobj_sl = ie_obj.get_start_line_number()
                #print(len(self.class_if_then_endif_list),"if")

            elif isinstance(ie_obj,class_endif):
                ieobj_ln = ie_obj.get_line_number()
                #print(len(self.class_if_then_endif_list),"endif")
                try:
                    t_cite = self.class_if_then_endif_list.pop()
                except IndexError:
                    print("IndexError in update_if_statement", self.fname, i)
                    exit(-1)
                
                t_cite.set_end_line_number(ieobj_ln)
                t_cite_list.append(t_cite)
                # #if 1:#((isinstance(ie_obj_next,class_if) and cnt_if ==1) or  (isinstance(ie_obj_next,class_endif) and cnt_if > 0)):
                # ieobj_ln = ie_obj.get_line_number()
                # cnt_endif += 1
                # dstk = cnt_if - cnt_endif
                
                # #print(base + dstk)
                # self.class_if_then_endif_list[base + dstk].set_end_line_number(ieobj_ln)

                # # if dstk !=0 and isinstance(ie_obj_next,class_if):                  
                # #     base += 1
                # #     #cnt_endif = 0
                # #     print(base)

                # if dstk == 0:
                #     base += cnt_if 
                #     cnt_if = 0
                #     cnt_endif = 0
            else:
                print("error in uis")
        
        
                
                
                
                #print(dstk,self.ftext[ieobj_ln])
        self.class_if_then_endif_list = t_cite_list
        self.sort_list(self.class_if_then_endif_list)
        for i in range(0, int(cnt_ie/2)):
            cite = self.class_if_then_endif_list[i]
            cite_sl = cite.get_start_line_number()
            cite_el = cite.get_end_line_number()
            cite_idx = cite.get_idx()
            # print(self.ftext[cite_sl])
            # print(self.ftext[cite_el])
            if self.fname =="rdmet":
                #print("debug67",self.ftext[cite_sl],self.ftext[cite_el])
                #print("debug68", cg2_sl, cg2_el, cif_sl, cif_el)
                #print("debug69",cite_idx,cite_sl,cite_el)
                pass

        #self.get_class_endif_list()
        
        # len_if = len(self.class_if_list)
        # len_endif = len(self.class_endif_list)

        # if len_if != len_endif:
        #     print("error in subprogram {0}".self.fname)
        #     exit()
        
        # for i in range(1, len_endif + 1):
        #     cif_el = self.class_endif_list[i - 1].get_line_number()
        #     #print(len_endif - i)
        #     self.class_if_list[len_endif - i].set_end_line_number(cif_el) 

        # for cif in self.class_if_list:
        #     cif_sl = cif.get_start_line_number()
        #     cif_el = cif.get_end_line_number()
        #     #print(cif_sl,cif_el)
        #     #print("debug17", self.ftext[cif_sl],self.ftext[cif_el])           
                    

                
    # do do gs ed ed
    # ^     ^      
    def has_um_do_in_range(self, cdo_sl, cg2_sl):
        for line in self.ftext[cdo_sl + 1 :cg2_sl]:
            if re.search(r"^\s*!",line):
                continue
            #whrn
            if re.search(r"(?i)\Wdo\W",line):
                #print("debug22",line)
                return True
                pass
            else:
                #print("debug23",line)
                pass
        
        return False
    # do gs do ed ed ge 
    #    ^           ^
    def has_um_enddo_in_range(self, cg2,cdo):
        cg2_sl = cg2.get_start_line_number()
        cg2_el = cg2.get_end_line_number()
        cg2_lb = cg2.get_label_id()
        cdo_lb = cdo.get_label_id()
        cdo_idx = cdo.get_idx()
        
        cnt_um_enddo = 0
        ded_stack = []

        self.update_do_label_statement()

        for cdoidx, cdo in enumerate(self.class_do_list):
            cdo_sl = cdo.get_start_line_number()
            cdo_el = cdo.get_end_line_number()
            cdo_lb = cdo.get_label_id()
            # if cdo.get_idx() > cdo_idx:
            #     continue
            # do ed gs ge
            #       ^  ^
            # do gs ge ed
            #    ^  ^
            if is_in_range(cg2_sl, cg2_el, cdo_sl) is False and is_in_range(cg2_sl, cg2_el, cdo_el) is False:
                continue
            # gs do ed ge 
            # ^        ^
            if is_in_range(cg2_sl, cg2_el, cdo_sl) and is_in_range(cg2_sl, cg2_el, cdo_el):
                continue
            # do do gs ed ed ge
            #       ^        ^
            # do gs ed ge
            #    ^     ^
            if is_in_range(cg2_sl, cg2_el, cdo_el) and is_in_range(cdo_sl, cdo_el, cg2_sl):
                #print("debug27", self.ftext[cg2_sl])
                ded_stack.append(cdo)

        len_stk = len(ded_stack)
        return len_stk, ded_stack

        # for lidx, line in enumerate(self.ftext[cg2_sl + 1 :cg2_el]):
        #     if re.search(r"^\s*!",line):
        #         continue
        #     if re.search(r"(?i)\Wdo\W",line):
        #         line_dict = {line:lidx}
        #         ded_stack.append(line_dict)
        #     #whrn
        #     if re.search(r"(?i)\Wenddo\W",line) or re.search(str(cdo_lb)):
        #         ded_stack.pop()
        #         cnt_um_enddo += 1
        #         #print("debug22",line)
        #         #return True
        #         #pass
            
        
        # print(ded_stack)
                
                
    
    
    def update_do_label_statement(self):
        sd = self.get_special_do()
        self.class_do_list = []
        self.class_do_list.extend(sd)
        #print(self.class_do_list)
        pdo = r"(?i)(?<=do)\s+\d+\s*"
        pdol = r"(?<=)"
        patdo = re.compile(pdo)
        di = 0
        #print(self.fname)
        for l,s in enumerate(self.ftext):#find a do statement in text
            if re.search(r"(?i)^\s*!",s):
                continue
            if re.search(r"(?i)^\s*do\s+",s):

                if re.search(r"(?i)^\s*do\s*!",s):
                    #print("debug13",s)
                    continue

                #get the do label if exists
                r = patdo.search(s)
                start_do = True
                #label do exists
                if r is not None :
                    ld = r.group(0).strip()
                    #lds.append(ld)
                    #do_dict[l] = s
                    cdo = class_do(di,l,l,int(ld),0)
                    cdo.start_line_number = l #self.start_line_number + l
                    cdo.idx = di
                    cdo.end_line_number = 0
                    cdo.do_label = int(ld)
                    #level start at 0, increase it below
                    cdo.do_level = 0
                    self.class_do_list.append(cdo)
                    di =di + 1
                #label not exist
                else:
                    if self.fname == "SNSQ":
                        #print("debug12",s)
                        pass
                    #print("occur:  "+self.ftext[l])
                    depth_1 = 1
                    frist_enddo_line_number = 0
                    num_do_between = 0
                    cnt_enddo = 0
                    depth_2 = 1
                    while True:
                        line = self.ftext[l+depth_1]
                        #print(line)
                        if re.search(r"(?i)\s*end\s*do\s*",line) and not re.search(r"(?i)^\s*!",line):
                            frist_enddo_line_number = l+depth_1
                            break
                        depth_1 = depth_1 + 1 
                    #print("first enddo ln: "+str(frist_enddo_line_number)+"  "+self.ftext[frist_enddo_line_number]  )  
                    if frist_enddo_line_number != 0:
                        for i in range(l+1,frist_enddo_line_number+1):
                            if re.search(r"(?i)^\s*!",self.ftext[i]):
                                continue
                            if re.search(r"(?i)^\s*do\s+[^\d]",self.ftext[i]):
                                num_do_between = num_do_between + 1
                    #print("do between: "+str(num_do_between))

                    while cnt_enddo != num_do_between:
                        #print(self.ftext[frist_enddo_line_number+depth_2])
                        if re.search(r"(?i)^\s*!",self.ftext[frist_enddo_line_number+depth_2]):
                            #print(depth_2)
                            depth_2 = depth_2 + 1
                            continue
                        if re.search(r"(?i)end\s*do\s*",self.ftext[frist_enddo_line_number+depth_2]):
                            cnt_enddo = cnt_enddo + 1
                            #print("find a enddo: "+str(cnt_enddo))
                        depth_2 = depth_2 + 1


                    #print(self.ftext[l],self.ftext[l+depth_2+depth_1-1])    
                    #print(di,l,l+depth_2+depth_1-1,-999,0)
                    cdo = class_do(di,l,l+depth_2+depth_1-1,-999,0)
                    self.class_do_list.append(cdo)
                    di =di + 1
                    #if len(self.class_do_list) == 0 :
                    #    self.class_do_list.append(cdo)
                    #    print(cdo)
                    #    continue

                    #if self.class_do_list[-1].do_level==0:
                    #    self.class_do_list.append(cdo)
                    #    self.class_do_list[-1].do_level = 1
                    #else:
                    #    self.class_do_list[-1].do_level += 1
                    #    print(self.class_do_list[-1].do_label)
                    #is_succ = True
                    #if the the do label encountered, increase the level
                    #if ld == cdo.do_label and is_succ:
                    #    self.class_do_list[-1].do_level = self.class_do_list[-1].do_level
                        #cdo.do_level = cdo.do_level + 1
            else:
                pass
                    #print(l,s)
            #find label  
            #determine the end line number of class_do
            for i,clb in enumerate((self.class_label_list)):
                #print(self.class_label_list[i].label_id)
                for j,cdo in enumerate((self.class_do_list)):
                    if clb.label_id == cdo.do_label:
                        #print(clb.label_id)
                        self.class_do_list[j].end_line_number = clb.line_number#self.start_line_number + clb.line_number
        #print("-----------------{0}--------------------".format(self.fname))
        for cdo in self.class_do_list:
            cdo_st_ln = cdo.start_line_number
            cdo_ed_ln = cdo.end_line_number
            if self.fname=="tiblset":
                #print("debug11",self.ftext[cdo_st_ln],self.ftext[cdo_ed_ln])
                pass
        #print("-----------------{0}--------------------".format(self.fname))
        

    def update_goto_statement(self):
        self.update_label()
        self.update_goto_statement_start()
        self.update_goto_statement_end()   

    #find goto statement
    def update_goto_statement_start(self):
        self.class_goto_list = []
        #find all goto statement, and add it to self.class_goto_list
        iidx = 0
        for l,s in enumerate(self.ftext):
            if re.search(r"^\s*!",s):
                continue
            
            res = re.search(r"(?i)\Wgo\s*to\W\d+",s)
            res_mark = re.search(r"!",s)
            start_mark_pos = -1
            if res_mark is not None:
                start_mark_pos = res_mark.start()            
                #print(start_mark_pos)

            if res is not None:
                start_pos = res.start()
                #find string like !goto xxx just skip it
                if res_mark is not None:                  
                    #print(s)
                    if start_pos > start_mark_pos:
                        # print(start_pos , start_mark_pos)
                        # print(s)
                        continue
                
                #print(res.group(0))
                #g = res.group()
                #print(res.start())
                #print("debug7",s[s:start_pos])
                
                rs = re.search(r"(?i)(?<=to)\s*\d+",s)
                #print(l,s)
                if rs is not None:
                    # if self.fname == "output":
                    #         print("zgy1: "+ s)
                    #print(s)
                    label = rs.group(0).strip()
                else:
                    print("ese",s)
                    continue
                tcg2 = class_goto(iidx,l,s,int(label))
                #print("clline"+str(l)+s)
                iidx += 1
                self.class_goto_list.append(tcg2)
                #print(s)
        if self.fname == "SNSQ":
            for cg2 in self.class_goto_list:
                cg2_sl = cg2.get_start_line_number()
                #print("debug109",self.ftext[cg2_sl])
            
    def update_label(self):
        self.class_label_list=[]
        
        for l,s in enumerate(self.ftext):
            clb_idx = 0
            se = 0
            l2e = 0
            is_return_label =False
            if re.search(r"^\s*\d+\s*",s[0:6]):
                se = l
                for i in range(l+1, len(self.ftext)):
                    if re.search(r"^\s*!",self.ftext[i]):
                        continue
                    if re.search(r"^\s*\d+\s*",self.ftext[i]):
                        break
                    if re.search(r"(?i)\s*((if)|(do))",self.ftext[i]):
                        break

                    if re.search(r"(?i)^\s*stop",self.ftext[i]):
                        l2e = i-l

                #len_do_label = len(lds)

                #TODO should check is the return clause after !
                if re.search(r"(?i)return",s):
                    is_return_label = True
                
                clb = class_label(clb_idx,l,int(s[0:6].strip()), line_to_end=l2e, is_return=is_return_label)
                for i in range(0, l2e):

                    line = self.ftext[se + i][5:]
                    clb.append_text(line)
                self.class_label_list.append(clb)
                clb_idx += 1

    #find crossponding label
    def update_goto_statement_end(self):
        #print("###########update_goto_statement_end start##############")
        for g2idx,cg2 in enumerate(self.class_goto_list):
            sln = cg2.line_number
            for lbidx, clb in enumerate(self.class_label_list):
                if cg2.label == clb.label_id:
                    self.class_goto_list[g2idx].set_se_line_number(sln,clb.line_number)
                    #self.class_goto_list[g2idx].end_line_number = clb.line_number

            sln = self.class_goto_list[g2idx].start_line_number
            eln = self.class_goto_list[g2idx].end_line_number
            #if self.fname == "SNSQ":
                #print("debug109",self.class_goto_list[g2idx].start_line_number,self.class_goto_list[g2idx].end_line_number)
                #print("debug109",self.ftext[sln],self.ftext[eln])

        #print("###########update_goto_statement_end end################")
    
    def update_line_number(self,inserted_ln):
        for clbidx,clb in enumerate(self.class_label_list):
            t_clb_ln = clb.get_line_number()
            if t_clb_ln >= inserted_ln:
                 self.class_label_list[clbidx].set_line_number(t_clb_ln+1)
            
        for cg2idx, cg2 in enumerate(self.class_goto_list):
            t_cg2_sl = cg2.get_start_line_number()
            t_cg2_el = cg2.get_end_line_number()
            if t_cg2_sl >= inserted_ln:
                self.class_goto_list[cg2idx].set_start_line_number(t_cg2_sl + 1)
            if t_cg2_el >= inserted_ln:
                self.class_goto_list[cg2idx].set_end_line_number(t_cg2_el + 1)
        
        for cdoidx,cdo in enumerate(self.class_do_list):
            t_cdo_sl = cdo.get_start_line_number()
            t_cdo_el = cdo.get_end_line_number()
            if inserted_ln <= t_cdo_sl:
                self.class_do_list[cdoidx].set_start_line_number(t_cdo_sl + 1)
            
            if inserted_ln <= t_cdo_el:
                self.class_do_list[cdoidx].set_end_line_number(t_cdo_el + 1)
        
        for cifidx, cif in enumerate(self.class_if_then_endif_list):
            t_cif_sl = cif.get_start_line_number()
            t_cif_el = cif.get_end_line_number()
            if inserted_ln <= t_cif_sl:
                self.class_if_then_endif_list[cifidx].set_start_line_number(t_cif_sl + 1)
            if inserted_ln <= t_cif_el:
                self.class_if_then_endif_list[cifidx].set_end_line_number(t_cif_el + 1)

        pass

    def is_clb_in_do(self,clb):
        #self.update_goto_statement()
        self.update_do_label_statement()
        for cdoidx,cdo in enumerate(self.class_do_list):
            cdo_sl = cdo.get_start_line_number()
            cdo_el = cdo.get_end_line_number()
            clb_sl = clb.get_line_number()
            if clb_sl <= cdo_el and clb_sl >= cdo_el:
                return True
            
        return False

    def modify_do(self):
        pdo = r"(?<=do)\s+\d+\s*"
        pdol = r"(?<=)"
        patdo = re.compile(pdo)
        #label in do ?
        lind = False
        lds = []
        do_dict = OrderedDict()
        start_do = False
        detect_return = False
        lad = 0
        iidx = 0
        #cg2 = class_goto()
        print("--{0}-----{1}----{2}--{3}----{4}---".format(self.order,self.fname,self.ng2,self.start_line_number,self.end_line_number))
        #for l,s in enumerate(self.statements):
        di = 0
        #cdo = class_do(0,0,0,0)
        do_level = 0
        do_stack = []
        is_succ = False
        do_level = 0
        clb_idx = 0
        for l,s in enumerate(self.ftext):

            if re.search(r"(?i)\s*do\s+",s):
                if patdo.search(s) is not None:
                    pass
                else:
                    pass
            #find a do statement in text
            if re.search(r"(?i)\s*do\s+",s):
                #get the do label if exists
                r = patdo.search(s)
                start_do = True
                #label do exists
                if r is not None :
                    ld = r.group(0).strip()
                    lds.append(ld)
                    do_dict[l] = s
                    cdo = class_do(di,l,l,int(ld),0)
                    cdo.start_line_number = l#self.start_line_number + l
                    cdo.idx = di
                    cdo.end_line_number = 0
                    cdo.do_label = int(ld)
                    #level start at 0, increase it below
                    cdo.do_level = 0
                    self.class_do_list.append(cdo)
                    di =di + 1
                    
                    #if len(self.class_do_list) == 0 :
                    #    self.class_do_list.append(cdo)
                    #    print(cdo)
                    #    continue

                    #if self.class_do_list[-1].do_level==0:
                    #    self.class_do_list.append(cdo)
                    #    self.class_do_list[-1].do_level = 1
                    #else:
                    #    self.class_do_list[-1].do_level += 1
                    #    print(self.class_do_list[-1].do_label)
                    #is_succ = True
                    #if the the do label encountered, increase the level
                    #if ld == cdo.do_label and is_succ:
                    #    self.class_do_list[-1].do_level = self.class_do_list[-1].do_level
                        #cdo.do_level = cdo.do_level + 1
                    

                else:
                    pass
                    #print(l,s)
            #find label         
            if re.search(r"^\s*\d+\s*",s[0:6]):
                len_do_label = len(lds)
                clb = class_label(clb_idx,l,int(s[0:6].strip()))
                self.class_label_list.append(clb)
                clb_idx += 1
                #self.label_list.append(s[0:6].strip())
                #print(len_do_label)
                #if s[0:6].strip() in lds:
                #    is_succ = True
                """
                for idx,ld in enumerate((lds)):
                    if s[0:6].strip() == ld:
                        #print(idx,ld)
                        #pass
                        #self.statements2insert[l] = "\t\tenddo"
                        #self.insert_num.append(l)
                        if idx == 0:
                            #print()
                            self.class_do_list[idx].end_line = self.start_line_number + l
                            #cdo.end_line_number = self.start_line_number + l
                            #self.class_do_list.append(cdo)
                            #print(cdo.do_level,cdo.start_line_number)
                            #do_level = do_level + 1
                            is_succ = False
                        #the same do label
                        elif lds[idx-1]==lds[idx]:
                            is_succ = True
                            
                            #do_level = do_level - 1
                            #pass
                        #condition that two succsive do label are not equal
                        elif lds[idx-1] != lds[idx]:
                            self.class_do_list[idx-1].end_line = self.start_line_number + l
                            #cdo.end_line_number = self.start_line_number + l
                            #self.class_do_list.append(cdo)
                            is_succ = False
                            #do_level = do_level+1
                            

                        #is_succ = False
                        #print(ld)
                        #print(l,s)
                """
            #find all goto statement, and add it to self.class_goto_list
            if re.search(r"(?i)\s*go\s*to\s*\d+",s):
                rs = re.search(r"(?i)(?<=to)\s*\d+",s)
                #print(l,s)
                if rs is not None:
                    label = rs.group(0).strip()
                else:
                    print("ese",s)
                    continue
                tcg2 = class_goto(iidx,l,s,int(label))
                #print("clline"+str(l)+s)
                iidx += 1
                self.class_goto_list.append(tcg2)
                #print(s)
            
            #find exit label of a subprogram
            if re.search(r"(?i)\s*\d+\s*return",s):
                #print(s)
                detect_return = True
                rs = re.search(r"(?i)\d+",s)
                #print(rs)
                if rs is not None:
                    exit_label = rs.group(0).strip()
                    #print("el:"+exit_label)
                    self.exit_label = exit_label
            if detect_return and re.search(r"\s*end[ ]*$",s):
                #print(s)
                detect_return = False
        #print(len(self.class_label_list))
        #determine the end line number of class_do
        for i,clb in enumerate((self.class_label_list)):
            #print(self.class_label_list[i].label_id)
            for j,cdo in enumerate((self.class_do_list)):
                if clb.label_id == cdo.do_label:
                    #print(clb.label_id)
                    self.class_do_list[j].end_line = clb.line_number#self.start_line_number + clb.line_number

        for idx, cdo in enumerate(self.class_do_list):
            #print("do_list",cdo.do_label,cdo.start_line_number,cdo.end_line_number)
            offset = 0
            have_goto = False
            search_start = cdo.start_line_number - self.start_line_number
            search_end   = cdo.end_line_number - self.start_line_number
            if idx != len(self.class_do_list)-1:
                self.class_do_list[idx+1].end_line += 1 + idx
                self.class_do_list[idx+1].start_line -= 1 + idx
            for i in range(search_start,search_end + 1 ):
                if re.search(r"(?i)do\s+\d+",self.ftext[i]):
                    if re.search(cdo.do_label,self.ftext[i]):
                        search_start = i
                if re.search(r"(?i)go\s*to\s+",self.ftext[i]):
                    have_goto = True
                    #print(self.ftext[i])
            if have_goto == True:
                #print("have goto in do statement")
                continue
            else:
                for i in range(search_start,search_end + 1 ):
                    #print(self.ftext[i])
                    if re.search(r"\s*\d+\s*continue",self.ftext[i]):
                        #print(self.ftext[i])
                        #self.ftext.pop(i)
                        if re.search(cdo.do_label,self.ftext[i][0:6]):
                            self.ftext.insert(i,"      enddo !add by @creaqi {0}".format(cdo.do_label))
                            break
                self.ftext[search_start] = re.sub(r"(?i)(?<=do)\s*\d+","",self.ftext[search_start])
                #self.ftext[search_end]   = re.sub(r"(?i)")
                #r = re.sub(r"^\s{0,5}\d{0,5}\s*continue\s*","")
                #print("sss",self.ftext[search_end+idx])
                #print(self.ftext[search_start],self.ftext[search_end])


        #subsitute goto end label with stop
        for cg2 in self.class_goto_list:
            if cg2.label == self.exit_label:
                #self.statements[cg2.line_number]=re.sub(r"(?i)go\s*to\s*\d+","stop",self.statements[cg2.line_number])
                #print(str(cg2.line_number)+self.ftext[cg2.line_number])
                self.ftext[cg2.line_number]=re.sub(r"(?i)go\s*to\s*\d+"," stop",self.ftext[cg2.line_number])
                #print("goto end"+ self.ftext[cg2.line_number]) 
                #print("goto:"+str(cg2.line_number),self.statements[cg2.line_number])
                #self.statements[line_number] = cg2.
                #print(cg2.statement)
                pass
            #print(cg2.idx,cg2.line_number,cg2.statement)
        
        for cdo in self.class_do_list:
            relative_start = cdo.start_line_number - self.start_line_number
            relative_end   = cdo.end_line_number - self.start_line_number
            #print(self.ftext[cdo.start_line_number - self.start_line_number],self.ftext[cdo.end_line_number - self.start_line_number])
            self.ftext[relative_start] = re.sub(r"(?i)(?<=do)\d+\s","",self.ftext[relative_start])
            #self.ftext.remove(relative_end)
            for i in range(0,cdo.do_level):
                self.ftext.insert(relative_end+i,"      enddo ! {0} and {1} add by @rho ".format(i,cdo))
                #print(i,cdo)
            
            
            #print(cdo.start_line_number,cdo.end_line_number,cdo.do_level)

        for idx,state in enumerate(self.statements):
            if idx in self.insert_num:
                #print(self.statements2insert[idx])
                pass
            #print(state)
            pass



        #print(len(do_dict.items()))

    def show_property(self):
        nj = 16
        cor = "+---------------+----------------+"
        bli = "|---------------|----------------|"
        print(cor)
        print("| property name |   value        |")
        print(bli)
        print("| order         |"+"   {0}       ".format(self.order).ljust(nj)+"|")
        print(bli)
        print("| name          |"+"   {0}       ".format(self.fname).ljust(nj)+"|")
        print(bli)
        print("| ftype         |"+"   {0}   ".format(self.ftype).ljust(nj)+"|")
        print(bli)
        print("| flines        |"+"   {0}       ".format(self.nlines).ljust(nj)+"|")
        print(bli)
        print("| nstatements   |"+"   {0}       ".format(self.nstatements).ljust(nj)+"|")
        print(bli)
        print("| ncomments     |"+"   {0}       ".format(self.ncomments).ljust(nj)+"|")
        print(bli)
        print("| g2            |"+"   {0}       ".format(self.ng2).ljust(nj)+"|")
        print(cor)

    def show_statements(self):
        for idx,state in enumerate(self.statements):
            print(str(idx)+" "+state)

    def show_content(self):
        #print(type(self.ftext))
        for idx,line in enumerate(self.ftext):
            print(str(idx)+line)

class class_label:
    def __init__(self,idx,line_number,label_id, line_to_end = 0, is_return = False):
        self.idx = idx
        self.line_number = line_number
        self.label_id = label_id
        self.line_to_end = line_to_end
        self.text = []
        self.is_return =is_return

    def get_label_id(self):
        return self.label_id
    def set_label_id(self,label_id):
        self.label_id = label_id
    def get_line_number(self):
        return self.line_number
    def set_line_number(self,line_number):
        self.line_number = line_number
    def get_idx(self):
        return self.idx
    def set_idx(self,idx):
        self.idx = idx
    
    def get_line_to_end(self):
        return self.line_to_end

    def set_line_to_end(self):
        self.line_to_end = line_to_end

    def get_text(self,i):
        return self.text[i]

    def append_text(self,line):
        self.text.append(line)
    
class class_goto:
    order = 0
    fn = ""
    preceed_label = []
    post_label = []
    def  __init__(self,idx,line_number,statement,label):
        self.idx = idx
        self.line_number = line_number
        self.statement = statement
        self.label = label
        self.start_line_number = 0
        self.end_line_number = 0
    
    def set_se_line_number(self,start_line_number,end_line_number):
        self.start_line_number = start_line_number
        self.end_line_number = end_line_number
        
    def get_line_number(self):
        return self.start_line_number,self.end_line_number

    def get_label_id(self):
        return self.label
    def set_label_id(self,label_id):
        self.label = label_id
    def get_start_line_number(self):
        return self.start_line_number
    def set_start_line_number(self,start_line_number):
        self.start_line_number = start_line_number
    def get_end_line_number(self):
        return self.end_line_number 
    def set_end_line_number(self,end_line_number):
        self.end_line_number = end_line_number
    def get_idx(self):
        return self.idx
    def set_idx(self,idx):
        self.idx = idx

class class_um_if:
    def __init__(self,idx):
        pass

class class_do:
    def __init__(self,idx,start_line_number,end_line_number,do_label,level):
        self.idx = idx
        #do numeric label
        self.start_line_number = start_line_number
        #numeric label continue
        self.end_line_number = end_line_number
        self.do_label = do_label
        self.do_level = level
    
    def get_label_id(self):
        return self.do_label
    def set_label_id(self,label_id):
        self.label_id = label_id
    def get_start_line_number(self):
        return self.start_line_number
    def set_start_line_number(self,start_line_number):
        self.start_line_number = start_line_number
    def get_end_line_number(self):
        return self.end_line_number
    def set_end_line_number(self,end_line_number):
        self.end_line_number = end_line_number
    def get_idx(self):
        return self.idx
    def set_idx(self,idx):
        self.idx = idx

class class_if:
    def __init__(self,idx, start_line_number, end_line_number, deep_if, has_then, identity = -666):
        self.start_line_number = start_line_number
        self.end_line_number = end_line_number
        self.idx = idx
        self.deep_if = deep_if
        self.has_then = has_then
        self.identity = identity

    def get_start_line_number(self):
        return self.start_line_number
    
    def set_start_line_number(self,start_line_number):
        self.start_line_number = start_line_number
    
    def get_end_line_number(self):
        return self.end_line_number
    
    def set_end_line_number(self, end_line_number):
        self.end_line_number = end_line_number

    def get_idx(self):
        return self.idx

    def set_idx(self, idx):
        self.idx = idx

    def get_deep_if(self):
        return self.deep_if

    def set_deep_if(self, deep_if):
        self.deep_if = deep_if
    
    def has_then(self):
        return has_then

    def get_identity(self):
        return self.identity

    def set_identity(self, identity):
        self.identity = identity

class class_endif:
    def __init__(self,idx,line_number):
        self.idx = idx
        self.line_number = line_number

    def get_idx(self):
        return self.idx

    def set_idx(self,idx):
        self.idx = idx

    def get_line_number(self):
        return self.line_number

    def set_line_number(self,line_number):
        self.line_number = line_number

class class_if_then_endif(class_if):
    def __init__(self,cif):
        self.start_line_number = cif.start_line_number
        self.end_line_number = cif.end_line_number
        self.idx = cif.idx
        self.deep_if = cif.deep_if
        self.has_then = cif.has_then
    # def __init__(self, idx, start_line_number, end_line_number,deep_if):
    #     self.idx = idx
    #     self.start_line_number = start_line_number
    #     self.end_line_number = end_line_number
    #     self.deep_if = deep_if

    # def get_idx(self):
    #     return self.idx
    
    # def set_idx(self,idx):
    #     self.idx = idx

    # def get_start_line_number(self):
    #     return self.start_line_number()

    # def set_start_line_number(self,start_line_number):
    #     self.start_line_number = start_line_number

    # def get_end_line_number(self):
    #     return self.end_line_number

    # def set_end_line_number(self,end_line_number):
    #     self.end_line_number = end_line_number

    # def get_deep_if(self):
    #     return self.deep_if
    
    # def set_deep_if(self,deep_if):
    #     self.deep_if = deep_if

    
    
def is_in_range(start, end, test):
    return (test>start) and (test < end)     


def test_os():
    print(sys.platform)
    print(os.name)
    print(os.uname())

def get_content_f2l(fn):
    if os.path.exists(fn):
        with open(fn,"r") as f:
            return f.readlines()
def compile_pattern(pat):
    return re.compile(pat)



def get_subroutine(con):
    #lines = re.findall(".*",con)
    #lines = re.findall(r"(?i)^\s*subroutine\s+\w+",con)
    lines = con
    p1 = r"(?i)^\s*subroutine\s+(\w+\(.*\)){0,1}"
    p2 = r"(?i)^\s*\w*\s*function\s+(\w+\(.*\)){0,1}"
    spat = p1+"|"+p2
    #print(spat)
    pat = re.compile(p1+"|"+p2)
    #pat.match()
    #print(len(lines))
    sp_cnt=0
    #subr_ls=[[] for x in range(500)]
    sp_idx = 0
    for idx,line in enumerate(lines):
        if pat.search(line):
            #print(line)
            #subr_ls[sp_cnt].append(line)
            sgl=[]
            subr_ls.extend(sgl)
            sp_cnt=sp_cnt+1
        subr_ls[sp_cnt-1].append(re.sub(r"\n$","",line))
    #print(idx)
    #print(line)
        #print(line)
        #pass
    #print(sp_cnt)
    #print(len(subr_ls))

    return lines,sp_cnt,subr_ls 

#con = "subroutine aaa()"
    #pat = re.compile("subroutine")
    #result = pat.match(con)
    #print(result)
    #print(result.groups())

def get_num_cmt():
    pass

def get_ng2(sp_cnt,sbur_ls):
    #print(len(subr_ls[0]))
    p1 = r"(?i)go\s*to"
    p2 = r"^\s*!"
    pat1 = re.compile(p1)
    pat2 = re.compile(p2)
    n_g2=0
    rn_g2=OrderedDict()
    for idx,fun in enumerate(subr_ls):
        if idx < sp_cnt:
            n_g2=0
            #print(subr_ls[idx][0])
            for line in subr_ls[idx]:
                #print(line)
                if pat2.search(line):
                    continue
                if pat1.search(line):
                    #print(line)
                    ksp = line
                    n_g2 = n_g2 + 1
            #print(fun[0])
            g = re.search(r"(?i)((?i)(?<=subroutine)|(?<=function))\s*\w*",fun[0])

            if g is not None:
                #print(g.group(0))
                rn_g2[g.group(0).strip()] = n_g2
                #pass
            else:
                print("err",fun[0])
            #pass
        if idx == 0:
            for fl in subr_ls[idx]:
                #print(fl)
                pass


    return rn_g2


def ng2_in_fun(rn_g2):
    for fun,ng2 in rn_g2.items():
        print(fun,ng2)

def get_wl(rn_g2,ss,se):
    wl_g2 = 0
    wl_nf = 0
    fws = 0x3f3f3f3f
    fwe = 0x3f3f3f3f
    for idx,(fun,ng2) in enumerate(rn_g2.items()):
        #print(idx,fun,ng2)
        if fun == ss:
            fws = idx
            #print("start",fws,fun)
        if fun == se:
            fwe = idx
            #print("end",fwe,fun)
        if fws <= idx and fwe >= idx :
            #print(idx,fun,ng2)
            wl_g2 = wl_g2 + ng2
            wl_nf = wl_nf + 1
            #print(fwe,idx)
            pass

    return wl_nf,wl_g2
if __name__ == "__main__":
    #test_os()
    cdir = os.getcwd()
    fn = os.path.join(cdir,"savedir6","calpuff.f90")
    con = get_content_f2l(fn)

    subr_ls=[[] for x in range(500)]
    lines,sp_cnt,subr_ls = get_subroutine(con)
    #print(sp_cnt)
    spng2=get_ng2(sp_cnt,subr_ls)
    #ng2_in_fun(spng2)
    wng2,wnf = get_wl(spng2,"qainp","tiblgro")
    #print(wng2,wnf)

    fm = file_mannager()



    

    #print(spng2)
    l = []
    s="sss\n"
    l.append(s)
    #print(l[0])
    calpuff = subprogram_manager(fn)
    ws = calpuff.get_order("wrfiles")
    we = calpuff.get_order("tiblgro")
    print(ws,we)
    #exit()
    calpuff.set_se_idx(0,we)
    calpuff.show_ifu_ng2(ws,we)
    #print(ws,we)
    iter_puff = iter(calpuff)
    ng2 = iter_puff.get_goto_number(ws,we)
    print("now number of goto statement is {0}".format(ng2))
    iter_puff.modify_goto_end_label()
    iter_puff.modify_do_label()
    iter_puff.modify_goto_statement()
    ng2 = iter_puff.get_goto_number(ws,we)
    print("now number of goto statement is {0}".format(ng2))
    iter_puff.write_to_file("puff3.f90","wrfiles","tiblgro")
    iter_puff.write2file_full("calpuff_2.f90")



    #shutil.copy("calpuff_1.f90","../../calpuff_1.f90")
    #os.system("cd ../../ && ./compile_run.sh")


    # subname = os.path.join(cdir,"../../", "calpuff1.f90")
    # besubname = os.path.join(cdir,"./", "puff3.f90")
    # if not os.path.exists(subname):
    #     print("subname not exist!")
    #     exit(-1)
    # if not os.path.exists(besubname):
    #     print("besubname not exist!")
    #     exit(-1)
    # sub_sm = subprogram_manager(subname)
    # be_sub_sm = subprogram_manager(besubname)

    # sm_list = [sub_sm, be_sub_sm]
    # name_list = ["sub_sm", "be_sub_sm"]


     

    # fm.add_subprogram_manager(name_list,sm_list)

    # fm.get_name_list(sub_sm,"wrfiles","tiblgro")

    # fm.subprogram_manager_range(sub_sm,be_sub_sm)
    
    #iter_puff.show_ifu_ng2(ws,we)
    
    # puff3_fn = os.path.join(cdir,"puff3.f90")
    # cls_puf3 = collect_subprogram(puff3_fn)
    # ws = cls_puf3.get_order("wrfiles")
    # we = cls_puf3.get_order("tiblgro")
    # print(ws,we)
    # cls_puf3.set_se_idx(ws,we)
    # cls_puf3.show_ifu_ng2(ws,we)

    #print(iter_puff)
    #fun = next(iter_puff)
    #fun.show_property()
    """
    while True:
        try:
            fun = next(iter_puff)
            fun.modify_do()
            #fun.show_content()

        except StopIteration:
            break
    
    iter_puff.update_text()
    iter_puff.write_to_file("puff1.f90")
    """
    #fun.get_statement()
    #print(fun.statements)
    #fun.show_statements()
    #fun.modify_do()
    #fun = next(iter_puff)
    #fun.show_property()
    
    #calpuff.show_fun_name()
    #calpuff.show_status()
    #calpuff.show_subprogram()

    #print(get_content_f(os.path.join(os.getcwd(),"savedir6","calpuff.f90")))

