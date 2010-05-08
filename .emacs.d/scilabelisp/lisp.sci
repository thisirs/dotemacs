function lisp(flag,cmd)
//Runs any emacs-lisp command "cmd" in the current emacs/Xemacs via
//gnuclient, if and only if gnuserv runs in the current emacs, 
//for flag see help on gnuclient    
//Examples: lisp([],"~/.emacs") - open ~/.emacs file
//          lisp(5,"~/.emacs") - open ~/.emacs file and put cursor on line 5
//          lisp("e","(eval-expression scilab-shell-command)" 
//          lisp("e","(font-lock-mode)") - toggle font-lock-mode
  if %version($)~="gnuclient"
    error("There is no gnuclient at this emacs envirounment."+...
	  " lisp cannot run")
  end // if %version($)~="gnuclient"
  gnuclient= "gnuclient -q  " 
  [nl,nr]=argn(0);
  if nr<2, 
    cmd=flag;
    flag="-eval"
  end // if nr<2, 
  if flag==[] | flag=="", flag=" "; end
  if typeof(flag)=="constant", flag=string(flag); end
  if typeof(flag)~="string", error("Wrong flag");end
  if part(flag,1)=="-"
    flag=part(flag,2);
  else
    flag=part(flag,1);
  end
  select flag   
   case "f"
    cmd=gnuclient+ "-f "+cmd;
   case "e" 
    cmd=strsubst(cmd,"\","\\");  
    cmd=strsubst(cmd,"""","\""");
    cmd= gnuclient+ "-eval """+cmd+"""";
   case "l" 
    cmd=gnuclient+ "-l "+cmd;
   case " "
    cmd=gnuclient+cmd;
  else
    cmd=gnuclient+"+"+flag+" "+cmd;
  end // select flag   
  unix(cmd)
endfunction // function lisp(flag,cmd)

function [out,err]=lisp_g(cmd)
//  Runs any lisp form like (...) in the current emacs/xemacs. 
//  Returns result into variable out. If result is string, boolean or 
//  numeric  then the correponding variable out is returned. For more
//  difficult objects out is tlist of type lisp_obj. The field "entry" of 
//  this tlist contains printable version of the lisp object.  
//  Examples:
//  out=lisp_g("scilab-shell-command")
//  out=lisp_g("font-lock-mode")
//  out=lisp_g("(message ""Hello Scilab/emacs user!"")")
//  out=lisp_g("(setq lisp-from-scilab ""We can do this!"")")
//  out=lisp_g("(sqrt 4)")
  out=[];
  [nl,nr]=argn(0);
  if %version($)~="gnuclient"
    str="There is no gnuclient at this emacs envirounment."+...
	  " lisp cannot run";
    if nl<2
      error(str)
    else
       err=str;
       warning(str);
       return;
    end // if nl<2
  end // if %version($)~="gnuclient"
  gnuclient= "gnuclient -q  " 
  lisp_out=getenv("TMPDIR")+"/lisp_out_"+unix_g("echo $$");
  cmd="(with-temp-file """+ lisp_out+""" (insert-string (with-output-to-string"+...
      " (print " + cmd+"))))"
  cmd=strsubst(cmd,"\","\\");  
  cmd=strsubst(cmd,"""","\""");  
  cmd= gnuclient+ "-eval """+cmd+"""";
  unix(cmd)
  out=read(lisp_out,-1,1,'(a)');
  err=unix_g("rm -f "+lisp_out)
  out=out(2:$);
  err=unix_g("rm -f "+lisp_out)
  if [part(out,1),part(out,length(out))]=="""" 
    out=part(out,2:length(out)-1);
    if out=="", out=""""""; end
  elseif out=="t"
    out=%t;
  elseif out=="nil"
    out=%f;
  else
    if or(part(out,1)==["-",string(0:9),"."]) & ...
	 or(part(out,length(out))==[string(0:9),"."])
      [outnew,ierr]=evstr(out);
      if ierr 
	out=tlist(["lisp_obj","entry"],out)
      else
	out=outnew;
      end // if ierr 
    else
      out=tlist(["lisp_obj","entry"],out)
    end // if or(part(out,1)==["-",string(0:9),".
  end // if part(out,[1,length(out)]=="""" 
endfunction // function lisp(flag,cmd)

function %lisp_obj_p(s)
  disp(s.entry)
endfunction // function %lisp_obj_p(s)




