/*
    ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
    º                    Config Update/Batch Editing                        º
    º                                                                       º
    º  Batch update of CONFIG.SYS-like files. CUBE modifies a Target ASCII  º
    º  file, given a set of commands in a Procedure file.                   º
    º                                                                       º
    º  2019-02-09 V3.0 RLW Adjust box-drawing characters for CP850          º
    º  2019-02-09 V3.0 RLW Correct endless REPSTRING loop                   º
    º  2019-02-09 V3.0 RLW Remove dead code                                 º
    º  2019-01-25 V2.9 SCS Remove the bootdrive hack - no longer needed     º
    º  2018-06-12 V2.8 SHL Correct comments - no functional changes         º
    º  2017-04-01 V2.8 SHL Correct ADDSTRING ALWAYS logic more              º
    º  2015-04-28 V2.7 SHL Correct ADDSTRING logic                          º
    º  2015-04-28 V2.7 SHL Avoid spurious REPLSTRING uppercasing logic      º
    º  2010-10-28 V2.6 YD  overwrite c:\ with the current bootdrive         º
    º                      (this addition was because of rpm installers)    º
    º  2009-12-06 V2.6 TJB Removed spurious 'say arg' from line 77          º
    º                      [see bug 1066]                                   º
    º                      Fixed bug in ADDSTRING section where it would    º
    º                      not process all relevant lines.                  º
    º                      Fixed bug in REPSTRING section where the         º
    º                      resulting line would be uppercased.              º
    º                                                                       º
    º  04/06/93: V2.6 - Add 'procedure read from QUEUE' (from Steve Farrell)º
    º  03/06/93: V2.5 - Add IF/IFNOT to xLINE cmds (generalize N. Marks req)º
    º                   Correct ADDSTRING BEFORE option  (Neil Marks)       º
    º  07/04/93: V2.4 - Corrected ADDBOTTOM/ADDTOP in ADDSTRING (Per Hertz) º
    º                   Address cmd + CHECK Option + new exit rtne          º
    º  21/01/93: V2.3 - Added user defined string delimiter in CUBE cmds    º
    º  21/12/92: V2.2 - Added conditionnal command processing (WHEN)        º
    º  26/11/92: V2.1 - RS() for DL, DS (desinstallation case)              º
    º                   New LINEID command (strip leading chars)            º
    º                   ADDTOP,ADDBOTTOM for AS (W. Pachl requirement)      º
    º                   Fix Whereis (Walter Pachl).                         º
    º                   Exit with SaveFile return code (Walter Pachl)       º
    º  18/11/92: V2.0 - Changes with environment variable substitution (RS) º
    º                       (AS, RS, AL & AL now all have same RS() option) º
    º                   Logging of all changes made to Target File          º
    º                   Adapt/Include some of Walter Pachl's enhancements:  º
    º                       Single CUBE command on command line             º
    º                       Add'l string substitution at command line level º
    º                       PAUSE option (debugging purposes)               º
    º  05/11/92: V1.5 - AL with pre substitution                            º
    º  03/11/92: V1.4 - AS with substitution ; fix RS recursion.            º
    º  02/11/92: V1.3 - Bug fixes & cmds abbrev, thanks to Walter Pachl.    º
    º                   Target Backup & lineid no more limited to col 1.    º
    º  30/10/92: V1.2 - Added env variable substitution + version #         º
    º  31/08/92: V1.1 - Bug fix                                             º
    º  21/07/92: V1.0 - Initial revision                                    º
    º  Didier LAFON - LAFON at CBEPROFS                                     º
    ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ */

'@echo off'
version = '2.9'
pf = 0
if left(arg(1),1) = '{' then parse arg '{'PFile'}' TFile Bkup . '(' Opt
else do
  parse arg PFile TFile Bkup . '(' Opt
  pf = 1
end

parse upper source source                          /* who am I ?              */

if pf then do
  if PFile = '' then call Exit 0 'no procedure !'    /* no or missing PFile     */
  if Pfile <> 'QUEUE' then if exists(PFile)='' then call Exit 0 PFile 'not found'
end
if TFile = ''  then call Exit 0 'no target !'      /* no or missing Tfile     */
if exists(TFile)='' then call Exit 0 TFile 'not found'

pause=(wordpos('PAUSE',translate(Opt))>0)          /* Pause mode ?            */
chkmd=(wordpos('CHECK',translate(Opt))>0)          /* Check mode ?            */
ap = wordpos('MAKE',translate(Opt))                /* make  specified ?       */
if ap>0 then Make=translate(word(Opt,ap+1))        /* ...when                 */
        else Make = '*'                            /* default make = all      */
MWhen='*'                                          /* Default when = all      */
dlm = wordpos('DLM',translate(Opt))                /* New delimiter specified?*/
if dlm>0 then _d_=left(word(Opt,dlm+1),1)          /*   yes use it            */
         else _d_ = '"'                            /* else use default        */
OnErr = 'STOP'                                     /* Default OnError setting */
LStrip= ''                                         /* No lineid strip         */
CaseM = 'I'                                        /* String compare default  */
call get_cmdrs(Opt)
NumberOfChanges = 0

say 'CUBE' version 'applying' PFile 'to' TFile 'on' date() time()
if Bkup <> "" then do
   address cmd 'copy' Tfile Bkup '1>nul 2>nul'
   if rc = 0 then say Tfile 'backup is:' Bkup
end

Proc. = ''
if pf then do
   If PFile = 'QUEUE' then Do
     i = 1
     Do Queued()
       Parse Pull procline
       proc.i = proc.i || upkw(procline)
       if right(Proc.i,1) = ','                       /*   continuation char ?   */
         then proc.i=left(proc.i,length(proc.i)-1)' ' /*     yes: blank it out   */
         else  i = i + 1                              /*     no: new Proc line   */
     end
     Proc.0 = i-1                                     /* Proc.0 = # of lines     */
     if Proc.0 <= 0 then call Exit 0 PFile 'empty'
  end
  Else do
     i = 1 ;                                          /* current Proc line: null */
     do while lines(PFile)                            /* for all PFile's lines   */
       Proc.i = Proc.i || upkw(linein(PFile))         /*   concat to Proc line   */
       if right(Proc.i,1) = ','                       /*   continuation char ?   */
         then proc.i=left(proc.i,length(proc.i)-1)' ' /*     yes: blank it out   */
         else  i = i + 1                              /*     no: new Proc line   */
     end
     Proc.0 = i-1                                     /* Proc.0 = # of lines     */
     call close PFile
     if Proc.0 <= 0 then call Exit 0 PFile 'empty'
  End
end
else do
  Proc.0 = 1
  Proc.1 = upkw(Pfile)
end

i = 0
do while lines(TFile)                              /* for all TFile's lines   */
  i = i + 1                                        /*   get line in           */
  Target.i = linein(TFile)                         /*   Target. stem          */
end
Target.0 = i                                       /* Target.0 = # of lines   */
call close Tfile

/*  ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
    ºThe real thing: go thru procedure file, interpret/execute its commands º
    ºsequentially.                                                          º
    ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ */

p = 0                                              /* Proc lines index        */
do while p <= Proc.0                               /* for all PFile's lines   */
  p = p + 1                                        /*   index next line       */
  if Proc.p = '' then iterate                      /*   ignore null lines     */
  parse var Proc.p Verb Parms                      /*   Isolate command verb  */
  say ''
  say '>>>' Proc.p
  Verb = translate(Verb)
  Select                                           /*   Process verb          */
    When left(Verb,1) = '*'   then iterate
    When left(Verb,2) = '--'  then iterate
               /* commands that always get executed  */
    When Verb = 'WHEN'        then call APPLYWHEN
    When Verb = 'ONERROR'     then call ONERROR
    When Verb = 'CASE'        then call CASE
    When Verb = 'LINEID'      then call SLINEID
    When wordpos(Make,MWhen)=0 then iterate
               /* commands executed when WHEN/MAKE match */
    When Verb = 'REPLINE'     | verb = 'RL'  then call REPLINE
    When Verb = 'ADDLINE'     | verb = 'AL'  then call ADDLINE
    When Verb = 'ADDSTRING'   | verb = 'AS'  then call ADDSTRING
    When Verb = 'DELSTRING'   | verb = 'DS'  then call DELSTRING
    When Verb = 'REPSTRING'   | verb = 'RS'  then call REPSTRING
    When Verb = 'COMMENTLINE' | verb = 'CL'  then call COMMENTL
    When Verb = 'DELLINE'     | verb = 'DL'  then call DELLINE
    Otherwise rc=OnErrorDo(p,"Don't know what to do")
  end
  if pause then Pull .
end
if chkmd = 0 then call exit 1 source 'ended.'      /* It's OVER !! and OK !!  */
if chkmd = 1 then call exit 2 source 'ended.'      /*                         */

/* ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
   º  Error report and action (based on Onerr setting, from ONERROR cmd)   º
   ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼  */
OnErrorDo:
  parse arg line,msg
  say PFile', line' line':' msg
  if OnErr = 'STOP' then call Exit 0 source 'stopped.'
                    else return 0

/* ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
   º  Searches All or First or Last lines in Target starting with string   º
   º  Returns the line number(s) found.                                    º
   ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼  */
Whereis: procedure expose Target. CaseM LStrip
 parse arg string,direction,mode
 if wordpos(direction,'F A')>0 then do; de=1; a=Target.0; par=1; end
                               else do; de=Target.0; a=1; par=-1; end
 stringlength=length(string); ret = ''
 do i = de to a by par
   If CaseM = 'S' then do; T = Target.i ; S = string ; end
      else do; T = translate(Target.i) ; S = translate(string) ; end
   If length(LStrip) = 1 then T = strip(T,'L',Lstrip)
   if mode=1 then do
     if left(T,stringlength)=S then do
       ret = ret i
       if direction \= 'A' then leave
     end
   end
   else do
     if pos(S,T) > 0 then do
       ret = ret i
       if direction \= 'A' then leave
     end
   end
 end
 return ret

/* ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
   º  Update Target file from Target. stem. Remove '       ' lines º
   ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼  */
SaveFile:
 address cmd 'erase' TFile
 src = rc
 if src = 0 then do
   do i = 1 to Target.0
     if Target.i = '       ' then iterate
     rc=lineout(TFile,Target.i)
   end
   call close Tfile
 end
 return src

/* ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
   º  Insert a line in Target file (stem) after line number i.             º
   ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼  */
Insert: procedure  expose Target. NumberOfChanges
 parse arg i string
 if i = Target.0 then k = Target.0 + 1
 else do
   do j = Target.0 to i+1 by -1
     k = j + 1
     Target.k = Target.j
   end
   k = i + 1
 end
 Target.k = string
 Target.0 = Target.0 + 1
 say 'Inserted line' k ': "'Target.k'"'
 NumberOfChanges = NumberOfChanges + 1
 return

/* ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
   º  returns a procedure command line  with all strings uppercased, exceptº
   º  doubled-quoted strings.                                              º
   ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼  */
upkw: procedure expose vn. vv. _d_
  parse arg sentence
  sentence = strip(sentence)
  phrase = ""
  do forever
    if sentence = '' then leave
    if left(word(sentence,1),1) = _d_ then do
       parse var sentence (_d_) y (_d_) sentence
       phrase = phrase _d_ || y || _d_
    end
    else do
       parse var sentence y sentence
       phrase = phrase translate(y)
    end
  end
  return phrase

/* ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
   º  apply command line-specified substitutions within a string           º
   ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼  */
Cmdrs: procedure expose vn. vv.
 parse arg y
 do i = 1 to vn.0
   out = ''
   do forever
     if pos(vn.i,y) > 0 then do
        parse var y x (vn.i) y
        out = out || x || vv.i
     end
     else leave
   end
 y = out || y
 end
 return y

/* ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
   º  apply env. variables substitutions to STRING if req. in OPTION.      º
   ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼  */
Envrs: procedure
  Parse arg String,Option
  out = ''
  parse var Option x 'RS('c')' .
  if length(c) = 1 then do
     do forever
        parse var String x (c) name (c) String
        if name = "" then leave
        out = out || x || value(name,,'OS2ENVIRONMENT')
     end
     String = out || x
  end
  return String

/* ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
   º All that must be done to quit and more: say msg, save Target file if  º
   º necessary (type=1).                                                   º
   ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼  */
Exit:
 parse arg type msg
 src=0
 Select
   When type=1 then do
        src = SaveFile()
        if src <> 0 then msg = msg 'Error writing' TFile
                    else msg = msg NumberOfChanges 'changes applied'
        end
   When type=2 then do
        src = NumberOfChanges
        msg = msg NumberOfChanges 'changes applied'
        end
   Otherwise nop
 End
 say msg
 Exit src

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ ONERROR [CONTINUE] [STOP] : what to do on syntax errors ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
ONERROR:
 if wordpos(translate(Parms),'CONTINUE STOP')>0 then OnErr = translate(Parms)
    else rc=OnErrorDo(p,'On Error what ?')
 return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ WHEN     ... wordlist of when codes ...                 ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
APPLYWHEN:
 MWhen = strip(translate(Parms),'B')
 if MWhen = '*' then MWhen = Make
 return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ CASE [SENSITIVE] [IGNORE] : string compare mode         ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
CASE:
 if wordpos(translate(Parms),'SENSITIVE IGNORE')>0 then CaseM = translate(left(Parms,1))
    else rc=OnErrorDo(p,'Case what ?')
 return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ LINEID [NOSTRIP] [STRIP "x"]                            ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
SLINEID:
 Select
   When word(translate(Parms),1) = 'NOSTRIP' then Lstrip = ""
   When word(translate(Parms),1) = 'STRIP' then do
      Parse var Parms 'STRIP' (_d_) ww (_d_)
      if length(ww) <> 1 then rc=OnErrorDo(p,'Strip leading what ?')
                         else Lstrip = ww
      end
   Otherwise rc=OnErrorDo(p,'Lineid what ?')
 end
 return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ REPLINE lineid WITH replacement [( options]             ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
REPLINE:
  parse var Parms (_d_) Lineid (_d_)  'WITH' (_d_) With (_d_)  '(' Opt
  if Lineid = '' then do                           /* No line identifier      */
    rc=OnErrorDo(p,'Replace what line ?')
    return
  end
  if With = '' then do                             /* No replacement string   */
    rc=OnErrorDo(p,'Replace line with ?')          /*   process error         */
    return                                         /*   ignore command        */
  end
  With = Cmdrs(With)                               /* cmd substitution        */
  With = Envrs(With,Opt)                           /* env substitution if req */
  dir = Searchdir(opt)                             /* What target lines ?     */
  mod = Lidmod(opt)                                /* floating line id ?      */
  select                                           /* What if no target lines?*/
    when wordpos('ADDTOP',Opt)>0 then after=0      /* add after line 0        */
    when wordpos('ADDBOTTOM',Opt)>0 then after=Target.0 /* add after last line     */
    when wordpos('DONTADD',Opt)>0 then after=-1    /* don't add               */
    otherwise after=-1                             /* don't add is the default*/
  end
  if ififnot() then return                         /* Process only when       */
  where = Whereis(Lineid,dir,mod)                  /* Get target lines numbers*/
  if where \= '' then do                           /* if target(s) found      */
    do until where = ''                            /*   process all targets   */
      parse var where w where                      /*     1 at a time         */
      was = Target.w                               /* save old value for log  */
      Target.w = With                              /*     target = replacmnt. */
      call logrep w,was,Target.w                   /*     log action          */
      if dir \= 'A' then leave                     /*     quit if not ALL     */
    end
  end
  else if after>-1 then call insert after With     /* if no target, try add   */
  return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ ADDLINE     line  [( options]                             ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ         */
ADDLINE:
  parse var Parms (_d_) Line (_d_)  '(' Opt
  if Line = '' then do                             /* No line identifier      */
    rc=OnErrorDo(p,'Add what line ?')              /*    process error        */
    return                                         /*    ignore command       */
  end
  select                                           /* When to add ?           */
    when wordpos('IFNEW',Opt)>0 then always=0      /*   if not already there  */
    when wordpos('ALWAYS',Opt)>0 then always=1     /*   even if already there */
    otherwise always=0                             /*   IFNEW is the default  */
  end
  Line = Cmdrs(Line)                               /* cmd substitution        */
  Line = envrs(Line,Opt)                           /* env substitution if req */
  mod = Lidmod(opt)                                /* floating line id ?      */
  exist = Whereis(Line,'F',mod)                    /* If this line exists and */
  if exist \= '' & always = 0 then return          /* IFNEW , don't add !     */
  if ififnot() then return

  select                                           /* Where to add ?          */
    when wordpos('AFTER',Opt)>0 then do;           /* 1) After a given line   */
      parse var Opt 'AFTER' (_d_) astr (_d_)  .    /*    line identifier ?    */
      if astr = '' then after = Target.0           /*    no id = add bottom   */
      else after = Whereis(astr,'F',mod)           /*    else get # of 1st    */
      parse var after after .                      /*    line with this id.   */
      if after='' then do                          /*    no match found       */
        if wordpos('ONLY',Opt)>0 then after=-1     /*      if ONLY, don't add */
           else after=Target.0                     /*      else add bottom    */
      end
    end
    when wordpos('BEFORE',Opt)>0 then do;          /* 2) Before a given line  */
      parse var Opt 'BEFORE' (_d_) bstr (_d_)  .   /*    line identifier ?    */
      if bstr = '' then after = 0                  /*    no id = add top      */
      else after = Whereis(bstr,'F',mod)           /*    else get # of 1st    */
      parse var after after .                      /*    line with this id.   */
      if after ='' then do                         /*    no match found       */
        if wordpos('ONLY',Opt)>0 then after=-1     /*      if ONLY don't add  */
           else after=0                            /*      else add top       */
      end
      else after=max(0,after-1)                    /*    match found          */
    end
    otherwise after=Target.0                       /* 3) default = add bottom */
  end
  if after \= -1 then call insert after Line       /* add the line            */
  return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ ADDSTRING string IN lineid [(Options]                     ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ         */
ADDSTRING:
  parse var Parms (_d_) With (_d_)  'IN' (_d_) Lineid (_d_)   '(' Opt
  if Lineid = '' then do                           /* No line identifier      */
    rc=OnErrorDo(p,'Add string where ?')           /*    process error        */
    return                                         /*    ignore command       */
  end
  if With = '' then do                             /* No string to add        */
    rc=OnErrorDo(p,'Add what string ?')            /*    process error        */
    return                                         /*    ignore command       */
  end
  With = Cmdrs(With)                               /* cmd substitution        */
  With = Envrs(With,Opt)                           /* env substitution if req */
  dir=Searchdir(opt)                               /* Which target line ?     */
  mod=Lidmod(opt)                                  /* floating line id ?      */
  where = Whereis(Lineid,dir,mod)                  /* Select target(s)        */
  if where \= '' then do                           /*   if target found       */
    do until where = ''                            /*     process target(s)   */
      parse var where w where                      /*     1 at a time         */
      if CaseM = 'S' then do
         Tar= Target.w; Wi = With; end             /*     string compare mode */
      else do
         Tar = translate(Target.w); Wi=translate(With); end  /*     string compare mode */
      if pos(Wi,Tar) = 0 |,                        /* String not there or    */
         wordpos('ALWAYS',Opt) <> 0 then do        /* ALWAYS specified.       */
      select                                       /* Where to add ?          */
        when wordpos('AFTER',Opt)>0 then do        /* 1) After a given string */
          astr=''                                  /*    defaulted to null    */
          parse var Opt 'AFTER' (_d_) astr (_d_)  .  /*    what is this string  */
          If CaseM = 'I' then astr=translate(astr)
          was = Target.w                           /*    save for logging     */
          if astr = '' | pos(astr,Tar)=0           /* if no string or no match*/
             then Target.w = Target.w || With      /*    add at end of target */
          else do
             parse var Tar xx (astr) rest          /* insert string after     */
             Target.w = xx || astr || With || rest /*   specified string      */
          end
          call logrep w,was,Target.w               /*   log action            */
        end
        when wordpos('BEFORE',Opt)>0 then do       /* 2) Before a given string*/
          bstr=''                                  /*    defaulted to null    */
          parse var Opt 'BEFORE' (_d_) bstr (_d_)  . /*    what is this string  */
          If CaseM = 'I' then bstr=translate(bstr)
          was = Target.w                           /*    save for logging     */
          if bstr = '' | pos(bstr,Tar)=0           /* if no string or no match*/
          then do                                  /* add at beginning        */
            If CaseM = 'I' then Lid=translate(Lineid)
            Parse var Tar (Lid) rest               /* but after identifier    */
            Target.w = Lineid || With || rest      /*                         */
          end
          else do
             parse var Tar xx (bstr) rest          /* insert string before    */
             Target.w = xx || With || bstr || rest /*   specified string      */
          end
          call logrep w,was,Target.w               /*   log action            */
        end
        otherwise nop
      end /* select */
      end /* ALWAYS specified */
      if dir \= 'A' then leave                     /* leave if not ALL targets*/
    end
  end
  else do                                          /* no target : add line ?  */
    if wordpos('ADDTOP',Opt)>0 then call insert 0 Lineid || With
    if wordpos('ADDBOTTOM',Opt)>0 then call insert Target.0 Lineid || With
  end
  return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ DELSTRING string [IN lineid] [(Options]                   ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
DELSTRING:
  parse var Parms (_d_) What (_d_)  'IN' (_d_) Lineid (_d_)  '(' Opt
  if Lineid = '' then parse var Parms (_d_) What (_d_)  '(' Opt
  if What = '' then do                             /* No string to del        */
    rc=OnErrorDo(p,'Delete what string ?')         /*    process error        */
    return                                         /*    ignore command       */
  end
  What = Cmdrs(What)                               /* cmd substitution        */
  What = envrs(What,Opt)                           /* env substitution if req */
  dir=Searchdir(opt)                               /* Which target line ?     */
  mod=Lidmod(opt)                                  /* floating line id ?      */
  where = Whereis(Lineid,dir,mod)                  /* Select target(s)        */
  if where \= '' then do                           /*   if target found       */
    do until where = ''                            /*     process target(s)   */
      parse var where w where                      /*     1 at a time         */
      do forever                                   /* for all occurences      */
        if CaseM = 'S' then do
           Tar = Target.w; Wh=What; end            /*     string compare mode */
        else do
           Tar = translate(Target.w); Wh=translate(What); end  /*     string compare mode */
        if pos(Wh,Tar) > 0 then do                 /*     String is there     */
           was = Target.w                          /*     save for logging    */
           parse var Tar xx (Wh) rest              /*     isolate string      */
           Target.w = xx || rest                   /*     delete string       */
           call logrep w,was,Target.w              /*     log action          */
        end
        else leave
      end
      if dir \= 'A' then leave                     /* leave if not ALL targets*/
    end
  end
  return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ REPSTRING ostring [WITH nstring] [IN lineid] [(Options]   ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
REPSTRING:
  parse var Parms (_d_) Ostr (_d_)  'WITH' (_d_) With (_d_)  'IN' (_d_) Lineid (_d_)  '(' Opt
  if Ostr = '' then do                             /* No old string specif.   */
    rc=OnErrorDo(p,'Replace what string ?')        /*    process error        */
    return                                         /*    ignore command       */
  end
  if With = '' then parse var Parms (_d_) Ostr (_d_)  'IN' (_d_) Lineid (_d_)  '(' Opt
  if Lineid = '' then parse var Parms (_d_) Ostr (_d_)  '(' Opt
  if With = '' then With = Ostr                    /* No rep string specif.   */
  With = Cmdrs(With)                               /* cmd substitution        */
  With = envrs(With,Opt)                           /* env substitution if req.*/
  dir=Searchdir(opt)                               /* Which target line ?     */
  mod=Lidmod(opt)                                  /* floating line id ?      */
  where = Whereis(Lineid,dir,mod)                  /* Select target(s)        */
  do while where \= ''                             /*   if target found       */
      parse var where w where                      /*     1 at a time         */
      was = Target.w
      newtar = ''
      do forever                                   /* for all occurences      */
        if CaseM = 'S' then do
           Tar = Target.w; Os=Ostr; end            /*     string compare mode */
        else do
           Tar = translate(Target.w); Os=translate(Ostr); end  /*     string compare mode */
        ospos = pos(Os,Tar)
        if ospos > 0 then do                       /*     String is there     */
           s = left(Target.w, ospos - 1)
           Target.w = substr(Target.w, ospos + length(os))
           newtar = newtar || s || With            /*     replace occurrence  */
        end
        else leave
      end
      Target.w = newtar || target.w
      call logrep w,was,Target.w                   /* log action              */
      if dir \= 'A' then leave                     /* leave if not ALL targets*/
  end
  return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ COMMENTLINE lineid WITH type [(options ]                ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
COMMENTL:
  parse var Parms (_d_) Lineid (_d_)  'WITH' (_d_) cmnt (_d_)  '(' Opt
  if Lineid = '' then do                           /* No identifier           */
    rc=OnErrorDo(p,'Comment what ?')               /*    process error        */
    return                                         /*    ignore command       */
  end
  if cmnt = '' then do                             /* No comment string       */
    rc=OnErrorDo(p,'Comment how ?')                /*    process error        */
    return                                         /*    ignore command       */
  end
  dir=Searchdir(opt)                               /* Which target lines ?    */
  mod=Lidmod(opt)                                  /* floating line id ?      */
  if ififnot() then return
  where= whereis(Lineid,dir,mod)                   /* get target lines #s     */
  if where \= '' then do                           /* if match(es) found      */
    do until where = ''                            /*   process target(s)     */
      parse var where w where                      /*   1 at a time           */
      was = Target.w                               /*  save for logging       */
      Target.w = cmnt Target.w                     /*   comment target        */
      call logrep w,was,Target.w                   /*   log action            */
      if dir \= 'A' then leave                     /* leave if not ALL targets*/
    end
  end
  return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ DELLINE lineid [(options ]                              ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
DELLINE:
  parse var Parms (_d_) Lineid (_d_)   '(' Opt
  if Lineid = '' then do                           /* No identifier           */
    rc=OnErrorDo(p,'Delete what line ?')           /*    process error        */
    return                                         /*    ignore command       */
  end
  Lineid = Cmdrs(Lineid)                           /* cmd substitution        */
  Lineid = envrs(Lineid,Opt)                       /* env substitution if req */
  dir=Searchdir(opt)                               /* Which target line(s) ?  */
  mod=Lidmod(opt)                                  /* floating line id ?      */
  if ififnot() then return
  where= whereis(Lineid,dir,mod)                   /* Get target lin(s) #s    */
  if where \= '' then do                           /* if match(es) found      */
    do until where = ''                            /*    process all targets  */
      parse var where w where                      /*    one at a time        */
      say 'Deleted line' w
      say '  was:' Target.w
      NumberOfChanges = NumberOfChanges + 1
      Target.w = '       '                 /*    mark for delete      */
      if dir \= 'A' then leave                     /* leave if not ALL targets*/
    end
  end
  return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ SEARCHDIR: Direction for line search in Target File     ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
Searchdir: procedure
  select                                           /* What target lines ?     */
    when wordpos('LAST',arg(1))>0 then dir='L'     /* set reverse search      */
    when wordpos('FIRST',arg(1))>0 then dir='F'    /* set forward search      */
    otherwise dir='A'                              /* default is all lines    */
  end
  return dir

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ LIDMOD: Identify line at 1st col or anywhere in line    ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
Lidmod: procedure
  if wordpos('*ID',arg(1))>0 then return(0)
                             else return(1)


/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ Stream functions (close & exists)                       ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
close:
  return stream(arg(1),'C','CLOSE')

exists:
  return stream(arg(1),'C','QUERY EXISTS')

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ get substitution variables and their values in cmdline. ³
        ³ ( RS(#aaa#=aaa #bbb#=bbb)                               ³
        ³Output: vn.i = number and names of subst. variables      ³
        ³        vv.i = values of these variables                 ³
        ³Adapted from Walter Pachl's                              ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
get_cmdrs: procedure expose vn. vv.
  parse arg Opt
  vn.0 = 0
  p = pos('RS(',translate(Opt))
  if p > 0 then do
    rs = substr(Opt,p+3)
    Parse var rs rs ')'
    Do i=1 By 1 While rs<>''
      rs=strip(rs,'L')
      Parse Var rs vn '=' vv rs
      if left(vn,1) = right(vn,1) then do
        vn.i = vn
        vv.i = vv
      end
      else call Exit 0 'Invalid substitution variables ('vn vv')'
    End
    vn.0=i-1
  end
  Return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ Log a change in a target line                           ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
logrep: procedure expose NumberOfChanges
parse arg ln,old,new
if old<>new then do
  say 'Changed line' ln
  say '  old: "'old'"'
  say '  new: "'new'"'
  NumberOfChanges = NumberOfChanges + 1
end

return

/*      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
        ³ Process IF/IFNOT logic for Lines commands               ³
        ³    returns 1 if IF/IFNOT condition is false !!          ³
        ³    returns 0 otherwise                                  ³
        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ           */
ififnot:
if wordpos('IFNOT',Opt)>0 Then Do                /* Process only when       */
  parse var Opt 'IFNOT' (_d_) istr (_d_) .       /* another line doesn't    */
  iexist = Whereis(istr,'F',mod)                 /* exists                  */
  if iexist \= '' then return(1)                 /*                         */
end                                              /*                         */
if wordpos('IF',Opt)>0 Then Do                   /* Process only when       */
  parse var Opt 'IF' (_d_) istr (_d_) .          /* another line exists     */
  iexist = Whereis(istr,'F',mod)                 /*                         */
  if iexist  = '' then return(1)                 /*                         */
end                                              /*                         */
return(0)
