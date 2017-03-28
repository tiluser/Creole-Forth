(*
Copyright 2003 Joseph M. O'Connor. https://github.com/tiluser

All rights reserved. (Derived from the FreeBSD copyright at http://www.freebsd.org/copyright/freebsd-license.html).

Redistribution and use in source and binary forms, with or without modification, are permitted provided that 
the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the 
      following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and 
      the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED `AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL 
THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those of the author and should not be 
interpreted as representing official policies, either expressed or implied.
*)

// Compilers, defining-word and compiling-word routines, part II.

UNIT Compiler2;

{$MODE Delphi}

// {$MODE Delphi}

INTERFACE
USES
  Dialogs, SysUtils, Math, classes, stdctrls, TILable, Coreprims,
  Compiler1, Interpreter, CrSysUtils;

PROCEDURE CompileList(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
PROCEDURE CompileList2(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
PROCEDURE CompileHelp(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
PROCEDURE DoMyself(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);


IMPLEMENTATION

// Mirrors colon compiler in action
// It simply places words one at a time in PAD
PROCEDURE CompileList(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  I, LookupIndex : integer;
  CurrentWord, TempStr : STRING;
BEGIN
  EI.Push(EI.VocabStack,'LIST');
    // First search list vocabulary. If word is found, then execute it. If
  // not, then search other vocabularies on the vocab stack.
  EI.SetOPtr(EI.GetOPtr+1);
  WHILE (EI.VocabStack[0] = 'LIST') DO
  BEGIN
    CurrentWord := ParseStrWSD(EI.Input.Text,EI.GetOPtr);
    // Only LIST vocabulary is searched.
    LookupIndex := DI.Dictionary.IndexOf(AppendVocab(CurrentWord,EI.VocabStack[0],'.'));
    // If in LIST vocabulary, execute.
    IF LookupIndex <> -1 THEN
    BEGIN
      CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
      DoInner(Owner,EI,DI,CI,CWI);
      CWI := NIL;
    END
    ELSE
      TempStr := TempStr + ' ' + CurrentWord;
    EI.SetOPtr(EI.GetOPtr+1);  
  END;
  // important to trim leading and trailing spaces
  EI.Push(EI.DataStack,Trim(TempStr));
END;
// Question : can this be the basis of 
// all the compilers?
// Builds a list and puts it on the stack
PROCEDURE CompileList2(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  I, LookupIndex : integer;
  CurrentWord, TempStr : STRING;
BEGIN
  EI.SetOPtr(EI.GetOPtr+1);
  LookupIndex := -1;
  WHILE (LookupIndex = -1) DO
  REPEAT
    CurrentWord := ParseStrWSD(EI.Input.Text,EI.GetOPtr);
    LookupIndex := DI.Dictionary.IndexOf(AppendVocab(CurrentWord,'LIST','.'));
    TempStr := TempStr + ' ' + CurrentWord;
    EI.SetOPtr(EI.GetOPtr+1);  
  UNTIL LookupIndex <> -1;  
  // important to trim leading and trailing spaces
  EI.Push(EI.DataStack,Trim(TempStr));
END;

PROCEDURE CompileHelp(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  RawWord : STRING;
  LookupIndex : integer;
BEGIN
  RawWord := ParseStrWSD(EI.Input.Text,EI.GetOPtr+1);
  LookupIndex := DI.SearchAvailVocabs(EI.VocabStack, RawWord);
  CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
  CWI.SetField('NameField',RawWord);
  EI.SetOPtr(EI.GetOPtr+1);
  // Place a list of words on the stack and then store.
  CompileList(Owner,EI,DI,CI,CWI);
  CWI.SetField('HelpField',Pop(EI.DataStack));
  CWI := NIL;
END;

PROCEDURE DoMyself(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  DoHere(Owner,EI,DI,CI,CWI);
  EI.PAD.Append(EI.Pop(EI.DataStack));
END;

end.
