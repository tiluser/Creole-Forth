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

// Outer and inner interpreters; runtime code for high-level definitions,
// and pre and post filtering routines.
unit interpreter;

{$MODE Delphi}

interface
USES
  Dialogs, SysUtils, Math, classes, stdctrls, TILable, Coreprims,
  CrSysUtils;

  PROCEDURE DoColon(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoOuter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoInner(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoExit(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoPreFilters(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
  CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoStripCmtPreFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
  CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoPostFilters(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
  CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoExecute1(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
  CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoNothingFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
  CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoIntegerFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
  CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoFloatFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
  CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoHexFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
  CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoToDS(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
  CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoExecute(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoTick(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

implementation

PROCEDURE DoOuter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce;
CWI : TCreoleWord);
LABEL
  Exit1;
VAR
  I, X, LookupIndex : integer;
  CurrentWord, RawWord : STRING;
BEGIN
  LookupIndex := -1;
  // Activate routine to preprocess input (and or other) fields. This could be
  // an empty routine or be a full-fledged language in itself.
  DoPreFilters(Owner,EI,DI,CI,CWI);
  // Next, examine the input field and process each word available one at a time.
  CWI := NIL;
  X := WordCntWSD(EI.Input.Text);
  EI.SetOPtr(0);
  // Examine each of the vocabularies on the vocabulary stack to find the word.
  WHILE (EI.GetOPtr < X) DO
  BEGIN
    RawWord := ParseStrWSD(EI.Input.Text,EI.GetOPtr);
    EI.SetField('DeNamespacedWord',RawWord);
    FOR I := 0 TO EI.VocabStack.Count-1 DO
    BEGIN
      CurrentWord := AppendVocab(RawWord,EI.VocabStack[I],'.');
      EI.SetField('CurrentWord',CurrentWord);
      LookupIndex := DI.Dictionary.IndexOf(CurrentWord);
      // If found, execute and exit. Otherwise search through the next vocabulary.
      IF (LookupIndex <> -1) THEN
      BEGIN
        CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
        DoInner(Owner,EI,DI,CI,CWI);
        CWI := NIL;
        GOTO Exit1;
      END;
    END;
    Exit1:
    IF (LookupIndex = -1) THEN DoPostFilters(Owner,EI,DI,CI,CWI);
    EI.SetOPtr(EI.GetOptr+1);
  END;
END;

PROCEDURE DoInner(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  PP : PrimProc;
  TestStr : STRING;
  X : integer;
BEGIN
  PP := CWI.CodeField;
  PP(Owner,EI,DI,CI,CWI);
END;

PROCEDURE DoColon(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  WordList : ARRAY[0..99] OF TCreoleWord; // Stores prefetched words from dictionary
  S : STRING;
  X, I, LookupIndex : integer;
  NewWord : TCreoleWord;
  Time1, Time2 : TDateTime;
  PP : ARRAY[0..99] OF PrimProc;
  PP1 : PrimProc;
BEGIN
  DI.SetIPtr(0);
  DI.SetField('ParamField',CWI.ParamField);
  DI.SetField('MetaField',CWI.MetaField);
  X := WordCntWSD(CWI.ParamField);
  // Prefetch word objects and procedural pointers into list.
  FOR I := 0 TO X-1 DO
  BEGIN
    WordList[I] := TCreoleWord.Create;
    WordList[I] := DI.Dictionary.Objects[CWI.ParamArray[I]] AS TCreoleWord;
    PP[I] := WordList[I].CodeField;
  END;
  Time1 := Now;
  // Now execute contents of the colon definition.
  WHILE (DI.InnerPtr < X) DO
  BEGIN
    PP1 := PP[DI.InnerPtr];       // Putting the value in the single variable seems
    NewWord := WordList[DI.InnerPtr]; // to make things move faster for some reason rather
                                  // than executing directly from the array.
    // The two lines of code below are equivalent to Push(EI.RSTop),
    // index of next word to execute.
    EI.RSTop := EI.RSTop + 1;
    EI.ReturnStack[EI.RSTop] := DI.InnerPtr + 1;
    PP1(Owner,EI,DI,CI,NewWord);
    // Equivalent of popping return stack.
    DI.InnerPtr := EI.ReturnStack[EI.RSTop];
    EI.RSTop := EI.RSTop - 1;
  END;
  Time2 := Now;
//  ShowMessage('Colon def. completed in ' + FloatToStr(100000*(Time2-Time1)) + ' sec.');
  DI.SetField('ParamField',CWI.ParamField);
  NewWord := NIL;
  FOR I := 0 TO X-1 DO
  BEGIN
    WordList[I] := NIL;
    WordList[I].Free;
  END;
END;

// Applies each filter rule that is on the prefilter stack in sequence
PROCEDURE DoPreFilters(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  I, LookupIndex : integer;
  CurrentWord : STRING;
BEGIN
  FOR I := 0 TO EI.PreFilterStack.Count-1 DO
  BEGIN
    CurrentWord := AppendVocab(EI.PreFilterStack[I],'PREFILTER','.');
    LookupIndex := DI.Dictionary.IndexOf(CurrentWord);
    CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
    DoInner(Owner,EI,DI,CI,CWI);
  END;
  CWI := NIL;
END;

// Strips out one-line comments
PROCEDURE DoStripCmtPreFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  I, X : integer;
BEGIN
  FOR I := 0 TO EI.Input.Count-1 DO
  BEGIN
     X := Pos('\ ', EI.Input[I]);
    IF (X <> 0) THEN
    BEGIN
      EI.Input[I] := ParseStrNG(EI.Input[I],'\',0);
    END;
  END;
END;

// Applies each filter rule that is on the filter stack in sequence
PROCEDURE DoPostFilters(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  I, LookupIndex : integer;
  CurrentFilter : STRING;
BEGIN
  FOR I := 0 TO EI.PostFilterStack.Count-1 DO
  BEGIN
    CurrentFilter := AppendVocab(EI.PostFilterStack[I],'POSTFILTER','.');
    LookupIndex := DI.Dictionary.IndexOf(CurrentFilter);
    CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
    DoInner(Owner,EI,DI,CI,CWI);
  END;
END;

// Executes first word stuffed into the CommandField . Right now this is
// a helper word for implementing data structure vectoring (as opposed to
// vectored execution).
PROCEDURE DoExecute1(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
LABEL
  Exit1;
VAR
  CommandStr, CurrentWord : STRING;
  I, LookupIndex : integer;
BEGIN
  CommandStr := EI.GetField('CommandField');
  FOR I := 0 TO EI.VocabStack.Count-1 DO
  BEGIN
  //  CurrentWord := ParseStrWSD(CommandStr,0);
    CurrentWord := AppendVocab(ParseStrWSD(CommandStr,0),EI.VocabStack[I],'.');
    LookupIndex := DI.Dictionary.IndexOf(CurrentWord);
    // If found, execute and exit. Otherwise search through the next vocabulary.
      IF (LookupIndex <> -1) THEN
      BEGIN
        CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
        DoInner(Owner,EI,DI,CI,CWI);
        GOTO Exit1;
      END;
  END;
  Exit1:
END;

// "Filter" that just pushes data onto the stack.No validation
// or transformation is done.
PROCEDURE DoNothingFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  Push(EI.DataStack,EI.GetField('DeNamespacedWord'));
END;

// Filter that transforms a number to hexadecimal
PROCEDURE DoHexFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  Test, Hex1 : integer;
  Str1 : STRING;
BEGIN
  TRY
    Str1 := EI.GetField('DeNamespacedWord');
    IF Pos('$',Str1) <> 0 THEN
    BEGIN
      Test := StrToInt(Str1);
      Str1 := '$' + IntToHex(Test,2);
    END;
    EI.DataStack.Insert(0,Str1);
  EXCEPT
    ShowMessage('?');
    EI.DataStack.Clear;
  END;
END;

// Filter rule for a "standard" Forth. Any data that is not a numeric
// integer is rejected and empties the stack.
PROCEDURE DoIntegerFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  Test : integer;
  Str1 : STRING;
BEGIN
  TRY
    Str1 := EI.GetField('DeNamespacedWord');
    Test := StrToInt(Str1);
    EI.DataStack.Insert(0,Str1);
  EXCEPT
    ShowMessage('?');
    EI.DataStack.Clear;
  END;
END;

// Filter rule for floating-point arithmetic. Any data that is not a numeric
// value is rejected and empties the stack.
PROCEDURE DoFloatFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  Test : extended;
  Str1 : STRING;
BEGIN
  TRY
    Str1 := EI.GetField('DecryptedWord');
    Test := StrToFloat(Str1);
    EI.DataStack.Insert(0,Str1);
  EXCEPT
    ShowMessage('?');
    EI.DataStack.Clear;
  END;
END;

// Takes the name of a data structure word and makes it the default
// global data structure. The default is the standard data stack; but
// in this case it could be a queue, a binary tree, a hash table, etc.
PROCEDURE DoToDS(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  DSName : STRING;
BEGIN
  DSName := ParseStrWSD(EI.Input.Text,EI.GetOPtr+1);
  EI.SetField('CurrentGlobalDS',DSName);
  EI.SetOPtr(EI.GetOPtr+1);
END;

// Exits a loop and moves the return stack up one 'level'.
PROCEDURE DoExit(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN

END;

// Execute dictionary entry. Takes address off the stack.
PROCEDURE DoExecute(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  LookupIndex : integer;
  PP1 : PrimProc;
BEGIN
  LookupIndex := StrToIntDef(Pop(EI.DataStack),0);
  CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
  PP1 := CWI.CodeField;
  PP1(Owner,EI,DI,CI,CWI);
END;

// Ticks address of word following it onto the stack.
PROCEDURE DoTick(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  WordName : STRING;
  LookupIndex : integer;
BEGIN
  WordName := ParseStrWSD(EI.Input.Text,EI.GetOPtr+1);
  LookupIndex := DI.SearchAvailVocabs(EI.VocabStack,WordName);
  EI.Push(EI.DataStack,IntToStr(LookupIndex));
  EI.SetOPtr(EI.GetOPtr+1);
END;

end.
