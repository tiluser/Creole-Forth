(*
Copyright 2003 Joseph M. O'Connor. http://www.creoleforth.org

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

unit coreprims;

//{$MODE Delphi}

interface
USES
  Dialogs, SysUtils, Math, classes, stdctrls, CrSysUtils, TILable;
  // Basic stack operators
  PROCEDURE DoNOP(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoPlus(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoMinus(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoMultiply(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoDivide(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoDivideR(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

  PROCEDURE DoMod(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoNip(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoTuck(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoOver(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoDrop(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoDup(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE Do2Dup(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoSwap(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoRot(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoMinusRot(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoDepth(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoDot(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoSDot(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

  // Tests for equality
  PROCEDURE DoEquals(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoNotEquals(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoLessThan(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoGreaterThan(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

  // Gets time now
  PROCEDURE DoDateTime(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

  PROCEDURE DoHello(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoGoodbye(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoCR(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  // Pops up dialog box with all the words in the dictionary
  PROCEDURE DoVlist(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  // If Help Field of a word is set, pops up a message box with the field contents
  PROCEDURE DoHelp(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

  PROCEDURE Push(TheStrings : TStrings; Str1 : STRING);
  FUNCTION Pop(TheStrings : TStrings) : STRING;

  PROCEDURE DoExamine(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

implementation

PROCEDURE DoNOP(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
   (* *)
END;

PROCEDURE DoPlus(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  First, Second : extended;
  Third,Fourth : integer;
BEGIN
  IF EI.PostFilterStack[0] = 'HEX' THEN
  BEGIN
    Third  := StrToInt(Pop(EI.DataStack));
    Fourth := StrToInt(Pop(EI.DataStack));
    Push(EI.DataStack,'$' + IntToHex(Fourth + Third,2));
  END
  ELSE
  BEGIN
    First  := StrToFloat(Pop(EI.DataStack));
    Second := StrToFloat(Pop(EI.DataStack));
    Push(EI.DataStack,FloatToStr(Second + First));
  END;
END;

PROCEDURE DoMinus(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  First, Second : extended;
BEGIN
  First  := StrToFloat(Pop(EI.DataStack));
  Second := StrToFloat(Pop(EI.DataStack));
  Push(EI.DataStack,FloatToStr(Second - First));
END;

PROCEDURE DoMultiply(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  First, Second : extended;
BEGIN
  First  := StrToFloat(Pop(EI.DataStack));
  Second := StrToFloat(Pop(EI.DataStack));
  Push(EI.DataStack,FloatToStr(Second * First));
END;

// Floored integer division
PROCEDURE DoDivide(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  First, Second : extended;
BEGIN
  First  := StrToFloat(Pop(EI.DataStack));
  Second := StrToFloat(Pop(EI.DataStack));
  Push(EI.DataStack,FloatToStr(Floor(Second / First)));
END;

// Real division
PROCEDURE DoDivideR(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  First, Second : extended;
BEGIN
  First  := StrToFloat(Pop(EI.DataStack));
  Second := StrToFloat(Pop(EI.DataStack));
  Push(EI.DataStack,FloatToStr(Second / First));
END;

// Puts remainder of an integer division on the stack
PROCEDURE DoMod(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  First, Second, Third : integer;
BEGIN
  First := StrToInt(Pop(EI.DataStack));
  Second := StrToInt(Pop(EI.DataStack));
  Third := Second MOD First;
  Push(EI.DataStack,IntToStr(Third));
END;

// Deletes item underneath the top of the stack
PROCEDURE DoNip(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  EI.DataStack.Delete(1);
END;

// Copies top stack item under second item
PROCEDURE DoTuck(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  EI.DataStack.Insert(2,EI.DataStack[0]);
END;

// Copies second item to the top of the stack
PROCEDURE DoOver(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  EI.DataStack.Insert(0,EI.DataStack[1]);
END;

PROCEDURE DoDrop(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  Pop(EI.DataStack);
END;

// Duplicates top item of stack
PROCEDURE DoDup(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  Push(EI.DataStack,EI.DataStack[0]);
END;

// Duplicates top two items of stack
PROCEDURE Do2Dup(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  Push(EI.DataStack,EI.DataStack[1]);
  Push(EI.DataStack,EI.DataStack[1]);
END;

// Swaps top two items of stack
PROCEDURE DoSwap(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  TempStr : STRING;
BEGIN
  TempStr := EI.DataStack[1];
  EI.DataStack[1] := EI.DataStack[0];
  EI.DataStack[0] := TempStr;
END;

// Moves third item in stack to top; pushes top 2 below it
PROCEDURE DoRot(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  TempStr : STRING;
BEGIN
  TempStr := EI.DataStack[2];
  EI.DataStack.Delete(2);
  Push(EI.DataStack,TempStr);
END;

// Moves top item to third, pushes two items above it.
PROCEDURE DoMinusRot(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  TempStr : STRING;
BEGIN
  TempStr := EI.DataStack[2];
  EI.DataStack[2] := EI.DataStack[0];
  EI.DataStack[0] := TempStr;
END;

// Puts depth of data stack on top of itself
PROCEDURE DoDepth(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  EI.Push(EI.DataStack,IntToStr(EI.DataStack.Count));
END;

// Prints number on stack to output
PROCEDURE DoDot(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  EI.Output.Append(Pop(EI.DataStack));
END;

// Nondestructive stack print
PROCEDURE DoSDot(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  TempStr : STRING;
  I : integer;
BEGIN
 // DoDepth(Owner,EI,DI,CI,CWI);
 // ShowMessage(EI.DataStack.Text);
  FOR I := 0 TO EI.DataStack.Count-1 DO EI.Output.Append(' { ' + EI.DataStack[I] + ' } ');
END;


// Equality tests
PROCEDURE DoEquals(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  First, Second : STRING;
BEGIN
  First  := Pop(EI.DataStack);
  Second := Pop(EI.DataStack);
  IF (First = Second) THEN Push(EI.DataStack,'-1') ELSE Push(EI.DataStack,'0');
END;

PROCEDURE DoNotEquals(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  First, Second : STRING;
BEGIN
  First  := Pop(EI.DataStack);
  Second := Pop(EI.DataStack);
  IF (First <> Second) THEN Push(EI.DataStack,'-1') ELSE Push(EI.DataStack,'0');
END;

PROCEDURE DoLessThan(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  First, Second : STRING;
BEGIN
  First  := Pop(EI.DataStack);
  Second := Pop(EI.DataStack);
  IF (First < Second) THEN Push(EI.DataStack,'-1') ELSE Push(EI.DataStack,'0');
END;

PROCEDURE DoGreaterThan(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  First, Second : STRING;
BEGIN
  First  := Pop(EI.DataStack);
  Second := Pop(EI.DataStack);
  IF (First > Second) THEN Push(EI.DataStack,'-1') ELSE Push(EI.DataStack,'0');
END;

PROCEDURE DoDateTime(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  EI.Output.Append(DateTimeToStr(Now));
END;

PROCEDURE DoHello(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  ShowMessage('Hello World');
END;

PROCEDURE DoGoodbye(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  ShowMessage('Goodbye World');
END;

// Carriage return line feed
PROCEDURE DoCR(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  EI.Output.Append(#13#10);
END;


PROCEDURE DoVlist(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  TempStr : STRING;
  I, J, K : integer;
  TheWord : TCreoleWord;
begin
  K := 0;
  FOR I := 0 TO DI.Dictionary.Count-1 DO
  BEGIN
    FOR J := 0 TO EI.VocabStack.Count-1 DO
    BEGIN
      TheWord := Di.Dictionary.Objects[I] AS TCreoleWord;
      IF (TheWord.GetField('Vocabulary') = EI.VocabStack[J]) THEN
      BEGIN
        TempStr := TempStr + ' ' + TheWord.GetField('NameField');
        K := K + 1;
        IF (K MOD 10 = 0) AND (K <> 0) THEN TempStr := TempStr + #13 + #10;
      END;
    END;
  END;
  TempStr := TempStr + #13 + #10 + IntToStr(K) + ' words';
  ShowMessage(TempStr);
END;

PROCEDURE DoHelp(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
LABEL
  Exit1;
VAR
  CWDName, CurrentWord : STRING;
  AWord : TCreoleWord;
  I, LookupIndex : integer;
BEGIN
  CWDName := ParseStrWSD(EI.Input.Text,EI.GetOPtr+1);
  FOR I := 0 TO EI.VocabStack.Count-1 DO
  BEGIN
    CurrentWord := EncryptMsg(CWDName,EI.VocabStack[I],5,1);
    LookupIndex := DI.Dictionary.IndexOf(CurrentWord);
    // If found, execute and exit. Otherwise search through the next vocabulary.
    IF (LookupIndex <> -1) THEN
    BEGIN
      AWord := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
      ShowMessage(AWord.HelpField);
      AWord := NIL;
      GOTO Exit1;
    END;
  END;
  Exit1:
  IF (LookupIndex = -1) THEN ShowMessage('Word not found in dictionary');
  ei.SetOPtr(EI.GetOPtr+1);
END;

PROCEDURE Push(TheStrings : TStrings; Str1 : STRING);
VAR
  Test : extended;
BEGIN
  TRY
    TheStrings.Insert(0,Str1);
  EXCEPT
    ShowMessage('?');
    TheStrings.Clear;
  END;
END;

FUNCTION Pop(TheStrings : TStrings) : STRING;
BEGIN
  TRY
    Result := TheStrings[0];
    TheStrings.Delete(0);
  EXCEPT
    ShowMessage('Stack Underflow');
    TheStrings.Clear;
  END;
END;

// Examines contents of a dictionary entry
PROCEDURE DoExamine(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  LookupIndex : integer;
BEGIN
  LookupIndex := StrToIntDef(Pop(EI.DataStack),0);
  CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
  ShowMessage('NameField is ' + CWI.GetField('NameField') + #13#10 +
  'Parameter Field is ' + CWI.GetField('ParamField') + #13#10 +
  'Data Field is ' + CWI.GetField('DataField') + #13#10 +
  'Redefined Field is ' + CWI.GetField('RedefinedField'));
END;

end.
