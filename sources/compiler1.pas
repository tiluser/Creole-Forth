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
unit Compiler1;

{$MODE Delphi}

interface
USES
  Dialogs, SysUtils, Math, classes, stdctrls, TILable, Coreprims,
  Interpreter, CrSysUtils;

  // 1. Colon definition and dictionary manipulation.
  PROCEDURE DoCreate(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoHere(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoComma(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoFetch(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoStore(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE CompileDoes(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoDoes(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);  
  PROCEDURE CompileColon(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoSemi(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoImmediate(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE CompileVocab(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoVocab(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoContextToCurrent(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoForget(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

  // Branching primitives. A separate primitive is needed to handle compile-time
  // and run-time code. The compile-time primitives must be marked as IMMEDIATE
  // (currently this means putting them in the immediate vocabulary).
  // This allows them to execute during compilation, compiling in the run-time
  // code with the associated jump address information.

  //2. DO-LOOP primitives.
  PROCEDURE CompileDo(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE CompileLoop(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE StartDoDo(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoDo(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoLoop(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

  // 3. IF-THEN-ELSE primitives
  PROCEDURE CompileIf(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE CompileElse(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE CompileThen(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoJump(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoZeroBranch(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoElse(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoThen(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

  // 4. BEGIN-UNTIL primitives
  PROCEDURE CompileBegin(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE CompileUntil(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
  PROCEDURE DoUntil(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

  // 5. Literal compilation
PROCEDURE CompileLit(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
PROCEDURE DoLit(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);

 // 6. String handling inside a colon definition.
PROCEDURE CompileDotQuote(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
PROCEDURE DoDotQuote(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);

  // 6. Hash tables
PROCEDURE CompileHash(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
PROCEDURE DoHash(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
// Hash filter directs data towards a defined hash table.
PROCEDURE DoHashFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);

// 7. Decompiler
PROCEDURE DoSee(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);

// 8. Compiler extennsion
PROCEDURE CompilePostpone(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
PROCEDURE DoPostpone(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);


implementation

// Creates a new entry into the dictionary
PROCEDURE DoCreate(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  AWord : TCreoleWord;
  EncryptedName : STRING;
BEGIN
  AWord := TCreoleWord.Create;
  AWord.SetField('NameField',ParseStrWSD(EI.Input.Text,EI.GetOPtr+1));
  AWord.CodeField := DoHere;
  AWord.IndexField := DI.Dictionary.Count;
  AWord.SetField('Vocabulary',EI.GetField('ContextVocab'));
  CWI := AWord;
  EncryptedName := EncryptMsg(AWord.GetField('NameField'),EI.GetField('ContextVocab'),5,1);
  DI.Dictionary.AddObject(EncryptedName,CWI);
  EI.SetOPtr(EI.GetOPtr + 1);
 // EI.Push(EI.DataStack,EI.GetField('IndexField'));
END;

// Puts address of top (or bottom if you like) of dictionary
PROCEDURE DoHere(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  Push(EI.DataStack,IntToStr(DI.Dictionary.Count-1));
END;

// Takes a value off the stack and compiles it into the data field of the
// definition at the top of the dictionary.
PROCEDURE DoComma(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  LookupIndex : integer;
  TempStr : STRING;
BEGIN
  LookupIndex := DI.Dictionary.Count-1;
  CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
  TempStr := CWI.GetField('DataField') + ' ' + EI.Pop(EI.DataStack);
  CWI.SetField('DataField',TempStr);
END;

// Fetches value at address on the stack.
PROCEDURE DoFetch(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  LookupIndex : integer;
  TempStr : STRING;
BEGIN
  TRY
    LookupIndex := StrToIntDef(Pop(EI.DataStack),-1);
    CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
    TempStr := Trim(CWI.GetField('DataField'));
    Push(EI.DataStack,TempStr);
  EXCEPT
    ShowMessage('Error : No word is at that address');
  END;
END;

// Stores value at address on the stack.
PROCEDURE DoStore(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  LookupIndex : integer;
  TempStr : STRING;
BEGIN
  TRY
    LookupIndex := StrToIntDef(Pop(EI.DataStack),-1);
    CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
    TempStr := Pop(EI.DataStack);
    CWI.SetField('DataField',TempStr);
  EXCEPT
    ShowMessage('Error : No word is at that address');
  END;
END;

// Compile-time code for defining words - executed when the defining word
// is created. It copies the runtime code following DOES> into the child
// word's parameter field and then exits.
PROCEDURE CompileDoes(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  X, PF, TempStr : STRING;
  LookupIndex, Where, I : integer;
  DefinedWord : TCreoleWord;
BEGIN
  DefinedWord := TCreoleWord.Create;
  LookupIndex := DI.Dictionary.Count-1;
  DefinedWord := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
  X := CWI.GetField('IndexField');
  PF := DI.GetField('ParamField');
  Where := WordPosWSD(PF,X);
  FOR I := Where+1 TO WordCntWSD(PF)-1 DO
  BEGIN
    TempStr := TempStr + ' ' + ParseStrWSD(PF,I);
  END;
  // This moves the inner pointer past the runtime code so it
  // won't be executed at this pass.
  EI.ReturnStack[EI.RSTop] := DI.InnerPtr + I;
  DefinedWord.SetField('ParamField',TempStr);
  DefinedWord.CodeField := DoDoes;
  DefinedWord.SetField('TypeField','Defined');
  DefinedWord := NIL;
  DefinedWord.Free;
END;

// Runtime code for defining words.
PROCEDURE DoDoes(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  LookupIndex : integer;
  TempStr : STRING;
BEGIN
  Push(EI.DataStack,CWI.GetField('IndexField'));
  DoColon(Owner,EI,DI,CI,CWI);
END;

// Revamped colon compiler.
// Doesn't use a state variable. Instead, at start of definition,
// immediate vocabulary is pushed to the top of the vocabulary stack.
// This vocabulary is searched first and words are executed if they
// are found there. Other vocabularies on the stack are then searched and
// if a word is found there, it is compiled. If not found, an attempt is made
// to compile it as a numeric literal*.
// * This may change according to the rulesets, but haven't implemented
// this feature yet. For example, the standard Forth rule of attempting
// to convert to a numeric literal and placing it on the stack could be
// changed to simply discarding a value that isn't present
// in the dictionary.

PROCEDURE CompileColon(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
LABEL
  Exit1;
VAR
  AWord, BWord : TCreoleWord;
  I, LookupIndex, OldIndex : integer;
  CurrentWord, TempStr, WordStr : STRING;
  PP : PrimProc;
BEGIN
  AWord := TCreoleWord.Create;
  BWord := TCreoleWord.Create;
  AWord.SetField('NameField',ParseStrWSD(EI.Input.Text,EI.GetOPtr+1));
  // Find if word has already been defined in current vocabulary. If so,
  // do another encryption so it is not visible anymore
  LookupIndex := DI.Dictionary.IndexOf(EncryptMsg(AWord.NameField,EI.GetField('ContextVocab'),5,1));
  IF LookupIndex <> -1 THEN
  BEGIN
    BWord := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
    OldIndex := LookupIndex;
    DI.Dictionary.Delete(LookupIndex);
    WordStr := EncryptMsg(DI.Dictionary[I],EI.GetField('ContextVocab'),5,2);
    DI.Dictionary.InsertObject(LookupIndex,WordStr,BWord);
    ShowMessage(AWord.NameField + ' will be redefined');
    BWord := NIL;
    BWord.Free;
  END;
  AWord.CodeField := DoColon;
  AWord.IndexField := DI.Dictionary.Count;
  AWord.Vocabulary := EI.GetField('ContextVocab');
  AWord.TypeField := 'Colon';
  AWord.SetField('RedefinedField',IntToStr(OldIndex));
  DI.Dictionary.AddObject(EncryptMsg(AWord.NameField,EI.GetField('ContextVocab'),5,1),AWord);
  EI.VocabStack.Insert(0,'IMMEDIATE');
  EI.SetOPtr(EI.GetOPtr + 2);

  // First search immediate vocabulary. If word is found, then execute it. If
  // not, then search other vocabularies on the vocab stack.
  WHILE (EI.VocabStack[0] = 'IMMEDIATE') DO
  BEGIN
    CurrentWord := ParseStrWSD(EI.Input.Text,EI.GetOPtr);
    LookupIndex := DI.Dictionary.IndexOf(EncryptMsg(CurrentWord,EI.VocabStack[0],5,1));
    IF LookupIndex <> -1 THEN
    BEGIN
      CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
      DoInner(Owner,EI,DI,CI,CWI);
      CWI := NIL;
      GOTO Exit1;
    END;

    FOR I := 1 TO EI.VocabStack.Count-1 DO
    BEGIN
      LookupIndex := DI.Dictionary.IndexOf(EncryptMsg(CurrentWord,EI.VocabStack[I],5,1));
      IF LookupIndex <> -1 THEN
      BEGIN
        EI.PAD.Append(IntToStr(LookupIndex));
        GOTO Exit1;
      END;
    END;
    Exit1 :
    // If not in dictionary and is an integer literal, compile it as a literal
    IF (LookupIndex = -1) AND IsValidInt(CurrentWord) THEN CompileLit(Owner,EI,DI,CI,CWI);
    EI.SetOptr(EI.GetOPtr+1);
  END;

  // Final step of compilation
  TempStr := '';
  FOR I := 0 TO EI.PAD.Count-1 DO
  BEGIN
    TempStr := TempStr + Trim(EI.PAD[I]) + ' ';
  END;

  AWord.SetField('ParamField',TempStr);
  DI.Dictionary.Objects[DI.Dictionary.Count-1] := AWord;
  EI.PAD.Clear;
  CWI := NIL; 
END;

// Immediate word that halts compiling process by popping 'IMMEDIATE' off the
// top of the vocabulary stack.
PROCEDURE DoSemi(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  Pop(EI.VocabStack);
  EI.SetOptr(EI.GetOPtr-1);
END;

// Flags word just defined as immediate. With the current colon compiler, it
// places an immediate word in the IMMEDIATE vocabulary.
PROCEDURE DoImmediate(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  AWord : TCreoleWord;
BEGIN
  AWord := TCreoleWord.Create;
  AWord := DI.Dictionary.Objects[DI.Dictionary.Count-1] AS TCreoleWord;
  DI.Dictionary.Delete(DI.Dictionary.Count-1);
  AWord.SetField('Vocabulary','IMMEDIATE');
  DI.Dictionary.AddObject(EncryptMsg(AWord.NameField,'IMMEDIATE',5,1),AWord);
  AWord := NIL;
  AWord.Free;
END;

// Sets context vocabulary to current one (At top of vocabulary stack).
// Context vocabulary sets up where new words are defined, as opposed
// to the search order.
PROCEDURE DoContextToCurrent(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  EI.SetField(EI.GetField('ContextVocab'),EI.VocabStack[0]);
END;

// Compiles a new vocabulary into the dictionary
PROCEDURE CompileVocab(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  AWord : TCreoleWord;
BEGIN
  AWord := TCreoleWord.Create;
  AWord.SetField('NameField',ParseStrWSD(EI.Input.Text,EI.GetOPtr+1));
  AWord.CodeField := DoVocab;
  AWord.Vocabulary := EI.GetField('ContextVocab');
  AWord.IndexField := DI.Dictionary.Count;
  DI.Dictionary.AddObject(EncryptMsg(AWord.NameField,EI.GetField('ContextVocab'),5,1),AWord);
  EI.SetOptr(EI.GetOPtr+1);
END;

// Sets current vocabulary by placing it at the top of the vocabulary stack
PROCEDURE DoVocab(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  VocabName : STRING;
BEGIN
  VocabName := CWI.GetField('NameField');
  IF VocabName = 'ONLY' THEN EI.VocabStack.Clear;
  EI.VocabStack.Insert(0,VocabName);
END;

// Creates a TStringList to be used as a hash table
PROCEDURE CompileHash(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  Hash1 : TStringList;
  AWord : TCreoleWord;
BEGIN
  Hash1 := TStringList.Create;
  AWord := TCreoleWord.Create;
  AWord.SetField('NameField',ParseStrWSD(EI.Input.Text,EI.GetOPtr+1));
  AWord.CodeField := DoHash;
  AWord.ObjField := Hash1;
  AWord.Vocabulary := EI.GetField('ContextVocab');
  AWord.IndexField := DI.Dictionary.Count;
  AWord.SetField('TypeField','Hash');
  DI.Dictionary.AddObject(EncryptMsg(AWord.NameField,EI.GetField('ContextVocab'),5,1),AWord);
  EI.SetOptr(EI.GetOPtr+1);
END;

PROCEDURE DoHash(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  I, ParamIndex : integer;
  Hash1 : TStringList;
  Command, CommandField, ParamValue : STRING;
BEGIN
  Hash1 := TStringList(CWI.ObjField);
  CommandField := EI.GetField('CommandField');
  ShowMessage('DoHash executed. CommandField is : ' + CommandField);

  // If commands have been stuffed into command field execute them. Otherwise
  // look for them in the input stream.
  IF (CommandField <> '') THEN
  BEGIN
    Command := ParseStrWSD(EI.GetField('CommandField'),1);
    ParamValue := ParseStrWSD(EI.GetField('CommandField'),2);
  END
  ELSE
  BEGIN
    Command := ParseStrWSD(EI.Input.Text,EI.GetOPtr+1);
  END;

  IF (Command = 'ADD') THEN
  BEGIN
    IF (ParamValue='') THEN ParamValue := ParseStrWSD(EI.Input.Text,EI.GetOPtr+2);
    Hash1.Append(ParamValue);
        IF (CommandField = '') THEN EI.SetOptr(EI.GetOPtr+2);
  END;
  IF (Command = 'VALUEOF') THEN
  BEGIN
    IF (ParamValue='') THEN ParamValue := ParseStrWSD(EI.Input.Text,EI.GetOPtr+2);
    ParamIndex := Hash1.IndexOfName(ParamValue);
    IF ParamIndex <> -1 THEN ShowMessage(Hash1.Values[ParamValue]);
    IF (CommandField = '') THEN EI.SetOptr(EI.GetOPtr+2);
  END;

  IF (Command = 'SHOW') THEN
  BEGIN
    ShowMessage(Hash1.Text);
  (*
    FOR I := 0 TO Hash1.Count-1 DO
    BEGIN
      EI.DataStack.Insert(0,Hash1[I]);
    END;
    *)
        IF (CommandField = '') THEN EI.SetOptr(EI.GetOPtr+1);
  END;
END;

PROCEDURE DoHashFilter(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  TheData, CurrentGlobalDS, CommandStr : STRING;
BEGIN
  TheData := EI.GetField('CurrentWord');
  CurrentGlobalDS := EI.GetField('CurrentGlobalDS');
  CommandStr := CurrentGlobalDS + ' ADD ' + TheData;
  EI.SetField('CommandField',CommandStr);
  DoExecute1(Owner,EI,DI,CI,CWI);
  EI.SetField('CommandField','');
END;

// Can only forget a word if it's in the context vocabulary or is immediate
PROCEDURE DoForget(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  ForgetWord, FormerWord : TCreoleWord;
  ForgetIndex, FormerIndex, I, DictSize : integer;
  RawWord, SeeWord, WordStr : STRING;
BEGIN
  EI.Push(EI.VocabStack,'IMMEDIATE'); // Do so immediate vocab is searched.
  RawWord := ParseStrWSD(EI.Input.Text,EI.GetOPtr+1);
  ForgetIndex := DI.SearchAvailVocabs(EI.VocabStack, RawWord);
  ForgetWord := DI.Dictionary.Objects[ForgetIndex] AS TCreoleWord;
  DictSize := DI.Dictionary.Count;
  FormerIndex := StrToIntDef(ForgetWord.GetField('RedefinedField'),-1);

  IF ForgetIndex <> -1 THEN
  BEGIN
    WHILE (ForgetIndex <= DI.Dictionary.Count-1) DO
    BEGIN
      ForgetWord := DI.Dictionary.Objects[ForgetIndex] AS TCreoleWord;
      ForgetWord.Free;
      DI.Dictionary.Delete(ForgetIndex);

      // If word was redefined, then go back to former word and encrypt so
      // it is visible in the dictionary again. If a word was redefined, the
      // new word with the same name would have the index of the old stored
      // in its RedefinedField and the field would have as a value one of the
      // dictionary indexes. If the RedefinedField is outside these bounds, it
      // was never set and would fall outside these bounds. 
      IF (FormerIndex > 0) AND (FormerIndex < DictSize) THEN
      BEGIN
        FormerWord := DI.Dictionary.Objects[FormerIndex] AS TCreoleWord;
        WordStr := EncryptMsg(FormerWord.GetField('NameField'),EI.GetField('ContextVocab'),5,1);
        DI.Dictionary.Strings[FormerIndex] := WordStr;
      END;
    END;
  END
  ELSE
  BEGIN
    ShowMessage('?');
    EI.DataStack.Clear;
  END;
  EI.Pop(EI.VocabStack);  // Pop off IMMEDIATE vocabulary.
  EI.SetOPtr(EI.GetOPtr+1);
END;

// Compiles a do marker into the new definition; pushes location onto stack
PROCEDURE CompileDo(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  DoPos : word;
BEGIN
  DoPos := EI.PAD.Count+1;
  EI.PAD.Append(IntToStr(BranchVals[StartDoVal]));
  EI.PAD.Append(IntToStr(BranchVals[DoVal]));
  Push(EI.DataStack,IntToStr(DoPos));
END;

// Compiles Loop marker and pops do location off the stack
PROCEDURE CompileLoop(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  LoopPastPos : integer;
BEGIN
  LoopPastPos := EI.PAD.Count + 4;
  EI.PAD.Append(IntToStr(BranchVals[LoopVal]));
  EI.PAD.Append(Pop(EI.DataStack));
  EI.PAD.Append(IntToStr(LoopPastPos));
END;

// Run-time code for DO and LOOP
// Each time DoDo is encountered it increments the top of the stack by 1
PROCEDURE StartDoDo(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  X : integer;
BEGIN
  EI.LoopNestNum := EI.LoopNestNum + 1;
  EI.LoopStart[EI.LoopNestNum] := StrToInt(Pop(EI.DataStack));
  EI.LoopEnd[EI.LoopNestNum]   := StrToInt(Pop(EI.DataStack));
END;

PROCEDURE DoDo(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  EI.LoopStart[EI.LoopNestNum] := EI.LoopStart[EI.LoopNestNum] + 1;
END;

// If counter is finished, DoLoop removes loop index and loop finish value
// from the stack. Otherwise it keeps them.
// If counter is finished, DoLoop removes loop index and loop finish value
// from the stack. Otherwise it keeps them.
PROCEDURE DoLoop(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce;
                  CWI : TCreoleWord);
VAR
  TrueOrFalse : integer;
BEGIN
 // TrueOrFalse := NOT (StrToInt(EI.DataStack[0]) XOR StrToInt(EI.DataStack[1]));
   TrueOrFalse := NOT EI.LoopStart[EI.LoopNestNum] XOR EI.LoopEnd[EI.LoopNestNum];
  // -1 indicates finish; 0 inequality and loop term. not reached
  IF TrueOrFalse  <>  -1 THEN TrueOrFalse := 0;
  // Don't even have to do testing. If TrueOrFalse is 0, InnerPtr is set
  // to adjacent value (index or address of QDoDo). If TrueOrFalse is  -1, just
  // subtract -1; ie. add 1 to get the exit index.

  DI.InnerPtr := DI.ParamArray[DI.InnerPtr-TrueOrFalse+1];

  // Clean up data stack arguments
  IF TrueOrFalse = -1 THEN
  BEGIN
    EI.LoopNestNum := EI.LoopNestNum - 1;
  END;
  // Then do equivalent of pushing back on return stack
  EI.ReturnStack[EI.RSTop] := DI.InnerPtr;
END;

// Compiles a 0Branch ID into the new definition
PROCEDURE CompileIf(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  JumpAddrPos : word;
BEGIN
  EI.PAD.Append(IntToStr(BranchVals[IfVal]));
  JumpAddrPos := EI.PAD.Count;
  Push(EI.DataStack,IntToStr(JumpAddrPos));
END;

PROCEDURE CompileElse(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  JumpAddrPos1, JumpAddrPos2, ZeroBrAddr : word;
BEGIN
  // Compile unconditional branch to THEN just before the ELSE
  EI.PAD.Append(IntToStr(BranchVals[JmpVal]));
  JumpAddrPos2 := EI.PAD.Count;
  EI.PAD.Append(IntToStr(BranchVals[ElseVal]));
  ZeroBrAddr := EI.PAD.Count;
  // Pop jump address location 0Branch will use off of data stack
  JumpAddrPos1 := StrToInt(Pop(EI.DataStack));
  EI.PAD.Insert(JumpAddrPos1,IntToStr(ZeroBrAddr));
  // Pushes ElsePos on stack for Compile_Then to grab
  Push(EI.DataStack,IntToStr(JumpAddrPos2));
END;

PROCEDURE CompileThen(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  ThenBranchAddr, ThenPos : word;
BEGIN
  EI.PAD.Append(IntToStr(BranchVals[ThenVal]));
  ThenPos := EI.PAD.Count;
  ThenBranchAddr := StrToInt(Pop(EI.DataStack));
  EI.PAD.Insert(ThenBranchAddr,IntToStr(ThenPos));
END;

// Run-time code for IF-THEN-ELSE
PROCEDURE DoZeroBranch(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  IfResult, JumpAddr : integer;
BEGIN
  EI.RSPop;
  IfResult := StrToInt(EI.Pop(EI.DataStack));
  IF (IfResult = 0) THEN
  BEGIN
    // First find value of jump address.
    JumpAddr := DI.ParamArray[DI.GetIPtr+1];
    DI.SetIPtr(JumpAddr);
  END
  ELSE
  BEGIN
    DI.SetIPtr(DI.GetIPtr+2);
  END;
  EI.RSPush(DI.GetIPtr);
END;

// Unconditional Branching
PROCEDURE DoJump(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  IfResult, JumpAddr : integer;
BEGIN
  EI.RSPop;
  // First find value of jump address.
  JumpAddr := DI.ParamArray[DI.GetIPtr+1];
  DI.SetIPtr(JumpAddr);
  EI.RSPush(DI.GetIPtr);
END;

PROCEDURE DoElse(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  ElseResult, JumpAddr : integer;
BEGIN
  EI.RSPop;
  DI.SetIPtr(DI.GetIPtr+1);
  EI.RSPush(DI.GetIPtr);
END;

// Just a placeholder
PROCEDURE DoThen(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
 // Pop(EI.DataStack);
END;

// Compiles a begin marker into the new definition; pushes location onto stack
PROCEDURE CompileBegin(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  BeginPos : word;
BEGIN
  EI.PAD.Append(IntToStr(BranchVals[BeginVal]));
  BeginPos := EI.PAD.Count-1;
  Push(EI.DataStack,IntToStr(BeginPos));
END;

// Compiles 0Branch into the new definition; pops location of BEGIN off stack
// and appends to definition
PROCEDURE CompileUntil(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
BEGIN
  // BeginVal same as IfVal - they both use 0Branch code
  EI.PAD.Append(IntToStr(BranchVals[UntilVal]));
  EI.PAD.Append(Pop(EI.DataStack));
END;

// Like DoZeroBranch except it pops value off of the data stack
PROCEDURE DoUntil(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  UntilResult, JumpAddr : integer;
BEGIN
  EI.RSPop;
  UntilResult := StrToInt(Pop(EI.DataStack));
  IF (UntilResult = 0) THEN
  BEGIN
    // First find value of jump address.
    JumpAddr := DI.ParamArray[DI.GetIPtr+1];
    DI.SetIPtr(JumpAddr);
  END
  ELSE
    DI.SetIPtr(DI.GetIPtr+2);
  EI.RSPush(DI.GetIPtr);
END;

PROCEDURE CompileLit(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  TheLit : STRING;
BEGIN
  TheLit := ParseStrWSD(EI.Input.Text,EI.GetOPtr);
  EI.PAD.Append(IntToStr(BranchVals[LitVal]));
  EI.PAD.Append(IntToStr(DI.GetIPtr+1));
  DI.MetaArray[DI.GetIPtr+1] := StrToFloat(TheLit);
END;

// Run-time code for pushing a literal on the stack
PROCEDURE DoLit(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  RSVal :  integer;
  Literal : STRING;
  WordAboveIndex, I, X : integer;
  Y : extended;
  WordAbove : TCreoleWord;
BEGIN
  RSVal := EI.RSPop;
  DI.SetIPtr(RSVal);
  X := DI.ParamArray[DI.GetIPtr];
  Y := DI.MetaArray[X];
  Push(EI.DataStack,FloatToStr(Y));
   // Push back IDValue and modified value of IP onto return stack
  EI.RSPush(RSVal+1);
END;

PROCEDURE CompileDotQuote(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  QuotedStuff : STRING;
  LookupIndex, I : integer;
BEGIN
  I := EI.GetOPtr+1;
  REPEAT
    EI.SetOPtr(I);
    QuotedStuff := QuotedStuff + ' ' + ParseStrWSD(EI.Input.Text,EI.GetOPtr);
    I := I + 1;
  UNTIL Pos('"',QuotedStuff) <> 0;
  LookupIndex := DI.Dictionary.Count-1;
  CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
  EI.PAD.Append(IntToStr(BranchVals[DotQVal1]));
  EI.PAD.Append(IntToStr(LookupIndex));
  CWI.SetField('DataField',Copy(QuotedStuff,1,Length(QuotedStuff)-1));
 // ShowMessage(CWI.GetField('DataField'));
END;

PROCEDURE DoDotQuote(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  QuotedStuff : STRING;
  RSVal :  integer;
  Literal : STRING;
  LookupIndex, I, X : integer;
BEGIN
  RSVal := EI.RSPop;
  DI.SetIPtr(RSVal);
  X := DI.ParamArray[DI.GetIPtr];
  Push(EI.DataStack,FloatToStr(X));
   // Push back IDValue and modified value of IP onto return stack
  RSVal := DI.GetIPtr+1;
  EI.RSPush(RSVal);
  LookupIndex := StrToIntDef(Pop(EI.DataStack),-1);
  CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
  QuotedStuff := CWI.GetField('DataField');
  EI.Output.Append(QuotedStuff);
END;

// Poor man's decompiler
PROCEDURE DoSee(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
LABEL
  Exit1;
VAR
  I, X, LookupIndex, WC : integer;
  RawWord, SeeWord, TempStr : STRING;
  LookupWord : TCreoleWord;
BEGIN
  I := 0;
  // Get next word on the input stream.
  RawWord := ParseStrWSD(EI.Input.Text,EI.GetOPtr+1);
  SeeWord := EncryptMsg(RawWord,EI.VocabStack[I],5,1);
  LookupIndex := DI.Dictionary.IndexOf(SeeWord);
  IF (LookupIndex <> -1) THEN
  BEGIN
    CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
  END
  ELSE
  BEGIN
    ShowMessage('Word not found in dictionary');
    GOTO Exit1;
  END;
  IF CWI.GetField('TypeField') = 'Primitive' THEN
    ShowMessage(RawWord + ' is a primitive')
  ELSE
  BEGIN
    WC := WordCntWSD(CWI.GetField('ParamField'));
    WHILE (I < WC) DO
    BEGIN
      X := StrToInt(ParseStrWSD(CWI.GetField('ParamField'),I));
      IF (X = BranchVals[LitVal]) THEN
      BEGIN
        I := I + 1;
        X := StrToInt(ParseStrWSD(CWI.GetField('ParamField'),I));
        TempStr := TempStr + ' ' + IntToStr(X);
      END
      ELSE IF (X = BranchVals[StartDoVal]) THEN
      BEGIN
        I := I + 1;
        TempStr := TempStr + ' ' + 'DO';
      END
      ELSE IF (X = BranchVals[LoopVal]) THEN
      BEGIN
        I := I + 2;
        TempStr := TempStr + ' ' + 'LOOP';
      END
      ELSE IF (X = BranchVals[IfVal]) THEN
      BEGIN
        I := I + 1;
        TempStr := TempStr + ' ' + 'IF';
      END
      ELSE IF (X = BranchVals[ElseVal]) THEN
      BEGIN
        I := I + 1;
        TempStr := TempStr + ' ' + 'ELSE';
      END
      ELSE IF (X = BranchVals[ThenVal]) THEN
      BEGIN
        TempStr := TempStr + ' ' + 'THEN';
      END
      ELSE
      BEGIN
        LookupWord := DI.Dictionary.Objects[X] AS TCreoleWord;
        TempStr := TempStr + ' ' + LookupWord.GetField('NameField');
      END;
      IF (I MOD 10 = 0) THEN TempStr := TempStr + #13#10;
      I := I + 1;
    END;
    ShowMessage(IntToStr(BranchVals[ThenVal]));
    ShowMessage(': ' + RawWord + ' ' + TempStr + ' ;');
  END;
  Exit1:
  EI.SetOptr(EI.GetOPtr+1);
END;

// If word following is immediate, force a compile instead of allowing it to
// execute. If it's not immediate, compilation of word following is deferred
// until the colon definition is executed. This is done by compiling the
// dictionary index of DoPostpone into the definition, then the index of the
// word to be postponed. The colon definition with POSTPONE in it then must
// be flagged as IMMEDIATE.
PROCEDURE CompilePostpone(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  RawWord, SeeWord : STRING;
  LookupIndex, I : integer;
BEGIN
  RawWord := ParseStrWSD(EI.Input.Text,EI.GetOPtr+1);
  LookupIndex := DI.SearchAvailVocabs(EI.VocabStack,RawWord);
  CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
  IF CWI.GetField('Vocabulary') = 'IMMEDIATE' THEN EI.PAD.Append(IntToStr(LookupIndex))
  ELSE
  BEGIN
    EI.PAD.Append(IntToStr(BranchVals[DoCompVal]) + ' ' + IntToStr(LookupIndex));
  END;
  EI.SetOptr(EI.GetOPtr+1);
END;

PROCEDURE DoPostpone(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  RSVal :  integer;
  Literal : STRING;
  WordAboveIndex, I, X : integer;
  WordAbove : TCreoleWord;
BEGIN
  RSVal := EI.RSPop;
  DI.SetIPtr(RSVal);
  X := DI.ParamArray[DI.GetIPtr];
  EI.PAD.Append(IntToStr(X));
   // Push back IDValue and modified value of IP onto return stack
  RSVal := DI.GetIPtr+1;
  EI.RSPush(RSVal);
END;

end.
