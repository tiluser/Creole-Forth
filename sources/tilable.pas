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


unit TILable;

{$MODE Delphi}

interface
USES
  Dialogs, SysUtils, Math, classes, CrSysUtils;
TYPE
  ControlStructVal = (IFVal,ElseVal,ThenVal,JmpVal,StartDoVal,DoVal,LoopVal,
  BeginVal,UntilVal,LitVal,DotQVal1,DotQVal2,DoesVal,NOPVal,ExitVal,DoCompVal);

  TExtIntfce = CLASS(TObject)
  PRIVATE
    OuterPtr         : integer;   // outer interpreter pointer
    CurrentVocab     : STRING;    // vocabulary where new words are defined
    CurrentWord      : STRING;    // Word outer interpreter pointer is at
    DeNamespacedWord : STRING;    // Current word without attached namespace
    CommandField     : STRING;    // optional command words can pass to one another
    CurrentGlobalDS  : STRING;    // Current global data structure. Default is
                                  // the data stack
  PUBLIC
    LoopNestNum     : integer;
    LoopStart, LoopEnd : ARRAY[0..99] OF integer;
    ReturnStack     : ARRAY[0..99] OF integer;
    RSTop           : integer;   // these point to stack tops
    DataStack       : TStrings;
    ReturnStackIF   : TStrings;
    Input           : TStrings;
    ProcessedInput  : TStrings;  // holds contents of preprocessing filter (if any)
    Output          : TStrings;
    PAD             : TStrings;
    VocabStack      : TStrings;
    PreFilterStack  : TStrings;
    PostFilterStack : TStrings;

    // getter and setter routines
    CONSTRUCTOR Create;
    DESTRUCTOR  Free;

    PROCEDURE Push(TheStrings : TStrings; Str1 : STRING);
    FUNCTION  Pop(TheStrings : TStrings) : STRING;
    PROCEDURE SetOPtr(OPtr : integer);
    FUNCTION  GetOPtr : integer;
    PROCEDURE RSPush(int1 : integer);
    FUNCTION  RSPop : integer;
    PROCEDURE RefreshRSStackImage; // Puts values of integer return stack on
                                   // ReturnStackIF

    PROCEDURE SetField(FieldName, FieldVal : STRING);
    FUNCTION GetField(FieldName : STRING) : STRING;
  END;

  TDictIntfce = CLASS(TObject)
  PRIVATE
    IndexField    : integer;
    NameField     : STRING;
    ParamField    : STRING;
    MetaField     : STRING;
    DataField     : STRING;
    ImmediateFlag : Boolean;
    TypeField     : STRING;  // Indicates type of word; primitive, colon.
    RedefinedField : integer;
    Vocabulary    : STRING;
    HelpField     : STRING;
  PUBLIC
    InnerPtr        : integer;   // inner interpreter pointer
    ParamArray    : ARRAY[0..99] OF integer;
    MetaArray     : ARRAY[0..99] OF extended;
 //   ParamField    : STRING;
    Dictionary      : TStringList;
    CONSTRUCTOR Create;
    DESTRUCTOR  Free;
    // getter and setter routines
    PROCEDURE SetIPtr(IPtr : integer);
    FUNCTION  GetIPtr : integer;
    PROCEDURE SetField(FieldName, FieldVal : STRING);
    FUNCTION  GetField(FieldName : STRING) : STRING;
    // searches through all vocabularies on the vocab stack
    // to find index of a given word.
    FUNCTION SearchAvailVocabs(VS : TStrings; RawWord : STRING) : integer;
  END;

  TCtrlIntfce = CLASS(TObject)
  PUBLIC
    ControlInfo     : TStrings;
  END;

  TCreoleWord = CLASS;
  PrimProc  = PROCEDURE(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
                         CI : TCtrlIntfce; CWI : TCreoleWord);
  TCreoleWord = CLASS(TObject)
  PUBLIC
    NameField     : STRING;
    CodeField     : PrimProc;
    CodeFieldStr  : STRING;
    IndexField    : integer;
    ParamField    : STRING;
    MetaField     : STRING;     // Parameters for parameters - ie branch address
                                // for DoLoop primitive. Don't know if this is
                                // going to go anywhere
    ParamArray    : ARRAY[0..99] OF integer;
    MetaArray     : ARRAY[0..99] OF extended;
    DataField     : STRING;
    TypeField     : STRING;  // Indicates type of word; primitive, colon.
    Vocabulary    : STRING;
    HelpField     : STRING;
    ObjField      : TObject;
    RedefinedField : integer; // If word has been redefined, holds address of
                              // previous word. Useful when using FORGET.                        
    PROCEDURE SetField(FieldName, FieldVal : STRING);
    PROCEDURE SetCodeField(CFVal : PrimProc; CFValStr : STRING);
    FUNCTION  GetField(FieldName: STRING) : STRING;
  END;

VAR
  BranchVals : ARRAY[IfVal..DoCompVal] OF integer;
implementation
USES Coreprims;

CONSTRUCTOR TExtIntfce.Create;
BEGIN
  INHERITED Create;
  DataStack       := TStringList.Create;
  ReturnStackIF   := TStringList.Create;
  Input           := TStringList.Create;
  Output          := TStringList.Create;
  PAD             := TStringList.Create;
  VocabStack      := TStringList.Create;
  PreFilterStack  := TStringList.Create;
  PostFilterStack := TStringList.Create;
  OuterPtr := 0;
  CurrentGlobalDS := 'STACK';
END;

DESTRUCTOR TExtIntfce.Free;
BEGIN
  DataStack.Free;
  ReturnStackIF.Free;
  Input.Free;
  Output.Free;
  PAD.Free;
  VocabStack.Free;
  PreFilterStack.Free;
  PostFilterStack.Free;
  INHERITED Free;
END;

PROCEDURE  TExtIntfce.Push(TheStrings : TStrings; Str1 : STRING);
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

FUNCTION TExtIntfce.Pop(TheStrings : TStrings) : STRING;
BEGIN
  TRY
    Result := TheStrings[0];
    TheStrings.Delete(0);
  EXCEPT
    ShowMessage('Stack Underflow');
    TheStrings.Clear;
  END;
END;

PROCEDURE TExtIntfce.SetOPtr(OPtr : integer);
BEGIN
  OuterPtr := OPtr;
END;

FUNCTION  TExtIntfce.GetOPtr : integer;
BEGIN
  Result := OuterPtr;
END;

PROCEDURE TExtIntfce.SetField(FieldName, FieldVal : STRING);
BEGIN
  IF (FieldName = 'CurrentWord')      THEN CurrentWord     := FieldVal;
  IF (FieldName = 'DeNamespacedWord') THEN DeNamespacedWord := FieldVal;
  IF (FieldName = 'CurrentVocab')     THEN CurrentVocab    := FieldVal;
  IF (FieldName = 'CommandField')     THEN CommandField    := FieldVal;
  IF (FieldName = 'CurrentGlobalDS')  THEN CurrentGlobalDS := FieldVal;
END;

FUNCTION TExtIntfce.GetField(FieldName : STRING) : STRING;
BEGIN
  IF (FieldName = 'CurrentWord')     THEN Result := CurrentWord;
  IF (FieldName = 'DeNamespacedWord')   THEN Result := DeNamespacedWord;
  IF (FieldName = 'CurrentVocab')    THEN Result := CurrentVocab;
  IF (FieldName = 'CommandField')    THEN Result := CommandField;
  IF (FieldName = 'CurrentGlobalDS') THEN Result := CurrentGlobalDS;
END;

PROCEDURE TExtIntfce.RSPush(int1 : integer);
BEGIN
  TRY
    RSTop := RSTop + 1;
    ReturnStack[RSTop] := int1;
  EXCEPT
    ShowMessage('Error : Return Stack push failure');
    RSTop := 0;
  END;
END;

FUNCTION TExtIntfce.RSPop : integer;
BEGIN
  TRY
    Result := ReturnStack[RSTop];
    RSTop  := RSTop - 1;
  EXCEPT
    ShowMessage('Integer Stack Underflow');
    RSTop := 0;
  END;
END;

PROCEDURE TExtIntfce.RefreshRSStackImage;
VAR
  I : integer;
BEGIN
  FOR I := RSTop DOWNTO 1 DO
  BEGIN
    Push(ReturnStackIF,IntToStr(ReturnStack[I]));
  END;
END;

CONSTRUCTOR TDictIntfce.Create;
BEGIN
  INHERITED Create;
  Dictionary := TStringList.Create;
  InnerPtr := 0;
END;

DESTRUCTOR TDictIntfce.Free;
BEGIN
  Dictionary.Free;
  INHERITED Free;
END;

PROCEDURE TDictIntfce.SetIPtr(IPtr : integer);
BEGIN
  InnerPtr := IPtr;
END;

FUNCTION  TDictIntfce.GetIPtr : integer;
BEGIN
  Result := InnerPtr;
END;

// Fields in the dictionary interface are the fields of the
// definition currently being executed.
PROCEDURE TDictIntfce.SetField(FieldName, FieldVal : STRING);
VAR
  I : integer;
BEGIN
  IF FieldName = 'DataField'      THEN DataField      := FieldVal;
  IF FieldName = 'TypeField'      THEN TypeField      := FieldVal;
  IF FieldName = 'Vocabulary'     THEN Vocabulary     := FieldVal;
  IF FieldName = 'RedefinedField' THEN RedefinedField := StrToIntDef(FieldVal,-1);
  IF FieldName = 'HelpField'      THEN HelpField      := FieldVal;
  IF FieldName = 'ParamField' THEN
  BEGIN
    ParamField     := FieldVal;
    FOR I := 0 TO WordCntWSD(ParamField)-1 DO
    BEGIN
      ParamArray[I] := StrToInt(ParseStrWSD(ParamField,I));
    END;
  END;
  IF FieldName = 'MetaField' THEN
  BEGIN
    MetaField     := FieldVal;
    FOR I := 0 TO WordCntWSD(MetaField)-1 DO
    BEGIN
      MetaArray[I] := StrToInt(ParseStrWSD(MetaField,I));
    END;
  END;
END;

FUNCTION TDictIntfce.GetField(FieldName : STRING) : STRING;
VAR
  I : integer;
BEGIN
  IF FieldName = 'NameField'      THEN Result := NameField;
  IF FieldName = 'IndexField'     THEN Result := IntToStr(IndexField);
  IF FieldName = 'ParamField'     THEN Result := ParamField;
  IF FieldName = 'MetaField'     THEN Result :=  MetaField;
  IF FieldName = 'DataField'      THEN Result := DataField;
  IF FieldName = 'TypeField'      THEN Result := TypeField;
  IF FieldName = 'Vocabulary'     THEN Result := Vocabulary;
  IF FieldName = 'RedefinedField' THEN Result := IntToStr(RedefinedField);
  IF FieldName = 'HelpField'      THEN Result := HelpField;
END;

FUNCTION TDictIntfce.SearchAvailVocabs(VS : TStrings; RawWord : STRING) : integer;
LABEL
  Exit1;
VAR
  I, LookupIndex : integer;
  SeeWord, TestWord : STRING;
BEGIN
  FOR I := 0 TO VS.Count-1 DO
  BEGIN
    SeeWord := AppendVocab(RawWord,VS[I],'.');
    TestWord :=  DecryptMsg(SeeWord,VS[I],5,1);
    LookupIndex := Dictionary.IndexOf(SeeWord);
    IF (LookupIndex <> -1) THEN GOTO Exit1;
  END;
  Exit1:
  Result := LookupIndex;
END;

PROCEDURE TCreoleWord.SetField(FieldName, FieldVal : STRING);
VAR
  I : integer;
BEGIN
  IF FieldName = 'NameField'      THEN NameField      := FieldVal;
  IF FieldName = 'IndexField'     THEN IndexField     := StrToInt(FieldVal);
  IF FieldName = 'ParamField'     THEN
  BEGIN
    ParamField     := FieldVal;
    FOR I := 0 TO WordCntWSD(ParamField)-1 DO
    BEGIN
      ParamArray[I] := StrToInt(ParseStrWSD(ParamField,I));
    END;
  END;
  IF FieldName = 'DataField'      THEN DataField      := FieldVal;
  IF FieldName = 'TypeField'      THEN TypeField      := FieldVal;
  IF FieldName = 'Vocabulary'     THEN Vocabulary     := FieldVal;
  IF FieldName = 'RedefinedField' THEN RedefinedField := StrToIntDef(FieldVal,-1);
  IF FieldName = 'HelpField'      THEN HelpField      := FieldVal;
  IF FieldName = 'MetaField'      THEN MetaField      := FieldVal;
END;

// Sets code field and associates string value with it.
// Example CWI.SetField(DoColon,'DoColon');
PROCEDURE TCreoleWord.SetCodeField(CFVal : PrimProc; CFValStr : STRING);
BEGIN
  CodeField := CFVal;
  CodeFieldStr := CFValStr;
END;

FUNCTION  TCreoleWord.GetField(FieldName : STRING) : STRING;
BEGIN
  IF FieldName = 'NameField'      THEN Result := NameField;
  IF FieldName = 'IndexField'     THEN Result := IntToStr(IndexField);
  IF FieldName = 'CodeField'      THEN Result := CodeFieldStr;
  IF FieldName = 'ParamField'     THEN Result := ParamField;
  IF FieldName = 'DataField'      THEN Result := DataField;
  IF FieldName = 'TypeField'      THEN Result := TypeField;
  IF FieldName = 'Vocabulary'     THEN Result := Vocabulary;
  IF FieldName = 'RedefinedField' THEN Result := IntToStr(RedefinedField);
  IF FieldName = 'HelpField'      THEN Result := HelpField;
END;

end.
