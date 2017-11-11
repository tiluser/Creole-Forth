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

// Application-specific routines
unit crsysutils;

{$MODE Delphi}

interface
USES
  FileCtrl, SysUtils, Dialogs, Math;

  FUNCTION StrToBool(S : STRING) : Boolean;
  FUNCTION BoolToStr(BoolVal : Boolean) : STRING;
  FUNCTION IsValidInt(S : STRING) : Boolean;
  FUNCTION AppendVocab(WordStr, Vocab, Delim : STRING) : STRING;
  FUNCTION RemoveVocab(WordStr, Vocab, Delim : STRING) : STRING;
  FUNCTION Reverse(S :STRING) : STRING;
  FUNCTION RemovePath(S :STRING) : STRING;
  FUNCTION RemoveUnixPath(S :STRING) : STRING;
  FUNCTION ModExt(S, NewExt :STRING) : STRING;

// Parsing routines, whitespace-delimited. Are "greedy" in
// that they take as much whitespace as possible between words
FUNCTION ParseStrWSD(S : STRING; WordNum : WORD) : STRING;
FUNCTION WordCntWSD(S : STRING) : integer;
FUNCTION WordPosWSD(S : STRING; W : STRING) : Word;
// Reverses order of words
FUNCTION WordReverseWSD(S : STRING) : STRING;

// Parsing routines, "non-greedy". For example, in
// [,,,Hello] the fourth word is hello. The routines count
// one delimiter as one word.
FUNCTION ParseStrNG(S, Delim : STRING; WordNum : WORD) : STRING;
FUNCTION WordCntNG(S, Delim : STRING) : word;

// Other routines
PROCEDURE StrLeftTrim(VAR S : STRING; Delimiter : STRING);
PROCEDURE StrRightTrim(VAR S : STRING; Delimiter : STRING);
FUNCTION  WordCount(S : STRING; Delimiter : STRING) : Integer;
FUNCTION ParseString( S : STRING; WordNum : Word; Delimiter : STRING)
: STRING;
FUNCTION InsertAt(S : STRING; W : STRING; Where : word; Delimiter : STRING) : STRING;
FUNCTION WordPos(S : STRING; W : STRING; Delimiter : STRING) : Word;
// swap one extension for another
FUNCTION WordCountNoTrim(S : STRING; Delimiter : STRING) : Integer;
FUNCTION ParseStringNoTrim(S : STRING; WordNum : Word; Delimiter : STRING)
: STRING;
// recursively creates directories
FUNCTION MkDirs(S : STRING) : boolean;
FUNCTION PadWith(PadChar : char; PadTo : word; S : STRING) : STRING;
FUNCTION RPadWith(PadChar : char; PadTo : word; S : STRING) : STRING;
PROCEDURE StrPadTo(VAR Replacement: STRING; Len : word);
FUNCTION IsNum(S : STRING) : Boolean;
// Increments string by one
FUNCTION StrInc(S : STRING) : STRING;
FUNCTION StrDec(S : STRING) : STRING;

CONST
  TAB = ^I;
  CR  = ^M;
  LF  = ^J;
implementation

// String parsing for whitespace-delimited strings
FUNCTION ParseStrWSD(S : STRING; WordNum : WORD) : STRING;
VAR
  WC, RightValue, TempWrd : word;
  WSPos : ARRAY[0..2] OF integer; // holds nearest locations of white space types
  RemainStr, CurrWordStr : STRING;
BEGIN
  WordNum := WordNum + 1;         // starts things off at zero
  WC := 0;
  RemainStr  := S + ' ' + CR + TAB;
  WHILE (WC < WordNum) DO
  BEGIN
    RemainStr := TrimLeft(RemainStr);
    WSPos[0]  := Pos(' ',RemainStr);
    WSPos[1]  := Pos(CR,RemainStr);
    WSPos[2]  := Pos(TAB,RemainStr);
    RightValue := MinIntValue(WSPos);
    CurrWordStr := RemainStr;
    Delete(RemainStr,1,RightValue);
    Delete(CurrWordStr,RightValue+1,Length(CurrWordStr)-RightValue);
    WC := WC + 1;
  END;
  Result := Trim(CurrWordStr);
END;

// Word counting for whitespace-delimited strings
FUNCTION WordCntWSD(S : STRING) : integer;
VAR
  WC, I, RightValue, TempWrd : word;
  WSPos : ARRAY[0..2] OF integer; // holds nearest locations of white space types
  RemainStr, CurrWordStr : STRING;
BEGIN
  RemainStr  := S + ' ' + CR + TAB;
  WC := 0;
  WHILE (Trim(RemainStr) <> '') DO
  BEGIN
    RemainStr := TrimLeft(RemainStr);
    WSPos[0]  := Pos(' ',RemainStr);
    WSPos[1]  := Pos(CR,RemainStr);
    WSPos[2]  := Pos(TAB,RemainStr);
    RightValue := MinIntValue(WSPos);
    CurrWordStr := RemainStr;
    Delete(RemainStr,1,RightValue);
    Delete(CurrWordStr,RightValue+1,Length(CurrWordStr)-RightValue);
    WC := WC + 1;
  END;
  Result := WC;
END;

// returns the number of a given word in a whitespace-delimited string
FUNCTION WordPosWSD(S : STRING; W : STRING) : Word;
VAR
  Where : Word;
BEGIN
  Where := Pos(W,S);
  IF (Where = 0) THEN Result := 0
  ELSE
  BEGIN
    Delete(S,Where-1,Length(S));
    Result := WordCntWSD(S);
  END;
END;

FUNCTION WordReverseWSD(S : STRING) : STRING;
VAR
  HowMany, I : integer;
  TempStr : STRING;
BEGIN
  HowMany := WordCntWSD(S);
  FOR I := HowMany-1 DOWNTO 0 DO
  BEGIN
    TempStr := TempStr + ' ' + ParseStrWSD(S,I);
  END;
  Result := TempStr;
END;

// "Non-greedy" string parsing for delimited strings
// Intended for comma-delimited, pipe-delimited, etc. strings
// Blank line, no delimiters returns nothing
// 1 or more chars, no delimiters, returns that string
// Note : should be used one line at a time.
FUNCTION ParseStrNG(S, Delim : STRING; WordNum : WORD) : STRING;
VAR
  WC, RightValue, TempWrd : word;
  RemainStr, CurrWordStr : STRING;
BEGIN
  WordNum := WordNum + 1;             //  function uses zero-based indexing
  IF Trim(S) = '' THEN Result := ''   // empty string or whitespace returns                                      // nothing
  ELSE
  BEGIN
    RemainStr := S + Delim;
    WC := 0;
    WHILE (WC < WordNum) DO
    BEGIN
      RightValue := Pos(Delim, RemainStr);
      CurrWordStr := RemainStr;
      Delete(RemainStr,1,RightValue);
      Delete(CurrWordStr,RightValue,Length(CurrWordStr)-RightValue+1);
      WC := WC + 1;
    END;
    Result := Trim(CurrWordStr);
  END;  
END;

FUNCTION WordCntNG(S, Delim : STRING) : word;
VAR
  WC, RightValue, TempWrd : word;
  RemainStr, CurrWordStr : STRING;
BEGIN
  IF Trim(S) = '' THEN Result := 0    // empty string or whitespace returns 0                                     // nothing
  ELSE
  BEGIN
    RemainStr := S + Delim;
    WC := 0;
    WHILE (Trim(RemainStr) <> '') DO
    BEGIN
      RightValue := Pos(Delim, RemainStr);
      CurrWordStr := RemainStr;
      Delete(RemainStr,1,RightValue);
      Delete(CurrWordStr,RightValue,Length(CurrWordStr)-RightValue+1);
      WC := WC + 1;
    END;
    Result := WC;
  END;
END;

FUNCTION StrToBool(S : STRING) : Boolean;
BEGIN
  IF UpperCase(S) = 'TRUE'  THEN Result := True;
  IF UpperCase(S) = 'FALSE' THEN Result := False;
END;
FUNCTION BoolToStr(BoolVal : Boolean) : STRING;
BEGIN
  IF BoolVal = True  THEN Result := 'TRUE';
  IF BoolVal = False THEN Result := 'FALSE';
END;

FUNCTION IsValidInt(S : STRING) : Boolean;
VAR
  X : integer;
BEGIN
  X := StrToIntDef(S,-1);
  IF (S <> '-1') AND (X = -1) THEN Result := False
  ELSE Result := True;
END;

FUNCTION AppendVocab(WordStr, Vocab, Delim : STRING) : STRING;
BEGIN
  Result := WordStr + Delim + Vocab;
END;

FUNCTION RemoveVocab(WordStr, Vocab, Delim : STRING) : STRING;
BEGIN
  Result := ParseStrNG(WordStr,Delim,0);
END;

FUNCTION Reverse(S :STRING) : STRING;
VAR
  I : integer;
  RevStr : STRING;
BEGIN
  I := Length(S);
  WHILE I >= 1 DO
  BEGIN
    RevStr := RevStr + S[I];
    I := I - 1;
  END;
  Result := RevStr;
END;

FUNCTION RemovePath(S :STRING) : STRING;
BEGIN
  S := Reverse(S);
  S := ParseStrNG(S,'\',0);
  Result := Reverse(S);
END;

FUNCTION RemoveUnixPath(S :STRING) : STRING;
BEGIN
  S := Reverse(S);
  S := ParseStrNG(S,'/',0);
  Result := Reverse(S);
END;

FUNCTION ModExt(S, NewExt :STRING) : STRING;
VAR
  WhereDot : integer;
BEGIN
  S := Reverse(S);
  WhereDot := Pos('.',S);
  S := Copy(S,WhereDot+1,Length(S)-WhereDot+1);
  S := ParseStrNG(S,'.',1);
  Result := Reverse(S) + '.' + NewExt;
END;

// trims leading delimiters off a string
PROCEDURE StrLeftTrim(VAR S : STRING; Delimiter : STRING);
VAR
  TempStr, X : STRING;

BEGIN
   WHILE (Copy(S,1,1) = Delimiter) AND (Length(S) > 1) DO
   BEGIN
      TempStr := S;
      IF TempStr[1] = Delimiter THEN
      BEGIN
        Delete(TempStr,1,1);
        S := TempStr;
      END;
   END;
END;

// trims trailing delimiters off a string
PROCEDURE StrRightTrim(VAR S : STRING; Delimiter : STRING);

VAR
  StrLength : Word;
  TempStr : STRING;
BEGIN
   StrLength := Length(S);
   WHILE (Copy(S,StrLength,1) = Delimiter) AND (StrLength > 1) DO
   BEGIN
      TempStr := S;
      IF TempStr[StrLength] = Delimiter THEN  Delete(TempStr,Length(TempStr),1);
      StrLength := Length(TempStr);
      S := TempStr;
   END;
END;

// returns the number of words in a delimited string
FUNCTION WordCount(S : STRING; Delimiter : STRING) : Integer;
CONST
    TAB = ^I;
VAR
   WC, StrPtr, CRPtr, TabPtr : Word;

BEGIN
   WC := 0;
   StrLeftTrim(S,Delimiter);
   StrRightTrim(S,Delimiter);
   { do this just to be sure there is a
     rightmost delimiter - otherwise word
     count could be off by one }
   IF S <> '' THEN S := S + Delimiter;
   REPEAT
      {

      }
      StrPtr := Pos(Delimiter, S);
      CRPtr  := Pos(Chr(13),S);
      TabPtr := Pos(TAB,S);
      IF (CRPtr <> 0) AND (CRPtr < StrPtr) THEN StrPtr := CRPtr;
      IF StrPtr <> 0 THEN
      BEGIN
         WC := WC + 1;
         Delete(S,1,StrPtr);
         StrLeftTrim(S,Delimiter);
      END
   UNTIL StrPtr = 0;
   WordCount := WC ;
END;

// gets the nth word in a delimited string. Carriage returns always demarcate
// another word.
FUNCTION ParseString( S : STRING; WordNum : Word; Delimiter : STRING)
: STRING;

VAR
   RDelimPos, CRPtr, Count, I , StringPtr : Word;

BEGIN
   {Handle the special cases first }
   IF (WordNum = 0) THEN Result := '';
   IF (WordNum > WordCount(S, Delimiter)) THEN Result := '';

 { Get rid of leading blanks if any }
   StrRightTrim(S,Delimiter);
   StrLeftTrim(S,Delimiter);
   StrLeftTrim(S,Chr(13));
    { Get rid of trailing blanks if any }
   StrRightTrim(S,Chr(13));

   IF (WordNum > 1) THEN
   BEGIN
     Count := 1;
     REPEAT
       BEGIN
         { Find first blank before next word }
         RDelimPos := Pos(Delimiter, S);
         CRPtr := Pos(Delimiter, Chr(13));
         IF (CRPtr <> 0) AND (CRPtr < RDelimPos) THEN RDelimPos := CRPtr;
         Delete(S,1, RDelimPos - 1);
         StrLeftTrim(S,Delimiter);
       END;
       Count := Count + 1;
     UNTIL Count = WordNum;
   END;

  { Finally, delete right side }
   StrLeftTrim(S,Delimiter);
   RDelimPos := Pos(Delimiter, S);
   Delete(S,RDelimPos,Length(S));
   ParseString := S;
END;

// returns the number of a given word in a delimited string
FUNCTION WordPos(S : STRING; W : STRING; Delimiter : STRING) : Word;

VAR
  Where : Word;
BEGIN
  Where := Pos(W,S);
  IF (Where = 0) THEN WordPos := 0;
  Delete(S,Where-1,Length(S));
  WordPos := WordCount(S,Delimiter);
END;

// returns the number of words in a delimited string
// does not trim delimiters - extras w/o spaces counted
// as words
FUNCTION WordCountNoTrim(S : STRING; Delimiter : STRING) : Integer;

VAR
   WC, StrPointer : Word;

BEGIN
  WC := 0;
  REPEAT
    StrPointer := Pos(Delimiter, S);
    IF StrPointer <> 0 THEN
    BEGIN
      WC := WC + 1;
      Delete(S,1,StrPointer);
    END;
  UNTIL StrPointer = 0;
  Result := WC + 1;
END;

// gets the nth word in a delimited string - does not trim
// "extra" delimiters.
FUNCTION ParseStringNoTrim(S : STRING; WordNum : Word; Delimiter : STRING)
: STRING;
LABEL
  EndFcn;
VAR
   RDelimPos, Count: Word;
   TempStr : STRING;
BEGIN
   {Handle the special cases first }
   IF (WordNum = 0) THEN
   BEGIN
     Result := '';
     GOTO EndFcn;
   END;
   IF Pos(Delimiter,S) = 0 THEN
   BEGIN
     Result := S;
     GOTO EndFcn;
   END;
   IF (WordNum > WordCountNoTrim(S, Delimiter)) THEN
   BEGIN
     Result := '';
     GOTO EndFcn;
   END;
   Count := 0;
   REPEAT
     RDelimPos := Pos(Delimiter, S);
     TempStr := Copy(S,1,RDelimPos);
     Delete(S,1, RDelimPos);
     Count := Count + 1;
   UNTIL Count = WordNum;
   StrRightTrim(TempStr,Delimiter);
   IF Length(TempStr) > 0 THEN
   BEGIN
     IF TempStr[1] = Delimiter THEN TempStr := ' ';
   END;
   Result := TempStr;
   EndFcn:
END;

// returns string with inserted word at given position in a delimited string
FUNCTION InsertAt(S : STRING; W : STRING; Where : word; Delimiter : STRING) : STRING;
VAR
  TempStr : STRING;
  I : integer;
BEGIN
  TempStr := '';
  FOR I := 1 TO Where-1 DO
  BEGIN
    TempStr := TempStr + ParseString(S,I,Delimiter) + ' ';
  END;
  TempStr := TempStr + W + ' ';
  FOR I := Where TO WordCount(S,Delimiter) DO
  BEGIN
    TempStr := TempStr + ParseString(S,I,Delimiter) + ' ';
  END;
  Result := TempStr;
END;

// iteratively creates subdirectories
FUNCTION MkDirs(S : STRING) : boolean;
VAR
  CatDir : STRING;
  I : integer;
BEGIN
  CatDir := ParseString(S,1,'\');
  FOR I :=  2 TO WordCount(S,'\') DO
  BEGIN
    CatDir := CatDir + '\' + ParseString(S,I,'\');
    IF NOT DirectoryExists(CatDir) THEN MkDirs := CreateDir(CatDir);
  END;
END;

PROCEDURE StrPadTo(VAR Replacement: STRING; Len : word);
VAR
  Blanks : STRING;
  I : word;

BEGIN
  Blanks := '';
  IF Length(Replacement) < Len THEN
  BEGIN
    FOR I := 1 TO Len-Length(Replacement) DO
    BEGIN
      Blanks := Concat(Blanks,' ');
    END;
  END;
  Replacement := Concat(Replacement,Blanks);
END;

FUNCTION PadWith(PadChar : char; PadTo : word; S : STRING) : STRING;
VAR
  NumChars : integer ;
  I : word;
  TempStr : STRING;

BEGIN
  NumChars := PadTo - Length(S);
  TempStr := S;
  IF NumChars > 0 THEN
  BEGIN
    FOR I := 1 TO NumChars DO
    BEGIN
      Insert(PadChar,TempStr,Length(TempStr)+ 1 );
    END;
    PadWith := TempStr;
  END
  ELSE
  BEGIN
    PadWith := S;
  END;
END;

// Right justifies with padding
FUNCTION RPadWith(PadChar : char; PadTo : word; S : STRING) : STRING;
VAR
  NumChars : integer ;
  I : word;
  TempStr : STRING;

BEGIN
  NumChars := PadTo - Length(S);
  TempStr := S;
  IF NumChars > 0 THEN
  BEGIN
    FOR I := 1 TO NumChars DO
    BEGIN
      Insert(PadChar,TempStr,1);
    END;
    RPadWith := TempStr;
  END
  ELSE
  BEGIN
    RPadWith := S;
  END;
END;

FUNCTION StrInc(S : STRING) : STRING;
VAR
  N1 : word;
BEGIN
  N1 := StrToIntDef(S,0);
  N1 := N1 + 1;
  Result := IntToStr(N1);
END;

FUNCTION StrDec(S : STRING) : STRING;
VAR
  N1 : word;
BEGIN
  N1 := StrToIntDef(S,0);
  N1 := N1 - 1;
  Result := IntToStr(N1);
END;

// Returns true for a string numeric
FUNCTION IsNum(S : STRING) : Boolean;
VAR
  Test : integer;
BEGIN
  IF (S = '0')  THEN Result := True;
  IF (S <> '0') THEN
  BEGIN
   Test := StrToIntDef(S,0);
   IF (Test = 0) THEN Result := False
   ELSE Result := True;
  END;
END;

end.
