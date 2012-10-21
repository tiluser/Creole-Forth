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

// Application-specific routines
unit crsysutils;

{$MODE Delphi}

interface
USES
  FileCtrl, SysUtils, Dialogs, Math;

  FUNCTION StrToBool(S : STRING) : Boolean;
  FUNCTION BoolToStr(BoolVal : Boolean) : STRING;
  FUNCTION IsValidInt(S : STRING) : Boolean;
  FUNCTION EncryptMsg (Msg1, CurrVocab : STRING; EncryptNo1, EncryptNo2 : integer) : STRING;
  FUNCTION DecryptMsg (Msg1, CurrVocab : STRING; DecryptNo1, DecryptNo2 : integer) : STRING;
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

FUNCTION EncryptMsg (Msg1, CurrVocab : STRING; EncryptNo1, EncryptNo2 : integer) : STRING;
VAR
  ResultStr: STRING;
  Temp : char;
  I, EncryptIndex: integer;

BEGIN
  ResultStr := '';
  Temp := ' ';
  Msg1 := Msg1 + '_' + CurrVocab;
  { Encrypt message routine }
  FOR I := 1 TO length(Msg1) DO
  BEGIN
    Temp := Chr(Ord(Msg1[I]) XOR EncryptNo1 XOR EncryptNo2);
    Temp := Chr(Ord(Temp) + EncryptNo2);
    ResultStr := ResultStr + Temp;
  END;
  Result := ResultStr;
end;

FUNCTION DecryptMsg(Msg1, CurrVocab : STRING; DecryptNo1, DecryptNo2 : integer) : STRING;
VAR
  ResultStr, Vocab : STRING;
  Temp, MarkEqChr : char;
  I, EncryptIndex: integer;
BEGIN
  ResultStr := '';
  Temp := ' ';
  { Decrypt message routine }
  FOR I := 1 TO length(Msg1) DO
  BEGIN
    Temp := Chr(Ord(Msg1[I]) - DecryptNo2);
    Temp := Chr(Ord(Temp) XOR DecryptNo2 XOR DecryptNo1);
    ResultStr := ResultStr + Temp;
  END;
  ResultStr := ParseStrNG(ResultStr,'_',0);
  Vocab     := ParseStrNG(ResultStr,'_',1);
  Result := ResultStr;
END;

FUNCTION Reverse(S :STRING) : STRING;
VAR
  I : integer;
  RS : STRING;
BEGIN
  FOR I := Length(S) DOWNTO 1 DO
  BEGIN
    RS := RS + S[I];
  END;
  Result := RS;
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

end.
