// Personally developed string-handling routines
unit JMOStr;

interface

USES
  FileCtrl, SysUtils, Dialogs, LMString, Math;

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
FUNCTION ModExt(S, NewExt : STRING) : STRING;
// recursively creates directories
FUNCTION MkDirs(S : STRING) : boolean;
FUNCTION RemovePath(S :STRING) : STRING;
FUNCTION PadWith(PadChar : char; PadTo : word; S : STRING) : STRING;
FUNCTION RPadWith(PadChar : char; PadTo : word; S : STRING) : STRING;
PROCEDURE StrPadTo(VAR Replacement: STRING; Len : word);
FUNCTION IsNum(S : STRING) : Boolean;
// Increments string by one
FUNCTION StrInc(S : STRING) : STRING;
FUNCTION StrDec(S : STRING) : STRING;

implementation

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

// swap one extension for another
FUNCTION ModExt(S, NewExt : STRING) : STRING;
VAR
  DotPos : word;
BEGIN
  DotPos := Pos('.',S);
  IF (DotPos > 0) THEN
  BEGIN
    Delete(S,DotPos,Length(S)- DotPos + 1);
  END;
  ModExt := S + NewExt;
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

FUNCTION RemovePath(S :STRING) : STRING;
BEGIN
  S := StrReverse(S);
  S := ParseString(S,1,'\');
  RemovePath := StrReverse(S);
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
