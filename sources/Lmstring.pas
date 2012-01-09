(*
Some basic string handling functions written by Frank Engo, author
of How To Program Delphi 3.
*)

unit LMString;

interface

Uses
  SysUtils, Forms, Graphics, ExtCtrls, Dialogs; // WinProcs;

function FormatUpper (Str1: string): string;
//function EncryptMsg (Msg1: string; EncryptNo: integer): string;
//function DecryptMsg (Msg1: string; DecryptNo: integer): string;
function StrExtractCmd (CommandStr: string; optParam:
                             integer): string ;
function StrStripSpaces (Msg1: string): string;
function StrReverse (S: string): string;
function IsCommented (CmdStr: string): boolean;
//function WinDir (optWin: integer ): string;
//function WinVer: string;
//function CheckPentium: integer;
function StrDeleteAll (S: string; DelChar: char): string;
function StrReplaceAll (S: string; OldChar, NewChar: char): string;
function IsNumVal (StrVal: string): boolean;

var
  CountIndex, xPos, yPos: integer;

implementation

function FormatUpper (Str1: string): string;

{ Capitalize first letter of each part of string }

var
  FormattedStr: string;
  LastChar: Char;
  I: integer;

begin

  FormattedStr := '';
  LastChar := ' ';

  for I := 1 to length(Str1) do

      begin

        if (I = 1) or (LastChar = ' ') then
           FormattedStr := FormattedStr + UpperCase(Str1[I])
        else
           FormattedStr := FormattedStr + Str1[I];

        LastChar := Str1[I];

      end;

  FormatUpper := FormattedStr;

end;

function EncryptMsg (Msg1: string; EncryptNo: integer): string;

var
  ResultStr: string;
  Temp: char;
  I, EncryptIndex: integer;

begin

  ResultStr := '';
  Temp := ' ';

  { Encrypt message routine }

  for I := 1 to length(Msg1) do

      begin

         for EncryptIndex := 1 to EncryptNo do

            begin
              Temp := Succ (Msg1[I]);
              Msg1[I] := Temp;
            end;

        ResultStr := ResultStr + Temp;

      end;

  EncryptMsg := ResultStr;

end;

function DecryptMsg (Msg1: string; DecryptNo: integer): string;

var
  ResultStr: string;
  Temp: char;
  I, DecryptIndex: integer;

begin

  ResultStr := '';
  Temp := ' ';

  { Decrypt message routine }

  for I := 1 to length(Msg1) do

      begin

         for DecryptIndex := 1 to DecryptNo do

            begin
              Temp := Pred (Msg1[I]);
              Msg1[I] := Temp;
            end;

        ResultStr := ResultStr + Temp;

      end;

  DecryptMsg := ResultStr;

end;

function StrExtractCmd (CommandStr: string; optParam:
                        integer): string ;

var
  Temp: string;
  P: integer;
begin

  P := Pos('=', CommandStr);

  if optParam = 1 then
     Temp := Copy(CommandStr, 1, P - 1)
  else
     Temp := Copy(CommandStr, P + 1, Length (CommandStr) - P);

  StrExtractCmd := Temp;

end;

function StrStripSpaces (Msg1: string): string;

var
  Temp: string;
  I: integer;

begin

  { Purge blank spaces from string }

  Temp := '';

  for I := 1 to Length (Msg1) do
      if Msg1 [I] <> ' ' then
         Temp := Temp + Msg1 [I];

  StrStripSpaces := Temp;

end;

function IsCommented (CmdStr: String): Boolean;

var
  CommentStatus: Boolean;

begin

  { Remove spaces from string before checking
    to see if it contains a comment }

  CmdStr := StrStripSpaces (CmdStr);
  CommentStatus := False;

  if Pos ('REM', Uppercase (CmdStr)) <> 0 then
     CommentStatus := True;

  if Pos (';', CmdStr) <> 0 then
     CommentStatus := True;

  IsCommented := CommentStatus;

end;
                                           (*
function WinDir (optWin: integer ): string;
const
  WINBUF = 144;
var
  WinArray, WinSysArray: array [0..144] of char;

begin

  { Get \Windows directory or \Windows\System
    directory }

  if optWin = 0 then
     begin
       GetWindowsDirectory (WinArray, WINBUF);
       WinDir := StrPas (WinArray);
     end
  else
     begin
       GetSystemDirectory (WinSysArray, WINBUF);
       WinDir := StrPas (WinSysArray);
     end;
end;

function WinVer: string;

var
  WindVersion: word;
begin

  { Get Windows version number }

  WindVersion := GetVersion;
  WinVer := IntToStr (Hi(WindVersion));

end;

function CheckPentium: integer;

{ Check if Pentium processor if faulty }

const
  tstBugVal1: single = 4195835.0;
  tstBugVal2: single = 3145727.0;
var
  Answer: double;

begin

{$U-}

  Answer := tstBugVal1 / tstBugVal2;

{$U+}

  if tstBugVal1 - Answer * tstBugVal2 > 1.0 then
     CheckPentium := 0
  else
     CheckPentium := 1;

end;
       *)
function StrDeleteAll (S: string; DelChar: char): string;

var
  Temp: string;
  I: integer;

begin

  { Remove all occurences of the specified
    character from string }

  Temp := '';

  for I := 1 to Length (S) do
      begin
        if S[I] <> DelChar then
           Temp := Temp + S[I];
      end;

  StrDeleteAll := Temp;

end;

function StrReplaceAll (S: string; OldChar, NewChar: char): string;

var
  Temp: string;
  I: integer;

begin

  { Remove all occurences of OldChar with
    NewChar }

  Temp := '';

  for I := 1 to Length (S) do
      begin
        if S[I] <> OldChar then
           Temp := Temp + S[I]
        else
           Temp := Temp + NewChar;

      end;

  StrReplaceAll := Temp;

end;

function IsNumVal (StrVal: string): boolean;

{ Returns True if the value contained within
  a string evaluates to be numeric }

var
  I, DecimalCount: Integer;
  InvalChar: Boolean;

begin

  InvalChar := False;
  DecimalCount := 0;

  for I := 1 to Length (StrVal) do begin
      if (StrVal [I] < '0') or
         (StrVal [I] > '9') then
          if StrVal [I] = '.' then
             DecimalCount := DecimalCount + 1
          else begin
             if I = 1 then begin
                if StrVal [I] <> '-' then
                   InvalChar := True;
                end
             else
                InvalChar := True;
             end;
  end; { for }

  if (InvalChar) or (DecimalCount > 1) then
      IsNumVal := False
  else
      IsNumVal := True;

end;

function StrReverse (S: string): string;

var
  Temp: string;
  I: integer;

begin

  { Traverse string }

  Temp := '';

  for I := Length (S) downto 1 do
      Temp := Temp + S[I];

  StrReverse := Temp;

end;

end.
