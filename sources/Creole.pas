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
unit Creole;

{$MODE Delphi}

interface

uses
//  LCLIntf,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Coreprims, TILable, Interpreter, Compiler1, Compiler2, CrSysUtils;

type
  TDictChgEvent = PROCEDURE(Sender : TObject) OF OBJECT;
  TCreole = class(TComponent)
  private
    { Private declarations }
    IsDictChanged : Boolean;
    // Data structures
    FDataStack, FReturnStack,
    FInput, FOutput, FPAD,
    FPreFilterStack, FPostFilterStack,
    FVocabStack : TStrings;
    FDictionary                   : TStringList;
    FOnDictChange : TDictChgEvent;
    // Interfaces;
    EI : TExtIntfce;
    DI : TDictIntfce;
    CI : TCtrlIntfce;
    CWI : TCreoleWord;
    PP : PrimProc;
    AWord, BWord : TCreoleWord;

    //  Accessor functions for data structures
    PROCEDURE SetDataStack(Value       :  TStrings);
    PROCEDURE SetReturnStack(Value     :  TStrings);
    PROCEDURE SetInput(Value           :  TStrings);
    PROCEDURE SetOutput(Value          :  TStrings);
    PROCEDURE SetPAD(Value             :  TStrings);
    PROCEDURE SetDictionary(Value      :  TStringList);
    PROCEDURE SetPreFilterStack(Value  :  TStrings);
    PROCEDURE SetPostFilterStack(Value :  TStrings);
    PROCEDURE SetVocabStack(Value      :  TStrings);
    // Buildup and free the startup defs
    PROCEDURE BuildDefs;
    PROCEDURE FreeDefs;
  protected
    { Protected declarations }
    PROCEDURE DictChanged;  // dictionary change dispatch method
  public
    { Public declarations }
    PROCEDURE BuildPrimitive(NF : STRING; CF : PrimProc; CFStr, PF, DF,HF,Vocab : STRING);
    PROCEDURE ReplacePrimitive(NF : STRING; CF : PrimProc; CFStr, Vocab : STRING);
    // Has to be called at application startup
    PROCEDURE BuildHighLevel(DefStr,HelpDefStr : STRING);
    PROCEDURE RebuildDefs;
    CONSTRUCTOR Create(AOwner : TComponent); OVERRIDE; // OVERRIDE is
                                                       // IMPORTANT!!!
    DESTRUCTOR  Free;
    PROCEDURE Push(TheStrings : TStrings; Str1 : STRING);
    FUNCTION  Pop(TheStrings : TStrings) : STRING;
    PROCEDURE Submit;
    PROCEDURE ClearInterfaces;
  published
    { Published declarations }
    // Exposed data structures - data stack, return stack, input, output,
    // dictionary, and PAD
    PROPERTY DataStack : TStrings
    READ FDataStack WRITE SetDataStack;

    PROPERTY ReturnStack : TStrings
    READ FReturnStack WRITE SetReturnStack;

    PROPERTY Input : TStrings
    READ FInput WRITE SetInput;

    PROPERTY Output : TStrings
    READ FOutput WRITE SetOutput;

    PROPERTY PAD : TStrings
    READ FPAD WRITE SetPAD;
    // Dictionary access
    PROPERTY Dictionary : TStringList
    READ FDictionary WRITE SetDictionary;

    // List of rules for transforming input stream before outer interpreter
    // processes it.
    PROPERTY PreFilterStack : TStrings
    READ FPreFilterStack WRITE SetPreFilterStack;

    // List of rules for transforming data.
    PROPERTY PostFilterStack : TStrings
    READ FPostFilterStack WRITE SetPostFilterStack;

    PROPERTY VocabStack : TStrings
    READ FVocabStack WRITE SetVocabStack;

    property OnDictChange : TDictChgEvent READ FOnDictChange write FOnDictChange;
  end;

procedure Register;

implementation

CONSTRUCTOR TCreole.Create(AOwner : TComponent);
BEGIN
  INHERITED Create(AOwner);

  FDataStack       := TStringList.Create;    // Data stack
  FReturnStack     := TStringList.Create;    // Return stack
  FInput           := TStringList.Create;
  FOutput          := TStringList.Create;
  FPAD             := TStringList.Create;    // Good for building colon defs
  FDictionary      := TStringList.Create;    // Forth dictionary
  FPreFilterStack  := TStringList.Create;
  FPreFilterStack.Insert(0,'COMMENTS');
  FPostFilterStack := TStringList.Create;
  FPostFilterStack.Insert(0,'NOFILTER');
  FVocabStack      := TStringList.Create;
  FVocabStack.Insert(0,'ONLY');
  FVocabStack.Insert(0,'FORTH');
  EI := TExtIntfce.Create;
  DI := TDictIntfce.Create;
  CI := TCtrlIntfce.Create;
  CWI := TCreoleWord.Create;

  EI.DataStack := FDataStack;
  EI.ReturnStackIF := FReturnStack;
  EI.SetField('ContextVocab','FORTH');
  BuildDefs;
END;
(*
  BuildPrimitive('EDIT',DoEditor,'','Invoke Editor','','FORTH');

                 *)
PROCEDURE TCreole.BuildDefs;
BEGIN
  // Vocabularies - ONLY, FORTH, PREFILTER, POSTFILTER, and IMMEDIATE
  BuildPrimitive('ONLY',DoVocab,'DoVocab','','','Minimal vocabulary always on the vocab stack','ONLY');
  BuildPrimitive('FORTH',DoVocab,'DoVocab','','','Main Creole Forth vocabulary','ONLY');

  // Filters - these are the rules that test and convert
  // the data before placing it on the stack (or whatever
  // data structure is designated). For example, a filter
  // rule can test that an input is a valid integer before
  // placing it on the stack. Currently all the filters are
  // in the FILTER vocabulary.

  // The 'NONE' filter just pushes the data on the stack without
  // any validation checking whatsoever.
  BuildPrimitive('COMMENTS',DoStripCmtPreFilter,'DoStripCmtPreFilter','','','Strips out comments','PREFILTER');
  BuildPrimitive('NOFILTER',DoNOP,'DoNOP','','','No prefiltering','PREFILTER');
  BuildPrimitive('NOFILTER',DoNothingFilter,'DoNothingFilter','','','No filtering','POSTFILTER');
  BuildPrimitive('INTEGER',DoIntegerFilter,'DoIntegerFilter','','','Integers only','POSTFILTER');
  BuildPrimitive('FLOAT',DoFloatFilter,'DoFloatFilter','','','Numbers only','POSTFILTER');
  BuildPrimitive('HEX',DoHexFilter,'DoHexFilter','','','Hex numbers','POSTFILTER');

  // FORTH wordset with some IMMEDIATE words
  BuildPrimitive('NOP',DoNOP,'DoNOP','','','Do nothing','FORTH');
  BuildPrimitive('+',DoPlus,'DoPlus','','','( n1 n2 -- sum)','FORTH');
  BuildPrimitive('-',DoMinus,'DoMinus','','','( n1 n2 -- diff)','FORTH');
  BuildPrimitive('*',DoMultiply,'DoMultiply','','','( n1 n2 -- product)','FORTH');
  BuildPrimitive('/',DoDivide,'DoDivide','','','( n1 n2 -- floored dividend) integer division','FORTH');
  BuildPrimitive('/R',DoDivideR,'DoDivideR','','','( n1 n2 -- dividend) real division','FORTH');
  BuildPrimitive('MOD',DoMod,'DoMod','','','( n1 n2 -- remainder)','FORTH');
  BuildPrimitive('NIP',DoNip,'DoNip','','','( n1 n2 -- n2) Removes 2nd stack arg','FORTH');
  BuildPrimitive('TUCK',DoTuck,'DoTuck','','','( n1 n2 -- n2 n1 n2) Copies top stack item under second item','FORTH');
  BuildPrimitive('OVER',DoOver,'DoOver','','','( n1 n2 -- n1 n2 n1) Copies second item to the top of the stack','FORTH');
  BuildPrimitive('DROP',DoDrop,'DoDrop','','','( n1 -- ) Drops top item off stack','FORTH');
  BuildPrimitive('DUP',DoDup,'DoDup','','','( n1 -- n1 n1) Duplicates top item','FORTH');
  BuildPrimitive('2DUP',Do2Dup,'Do2Dup','','','( n1 n2 -- n1 n2 n1 n2 ) Dups. top 2 items.','FORTH');
  BuildPrimitive('SWAP',DoSwap,'DoSwap','','','( n1 n2 -- n2 n1) Exchanges top 2 items','FORTH');
  BuildPrimitive('ROT',DoRot,'DoRot','','','( n1 n2 n3 -- n2 n3 n1). Brings 3rd item to top','FORTH');
  BuildPrimitive('-ROT',DoMinusRot,'DoMinusRot','','','( n1 n2 n3 -- n3 n1 n2) Brings top item to 3rd','FORTH');
  BuildPrimitive('DEPTH',DoDepth,'DoDepth','','','( -- n) count of items on stack','FORTH');
  BuildPrimitive('=',DoEquals,'DoEquals','','','( n1 n2 -- n) -1 if true, 0 if false.','FORTH');
  BuildPrimitive('<>',DoNotEquals,'DoNotEquals','','','( n1 n2 -- n) -1 if true, 0 if false.','FORTH');
  BuildPrimitive('<',DoLessThan,'DoLessThan','','','( n1 n2 -- n) -1 if true, 0 if false.','FORTH');
  BuildPrimitive('>',DoGreaterThan,'DoGreaterThan','','','( n1 n2 -- n) -1 if true, 0 if false.','FORTH');
  BuildPrimitive('.',DoDot,'DoDot','','','Prints top item to output','FORTH');
  BuildPrimitive('.S',DoSDot,'DoSDot','','','Nondestructive stack print','FORTH');
  BuildPrimitive('[lit]',CompileLit,'CompileLit','','','compile lit def','FORTH');
  BuildPrimitive('lit',DoLit,'DoLit','','','lit def','FORTH');
  BranchVals[LitVal] := FDictionary.Count-1;

  BuildPrimitive('."',CompileDotQuote,'CompileDotQuote','','','compile a quoted string into a colon def.','IMMEDIATE');
  BuildPrimitive('do."',DoDotQuote,'DoDotQuote','','','print a quoted string inside a colon def.','FORTH');
  BranchVals[DotQVal1] := FDictionary.Count-1;
  BuildPrimitive('CREATE',DoCreate,'DoCreate','','','Creates entry in dictionary','FORTH');
  BuildPrimitive('DOES>',CompileDoes,'CompileDoes','','','Sets up runtime code for a defining word','FORTH');
  BuildPrimitive('HERE',DoHere,'DoHere','','','( -- address) returns current top of dictionary','FORTH');
  BuildPrimitive(',',DoComma,'DoComma','','',
  '( val address -- ) Compiles a value into datafield for word at address','FORTH');
  BuildPrimitive('@',DoFetch,'DoFetch','','','( address -- value) Fetches data at address','FORTH');
  BuildPrimitive('!',DoStore,'DoStore','','','( address value --) Stores data at address','FORTH');
  BuildPrimitive(':',CompileColon,'','','CompileColon','Compiles a definition','FORTH');
  BuildPrimitive(';',DoSemi,'DoSemi','','','Terminates a  colon definition','IMMEDIATE');

  BuildPrimitive('IMMEDIATE',DoImmediate,'DoImmediate','','','Places a word in IMMEDIATE vocabulary','FORTH');
  BuildPrimitive('DEFINITIONS',DoContextToCurrent,'DoContextToCurrent','','',
  'Sets vocabulary where words are defined to the current one','FORTH');
  BuildPrimitive('VOCABULARY',CompileVocab,'CompileVocab','','','Creates a new vocabulary','FORTH');
  BuildPrimitive('FORGET',DoForget,'DoForget','','',
  'Removes definition from dictionary and words defined after it','FORTH');

  BuildPrimitive('DO',CompileDo,'CompileDo','','','Compiles a DO','IMMEDIATE');
  BuildPrimitive('StartDoDo',StartDoDo,'StartDoDo','','','One of the values compiled by CompileDo','FORTH');
  BranchVals[StartDoVal] := FDictionary.Count-1;
  BuildPrimitive('DoDo',DoDo,'DoDo','','','One of the values compiled by CompileDo','FORTH');
  BranchVals[DoVal] := FDictionary.Count-1;
  BuildPrimitive('LOOP',CompileLoop,'CompileLoop','','','Compiles a loop with the branch address','IMMEDIATE');
  BuildPrimitive('DoLoop',DoLoop,'DoLoop','','','Runtime code for LOOP','FORTH');
  BranchVals[LoopVal] := FDictionary.Count-1;
  BuildPrimitive('JMP',DoJump,'DoJump','','','Unconditional branching','FORTH');
  BranchVals[JmpVal] := FDictionary.Count-1;
  BuildPrimitive('IF',CompileIf,'CompileIf','','','Compile-time code for IF','IMMEDIATE');
  BuildPrimitive('0Branch',DoZeroBranch,'DoZeroBranch','','','Run-time code for IF','FORTH');
  BranchVals[IfVal] := FDictionary.Count-1;
  BuildPrimitive('ELSE',CompileElse,'CompileElse','','','Compile-time code for ELSE','IMMEDIATE');
  BuildPrimitive('DoElse',DoElse,'DoElse','','','Run-time code for ELSE','FORTH');
  BranchVals[ElseVal] := FDictionary.Count-1;
  BuildPrimitive('THEN',CompileThen,'CompileThen','','','Compile-Time code for THEN','IMMEDIATE');
  BuildPrimitive('DoThen',DoThen,'DoThen','','','Run-time code for THEN','FORTH');
  BranchVals[ThenVal] := FDictionary.Count-1;

  BuildPrimitive('BEGIN',CompileBegin,'CompileBegin','','','Compile-Time code for BEGIN','IMMEDIATE');
  BuildPrimitive('DoBegin',DoNop,'DoNop','','','Run-time code for BEGIN','FORTH');
  BranchVals[BeginVal]  := FDictionary.Count-1;
  BuildPrimitive('UNTIL',CompileUntil,'CompileUntil','','','Compile-Time code for UNTIL','IMMEDIATE');
  BuildPrimitive('DoUntil',DoUntil,'DoUntil','','','Run-time code for UNTIL','FORTH');
  BranchVals[UntilVal] := FDictionary.Count-1;

  BuildPrimitive('POSTPONE',CompilePostpone,'CompilePostpone','','','Used to define a compiling word','IMMEDIATE');
  BuildPrimitive('DoPostpone',DoPostpone,'DoPostpone','','','Runtime code for a compiling word','FORTH');
    BranchVals[DoCompVal] := FDictionary.Count-1;

  BuildPrimitive('Hello',DoHello,'DoHello','','','Hello def.','FORTH');
  BuildPrimitive('Goodbye',DoGoodbye,'DoGoodbye','','','Goodbye def.','FORTH');
  BuildPrimitive('CR',DoCR,'DoCR','','','Carriage return line feed','FORTH');
  BuildPrimitive('VLIST',DoVlist,'DoVlist','','','List words in dictionary','ONLY');
  BuildPrimitive('HELP',DoHelp,'DoHelp','','','Show word''s help field','FORTH');
  BuildPrimitive('DATE',DoDateTime,'DoDateTime','','','Date/Time Now','FORTH');

  BuildPrimitive('HASH',CompileHash,'CompileHash','','','Create a Hash Table','FORTH');
  BuildPrimitive('HASHF',DoHashFilter,'DoHashFilter','','','Hash Table Filter','FILTER');
  BuildPrimitive('>DS',DoToDS,'DoToDS','','','Redirect to another data structure','FORTH');
  BuildPrimitive('INSPECT',DoExamine,'DoExamine','','','Examine word contents','FORTH');
  BuildPrimitive('SEE',DoSee,'DoSee','','','First pass at a decompiler','FORTH');

  BuildPrimitive('{',CompileList,'CompileList','','','Creates a list and pushes on stack','FORTH');
  BuildPrimitive('}',DoSemi,'DoSemi','','','Terminates list compilation','LIST');
  BuildPrimitive(':HELP',CompileHelp,'CompileHelp','','','Help compiler','FORTH');
  BuildPrimitive(';HELP',DoSemi,'DoSemi','','','Terminates help compilation','LIST');
  BuildPrimitive('EXECUTE',DoExecute,'DoExecute','','','( addr --) Executes word at addr','FORTH');
  BuildPrimitive('''',DoTick,'DoTick','','','(  --) Pushes address of word in input stream on the stack','FORTH');
  BuildPrimitive('L{',CompileList2,'CompileList2','','','(  --) Compiles list onto stack','FORTH');
END;

DESTRUCTOR TCreole.Free;
BEGIN
  FDataStack.Free;
  FReturnStack.Free;
  FInput.Free;
  FOutput.Free;
  FPAD.Free;
  FDictionary.Free;
  FPreFilterStack.Free;
  FPostFilterStack.Free;
  FVocabStack.Free;
  EI.Free;
  DI.Free;
  CI.Free;
  CWI.Free;
  FreeDefs;
  INHERITED Free;
END;

PROCEDURE TCreole.Push(TheStrings : TStrings; Str1 : STRING);
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

FUNCTION TCreole.Pop(TheStrings : TStrings) : STRING;
BEGIN
  TRY
    Result := TheStrings[0];
    TheStrings.Delete(0);
  EXCEPT
    ShowMessage('Stack Underflow');
    TheStrings.Clear;
  END;
END;

PROCEDURE TCreole.BuildHighLevel(DefStr,HelpDefStr : STRING);
BEGIN
  ClearInterfaces;
  EI.PreFilterStack := FPreFilterStack;
  EI.PostFilterStack := FPostFilterStack;
  EI.VocabStack  := FVocabStack;
  DI.Dictionary  := FDictionary;
  EI.Input.Append(DefStr);
//  EI.Output.Append(HelpDefStr);
  DoOuter(Self,EI,DI,CI,CWI);
  ClearInterfaces;
END;

PROCEDURE TCreole.Submit;
VAR
  TempStack : TStrings;
  I : integer;
  DictCountBefore, DictCountAfter : integer;
BEGIN
  EI.DataStack   := FDataStack;
  EI.ReturnStackIF := FReturnStack;
  EI.Input       := FInput;
  EI.Output      := FOutput;
  EI.PAD         := FPAD;
  EI.PreFilterStack := FPreFilterStack;
  EI.PostFilterStack := FPostFilterStack;
  EI.VocabStack  := FVocabStack;
  DI.Dictionary  := FDictionary;
  DictCountBefore := FDictionary.Count;
  DoOuter(Self,EI,DI,CI,CWI);
  FDictionary    := DI.Dictionary;
  EI.DataStack   := FDataStack;
  EI.RefreshRSStackImage;
  FReturnStack := EI.ReturnStackIF;;
  FInput.Clear;
  fOutput      := EI.Output;
  FPAD := EI.PAD;
  FPreFilterStack := EI.PreFilterStack;
  FPostFilterStack := EI.PostFilterStack;
  FVocabStack  := EI.VocabStack;
  DictCountAfter := FDictionary.Count;
  IF (DictCountAfter <> DictCountBefore) THEN
  BEGIN
    IsDictChanged := True;
    DictChanged;
  END
  ELSE IsDictChanged := False;
END;

PROCEDURE TCreole.BuildPrimitive(NF : STRING; CF : PrimProc; CFStr, PF, DF, HF, Vocab : STRING);
VAR
  TempStr : STRING;
BEGIN
  AWord := TCreoleWord.Create;
  AWord.SetField('NameField',NF);
  AWord.SetCodeField(CF,CFStr);
  AWord.SetField('ParamField',PF);
  AWord.SetField('DataField',DF);
  AWord.SetField('HelpField',HF);
  AWord.SetField('Vocabulary',Vocab);
  AWord.IndexField := FDictionary.Count;
  AWord.SetField('TypeField','Primitive');
  TempStr := EncryptMsg(NF,Vocab,5,1);
  FDictionary.InsertObject(FDictionary.Count,TempStr,AWord);
END;

// Replaces the codefield of a primitive
PROCEDURE TCreole.ReplacePrimitive(NF : STRING; CF : PrimProc; CFStr, Vocab : STRING);
VAR
  CurrentWord : STRING;
  LookupIndex : integer;
  CWI : TCreoleWord;
BEGIN
  CurrentWord := EncryptMsg(NF,Vocab,5,1);
  LookupIndex := FDictionary.IndexOf(CurrentWord);
  IF (LookupIndex <> -1) THEN
  BEGIN
    CWI := DI.Dictionary.Objects[LookupIndex] AS TCreoleWord;
    CWI.SetCodeField(CF,CFStr);
    FDictionary.Objects[LookupIndex] := CWI;
  END
  ELSE
    MessageDlg('Error : word not found in the dictionary',mtError,[mbOK],0);
END;

PROCEDURE TCreole.RebuildDefs;
BEGIN
  FDictionary.Clear;
  BuildDefs;
END;

PROCEDURE TCreole.FreeDefs;
VAR
  I : word;
BEGIN
  FOR I := 0 TO FDictionary.Count-1 DO
  BEGIN
    AWord := FDictionary.Objects[I] AS TCreoleWord;
    AWord.Free;
  END;
END;

PROCEDURE TCreole.SetDataStack(Value : TStrings);
BEGIN
  FDataStack.Assign(Value);
END;

PROCEDURE TCreole.ClearInterfaces;
BEGIN
  DataStack.Clear;
  ReturnStack.Clear;
  Input.Clear;
  Output.Clear;
END;

PROCEDURE TCreole.SetReturnStack(Value : TStrings);
BEGIN
  FReturnStack.Assign(Value);
END;

PROCEDURE TCreole.SetInput(Value : TStrings);
BEGIN
  FInput.Assign(Value);
END;

PROCEDURE TCreole.SetOutput(Value : TStrings);
BEGIN
  FOutput.Assign(Value);
END;

PROCEDURE TCreole.SetPAD(Value : TStrings);
BEGIN
  FPAD.Assign(Value);
END;

PROCEDURE TCreole.SetDictionary(Value : TStringList);
BEGIN
  FDictionary.Assign(Value);
END;

PROCEDURE TCreole.SetPreFilterStack(Value : TStrings);
BEGIN
  FPreFilterStack.Assign(Value);
END;

PROCEDURE TCreole.SetPostFilterStack(Value : TStrings);
BEGIN
  FPostFilterStack.Assign(Value);
END;

PROCEDURE TCreole.SetVocabStack(Value : TStrings);
BEGIN
  FVocabStack.Assign(Value);
END;

PROCEDURE TCreole.DictChanged;
BEGIN
  IF Assigned(FOnDictChange) THEN
  BEGIN
    FOnDictChange(Self);
  END;
END;


procedure Register;
begin
  RegisterComponents('CreoleForth', [TCreole]);
end;

end.
