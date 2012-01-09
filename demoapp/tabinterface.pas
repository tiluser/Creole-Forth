(*
Copyright 2003 Joseph M. O'Connor Inc. All rights reserved. (Derived from the FreeBSD copyright at
http://www.freebsd.org/copyright/freebsd-license.html).

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED `AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those of the author and should not be interpreted as representing official policies, either expressed or implied.

*)

unit tabinterface;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, IdBaseComponent, IdComponent, IdTCPServer,
  IdCustomHTTPServer, IdHTTPServer, Creole , TILable, Dialogs, FileCtrl,
  crsysutils;


CONST SendKey = 'SK8765.DLL';

FUNCTION EnumerateWindows(hWnd : HWND; lParam : LPARAM) : BOOL; STDCALL;
{Declare the exported function in our DLL.}
function SendKeys(lpszMacro: PCHAR): LRESULT stdcall; external SendKey name 'SendKeys';

type
  TPagesDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    CreoleExtSheet: TTabSheet;
    WSSheet: TTabSheet;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    TabSheet1: TTabSheet;
    Creole1: TCreole;
    IdHTTPServer1: TIdHTTPServer;
    InputMemo1: TMemo;
    DataStackMemo: TMemo;
    VocabStackMemo: TMemo;
    PostfilterStackMemo: TMemo;
    PrefilterStackMemo: TMemo;
    Label1: TLabel;
    OutputMemo1: TMemo;
    CreoleIntSheet: TTabSheet;
    DictLB: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PADMemo: TMemo;
    Label7: TLabel;
    Label8: TLabel;
    PortNumEdit: TEdit;
    Label9: TLabel;
    HomePageEdit: TEdit;
    Label10: TLabel;
    InputMemo2: TMemo;
    Label11: TLabel;
    OutputMemo2: TMemo;
    Label12: TLabel;
    EnumWinMemo: TMemo;
    EnumWinBtn: TButton;
    StopEnumWinBtn: TButton;
    RadioGroup1: TRadioGroup;
    OnRB: TRadioButton;
    OffRB: TRadioButton;
    SubmitBtn: TButton;
    Label13: TLabel;
    ReturnStackMemo: TMemo;
    LoadSB1: TSpeedButton;
    SaveSB1: TSpeedButton;
    RecallSB1: TSpeedButton;
    SaveSB2: TSpeedButton;
    RecallSB2: TSpeedButton;
    CLSSB2: TSpeedButton;
    CLSSB1: TSpeedButton;
    CleanupStackBtn: TButton;
    AboutBtn: TButton;
    TypeFieldEdit: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    CodeFieldEdit: TEdit;
    Label16: TLabel;
    PFMemo: TMemo;
    Label17: TLabel;
    DFMemo: TMemo;
    Label18: TLabel;
    HFMemo: TMemo;
    Label19: TLabel;
    VocabEdit: TEdit;
    Button1: TButton;
    FileListBox1: TFileListBox;
    Label20: TLabel;
    TextSendEdit: TEdit;
    Label21: TLabel;
    Button3: TButton;
    HelpWebServerBtn: TButton;
    DLLHelpBtn: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    procedure AboutBtnClick(Sender: TObject);
    procedure IdHTTPServer1CommandGet(AThread: TIdPeerThread;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure FormCreate(Sender: TObject);
    procedure CleanupStackBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure DictLBClick(Sender: TObject);
    procedure OnRBClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure GotoHomePageBtnClick(Sender: TObject);
    procedure LoadSB1Click(Sender: TObject);
    procedure EnumWinBtnClick(Sender: TObject);
    procedure StopEnumWinBtnClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure HelpWebServerBtnClick(Sender: TObject);
    procedure DLLHelpBtnClick(Sender: TObject);
  private
    { Private declarations }
    PROCEDURE ClearInterfaces;
    // procedures for submitting and receiving output for external-internal
    // tabs.
    PROCEDURE InitInterfaces1;
    PROCEDURE SubmitInterfaces1;
    // procedures for submitting and receiving for web server demo tab.
    PROCEDURE InitInterfacesHTTPD;
    PROCEDURE SubmitInterfacesHTTPD;
    FUNCTION GetSelectedIndex(LB : TListBox) : integer;
  public
    { Public declarations }
  end;

var
  PagesDlg: TPagesDlg;

implementation

uses cr2about;

{$R *.dfm}

PROCEDURE TPagesDlg.ClearInterfaces;
BEGIN
  DataStackMemo.Lines.Clear;
  ReturnStackMemo.Lines.Clear;
  VocabStackMemo.Lines.Clear;
  PreFilterStackMemo.Lines.Clear;
  PostFilterStackMemo.Lines.Clear;
  InputMemo1.Lines.Clear;
  OutputMemo1.Lines.Clear;
  PADMemo.Lines.Clear;
  InputMemo2.Lines.Clear;
  OutputMemo2.Lines.Clear;
  EnumWinMemo.Lines.Clear;
END;

PROCEDURE TPagesDlg.InitInterfaces1;
BEGIN
  DataStackMemo.Lines := Creole1.DataStack;
  ReturnStackMemo.Lines := Creole1.ReturnStack;
  VocabStackMemo.Lines := Creole1.VocabStack;
  PreFilterStackMemo.Lines := Creole1.PrefilterStack;
  PostFilterStackMemo.Lines := Creole1.FilterStack;
  InputMemo1.Lines := Creole1.Input;
  OutputMemo1.Lines := Creole1.Output;
  PADMemo.Lines := Creole1.PAD;
END;

PROCEDURE TPagesDlg.SubmitInterfaces1;
BEGIN
  Creole1.DataStack      := DataStackMemo.Lines ;
  Creole1.ReturnStack    := ReturnStackMemo.Lines;
  Creole1.VocabStack     := VocabStackMemo.Lines;
  Creole1.PrefilterStack := PreFilterStackMemo.Lines;
  Creole1.FilterStack    := PostFilterStackMemo.Lines;
  Creole1.Input          := InputMemo1.Lines;
  Creole1.Output         := OutputMemo1.Lines;
  Creole1.PAD            := PADMemo.Lines;
END;

PROCEDURE TPagesDlg.InitInterfacesHTTPD;
BEGIN
  InputMemo2.Lines := Creole1.Input;
  OutputMemo2.Lines := Creole1.Output;
END;

PROCEDURE TPagesDlg.SubmitInterfacesHTTPD;
BEGIN
  Creole1.Input          := InputMemo1.Lines;
  Creole1.Output         := OutputMemo1.Lines;
END;

procedure TPagesDlg.CleanupStackBtnClick(Sender: TObject);
begin
  ClearInterfaces;
  VocabStackMemo.Lines.Append('ONLY');
  VocabStackMemo.Lines.Append('FORTH');
  PreFilterStackMemo.Lines.Append('COMMENTS');
  PostFilterStackMemo.Lines.Append('NONE');
end;

procedure TPagesDlg.SubmitBtnClick(Sender: TObject);
begin
  SubmitInterfaces1;
  Creole1.Submit;
  InitInterfaces1;
end;

FUNCTION TPagesDlg.GetSelectedIndex(LB : TListBox) : integer;
VAR
  I : integer;
BEGIN
  FOR I := 0 TO LB.Items.Count-1 DO
  BEGIN
    IF LB.Selected[I] THEN
    BEGIN
      Result := I;
      Break;
    END;
  END;
END;

procedure TPagesDlg.DictLBClick(Sender: TObject);
VAR
  CW : TCreoleWord;
  SelectIndex : integer;
begin
  SelectIndex := GetSelectedIndex(DictLB);
  CW := Creole1.Dictionary.Objects[SelectIndex] AS TCreoleWord;
  VocabEdit.Text := CW.GetField('Vocabulary');
  TypeFieldEdit.Text := CW.GetField('TypeField');
//  VocabEdit.Text := CW.GetField('CodeField');  - to do
  PFMemo.Lines.Text := CW.GetField('ParamField');
  DFMemo.Lines.Text := CW.GetField('DataField');
  HFMemo.Lines.Text := CW.GetField('HelpField');
end;

procedure TPagesDlg.OnRBClick(Sender: TObject);
begin
  IdHttpServer1.DefaultPort := StrToIntDef(PortNumEdit.Text,3535);
  IdHttpServer1.Active := OnRB.Checked;
end;

procedure TPagesDlg.Button1Click(Sender: TObject);
VAR
  ExecStr : STRING;
begin
  ExecStr := 'Notepad.exe ' + HomePageEdit.Text;
  WinExec(pchar(ExecStr),SW_SHOWNORMAL);
end;

procedure TPagesDlg.GotoHomePageBtnClick(Sender: TObject);
VAR
  ExecStr : STRING;
begin
  ExecStr := '"C:\Program Files\Netscape\Netscape 6\netscp6.exe" ' +
  'http://localhost:' + PortNumEdit.Text;
  IF NOT OnRB.Checked THEN ShowMessage('Error: Web server is currently offline')
  ELSE
    WinExec(pchar(ExecStr),SW_SHOWNORMAL);
    ShowMessage(Creole1.Output.Text);
end;

procedure TPagesDlg.LoadSB1Click(Sender: TObject);
begin
 // Creole1.Push(Creole1.DataStack,'c:\zip\crws');
//  Creole1.Push(Creole1.DataStack,'index.html');
    Creole1.Input.Append('c:/zip/crws');
    Creole1.Input.Append('index.html');
    DataStackMemo.Lines := Creole1.DataStack;
 //   Creole1.Input.Append('HOMEPAGE');
    Creole1.Submit;
end;

PROCEDURE DoEnumWindows(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  TheWindow : HWND;
begin
  EnumWindows(@EnumerateWindows,0);
end;

FUNCTION EnumerateWindows(hWnd : HWND; lParam : LPARAM) : BOOL;
VAR
  TheText : ARRAY[0..255] OF char;
BEGIN
  IF (GetWindowText(hWnd, TheText, 255)=0) THEN
    { display window handle and a note }
    PagesDlg.EnumWinMemo.Lines.Append(Format('%d = {This window has no text)',[hWnd]))
  ELSE
    { otherwise display the window handle and the window text }
    PagesDlg.EnumwinMemo.Lines.Append(Format('%d = %s',[hWnd,TheText]));
    { continue enmeration }
    Result := True;
 //   ShowMessage(IntToStr(PagesDlg.EnumwinMemo.Lines.Count));
END;

PROCEDURE DoSend(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  TheHandle :  HWND;
BEGIN
  TheHandle:=WinExec('Notepad',SW_SHOWNORMAL);
  SetForegroundWindow(TheHandle);
  SetFocus(TheHandle);
  SendKeys(pchar(EI.Pop(EI.DataStack)));
END;

// Clear the input memo field
PROCEDURE DoCLS(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  p : pointer;
BEGIN
  PagesDlg.InputMemo1.Lines.Clear;
  PagesDlg.InputMemo1.SetFocus;
END;

// Renders home page
PROCEDURE DoHomePage(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  FileName, Directory : STRING;
BEGIN
  FileName := EI.Pop(EI.DataStack);
  Directory := EI.Pop(EI.DataStack);
  FileName := Directory + '\' + FileName;
  EI.Output.LoadFromFile(FileName);
END;

PROCEDURE DoHTMLHelp(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  FileName, Directory : STRING;
  I : integer;
BEGIN
  FOR I := 0 TO DI.Dictionary.Count-1 DO
  BEGIN
    CWI := DI.Dictionary.Objects[I] AS TCreoleWord;
    EI.Output.Append(CWI.GetField('NameField') + '. ' + CWI.GetField('HelpField')
    +  '. In ' + CWI.GetField('Vocabulary') + ' Vocabulary<BR>');
  END;
  EI.Output.Append('<BR>');
  EI.Output.Append('Currently there are ' + IntToStr(DI.Dictionary.Count) +
  ' definitions in the Creole dictionary<BR>');
END;

// produces HTML code to make button to take user back to the home page.
PROCEDURE DoHomePageBtn(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  FileName, Directory : STRING;
  I : integer;
BEGIN
  EI.Output.Append('<HR>');
  EI.Output.Append('<BR>');
  EI.Output.Append('<FORM METHOD=GET ACTION="http://localhost:3535">');
  EI.Output.Append('Click on button below to return to the home page.');
  EI.Output.Append('<INPUT TYPE=submit NAME=Home VALUE=Home>');
  EI.Output.Append('</FORM>');
END;

// Leaves selected filename on the stack
PROCEDURE DoFileDialog(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  FD : TOpenDialog;
  DirStr, FileStr : STRING;
  FileStrings : TStrings;
BEGIN
  FD := TOpenDialog.Create(Owner);
  FD.FileName := '*.pl';
  FD.Filter := '*.pl';
  IF FD.Execute THEN
  BEGIN
    FileStr := FD.FileName;
    EI.Push(EI.DataStack,FileStr);
  END;
  FD.Free;
END;

// Runtime code for an external script
PROCEDURE DoScript(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce;
                   CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  I, CutPoint : integer;
  ScriptFile,ExecStr, ArgvStr, StackPath : STRING;
  FileStrings : TStrings;
BEGIN
  ScriptFile := CWI.GetField('DataField');
  ExecStr  := 'c:\perl\bin\perl.exe ' + '"' + ScriptFile + '"';

  CutPoint := Pos(RemovePath(ScriptFile),ScriptFile)-1;
  StackPath := Copy(CWI.GetField('DataField'),1,CutPoint);

  FOR I := EI.DataStack.Count-1 DOWNTO 0 DO
  BEGIN
    ArgvStr := ArgvStr + ' ' + EI.DataStack[I];
  END;
  FileStrings := TStringList.Create;
  FileStrings.Append('@echo off');
  FileStrings.Append(ExecStr + ' ' + ArgvStr);
 // FileStrings.Append('pause');
  FileStrings.SaveToFile('execme.bat');
  WinExec('execme.bat',SW_SHOWNORMAL);
  // wait for this file, then exit
  ShowMessage('Script Executed');
  WHILE NOT FileExists(StackPath + 'stack.txt') DO Application.ProcessMessages;
  FileStrings.Free;
  EI.DataStack.LoadFromFile(StackPath + 'stack.txt');
END;

// Reference code written in another language; ie. Perl, TCL, Python, REXX, Lisp,
// Java. Usually it's a scripting language.
PROCEDURE CompileScript(Owner : TComponent; EI : TExtIntfce; DI : TDictIntfce; CI : TCtrlIntfce; CWI : TCreoleWord);
VAR
  AWord : TCreoleWord;
BEGIN
  AWord := TCreoleWord.Create;
  AWord.SetField('NameField',ParseStrWSD(EI.Input.Text,EI.GetOPtr+1));
  AWord.CodeField := DoScript;
  AWord.DataField := EI.Pop(EI.DataStack);
  AWord.IndexField := DI.Dictionary.Count;
  DI.Dictionary.AddObject(EncryptMsg(AWord.NameField,EI.GetCurrent,5,1),AWord);
  EI.SetOptr(EI.GetOPtr+2);
END;

procedure TPagesDlg.AboutBtnClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TPagesDlg.IdHTTPServer1CommandGet(AThread: TIdPeerThread;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
VAR
  DocStr, DateTimeStr, MathStr, HelpStr : STRING;
begin
  Creole1.Output.Clear;
  DocStr := Copy(ARequestInfo.Document,2,Length(ARequestInfo.Document)-1);
  DateTimeStr := ARequestInfo.Params.Values['Date'];
  MathStr := ARequestInfo.Params.Values['mathproblem'];
  HelpStr := ARequestInfo.Params.Values['help'];
  IF (Length(DocStr)+ Length(DateTimeStr) + Length(MathStr) + Length(HelpStr)) = 0 THEN
  BEGIN
    Creole1.Push(Creole1.DataStack,FileListBox1.Directory);
    Creole1.Push(Creole1.DataStack,HomePageEdit.Text);
    Creole1.Input.Append('HOMEPAGE');
  END
  ELSE
  BEGIN
    IF Length(DateTimeStr) <> 0 THEN
      Creole1.Input.Append(' { Current date and time is : } . ' + DateTimeStr);
    IF Length(MathStr) <> 0 THEN
    BEGIN
      MathStr := '{ <BR><BR>Results of calculation : }  . ' + MathStr + ' . ';
      Creole1.Input.Append(MathStr);
    END;
    IF Length(HelpStr) <> 0 THEN
    BEGIN
      Creole1.Input.Append(' {  <BR><BR>List of available definitions in Creole : <BR> } . ');
      Creole1.Input.Append('WWW' + HelpStr);
    END;
    Creole1.Input.Append('HPBTN');
    InputMemo2.Lines := Creole1.Input;
  END;
  Creole1.Submit;
  Application.ProcessMessages;
  AResponseInfo.ContentText := Creole1.Output.Text;
  OutputMemo2.Lines := Creole1.Output;
end;

procedure TPagesDlg.FormCreate(Sender: TObject);
VAR
  I : integer;
  CW : TCreoleWord;
  NameField : STRING;
begin
  Creole1.Submit2;      // Build dictionary objects
  Creole1.BuildPrimitive('HOMEPAGE',DoHomePage,'','','FORTH');
  Creole1.BuildPrimitive('WWWHELP',DoHTMLHelp,'','','FORTH');
  Creole1.BuildPrimitive('HPBTN',DoHomePageBtn,'','','FORTH');
  Creole1.BuildPrimitive('DLG',DoFileDialog,'','','FORTH');
  Creole1.BuildPrimitive('PSCRIPT',CompileScript,'','','FORTH');
  Creole1.BuildPrimitive('CLS',DoCLS,'','','FORTH');  
  Creole1.BuildPrimitive('LISTWIN',DoEnumWindows,'','','FORTH');
  Creole1.BuildPrimitive('SM',DoSend,'','','FORTH');
  ClearInterfaces;      // Remove names that appear in design-time memos.
   // Take care of these too.
  VocabEdit.Text := '';
  TypeFieldEdit.Text := '';
  CodeFieldEdit.Text := '';
  TextSendEdit.Text :='';
  PFMemo.Lines.Clear;
  DFMemo.Lines.Clear;
  HFMemo.Lines.Clear;
  InitInterfaces1;      // Set interfaces to Creole's data structures; ie
  InitInterfacesHTTPD;  // data stack memo is set to stack.

  FOR I := 0 TO Creole1.Dictionary.Count - 1 DO
  BEGIN
    CW := Creole1.Dictionary.Objects[I] AS TCreoleWord;
    NameField := CW.GetField('NameField');
    DictLB.Items.Append(IntToStr(I) + '. ' + NameField);
  END;
end;


procedure TPagesDlg.EnumWinBtnClick(Sender: TObject);
begin
  Creole1.Input.Append('LISTWIN');
  Creole1.Submit;
end;

procedure TPagesDlg.StopEnumWinBtnClick(Sender: TObject);
VAR
  TempStr, TempStr2 : STRING;
begin
  Creole1.Input.Append('SM');
  Creole1.Push(Creole1.DataStack,TextSendEdit.Text);
  Creole1.Submit;
 // DataStackMemo.Lines := Creole1.DataStack;
end;

procedure TPagesDlg.Button3Click(Sender: TObject);
begin
  WinExec('notepad .\help\perlex.txt',SW_SHOWNORMAL);
end;

procedure TPagesDlg.HelpWebServerBtnClick(Sender: TObject);
begin
  WinExec('notepad .\help\webserverex.txt',SW_SHOWNORMAL);
end;

procedure TPagesDlg.DLLHelpBtnClick(Sender: TObject);
begin
    WinExec('notepad .\help\dllex.txt',SW_SHOWNORMAL);
end;

end.

