{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit forthscript; 

interface

uses
    TILable, Compiler1, Compiler2, coreprims, Creole, crsysutils, interpreter, 
  JMOStr, LMString, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('Creole', @Creole.Register); 
end; 

initialization
  RegisterPackage('forthscript', @Register); 
end.
