{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DKDatabase;

{$warn 5023 off : no warning about unused units}
interface

uses
  DK_SQLite3, USQLite3ListForm, USQLite3KeyPickForm, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DKDatabase', @Register);
end.
