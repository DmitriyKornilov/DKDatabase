{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DKDatabase;

{$warn 5023 off : no warning about unused units}
interface

uses
  DK_SQLite3, DK_DBUtils, USQLite3CheckList, DK_DBTable, UDBImages, 
  UDBTableSingle, UDBTableDouble, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DKDatabase', @Register);
end.
