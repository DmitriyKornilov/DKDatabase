unit USQLite3DoubleTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Forms, Graphics, ExtCtrls,


  DK_VSTTypes, DK_Vector, DK_Matrix, DK_DBTable, DK_PPI, DK_CtrlUtils,
  DK_SQLite3;

type

  { TSQLite3DoubleTable }

  TSQLite3DoubleTable = class(TForm)
    LeftPanel: TPanel;
    LeftQuery: TSQLQuery;
    RightQuery: TSQLQuery;
    RightPanel: TPanel;
    Splitter1: TSplitter;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    LeftDBTable: TDBTable;
    RightDBTable: TDBTable;
    LeftTotalWidth: Integer;
    RightTotalWidth: Integer;
    procedure LeftTableSelect;
    procedure RightTableSelect;
  public
    procedure SetLeftTable(const ASQLite3: TSQLite3;
                       const AFont: TFont;
                       const ATableName, AIDFieldName: String;
                       const AFieldNames, AColumnNames: TStrVector;
                       const AColumnTypes: TVSTColumnTypes;
                       const AColumnNeedValues: TBoolVector;
                       const AColumnWidths: TIntVector;
                       const AColumnAlignments: array of TAlignment;
                       const AIDNotZero, AHeaderVisible: Boolean;
                       const AOrderFieldNames: TStrVector = nil;
                       const AAutoSizeColumnNumber: Integer = 1;
                       const AKeys: TIntMatrix = nil;
                       const APicks: TStrMatrix = nil);
    procedure SetRightTable(const ASQLite3: TSQLite3;
                       const AFont: TFont;
                       const ATableName, AIDFieldName: String;
                       const AFieldNames, AColumnNames: TStrVector;
                       const AColumnTypes: TVSTColumnTypes;
                       const AColumnNeedValues: TBoolVector;
                       const AColumnWidths: TIntVector;
                       const AColumnAlignments: array of TAlignment;
                       const AIDNotZero, AHeaderVisible: Boolean;
                       const AOrderFieldNames: TStrVector = nil;
                       const AAutoSizeColumnNumber: Integer = 1;
                       const AKeys: TIntMatrix = nil;
                       const APicks: TStrMatrix = nil;
                       const AMasterIDFieldName: String = '');

  end;

var
  SQLite3DoubleTable: TSQLite3DoubleTable;

implementation

{$R *.lfm}

{ TSQLite3DoubleTable }

procedure TSQLite3DoubleTable.FormDestroy(Sender: TObject);
begin
  if Assigned(RightDBTable) then FreeAndNil(RightDBTable);
  if Assigned(LeftDBTable) then FreeAndNil(LeftDBTable);
end;

procedure TSQLite3DoubleTable.FormShow(Sender: TObject);
var
  TotalWidth: Integer;
begin
  TotalWidth:= LeftTotalWidth + RightTotalWidth;
  if TotalWidth<Constraints.MinWidth then
    TotalWidth:= Constraints.MinWidth;
  TotalWidth:= WidthFromDefaultToScreen(TotalWidth);
  if TotalWidth>Screen.Width then
    TotalWidth:= Screen.Width - 20;
  Width:= TotalWidth;
  FormToScreenCenter(Sender as TForm);
end;

procedure TSQLite3DoubleTable.LeftTableSelect;
begin
  if RightDBTable.Edit.IsEditing then
    RightDBTable.EditingCancel;
  RightDBTable.MasterIDUpdate(LeftDBTable.IDValue);
end;

procedure TSQLite3DoubleTable.RightTableSelect;
begin
  if LeftDBTable.Edit.IsEditing then
    LeftDBTable.EditingCancel;
end;

procedure TSQLite3DoubleTable.SetLeftTable(const ASQLite3: TSQLite3;
                       const AFont: TFont;
                       const ATableName, AIDFieldName: String;
                       const AFieldNames, AColumnNames: TStrVector;
                       const AColumnTypes: TVSTColumnTypes;
                       const AColumnNeedValues: TBoolVector;
                       const AColumnWidths: TIntVector;
                       const AColumnAlignments: array of TAlignment;
                       const AIDNotZero, AHeaderVisible: Boolean;
                       const AOrderFieldNames: TStrVector = nil;
                       const AAutoSizeColumnNumber: Integer = 1;
                       const AKeys: TIntMatrix = nil;
                       const APicks: TStrMatrix = nil);
begin
  if not Assigned(LeftDBTable) then
  begin
    LeftDBTable:= TDBTable.Create(LeftPanel, ASQLite3);
    LeftDBTable.OnSelect:= @LeftTableSelect;
    LeftDBTable.Edit.HeaderFont.Style:= LeftDBTable.Edit.HeaderFont.Style + [fsBold];
  end;

  LeftTotalWidth:= VSum(AColumnWidths) + 10;
  LeftDBTable.Settings(AFont, ATableName, AIDFieldName, AFieldNames,
      AColumnNames, AColumnTypes, AColumnNeedValues, AColumnWidths, AColumnAlignments,
      AIDNotZero, AHeaderVisible, AOrderFieldNames, AAutoSizeColumnNumber,
      AKeys, APicks);
end;

procedure TSQLite3DoubleTable.SetRightTable(const ASQLite3: TSQLite3;
                       const AFont: TFont;
                       const ATableName, AIDFieldName: String;
                       const AFieldNames, AColumnNames: TStrVector;
                       const AColumnTypes: TVSTColumnTypes;
                       const AColumnNeedValues: TBoolVector;
                       const AColumnWidths: TIntVector;
                       const AColumnAlignments: array of TAlignment;
                       const AIDNotZero, AHeaderVisible: Boolean;
                       const AOrderFieldNames: TStrVector = nil;
                       const AAutoSizeColumnNumber: Integer = 1;
                       const AKeys: TIntMatrix = nil;
                       const APicks: TStrMatrix = nil;
                       const AMasterIDFieldName: String = '');
begin
  if not Assigned(RightDBTable) then
  begin
    RightDBTable:= TDBTable.Create(RightPanel, ASQLite3);
    RightDBTable.OnSelect:= @RightTableSelect;
    RightDBTable.Edit.HeaderFont.Style:= RightDBTable.Edit.HeaderFont.Style + [fsBold];
  end;

  RightTotalWidth:= VSum(AColumnWidths) + 10;
  RightDBTable.Settings(AFont, ATableName, AIDFieldName, AFieldNames,
      AColumnNames, AColumnTypes, AColumnNeedValues, AColumnWidths, AColumnAlignments,
      AIDNotZero, AHeaderVisible, AOrderFieldNames, AAutoSizeColumnNumber,
      AKeys, APicks, AMasterIDFieldName);
end;

end.

