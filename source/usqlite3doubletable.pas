unit USQLite3DoubleTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Forms, Graphics, ExtCtrls,


  DK_VSTTypes, DK_Vector, DK_Matrix, DK_DBTable, DK_PPI, DK_CtrlUtils;

type

  { TSQLite3DoubleTable }

  TSQLite3DoubleTable = class(TForm)
    LeftPanel: TPanel;
    LeftQuery: TSQLQuery;
    RightQuery: TSQLQuery;
    RightPanel: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
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
    procedure SetLeftTable(const AFont: TFont;
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
    procedure SetRightTable(const AFont: TFont;
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

procedure TSQLite3DoubleTable.FormCreate(Sender: TObject);
begin
  LeftDBTable:= TDBTable.Create(LeftPanel, LeftQuery);
  LeftDBTable.OnSelect:= @LeftTableSelect;

  RightDBTable:= TDBTable.Create(RightPanel, RightQuery);
  RightDBTable.OnSelect:= @RightTableSelect;
end;

procedure TSQLite3DoubleTable.FormDestroy(Sender: TObject);
begin
  FreeAndNil(RightDBTable);
  FreeAndNil(LeftDBTable);
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
  RightDBTable.Update(LeftDBTable.IDValue);
end;

procedure TSQLite3DoubleTable.RightTableSelect;
begin
  if LeftDBTable.Edit.IsEditing then
    LeftDBTable.EditingCancel;
end;

procedure TSQLite3DoubleTable.SetLeftTable(const AFont: TFont;
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
  LeftTotalWidth:= VSum(AColumnWidths) + 10;
  LeftDBTable.Settings(AFont, ATableName, AIDFieldName, AFieldNames,
      AColumnNames, AColumnTypes, AColumnNeedValues, AColumnWidths, AColumnAlignments,
      AIDNotZero, AHeaderVisible, AOrderFieldNames, AAutoSizeColumnNumber,
      AKeys, APicks);
  LeftDBTable.Edit.HeaderFont.Style:= LeftDBTable.Edit.HeaderFont.Style + [fsBold];
end;

procedure TSQLite3DoubleTable.SetRightTable(const AFont: TFont;
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
  RightTotalWidth:= VSum(AColumnWidths) + 10;
  RightDBTable.Settings(AFont, ATableName, AIDFieldName, AFieldNames,
      AColumnNames, AColumnTypes, AColumnNeedValues, AColumnWidths, AColumnAlignments,
      AIDNotZero, AHeaderVisible, AOrderFieldNames, AAutoSizeColumnNumber,
      AKeys, APicks, AMasterIDFieldName);
  RightDBTable.Edit.HeaderFont.Style:= RightDBTable.Edit.HeaderFont.Style + [fsBold];
end;

end.

