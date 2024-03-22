unit USQLite3Table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Forms, Graphics, ExtCtrls,


  DK_VSTTables, DK_Vector, DK_Matrix, DK_DBTable, DK_PPI, DK_CtrlUtils;

type

  { TSQLite3Table }

  TSQLite3Table = class(TForm)
    MainPanel: TPanel;
    Query: TSQLQuery;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    DBTable: TDBTable;
    TotalWidth: Integer;
  public
    procedure SetTable(const AFont: TFont;
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
  end;

var
  SQLite3Table: TSQLite3Table;

implementation

{$R *.lfm}

{ TSQLite3Table }

procedure TSQLite3Table.FormCreate(Sender: TObject);
begin
  DBTable:= TDBTable.Create(MainPanel, Query);
end;

procedure TSQLite3Table.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DBTable);
end;

procedure TSQLite3Table.FormShow(Sender: TObject);
begin
  if TotalWidth<Constraints.MinWidth then
    TotalWidth:= Constraints.MinWidth;
  TotalWidth:= WidthFromDefaultToScreen(TotalWidth);
  if TotalWidth>Screen.Width then
    TotalWidth:= Screen.Width - 20;
  Width:= TotalWidth;
  FormToScreenCenter(Sender as TForm);
end;

procedure TSQLite3Table.SetTable(const AFont: TFont;
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
  TotalWidth:= VSum(AColumnWidths) + 10;
  DBTable.Settings(AFont, ATableName, AIDFieldName, AFieldNames,
      AColumnNames, AColumnTypes, AColumnNeedValues, AColumnWidths, AColumnAlignments,
      AIDNotZero, AHeaderVisible, AOrderFieldNames, AAutoSizeColumnNumber,
      AKeys, APicks);
  DBTable.Edit.HeaderFont.Style:= DBTable.Edit.HeaderFont.Style + [fsBold];
end;

end.

