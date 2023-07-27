unit USQLite3TableForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  RxDBGrid, DbCtrls, DK_DBUtils, DK_Vector, DK_SQLUtils, Grids, DBGrids;

type

  { TSQLite3TableForm }

  TSQLite3TableForm = class(TForm)
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    Panel1: TPanel;
    Panel2: TPanel;
    ReadQuery: TSQLQuery;
    RxDBGrid1: TRxDBGrid;
    WriteQuery: TSQLQuery;
    procedure FormShow(Sender: TObject);
    procedure ReadQueryAfterDelete(DataSet: TDataSet);
    procedure ReadQueryAfterPost(DataSet: TDataSet);
    procedure RxDBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      {%H-}DataCol: Integer; Column: TColumn; {%H-}State: TGridDrawState);
  private
    TableName, IDFieldName: String;
    FieldNames, ColumnNames: TStrVector;

  public
    procedure SetColors(const ASelectedColor, ASelectedFontColor: TColor);
    procedure SetNavigatorGlyphs(const AImageList: TImageList);
    procedure SetTable(const ATableName, AIDFieldName: String;
                         const AFieldNames, AColumnNames: TStrVector;
                         const AColumnWidths: TIntVector;
                         const AIDNotZero: Boolean;
                         const AOrderFieldNames: TStrVector = nil);

  end;

var
  SQLite3TableForm: TSQLite3TableForm;

implementation

{$R *.lfm}

{ TSQLite3TableForm }

procedure TSQLite3TableForm.FormShow(Sender: TObject);
begin
  if ReadQuery.SQL.Text<>'' then
    ReadQuery.Open;
end;

procedure TSQLite3TableForm.ReadQueryAfterDelete(DataSet: TDataSet);
begin
  DataSetChangesSave(DataSet);
end;

procedure TSQLite3TableForm.ReadQueryAfterPost(DataSet: TDataSet);
begin
  DataSetChangesSave(DataSet);
end;

procedure TSQLite3TableForm.RxDBGrid1DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  Grid: TRxDBGrid;
  y, x: Integer;
begin
  Grid:= Sender AS TRxDBGrid;
  Grid.Canvas.Font.Assign(Grid.Font);
  Grid.Canvas.Pen.Color:= clWindowText;
  y:= Rect.Top-1;
  if y<0 then y:= 0;
  x:= Rect.Left-1;
  if x<0 then x:=0;
  Grid.Canvas.Rectangle(x, y, Rect.Right, Rect.Bottom);
  if not ReadQuery.IsEmpty then
    Grid.Canvas.TextOut(Rect.Left + 3, Rect.Top + 2, Column.Field.AsString);
end;

procedure TSQLite3TableForm.SetColors(const ASelectedColor, ASelectedFontColor: TColor);
begin
  RxDBGrid1.SelectedColor:= ASelectedColor;
  RxDBGrid1.SelectedFont.Color:= ASelectedFontColor;
end;

procedure TSQLite3TableForm.SetNavigatorGlyphs(const AImageList: TImageList);
begin
  if Assigned(AImageList) then
    ChangeDBNavigatorGlyphs(DBNavigator1, AImageList);
end;

procedure TSQLite3TableForm.SetTable(const ATableName, AIDFieldName: String;
                         const AFieldNames, AColumnNames: TStrVector;
                         const AColumnWidths: TIntVector;
                         const AIDNotZero: Boolean;
                         const AOrderFieldNames: TStrVector = nil);
var
  i: Integer;
  Col: TRxColumn;
  S: String;
begin
  TableName:= ATableName;
  IDFieldName:= AIDFieldName;
  FieldNames:= AFieldNames;
  ColumnNames:= AColumnNames;


  i:= VSum(AColumnWidths) + 50;
  Width:= i;

  for i:= 0 to High(AFieldNames) do
  begin
    Col:= RxDBGrid1.Columns.Add;
    Col.FieldName:= AFieldNames[i];
    Col.Title.Caption:= AColumnNames[i];
    Col.Width:= AColumnWidths[i];
  end;

  S:= 'SELECT * FROM' + SqlEsc(ATableName);
  if AIDNotZero then
    S:= S + 'WHERE' + SqlEsc(AIDFieldName) + '> 0 ';
  if not VIsNil(AOrderFieldNames) then
    S:= S + 'ORDER BY' + SqlFieldsEnum(AOrderFieldNames);
  QSetQuery(ReadQuery);
  QSetSQL(S);
end;



end.

