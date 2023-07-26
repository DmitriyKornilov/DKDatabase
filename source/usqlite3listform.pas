unit USQLite3ListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, rxdbgrid, Forms, Controls, Graphics,
  Dialogs, DbCtrls, Buttons, Grids, DBGrids, ExtCtrls, DK_SQLUtils, DK_DBUtils;

type

  { TSQLite3ListForm }

  TSQLite3ListForm = class(TForm)
    ColorButton: TSpeedButton;
    ColorDialog1: TColorDialog;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    Panel1: TPanel;
    Panel2: TPanel;
    RxDBGrid1: TRxDBGrid;
    WriteQuery: TSQLQuery;
    ReadQuery: TSQLQuery;
    procedure ColorButtonClick(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; {%H-}Field: TField);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReadQueryAfterDelete(DataSet: TDataSet);
    procedure ReadQueryAfterPost(DataSet: TDataSet);
    procedure ReadQueryBeforePost({%H-}DataSet: TDataSet);
    procedure RxDBGrid1Columns0DrawColumnCell(Sender: TObject;
      const Rect: TRect; {%H-}DataCol: Integer; Column: TColumn;
      {%H-}State: TGridDrawState);
  private
    { private declarations }
    TableName, IDFieldName, ValueFieldName, ColorField : String;

    procedure SetGridColumnWidth;
    procedure SetListColor;

  public
    { public declarations }
    procedure SetTable(const ATableName, AIDFieldName,
                             AValueFieldName, AColorFieldName: String;
                       const AIDNotZero, AOrderByName: Boolean);
    procedure SetColors(const ASelectedColor, ASelectedFontColor: TColor);
    procedure SetNavigatorGlyphs(const AImageList: TImageList);


  end;

var
  SQLite3ListForm: TSQLite3ListForm;

implementation

{$R *.lfm}

procedure TSQLite3ListForm.SetTable(const ATableName, AIDFieldName,
                                          AValueFieldName, AColorFieldName: String;
                                    const AIDNotZero, AOrderByName: Boolean);
var
  S: String;
begin
  TableName:= ATableName;
  IDFieldName:= AIDFieldName;
  ValueFieldName:= AValueFieldName;
  ColorField:= AColorFieldName;

  RxDBGrid1.Columns.Items[0].FieldName:= ValueFieldName;

  S:= 'SELECT * FROM' + SqlEsc(TableName);
  if AIDNotZero then
    S:= S + ' WHERE' + SqlEsc(IDFieldName) + '> 0';
  if AOrderByName then
    S:= S + ' ORDER BY ' + SqlEsc(ValueFieldName);
  QSetQuery(ReadQuery);
  QSetSQL(S);
end;

procedure TSQLite3ListForm.SetColors(const ASelectedColor, ASelectedFontColor: TColor);
begin
  RxDBGrid1.SelectedColor:= ASelectedColor;
  RxDBGrid1.SelectedFont.Color:= ASelectedFontColor;
end;

procedure TSQLite3ListForm.SetNavigatorGlyphs(const AImageList: TImageList);
begin
  if Assigned(AImageList) then
    ChangeDBNavigatorGlyphs(DBNavigator1, AImageList);
end;

procedure TSQLite3ListForm.FormResize(Sender: TObject);
begin
  SetGridColumnWidth;
end;

procedure TSQLite3ListForm.FormShow(Sender: TObject);
begin
  ColorButton.Visible:= ColorField<>EmptyStr;
  if ReadQuery.SQL.Text<>'' then
    ReadQuery.Open;
end;

procedure TSQLite3ListForm.FormChangeBounds(Sender: TObject);
begin
  SetGridColumnWidth;
end;

procedure TSQLite3ListForm.ColorButtonClick(Sender: TObject);
begin
  SetListColor;
end;

procedure TSQLite3ListForm.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  if ColorField='' then Exit;
  ColorButton.Enabled:= not ReadQuery.IsEmpty;
end;

procedure TSQLite3ListForm.ReadQueryAfterDelete(DataSet: TDataSet);
begin
  DataSetChangesSave(DataSet);
end;

procedure TSQLite3ListForm.ReadQueryAfterPost(DataSet: TDataSet);
begin
  DataSetChangesSave(DataSet);
end;

procedure TSQLite3ListForm.ReadQueryBeforePost(DataSet: TDataSet);
begin
  if ColorField='' then Exit;
  ReadQuery.FieldByName(ColorField).AsInteger:= 16777215;
end;

procedure TSQLite3ListForm.RxDBGrid1Columns0DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  ColorValue: Integer;
  Grid: TRxDBGrid;
  y: Integer;
begin
  Grid:= Sender AS TRxDBGrid;
  Grid.Canvas.Font.Assign(Grid.Font);
  if ColorField<>EmptyStr then
  begin
    ColorValue:= 0;
    if not ReadQuery.IsEmpty then
      ColorValue:= ReadQuery.FieldByName(ColorField).AsInteger;
    if ColorValue= 0 then
      ColorValue:= clWindow;
    Grid.Canvas.Brush.Color:= ColorValue;
  end;
  Grid.Canvas.Pen.Color:= clWindowText;
  y:= Rect.Top-1;
  if y<0 then y:= 0;
  Grid.Canvas.Rectangle(Rect.Left, y, Rect.Right, Rect.Bottom);
  if not ReadQuery.IsEmpty then
    Grid.Canvas.TextOut(Rect.Left + 3, Rect.Top + 2, Column.Field.AsString);
end;

procedure TSQLite3ListForm.SetGridColumnWidth;
begin
  RxDBGrid1.Columns.Items[0].Width:= RxDBGrid1.Width-30;
end;

procedure TSQLite3ListForm.SetListColor;
var
  NewValue, IDValue: Integer;
begin
  if not ColorDialog1.Execute then Exit;
  NewValue:= ColorToRGB(ColorDialog1.Color);
  IDValue:= ReadQuery.FieldByName(IDFieldName).AsInteger;

  try
    QSetQuery(WriteQuery);
    QSetSQL(
      'UPDATE' + SqlEsc(TableName) +
      'SET'    + SqlEsc(ColorField)   + '= :NewValue ' +
      'WHERE'  + SqlEsc(IDFieldName) + '= :IDValue'
      );
    QParamInt('IDValue', IDValue);
    QParamInt('NewValue', NewValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;

  ReadQuery.Refresh;
end;



end.

