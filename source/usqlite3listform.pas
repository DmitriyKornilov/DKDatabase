unit USQLite3ListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, rxdbgrid, Forms, Controls, Graphics,
  Dialogs, DbCtrls, Buttons, Grids, DBGrids, ExtCtrls, DK_SQLUtils;

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
    ListQuery: TSQLQuery;
    procedure ColorButtonClick(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; {%H-}Field: TField);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListQueryAfterDelete(DataSet: TDataSet);
    procedure ListQueryAfterPost(DataSet: TDataSet);
    procedure ListQueryBeforePost({%H-}DataSet: TDataSet);
    procedure RxDBGrid1Columns0DrawColumnCell(Sender: TObject;
      const Rect: TRect; {%H-}DataCol: Integer; Column: TColumn;
      {%H-}State: TGridDrawState);
  private
    { private declarations }
    TableName, IDField, NameField, ColorField : String;
    SelectedColor: TColor;
    SelectedFontColor: TColor;

    procedure SetGridColumnWidth;
    procedure SetListColor;
    procedure ChangeDBNavButton(DBNav: TDbNavigator;
                            const DBBtnType: TDBNavButtonType;
                            const DBBtnGlyph: TBitmap;
                            const DBBtnCursor: TCursor = crDefault);
    procedure ChangeDBNavigatorGlyphs(DBNav: TDbNavigator);
  public
    { public declarations }
    procedure SetNames(const ATableName, AIDFieldName,
                             AFieldName, AColorFieldName: String);
    procedure SetSettings(const ASelectedColor, ASelectedFontColor: TColor);

  end;

var
  SQLite3ListForm: TSQLite3ListForm;
  ImageList: TImageList;


implementation

{$R *.lfm}

procedure TSQLite3ListForm.ChangeDBNavButton(DBNav: TDbNavigator;
                            const DBBtnType: TDBNavButtonType;
                            const DBBtnGlyph: TBitmap;
                            const DBBtnCursor: TCursor = crDefault);
var
  i: Integer;
  NB: TDBNavButton;
begin
  for i := 0 to DBNav.ControlCount - 1 do
  begin
    if DBNav.Controls[i].ClassName = 'TDBNavButton' then
    begin
      NB:= (DBNav.Controls[i] As TDBNavButton);
      if NB.Index= DBBtnType then
      begin
        NB.Glyph := DBBtnGlyph;
        NB.Cursor:= DBBtnCursor;
      end;
    end;
  end;
end;

procedure TSQLite3ListForm.ChangeDBNavigatorGlyphs(DBNav: TDbNavigator);
var
  BM: TBitmap;
begin
  if not Assigned(ImageList) then Exit;
  if ImageList.Count<5 then Exit;
  BM:= TBitmap.Create;
  try
    ImageList.GetBitmap(0, BM);
    ChangeDBNavButton(DBNav, nbInsert, BM, crHandPoint);
    ImageList.GetBitmap(1, BM);
    ChangeDBNavButton(DBNav, nbDelete, BM, crHandPoint);
    ImageList.GetBitmap(2, BM);
    ChangeDBNavButton(DBNav, nbEdit, BM, crHandPoint);
    ImageList.GetBitmap(3, BM);
    ChangeDBNavButton(DBNav, nbPost, BM, crHandPoint);
    ImageList.GetBitmap(4, BM);
    ChangeDBNavButton(DBNav, nbCancel, BM, crHandPoint);
  finally
    FreeAndNil(BM);
  end;
end;

procedure TSQLite3ListForm.SetNames(const ATableName, AIDFieldName, AFieldName,
  AColorFieldName: String);
begin
  TableName:= ATableName;
  IDField:= AIDFieldName;
  NameField:= AFieldName;
  ColorField:= AColorFieldName;
end;

procedure TSQLite3ListForm.SetSettings(const ASelectedColor,
  ASelectedFontColor: TColor);
begin
  SelectedColor:= ASelectedColor;
  SelectedFontColor:=  ASelectedFontColor;
end;

procedure TSQLite3ListForm.FormResize(Sender: TObject);
begin
  SetGridColumnWidth;
end;

procedure TSQLite3ListForm.FormShow(Sender: TObject);
begin
  ChangeDBNavigatorGlyphs(DBNavigator1);
  ColorButton.Visible:= ColorField<>EmptyStr;
  RxDBGrid1.SelectedColor:= SelectedColor;
  RxDBGrid1.SelectedFont.Color:= SelectedFontColor;
  RxDBGrid1.Columns.Items[0].FieldName:= NameField;
  ListQuery.Open;
end;

procedure TSQLite3ListForm.FormChangeBounds(Sender: TObject);
begin
  SetGridColumnWidth;
end;

procedure TSQLite3ListForm.FormCreate(Sender: TObject);
begin
  SelectedColor:= clHighlight;
  SelectedFontColor:=  clWindowText;
end;

procedure TSQLite3ListForm.ColorButtonClick(Sender: TObject);
begin
  SetListColor;
end;

procedure TSQLite3ListForm.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  if ColorField='' then Exit;
  ColorButton.Enabled:= not ListQuery.IsEmpty;
end;

procedure DataSetChangesSave(const ADataSet: TDataSet);
begin
  (ADataSet As TSQLQuery).ApplyUpdates;
  (ADataSet As TSQLQuery).SQLTransaction.CommitRetaining;
end;

procedure TSQLite3ListForm.ListQueryAfterDelete(DataSet: TDataSet);
begin
  DataSetChangesSave(DataSet);
end;

procedure TSQLite3ListForm.ListQueryAfterPost(DataSet: TDataSet);
begin
  DataSetChangesSave(DataSet);
end;

procedure TSQLite3ListForm.ListQueryBeforePost(DataSet: TDataSet);
begin
  if ColorField='' then Exit;
  ListQuery.FieldByName(ColorField).AsInteger:= 16777215;
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
    if not ListQuery.IsEmpty then
      ColorValue:= ListQuery.FieldByName(ColorField).AsInteger;
    if ColorValue= 0 then
      ColorValue:= clWindow;
    Grid.Canvas.Brush.Color:= ColorValue;
  end;
  Grid.Canvas.Pen.Color:= clWindowText;
  y:= Rect.Top-1;
  if y<0 then y:= 0;
  Grid.Canvas.Rectangle(Rect.Left, y, Rect.Right, Rect.Bottom);
  if not ListQuery.IsEmpty then
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
  IDValue:= ListQuery.FieldByName(IDField).AsInteger;

  try
    QSetQuery(WriteQuery);
    QSetSQL(
      'UPDATE' + SqlEsc(TableName) +
      'SET'    + SqlEsc(ColorField)   + '= :NewValue ' +
      'WHERE'  + SqlEsc(IDField) + '= :IDValue'
      );
    QParamInt('IDValue', IDValue);
    QParamInt('NewValue', NewValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;

  ListQuery.Refresh;
end;

end.

