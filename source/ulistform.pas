unit UListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, rxdbgrid, Forms, Controls, Graphics,
  Dialogs, DbCtrls, Buttons, Grids, DBGrids, DBUtils, SheetUtils;

type

  { TListForm }

  TListForm = class(TForm)
    ColorDialog1: TColorDialog;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    ImageList1: TImageList;
    RxDBGrid1: TRxDBGrid;
    ListQuery: TSQLQuery;
    CloseButton: TSpeedButton;
    ColorButton: TSpeedButton;
    procedure ColorButtonClick(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; {%H-}Field: TField);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListQueryAfterDelete(DataSet: TDataSet);
    procedure ListQueryAfterPost(DataSet: TDataSet);
    procedure CloseButtonClick(Sender: TObject);
    procedure ListQueryBeforePost(DataSet: TDataSet);
    procedure RxDBGrid1Columns0DrawColumnCell(Sender: TObject;
      const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
  private
    { private declarations }
    TableName, IDField, NameField, ColorField : String;
    procedure SetGridColumnWidth;
    procedure SetListColor;
    procedure ChangeDBNavButton(DBNav: TDbNavigator;
                            const DBBtnType: TDBNavButtonType;
                            const DBBtnGlyph: TBitmap;
                            const DBBtnCursor: TCursor = crDefault);
    procedure ChangeDBNavigatorGlyphs(DBNav: TDbNavigator);
  public
    { public declarations }
  end;

var
  ListForm: TListForm;

  function ListFormOpen(const ATableName, AIDField, ANameField: String;
                        const AOrderByName: Boolean = False;
                        const AColorField: String = ''): Boolean;

implementation



{$R *.lfm}




function ListFormOpen(const ATableName, AIDField, ANameField: String;
                      const AOrderByName: Boolean = False;
                      const AColorField: String = ''): Boolean;
var
  LF: TListForm;
begin
  Result:= False;
  LF:= TListForm.Create(nil);
  LF.TableName:= ATableName;
  LF.IDField:= AIDField;
  LF.NameField:= ANameField;
  LF.ColorField:= AColorField;
  LF.ColorButton.Visible:= LF.ColorField<>'';
  LF.RxDBGrid1.Columns.Items[0].FieldName:= ANameField;
  LF.ListQuery.SQL.Clear;
  LF.ListQuery.SQL.Add('SELECT * FROM ' + ATableName);
  LF.ListQuery.SQL.Add('WHERE ' + AIDField + ' > 0');
  if AOrderByName then
    LF.ListQuery.SQL.Add('ORDER BY ' + ANameField);
  try
    LF.ShowModal;
  finally
    FreeAndNil(LF);
  end;
  Result:= True;
end;

procedure TListForm.ChangeDBNavButton(DBNav: TDbNavigator;
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

procedure TListForm.ChangeDBNavigatorGlyphs(DBNav: TDbNavigator);
var
  BM: TBitmap;
begin
  BM:= TBitmap.Create;
  try
    ImageList1.GetBitmap(0, BM);
    ChangeDBNavButton(DBNav, nbInsert, BM, crHandPoint);
    ImageList1.GetBitmap(1, BM);
    ChangeDBNavButton(DBNav, nbDelete, BM, crHandPoint);
    ImageList1.GetBitmap(2, BM);
    ChangeDBNavButton(DBNav, nbEdit, BM, crHandPoint);
    ImageList1.GetBitmap(3, BM);
    ChangeDBNavButton(DBNav, nbPost, BM, crHandPoint);
    ImageList1.GetBitmap(4, BM);
    ChangeDBNavButton(DBNav, nbCancel, BM, crHandPoint);
  finally
    FreeAndNil(BM);
  end;
end;

procedure TListForm.FormResize(Sender: TObject);
begin
  SetGridColumnWidth;
end;

procedure TListForm.FormShow(Sender: TObject);
begin
  ListQuery.Open;
  ChangeDBNavigatorGlyphs(DBNavigator1);
end;

procedure TListForm.FormChangeBounds(Sender: TObject);
begin
  SetGridColumnWidth;
end;

procedure TListForm.FormCreate(Sender: TObject);
begin
  RxDBGrid1.SelectedColor:= COLOR_BACKGROUND_SELECTED;
  RxDBGrid1.SelectedFont.Color:= clWindowText;
end;

procedure TListForm.ColorButtonClick(Sender: TObject);
begin
  SetListColor;
end;

procedure TListForm.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  if ColorField='' then Exit;
  ColorButton.Enabled:= not ListQuery.IsEmpty;
end;

procedure TListForm.ListQueryAfterDelete(DataSet: TDataSet);
begin
  DataSetChangesSave(DataSet);
end;

procedure TListForm.ListQueryAfterPost(DataSet: TDataSet);
begin
  DataSetChangesSave(DataSet);
end;

procedure TListForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TListForm.ListQueryBeforePost(DataSet: TDataSet);
begin
  if ColorField='' then Exit;
  ListQuery.FieldByName(ColorField).AsInteger:= 16777215;
end;

procedure TListForm.RxDBGrid1Columns0DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  ColorValue: Integer;
  Grid: TRxDBGrid;
begin
  if ListQuery.IsEmpty then Exit;
  Grid:= Sender AS TRxDBGrid;
  if ColorField='' then
    Grid.DefaultDrawColumnCell(Rect, DataCol, Column, State)
  else begin
    ColorValue:= ListQuery.FieldByName(ColorField).AsInteger;
    if ColorValue= 0 then
      ColorValue:= 16777215;
    Grid.Canvas.Font.Assign(Grid.Font);
    Grid.Canvas.Brush.Color:= ColorValue;
    Grid.Canvas.FillRect(Rect.Left, Rect.Top, Rect.Right-1, Rect.Bottom-1);
    Grid.Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, Column.Field.AsString);
  end;
end;

procedure TListForm.SetGridColumnWidth;
begin
  RxDBGrid1.Columns.Items[0].Width:= RxDBGrid1.Width-30;
end;

procedure TListForm.SetListColor;
var
  NewValue, IDValue: Integer;
begin
  if not ColorDialog1.Execute then Exit;
  NewValue:= ColorToRGB(ColorDialog1.Color);
  IDValue:= ListQuery.FieldByName(IDField).AsInteger;
  UpdateWithID(TableName, IDField, ColorField, IDValue, NewValue);

  ListQuery.Refresh;


end;

end.

