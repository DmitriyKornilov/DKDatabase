unit USQLite3Table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, RxTimeEdit, VirtualTrees,

  DK_VSTTables, DK_Vector, DK_Matrix, DK_SQLUtils;

type

  { TSQLite3Table }

  TSQLite3Table = class(TForm)
    RxTimeEdit1: TRxTimeEdit;
    UpdateButton: TSpeedButton;
    DelButton: TSpeedButton;
    AddButton: TSpeedButton;
    EditButton: TSpeedButton;
    SaveButton: TSpeedButton;
    ReadQuery: TSQLQuery;
    CancelButton: TSpeedButton;
    ToolPanel: TPanel;
    VT1: TVirtualStringTree;
    WriteQuery: TSQLQuery;
    procedure CancelButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    VSTEdit: TVSTEdit;
    TableName, IDFieldName, ReadSQL: String;
    FieldNames, ColumnNames: TStrVector;
    ColumnWidths: TIntVector;
    ColumnTypes: TVSTColumnTypes;
    DataValues: TStrMatrix;
    procedure TableUpdate;

    procedure SelectCell;
    procedure BeginEdit;
  public
    procedure SetTable(const ATableName, AIDFieldName: String;
                       const AFieldNames, AColumnNames: TStrVector;
                       const AColumnTypes: TVSTColumnTypes;
                       const AColumnWidths: TIntVector;
                       const AIDNotZero, AHeaderVisible: Boolean;
                       const AOrderFieldNames: TStrVector = nil);
  end;

var
  SQLite3Table: TSQLite3Table;

implementation

{$R *.lfm}

{ TSQLite3Table }

procedure TSQLite3Table.FormCreate(Sender: TObject);
begin
  VSTEdit:= TVSTEdit.Create(VT1);
  VSTEdit.OnSelect:= @SelectCell;
  VSTEdit.OnEdititingBegin:= @BeginEdit;

  DelButton.Enabled:= False;
  EditButton.Enabled:= False;
  SaveButton.Enabled:= False;
  CancelButton.Enabled:= False;
end;

procedure TSQLite3Table.CancelButtonClick(Sender: TObject);
begin
  VSTEdit.UnSelect(False);
  CancelButton.Enabled:= False;
  SaveButton.Enabled:= False;
end;

procedure TSQLite3Table.EditButtonClick(Sender: TObject);
begin
  if VSTEdit.IsSelected then
    VSTEdit.Select(VSTEdit.SelectedRowIndex, VSTEdit.SelectedColIndex);
end;

procedure TSQLite3Table.FormDestroy(Sender: TObject);
begin
  FreeAndNil(VSTEdit);
end;

procedure TSQLite3Table.TableUpdate;
var
  i: Integer;
  V: TStrVector;
begin
  DataValues:= nil;
  MDim(DataValues, Length(FieldNames));

  QSetQuery(ReadQuery);
  QSetSQL(ReadSQL);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      for i:= 0 to High(FieldNames) do
      begin
        case ColumnTypes[i] of
          ctInteger: VAppend(DataValues[i], IntToStr(QFieldInt(FieldNames[i])));
          ctString:  VAppend(DataValues[i], QFieldStr(FieldNames[i]));
          ctDate:    VAppend(DataValues[i], DateToStr(QFieldDT(FieldNames[i])));
          ctTime:    VAppend(DataValues[i], TimeToStr(QFieldDT(FieldNames[i])));
          //ctFloat
        end;
      end;
      QNext;
    end;
  end;
  QClose;

  VSTEdit.Clear;


  VSTEdit.AddColumnRowTitles('', 1);  //!!!
  for i:= 0 to High(FieldNames) do
  begin
    case ColumnTypes[i] of
      ctInteger: VSTEdit.AddColumnInteger(ColumnNames[i], ColumnWidths[i], taCenter, taCenter);
      ctString:  VSTEdit.AddColumnString(ColumnNames[i], ColumnWidths[i], taCenter, taLeftJustify);
      ctDate:    VSTEdit.AddColumnDate(ColumnNames[i], 'dd.mm.yyyy', ColumnWidths[i], taCenter, taCenter);
      ctTime:    VSTEdit.AddColumnTime(ColumnNames[i], '00:00:00', ColumnWidths[i], taCenter, taCenter);
      //ctFloat
    end;
  end;
  VT1.Header.Columns[0].MinWidth:= 0; //!!!!

  if (not MIsNil(DataValues)) and (not VIsNil(DataValues[0])) then
  begin
    VSTEdit.SetColumnRowTitles(VIntToStr(VOrder(Length(DataValues[0]))), taLeftJustify);
    for i:= 0 to High(FieldNames) do
      VSTEdit.SetColumnString(ColumnNames[i], DataValues[i]);
  end;

  VSTEdit.Draw;

end;

procedure TSQLite3Table.SelectCell;
begin
  DelButton.Enabled:= VSTEdit.IsSelected;
  EditButton.Enabled:= VSTEdit.IsSelected and (not SaveButton.Enabled);
end;

procedure TSQLite3Table.BeginEdit;
begin
  //EditButton.Enabled:= False;
  SaveButton.Enabled:= True;
  CancelButton.Enabled:= True;
end;

procedure TSQLite3Table.SetTable(const ATableName, AIDFieldName: String;
                       const AFieldNames, AColumnNames: TStrVector;
                       const AColumnTypes: TVSTColumnTypes;
                       const AColumnWidths: TIntVector;
                       const AIDNotZero, AHeaderVisible: Boolean;
                       const AOrderFieldNames: TStrVector = nil);
var
  i: Integer;
begin
  TableName:= ATableName;
  IDFieldName:= AIDFieldName;
  FieldNames:= AFieldNames;
  ColumnTypes:= AColumnTypes;
  ColumnWidths:= AColumnWidths;
  if not VIsNil(AColumnNames) then
    ColumnNames:= AColumnNames
  else begin
    VDim(ColumnNames, Length(AFieldNames));
    for i:= 0 to High(ColumnNames) do
      ColumnNames[i]:= 'Column' + IntToStr(i+1);
  end;

  ReadSQL:= 'SELECT ' +  SqlFieldsEnum(AFieldNames)  + ' FROM' + SqlEsc(ATableName);
  if AIDNotZero then
    ReadSQL:= ReadSQL + 'WHERE' + SqlEsc(AIDFieldName) + ' > 0 ';
  if not VIsNil(AOrderFieldNames) then
    ReadSQL:= ReadSQL + 'ORDER BY' + SqlFieldsEnum(AOrderFieldNames);

  VSTEdit.HeaderVisible:= AHeaderVisible;


 // VSTEdit.Draw;
  //VSTEdit.HeaderBGColor:= clBtnFace;
  //VSTEdit.AutosizeColumnDisable;

  TableUpdate;
end;

end.

