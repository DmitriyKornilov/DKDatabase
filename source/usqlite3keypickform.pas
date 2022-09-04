unit USQLite3KeyPickForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, DK_VSTTables, DK_Vector, SQLDB, DividerBevel;

type

  { TSQLite3KeyPickForm }

  TSQLite3KeyPickForm = class(TForm)
    ButtonPanel: TPanel;
    CancelButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    ImageList1: TImageList;
    ListCheckButton: TSpeedButton;
    ListQuery: TSQLQuery;
    ListUncheckButton: TSpeedButton;
    Panel1: TPanel;
    SaveButton: TSpeedButton;
    VT1: TVirtualStringTree;
    procedure CancelButtonClick(Sender: TObject);
    procedure ListCheckButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure ListUncheckButtonClick(Sender: TObject);
  private
    VST: TVSTCheckTable;


  public
    KeyValues, OutKeyValues: TIntVector;
    PickValues, OutPickValues: TStrVector;
    IsAllChecked: Boolean;
  end;

var
  SQLite3KeyPickForm: TSQLite3KeyPickForm;

implementation

{$R *.lfm}

{ TSQLite3KeyPickForm }

procedure TSQLite3KeyPickForm.FormCreate(Sender: TObject);
begin
  VST:= TVSTCheckTable.Create(VT1);
  VST.SelectedBGColor:= VT1.Color;
  VST.HeaderFont.Style:= [fsBold];
end;

procedure TSQLite3KeyPickForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSQLite3KeyPickForm.ListCheckButtonClick(Sender: TObject);
begin
  VST.CheckAll(True);
end;

procedure TSQLite3KeyPickForm.ListUncheckButtonClick(Sender: TObject);
begin
  VST.CheckAll(False);
end;

procedure TSQLite3KeyPickForm.FormDestroy(Sender: TObject);
begin
  if Assigned(VST) then FreeAndNil(VST);
end;

procedure TSQLite3KeyPickForm.FormShow(Sender: TObject);
var
  i, n: Integer;
begin
  VST.AddColumn(Caption, 150);
  VST.SetColumn(Caption, PickValues, taLeftJustify);
  VST.Draw;
  for i:= 0 to High(OutKeyValues) do
  begin
    n:= VIndexOf(KeyValues, OutKeyValues[i]);
    if n>=0 then VST.Checked[n]:= True;
  end;
  ListCheckButton.Enabled:= not VIsNil(KeyValues);
  ListUncheckButton.Enabled:= ListCheckButton.Enabled;
end;

procedure TSQLite3KeyPickForm.SaveButtonClick(Sender: TObject);
var
  i: Integer;
begin
  OutKeyValues:= nil;
  OutPickValues:= nil;

  if VST.IsAllUnchecked then
    VST.CheckAll(True);

  if not VST.IsAllChecked then
  begin
    for i:= 0 to High(KeyValues) do
    begin
      if VST.Checked[i] then
      begin
        VAppend(OutKeyValues, KeyValues[i]);
        VAppend(OutPickValues, PickValues[i]);
      end;
    end;
  end;

  IsAllChecked:= VST.IsAllChecked;

  ModalResult:= mrOK;
end;



end.

