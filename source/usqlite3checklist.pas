unit USQLite3CheckList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, SQLDB,
  DK_VSTTables, DK_Vector, DK_CtrlUtils,
  UDBImages;

type

  { TSQLite3CheckList }

  TSQLite3CheckList = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    AllCheckButton: TSpeedButton;
    ListQuery: TSQLQuery;
    AllUncheckButton: TSpeedButton;
    ToolPanel: TPanel;
    SaveButton: TSpeedButton;
    VT1: TVirtualStringTree;
    procedure CancelButtonClick(Sender: TObject);
    procedure AllCheckButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure AllUncheckButtonClick(Sender: TObject);
  private
    VST: TVSTCheckTable;
    DBImages: TDBImages;

  public
    KeyValues, OutKeyValues: TIntVector;
    PickValues, OutPickValues: TStrVector;
    IsAllChecked: Boolean;
  end;

var
  SQLite3CheckList: TSQLite3CheckList;

implementation

{$R *.lfm}

{ TSQLite3CheckList }

procedure TSQLite3CheckList.FormCreate(Sender: TObject);
begin
  SetToolPanels([ToolPanel]);

  DBImages:= TDBImages.Create(nil);
  DBImages.ToButtons([AllCheckButton, AllUncheckButton, SaveButton, CancelButton]);

  VST:= TVSTCheckTable.Create(VT1);
  VST.SelectedBGColor:= VT1.Color;
  VST.HeaderFont.Style:= [fsBold];
end;

procedure TSQLite3CheckList.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSQLite3CheckList.AllCheckButtonClick(Sender: TObject);
begin
  VST.CheckAll(True);
end;

procedure TSQLite3CheckList.AllUncheckButtonClick(Sender: TObject);
begin
  VST.CheckAll(False);
end;

procedure TSQLite3CheckList.FormDestroy(Sender: TObject);
begin
  FreeAndNil(VST);
  FreeAndNil(DBImages);
end;

procedure TSQLite3CheckList.FormShow(Sender: TObject);
var
  i, n: Integer;
begin
  SetEventButtons([SaveButton, CancelButton]);

  VST.AddColumn(Caption, 150);
  VST.SetColumn(Caption, PickValues, taLeftJustify);
  VST.Draw;
  for i:= 0 to High(OutKeyValues) do
  begin
    n:= VIndexOf(KeyValues, OutKeyValues[i]);
    if n>=0 then VST.Checked[n]:= True;
  end;
  AllCheckButton.Enabled:= not VIsNil(KeyValues);
  AllUncheckButton.Enabled:= AllCheckButton.Enabled;
end;

procedure TSQLite3CheckList.SaveButtonClick(Sender: TObject);
var
  i: Integer;
begin
  OutKeyValues:= nil;
  OutPickValues:= nil;

  if VST.IsAllUnchecked then
    VST.CheckAll(True);

  for i:= 0 to High(KeyValues) do
  begin
    if VST.Checked[i] then
    begin
      VAppend(OutKeyValues, KeyValues[i]);
      VAppend(OutPickValues, PickValues[i]);
    end;
  end;

  IsAllChecked:= VST.IsAllChecked;

  ModalResult:= mrOK;
end;



end.

