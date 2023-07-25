unit DK_DBUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DbCtrls, Graphics, DB, SQLDB;

procedure DataSetChangesSave(const ADataSet: TDataSet);

procedure ChangeDbNavigatorGlyphs(ADbNavigator: TDbNavigator; AImageList: TImageList);

implementation

procedure DataSetChangesSave(const ADataSet: TDataSet);
begin
  (ADataSet As TSQLQuery).ApplyUpdates;
  (ADataSet As TSQLQuery).SQLTransaction.CommitRetaining;
end;

procedure ChangeDbNavButton(ADBNav: TDbNavigator;
                            const ADBBtnType: TDBNavButtonType;
                            const ADBBtnGlyph: TBitmap;
                            const ADBBtnCursor: TCursor = crDefault);
var
  i: Integer;
  NB: TDBNavButton;
begin
  for i := 0 to ADBNav.ControlCount - 1 do
  begin
    if ADBNav.Controls[i].ClassName = 'TDBNavButton' then
    begin
      NB:= (ADBNav.Controls[i] As TDBNavButton);
      if NB.Index= ADBBtnType then
      begin
        NB.Glyph := ADBBtnGlyph;
        NB.Cursor:= ADBBtnCursor;
      end;
    end;
  end;
end;

procedure ChangeDbNavigatorGlyphs(ADbNavigator: TDbNavigator; AImageList: TImageList);
var
  BM: TBitmap;
begin
  if not Assigned(AImageList) then Exit;
  if AImageList.Count<5 then Exit;
  BM:= TBitmap.Create;
  try
    AImageList.GetBitmap(0, BM);
    ChangeDBNavButton(ADbNavigator, nbInsert, BM, crHandPoint);
    AImageList.GetBitmap(1, BM);
    ChangeDBNavButton(ADbNavigator, nbDelete, BM, crHandPoint);
    AImageList.GetBitmap(2, BM);
    ChangeDBNavButton(ADbNavigator, nbEdit, BM, crHandPoint);
    AImageList.GetBitmap(3, BM);
    ChangeDBNavButton(ADbNavigator, nbPost, BM, crHandPoint);
    AImageList.GetBitmap(4, BM);
    ChangeDBNavButton(ADbNavigator, nbCancel, BM, crHandPoint);
  finally
    FreeAndNil(BM);
  end;
end;

end.

