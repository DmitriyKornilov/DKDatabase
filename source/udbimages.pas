unit UDBImages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TDBImages }

  TDBImages = class(TDataModule)
    PX24: TImageList;
    PX30: TImageList;
    PX36: TImageList;
    PX42: TImageList;
  private

  public

  end;

var
  DBImages: TDBImages;

implementation

{$R *.lfm}

end.

