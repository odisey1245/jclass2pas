unit MemJarUnzipper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zipper;

type

  { TMemJarUnzipper }

  TMemJarUnzipper = class(TUnZipper)
  private
    FStream: TMemoryStream;
    function GetFileName: string;
    procedure SetFileName(AValue: string);
    procedure ZipOpen(Sender: TObject; var AStream: TStream);
    procedure ZipClose(Sender: TObject; var AStream: TStream);
    procedure ZipCreateStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure ZipDoneStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
  public
    constructor Create;
    destructor Destroy; override;
    function Unzip(entry: TFullZipFileEntry): TStream;
    property FileName: string read GetFileName write SetFileName;
  end;

implementation

{ TMemJarUnzipper }

procedure TMemJarUnzipper.ZipClose(Sender: TObject; var AStream: TStream);
begin
  AStream := nil;
end;

procedure TMemJarUnzipper.ZipOpen(Sender: TObject; var AStream: TStream);
begin
  AStream := FStream;
end;

procedure TMemJarUnzipper.ZipCreateStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream := TMemoryStream.Create;
  AItem.Stream := AStream;
end;

procedure TMemJarUnzipper.ZipDoneStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream.Position := 0;
  AItem.Stream := AStream;
  AStream := nil;
end;

function TMemJarUnzipper.GetFileName: string;
begin
  Result := inherited FileName;
end;

procedure TMemJarUnzipper.SetFileName(AValue: string);
begin
  inherited FileName := AValue;
  FStream.Clear;
  FStream.LoadFromFile(AValue);
  Examine;
end;

constructor TMemJarUnzipper.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
  OnOpenInputStream := @ZipOpen;
  OnCloseInputStream := @ZipClose;
  OnCreateStream := @ZipCreateStream;
  OnDoneStream := @ZipDoneStream;
end;

destructor TMemJarUnzipper.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TMemJarUnzipper.Unzip(entry: TFullZipFileEntry): TStream;
begin
  OpenInput;
  UnZipOneFile(entry);
  Result := entry.Stream;
  CloseInput;
end;

end.

