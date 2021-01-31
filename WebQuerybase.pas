unit WebQuerybase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON;

type

{ TCustomJSONResponse }

  TCustomJSONResponse = class
  private
    fJSONData: TJSONData;
    function GetJSON: string;
    procedure SetJSON(AValue: string); virtual;
    procedure DeStream;
  public
    constructor Create(aJSON: string = '');
    destructor Destroy; override;
    function FormatJSON: string;
    property JSON: string read GetJSON write SetJSON;
  end;

implementation

uses fpjsonrtti;

{ TCustomJSONResponse }

constructor TCustomJSONResponse.Create(aJSON: string);
begin
  JSON:= aJSON;
end;

destructor TCustomJSONResponse.Destroy;
begin
  if Assigned(fJSONData) then
    FreeAndNil(fJSONData);
  inherited Destroy;
end;

function TCustomJSONResponse.FormatJSON: string;
begin
  if Assigned(fJSONData) then
    Result:= fJSONData.FormatJSON();
end;

function TCustomJSONResponse.GetJSON: string;
begin
  if Assigned(fJSONData) then
    Result:= fJSONData.AsJSON;
end;


procedure TCustomJSONResponse.SetJSON(AValue: string);
begin
  if Assigned(fJSONData) then
    FreeAndNil(fJSONData);
  fJSONData:= fpJSON.GetJSON(AValue);
  DeStream;
end;

procedure TCustomJSONResponse.DeStream;
var
  DeStreamer: TJSONDeStreamer;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.JSONToObject(JSON,self);
  finally
    DeStreamer.Free;
  end;
end;

end.

