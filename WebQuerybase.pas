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

  { TCollectionJSONResponse }

  TCollectionJSONResponse = class(TCollection)
    private
      fJSONData: TJSONData;
      function GetJSON: string;
      procedure SetJSON(AValue: string);
      procedure DeStream;
    public
      constructor Create(AItemClass: TCollectionItemClass; aJSON: string = '');
      destructor Destroy; override;
      function FormatJSON: string;
      property JSON: string read GetJSON write SetJSON;
    end;


  { TCustomJSONError }

  TCustomJSONError = class(TCustomJSONResponse)
    private
      fError: string;
      procedure SetError(AValue: string);
    published
      property Error: string read fError write SetError;
  end;

function ValidIMDBid(aID: string): Boolean;

implementation

uses fpjsonrtti, RegExpr; 

function ValidIMDBid(aID: string): Boolean;
var
  regex: TRegExpr;
begin
  regex:= TRegExpr.Create;
  try
    regex.Expression:= 'tt\d{5,7}';
    Result:= regex.Exec(aID);
  finally
    regex.Free;
  end;
end;

{ TCustomJSONError }

procedure TCustomJSONError.SetError(AValue: string);
begin
  if FError=AValue then Exit;
  FError:=AValue;
end;

{ TCollectionJSONResponse }

function TCollectionJSONResponse.GetJSON: string;
begin
  if Assigned(fJSONData) then
    Result:= fJSONData.AsJSON;
end;

procedure TCollectionJSONResponse.SetJSON(AValue: string);
begin
  if Assigned(fJSONData) then
    FreeAndNil(fJSONData);
  fJSONData:= fpJSON.GetJSON(AValue);
  DeStream;
end;

procedure TCollectionJSONResponse.DeStream;
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

constructor TCollectionJSONResponse.Create(AItemClass: TCollectionItemClass;
  aJSON: string);
begin
  inherited Create(AItemClass);
  JSON:= aJSON;
end;

destructor TCollectionJSONResponse.Destroy;
begin
  if Assigned(fJSONData) then
    FreeAndNil(fJSONData);
  inherited Destroy;
end;

function TCollectionJSONResponse.FormatJSON: string;
begin
  if Assigned(fJSONData) then
    Result:= fJSONData.FormatJSON();
end;

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

