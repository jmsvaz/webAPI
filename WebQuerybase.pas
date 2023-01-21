unit WebQuerybase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON, fgl;

type

  EHTTPError = class(Exception);
  E400BadRequestError = class(EHTTPError);
  E401UnauthorizedError = class(EHTTPError);
  E403ForbiddenError = class(EHTTPError);
  E404NotFoundError = class(EHTTPError);

resourcestring
  sEHTTPError = 'Unknow HTTP Error';
  sE400BadRequestError = 'Bad Request';
  sE401UnauthorizedError = 'Unauthorized';
  sE403ForbiddenError = 'Forbidden';
  sE404NotFoundError = 'Not Found';

type

{ TCustomJSONResponse }

  TCustomJSONResponse = class(TPersistent)
    private
      fJSONData: TJSONData;
      function GetJSON: string;
      procedure SetJSON(AValue: string); virtual;
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
    public
      constructor Create(AItemClass: TCollectionItemClass; aJSON: string = '');
      destructor Destroy; override;
      function FormatJSON: string;
      property JSON: string read GetJSON write SetJSON;
    end;

  { TStringsJSONResponse }

  TStringsJSONResponse = class(TStringList)
    private
      fJSONData: TJSONData;
      function GetJSON: string;
      procedure SetJSON(AValue: string);
    public
      constructor Create(aJSON: string = '');
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

  { TCollectionJSONError }

  TCollectionJSONError = class(TCollectionJSONResponse)
    private
      fError: string;
      procedure SetError(AValue: string);
    published
      property Error: string read fError write SetError;
  end;

  { TCollectionJSONError }

  { TStringsJSONError }

  TStringsJSONError = class(TStringsJSONResponse)
    private
      fError: string;
      procedure SetError(AValue: string);
    published
      property Error: string read fError write SetError;
  end;

  TIntegerList = specialize TFPGList<Integer>;

function ValidIMDBid(aID: string): Boolean;
function ValidRegion(aRegion: string): Boolean;

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

function ValidRegion(aRegion: string): Boolean;
var
  regex: TRegExpr;
begin
  regex:= TRegExpr.Create;
  try
    regex.Expression:= '^[A-Z]{2}$';
    Result:= regex.Exec(aRegion);
  finally
    regex.Free;
  end;
end;

procedure DeStream(Const JSON : TJSONStringType; AObject : TObject);
var
  DeStreamer: TJSONDeStreamer;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.Options:= DeStreamer.Options + [jdoIgnoreNulls,jdoCaseInsensitive];
    DeStreamer.JSONToObject(JSON,AObject);
  finally
    DeStreamer.Free;
  end;
end;

{ TStringsJSONResponse }

function TStringsJSONResponse.GetJSON: string;
begin
  if Assigned(fJSONData) then
    Result:= fJSONData.AsJSON;
end;

procedure TStringsJSONResponse.SetJSON(AValue: string);
begin
  if Assigned(fJSONData) then
    FreeAndNil(fJSONData);
  if AValue <> EmptyStr then
    begin
      fJSONData:= fpJSON.GetJSON(AValue);
      DeStream (AValue,self);
    end;
end;

constructor TStringsJSONResponse.Create(aJSON: string);
begin
  JSON:= aJSON;
  inherited Create;
end;

destructor TStringsJSONResponse.Destroy;
begin
  if Assigned(fJSONData) then
    FreeAndNil(fJSONData);
  inherited Destroy;
end;

function TStringsJSONResponse.FormatJSON: string;
begin
  if Assigned(fJSONData) then
    Result:= fJSONData.FormatJSON();
end;

{ TCollectionJSONError }

procedure TCollectionJSONError.SetError(AValue: string);
begin
  if fError=AValue then Exit;
  fError:=AValue;
end;

{ TStringsJSONError }

procedure TStringsJSONError.SetError(AValue: string);
begin
  if fError=AValue then Exit;
  fError:=AValue;
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
  if AValue <> EmptyStr then
    begin
      fJSONData:= fpJSON.GetJSON(AValue);
      DeStream(AValue,self);
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
  DeStream(AValue,self);
end;


end.

