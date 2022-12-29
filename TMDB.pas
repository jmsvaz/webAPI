unit TMDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON, WebQuerybase;

type

  { TTMDBAPIVersion }

  TTMDBAPIVersion = (TMDBAPIv3, TMDBAPIv4);

  { TTMDBAPIError }

  TTMDBAPIError = class(TCustomJSONError);
  TTMDBAPIMovieNotFoundError = class(TTMDBAPIError);
  TTMDBAPIIncorrectIMDbIDError = class(TTMDBAPIError);

  TTMDBAPICountriesError = class(TTMDBAPIError);

  { TTMDBCountryItem }

  TTMDBCountryItem = class(TCollectionitem)
    private
      FEnglissName: string;
      FISO_3116_1: string;
      procedure SetEnglissName(AValue: string);
      procedure SetISO_3116_1(AValue: string);
    published
      property ISO_3116_1: string read FISO_3116_1 write SetISO_3116_1;
      property EnglissName: string read FEnglissName write SetEnglissName;
  end;

  { TTMDBCountries }

  TTMDBCountries = class(TCustomJSONResponse)
    private
      FCountries: TCollection;
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Countries: TCollection read FCountries;
    end;

  { TTMDB }

  TTMDB = class
    private
      aCaption: string;
      fAPIKey: string;
      FLanguage: string;
      fTimeOut: Integer;
      fVersion: TTMDBAPIVersion;
      procedure SetAPIKey(AValue: string);
      procedure SetLanguage(AValue: string);
      procedure SetTimeOut(AValue: Integer);
      procedure SetVersion(AValue: TTMDBAPIVersion);
      function RequestURL(aParams: string): string;
      function DoRequest(aURL: string): string;
      function GetRequest(const aParams: string): string;
      function ProcessRequest(aJSON: string): TCustomJSONResponse;
      function GetResponse(aParams: string): TCustomJSONResponse;
      function CountriesParam: string;
    public
      constructor Create(aAPIKey: string = '');
      property TimeOut: Integer read fTimeOut write SetTimeOut;
      property APIKey: string read fAPIKey write SetAPIKey;
      property Version: TTMDBAPIVersion read fVersion write SetVersion;
      property Caption: string read aCaption;
      property Language: string read FLanguage write SetLanguage;
      function GetCountries: TCustomJSONResponse;
    end;

implementation

uses fphttpclient;

const
  TMDBBASEURL = 'https://api.themoviedb.org/';
  TMDBVersionString: array[TTMDBAPIVersion] of string = ('3', '4');

{ TTMDBCountries }

constructor TTMDBCountries.Create(aJSON: string);
begin
  FCountries:= TCollection.Create(TTMDBCountryItem);
  inherited Create(aJSON);
end;

destructor TTMDBCountries.Destroy;
begin
  FCountries.Free;
  inherited Destroy;
end;


{ TTMDBCountryItem }

procedure TTMDBCountryItem.SetISO_3116_1(AValue: string);
begin
  if FISO_3116_1=AValue then Exit;
  FISO_3116_1:=AValue;
end;

procedure TTMDBCountryItem.SetEnglissName(AValue: string);
begin
  if FEnglissName=AValue then Exit;
  FEnglissName:=AValue;
end;

{ TTMDB }

constructor TTMDB.Create(aAPIKey: string);
begin
  Version:= TMDBAPIv3;
  TimeOut:= 0; // Infinite timeout on most platforms
  Language:= 'en-US';
  APIKey:= aAPIKey;
  aCaption:= 'The Movie DB';
end;  

function TTMDB.CountriesParam: string;
begin
  Result:= 'configuration/countries';
end;

function TTMDB.GetCountries: TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= GetRequest(CountriesParam);
    Result:= TTMDBCountries.Create(aRequest);
  except
    Result:= TTMDBAPICountriesError.Create;
  end;
end;

procedure TTMDB.SetAPIKey(AValue: string);
begin
  if fAPIKey=AValue then Exit;
  fAPIKey:=AValue;
end;

function TTMDB.GetRequest(const aParams: string): string;
var
  completeURL: string;
begin
  completeURL:= RequestURL(aParams);
  Result:= DoRequest(completeURL);
end;

procedure TTMDB.SetLanguage(AValue: string);
begin
  if FLanguage=AValue then Exit;
  FLanguage:=AValue;
end;

procedure TTMDB.SetTimeOut(AValue: Integer);
begin
  if fTimeOut=AValue then Exit;
  fTimeOut:=AValue;
end;

procedure TTMDB.SetVersion(AValue: TTMDBAPIVersion);
begin
  if fVersion=AValue then Exit;
  fVersion:=AValue;
end;

function TTMDB.RequestURL(aParams: string): string;
begin
  //https://api.themoviedb.org/3/configuration/countries?api_key=<<api_key>>
  //https://api.themoviedb.org/3/movie/{movie_id}?api_key=<<api_key>>&language=en-US

  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/' + aParams + '?api_key='
           + APIKey + '?language=' + Language;
end;

function TTMDB.DoRequest(aURL: string): string;
var
  httpClient : TFPHTTPClient;
  aResult: string;
begin
  httpClient := TFPHTTPClient.Create(nil);
  try
    httpClient.AddHeader('Accept','application/json');
    httpClient.AddHeader('Content-Type','application/json');
    httpClient.IOTimeout:= Timeout;
    aResult:= httpClient.Get(aURL);
  finally
    httpClient.Free;
  end;
  Result:= aResult;
end;

function TTMDB.ProcessRequest(aJSON: string): TCustomJSONResponse;
var
  json: TJSONObject;
begin
  Result:= nil;

  try
    json:= TJSONObject(GetJSON(aJSON));

    if json.FindPath('Response').AsString = 'False' then
      begin
        if json.FindPath('Error').AsString = 'Movie not found!' then
          Result:= TTMDBAPIMovieNotFoundError.Create(aJSON)
        else
          if json.FindPath('Error').AsString = 'Incorrect IMDb ID.' then
            Result:= TTMDBAPIIncorrectIMDbIDError.Create(aJSON)
          else
            Result:= TTMDBAPIError.Create(aJSON);
      end
    else
      begin
{        if json.FindPath('Response').AsString = 'True' then
          try
            data:= json.FindPath('Type');
            if Assigned(data) then
              begin
                if data.AsString = 'movie' then
                  Result:= TOMDBMovie.Create(aJSON);
                if data.AsString = 'series' then
                  Result:= TOMDBSeries.Create(aJSON);
                if data.AsString = 'episode' then
                  Result:= TOMDBEpisode.Create(aJSON);
                if data.AsString = 'game' then
                  Result:= TOMDBGame.Create(aJSON);
              end
            else
            begin
             data:= json.FindPath('Search');
            if Assigned(data) then
              Result:= TTMDBSearch.Create(aJSON);

            end;

          except
            Result:= TTMDBAPIError.Create(aJSON);
          end;}
      end;
  except
    Result:= TTMDBAPIError.Create;
  end;

end;

function TTMDB.GetResponse(aParams: string): TCustomJSONResponse;
var
  aRequest: string;
begin
  aRequest:= GetRequest(aParams);
  Result:= ProcessRequest(aRequest);
end;


end.

