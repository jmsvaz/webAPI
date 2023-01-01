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
  TTMDBCollectionError = class(TCollectionJSONError);
  TTMDBAPIMovieNotFoundError = class(TTMDBAPIError);
  TTMDBAPIIncorrectIMDbIDError = class(TTMDBAPIError);
  TTMDBAPIConfigurationError = class(TTMDBAPIError);

  TTMDBAPICountriesError = class(TTMDBCollectionError);
  TTMDBAPIJobsError = class(TTMDBCollectionError);
  TTMDBAPILanguagesError = class(TTMDBCollectionError);
  TTMDBAPITimeZonesError = class(TTMDBCollectionError);
  TTMDBAPIPrimaryTranslationsError = class(TStringsJSONError);


  { TTMDBCountryItem }

  TTMDBCountryItem = class(TCollectionitem)
    private
      Fenglish_name: string;
      FISO_3116_1: string;
      Fnative_name: string;
      procedure SetEnglish_name(AValue: string);
      procedure SetISO_3116_1(AValue: string);
      procedure Setnative_name(AValue: string);
    published
      property iso_3166_1: string read FISO_3116_1 write SetISO_3116_1;
      property english_name: string read Fenglish_name write SetEnglish_name;
      property native_name: string read Fnative_name write Setnative_name;
  end;

  { TTMDBCountries }

  TTMDBCountries = class(TCollectionJSONResponse);

  { TTMDBJobItem }

  TTMDBJobItem = class(TCollectionitem)
    private
      FDepartment: string;
      fJobs: TStrings;
      procedure SetDepartment(AValue: string);
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;
    published
      property Department: string read FDepartment write SetDepartment;
      property Jobs: TStrings read fJobs;
  end;

  TTMDBJobs = class(TCollectionJSONResponse);

  { TTMDBLanguageItem }

  TTMDBLanguageItem = class(TCollectionitem)
    private
      FEnglish_name: string;
      FISO_639_1: string;
      FName: string;
      procedure SetEnglish_name(AValue: string);
      procedure SetISO_639_1(AValue: string);
      procedure SetName(AValue: string);
    published
      property ISO_639_1: string read FISO_639_1 write SetISO_639_1;
      property English_name: string read FEnglish_name write SetEnglish_name;
      property Name: string read FName write SetName;
  end;

  TTMDBLanguages = class(TCollectionJSONResponse);

  { TTMDBTimeZoneItem }

  TTMDBTimeZoneItem = class(TCollectionitem)
    private
      FISO_3166_1: string;
      fZones: TStrings;
      procedure SetISO_3166_1(AValue: string);

    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;
    published
      property ISO_3166_1: string read FISO_3166_1 write SetISO_3166_1;
      property Zones: TStrings read fZones;
  end;

  TTMDBTimeZones = class(TCollectionJSONResponse);

  { TTMDBConfigurationImages }

  TTMDBConfigurationImages = class
    private
      fBackdrop_Sizes: TStrings;
      FBase_URL: string;
      fLogo_Sizes: TStrings;
      fPoster_Sizes: TStrings;
      fProfile_Sizes: TStrings;
      FSecure_Base_URL: string;
      fStill_Sizes: TStrings;
      procedure SetBase_URL(AValue: string);
      procedure SetSecure_Base_URL(AValue: string);
    public
      constructor Create;
      destructor Destroy; override;
    published
      property Base_URL: string read FBase_URL write SetBase_URL;
      property Secure_Base_URL: string read FSecure_Base_URL write SetSecure_Base_URL;
      property Backdrop_Sizes: TStrings read fBackdrop_Sizes;
      property Logo_Sizes: TStrings read fLogo_Sizes;
      property Poster_Sizes: TStrings read fPoster_Sizes;
      property Profile_Sizes: TStrings read fProfile_Sizes;
      property Still_Sizes: TStrings read fStill_Sizes;
  end;

  { TTMDBConfiguration }

  TTMDBConfiguration = class(TCustomJSONResponse)
    private
      fChange_Keys: TStrings;
      fImages: TTMDBConfigurationImages;
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Images: TTMDBConfigurationImages read fImages;
      property Change_Keys: TStrings read fChange_Keys;
  end;


  TTMDBPrimaryTranslations = class(TStringsJSONResponse);

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
      function ConfigurationParam: string;
      function CountriesParam: string;
      function JobsParam: string;
      function LanguagesParam: string;
      function TimeZonesParam: string;
      function PrimaryTranslationsParams: string;
    public
      constructor Create(aAPIKey: string = '');
      property TimeOut: Integer read fTimeOut write SetTimeOut;
      property APIKey: string read fAPIKey write SetAPIKey;
      property Version: TTMDBAPIVersion read fVersion write SetVersion;
      property Caption: string read aCaption;
      property Language: string read FLanguage write SetLanguage;
      function GetCountries: TCollectionJSONResponse;
      function GetJobs: TCollectionJSONResponse;
      function GetLanguages: TCollectionJSONResponse;
      function GetTimeZones: TCollectionJSONResponse;
      function GetConfiguration: TCustomJSONResponse;
      function GetPrimaryTranslations: TStringsJSONResponse;
    end;

implementation

uses fphttpclient, Dialogs;

const
  TMDBBASEURL = 'https://api.themoviedb.org/';
  TMDBVersionString: array[TTMDBAPIVersion] of string = ('3', '4');

{ TTMDBConfigurationImages }

procedure TTMDBConfigurationImages.SetSecure_Base_URL(AValue: string);
begin
  if FSecure_Base_URL=AValue then Exit;
  FSecure_Base_URL:=AValue;
end;

procedure TTMDBConfigurationImages.SetBase_URL(AValue: string);
begin
  if FBase_URL=AValue then Exit;
  FBase_URL:=AValue;
end;

constructor TTMDBConfigurationImages.Create;
begin
  inherited Create;
  fBackdrop_Sizes:= TStringList.Create;
  fLogo_Sizes:= TStringList.Create;
  fPoster_Sizes:= TStringList.Create;
  fProfile_Sizes:= TStringList.Create;
  fStill_Sizes:= TStringList.Create;
end;

destructor TTMDBConfigurationImages.Destroy;
begin
  fBackdrop_Sizes.Free;
  fLogo_Sizes.Free;
  fPoster_Sizes.Free;
  fProfile_Sizes.Free;
  fStill_Sizes.Free;
  inherited Destroy;
end;

{ TTMDBConfiguration }

constructor TTMDBConfiguration.Create(aJSON: string);
begin
  fChange_Keys:= TStringList.Create;
  fImages:= TTMDBConfigurationImages.Create;
  inherited Create(aJSON);
end;

destructor TTMDBConfiguration.Destroy;
begin
  Images.Free;
  fChange_Keys.Free;
  inherited Destroy;
end;

{ TTMDBTimeZoneItem }

procedure TTMDBTimeZoneItem.SetISO_3166_1(AValue: string);
begin
  if FISO_3166_1=AValue then Exit;
  FISO_3166_1:=AValue;
end;

constructor TTMDBTimeZoneItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fZones:= TStringList.Create;
end;

destructor TTMDBTimeZoneItem.Destroy;
begin
  fZones.Free;
  inherited Destroy;
end;

{ TTMDBLanguageItem }

procedure TTMDBLanguageItem.SetISO_639_1(AValue: string);
begin
  if FISO_639_1=AValue then Exit;
  FISO_639_1:=AValue;
end;

procedure TTMDBLanguageItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TTMDBLanguageItem.SetEnglish_name(AValue: string);
begin
  if FEnglish_name=AValue then Exit;
  FEnglish_name:=AValue;
end;


{ TTMDBJobItem }

procedure TTMDBJobItem.SetDepartment(AValue: string);
begin
  if FDepartment=AValue then Exit;
  FDepartment:=AValue;
end;

constructor TTMDBJobItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fJobs:= TStringList.Create;
end;

destructor TTMDBJobItem.Destroy;
begin
  Jobs.Free;
end;

{ TTMDBCountryItem }

procedure TTMDBCountryItem.SetISO_3116_1(AValue: string);
begin
  if FISO_3116_1=AValue then Exit;
  FISO_3116_1:=AValue;
end;

procedure TTMDBCountryItem.Setnative_name(AValue: string);
begin
  if Fnative_name=AValue then Exit;
  Fnative_name:=AValue;
end;

procedure TTMDBCountryItem.SetEnglish_name(AValue: string);
begin
  if Fenglish_name=AValue then Exit;
  Fenglish_name:=AValue;
end;

{ TTMDB }

constructor TTMDB.Create(aAPIKey: string);
begin
  Version:= TMDBAPIv3;
  TimeOut:= 0; // Infinite timeout on most platforms
  Language:= 'pt-BR';   //'en-US';
  APIKey:= aAPIKey;
  aCaption:= 'The Movie DB';
end;  

procedure TTMDB.SetAPIKey(AValue: string);
begin
  if fAPIKey=AValue then Exit;
  fAPIKey:=AValue;
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

function TTMDB.GetConfiguration: TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= GetRequest(ConfigurationParam);
    Result:= TTMDBConfiguration.Create(aRequest);
  except
    Result:= TTMDBAPIConfigurationError.Create;
  end;
end;

function TTMDB.ConfigurationParam: string;
begin
  Result:= 'configuration';
end;

function TTMDB.GetCountries: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= GetRequest(CountriesParam);
    Result:= TTMDBCountries.Create(TTMDBCountryItem, aRequest);
  except
    Result:= TTMDBAPICountriesError.Create(TTMDBCountryItem);
  end;
end;

function TTMDB.CountriesParam: string;
begin
  Result:= 'configuration/countries';
end;

function TTMDB.GetJobs: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= GetRequest(JobsParam);
    Result:= TTMDBJobs.Create(TTMDBJobItem, aRequest);
  except
    Result:= TTMDBAPIJobsError.Create(TTMDBJobItem);
  end;
end;

function TTMDB.JobsParam: string;
begin
  Result:= 'configuration/jobs';
end;

function TTMDB.GetLanguages: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= GetRequest(LanguagesParam);
    Result:= TTMDBLanguages.Create(TTMDBLanguageItem, aRequest);
  except
    Result:= TTMDBAPILanguagesError.Create(TTMDBLanguageItem);
  end;
end;

function TTMDB.LanguagesParam: string;
begin
  Result:= 'configuration/languages';
end;

function TTMDB.GetTimeZones: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= GetRequest(TimeZonesParam);
    Result:= TTMDBTimeZones.Create(TTMDBTimeZoneItem, aRequest);
  except
    Result:= TTMDBAPITimeZonesError.Create(TTMDBLanguageItem);
  end;
end;

function TTMDB.TimeZonesParam: string;
begin
  Result:= 'configuration/timezones';
end;

function TTMDB.GetPrimaryTranslations: TStringsJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= GetRequest(PrimaryTranslationsParams);
    Result:= TTMDBPrimaryTranslations.Create(aRequest);
  except
    Result:= TTMDBAPIPrimaryTranslationsError.Create;
  end;
end;

function TTMDB.PrimaryTranslationsParams: string;
begin
  Result:= 'configuration/primary_translations';
end;

function TTMDB.GetRequest(const aParams: string): string;
var
  completeURL: string;
begin
  completeURL:= RequestURL(aParams);
  Showmessage(completeURL);
  Result:= DoRequest(completeURL);
end;

function TTMDB.RequestURL(aParams: string): string;
begin
  //https://api.themoviedb.org/3/configuration/countries?api_key=<<api_key>>&language=en-US
  //https://api.themoviedb.org/3/movie/{movie_id}?api_key=<<api_key>>&language=en-US

  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/' + aParams + '?api_key='
           + APIKey + '&language=' + Language;
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

