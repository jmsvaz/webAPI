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
  TTMDBGenreListError = class(TTMDBAPIError);


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

  { TTMDBGenreItem }

  TTMDBGenreItem =  class(TCollectionitem)
    private
      FId: Integer;
      FName: string;
      procedure SetId(AValue: Integer);
      procedure SetName(AValue: string);
    published
      property Id: Integer read FId write SetId;
      property Name: string read FName write SetName;
  end;

  { TTMDBGenreList }

  TTMDBGenreList = class(TCustomJSONResponse)
    private
      fGenres: TCollection;
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Genres: TCollection read fGenres;
  end;

  { TTMDB }

  TTMDB = class
    private
      aCaption: string;
      fAPIKey: string;
      fConfiguration: TTMDBConfiguration;
      fCountries: TTMDBCountries;
      fJobs: TTMDBJobs;
      FLanguage: string;
      fLanguages: TTMDBLanguages;
      fMovieGenres: TTMDBGenreList;
      fPrimaryTranslations: TTMDBPrimaryTranslations;
      fTimeOut: Integer;
      fTimeZones: TTMDBTimeZones;
      fTVGenres: TTMDBGenreList;
      fVersion: TTMDBAPIVersion;
      procedure SetAPIKey(AValue: string);
      procedure SetLanguage(AValue: string);
      procedure SetTimeOut(AValue: Integer);
      procedure SetVersion(AValue: TTMDBAPIVersion);
      function RequestURL(aParams: string): string;
      function DoRequest(aURL: string): string;
      function ConfigurationURL: string;
      function CountriesURL: string;
      function JobsURL: string;
      function LanguagesURL: string;
      function TimeZonesURL: string;
      function PrimaryTranslationsURL: string;
      function MovieGenresURL: string;
      function TVGenresURL: string;
      function GetCountries: TCollectionJSONResponse;
      function GetJobs: TCollectionJSONResponse; 
      function GetLanguages: TCollectionJSONResponse;
      function GetTimeZones: TCollectionJSONResponse;
      function GetConfiguration: TCustomJSONResponse; 
      function GetPrimaryTranslations: TStringsJSONResponse;
      function GetMovieGenres: TCustomJSONResponse;
      function GetTVGenres: TCustomJSONResponse;
    public
      constructor Create(aAPIKey: string = '');
      function UpdateConfiguration: boolean;
      property TimeOut: Integer read fTimeOut write SetTimeOut;
      property APIKey: string read fAPIKey write SetAPIKey;
      property Version: TTMDBAPIVersion read fVersion write SetVersion;
      property Caption: string read aCaption;
      property Language: string read FLanguage write SetLanguage;
      property Countries: TTMDBCountries read fCountries;
      property Jobs: TTMDBJobs read fJobs;
      property Languages: TTMDBLanguages read fLanguages;
      property TimeZones: TTMDBTimeZones read fTimeZones;
      property Configuration: TTMDBConfiguration read fConfiguration;
      property PrimaryTranslations: TTMDBPrimaryTranslations read fPrimaryTranslations;
      property MovieGenres: TTMDBGenreList read fMovieGenres;
      property TVGenres: TTMDBGenreList read fTVGenres;
    end;

implementation

uses fphttpclient, Dialogs;

const
  TMDBBASEURL = 'https://api.themoviedb.org/';
  TMDBVersionString: array[TTMDBAPIVersion] of string = ('3', '4');

{ TTMDBGenreList }

constructor TTMDBGenreList.Create(aJSON: string);
begin
  fGenres:= TCollection.Create(TTMDBGenreItem);
  inherited Create(aJSON);
end;

destructor TTMDBGenreList.Destroy;
begin
  fGenres.Free;
  inherited Destroy;
end;


{ TTMDBGenreItem }

procedure TTMDBGenreItem.SetId(AValue: Integer);
begin
  if FId=AValue then Exit;
  FId:=AValue;
end;

procedure TTMDBGenreItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

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

procedure TTMDB.SetAPIKey(AValue: string);
begin
  if fAPIKey=AValue then Exit;
  fAPIKey:=AValue;
  UpdateConfiguration;
end;

function TTMDB.UpdateConfiguration: boolean;
begin
  try
    fCountries:= TTMDBCountries(GetCountries);
    fJobs:= TTMDBJobs(GetJobs);
    fLanguages:= TTMDBLanguages(GetLanguages);
    fTimeZones:= TTMDBTimeZones(GetTimeZones);
    fConfiguration:= TTMDBConfiguration(GetConfiguration);
    fPrimaryTranslations:= TTMDBPrimaryTranslations(GetPrimaryTranslations);
    fMovieGenres:= TTMDBGenreList(GetMovieGenres);
    fTVGenres:= TTMDBGenreList(GetTVGenres);

    Result:= True;
  except
    Result:= False;
  end;
end;

function TTMDB.GetConfiguration: TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(ConfigurationURL);
    Result:= TTMDBConfiguration.Create(aRequest);
  except
    Result:= TTMDBAPIConfigurationError.Create;
  end;
end;

function TTMDB.ConfigurationURL: string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/configuration' +
           '?api_key=' + APIKey + '&language=' + Language;
end;

function TTMDB.GetCountries: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(CountriesURL);
    Result:= TTMDBCountries.Create(TTMDBCountryItem, aRequest);
  except
    Result:= TTMDBAPICountriesError.Create(TTMDBCountryItem);
  end;
end;

function TTMDB.CountriesURL: string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/configuration/countries' +
           '?api_key=' + APIKey + '&language=' + Language;
end;

function TTMDB.GetJobs: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(JobsURL);
    Result:= TTMDBJobs.Create(TTMDBJobItem, aRequest);
  except
    Result:= TTMDBAPIJobsError.Create(TTMDBJobItem);
  end;
end;

function TTMDB.JobsURL: string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/configuration/jobs' +
           '?api_key=' + APIKey + '&language=' + Language;
end;

function TTMDB.GetLanguages: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(LanguagesURL);
    Result:= TTMDBLanguages.Create(TTMDBLanguageItem, aRequest);
  except
    Result:= TTMDBAPILanguagesError.Create(TTMDBLanguageItem);
  end;
end;

function TTMDB.LanguagesURL: string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/configuration/languages' +
           '?api_key=' + APIKey + '&language=' + Language;
end;

function TTMDB.GetTimeZones: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(TimeZonesURL);
    Result:= TTMDBTimeZones.Create(TTMDBTimeZoneItem, aRequest);
  except
    Result:= TTMDBAPITimeZonesError.Create(TTMDBLanguageItem);
  end;
end;

function TTMDB.TimeZonesURL: string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/configuration/timezones' +
           '?api_key=' + APIKey + '&language=' + Language;
end;

function TTMDB.GetPrimaryTranslations: TStringsJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(PrimaryTranslationsURL);
    Result:= TTMDBPrimaryTranslations.Create(aRequest);
  except
    Result:= TTMDBAPIPrimaryTranslationsError.Create;
  end;
end;

function TTMDB.PrimaryTranslationsURL: string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/configuration/primary_translations' +
           '?api_key=' + APIKey + '&language=' + Language;
end;

function TTMDB.GetMovieGenres: TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(MovieGenresURL);
    Result:= TTMDBGenreList.Create(aRequest);
  except
    Result:= TTMDBGenreListError.Create;
  end;
end;

function TTMDB.MovieGenresURL: string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/genre/movie/list' +
           '?api_key=' + APIKey + '&language=' + Language;
end;

function TTMDB.GetTVGenres: TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(TVGenresURL);
    Result:= TTMDBGenreList.Create(aRequest);
  except
    Result:= TTMDBGenreListError.Create;
  end;
end;

function TTMDB.TVGenresURL: string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/genre/tv/list' +
           '?api_key=' + APIKey + '&language=' + Language;
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




end.

