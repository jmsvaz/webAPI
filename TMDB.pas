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
  TTMDBMovieError = class(TTMDBAPIError);


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
      property ISO_3166_1: string read FISO_3116_1 write SetISO_3116_1;
      property English_Name: string read Fenglish_name write SetEnglish_name;
      property Native_Name: string read Fnative_name write Setnative_name;
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

  { TTMDBMovieCollection }

  TTMDBMovieCollection = class
    private
      FBackdrop_Path: string;
      FID: Integer;
      FName: string;
      FPoster_Path: string;
      procedure SetBackdrop_Path(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetName(AValue: string);
      procedure SetPoster_Path(AValue: string);

    published
      property Name: string read FName write SetName;
      property ID: Integer read FID write SetID;
      property Poster_Path: string read FPoster_Path write SetPoster_Path;
      property Backdrop_Path: string read FBackdrop_Path write SetBackdrop_Path;
  end;

  { TTMDBMovieProductionCompany }

  TTMDBMovieProductionCompany = class(TCollectionitem)
    private
      FID: Integer;
      FLogo_Path: string;
      FName: string;
      FOrigin_Country: string;
      procedure SetID(AValue: Integer);
      procedure SetLogo_Path(AValue: string);
      procedure SetName(AValue: string);
      procedure SetOrigin_Country(AValue: string);
    published
      property Name: string read FName write SetName;
      property ID: Integer read FID write SetID;
      property Logo_Path: string read FLogo_Path write SetLogo_Path;
      property Origin_Country: string read FOrigin_Country write SetOrigin_Country;
  end;

  { TTMDBMovieCountry }

  TTMDBMovieCountry = class(TCollectionitem)
    private
      FISO_3166_1: string;
      FName: string;
      procedure SetISO_3166_1(AValue: string);
      procedure SetName(AValue: string);
    published
      property ISO_3166_1: string read FISO_3166_1 write SetISO_3166_1;
      property Name: string read FName write SetName;
  end;

  { TTMDBMovie }

  TTMDBMovie = class(TCustomJSONResponse)
    private
      FAdult: Boolean;
      FBackdrop_Path: string;
      fBelongs_To_Collection: TTMDBMovieCollection;
      FBudget: Integer;
      fGenres: TCollection;
      FHomepage: string;
      FID: Integer;
      FIMDB_ID: string;
      FOriginal_Language: string;
      FOriginal_Title: string;
      FOverview: string;
      FPopularity: Double;
      FPoster_Path: string;
      fProduction_Companies: TCollection;
      fProduction_Countries: TCollection;
      FRelease_Date: string;
      FRevenue: Integer;
      FRuntime: Integer;
      fSpoken_Languages: TCollection;
      FStatus: string;
      FTagline: string;
      FTitle: string;
      FVideo: Boolean;
      FVote_Average: Double;
      FVote_Count: Integer;
      procedure SetAdult(AValue: Boolean);
      procedure SetBackdrop_Path(AValue: string);
      procedure SetBudget(AValue: Integer);
      procedure SetHomepage(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetIMDB_ID(AValue: string);
      procedure SetOriginal_Language(AValue: string);
      procedure SetOriginal_Title(AValue: string);
      procedure SetOverview(AValue: string);
      procedure SetPopularity(AValue: Double);
      procedure SetPoster_Path(AValue: string);
      procedure SetRelease_Date(AValue: string);
      procedure SetRevenue(AValue: Integer);
      procedure SetRuntime(AValue: Integer);
      procedure SetStatus(AValue: string);
      procedure SetTagline(AValue: string);
      procedure SetTitle(AValue: string);
      procedure SetVideo(AValue: Boolean);
      procedure SetVote_Average(AValue: Double);
      procedure SetVote_Count(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Adult: Boolean read FAdult write SetAdult;
      property Backdrop_Path: string read FBackdrop_Path write SetBackdrop_Path;
      property Belongs_To_Collection: TTMDBMovieCollection read fBelongs_To_Collection;
      property Budget: Integer read FBudget write SetBudget;
      property Genres: TCollection read fGenres;
      property Homepage: string read FHomepage write SetHomepage;
      property ID: Integer read FID write SetID;
      property IMDB_ID: string read FIMDB_ID write SetIMDB_ID;
      property Original_Language: string read FOriginal_Language write SetOriginal_Language;
      property Original_Title: string read FOriginal_Title write SetOriginal_Title;
      property Overview: string read FOverview write SetOverview;
      property Popularity: Double read FPopularity write SetPopularity;
      property Poster_Path: string read FPoster_Path write SetPoster_Path;
      property Production_Companies: TCollection read fProduction_Companies;
      property Production_Countries: TCollection read fProduction_Countries;
      property Release_Date: string read FRelease_Date write SetRelease_Date;
      property Revenue: Integer read FRevenue write SetRevenue;
      property Runtime: Integer read FRuntime write SetRuntime;
      property Spoken_Languages: TCollection read fSpoken_Languages;
      property Status: string read FStatus write SetStatus;
      property Tagline: string read FTagline write SetTagline;
      property Title: string read FTitle write SetTitle;
      property Video: Boolean read FVideo write SetVideo;
      property Vote_Average: Double read FVote_Average write SetVote_Average;
      property Vote_Count: Integer read FVote_Count write SetVote_Count;
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
      function DoRequest(aURL: string): string;
      function ConfigurationURL: string;
      function CountriesURL: string;
      function JobsURL: string;
      function LanguagesURL: string;
      function TimeZonesURL: string;
      function PrimaryTranslationsURL: string;
      function MovieGenresURL: string;
      function TVGenresURL: string;
      function MovieURL(aMovieID: string): string;
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
      function GetMovie(aMovieID: string): TCustomJSONResponse;
    end;

implementation

uses fphttpclient, Dialogs;

const
  TMDBBASEURL = 'https://api.themoviedb.org/';
  TMDBVersionString: array[TTMDBAPIVersion] of string = ('3', '4');

{ TTMDBMovieCollection }

procedure TTMDBMovieCollection.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TTMDBMovieCollection.SetPoster_Path(AValue: string);
begin
  if FPoster_Path=AValue then Exit;
  FPoster_Path:=AValue;
end;

procedure TTMDBMovieCollection.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBMovieCollection.SetBackdrop_Path(AValue: string);
begin
  if FBackdrop_Path=AValue then Exit;
  FBackdrop_Path:=AValue;
end;

{ TTMDBMovieProductionCompany }

procedure TTMDBMovieProductionCompany.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TTMDBMovieProductionCompany.SetOrigin_Country(AValue: string);
begin
  if FOrigin_Country=AValue then Exit;
  FOrigin_Country:=AValue;
end;

procedure TTMDBMovieProductionCompany.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBMovieProductionCompany.SetLogo_Path(AValue: string);
begin
  if FLogo_Path=AValue then Exit;
  FLogo_Path:=AValue;
end;

{ TTMDBMovieCountry }

procedure TTMDBMovieCountry.SetISO_3166_1(AValue: string);
begin
  if FISO_3166_1=AValue then Exit;
  FISO_3166_1:=AValue;
end;

procedure TTMDBMovieCountry.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;


{ TTMDBMovie }

procedure TTMDBMovie.SetAdult(AValue: Boolean);
begin
  if FAdult=AValue then Exit;
  FAdult:=AValue;
end;

procedure TTMDBMovie.SetBackdrop_Path(AValue: string);
begin
  if FBackdrop_Path=AValue then Exit;
  FBackdrop_Path:=AValue;
end;

procedure TTMDBMovie.SetBudget(AValue: Integer);
begin
  if FBudget=AValue then Exit;
  FBudget:=AValue;
end;

procedure TTMDBMovie.SetHomepage(AValue: string);
begin
  if FHomepage=AValue then Exit;
  FHomepage:=AValue;
end;

procedure TTMDBMovie.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBMovie.SetIMDB_ID(AValue: string);
begin
  if FIMDB_ID=AValue then Exit;
  FIMDB_ID:=AValue;
end;

procedure TTMDBMovie.SetOriginal_Language(AValue: string);
begin
  if FOriginal_Language=AValue then Exit;
  FOriginal_Language:=AValue;
end;

procedure TTMDBMovie.SetOriginal_Title(AValue: string);
begin
  if FOriginal_Title=AValue then Exit;
  FOriginal_Title:=AValue;
end;

procedure TTMDBMovie.SetOverview(AValue: string);
begin
  if FOverview=AValue then Exit;
  FOverview:=AValue;
end;

procedure TTMDBMovie.SetPopularity(AValue: Double);
begin
  if FPopularity=AValue then Exit;
  FPopularity:=AValue;
end;

procedure TTMDBMovie.SetPoster_Path(AValue: string);
begin
  if FPoster_Path=AValue then Exit;
  FPoster_Path:=AValue;
end;

procedure TTMDBMovie.SetRelease_Date(AValue: string);
begin
  if FRelease_Date=AValue then Exit;
  FRelease_Date:=AValue;
end;

procedure TTMDBMovie.SetRevenue(AValue: Integer);
begin
  if FRevenue=AValue then Exit;
  FRevenue:=AValue;
end;

procedure TTMDBMovie.SetRuntime(AValue: Integer);
begin
  if FRuntime=AValue then Exit;
  FRuntime:=AValue;
end;

procedure TTMDBMovie.SetStatus(AValue: string);
begin
  if FStatus=AValue then Exit;
  FStatus:=AValue;
end;

procedure TTMDBMovie.SetTagline(AValue: string);
begin
  if FTagline=AValue then Exit;
  FTagline:=AValue;
end;

procedure TTMDBMovie.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
end;

procedure TTMDBMovie.SetVideo(AValue: Boolean);
begin
  if FVideo=AValue then Exit;
  FVideo:=AValue;
end;

procedure TTMDBMovie.SetVote_Average(AValue: Double);
begin
  if FVote_Average=AValue then Exit;
  FVote_Average:=AValue;
end;

procedure TTMDBMovie.SetVote_Count(AValue: Integer);
begin
  if FVote_Count=AValue then Exit;
  FVote_Count:=AValue;
end;

constructor TTMDBMovie.Create(aJSON: string);
begin
  fBelongs_To_Collection:= TTMDBMovieCollection.Create;
  fGenres:= TCollection.Create(TTMDBGenreItem);
  fProduction_Companies:= TCollection.Create(TTMDBMovieProductionCompany);
  fProduction_Countries:= TCollection.Create(TTMDBMovieCountry);
  fSpoken_Languages:= TCollection.Create(TTMDBLanguageItem);
    
  inherited Create(aJSON);
end;

destructor TTMDBMovie.Destroy;
begin
  fBelongs_To_Collection.Free;
  fProduction_Countries.Free;
  fProduction_Companies.Free;
  fSpoken_Languages.Free;
  fGenres.Free;
  inherited Destroy;
end;

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

function TTMDB.GetMovie(aMovieID: string): TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(MovieURL(aMovieID));
    Result:= TTMDBMovie.Create(aRequest);
  except
    Result:= TTMDBMovieError.Create;
  end;

end;

function TTMDB.MovieURL(aMovieID: string): string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/movie/' + aMovieID +
           '?api_key=' + APIKey + '&language=' + Language + '&include_image_language=en,null';

//           + '&append_to_response=videos,images';
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

