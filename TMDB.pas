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
  TTMDBCollectionJSONError = class(TCollectionJSONError);
  TTMDBAPIMovieNotFoundError = class(TTMDBAPIError);
  TTMDBAPIIncorrectIMDbIDError = class(TTMDBAPIError);
  TTMDBAPIConfigurationError = class(TTMDBAPIError);
  TTMDBGenreListError = class(TTMDBAPIError);
  TTMDBMovieError = class(TTMDBAPIError);
  TTMDBCompanyError = class(TTMDBAPIError);
  TTMDBPersonError = class(TTMDBAPIError);
  TTMDBCollectionError = class(TTMDBAPIError);
  TTMDBSearchError = class(TTMDBAPIError);
  TTMDBRequestTokenError = class(TTMDBAPIError);

  TTMDBAPICountriesError = class(TTMDBCollectionJSONError);
  TTMDBAPIJobsError = class(TTMDBCollectionJSONError);
  TTMDBAPILanguagesError = class(TTMDBCollectionJSONError);
  TTMDBAPITimeZonesError = class(TTMDBCollectionJSONError);
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
      property English_Name: string read FEnglish_name write SetEnglish_name;
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
      property ID: Integer read FId write SetId;
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

  { TTMDBCollectionItem }

  TTMDBCollectionItem = class(TCollectionitem)
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

  { TTMDBCompanyItem }

  TTMDBCompanyItem = class(TCollectionitem)
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

  { TTMDBMovieTitleItem }

  TTMDBMovieTitleItem = class(TCollectionitem)
    private
      FISO_3166_1: string;
      FTitle: string;
      procedure SetISO_3166_1(AValue: string);
      procedure SetTitle(AValue: string);

    published
      property ISO_3166_1: string read FISO_3166_1 write SetISO_3166_1;
      property Title: string read FTitle write SetTitle;
//      property Type: string   // cannot get this attribute because "type" is a reserved word in freepascal
  end;

  { TTMDBMovieAlternativeTitles }

  TTMDBMovieAlternativeTitles = class(TCustomJSONResponse)
    private
      FID: Integer;
      fTitles: TCollection;
      procedure SetID(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Titles: TCollection read fTitles;
      property ID: Integer read FID write SetID;
  end;


  { TTMDBMovieCastItem }

  TTMDBMovieCastItem = class(TCollectionitem)
    private
      FAdult: string;
      FCast_ID: Integer;
      FCharacter: string;
      FCredit_ID: string;
      FGender: Integer;
      FID: Integer;
      FKnown_for_Department: string;
      FName: string;
      FOrder: Integer;
      FOriginal_Name: string;
      FPopularity: Double;
      FProfile_Path: string;
      procedure SetAdult(AValue: string);
      procedure SetCast_ID(AValue: Integer);
      procedure SetCharacter(AValue: string);
      procedure SetCredit_ID(AValue: string);
      procedure SetGender(AValue: Integer);
      procedure SetID(AValue: Integer);
      procedure SetKnown_for_Department(AValue: string);
      procedure SetName(AValue: string);
      procedure SetOrder(AValue: Integer);
      procedure SetOriginal_Name(AValue: string);
      procedure SetPopularity(AValue: Double);
      procedure SetProfile_Path(AValue: string);
    published
      property Adult: string read FAdult write SetAdult;
      property Gender: Integer read FGender write SetGender;
      property ID: Integer read FID write SetID;
      property Known_for_Department: string read FKnown_for_Department write SetKnown_for_Department;
      property Name: string read FName write SetName;
      property Original_Name: string read FOriginal_Name write SetOriginal_Name;
      property Popularity: Double read FPopularity write SetPopularity;
      property Profile_Path: string read FProfile_Path write SetProfile_Path;
      property Cast_ID: Integer read FCast_ID write SetCast_ID;
      property Character: string read FCharacter write SetCharacter;
      property Credit_ID: string read FCredit_ID write SetCredit_ID;
      property Order: Integer read FOrder write SetOrder;
  end;

  { TTMDBMovieCrewItem }

  TTMDBMovieCrewItem = class(TCollectionitem)
    private
      FAdult: string;
      FCredit_ID: string;
      FDepartment: string;
      FGender: Integer;
      FID: Integer;
      FJob: string;
      FKnown_for_Department: string;
      FName: string;
      FOriginal_Name: string;
      FPopularity: Double;
      FProfile_Path: string;
      procedure SetAdult(AValue: string);
      procedure SetCredit_ID(AValue: string);
      procedure SetDepartment(AValue: string);
      procedure SetGender(AValue: Integer);
      procedure SetID(AValue: Integer);
      procedure SetJob(AValue: string);
      procedure SetKnown_for_Department(AValue: string);
      procedure SetName(AValue: string);
      procedure SetOriginal_Name(AValue: string);
      procedure SetPopularity(AValue: Double);
      procedure SetProfile_Path(AValue: string);
    published
      property Adult: string read FAdult write SetAdult;
      property Gender: Integer read FGender write SetGender;
      property ID: Integer read FID write SetID;
      property Known_for_Department: string read FKnown_for_Department write SetKnown_for_Department;
      property Name: string read FName write SetName;
      property Original_Name: string read FOriginal_Name write SetOriginal_Name;
      property Popularity: Double read FPopularity write SetPopularity;
      property Profile_Path: string read FProfile_Path write SetProfile_Path;
      property Credit_ID: string read FCredit_ID write SetCredit_ID;
      property Department: string read FDepartment write SetDepartment;
      property Job: string read FJob write SetJob;
  end;

  { TTMDBMovieCredits }

  TTMDBMovieCredits = class(TCustomJSONResponse)
    private
      fCast: TCollection;
      fCrew: TCollection;
      FID: Integer;
      procedure SetID(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Cast: TCollection read fCast;
      property Crew: TCollection read fCrew;
      property ID: Integer read FID write SetID;
  end;

  { TTMDBExternalIDs }

  TTMDBExternalIDs = class
    private
      FFacebook_ID: string;
      FFreebase_ID: string;
      FFreebase_MID: string;
      FID: Integer;
      FIMDB_ID: string;
      FInstagram_ID: string;
      FTVRage_ID: string;
      FTwitter_ID: string;
      FWikidata_ID: string;
      procedure SetFacebook_ID(AValue: string);
      procedure SetFreebase_ID(AValue: string);
      procedure SetFreebase_MID(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetIMDB_ID(AValue: string);
      procedure SetInstagram_ID(AValue: string);
      procedure SetTVRage_ID(AValue: string);
      procedure SetTwitter_ID(AValue: string);
      procedure SetWikidata_ID(AValue: string);
    published
      property ID: Integer read FID write SetID;
      property IMDB_ID: string read FIMDB_ID write SetIMDB_ID;
      property Facebook_ID: string read FFacebook_ID write SetFacebook_ID;
      property Instagram_ID: string read FInstagram_ID write SetInstagram_ID;
      property Twitter_ID: string read FTwitter_ID write SetTwitter_ID;
      property Freebase_MID: string read FFreebase_MID write SetFreebase_MID;
      property Freebase_ID: string read FFreebase_ID write SetFreebase_ID;
      property TVRage_ID: string read FTVRage_ID write SetTVRage_ID;
      property Wikidata_ID: string read FWikidata_ID write SetWikidata_ID;
  end;

  { TTMDBImage }

  TTMDBImage = class(TCollectionitem)
    private
      FAspect_Ratio: Double;
      FFile_Path: string;
      FFile_Type: string;
      FHeight: Integer;
      FID: string;
      FISO_639_1: string;
      FVote_Average: Integer;
      FVote_Count: Integer;
      FWidth: Integer;
      procedure SetAspect_Ratio(AValue: Double);
      procedure SetFile_Path(AValue: string);
      procedure SetFile_Type(AValue: string);
      procedure SetHeight(AValue: Integer);
      procedure SetID(AValue: string);
      procedure SetISO_639_1(AValue: string);
      procedure SetVote_Average(AValue: Integer);
      procedure SetVote_Count(AValue: Integer);
      procedure SetWidth(AValue: Integer);
    published
      property Aspect_Ratio: Double read FAspect_Ratio write SetAspect_Ratio;
      property File_Path: string read FFile_Path write SetFile_Path;
      property File_Type: string read FFile_Type write SetFile_Type;
      property Height: Integer read FHeight write SetHeight;
      property ISO_639_1: string read FISO_639_1 write SetISO_639_1;
      property Vote_Average: Integer read FVote_Average write SetVote_Average;
      property Vote_Count: Integer read FVote_Count write SetVote_Count;
      property Width: Integer read FWidth write SetWidth;
      property ID: string read FID write SetID;
  end;

  { TTMDBMovieImages }

  TTMDBMovieImages = class(TCustomJSONResponse)
    private
      fBackdrops: TCollection;
      FID: Integer;
      fPosters: TCollection;
      procedure SetID(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property ID: Integer read FID write SetID;
      property Backdrops: TCollection read fBackdrops;
      property Posters: TCollection read fPosters;
  end;

  { TTMDBKeywordItem }

  TTMDBKeywordItem = class(TCollectionitem)
    private
      FID: Integer;
      FName: string;
      procedure SetID(AValue: Integer);
      procedure SetName(AValue: string);
    published
      property ID: Integer read FID write SetID;
      property Name: string read FName write SetName;
  end;

  { TTMDBMovieKeywords }

  TTMDBMovieKeywords = class(TCustomJSONResponse)
    private
      FID: Integer;
      fKeywords: TCollection;
      procedure SetID(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property ID: Integer read FID write SetID;
      property Keywords: TCollection read fKeywords;
  end;

  { TTMDBMovieReleaseDateItem }

  TTMDBMovieReleaseDateItem = class(TCollectionitem)
    private
      FCertification: string;
      FISO_639_1: string;
      FNote: string;
      FRelease_Date: string;
      procedure SetCertification(AValue: string);
      procedure SetISO_639_1(AValue: string);
      procedure SetNote(AValue: string);
      procedure SetRelease_Date(AValue: string);
    published
      property Certification: string read FCertification write SetCertification;
      property ISO_639_1: string read FISO_639_1 write SetISO_639_1;
      property Release_Date: string read FRelease_Date write SetRelease_Date;
//      property Type: Integer
      property Note: string read FNote write SetNote;
  end;

  { TTMDBMovieReleaseDateResult }

  TTMDBMovieReleaseDateResult = class(TCollectionitem)
    private
      FISO_3166_1: string;
      fRelease_Dates: TCollection;
      procedure SetISO_3166_1(AValue: string);
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;
    published
      property ISO_3166_1: string read FISO_3166_1 write SetISO_3166_1;
      property Release_Dates: TCollection read fRelease_Dates;
  end;

  { TTMDBMovieReleaseDates }

  TTMDBMovieReleaseDates = class(TCustomJSONResponse)
    private
      FID: Integer;
      fResults: TCollection;
      procedure SetID(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property ID: Integer read FID write SetID;
      property Results: TCollection read fResults;
  end;

  { TTMDBMovie }

  TTMDBMovie = class(TCustomJSONResponse)
    private
      FAdult: Boolean;
      fAlternative_Titles: TTMDBMovieAlternativeTitles;
      FBackdrop_Path: string;
      fBelongs_To_Collection: TTMDBCollectionItem;
      FBudget: Integer;
      fCredits: TTMDBMovieCredits;
      fExternal_IDs: TTMDBExternalIDs;
      fGenres: TCollection;
      FHomepage: string;
      FID: Integer;
      fImages: TTMDBMovieImages;
      FIMDB_ID: string;
      fKeywords: TTMDBMovieKeywords;
      FOriginal_Language: string;
      FOriginal_Title: string;
      FOverview: string;
      FPopularity: Double;
      FPoster_Path: string;
      fProduction_Companies: TCollection;
      fProduction_Countries: TCollection;
      FRelease_Date: string;
      fRelease_Dates: TTMDBMovieReleaseDates;
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
      property Belongs_to_Collection: TTMDBCollectionItem read fBelongs_To_Collection;
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
      property Alternative_Titles: TTMDBMovieAlternativeTitles read fAlternative_Titles;
      property Credits: TTMDBMovieCredits read fCredits;
      property External_IDs: TTMDBExternalIDs read fExternal_IDs;
      property Images: TTMDBMovieImages read fImages;
      property Keywords: TTMDBMovieKeywords read fKeywords;
      property Release_Dates: TTMDBMovieReleaseDates read fRelease_Dates;
  end;


  { TTMDBCompanyImages }

  TTMDBCompanyImages = class(TCustomJSONResponse)
    private
      FID: Integer;
      fLogos: TCollection;
      procedure SetID(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property ID: Integer read FID write SetID;
      property Logos: TCollection read fLogos;
  end;


  { TTMDBCompanyNameItem }

  TTMDBCompanyNameItem = class(TCollectionitem)
    private
      FName: string;
      procedure SetName(AValue: string);
    published
      property Name: string read FName write SetName;
//      property Type: string   // cannot get this attribute because "type" is a reserved word in freepascal
  end;


  { TTMDBCompanyAlternativeNames }

  TTMDBCompanyAlternativeNames = class(TCustomJSONResponse)
    private
      FID: Integer;
      fResults: TCollection;
      procedure SetID(AValue: Integer);

    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Results: TCollection read fResults;
      property ID: Integer read FID write SetID;
  end;

  { TTMDBCompany }

  TTMDBCompany = class(TCustomJSONResponse)
    private
      fAlternative_Names: TTMDBCompanyAlternativeNames;
      FDescription: string;
      FHeadquarters: string;
      FHomepage: string;
      FID: Integer;
      fImages: TTMDBCompanyImages;
      FLogo_Path: string;
      FName: string;
      FOrigin_Country: string;
      fParent_Company: TTMDBCompanyItem;
      procedure SetDescription(AValue: string);
      procedure SetHeadquarters(AValue: string);
      procedure SetHomepage(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetLogo_Path(AValue: string);
      procedure SetName(AValue: string);
      procedure SetOrigin_Country(AValue: string);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Description: string read FDescription write SetDescription;
      property Headquarters: string read FHeadquarters write SetHeadquarters;
      property Homepage: string read FHomepage write SetHomepage;
      property ID: Integer read FID write SetID;
      property Logo_Path: string read FLogo_Path write SetLogo_Path;
      property Name: string read FName write SetName;
      property Origin_Country: string read FOrigin_Country write SetOrigin_Country;
      property Parent_Company: TTMDBCompanyItem read fParent_Company;
      property Images: TTMDBCompanyImages read fImages;
      property Alternative_Names: TTMDBCompanyAlternativeNames read fAlternative_Names;
  end;


  { TTMDBPersonImages }

  TTMDBPersonImages = class(TCustomJSONResponse)
    private
      FID: Integer;
      fProfiles: TCollection;
      procedure SetID(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property ID: Integer read FID write SetID;
      property Profiles: TCollection read fProfiles;
  end;

  { TTMDBPersonMovieCastItem }

  TTMDBPersonMovieCastItem = class(TCollectionitem)
    private
      FAdult: string;
      FBackdrop_Path: string;
      FCharacter: string;
      FCredit_ID: string;
      fGenre_IDs: TStrings;
      FID: Integer;
      FOriginal_Language: string;
      FOriginal_Title: string;
      FOverview: string;
      FPopularity: Double;
      FPoster_Path: string;
      FRelease_Date: string;
      FTitle: string;
      FVideo: boolean;
      FVote_Average: Double;
      FVote_Count: Integer;
      procedure SetAdult(AValue: string);
      procedure SetBackdrop_Path(AValue: string);
      procedure SetCharacter(AValue: string);
      procedure SetCredit_ID(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetOriginal_Language(AValue: string);
      procedure SetOriginal_Title(AValue: string);
      procedure SetOverview(AValue: string);
      procedure SetPopularity(AValue: Double);
      procedure SetPoster_Path(AValue: string);
      procedure SetRelease_Date(AValue: string);
      procedure SetTitle(AValue: string);
      procedure SetVideo(AValue: boolean);
      procedure SetVote_Average(AValue: Double);
      procedure SetVote_Count(AValue: Integer);
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;
    published
      property Character: string read FCharacter write SetCharacter;
      property Credit_ID: string read FCredit_ID write SetCredit_ID;
      property Release_Date: string read FRelease_Date write SetRelease_Date;
      property Vote_Count: Integer read FVote_Count write SetVote_Count;
      property Video: boolean read FVideo write SetVideo;
      property Adult: string read FAdult write SetAdult;
      property Vote_Average: Double read FVote_Average write SetVote_Average;
      property Title: string read FTitle write SetTitle;
//      property Genre_IDs: TStrings read fGenre_IDs;  // TODO: discover how to get an array[integer]
      property Original_Language: string read FOriginal_Language write SetOriginal_Language;
      property Original_Title: string read FOriginal_Title write SetOriginal_Title;
      property Popularity: Double read FPopularity write SetPopularity;
      property ID: Integer read FID write SetID;
      property Backdrop_Path: string read FBackdrop_Path write SetBackdrop_Path;
      property Overview: string read FOverview write SetOverview;
      property Poster_Path: string read FPoster_Path write SetPoster_Path;
  end;


  { TTMDBPersonMovieCrewItem }

  TTMDBPersonMovieCrewItem = class(TCollectionitem)
    private
      FAdult: string;
      FBackdrop_Path: string;
      FCredit_ID: string;
      FDepartment: string;
      fGenre_IDs: TStrings;
      FID: Integer;
      FJob: string;
      FOriginal_Language: string;
      FOriginal_Title: string;
      FOverview: string;
      FPopularity: Double;
      FPoster_Path: string;
      FRelease_Date: string;
      FTitle: string;
      FVideo: boolean;
      FVote_Average: Double;
      FVote_Count: Integer;
      procedure SetAdult(AValue: string);
      procedure SetBackdrop_Path(AValue: string);
      procedure SetCredit_ID(AValue: string);
      procedure SetDepartment(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetJob(AValue: string);
      procedure SetOriginal_Language(AValue: string);
      procedure SetOriginal_Title(AValue: string);
      procedure SetOverview(AValue: string);
      procedure SetPopularity(AValue: Double);
      procedure SetPoster_Path(AValue: string);
      procedure SetRelease_Date(AValue: string);
      procedure SetTitle(AValue: string);
      procedure SetVideo(AValue: boolean);
      procedure SetVote_Average(AValue: Double);
      procedure SetVote_Count(AValue: Integer);
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;
    published
      property Credit_ID: string read FCredit_ID write SetCredit_ID;
      property Release_Date: string read FRelease_Date write SetRelease_Date;
      property Vote_Count: Integer read FVote_Count write SetVote_Count;
      property Video: boolean read FVideo write SetVideo;
      property Adult: string read FAdult write SetAdult;
      property Vote_Average: Double read FVote_Average write SetVote_Average;
      property Title: string read FTitle write SetTitle;
//      property Genre_IDs: TStrings read fGenre_IDs;
      property Original_Language: string read FOriginal_Language write SetOriginal_Language;
      property Original_Title: string read FOriginal_Title write SetOriginal_Title;
      property Popularity: Double read FPopularity write SetPopularity;
      property ID: Integer read FID write SetID;
      property Backdrop_Path: string read FBackdrop_Path write SetBackdrop_Path;
      property Overview: string read FOverview write SetOverview;
      property Poster_Path: string read FPoster_Path write SetPoster_Path;
      property Department: string read FDepartment write SetDepartment;
      property Job: string read FJob write SetJob;
  end;

  { TTMDBPersonMovieCredits }

  TTMDBPersonMovieCredits = class(TCustomJSONResponse)
    private
      fCast: TCollection;
      fCrew: TCollection;
      FID: Integer;
      procedure SetID(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Cast: TCollection read fCast;
      property Crew: TCollection read fCrew;
      property ID: Integer read FID write SetID;
  end;

  { TTMDBPersonTVCastItem }

  TTMDBPersonTVCastItem = class(TCollectionitem)
    private
      FBackdrop_Path: string;
      FCharacter: string;
      FCredit_ID: string;
      FEpisode_Count: Integer;
      fFirst_Air_Date: string;
      fGenre_IDs: TStrings;
      fOrigin_Country: TStrings;
      FID: Integer;
      fName: string;
      FOriginal_Language: string;
      FOriginal_Name: string;
      FOverview: string;
      FPopularity: Double;
      FPoster_Path: string;
      FVote_Average: Double;
      FVote_Count: Integer;
      procedure SetBackdrop_Path(AValue: string);
      procedure SetCharacter(AValue: string);
      procedure SetCredit_ID(AValue: string);
      procedure SetEpisode_Count(AValue: Integer);
      procedure SetFirst_Air_Date(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetName(AValue: string);
      procedure SetOriginal_Language(AValue: string);
      procedure SetOriginal_Name(AValue: string);
      procedure SetOverview(AValue: string);
      procedure SetPopularity(AValue: Double);
      procedure SetPoster_Path(AValue: string);
      procedure SetVote_Average(AValue: Double);
      procedure SetVote_Count(AValue: Integer);
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;
    published
      property Character: string read FCharacter write SetCharacter;
      property Credit_ID: string read FCredit_ID write SetCredit_ID;
      property First_Air_Date: string read fFirst_Air_Date write SetFirst_Air_Date;
      property Vote_Count: Integer read FVote_Count write SetVote_Count;
      property Vote_Average: Double read FVote_Average write SetVote_Average;
      property Name: string read fName write SetName;
//      property Genre_IDs: TStrings read fGenre_IDs;
      property Original_Language: string read FOriginal_Language write SetOriginal_Language;
      property Original_Name: string read FOriginal_Name write SetOriginal_Name;
      property Popularity: Double read FPopularity write SetPopularity;
      property ID: Integer read FID write SetID;
      property Backdrop_Path: string read FBackdrop_Path write SetBackdrop_Path;
      property Overview: string read FOverview write SetOverview;
      property Poster_Path: string read FPoster_Path write SetPoster_Path;
      property Episode_Count: Integer read FEpisode_Count write SetEpisode_Count;
      property Origin_Country: TStrings read fOrigin_Country;
  end;

  { TTMDBPersonTVCrewItem }

  TTMDBPersonTVCrewItem = class(TCollectionitem)
    private
      FBackdrop_Path: string;
      FCredit_ID: string;
      FDepartment: string;
      FEpisode_Count: Integer;
      fFirst_Air_Date: string;
      fGenre_IDs: TStrings;
      FID: Integer;
      FJob: string;
      fName: string;
      FOriginal_Language: string;
      FOriginal_Name: string;
      fOrigin_Country: TStrings;
      FOverview: string;
      FPopularity: Double;
      FPoster_Path: string;
      FVote_Average: Double;
      FVote_Count: Integer;
      procedure SetBackdrop_Path(AValue: string);
      procedure SetCredit_ID(AValue: string);
      procedure SetDepartment(AValue: string);
      procedure SetEpisode_Count(AValue: Integer);
      procedure SetFirst_Air_Date(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetJob(AValue: string);
      procedure SetName(AValue: string);
      procedure SetOriginal_Language(AValue: string);
      procedure SetOriginal_Name(AValue: string);
      procedure SetOverview(AValue: string);
      procedure SetPopularity(AValue: Double);
      procedure SetPoster_Path(AValue: string);
      procedure SetVote_Average(AValue: Double);
      procedure SetVote_Count(AValue: Integer);
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;
    published
      property Credit_ID: string read FCredit_ID write SetCredit_ID;
      property First_Air_Date: string read fFirst_Air_Date write SetFirst_Air_Date;
      property Vote_Count: Integer read FVote_Count write SetVote_Count;
      property Vote_Average: Double read FVote_Average write SetVote_Average;
      property Name: string read fName write SetName;
//      property Genre_IDs: TStrings read fGenre_IDs;
      property Original_Language: string read FOriginal_Language write SetOriginal_Language;
      property Original_Name: string read FOriginal_Name write SetOriginal_Name;
      property Popularity: Double read FPopularity write SetPopularity;
      property ID: Integer read FID write SetID;
      property Backdrop_Path: string read FBackdrop_Path write SetBackdrop_Path;
      property Overview: string read FOverview write SetOverview;
      property Poster_Path: string read FPoster_Path write SetPoster_Path;
      property Department: string read FDepartment write SetDepartment;
      property Job: string read FJob write SetJob;
      property Episode_Count: Integer read FEpisode_Count write SetEpisode_Count;
      property Origin_Country: TStrings read fOrigin_Country;
  end;

  { TTMDBPersonTVCredits }

  TTMDBPersonTVCredits = class(TCustomJSONResponse)
    private
      fCast: TCollection;
      fCrew: TCollection;
      FID: Integer;
      procedure SetID(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Cast: TCollection read fCast;
      property Crew: TCollection read fCrew;
      property ID: Integer read FID write SetID;
  end;

  { TTMDBPerson }

  TTMDBPerson = class(TCustomJSONResponse)
    private
      FAdult: Boolean;
      fAlso_Known_As: TStrings;
      FBiography: string;
      FBirthday: string;
      FDeathday: string;
      fExternal_IDs: TTMDBExternalIDs;
      FGender: Integer;
      FHomepage: string;
      FID: Integer;
      fImages: TTMDBPersonImages;
      FIMDB_ID: string;
      FKnown_for_Department: string;
      fMovie_Credits: TTMDBPersonMovieCredits;
      FName: string;
      FPlace_of_Birth: string;
      FPopularity: Double;
      fProfile_Path: string;
      fTV_Credits: TTMDBPersonTVCredits;
      procedure SetAdult(AValue: Boolean);
      procedure SetBiography(AValue: string);
      procedure SetBirthday(AValue: string);
      procedure SetDeathday(AValue: string);
      procedure SetGender(AValue: Integer);
      procedure SetHomepage(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetIMDB_ID(AValue: string);
      procedure SetKnown_for_Department(AValue: string);
      procedure SetName(AValue: string);
      procedure SetPlace_of_Birth(AValue: string);
      procedure SetPopularity(AValue: Double);
      procedure SetProfile_Path(AValue: string);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property ID: Integer read FID write SetID;
      property Name: string read FName write SetName;
      property Birthday: string read FBirthday write SetBirthday;
      property Known_for_Department: string read FKnown_for_Department write SetKnown_for_Department;
      property Deathday: string read FDeathday write SetDeathday;
      property Also_Known_As: TStrings read fAlso_Known_As;
      property Homepage: string read FHomepage write SetHomepage;
      property Gender: Integer read FGender write SetGender;
      property Biography: string read FBiography write SetBiography;
      property Popularity: Double read FPopularity write SetPopularity;
      property Place_of_Birth: string read FPlace_of_Birth write SetPlace_of_Birth;
      property Profile_Path: string read fProfile_Path write SetProfile_Path;
      property Adult: Boolean read FAdult write SetAdult;
      property IMDB_ID: string read FIMDB_ID write SetIMDB_ID;
      property Images: TTMDBPersonImages read fImages;
      property External_IDs: TTMDBExternalIDs read fExternal_IDs;
      property Movie_Credits: TTMDBPersonMovieCredits read fMovie_Credits;
      property TV_Credits: TTMDBPersonTVCredits read fTV_Credits;
  end;


  TTMDBCollection = class(TCustomJSONResponse)
    private
      FBackdrop_Path: string;
      FID: Integer;
      fImages: TTMDBMovieImages;
      fName: string;
      fOverview: string;
      fParts: TCollection;
      FPoster_Path: string;
      procedure SetBackdrop_Path(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetName(AValue: string);
      procedure SetOverview(AValue: string);
      procedure SetPoster_Path(AValue: string);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property ID: Integer read FID write SetID;
      property Name: string read fName write SetName;
      property Overview: string read fOverview write SetOverview;
      property Poster_Path: string read FPoster_Path write SetPoster_Path;
      property Backdrop_Path: string read FBackdrop_Path write SetBackdrop_Path;
      property Parts: TCollection read fParts;
      property Images: TTMDBMovieImages read fImages;
  end;

  { TTMDBSearchResult }

  TTMDBSearchResult = class(TCustomJSONResponse)
    private
      FPage: Integer;
      fResults: TCollection;
      FTotal_Pages: Integer;
      FTotal_Results: Integer;
      procedure SetPage(AValue: Integer);
      procedure SetTotal_Pages(AValue: Integer);
      procedure SetTotal_Results(AValue: Integer);
    protected
      property Results: TCollection read fResults;
    public
      destructor Destroy; override;
    published
      property Page: Integer read FPage write SetPage;
      property Total_Pages: Integer read FTotal_Pages write SetTotal_Pages;
      property Total_Results: Integer read FTotal_Results write SetTotal_Results;
  end;

  { TTMDBSearchCompanyResult }

  TTMDBSearchCompanyResult = class(TTMDBSearchResult)
    public
      constructor Create(aJSON: string = '');
    published
      property Results;
  end;

  { TTMDBSearchMovieItem }

  TTMDBSearchMovieItem = class(TCollectionitem)
    private
      FAdult: Boolean;
      FBackdrop_Path: string;
      FID: Integer;
      FOriginal_Language: string;
      FOriginal_Title: string;
      FOverview: string;
      FPopularity: Double;
      FPoster_Path: string;
      FRelease_Date: string;
      FTitle: string;
      FVideo: Boolean;
      FVote_Average: Double;
      FVote_Count: Integer;
      procedure SetAdult(AValue: Boolean);
      procedure SetBackdrop_Path(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetOriginal_Language(AValue: string);
      procedure SetOriginal_Title(AValue: string);
      procedure SetOverview(AValue: string);
      procedure SetPopularity(AValue: Double);
      procedure SetPoster_Path(AValue: string);
      procedure SetRelease_Date(AValue: string);
      procedure SetTitle(AValue: string);
      procedure SetVideo(AValue: Boolean);
      procedure SetVote_Average(AValue: Double);
      procedure SetVote_Count(AValue: Integer);
    published
      property Adult: Boolean read FAdult write SetAdult;
      property Backdrop_Path: string read FBackdrop_Path write SetBackdrop_Path;
      property ID: Integer read FID write SetID;
      property Original_Language: string read FOriginal_Language write SetOriginal_Language;
      property Original_Title: string read FOriginal_Title write SetOriginal_Title;
      property Overview: string read FOverview write SetOverview;
      property Popularity: Double read FPopularity write SetPopularity;
      property Poster_Path: string read FPoster_Path write SetPoster_Path;
      property Release_Date: string read FRelease_Date write SetRelease_Date;
      property Title: string read FTitle write SetTitle;
      property Video: Boolean read FVideo write SetVideo;
      property Vote_Average: Double read FVote_Average write SetVote_Average;
      property Vote_Count: Integer read FVote_Count write SetVote_Count;
  end;

  { TTMDBSearchMovieResult }

  TTMDBSearchMovieResult = class(TTMDBSearchResult)
    public
      constructor Create(aJSON: string = '');
    published
      property Results;
  end;

  { TTMDBSearchPersonItem }

  TTMDBSearchPersonItem = class(TCollectionitem)
    private
      FAdult: Boolean;
      FGender: Integer;
      FID: Integer;
      FKnown_for_Department: string;
      FName: string;
      FPopularity: Double;
      fProfile_Path: string;
      procedure SetAdult(AValue: Boolean);
      procedure SetGender(AValue: Integer);
      procedure SetID(AValue: Integer);
      procedure SetKnown_for_Department(AValue: string);
      procedure SetName(AValue: string);
      procedure SetPopularity(AValue: Double);
      procedure SetProfile_Path(AValue: string);
    published
      property ID: Integer read FID write SetID;
      property Name: string read FName write SetName;
      property Popularity: Double read FPopularity write SetPopularity;
      property Profile_Path: string read fProfile_Path write SetProfile_Path;
      property Adult: Boolean read FAdult write SetAdult;
      property Known_for_Department: string read FKnown_for_Department write SetKnown_for_Department;
      property Gender: Integer read FGender write SetGender;
  end;

  { TTMDBSearchPersonResult }

  TTMDBSearchPersonResult = class(TTMDBSearchResult)
    public
      constructor Create(aJSON: string = '');
    published
      property Results;
  end;

  { TTMDBSearchCollectionResult }

  TTMDBSearchCollectionResult = class(TTMDBSearchResult)
    public
      constructor Create(aJSON: string = '');
    published
      property Results;
  end;

  { TTMDBRequestToken }

  TTMDBRequestToken = class(TCustomJSONResponse)
    private
      FExpires_At: string;
      FRequest_Token: string;
      FSuccess: Boolean;
      procedure SetExpires_At(AValue: string);
      procedure SetRequest_Token(AValue: string);
      procedure SetSuccess(AValue: Boolean);
    published
      property Success: Boolean read FSuccess write SetSuccess;
      property Expires_At: string read FExpires_At write SetExpires_At;
      property Request_Token: string read FRequest_Token write SetRequest_Token;
  end;

  { TTMDBSessionID }

  TTMDBSessionID = class(TCustomJSONResponse)
    private
      fSession_ID: string;
      FSuccess: Boolean;
      procedure SetSession_ID(AValue: string);
      procedure SetSuccess(AValue: Boolean);
    published
      property Success: Boolean read FSuccess write SetSuccess;
      property Session_ID: string read fSession_ID write SetSession_ID;
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
      fPassword: string;
      fPrimaryTranslations: TTMDBPrimaryTranslations;
      fTimeOut: Integer;
      fTimeZones: TTMDBTimeZones;
      fTVGenres: TTMDBGenreList;
      fUserName: string;
      fVersion: TTMDBAPIVersion;
      fSessionID: string;
      procedure SetAPIKey(AValue: string);
      procedure SetLanguage(AValue: string);
      procedure SetPassword(AValue: string);
      procedure SetTimeOut(AValue: Integer);
      procedure SetUserName(AValue: string);
      procedure SetVersion(AValue: TTMDBAPIVersion);
      function DoRequest(aURL: string): string;
      function DoPost(aURL: string; aJSON: string): string;
      function ValidSession: Boolean;
      function ConfigurationURL: string;
      function CountriesURL: string;
      function JobsURL: string;
      function LanguagesURL: string;
      function TimeZonesURL: string;
      function PrimaryTranslationsURL: string;
      function MovieGenresURL: string;
      function TVGenresURL: string;
      function MovieURL(aMovieID: string): string;
      function CompanyURL(aCompanyID: string): string;
      function PersonURL(aPersonID: string): string;
      function NetworkURL(aNetworkID: string): string;
      function RequestTokenURL: string;
      function ValidateWithLoginURL: string;
      function CreateSessionURL: string;
      function CollectionURL(aCollectionID: string): string;
      function SearchCompanyURL(aCompany: string; aPage: Integer = 1): string;
      function SearchMovieURL(aMovie: string; aPage: Integer = 1; aIncludeAdult: Boolean = False;
                              aRegion: string = ''; aYear: Integer = 0; aPrimaryReleaseYear: Integer = 0): string;
      function SearchPersonURL(aPerson: string; aPage: Integer = 1; aIncludeAdult: Boolean = False;
                              aRegion: string = ''): string;
      function SearchCollectionURL(aCollection: string; aPage: Integer = 1): string;
      function GetCountries: TCollectionJSONResponse;
      function GetJobs: TCollectionJSONResponse;
      function GetLanguages: TCollectionJSONResponse;
      function GetTimeZones: TCollectionJSONResponse;
      function GetConfiguration: TCustomJSONResponse;
      function GetPrimaryTranslations: TStringsJSONResponse;
      function GetMovieGenres: TCustomJSONResponse;
      function GetTVGenres: TCustomJSONResponse;
      function CreateRequestToken: string;
      function ValidateRequestToken(aRequestToken: string): Boolean;
      function CreateSession(aRequestToken: string): string;
    public
      constructor Create(aAPIKey: string = '');
      function UpdateConfiguration: boolean;
      function Authenticate: boolean; overload;
      function Authenticate (aUserName, aPassword: string): boolean; overload;
      property TimeOut: Integer read fTimeOut write SetTimeOut;
      property APIKey: string read fAPIKey write SetAPIKey;
      property UserName: string read fUserName write SetUserName;
      property Password: string read fPassword write SetPassword;
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
      function GetCompany(aCompanyID: string): TCustomJSONResponse;
      function GetPerson(aPersonID: string): TCustomJSONResponse;
      function GetNetwork(aNetworkID: string): TCustomJSONResponse;
      function GetCollection(aCollectionID: string): TCustomJSONResponse;
      function SearchCompany(aCompany: string; aPage: Integer = 1): TCustomJSONResponse;
      function SearchMovie(aMovie: string; aPage: Integer = 1; aIncludeAdult: Boolean = False;
                           aRegion: string = ''; aYear: Integer = 0; aPrimaryReleaseYear: Integer = 0): TCustomJSONResponse;
      function SearchPerson(aPerson: string; aPage: Integer = 1; aIncludeAdult: Boolean = False;
                           aRegion: string = ''): TCustomJSONResponse;
      function SearchCollection(aCollection: string; aPage: Integer = 1): TCustomJSONResponse;
  end;

implementation

uses fphttpclient, Dialogs;

const
  TMDBBASEURL = 'https://api.themoviedb.org/';
  TMDBVersionString: array[TTMDBAPIVersion] of string = ('3', '4');

{ TTMDBSessionID }

procedure TTMDBSessionID.SetSession_ID(AValue: string);
begin
  if fSession_ID=AValue then Exit;
  fSession_ID:=AValue;
end;

procedure TTMDBSessionID.SetSuccess(AValue: Boolean);
begin
  if FSuccess=AValue then Exit;
  FSuccess:=AValue;
end;

{ TTMDBRequestToken }

procedure TTMDBRequestToken.SetSuccess(AValue: Boolean);
begin
  if FSuccess=AValue then Exit;
  FSuccess:=AValue;
end;

procedure TTMDBRequestToken.SetExpires_At(AValue: string);
begin
  if FExpires_At=AValue then Exit;
  FExpires_At:=AValue;
end;

procedure TTMDBRequestToken.SetRequest_Token(AValue: string);
begin
  if FRequest_Token=AValue then Exit;
  FRequest_Token:=AValue;
end;

{ TTMDBCollection }

procedure TTMDBCollection.SetBackdrop_Path(AValue: string);
begin
  if FBackdrop_Path=AValue then Exit;
  FBackdrop_Path:=AValue;
end;

procedure TTMDBCollection.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBCollection.SetName(AValue: string);
begin
  if fName=AValue then Exit;
  fName:=AValue;
end;

procedure TTMDBCollection.SetOverview(AValue: string);
begin
  if fOverview=AValue then Exit;
  fOverview:=AValue;
end;

procedure TTMDBCollection.SetPoster_Path(AValue: string);
begin
  if FPoster_Path=AValue then Exit;
  FPoster_Path:=AValue;
end;

constructor TTMDBCollection.Create(aJSON: string);
begin
  fImages:= TTMDBMovieImages.Create;
  fParts:= TCollection.Create(TTMDBSearchMovieItem);
  inherited Create(aJSON);
end;

destructor TTMDBCollection.Destroy;
begin
  fImages.Free;
  fParts.Free;
  inherited Destroy;
end;

{ TTMDBSearchCollectionResult }

constructor TTMDBSearchCollectionResult.Create(aJSON: string);
begin
fResults:= TCollection.Create(TTMDBCollectionItem);
inherited Create(aJSON);
end;

{ TTMDBSearchPersonItem }

procedure TTMDBSearchPersonItem.SetAdult(AValue: Boolean);
begin
  if FAdult=AValue then Exit;
  FAdult:=AValue;
end;

procedure TTMDBSearchPersonItem.SetGender(AValue: Integer);
begin
  if FGender=AValue then Exit;
  FGender:=AValue;
end;

procedure TTMDBSearchPersonItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBSearchPersonItem.SetKnown_for_Department(AValue: string);
begin
  if FKnown_for_Department=AValue then Exit;
  FKnown_for_Department:=AValue;
end;

procedure TTMDBSearchPersonItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TTMDBSearchPersonItem.SetPopularity(AValue: Double);
begin
  if FPopularity=AValue then Exit;
  FPopularity:=AValue;
end;

procedure TTMDBSearchPersonItem.SetProfile_Path(AValue: string);
begin
  if fProfile_Path=AValue then Exit;
  fProfile_Path:=AValue;
end;

{ TTMDBSearchPersonResult }

constructor TTMDBSearchPersonResult.Create(aJSON: string);
begin
fResults:= TCollection.Create(TTMDBSearchPersonItem);
inherited Create(aJSON);
end;

{ TTMDBSearchMovieItem }

procedure TTMDBSearchMovieItem.SetAdult(AValue: Boolean);
begin
  if FAdult=AValue then Exit;
  FAdult:=AValue;
end;

procedure TTMDBSearchMovieItem.SetBackdrop_Path(AValue: string);
begin
  if FBackdrop_Path=AValue then Exit;
  FBackdrop_Path:=AValue;
end;

procedure TTMDBSearchMovieItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBSearchMovieItem.SetOriginal_Language(AValue: string);
begin
  if FOriginal_Language=AValue then Exit;
  FOriginal_Language:=AValue;
end;

procedure TTMDBSearchMovieItem.SetOriginal_Title(AValue: string);
begin
  if FOriginal_Title=AValue then Exit;
  FOriginal_Title:=AValue;
end;

procedure TTMDBSearchMovieItem.SetOverview(AValue: string);
begin
  if FOverview=AValue then Exit;
  FOverview:=AValue;
end;

procedure TTMDBSearchMovieItem.SetPopularity(AValue: Double);
begin
  if FPopularity=AValue then Exit;
  FPopularity:=AValue;
end;

procedure TTMDBSearchMovieItem.SetPoster_Path(AValue: string);
begin
  if FPoster_Path=AValue then Exit;
  FPoster_Path:=AValue;
end;

procedure TTMDBSearchMovieItem.SetRelease_Date(AValue: string);
begin
  if FRelease_Date=AValue then Exit;
  FRelease_Date:=AValue;
end;

procedure TTMDBSearchMovieItem.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
end;

procedure TTMDBSearchMovieItem.SetVideo(AValue: Boolean);
begin
  if FVideo=AValue then Exit;
  FVideo:=AValue;
end;

procedure TTMDBSearchMovieItem.SetVote_Average(AValue: Double);
begin
  if FVote_Average=AValue then Exit;
  FVote_Average:=AValue;
end;

procedure TTMDBSearchMovieItem.SetVote_Count(AValue: Integer);
begin
  if FVote_Count=AValue then Exit;
  FVote_Count:=AValue;
end;

{ TTMDBSearchMovieResult }

constructor TTMDBSearchMovieResult.Create(aJSON: string);
begin
  fResults:= TCollection.Create(TTMDBSearchMovieItem);
  inherited Create(aJSON);
end;

{ TTMDBSearchCompanyResult }

constructor TTMDBSearchCompanyResult.Create(aJSON: string);
begin
  fResults:= TCollection.Create(TTMDBCompanyItem);
  inherited Create(aJSON);
end;

{ TTMDBSearchResult }

procedure TTMDBSearchResult.SetPage(AValue: Integer);
begin
  if FPage=AValue then Exit;
  FPage:=AValue;
end;

procedure TTMDBSearchResult.SetTotal_Pages(AValue: Integer);
begin
  if FTotal_Pages=AValue then Exit;
  FTotal_Pages:=AValue;
end;

procedure TTMDBSearchResult.SetTotal_Results(AValue: Integer);
begin
  if FTotal_Results=AValue then Exit;
  FTotal_Results:=AValue;
end;

destructor TTMDBSearchResult.Destroy;
begin
  fResults.Free;
  inherited Destroy;
end;

{ TTMDBPerson }

procedure TTMDBPerson.SetHomepage(AValue: string);
begin
  if FHomepage=AValue then Exit;
  FHomepage:=AValue;
end;

procedure TTMDBPerson.SetBirthday(AValue: string);
begin
  if FBirthday=AValue then Exit;
  FBirthday:=AValue;
end;

procedure TTMDBPerson.SetBiography(AValue: string);
begin
  if FBiography=AValue then Exit;
  FBiography:=AValue;
end;

procedure TTMDBPerson.SetAdult(AValue: Boolean);
begin
  if FAdult=AValue then Exit;
  FAdult:=AValue;
end;

procedure TTMDBPerson.SetDeathday(AValue: string);
begin
  if FDeathday=AValue then Exit;
  FDeathday:=AValue;
end;

procedure TTMDBPerson.SetGender(AValue: Integer);
begin
  if FGender=AValue then Exit;
  FGender:=AValue;
end;

procedure TTMDBPerson.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBPerson.SetIMDB_ID(AValue: string);
begin
  if FIMDB_ID=AValue then Exit;
  FIMDB_ID:=AValue;
end;

procedure TTMDBPerson.SetKnown_for_Department(AValue: string);
begin
  if FKnown_for_Department=AValue then Exit;
  FKnown_for_Department:=AValue;
end;

procedure TTMDBPerson.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TTMDBPerson.SetPlace_of_Birth(AValue: string);
begin
  if FPlace_of_Birth=AValue then Exit;
  FPlace_of_Birth:=AValue;
end;

procedure TTMDBPerson.SetPopularity(AValue: Double);
begin
  if FPopularity=AValue then Exit;
  FPopularity:=AValue;
end;

procedure TTMDBPerson.SetProfile_Path(AValue: string);
begin
  if fProfile_Path=AValue then Exit;
  fProfile_Path:=AValue;
end;

constructor TTMDBPerson.Create(aJSON: string);
begin
  fAlso_Known_As:= TStringlist.Create;
  fImages:= TTMDBPersonImages.Create;
  fExternal_IDs:= TTMDBExternalIDs.Create;
  fMovie_Credits:= TTMDBPersonMovieCredits.Create;
  fTV_Credits:= TTMDBPersonTVCredits.Create;
  inherited Create(aJSON);
end;

destructor TTMDBPerson.Destroy;
begin
  fAlso_Known_As.Free;
  fImages.Free;
  fExternal_IDs.Free;
  fMovie_Credits.Free;
  fTV_Credits.Free;
  inherited Destroy;
end;

{ TTMDBPersonTVCredits }

procedure TTMDBPersonTVCredits.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

constructor TTMDBPersonTVCredits.Create(aJSON: string);
begin
  fCast:= TCollection.Create(TTMDBPersonTVCastItem);
  fCrew:= TCollection.Create(TTMDBPersonTVCrewItem);
  inherited Create(aJSON);
end;

destructor TTMDBPersonTVCredits.Destroy;
begin
  fCast.Free;
  fCrew.Free;
  inherited Destroy;
end;

{ TTMDBPersonTVCrewItem }

procedure TTMDBPersonTVCrewItem.SetBackdrop_Path(AValue: string);
begin
  if FBackdrop_Path=AValue then Exit;
  FBackdrop_Path:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetCredit_ID(AValue: string);
begin
  if FCredit_ID=AValue then Exit;
  FCredit_ID:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetDepartment(AValue: string);
begin
  if FDepartment=AValue then Exit;
  FDepartment:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetEpisode_Count(AValue: Integer);
begin
  if FEpisode_Count=AValue then Exit;
  FEpisode_Count:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetFirst_Air_Date(AValue: string);
begin
  if fFirst_Air_Date=AValue then Exit;
  fFirst_Air_Date:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetJob(AValue: string);
begin
  if FJob=AValue then Exit;
  FJob:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetName(AValue: string);
begin
  if fName=AValue then Exit;
  fName:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetOriginal_Language(AValue: string);
begin
  if FOriginal_Language=AValue then Exit;
  FOriginal_Language:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetOriginal_Name(AValue: string);
begin
  if FOriginal_Name=AValue then Exit;
  FOriginal_Name:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetOverview(AValue: string);
begin
  if FOverview=AValue then Exit;
  FOverview:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetPopularity(AValue: Double);
begin
  if FPopularity=AValue then Exit;
  FPopularity:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetPoster_Path(AValue: string);
begin
  if FPoster_Path=AValue then Exit;
  FPoster_Path:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetVote_Average(AValue: Double);
begin
  if FVote_Average=AValue then Exit;
  FVote_Average:=AValue;
end;

procedure TTMDBPersonTVCrewItem.SetVote_Count(AValue: Integer);
begin
  if FVote_Count=AValue then Exit;
  FVote_Count:=AValue;
end;

constructor TTMDBPersonTVCrewItem.Create(ACollection: TCollection);
begin
  fGenre_IDs:= TStringList.Create;
  fOrigin_Country:= TStringList.Create;
  inherited Create(ACollection);
end;

destructor TTMDBPersonTVCrewItem.Destroy;
begin
  fGenre_IDs.Free;
  fOrigin_Country.Free;
  inherited Destroy;
end;

{ TTMDBPersonTVCastItem }

procedure TTMDBPersonTVCastItem.SetBackdrop_Path(AValue: string);
begin
  if FBackdrop_Path=AValue then Exit;
  FBackdrop_Path:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetCharacter(AValue: string);
begin
  if FCharacter=AValue then Exit;
  FCharacter:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetCredit_ID(AValue: string);
begin
  if FCredit_ID=AValue then Exit;
  FCredit_ID:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetEpisode_Count(AValue: Integer);
begin
  if FEpisode_Count=AValue then Exit;
  FEpisode_Count:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetFirst_Air_Date(AValue: string);
begin
  if fFirst_Air_Date=AValue then Exit;
  fFirst_Air_Date:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetName(AValue: string);
begin
  if fName=AValue then Exit;
  fName:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetOriginal_Language(AValue: string);
begin
  if FOriginal_Language=AValue then Exit;
  FOriginal_Language:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetOriginal_Name(AValue: string);
begin
  if FOriginal_Name=AValue then Exit;
  FOriginal_Name:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetOverview(AValue: string);
begin
  if FOverview=AValue then Exit;
  FOverview:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetPopularity(AValue: Double);
begin
  if FPopularity=AValue then Exit;
  FPopularity:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetPoster_Path(AValue: string);
begin
  if FPoster_Path=AValue then Exit;
  FPoster_Path:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetVote_Average(AValue: Double);
begin
  if FVote_Average=AValue then Exit;
  FVote_Average:=AValue;
end;

procedure TTMDBPersonTVCastItem.SetVote_Count(AValue: Integer);
begin
  if FVote_Count=AValue then Exit;
  FVote_Count:=AValue;
end;

constructor TTMDBPersonTVCastItem.Create(ACollection: TCollection);
begin
  fGenre_IDs:= TStringList.Create;
  fOrigin_Country:= TStringList.Create;
  inherited Create(ACollection);
end;

destructor TTMDBPersonTVCastItem.Destroy;
begin
  fGenre_IDs.Free;
  fOrigin_Country.Free;
  inherited Destroy;
end;

{ TTMDBPersonMovieCredits }

procedure TTMDBPersonMovieCredits.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

constructor TTMDBPersonMovieCredits.Create(aJSON: string);
begin
  fCast:= TCollection.Create(TTMDBPersonMovieCastItem);
  fCrew:= TCollection.Create(TTMDBPersonMovieCrewItem);
  inherited Create(aJSON);
end;

destructor TTMDBPersonMovieCredits.Destroy;
begin
  fCast.Free;
  fCrew.Free;
  inherited Destroy;
end;

{ TTMDBPersonMovieCrewItem }

procedure TTMDBPersonMovieCrewItem.SetAdult(AValue: string);
begin
  if FAdult=AValue then Exit;
  FAdult:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetBackdrop_Path(AValue: string);
begin
  if FBackdrop_Path=AValue then Exit;
  FBackdrop_Path:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetCredit_ID(AValue: string);
begin
  if FCredit_ID=AValue then Exit;
  FCredit_ID:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetDepartment(AValue: string);
begin
  if FDepartment=AValue then Exit;
  FDepartment:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetJob(AValue: string);
begin
  if FJob=AValue then Exit;
  FJob:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetOriginal_Language(AValue: string);
begin
  if FOriginal_Language=AValue then Exit;
  FOriginal_Language:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetOriginal_Title(AValue: string);
begin
  if FOriginal_Title=AValue then Exit;
  FOriginal_Title:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetOverview(AValue: string);
begin
  if FOverview=AValue then Exit;
  FOverview:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetPopularity(AValue: Double);
begin
  if FPopularity=AValue then Exit;
  FPopularity:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetPoster_Path(AValue: string);
begin
  if FPoster_Path=AValue then Exit;
  FPoster_Path:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetRelease_Date(AValue: string);
begin
  if FRelease_Date=AValue then Exit;
  FRelease_Date:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetVideo(AValue: boolean);
begin
  if FVideo=AValue then Exit;
  FVideo:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetVote_Average(AValue: Double);
begin
  if FVote_Average=AValue then Exit;
  FVote_Average:=AValue;
end;

procedure TTMDBPersonMovieCrewItem.SetVote_Count(AValue: Integer);
begin
  if FVote_Count=AValue then Exit;
  FVote_Count:=AValue;
end;

constructor TTMDBPersonMovieCrewItem.Create(ACollection: TCollection);
begin
  fGenre_IDs:= TStringList.Create;
  inherited Create(ACollection);
end;

destructor TTMDBPersonMovieCrewItem.Destroy;
begin
  fGenre_IDs.Free;
  inherited Destroy;
end;

{ TTMDBPersonMovieCastItem }

procedure TTMDBPersonMovieCastItem.SetCharacter(AValue: string);
begin
  if FCharacter=AValue then Exit;
  FCharacter:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetAdult(AValue: string);
begin
  if FAdult=AValue then Exit;
  FAdult:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetBackdrop_Path(AValue: string);
begin
  if FBackdrop_Path=AValue then Exit;
  FBackdrop_Path:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetCredit_ID(AValue: string);
begin
  if FCredit_ID=AValue then Exit;
  FCredit_ID:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetOriginal_Language(AValue: string);
begin
  if FOriginal_Language=AValue then Exit;
  FOriginal_Language:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetOriginal_Title(AValue: string);
begin
  if FOriginal_Title=AValue then Exit;
  FOriginal_Title:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetOverview(AValue: string);
begin
  if FOverview=AValue then Exit;
  FOverview:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetPopularity(AValue: Double);
begin
  if FPopularity=AValue then Exit;
  FPopularity:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetPoster_Path(AValue: string);
begin
  if FPoster_Path=AValue then Exit;
  FPoster_Path:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetRelease_Date(AValue: string);
begin
  if FRelease_Date=AValue then Exit;
  FRelease_Date:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetVideo(AValue: boolean);
begin
  if FVideo=AValue then Exit;
  FVideo:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetVote_Average(AValue: Double);
begin
  if FVote_Average=AValue then Exit;
  FVote_Average:=AValue;
end;

procedure TTMDBPersonMovieCastItem.SetVote_Count(AValue: Integer);
begin
  if FVote_Count=AValue then Exit;
  FVote_Count:=AValue;
end;

constructor TTMDBPersonMovieCastItem.Create(ACollection: TCollection);
begin
  fGenre_IDs:= TStringList.Create;
  inherited Create(ACollection);
end;

destructor TTMDBPersonMovieCastItem.Destroy;
begin
  fGenre_IDs.Free;
  inherited Destroy;
end;

{ TTMDBPersonImages }

procedure TTMDBPersonImages.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

constructor TTMDBPersonImages.Create(aJSON: string);
begin
  fProfiles:= TCollection.Create(TTMDBImage);
  inherited Create(aJSON);
end;

destructor TTMDBPersonImages.Destroy;
begin
  fProfiles.Free;
  inherited Destroy;
end;

{ TTMDBCompanyAlternativeNames }

procedure TTMDBCompanyAlternativeNames.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

constructor TTMDBCompanyAlternativeNames.Create(aJSON: string);
begin
  fResults:= TCollection.Create(TTMDBCompanyNameItem);
  inherited Create(aJSON);
end;

destructor TTMDBCompanyAlternativeNames.Destroy;
begin
  fResults.Free;
  inherited Destroy;
end;


{ TTMDBCompanyNameItem }

procedure TTMDBCompanyNameItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

{ TTMDBCompanyImages }

procedure TTMDBCompanyImages.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

constructor TTMDBCompanyImages.Create(aJSON: string);
begin
  fLogos:= TCollection.Create(TTMDBImage);
  inherited Create(aJSON);
end;

destructor TTMDBCompanyImages.Destroy;
begin
  fLogos.Free;
  inherited Destroy;
end;

{ TTMDBCompany }

procedure TTMDBCompany.SetDescription(AValue: string);
begin
  if FDescription=AValue then Exit;
  FDescription:=AValue;
end;

procedure TTMDBCompany.SetHeadquarters(AValue: string);
begin
  if FHeadquarters=AValue then Exit;
  FHeadquarters:=AValue;
end;

procedure TTMDBCompany.SetHomepage(AValue: string);
begin
  if FHomepage=AValue then Exit;
  FHomepage:=AValue;
end;

procedure TTMDBCompany.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBCompany.SetLogo_Path(AValue: string);
begin
  if FLogo_Path=AValue then Exit;
  FLogo_Path:=AValue;
end;

procedure TTMDBCompany.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TTMDBCompany.SetOrigin_Country(AValue: string);
begin
  if FOrigin_Country=AValue then Exit;
  FOrigin_Country:=AValue;
end;

constructor TTMDBCompany.Create(aJSON: string);
begin
  fParent_Company:= TTMDBCompanyItem.Create(nil);
  fImages:= TTMDBCompanyImages.Create;
  fAlternative_Names:= TTMDBCompanyAlternativeNames.Create;
  inherited Create(aJSON);
end;

destructor TTMDBCompany.Destroy;
begin
  fParent_Company.Free;
  fImages.Free;
  fAlternative_Names.Free;
  inherited Destroy;
end;

{ TTMDBMovieReleaseDates }

procedure TTMDBMovieReleaseDates.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

constructor TTMDBMovieReleaseDates.Create(aJSON: string);
begin
  fResults:= TCollection.Create(TTMDBMovieReleaseDateResult);
  inherited Create(aJSON);
end;

destructor TTMDBMovieReleaseDates.Destroy;
begin
  fResults.Free;
  inherited Destroy;
end;

{ TTMDBMovieReleaseDateResult }

procedure TTMDBMovieReleaseDateResult.SetISO_3166_1(AValue: string);
begin
  if FISO_3166_1=AValue then Exit;
  FISO_3166_1:=AValue;
end;

constructor TTMDBMovieReleaseDateResult.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fRelease_Dates:= TCollection.Create(TTMDBMovieReleaseDateItem);
end;

destructor TTMDBMovieReleaseDateResult.Destroy;
begin
  fRelease_Dates.Free;
  inherited;
end;

{ TTMDBMovieReleaseDateItem }

procedure TTMDBMovieReleaseDateItem.SetCertification(AValue: string);
begin
  if FCertification=AValue then Exit;
  FCertification:=AValue;
end;

procedure TTMDBMovieReleaseDateItem.SetISO_639_1(AValue: string);
begin
  if FISO_639_1=AValue then Exit;
  FISO_639_1:=AValue;
end;

procedure TTMDBMovieReleaseDateItem.SetNote(AValue: string);
begin
  if FNote=AValue then Exit;
  FNote:=AValue;
end;

procedure TTMDBMovieReleaseDateItem.SetRelease_Date(AValue: string);
begin
  if FRelease_Date=AValue then Exit;
  FRelease_Date:=AValue;
end;

{ TTMDBMovieKeywords }

procedure TTMDBMovieKeywords.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

constructor TTMDBMovieKeywords.Create(aJSON: string);
begin
  fKeywords:= TCollection.Create(TTMDBKeywordItem);
  inherited Create(aJSON);
end;

destructor TTMDBMovieKeywords.Destroy;
begin
  inherited Destroy;
end;

{ TTMDBKeywordItem }

procedure TTMDBKeywordItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBKeywordItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

{ TTMDBMovieImages }

procedure TTMDBMovieImages.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

constructor TTMDBMovieImages.Create(aJSON: string);
begin
  fBackdrops:= TCollection.Create(TTMDBImage);
  fPosters:= TCollection.Create(TTMDBImage);
  inherited Create(aJSON);
end;

destructor TTMDBMovieImages.Destroy;
begin
  fBackdrops.Free;
  fPosters.Free;
  inherited Destroy;
end;

{ TTMDBImage }

procedure TTMDBImage.SetAspect_Ratio(AValue: Double);
begin
  if FAspect_Ratio=AValue then Exit;
  FAspect_Ratio:=AValue;
end;

procedure TTMDBImage.SetFile_Path(AValue: string);
begin
  if FFile_Path=AValue then Exit;
  FFile_Path:=AValue;
end;

procedure TTMDBImage.SetFile_Type(AValue: string);
begin
  if FFile_Type=AValue then Exit;
  FFile_Type:=AValue;
end;

procedure TTMDBImage.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TTMDBImage.SetID(AValue: string);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBImage.SetISO_639_1(AValue: string);
begin
  if FISO_639_1=AValue then Exit;
  FISO_639_1:=AValue;
end;

procedure TTMDBImage.SetVote_Average(AValue: Integer);
begin
  if FVote_Average=AValue then Exit;
  FVote_Average:=AValue;
end;

procedure TTMDBImage.SetVote_Count(AValue: Integer);
begin
  if FVote_Count=AValue then Exit;
  FVote_Count:=AValue;
end;

procedure TTMDBImage.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
end;

{ TTMDBExternalIDs }

procedure TTMDBExternalIDs.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBExternalIDs.SetFacebook_ID(AValue: string);
begin
  if FFacebook_ID=AValue then Exit;
  FFacebook_ID:=AValue;
end;

procedure TTMDBExternalIDs.SetFreebase_ID(AValue: string);
begin
  if FFreebase_ID=AValue then Exit;
  FFreebase_ID:=AValue;
end;

procedure TTMDBExternalIDs.SetFreebase_MID(AValue: string);
begin
  if FFreebase_MID=AValue then Exit;
  FFreebase_MID:=AValue;
end;

procedure TTMDBExternalIDs.SetIMDB_ID(AValue: string);
begin
  if FIMDB_ID=AValue then Exit;
  FIMDB_ID:=AValue;
end;

procedure TTMDBExternalIDs.SetInstagram_ID(AValue: string);
begin
  if FInstagram_ID=AValue then Exit;
  FInstagram_ID:=AValue;
end;

procedure TTMDBExternalIDs.SetTVRage_ID(AValue: string);
begin
  if FTVRage_ID=AValue then Exit;
  FTVRage_ID:=AValue;
end;

procedure TTMDBExternalIDs.SetTwitter_ID(AValue: string);
begin
  if FTwitter_ID=AValue then Exit;
  FTwitter_ID:=AValue;
end;

procedure TTMDBExternalIDs.SetWikidata_ID(AValue: string);
begin
  if FWikidata_ID=AValue then Exit;
  FWikidata_ID:=AValue;
end;


{ TTMDBMovieCrewItem }

procedure TTMDBMovieCrewItem.SetAdult(AValue: string);
begin
  if FAdult=AValue then Exit;
  FAdult:=AValue;
end;

procedure TTMDBMovieCrewItem.SetCredit_ID(AValue: string);
begin
  if FCredit_ID=AValue then Exit;
  FCredit_ID:=AValue;
end;

procedure TTMDBMovieCrewItem.SetDepartment(AValue: string);
begin
  if FDepartment=AValue then Exit;
  FDepartment:=AValue;
end;

procedure TTMDBMovieCrewItem.SetGender(AValue: Integer);
begin
  if FGender=AValue then Exit;
  FGender:=AValue;
end;

procedure TTMDBMovieCrewItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBMovieCrewItem.SetJob(AValue: string);
begin
  if FJob=AValue then Exit;
  FJob:=AValue;
end;

procedure TTMDBMovieCrewItem.SetKnown_for_Department(AValue: string);
begin
  if FKnown_for_Department=AValue then Exit;
  FKnown_for_Department:=AValue;
end;

procedure TTMDBMovieCrewItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TTMDBMovieCrewItem.SetOriginal_Name(AValue: string);
begin
  if FOriginal_Name=AValue then Exit;
  FOriginal_Name:=AValue;
end;

procedure TTMDBMovieCrewItem.SetPopularity(AValue: Double);
begin
  if FPopularity=AValue then Exit;
  FPopularity:=AValue;
end;

procedure TTMDBMovieCrewItem.SetProfile_Path(AValue: string);
begin
  if FProfile_Path=AValue then Exit;
  FProfile_Path:=AValue;
end;

{ TTMDBMovieCastItem }

procedure TTMDBMovieCastItem.SetAdult(AValue: string);
begin
  if FAdult=AValue then Exit;
  FAdult:=AValue;
end;

procedure TTMDBMovieCastItem.SetCast_ID(AValue: Integer);
begin
  if FCast_ID=AValue then Exit;
  FCast_ID:=AValue;
end;

procedure TTMDBMovieCastItem.SetCharacter(AValue: string);
begin
  if FCharacter=AValue then Exit;
  FCharacter:=AValue;
end;

procedure TTMDBMovieCastItem.SetCredit_ID(AValue: string);
begin
  if FCredit_ID=AValue then Exit;
  FCredit_ID:=AValue;
end;

procedure TTMDBMovieCastItem.SetGender(AValue: Integer);
begin
  if FGender=AValue then Exit;
  FGender:=AValue;
end;

procedure TTMDBMovieCastItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBMovieCastItem.SetKnown_for_Department(AValue: string);
begin
  if FKnown_for_Department=AValue then Exit;
  FKnown_for_Department:=AValue;
end;

procedure TTMDBMovieCastItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TTMDBMovieCastItem.SetOrder(AValue: Integer);
begin
  if FOrder=AValue then Exit;
  FOrder:=AValue;
end;

procedure TTMDBMovieCastItem.SetOriginal_Name(AValue: string);
begin
  if FOriginal_Name=AValue then Exit;
  FOriginal_Name:=AValue;
end;

procedure TTMDBMovieCastItem.SetPopularity(AValue: Double);
begin
  if FPopularity=AValue then Exit;
  FPopularity:=AValue;
end;

procedure TTMDBMovieCastItem.SetProfile_Path(AValue: string);
begin
  if FProfile_Path=AValue then Exit;
  FProfile_Path:=AValue;
end;

{ TTMDBMovieCredits }

procedure TTMDBMovieCredits.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

constructor TTMDBMovieCredits.Create(aJSON: string);
begin
  fCast:= TCollection.Create(TTMDBMovieCastItem);
  fCrew:= TCollection.Create(TTMDBMovieCrewItem);
  inherited Create(aJSON);
end;

destructor TTMDBMovieCredits.Destroy;
begin
  fCast.Free;
  fCrew.Free;
  inherited Destroy;
end;

{ TTMDBMovieTitleItem }

procedure TTMDBMovieTitleItem.SetISO_3166_1(AValue: string);
begin
  if FISO_3166_1=AValue then Exit;
  FISO_3166_1:=AValue;
end;

procedure TTMDBMovieTitleItem.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
end;

{ TTMDBMovieAlternativeTitles }

procedure TTMDBMovieAlternativeTitles.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

constructor TTMDBMovieAlternativeTitles.Create(aJSON: string);
begin
  fTitles:= TCollection.Create(TTMDBMovieTitleItem);
  inherited Create(aJSON);
end;

destructor TTMDBMovieAlternativeTitles.Destroy;
begin
  fTitles.Free;
  inherited Destroy;
end;

{ TTMDBCollectionItem }

procedure TTMDBCollectionItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TTMDBCollectionItem.SetPoster_Path(AValue: string);
begin
  if FPoster_Path=AValue then Exit;
  FPoster_Path:=AValue;
end;

procedure TTMDBCollectionItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBCollectionItem.SetBackdrop_Path(AValue: string);
begin
  if FBackdrop_Path=AValue then Exit;
  FBackdrop_Path:=AValue;
end;


{ TTMDBCompanyItem }

procedure TTMDBCompanyItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TTMDBCompanyItem.SetOrigin_Country(AValue: string);
begin
  if FOrigin_Country=AValue then Exit;
  FOrigin_Country:=AValue;
end;

procedure TTMDBCompanyItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TTMDBCompanyItem.SetLogo_Path(AValue: string);
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
  fBelongs_To_Collection:= TTMDBCollectionItem.Create(nil);
  fGenres:= TCollection.Create(TTMDBGenreItem);
  fProduction_Companies:= TCollection.Create(TTMDBCompanyItem);
  fProduction_Countries:= TCollection.Create(TTMDBMovieCountry);
  fSpoken_Languages:= TCollection.Create(TTMDBLanguageItem);
  fAlternative_Titles:= TTMDBMovieAlternativeTitles.Create;
  fCredits:= TTMDBMovieCredits.Create;
  fExternal_IDs:= TTMDBExternalIDs.Create;
  fImages:= TTMDBMovieImages.Create;
  fKeywords:= TTMDBMovieKeywords.Create;
  fRelease_Dates:= TTMDBMovieReleaseDates.Create;
    
  inherited Create(aJSON);
end;

destructor TTMDBMovie.Destroy;
begin
  fBelongs_To_Collection.Free;
  fProduction_Countries.Free;
  fProduction_Companies.Free;
  fSpoken_Languages.Free;
  fGenres.Free;
  fCredits.Free;
  fAlternative_Titles.Free;
  fExternal_IDs.Free;
  fImages.Free;
  fKeywords.Free;
  fRelease_Dates.Free;
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
  fSessionID:= EmptyStr;
end;  

procedure TTMDB.SetLanguage(AValue: string);
begin
  if FLanguage=AValue then Exit;
  FLanguage:=AValue;
end;

procedure TTMDB.SetPassword(AValue: string);
begin
  if fPassword=AValue then Exit;
  fPassword:=AValue;
end;

procedure TTMDB.SetTimeOut(AValue: Integer);
begin
  if fTimeOut=AValue then Exit;
  fTimeOut:=AValue;
end;

procedure TTMDB.SetUserName(AValue: string);
begin
  if fUserName=AValue then Exit;
  fUserName:=AValue;
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

function TTMDB.Authenticate: boolean;
var
  aRequestToken: string;
begin
  Result:= False;
  aRequestToken:= CreateRequestToken;
  if ValidateRequestToken(aRequestToken) then
    fSessionID:= CreateSession(aRequestToken);
  Result:= ValidSession;
end;

function TTMDB.Authenticate(aUserName, aPassword: string): boolean;
begin
  Result:= False;
  UserName:= aUserName;
  Password:= aPassword;
  Result:= Authenticate;
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

function TTMDB.CreateRequestToken: string;
var
  aRequest: string;
  Token: TTMDBRequestToken;
begin
  Result:= EmptyStr;
  try
    aRequest:= DoRequest(RequestTokenURL);
    Token:= TTMDBRequestToken.Create(aRequest);
    Result:= Token.Request_Token;
  finally
    Token.Free;
  end;
end;

function TTMDB.ValidateRequestToken(aRequestToken: string): Boolean;
var
  aJSON: string;
  aRequest: string;
  Token: TTMDBRequestToken;
begin
  Result:= False;
  aJSON:= '{"username": "' + UserName + '",' +
           '"password": "' + Password + '",' +
           '"request_token": "' + aRequestToken + '"}';
  try
    aRequest:= DoPost(ValidateWithLoginURL,aJSON);
    Token:= TTMDBRequestToken.Create(aRequest);
    Result:= Token.Success;
  finally
    Token.Free;
  end;
end;

function TTMDB.CreateSession(aRequestToken: string): string;
var
  aJSON: string;
  aRequest: string;
  Session: TTMDBSessionID;
begin
  Result:= EmptyStr;
  aJSON:= '{"request_token": "' + aRequestToken + '"}';
  try
    aRequest:= DoPost(CreateSessionURL,aJSON);
    Session:= TTMDBSessionID.Create(aRequest);
    if Session.Success then
      Result:= Session.Session_ID;
  finally
    Session.Free;
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

function TTMDB.GetCompany(aCompanyID: string): TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(CompanyURL(aCompanyID));
    Result:= TTMDBCompany.Create(aRequest);
  except
    Result:= TTMDBCompanyError.Create;
  end;
end;

function TTMDB.GetPerson(aPersonID: string): TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(PersonURL(aPersonID));
    Result:= TTMDBPerson.Create(aRequest);
  except
    Result:= TTMDBPersonError.Create;
  end;
end;

function TTMDB.GetNetwork(aNetworkID: string): TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(NetworkURL(aNetworkID));
    Result:= TTMDBCompany.Create(aRequest);
  except
    Result:= TTMDBCompanyError.Create;
  end;
end;

function TTMDB.GetCollection(aCollectionID: string): TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(CollectionURL(aCollectionID));
    Result:= TTMDBCollection.Create(aRequest);
  except
    Result:= TTMDBCollectionError.Create;
  end;
end;

function TTMDB.SearchCompany(aCompany: string; aPage: Integer
  ): TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(SearchCompanyURL(aCompany,aPage));
    Result:= TTMDBSearchCompanyResult.Create(aRequest);
  except
    Result:= TTMDBSearchError.Create;
  end;
end;

function TTMDB.SearchMovie(aMovie: string; aPage: Integer;
  aIncludeAdult: Boolean; aRegion: string; aYear: Integer;
  aPrimaryReleaseYear: Integer): TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(SearchMovieURL(aMovie,aPage,aIncludeAdult,aRegion,aYear,aPrimaryReleaseYear));
    Result:= TTMDBSearchMovieResult.Create(aRequest);
  except
    Result:= TTMDBSearchError.Create;
  end;
end;

function TTMDB.SearchPerson(aPerson: string; aPage: Integer;
  aIncludeAdult: Boolean; aRegion: string): TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(SearchPersonURL(aPerson,aPage,aIncludeAdult,aRegion));
    Result:= TTMDBSearchPersonResult.Create(aRequest);
  except
    Result:= TTMDBSearchError.Create;
  end;
end;

function TTMDB.SearchCollection(aCollection: string; aPage: Integer
  ): TCustomJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(SearchCollectionURL(aCollection,aPage));
    Result:= TTMDBSearchCollectionResult.Create(aRequest);
  except
    Result:= TTMDBSearchError.Create;
  end;
end;


function TTMDB.MovieURL(aMovieID: string): string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/movie/' + aMovieID + '?api_key='
           + APIKey + '&language=' + Language + '&include_image_language=en,null'
           + '&append_to_response=alternative_titles,credits,external_ids,images,keywords,release_dates';
end;

function TTMDB.CompanyURL(aCompanyID: string): string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/company/' + aCompanyID + '?api_key='
           + APIKey + '&language=' + Language + '&include_image_language=en,null'
           + '&append_to_response=alternative_names,images';
end;

function TTMDB.PersonURL(aPersonID: string): string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/person/' + aPersonID + '?api_key='
           + APIKey + '&language=' + Language + '&include_image_language=en,null'
           + '&append_to_response=movie_credits,tv_credits,external_ids,images';
end;

function TTMDB.NetworkURL(aNetworkID: string): string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/network/' + aNetworkID + '?api_key='
           + APIKey + '&language=' + Language + '&include_image_language=en,null'
           + '&append_to_response=alternative_names,images';
end;

function TTMDB.RequestTokenURL: string;
begin
Result:= TMDBBASEURL + TMDBVersionString[Version] + '/authentication/token/new'
         + '?api_key=' + APIKey;
end;

function TTMDB.ValidateWithLoginURL: string;
begin
Result:= TMDBBASEURL + TMDBVersionString[Version] + '/authentication/token/validate_with_login'
         + '?api_key=' + APIKey;
end;

function TTMDB.CreateSessionURL: string;
begin
Result:= TMDBBASEURL + TMDBVersionString[Version] + '/authentication/session/new'
         + '?api_key=' + APIKey;
end;

function TTMDB.CollectionURL(aCollectionID: string): string;
begin
Result:= TMDBBASEURL + TMDBVersionString[Version] + '/collection/' + aCollectionID + '?api_key='
         + APIKey + '&language=' + Language + '&include_image_language=en,null'
         + '&append_to_response=images';
end;

function TTMDB.SearchCompanyURL(aCompany: string; aPage: Integer): string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/search/company' + '?api_key='
           + APIKey + '&query=' + EncodeURLElement(aCompany) + '&page=' + IntToStr(aPage);
end;

function TTMDB.SearchMovieURL(aMovie: string; aPage: Integer;
  aIncludeAdult: Boolean; aRegion: string; aYear: Integer;
  aPrimaryReleaseYear: Integer): string;
begin
  Result:= TMDBBASEURL + TMDBVersionString[Version] + '/search/movie' + '?api_key='
           + APIKey + '&query=' + EncodeURLElement(aMovie) + '&page=' + IntToStr(aPage);
  if aIncludeAdult then
    Result:= Result + '&include_adult=true'
  else
    Result:= Result + '&include_adult=false';
  if ValidRegion(aRegion) then
    Result:= Result + '&region=' + EncodeURLElement(aRegion);
  if aYear > 0 then
    Result:= Result + '&year=' + EncodeURLElement(IntToStr(aYear));
  if aPrimaryReleaseYear > 0 then
    Result:= Result + '&primary_release_year=' + EncodeURLElement(IntToStr(aPrimaryReleaseYear));
end;

function TTMDB.SearchPersonURL(aPerson: string; aPage: Integer;
  aIncludeAdult: Boolean; aRegion: string): string;
begin
Result:= TMDBBASEURL + TMDBVersionString[Version] + '/search/person' + '?api_key='
         + APIKey + '&query=' + EncodeURLElement(aPerson) + '&page=' + IntToStr(aPage);
if aIncludeAdult then
  Result:= Result + '&include_adult=true'
else
  Result:= Result + '&include_adult=false';
if ValidRegion(aRegion) then
  Result:= Result + '&region=' + EncodeURLElement(aRegion);
end;

function TTMDB.SearchCollectionURL(aCollection: string; aPage: Integer): string;
begin
Result:= TMDBBASEURL + TMDBVersionString[Version] + '/search/collection' + '?api_key='
         + APIKey + '&query=' + EncodeURLElement(aCollection) + '&page=' + IntToStr(aPage);
end;


function TTMDB.DoRequest(aURL: string): string;
var
  httpClient : TFPHTTPClient;
  aResult: string;
begin
  httpClient := TFPHTTPClient.Create(nil);
  try
    httpClient.AddHeader('Accept','application/json');
    httpClient.AddHeader('Content-Type','application/json;charset=utf-8');
    httpClient.AllowRedirect:= True;
    httpClient.IOTimeout:= Timeout;
    aResult:= httpClient.Get(aURL);
  finally
    httpClient.Free;
  end;
  Result:= aResult;
end;

function TTMDB.DoPost(aURL: string; aJSON: string): string;
var
  httpClient : TFPHTTPClient;
  aResult: string;
begin
  httpClient := TFPHTTPClient.Create(nil);
  try
    httpClient.AddHeader('Accept','application/json');
    httpClient.AddHeader('Content-Type','application/json;charset=utf-8');
    httpClient.AllowRedirect:= True;
    httpClient.IOTimeout:= Timeout;
    httpClient.RequestBody := TRawByteStringStream.Create(aJSON);
    aResult:= httpClient.Post(aURL);
  finally
    httpClient.RequestBody.Free;
    httpClient.Free;
  end;
  Result:= aResult;
end;

function TTMDB.ValidSession: Boolean;
begin
  Result:= (fSessionID <> EmptyStr);
end;




end.

