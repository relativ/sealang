{$IFNDEF FxtVer}
{$DEFINE FxtVer}
//
// *******************************
// ---
{$UNDEF D3_UP}   // Delphi 3
{$UNDEF D4_UP}   // Delphi 4
{$UNDEF D5_UP}   // Delphi 5
{$UNDEF D6_UP}   // Delphi 6
{$UNDEF D7_UP}   // Delphi 7
{$UNDEF D9_UP}   // BDS 2005
{$UNDEF D10_UP}  // BDS 2006
{$UNDEF D11_UP}  // RAD Studio 2007
{$UNDEF D12_UP}  // RAD Studio 2009
{$UNDEF D14_UP}  // RAD Studio 2010
{$UNDEF D15_UP}  // RAD Studio XE (2011)
{$UNDEF D16_UP}  // RAD Studio XE2 (2012)
{$UNDEF D17_UP}  // RAD Studio XE3 (2013)
{$UNDEF D18_UP}  // RAD Studio XE4 (2013)
{$UNDEF D19_UP}  // RAD Studio XE5 (2013)
{$UNDEF D20_UP}  // RAD Studio XE6 (2014)
{$UNDEF D21_UP}  // RAD Studio XE7 (2014)
{$UNDEF D22_UP}  // RAD Studio XE8 (2015)
{$UNDEF D23_UP}  // RAD Studio DX10 Seattle (2015)
// ---
{.$UNDEF _DEVEL_}
// ---
{$IFNDEF LINUX}
  {$IFNDEF CLR}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}
// ---
//{$IFDEF CONDITIONALEXPRESSIONS}
//  {$IF CompilerVersion >= 14.01} // Delphi 6 Up
//    {$DEFINE D6_UP}
//  {$IFEND}
//{$ENDIF}
// ---
{$UNDEF DELPHI}
{$UNDEF CBUILDER}

{$IFNDEF FPC}
//
// DELPHI
//
  {$IFDEF BCB} // C++Builder
    {$DEFINE CBUILDER}
  {$ELSE}
    {$DEFINE DELPHI}
  {$ENDIF}
// ---
{$IFNDEF VER80}
  {$IFNDEF VER90}
    {$IFNDEF VER93}
      {$DEFINE D3_UP} { Delphi 3.0 or higher }
      {$IFNDEF VER110}
        {$DEFINE D4_UP} { Delphi 4.0 or higher }
        {$IFNDEF VER120}
          {$DEFINE D5_UP}
          {$IFNDEF VER130}
            {$DEFINE D6_UP}   { Delphi 6.0 or higher }
            {$IFNDEF VER140}
              {$DEFINE D7_UP}   { Delphi 7.0 or higher }
              {$IFNDEF VER150}
                {$DEFINE D9_UP}   { BDS 2005 (9) or higher }
                {$IFNDEF VER170}
                  {$DEFINE D10_UP}   { BDS 2006 (10) or higher }
                  {$IFDEF VER185}
                    {$DEFINE D11_UP}   { RAD Studio 2007 (11) or higher }
                  {$ELSE}
                    {$IFNDEF VER180}
                       {$DEFINE D11_UP}   { RAD Studio 2007 (11) or higher }
                       {$DEFINE D12_UP}   { RAD Studio 2009 (12) or higher }
                       {$IFNDEF VER200}
                          {$DEFINE D14_UP}   { RAD Studio 2010 (14) or higher }
                          {$IFNDEF VER210}
                            {$DEFINE D15_UP}   { RAD Studio XE 2011 (15) or higher }
                            {$DEFINE DXE_UP}   { RAD Studio XE or higher }
                            {$IFNDEF VER220}
                              {$DEFINE D16_UP}   { RAD Studio XE2 2012 (16) or higher }
                              {$DEFINE DXE2_UP}  { RAD Studio XE2 or higher }
                              {$IFNDEF VER230}
                                {$DEFINE D17_UP}   { RAD Studio XE3 2013 (17) or higher }
                                {$DEFINE DXE3_UP}  { RAD Studio XE3 or higher }
                                {$IFNDEF VER240}
                                  {$DEFINE D18_UP}
                                  {$DEFINE DXE4_UP}  { RAD Studio XE4 or higher }
                                  {$IFNDEF VER250}
                                    {$DEFINE D19_UP}
                                    {$DEFINE DXE5_UP}  { RAD Studio XE5 or higher }
                                    {$IFNDEF VER260}
                                      {$DEFINE D20_UP}
                                      {$DEFINE DXE6_UP}  { RAD Studio XE6 or higher }
                                      {$IFNDEF VER270}
                                        {$DEFINE D21_UP}
                                        {$DEFINE DXE7_UP}  { RAD Studio XE7 or higher }
{$IFNDEF VER280}
  {$DEFINE D22_UP}
  {$DEFINE DXE8_UP}  { RAD Studio XE8 or higher }
  {$IFNDEF VER290}
    {$DEFINE D23_UP} { RAD Studio DX10 Seattle or higher }
    {$DEFINE DXE10_UP}
    {$IFNDEF VER300}
      {$DEFINE D24_UP} { RAD Studio DX11 or higher }
      {$DEFINE DXE11_UP}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
                                      {$ENDIF}
                                    {$ENDIF}
                                  {$ENDIF}
                                {$ENDIF}
                              {$ENDIF}
                            {$ENDIF}
                          {$ENDIF}
                       {$ENDIF}
                    {$ENDIF}
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
// ---
  {$UNDEF _GENERICS_}
  {$IFDEF D14_UP}
    {$DEFINE _GENERICS_}
  {$ENDIF}
  // ---
  {$UNDEF _UNICODE_}
  {$IFDEF D12_UP}
    {$IFDEF UNICODE}
      {$DEFINE _UNICODE_}
    {$ENDIF}
  {$ENDIF}
  // ---
  {$UNDEF _INLINE_}
  {$IFDEF D9_UP}
    {$DEFINE _INLINE_} { optional }
  {$ENDIF}
  // ---
  {$UNDEF _OVERLOAD_}
  {$IFDEF D4_UP}
    {$DEFINE _OVERLOAD_}
  {$ENDIF}
  // ---
  {$IFNDEF D16_UP} // XE2
    {$DEFINE CPUX86}
    {$DEFINE WIN32}
    {$DEFINE WINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
  // ---
  {$IFDEF D6_UP}
    {$WARN SYMBOL_PLATFORM OFF}    // Suppress .Net warnings
    {$WARN SYMBOL_DEPRECATED OFF}
    {$WARN UNIT_PLATFORM OFF}      // NOT certified for Kylix
    {$WARN UNIT_DEPRECATED OFF}    // NOT certified for Kylix
    {$WRITEABLECONST ON}           // J+
  {$ENDIF}
  // ---
  {$IFDEF D7_UP}
    {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
    {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
    {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
  {$ENDIF}
  //
  {$WARN GARBAGE OFF} // Turns on or off warnings produced when the presence of non-whitespace
  // text is detected past the end of the final 'end.' in the program, unit, library or package
  //
  //{$IFDEF DXE3_UP}
  //{.$IF CompilerVersion >= 24}
  //  {$LEGACYIFEND ON} // Allow old style mixed $endif $ifend
  //{.$IFEND}
  //{$ENDIF}
  // ---
  {$IFDEF CBUILDER} // C++Builder
    {$OBJEXPORTALL ON}
  {$ENDIF}
  // ---
  {$B-}
  {$O-}
  {$D+,L+,Y+}
  {$C-} { Evaluate assertions at runtime }

  {$IFDEF _INLINE_}
    {$IFDEF D12_UP} // 2009
      {.$INLINE AUTO} { optional }
    {$ENDIF}

    {$IFDEF D18_UP} // 2013 XE4 UP
      {$ZEROBASEDSTRINGS OFF}
      //{$IFDEF NEXTGEN}
      //{$IFDEF AUTOREFCOUNT}
      //{$IFDEF WEAKREF}
      //{$IFDEF CPUARM}
    {$ENDIF}
  {$ENDIF}

  {$DEBUGINFO ON}
  {$LOCALSYMBOLS ON}
  {$REFERENCEINFO OFF}

  {$STACKFRAMES ON}

  {$ALIGN 8}
  {$MINENUMSIZE 1}

  {$ASSERTIONS OFF}
  {$BOOLEVAL OFF}
  {$EXTENDEDSYNTAX ON}
  {$IOCHECKS ON}
  {$LONGSTRINGS ON}
  {$OPENSTRINGS ON}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS OFF}
  {$SAFEDIVIDE OFF}
  {$TYPEDADDRESS OFF}
  {$VARSTRINGCHECKS ON}
  {$WRITEABLECONST ON}

  {$IFDEF CPU64BITS} // XE2-XE7 (CPUX64), XE8+ (CPU64BITS)
    {$DEFINE CPUX64}  { !temporary! }
  {$ENDIF}
  {$IFNDEF CPUX64}
    {$DEFINE CPUX86}  { !temporary! }
  {$ENDIF}

{$ELSE FPC}
//
// FPC
//
  {$ifdef CPU64}
    {$DEFINE CPUX64}  { !temporary! }
  {$else}
    {$DEFINE CPUX86}  { !temporary! }
  {$endif}

  {$DEFINE CONDITIONALEXPRESSIONS}

  {$R-}{$Q-}

  {$B-}
  {$O-}

  {.$warnings off}
    //{$MODE DELPHI}   // Warning: Misplaced global compiler switch
    {$H+}
    //{$D+}            // Warning: Misplaced global compiler switch
    {$J+}
  {.$warnings on}

  {$ALIGN 8}
  {$MINENUMSIZE 1}
  {$ASSERTIONS OFF}

{$ENDIF FPC}
// ---
// *******************************
{.$DEFINE _NO_VISUAL_}
//
{$ENDIF !FxtVer}
