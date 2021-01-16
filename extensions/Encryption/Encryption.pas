unit Encryption;

interface

uses SysUtils ,Classes, IdGlobal, IdCoderMIME, IdHash, IdHashMessageDigest
,System.Hash;

type

  THash = class(TObject)
  private

  public
    class function EncodeMd5(const value: string): string;
    class function EncodeBase64(const value: string): string;
    class function DecodeBase64(const value: string): string;
    class function EncodeSha(const value: string; TypeOfSha: integer): string;
  end;





implementation



{ THash }

class function THash.DecodeBase64(const value: string): string;
begin
  Result := TIdDecoderMIME.DecodeString(value, IndyTextEncoding_UTF8);
end;

class function THash.EncodeBase64(const value: string): string;
begin
  Result := TIdEncoderMIME.EncodeBytes(ToBytes(value));
end;

class function THash.EncodeMd5(const value: string): string;
var
    hashMessageDigest5 : TIdHashMessageDigest5;
begin
  hashMessageDigest5 := nil;
  try
      hashMessageDigest5 := TIdHashMessageDigest5.Create;
      Result := IndyLowerCase ( hashMessageDigest5.HashStringAsHex ( value ) );
  finally
      hashMessageDigest5.Free;
  end;
end;

class function THash.EncodeSha(const value: string; TypeOfSha: integer): string;
begin
  case TypeOfSha of
    224: Result := THashSHA2.GetHashString(value, THashSHA2.TSHA2Version.SHA224).ToUpper;
    256: Result := THashSHA2.GetHashString(value, THashSHA2.TSHA2Version.SHA256).ToUpper;
    384: Result := THashSHA2.GetHashString(value, THashSHA2.TSHA2Version.SHA384).ToUpper;
    512: Result := THashSHA2.GetHashString(value, THashSHA2.TSHA2Version.SHA512).ToUpper;
    512224: Result := THashSHA2.GetHashString(value, THashSHA2.TSHA2Version.SHA512_224).ToUpper;
    512256: Result := THashSHA2.GetHashString(value, THashSHA2.TSHA2Version.SHA512_256).ToUpper;
  end;


end;

end.
