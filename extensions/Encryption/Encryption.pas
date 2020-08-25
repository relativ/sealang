unit Encryption;

interface

uses SysUtils ,Classes, IdGlobal, IdCoderMIME, IdHash, IdHashMessageDigest;

type

  THash = class(TObject)
  private

  public
    class function EncodeMd5(const value: string): string;
    class function EncodeBase64(const value: string): string;
    class function DecodeBase64(const value: string): string;
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

end.
