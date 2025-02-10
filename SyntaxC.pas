unit SyntaxC;

interface

uses
  SysUtils, Classes;

const
  CKeywords: array[0..32] of string = (
    'auto', 'break', 'case', 'char', 'const', 'continue', 'default', 'do', 'double', 
    'else', 'enum', 'extern', 'for', 'goto', 'if', 'inline', 'long', 'register', 'restrict', 
    'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch', 'typedef', 
    'union', 'unsigned', 'void', 'volatile', 'while'
  );

  CDataTypes: array[0..5] of string = (
    'int', 'char', 'float', 'double', 'long', 'void'
  );

procedure HighlightCSyntax(const Line: string; var HighlightedLine: string);

implementation

procedure HighlightCSyntax(const Line: string; var HighlightedLine: string);
var
  Word: string;
  i: Integer;
begin
  HighlightedLine := '';
  Word := '';
  
  for i := 1 to Length(Line) do
  begin
    if Line[i] in ['a'..'z', 'A'..'Z', '_'] then
      Word := Word + Line[i]
    else
    begin
      // Check if the word is a keyword
      if (Word in CKeywords) then
        HighlightedLine := HighlightedLine + '[Keyword]' + Word + '[/Keyword]'
      else if (Word in CDataTypes) then
        HighlightedLine := HighlightedLine + '[DataType]' + Word + '[/DataType]'
      else
        HighlightedLine := HighlightedLine + Word;

      // Reset the word and add the non-word character
      Word := '';
      HighlightedLine := HighlightedLine + Line[i];
    end;
  end;

  // Final check for any word left at the end
  if Word <> '' then
  begin
    if (Word in CKeywords) then
      HighlightedLine := HighlightedLine + '[Keyword]' + Word + '[/Keyword]'
    else if (Word in CDataTypes) then
      HighlightedLine := HighlightedLine + '[DataType]' + Word + '[/DataType]'
    else
      HighlightedLine := HighlightedLine + Word;
  end;
end;

end.
