unit SyntaxPascal;

interface

uses
  SysUtils, Classes;

const
  PascalKeywords: array[0..24] of string = (
    'begin', 'end', 'if', 'then', 'else', 'while', 'do', 'for', 'to', 'downto', 
    'repeat', 'until', 'case', 'of', 'var', 'const', 'type', 'function', 'procedure', 
    'record', 'object', 'class', 'interface', 'implementation', 'uses'
  );

  PascalDataTypes: array[0..5] of string = (
    'integer', 'real', 'char', 'boolean', 'string', 'array'
  );

procedure HighlightPascalSyntax(const Line: string; var HighlightedLine: string);

implementation

procedure HighlightPascalSyntax(const Line: string; var HighlightedLine: string);
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
      if (Word in PascalKeywords) then
        HighlightedLine := HighlightedLine + '[Keyword]' + Word + '[/Keyword]'
      else if (Word in PascalDataTypes) then
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
    if (Word in PascalKeywords) then
      HighlightedLine := HighlightedLine + '[Keyword]' + Word + '[/Keyword]'
    else if (Word in PascalDataTypes) then
      HighlightedLine := HighlightedLine + '[DataType]' + Word + '[/DataType]'
    else
      HighlightedLine := HighlightedLine + Word;
  end;
end;

end.
