unit SyntaxPython;

interface

uses
  SysUtils, Classes;

const
  PythonKeywords: array[0..33] of string = (
    'False', 'None', 'True', 'and', 'as', 'assert', 'async', 'await', 'break', 'class',
    'continue', 'def', 'del', 'elif', 'else', 'except', 'finally', 'for', 'from', 'global',
    'if', 'import', 'in', 'is', 'lambda', 'nonlocal', 'not', 'or', 'pass', 'raise', 'return',
    'try', 'while', 'with', 'yield'
  );

  PythonDataTypes: array[0..4] of string = (
    'int', 'str', 'float', 'bool', 'list'
  );

procedure HighlightPythonSyntax(const Line: string; var HighlightedLine: string);

implementation

procedure HighlightPythonSyntax(const Line: string; var HighlightedLine: string);
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
      if (Word in PythonKeywords) then
        HighlightedLine := HighlightedLine + '[Keyword]' + Word + '[/Keyword]'
      else if (Word in PythonDataTypes) then
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
    if (Word in PythonKeywords) then
      HighlightedLine := HighlightedLine + '[Keyword]' + Word + '[/Keyword]'
    else if (Word in PythonDataTypes) then
      HighlightedLine := HighlightedLine + '[DataType]' + Word + '[/DataType]'
    else
      HighlightedLine := HighlightedLine + Word;
  end;
end;

end.

