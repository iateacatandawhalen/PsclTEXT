unit SyntaxC;

interface

uses
  SysUtils, Classes;

const
  CKeywords: array[0..39] of string = (
    'auto', 'break', 'case', 'char', 'const', 'continue', 'default', 'do', 'double',
    'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long',
    'register', 'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct',
    'switch', 'typedef', 'union', 'unsigned', 'void', 'volatile', 'while', 'restrict', 
    'alignas', 'alignof', 'atomic', 'complex', 'generic'
  );

  CDataTypes: array[0..10] of string = (
    'int', 'char', 'float', 'double', 'void', 'long', 'short', 'signed', 'unsigned',
    'size_t', 'ptrdiff_t'
  );

  COperators: array[0..29] of string = (
    '+', '-', '*', '/', '%', '=', '==', '!=', '>', '<', '>=', '<=', '&&', '||', '!',
    '++', '--', '&', '|', '^', '~', '<<', '>>', '->', '?', ':', ',', '+=', '-=', '*=',
    '/=', '%=', '&&='
  );

  CPreprocessors: array[0..7] of string = (
    '#define', '#include', '#if', '#ifdef', '#endif', '#else', '#undef', '#pragma'
  );

  CTypes: array[0..6] of string = (
    'struct', 'union', 'enum', 'typedef', 'static', 'extern', 'inline'
  );

  CFunctions: array[0..12] of string = (
    'printf', 'scanf', 'main', 'malloc', 'free', 'exit', 'fopen', 'fclose', 'fread', 
    'fwrite', 'memcpy', 'memset'
  );

  CMacros: array[0..2] of string = (
    'NULL', 'MAX', 'MIN'
  );

  CComments: array[0..1] of string = ('//', '/*');

implementation

procedure HighlightSyntax(const Line: string; var HighlightedLine: string);
var
  i: Integer;
  Word: string;
  IsComment: Boolean;
  IsString: Boolean;
  IsKeyword: Boolean;
  IsPreprocessor: Boolean;
begin
  HighlightedLine := '';
  Word := '';
  IsComment := False;
  IsString := False;
  IsKeyword := False;
  IsPreprocessor := False;

  for i := 1 to Length(Line) do
  begin
    if IsString then
    begin
      // String literal
      HighlightedLine := HighlightedLine + Line[i];
      if Line[i] = '"' then
        IsString := False;
    end
    else if IsComment then
    begin
      // Comment (single-line or block)
      HighlightedLine := HighlightedLine + Line[i];
      if (Line[i] = '*') and (i < Length(Line)) and (Line[i+1] = '/') then
      begin
        HighlightedLine := HighlightedLine + Line[i+1];
        IsComment := False;
        Inc(i); // Skip the next character
      end;
    end
    else if (Line[i] = '/') and (i < Length(Line)) and (Line[i+1] = '/') then
    begin
      // Start of single-line comment
      IsComment := True;
      HighlightedLine := HighlightedLine + Line[i] + Line[i+1];
      Inc(i); // Skip the next character
    end
    else if (Line[i] = '/') and (i < Length(Line)) and (Line[i+1] = '*') then
    begin
      // Start of block comment
      IsComment := True;
      HighlightedLine := HighlightedLine + Line[i] + Line[i+1];
      Inc(i); // Skip the next character
    end
    else if Line[i] = '"' then
    begin
      // Start of string literal
      IsString := True;
      HighlightedLine := HighlightedLine + Line[i];
    end
    else if (Line[i] in ['a'..'z', 'A'..'Z', '_']) then
    begin
      // Collect word
      Word := Word + Line[i];
    end
    else if (Line[i] = '#') then
    begin
      // Preprocessor directive
      IsPreprocessor := True;
      HighlightedLine := HighlightedLine + Line[i];
    end
    else
    begin
      // Non-word character
      if (Word <> '') then
      begin
        // Check if the word is a keyword or data type
        IsKeyword := False;
        IsPreprocessor := False;

        if (Word in CKeywords) then
        begin
          HighlightedLine := HighlightedLine + '[Keyword]' + Word + '[/Keyword]';
        end
        else if (Word in CDataTypes) then
        begin
          HighlightedLine := HighlightedLine + '[DataType]' + Word + '[/DataType]';
        end
        else if (Word in COperators) then
        begin
          HighlightedLine := HighlightedLine + '[Operator]' + Word + '[/Operator]';
        end
        else if (Word in CPreprocessors) then
        begin
          HighlightedLine := HighlightedLine + '[Preprocessor]' + Word + '[/Preprocessor]';
        end
        else if (Word in CTypes) then
        begin
          HighlightedLine := HighlightedLine + '[Type]' + Word + '[/Type]';
        end
        else if (Word in CFunctions) then
        begin
          HighlightedLine := HighlightedLine + '[Function]' + Word + '[/Function]';
        end
        else if (Word in CMacros) then
        begin
          HighlightedLine := HighlightedLine + '[Macro]' + Word + '[/Macro]';
        end
        else
          HighlightedLine := HighlightedLine + Word;

        Word := '';
      end;

      // Add the current character (operators, punctuation, etc.)
      HighlightedLine := HighlightedLine + Line[i];
    end;
  end;

  // Check for any remaining word at the end of the line
  if Word <> '' then
  begin
    if Word in CKeywords then
      HighlightedLine := HighlightedLine + '[Keyword]' + Word + '[/Keyword]'
    else
      HighlightedLine := HighlightedLine + Word;
  end;
end;

end.
