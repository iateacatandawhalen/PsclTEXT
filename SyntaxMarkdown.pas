unit SyntaxMarkdown;

interface

uses
  SysUtils, Classes;

const
  MarkdownElements: array[0..3] of string = (
    '#', '*', '**', '```'
  );

procedure HighlightMarkdownSyntax(const Line: string; var HighlightedLine: string);

implementation

procedure HighlightMarkdownSyntax(const Line: string; var HighlightedLine: string);
begin
  HighlightedLine := Line; // Simple implementation for Markdown
  if Pos('#', Line) = 1 then
    HighlightedLine := '[Header]' + Line + '[/Header]'
  else if Pos('```', Line) > 0 then
    HighlightedLine := '[CodeBlock]' + Line + '[/CodeBlock]'
  else if Pos('*', Line) > 0 then
    HighlightedLine := '[Italic]' + Line + '[/Italic]';
end;

end.
