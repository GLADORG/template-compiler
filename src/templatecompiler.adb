with Ada.Wide_Wide_Text_IO, Ada.Wide_Wide_Text_IO.Text_Streams, Ada.Command_Line, Ada.Exceptions, Ada.Calendar, GNATCOLL.Mmap;
use Ada.Exceptions, GNATCOLL.Mmap;

with Compiler.Lexer, Compiler.Parser, Compiler.Print, Encoding;

procedure TemplateCompiler is

  use Compiler.Node_List;
  use Ada.Calendar;
  use Ada.Wide_Wide_Text_IO.Text_Streams;

  Start_Processing, End_Processing : Time;
  How_Long :  Duration;

  S : constant Stream_Access := Stream(Ada.Wide_Wide_Text_IO.Current_Output);
begin
  if Ada.Command_Line.Argument_Count < 1 then
    String'Write(S,"usage: glad [filename]");
    return;
  end if;

  Start_Processing := Clock;

  declare
    Source_File   : Mapped_File := Open_Read (Ada.Command_Line.Argument (1));
    Source_Region : constant Mapped_Region    := Read (Source_File);
    L             : constant Natural          := Natural (Length (Source_File));
    Source        : constant Str_Access       := Data (Source_Region);
    Wide_Source   : constant Wide_Wide_String := Encoding.Encode (Source (1 .. L));
    Lex_AST       : constant List := Compiler.Lexer.Lex (Wide_Source);
    Parse_AST     : constant List := Compiler.Parser.Parse (Lex_AST);
  begin
    Close (Source_File);
    Compiler.Print.Recreate (S, Parse_AST);
  end;

  End_Processing := Clock;
  How_Long       := (End_Processing - Start_Processing) * 1000;

  String'Write(S, "Duration: ");
  Wide_Wide_String'Write(S, How_Long'Wide_Wide_Image);
  String'Write(S, "ms");
  Character'Write(S, ASCII.LF);

exception
  when error : others =>
    String'Write(S, "Error: "  & Exception_Message(error));

end TemplateCompiler;