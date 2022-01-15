with Compiler, Ada.Wide_Wide_Text_IO.Text_Streams;
use Compiler, Ada.Wide_Wide_Text_IO.Text_Streams;

package Compiler.Print is
  
  use Compiler.Node_List;
  
  procedure Debug (S : Stream_Access; AST : List);
  
  procedure Recreate (S : Stream_Access; AST : List);

private
  
end Compiler.Print;