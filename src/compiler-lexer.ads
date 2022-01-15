
package Compiler.Lexer is
  
  use Node_List;
  
  subtype Uppercase is Wide_Wide_Character with
         Static_Predicate => Uppercase in 'A' .. 'Z';

  Lexer_Error : exception;
 
  function Lex(input : Wide_Wide_String) return List;
  
  end Compiler.Lexer;