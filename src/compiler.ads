with GNATCOLL.Strings_Impl, Ada.Wide_Wide_Characters.Handling, Ada.Characters,Ada.Containers.Doubly_Linked_Lists;    
use Ada.Wide_Wide_Characters.Handling;

package Compiler is

  package aString is new GNATCOLL.Strings_Impl.Strings
   (SSize            => GNATCOLL.Strings_Impl.Optimal_String_Size,
    Character_Type   => Wide_Wide_Character,
    Character_String => Wide_Wide_String);

  use Compiler.aString;
  
  type Token is
   (Keyword_Comment, Keyword_Complex_Comment_Start,
    Keyword_Simple_Comment_Start, Keyword_Complex_Comment_End, Keyword_Web,
    Keyword_Block_Start, Keyword_Block_Close, Keyword_Component,
    Keyword_Identifier_Opening, Keyword_Identifier_Closing, Attribute_Symbol,
    Blockparam_Symbol, Parantesis_Open_Symbol, Parantesis_Close_Symbol,
    Assignment_Symbol, Quotation_Symbol, Keyword_Tag_Start, Keyword_Tag_End,
    Keyword_Tag_Close, Keyword_Tag_Closing_End,Identifier, Token_Integer,
    Token_String, End_of_input, No_Token,
    
    Empty_Char_Error, Invalid_Escape_Error, Multi_Char_Error,
    EOF_Comment_Error, EOF_String_Error, EOL_String_Error, Invalid_Char_Error,
    Invalid_Num_Error);

  subtype Keyword is Token range Keyword_Comment .. Keyword_Tag_Close;
  subtype Error is Token range Empty_Char_Error .. Invalid_Num_Error;

  type Node is record
    TheToken  : Token   := No_Token;
    TheName   : XString := To_XString ("");
    TheValue  : XString := To_XString ("");
    Closing   : Boolean := false;
    Block     : Boolean := false;
    BlockClosing : Boolean := false;
    Component : Boolean := false;
  end record;

  package Node_List is new Ada.Containers.Doubly_Linked_Lists
   (Element_Type => Node);
 
private

end Compiler;