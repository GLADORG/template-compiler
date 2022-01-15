with Ada.Streams, Ada.Characters.Conversions, Ada.Strings, Ada.Streams.Stream_IO;

package body Compiler.Lexer is
  use Ada.Streams.Stream_IO;
  use Ada.Streams;

  use Ada.Characters.Conversions;
  
  function Lex (input : Wide_Wide_String) return List is

    type State is
     (State_Start, State_Identifier, State_Comment,
      State_ComplexComment, State_Component);

    curr_state : State := State_Start;
    curr_char  : Wide_Wide_Character;
    curr_index : Positive:= 1;
    token_text : XString := Null_XString;

    AST : List;
      
    function look_ahead return Wide_Wide_Character is
    
      l_char : constant Wide_Wide_Character := To_Wide_Wide_Character (ASCII.LF);
      index : constant Positive := curr_index + 1;
    begin
      if index > input'Length then
        return l_char;
      else
        return input (index);
      end if;
    end look_ahead;
    
    function look_further_ahead return Wide_Wide_Character is

      l_char : constant Wide_Wide_Character :=
       To_Wide_Wide_Character (ASCII.LF);
      index : constant Positive := curr_index + 2;
    begin
      if index > input'Length then
        return l_char;
      else
        return input (index);
      end if;
    end look_further_ahead;

    procedure next_char is
    begin    
      curr_index := curr_index + 1;
      if curr_index > input'Length then
        raise Stream_IO.End_Error;
      else
        curr_char := input (curr_index);
      end if;
    end next_char;

    procedure Add_Token_To_AST(tok : Token) is
  
      New_Node : constant Node :=
       (TheToken => tok, 
        TheName => To_XString (""),
        TheValue => To_XString (""), 
        others => false);
    begin
      AST.Append(New_Node);
    end Add_Token_To_AST;
  
    procedure Add_Token_To_AST
     (tok   : Token; 
      text : XString; 
      BlockClosing: Boolean := False;
      Block: Boolean := False)
    is

      New_Node : constant Node :=
       (TheToken => tok, 
        TheValue => text,
        TheName => To_XString (""),
        Block => Block,
        BlockClosing => BlockClosing,
        others => false);
    begin
      AST.Append (New_Node);
    end Add_Token_To_AST;
    
    procedure Add_Nonempty_Token_To_AST (tok : Token; text : XString) is
    begin
      if text.Length > 0 and then
        text.Trim (Ada.Strings.Both).Length > 0 then
     
        Add_Token_To_AST (tok, text);
      end if;            
    end Add_Nonempty_Token_To_AST;

    procedure Preserve_Text_And_Add_Token(tok1 : Token; text : XString; tok2 : Token; BlockClosing: Boolean := false; Block: Boolean := false) is
    begin
      Add_Nonempty_Token_To_AST (tok1, text);
      Add_Token_To_AST (tok2, To_XString (""), BlockClosing, Block);
      token_text.Set ("");
    end Preserve_Text_And_Add_Token;

  begin
    curr_char := input(curr_index);

    loop
      case curr_state is
      when State_Start =>
        case curr_char is
        when '<' =>
          if look_ahead = '/' or look_ahead in Uppercase then          
                Preserve_Text_And_Add_Token
                 (Keyword_Web, token_text, Keyword_Tag_Start,
                  look_ahead = '/');
                curr_state := State_Component;
          else
            token_text.Append (curr_char);
          end if;
    
        when '{' =>
          if look_ahead = '{' then
                Preserve_Text_And_Add_Token
                 (Keyword_Web, token_text, Keyword_Identifier_Opening,
                  look_further_ahead = '/', look_further_ahead = '#');
                next_char;
            curr_state := State_Identifier;
          end if;
        when others =>
          token_text.Append (curr_char);
        end case;
        next_char;

      when State_Identifier =>
        case curr_char is
        when '/' | '.' | '-' | '!' | 'a' .. 'z' | 'A' .. 'Z' |
         '0' .. '9' | '_' =>
          token_text.Append (curr_char);
          next_char;
        
        when '#' =>
          Add_Token_To_AST (Keyword_Block_Start);
          next_char;
          
        when '|' =>
          Preserve_Text_And_Add_Token(Identifier,token_text, Blockparam_Symbol);
          next_char;
          
        when '=' =>
          Preserve_Text_And_Add_Token(Identifier,token_text, Assignment_Symbol);
          next_char;
      
        when '@' =>
          Add_Token_To_AST (Attribute_Symbol);
          token_text.Set("");
          next_char;
          
        when  ''' | '"' =>
          Preserve_Text_And_Add_Token(Identifier,token_text, Quotation_Symbol);
          
        when '(' =>
          Preserve_Text_And_Add_Token(Identifier,token_text, Parantesis_Open_Symbol);
          
        when ')' =>
          Preserve_Text_And_Add_Token(Identifier,token_text, Parantesis_Close_Symbol);
          
        when others =>     
          if token_text = "!--" then
            curr_state := State_ComplexComment;
            Add_Token_To_AST (Keyword_Complex_Comment_Start);
            token_text.Set("");
            
          elsif token_text = "!" then
            curr_state := State_Comment;
            Add_Token_To_AST (Keyword_Simple_Comment_Start);
            token_text.Set("");
          else
            Add_Token_To_AST (Identifier, token_text);
            token_text.Set("");
          end if;
          
          if curr_char = '}' then
            if look_ahead = '}' then
              Preserve_Text_And_Add_Token(Identifier,token_text, Keyword_Identifier_Closing);    
              next_char;
              curr_state := State_Start;
            else 
              null;
              -- error..
            end if;
          end if;
          next_char;
        end case;
      when State_Comment =>
        case curr_char is
          when '}' =>
            if look_ahead = '}' then
              Add_Token_To_AST (Keyword_Comment, token_text);
              Add_Token_To_AST (Keyword_Identifier_Closing);

              token_text.Set("");
              next_char;
              curr_state := State_Start;
            end if;
          when others =>
            token_text.Append (curr_char);
        end case;
        next_char;

      when State_ComplexComment =>
        token_text.Append (curr_char);
        
        if curr_char = '}' and then 
        token_text.Tail(4) = ("--}}")
            then
          Add_Token_To_AST (Keyword_Complex_Comment_End);
          --Add_Token_To_AST (Keyword_Identifier_Closing);
          
          token_text.Set("");
          curr_state := State_Start;
        end if;  
        next_char;

      when State_Component =>
    
        case curr_char is
        when '/' =>
          Add_Nonempty_Token_To_AST (Identifier, token_text);
          
          if look_ahead = '>' then
            next_char;
            
            Add_Token_To_AST (Keyword_Tag_Closing_End);
            token_text.Set("");
            curr_state := State_Start;
            
          else
            Add_Token_To_AST (Keyword_Tag_Close);
            token_text.Set("");
          end if;
          
        when '>' =>
          Add_Nonempty_Token_To_AST (Identifier, token_text);
          
          Add_Token_To_AST (Keyword_Tag_End);
          token_text.Set("");
          curr_state := State_Start;

        when '|' =>
          Preserve_Text_And_Add_Token(Identifier,token_text, Blockparam_Symbol);
          
        when '=' =>
          Preserve_Text_And_Add_Token(Identifier,token_text, Assignment_Symbol);
          
        when '@' =>
          Add_Token_To_AST (Attribute_Symbol);
          token_text.Set("");
          
        when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_'  =>
          token_text.Append (curr_char);
        when ''' | '"' =>
          Preserve_Text_And_Add_Token(Identifier,token_text, Quotation_Symbol);
          
        when '(' =>
          Preserve_Text_And_Add_Token(Identifier,token_text, Parantesis_Open_Symbol);
          
        when ')' =>
          Preserve_Text_And_Add_Token(Identifier,token_text, Parantesis_Close_Symbol);         
          
        when '{' =>
          if look_ahead = '{' then
            Preserve_Text_And_Add_Token(Identifier,token_text, Keyword_Identifier_Opening);      
            next_char;
          end if;
          
        when '}' =>
          if look_ahead = '}' then
            Preserve_Text_And_Add_Token(Identifier,token_text, Keyword_Identifier_Closing);
            
            next_char;
          end if; 
          
        when others =>
        declare
          Element : Node := AST.Last_Element;
        begin
                if Element.TheName.Length = 0 then
                  Element.TheName := token_text;
                else
            Add_Token_To_AST (Identifier,token_text);
          end if;
          end;
          token_text.Set("");
        end case;
        next_char;
      end case;
    end loop;
exception
    when error : Stream_IO.End_Error =>
      --IO.Put_Line (End_of_input'Wide_Wide_Image);
      return AST;
    when error : Lexer.Lexer_Error =>
      --IO.Put_Line (Exception_Message (error));
      return AST;
  end Lex;
end Compiler.Lexer;