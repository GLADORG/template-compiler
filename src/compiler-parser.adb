pragma Ada_2012;
with Brackelib.Stacks;
with Ada.Characters.Conversions;

package body Compiler.Parser is

  type State is
   (State_Start, State_Inside_Handlebars, State_Inside_Component_Declaration,
    State_Inside_Complex_Comment, State_Inside_Simple_Comment);

   package State_Stacks is new Brackelib.Stacks (T => State);

  Parse_Error : exception;

  procedure Raise_Error (TheNode : Node) is

    Message : constant Wide_Wide_String :=
     TheNode.TheToken'Wide_Wide_Image & " " & To_String (TheNode.TheValue);
    Message2 : constant String :=
     Ada.Characters.Conversions.To_String (Message);
  begin
    raise Parse_Error with Message2;
  end Raise_Error;

  -----------
  -- Parse --
  -----------
  function Parse (AST_IN : List) return List is

    State_Stack   : State_Stacks.Stack;
    Current_State : State := State_Start;
    Current_Node : Node;
    AST_Out       : List;

    procedure Restore_State is
      Old_State : constant State := Current_State;
    begin
      if State_Stacks.Is_Empty (State_Stack) then
        Current_State := State_Start;
      else
        Current_State := State_Stacks.Pop (State_Stack);
        if Old_State = Current_State then
          Current_State := State_Stacks.Pop (State_Stack);
        else
          null; --Raise_Error();
        end if;

      end if;
    exception
         when State_Stacks.Stack_Empty =>
            Current_State := State_Start;
    end Restore_State;

    Inside_Comment_And_Not_Closing : Boolean := false;
  begin
    for E of AST_In loop

      if Current_State = State_Inside_Complex_Comment and then
          E.TheToken = Keyword_Complex_Comment_End then
          Restore_State;
      elsif Current_State = State_Inside_Simple_Comment
       and then E.TheToken = Keyword_Identifier_Closing then
          Restore_State;
      else
          case E.TheToken is

          when Keyword_Web =>
            AST_Out.Append (E);

          when Keyword_Identifier_Opening =>
            AST_Out.Append (E);
            Current_Node            := E;
            Current_Node.Component := False;
            State_Stacks.Push (State_Stack, State_Inside_Handlebars);
            Current_State := State_Inside_Handlebars;

          when Keyword_Tag_Start =>
            AST_Out.Append (E);
            State_Stacks.Push (State_Stack, State_Inside_Component_Declaration);
            Current_Node            := E;
            Current_Node.Component := True;
            Current_State          := State_Inside_Component_Declaration;

          when Keyword_Complex_Comment_Start =>

            if Current_State = State_Inside_Handlebars then
              AST_Out.Delete_Last;
              State_Stacks.Push (State_Stack, State_Inside_Complex_Comment);
              Current_State := State_Inside_Complex_Comment;

            else
              -- error
              null;

            end if;

          when Keyword_Simple_Comment_Start =>

            if Current_State = State_Inside_Handlebars then
              AST_Out.Delete_Last;
              State_Stacks.Push (State_Stack, State_Inside_Simple_Comment);
              Current_State := State_Inside_Simple_Comment;

            else
              -- error
              null;

            end if;

          when Attribute_Symbol =>

            AST_Out.Append (E);

          when Assignment_Symbol =>

            AST_Out.Append (E);

          when Quotation_Symbol =>

            AST_Out.Append (E);

          when Blockparam_Symbol =>

            AST_Out.Append (E);

          when Identifier =>

            if Current_Node.TheToken /= No_Token and then Current_Node.TheName.Length = 0 then
              Current_Node.TheName := E.TheValue;
            else
              AST_Out.Append (E);
            end if;

          when Keyword_Block_Start =>
            if Current_Node.TheToken /= No_Token then
              Current_Node.Block := true;
            end if;

          when Keyword_Identifier_Closing =>

            if Current_Node.TheToken /= No_Token then

              if not Current_Node.Block then
                -- not a block helper - there is no need to remember state
                Restore_State;
              end if;

            else
              -- error
              null;
            end if;
            Current_Node.TheToken := No_Token;
            AST_Out.Append (E);

          when Keyword_Tag_Closing_End =>

            if Current_Node.TheToken /= No_Token then

              -- Tag is closed - there is no need to remember state
              Restore_State;

            else
              -- error
              null;
            end if;

            AST_Out.Append (E);

          when Keyword_Tag_Close =>
            if Current_Node.TheToken /= No_Token then
              Current_Node.Closing := True;
            else
              -- error
              null;
            end if;


          when Keyword_Tag_End =>
            if Current_Node.TheToken /= No_Token then
              if not Current_Node.Component then
                -- error
                null;
              end if;
            end if;
            Current_Node.TheToken := No_Token;
            AST_Out.Append (E);

          when KEYWORD_BLOCK_CLOSE =>
            if Current_Node.TheToken /= No_Token then
              Current_Node.BlockClosing := True;
            else
              -- error
              null;
            end if;

          when KEYWORD_COMPLEX_COMMENT_END => null;
          when Keyword_Comment => null;
          when others =>
            Raise_Error (E);
          end case;
      end if;
    end loop;

    return AST_Out;
  end Parse;

end Compiler.Parser;
