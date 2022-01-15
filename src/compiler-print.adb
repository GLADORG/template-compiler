pragma Ada_2012;
package body Compiler.Print is
  
  -----------
  -- Debug --
  -----------
  procedure Debug (S: Stream_Access; AST : List) is
  begin
   for E of AST loop
     if E.TheValue.Length > 0 then
       Wide_Wide_String'Write (S, E.TheToken'Wide_Wide_Image);
        Character'Write (S, ASCII.HT);
        XString'Write (S, E.TheValue);
        Character'Write (S, ASCII.LF);
     else
       Wide_Wide_String'Write (S, E.TheToken'Wide_Wide_Image);
       Character'Write (S, ASCII.LF);
      end if;
    end loop;
  end Debug;

  procedure Recreate(S: Stream_Access; AST : List) is 
  begin
    for E of AST loop
      case E.TheToken is
      when Keyword_Web => XString'Write(S,E.TheValue);
      when Keyword_Identifier_Opening => 
        String'Write(S,"{{");
        if E.Block then 
          String'Write(S,"#");
        end if;
        if E.BlockClosing then
          String'Write (S, "/");
        end if;
        if E.TheName.Length > 0 then
          XString'Write(S, E.TheName);
        end if;  
      
      when Keyword_Tag_Start =>
          String'Write (S, "<");

          if E.BlockClosing then
            String'Write (S, "/");
          end if;
          if E.TheName.Length > 0 then
            XString'Write (S, E.TheName);
          end if;
  
      when Keyword_Tag_End =>
      
          if E.BlockClosing then
            String'Write (S, "/");
          end if;
          String'Write (S, ">");

      when Keyword_Identifier_Closing =>
        String'Write (S, "}}");
          
      when Attribute_Symbol => 
        String'Write (S, "@");
          
      when Blockparam_Symbol => 
        String'Write (S, "|");
        
      when Parantesis_Open_Symbol =>
        String'Write (S, "(");

      when Parantesis_Close_Symbol =>
        String'Write (S, ")");
        
      when Assignment_Symbol =>
        String'Write (S, "=");
      
      when Quotation_Symbol =>
         Character'Write (S, ASCII.Quotation);

      when Identifier =>
        XString'Write (S, E.TheValue);

      when others =>
        XString'Write (S, E.TheValue);
      end case;
    end loop;
    
  end Recreate;
  
end Compiler.Print;