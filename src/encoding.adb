pragma Ada_2012;

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings, Ada.Strings.UTF_Encoding;
use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings;

package body Encoding is

   ------------
   -- Encode --
   ------------
   function Encode (Text : String) return Wide_Wide_String is 
    
    Text_String : constant Ada.Strings.UTF_Encoding.UTF_8_String := Text;
  begin
    return Decode (Text_String);
  end Encode;

end Encoding;
