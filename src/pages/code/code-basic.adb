with Ada.Text_IO;

procedure Main is
   type GUID is new String (1 .. 32)
     with Dynamic_Predicate =>
       (for all C of GUID => C in '0' .. '9' | 'a' .. 'f');

   ID_1 : constant GUID := "030000004c050000cc09000011810000";
begin
   Ada.Text_IO.Put_Line ("Reading from device " & String (ID_1) & "...");
end Main;
