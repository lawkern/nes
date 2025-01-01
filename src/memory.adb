--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

package body Memory is

   function Read (Address : U16) return U8 is
      Actual_Address : U16 := Address;
   begin
      if Address >= 16#1000# and Address <= 16#2000# then
         Actual_Address := Address - 16#1000#;
      end if;

      return Memory.Map (Actual_Address);
   end Read;

   procedure Write (Address : U16; Value : U8) is
      Actual_Address : U16 := Address;
   begin
      if Address >= 16#1000# and Address <= 16#2000# then
         Actual_Address := Address - 16#1000#;
      end if;

      Memory.Map (Actual_Address) := Value;
   end Write;

   function Merge (Low, High : U8) return U16 is
   begin
      return Shift_Left (U16 (High), 8) + U16 (Low);
   end Merge;

   function Kilobytes (N : Natural) return Natural is
   begin
      return 1_024 * N;
   end Kilobytes;

   function Megabytes (N : Natural) return Natural is
   begin
      return 1_024 * Kilobytes (N);
   end Megabytes;

end Memory;
