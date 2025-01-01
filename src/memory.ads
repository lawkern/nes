--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Shared; use Shared;

package Memory is

   type Address_Space is array (U16) of U8;
   Map : Address_Space;

   -- NOTE: The 256-byte stack grows downward from 01FF to 0100, indexed by the
   -- 8-bit Stack_Pointer register.
   Stack_Base : constant U16 := 16#01FF#;
   Stack_Top  : constant U16 := 16#0100#;

   function Read (Address : U16) return U8;
   procedure Write (Address : U16; Value : U8);
   function Merge (Low, High : U8) return U16;

end Memory;
