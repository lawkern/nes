--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Shared; use Shared;

package PPU is

   -- NOTE: PPU Registers:
   PPU_CTRL    : U8;
   PPU_MASK    : U8;
   PPU_STATUS  : U8;
   OAM_ADDR    : U8;
   OAM_DATA    : U8;
   PPU_SCROLL  : U8;
   PPU_ADDRESS : U8;
   PPU_DATA    : U8;

   subtype Register_Address is U16 range 16#2000# .. 16#3FFF#;
   procedure Write_Register (Address : PPU.Register_Address; Value : U8);


   -- NOTE: PPU memory
   type Address_Space is array (U14) of U8;
   Memory : Address_Space;

   Pattern_Table0 : constant U14 := 16#0000#;
   Pattern_Table1 : constant U14 := 16#1000#;
   Name_Table0    : constant U14 := 16#2000#;
   Name_Table1    : constant U14 := 16#2400#;
   Name_Table3    : constant U14 := 16#2800#;

   function Read (Address : U14) return U8;
   procedure Write (Address : U14; Value : U8);

   procedure Render_Pattern_Table(Backbuffer : Texture);

end PPU;
