--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Shared; use Shared;

package PPU is
   -- NOTE: Registers:
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
end PPU;
