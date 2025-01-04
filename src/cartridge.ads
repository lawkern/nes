--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;
with Ada.Sequential_IO;
with Ada.Direct_IO;

with Shared; use Shared;
with CPU;    use CPU;

package Cartridge is
   type Cartridge_Identifer is array (1 .. 4) of Character;

   -- NOTE: The MS-DOS end-of-file (ASCII SUB) is used to terminate the
   -- cartridge header identifier
   EOF : constant Character := Character'Val (16#1A#);

   type Cartridge_Header is record
      Identifier : Cartridge_Identifer; -- 0-3

      PRG_ROM_Size_Low : U8; -- 4
      CHR_ROM_Size_Low : U8; -- 5
      Flags6           : U8; -- 6
      Flags7           : U8; -- 7

      Mapper_Submapper      : U8; -- 8
      PRG_CHR_ROM_Size_High : U8; -- 9
      PRG_RAM_EEPROM_Size   : U8; -- 10
      CHR_RAM_Size          : U8; -- 11

      CPU_PPU_Timing           : U8; -- 12
      System_Console_Type      : U8; -- 13
      Misc_ROM_Count           : U8; -- 14
      Default_Expansion_Device : U8; -- 15
   end record;
   for Cartridge_Header'Alignment use 1;
   for Cartridge_Header'Size use 128;

   Invalid_Format : exception;

   procedure Load (Path : String);
   procedure Load_Test_Program;
end Cartridge;
