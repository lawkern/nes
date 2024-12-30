--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;

package CPU is
   type U8 is new Unsigned_8;
   type U16 is new Unsigned_16;
   type U32 is new Unsigned_32;
   type U64 is new Unsigned_64;

   package U8_IO is new Ada.Text_IO.Modular_IO (U8);
   package U16_IO is new Ada.Text_IO.Modular_IO (U16);

   -- NOTE: Registers.
   Program_Counter  : U16;
   Stack_Pointer    : U8;
   Accumulator      : U8;
   Index_Register_X : U8;
   Index_Register_Y : U8;

   -- NOTE: Processor Status.
   Carry_Flag        : Boolean;
   Zero_Flag         : Boolean;
   Interrupt_Disable : Boolean;
   Decimal_Mode      : Boolean;
   Break_Command     : Boolean;
   Overflow_Flag     : Boolean;
   Negative_Flag     : Boolean;

   type Memory_Map is array (U16) of U8;
   Memory : Memory_Map;

   procedure Print_Registers;
   function Decode_And_Execute return Integer;

end CPU;
