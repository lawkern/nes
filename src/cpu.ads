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

   Carry_Flag_Bit        : constant U8 := 2#0000_0001#;
   Zero_Flag_Bit         : constant U8 := 2#0000_0010#;
   Interrupt_Disable_Bit : constant U8 := 2#0000_0100#;
   Decimal_Mode_Bit      : constant U8 := 2#0000_1000#;
   Break_Command_Bit     : constant U8 := 2#0001_0000#;
   Overflow_Flag_Bit     : constant U8 := 2#0100_0000#;
   Negative_Flag_Bit     : constant U8 := 2#1000_0000#;

   Carry_Flag_Mask        : constant U8 := not Carry_Flag_Bit;
   Zero_Flag_Mask         : constant U8 := not Zero_Flag_Bit;
   Interrupt_Disable_Mask : constant U8 := not Interrupt_Disable_Bit;
   Decimal_Mode_Mask      : constant U8 := not Decimal_Mode_Bit;
   Break_Command_Mask     : constant U8 := not Break_Command_Bit;
   Overflow_Flag_Mask     : constant U8 := not Overflow_Flag_Bit;
   Negative_Flag_Mask     : constant U8 := not Negative_Flag_Bit;

   type Memory_Map is array (U16) of U8;
   Memory : Memory_Map;

   -- NOTE: The 256-byte stack grows downward from 01FF to 0100, indexed by the
   -- 8-bit Stack_Pointer register.
   Stack_Base : constant U16 := 16#01FF#;
   Stack_Top  : constant U16 := 16#0100#;

   procedure Print_Registers;
   function Decode_And_Execute return Integer;

end CPU;
