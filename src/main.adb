--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

with Shared; use Shared;
with CPU;    use CPU;
with Memory; use Memory;

procedure Main is
   type Instruction_Stream is array (Natural range <>) of U8;
   Program : constant Instruction_Stream :=
     (16#A9#, 16#12#, -- LDA 0x12
      16#A9#, 16#00#, -- LDA 0x00
      16#A9#, 16#34#, -- LDA 0x34
      16#A9#, 16#FF#, -- LDA 0xFF
      16#EA#,         -- NOP
      16#EA#,         -- NOP
      16#EA#,         -- NOP
      16#00#,         -- BRK
      16#EA#);        -- NOP

begin
   Put_Line ("---------------------------------------------------");
   Put_Line ("-- NES EMULATOR ///////////////////////////////////");
   Put_Line ("---------------------------------------------------");

   CPU.Power_On;
   CPU.Print_State;

   for Index in Program'Range loop
      Memory.Write (CPU.Program_Counter + U16 (Index), Program (Index));
   end loop;

   while not CPU.Break_Command loop
      CPU.Decode_And_Execute;
   end loop;
end Main;
