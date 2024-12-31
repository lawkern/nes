--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

with CPU; use CPU;

procedure Main is
   Cycles : Integer := 0;

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
   CPU.Print_Registers;

   for Index in Program'Range loop
      CPU.Memory (CPU.Program_Counter + U16 (Index)) := Program (Index);
   end loop;

   while not CPU.Break_Command loop
      Cycles := Cycles + CPU.Decode_And_Execute;
      CPU.Print_Registers;
   end loop;

   Put_Line ("---------------------------------------------------");
   Put_Line ("TOTAL CYCLES: " & Cycles'Image);
end Main;
