--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO;
with CPU; use CPU;

procedure Main is
   Cycles : Integer := 0;
begin
   Ada.Text_IO.Put_Line ("----------------");
   Ada.Text_IO.Put_Line ("- NES EMULATOR -");
   Ada.Text_IO.Put_Line ("----------------");

   CPU.Program_Counter := 16#8000#;

   CPU.Memory (CPU.Program_Counter + 0) := 16#A9#;
   CPU.Memory (CPU.Program_Counter + 1) := 16#12#;
   CPU.Memory (CPU.Program_Counter + 2) := 16#A9#;
   CPU.Memory (CPU.Program_Counter + 3) := 16#00#;
   CPU.Memory (CPU.Program_Counter + 4) := 16#A9#;
   CPU.Memory (CPU.Program_Counter + 5) := 16#34#;
   CPU.Memory (CPU.Program_Counter + 4) := 16#A9#;
   CPU.Memory (CPU.Program_Counter + 5) := 16#FF#;
   CPU.Memory (CPU.Program_Counter + 6) := 16#00#;

   CPU.Print_Registers;

   while not CPU.Break_Command loop
      Cycles := Cycles + CPU.Decode_And_Execute;
      CPU.Print_Registers;
   end loop;
end Main;
