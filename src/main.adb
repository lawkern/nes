--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Shared;    use Shared;
with CPU;       use CPU;
with PPU;       use PPU;
with Cartridge; use Cartridge;
with Platform;  use Platform;

procedure Main is
   task CPU_Task is
      entry Start;
   end CPU_Task;

   task body CPU_Task is
   begin
      accept Start;

      CPU.Power_On;
      while not CPU.Break_Command loop
         CPU.Decode_And_Execute;
      end loop;
   end CPU_Task;

begin
   Put_Line ("---------------------------------------------------");
   Put_Line ("-- NES EMULATOR ///////////////////////////////////");
   Put_Line ("---------------------------------------------------");

   if Argument_Count = 1 then
      Cartridge.Load (Argument (1));
   else
      Cartridge.Load_Test_Program;
   end if;

   CPU_Task.Start;

   Platform.Initialize (Width => 256, Height => 240, Title => "NES Emulator");
   while Platform.Running loop
      Platform.Process_Input;

      PPU.Render_Clear (Backbuffer, 16#0000_00FF#);
      PPU.Render_Pattern_Table (Backbuffer);

      Platform.Render;
      Platform.Frame_End;
   end loop;

end Main;
