--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Shared;    use Shared;
with CPU;       use CPU;
with Cartridge; use Cartridge;

procedure Main is
begin
   Put_Line ("---------------------------------------------------");
   Put_Line ("-- NES EMULATOR ///////////////////////////////////");
   Put_Line ("---------------------------------------------------");

   if Argument_Count = 1 then
      Cartridge.Load (Argument (1));
   else
      Cartridge.Load_Test_Program;
   end if;

   CPU.Power_On;
   while not CPU.Break_Command loop
      CPU.Decode_And_Execute;
   end loop;
end Main;
