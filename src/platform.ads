--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

-- NOTE: Platform acts as the interface between the operating system and the
-- actual program, providing a window to draw into, user input handling, etc.

with Ada.Real_Time; use Ada.Real_Time;

with Shared; use Shared;

package Platform is
   Frames_Per_Second  : Natural;
   Frame_Time_Elapsed : Time_Span;

   Running    : Boolean := False;
   Backbuffer : Texture;

   procedure Log (Message : String);
   procedure Initialize (Width, Height : Integer; Title : String);
   procedure Begin_Frame;
   procedure Render;
   procedure End_Frame;
end Platform;
