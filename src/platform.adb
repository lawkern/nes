--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

-- NOTE: Right now all platform support is implemented by SDL3. OS-specific
-- implementations may be added as needed in the future.

with Ada.Text_IO;
with System;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces.C;   use Interfaces.C;

with SDL3; use SDL3;

package body Platform is
   Window   : SDL3.Window;
   Renderer : SDL3.Renderer;
   Texture  : SDL3.Texture;

   Frame_Count : Natural := 0;
   Include_Log : Boolean := False;

   Start_Time, Next_Frame_Time, Prev_Frame_Time : Time;

   ----------------------------------------------------------------------------
   procedure Log (Message : String) is
   begin
      if Include_Log then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Log;

   ----------------------------------------------------------------------------
   procedure Initialize (Width, Height : Integer; Title : String) is
      Window_Width  : Integer           := Width * 2;
      Window_Height : Integer           := Height * 2;
      Window_Flags  : SDL3.Window_Flags := SDL3.Window_Resizable;

      Use_High_DPI : Boolean := False;
   begin
      SDL3.Init (Flags => SDL3.Init_Video);

      Backbuffer.Width  := Width;
      Backbuffer.Height := Height;
      Backbuffer.Pixels := new Pixel_Buffer (0 .. (Width * Height) - 1);

      if Use_High_DPI then
         Window_Width  := Window_Width / 2;
         Window_Height := Window_Height / 2;
         Window_Flags  := Window_Flags or SDL3.Window_High_Pixel_Density;
      end if;

      SDL3.Create_Window_And_Renderer
        (Title => Title, W => Window_Width, H => Window_Height, Flags => Window_Flags,
         Window => Window, Renderer => Renderer);

      SDL3.Set_Render_V_Sync (Renderer, 1);
      SDL3.Set_Render_Logical_Presentation
        (Renderer, Width, Height, Logical_Presentation_Integer_Scale);

      Texture := SDL3.Create_Texture (Renderer, Width, Height);
      SDL3.Set_Texture_Scale_Mode (Texture, Scale_Mode_Nearest);

      Frames_Per_Second  := 60;
      Frame_Time_Elapsed := Microseconds (1_000_000) / Frames_Per_Second;

      Start_Time      := Clock;
      Prev_Frame_Time := Start_Time;
      Next_Frame_Time := Start_Time + Frame_Time_Elapsed;

      Running := True;
   exception
      when E : SDL3.Initialization_Error =>
         Log ("ERROR: SDL3 initialzation failed.");
         Log (Exception_Information (E));
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Begin_Frame is
      Event : SDL3.Event;
   begin
      while SDL3.Poll_Event (Event) loop
         case Event.Basic.Kind is
            when SDL3.Event_Quit =>
               Running := False;
               exit;

            when SDL3.Event_Key_Up | SDL3.Event_Key_Down =>
               declare
                  Pressed  : Boolean := Boolean (Event.Key.Down);
                  Repeated : Boolean := Boolean (Event.Key.Repeat);
               begin
                  if not Repeated then
                     case Event.Key.Key is
                        when Keycode_Escape =>
                           Running := False;
                           exit;

                        when Keycode_F | Keycode_F11 =>
                           if Pressed then
                              declare
                                 Currently_Fullscreen : Boolean;
                              begin
                                 Currently_Fullscreen := (SDL3.Get_Window_Flags (Window) and SDL3.Window_Fullscreen) /= 0;
                                 SDL3.Set_Window_Fullscreen (Window, not Currently_Fullscreen);
                              end;
                           end if;

                        when others =>
                           null;
                     end case;
                  end if;
               end;
            when others =>
               null;
         end case;
      end loop;
   end Begin_Frame;

   ----------------------------------------------------------------------------
   procedure Render is
      Pitch : Integer := Backbuffer.Width * Backbuffer.Pixels'Component_Size / 8;
   begin
      SDL3.Set_Render_Draw_Color (Renderer, R => 32, G => 32, B => 64, A => 255);
      SDL3.Render_Clear (Renderer);

      SDL3.Update_Texture (Texture, Backbuffer.Pixels, Pitch);
      SDL3.Render_Texture (Renderer, Texture);

      SDL3.Render_Present (Renderer);
   end Render;

   ----------------------------------------------------------------------------
   procedure End_Frame is
   begin
      Frame_Count := Frame_Count + 1;

      delay until Next_Frame_Time;

      if (Frame_Count mod (Frames_Per_Second / 2)) = 0 then
         declare
            Frame_Seconds : Float := Float (To_Duration (Next_Frame_Time - Prev_Frame_Time));
            Total_Seconds : Float := Float (To_Duration (Next_Frame_Time - Start_Time));
         begin
            Log ("Frame:" & Frame_Seconds'Image & "sec, Total:" & Total_Seconds'Image & "sec");
         end;
      end if;

      Prev_Frame_Time := Next_Frame_Time;
      Next_Frame_Time := Next_Frame_Time + Frame_Time_Elapsed;

      if Clock > Next_Frame_Time then
         Next_Frame_Time := Clock + Frame_Time_Elapsed;
      end if;
   end End_Frame;
end Platform;
