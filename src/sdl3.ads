--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

-- NOTE: These are hand-written bindings for SDL3. As additional SDL
-- functionality becomes required, it should be manually added here.

with Interfaces.C; use Interfaces.C;
with System;

with Shared; use Shared;

package SDL3 is
   package C renames Interfaces.C;

   Initialization_Error : exception;

   type Uint8 is mod 2**8 with Convention => C;
   type Uint16 is mod 2**16 with Convention => C;
   type Uint32 is mod 2**32 with Convention => C;
   type Uint64 is mod 2**64 with Convention => C;

   type Init_Flags is new Uint64;
   Init_Audio    : constant Init_Flags := 16#0000_0010#;
   Init_Video    : constant Init_Flags := 16#0000_0020#;
   Init_Joystick : constant Init_Flags := 16#0000_0200#;
   Init_Haptic   : constant Init_Flags := 16#0000_1000#;
   Init_Gamepad  : constant Init_Flags := 16#0000_2000#;
   Init_Events   : constant Init_Flags := 16#0000_4000#;
   Init_Sensor   : constant Init_Flags := 16#0000_8000#;
   Init_Camera   : constant Init_Flags := 16#0001_0000#;

   procedure Init (Flags : Init_Flags);

   type Window_Flags is new Uint64;
   Window_Fullscreen : constant Window_Flags := 16#0000_0000_0000_0001#;
   Window_Opengl : constant Window_Flags := 16#0000_0000_0000_0002#;
   Window_Occluded : constant Window_Flags := 16#0000_0000_0000_0004#;
   Window_Hidden : constant Window_Flags := 16#0000_0000_0000_0008#;
   Window_Borderless : constant Window_Flags := 16#0000_0000_0000_0010#;
   Window_Resizable : constant Window_Flags := 16#0000_0000_0000_0020#;
   Window_Minimized : constant Window_Flags := 16#0000_0000_0000_0040#;
   Window_Maximized : constant Window_Flags := 16#0000_0000_0000_0080#;
   Window_Mouse_Grabbed : constant Window_Flags := 16#0000_0000_0000_0100#;
   Window_Input_Focus : constant Window_Flags := 16#0000_0000_0000_0200#;
   Window_Mouse_Focus : constant Window_Flags := 16#0000_0000_0000_0400#;
   Window_External : constant Window_Flags := 16#0000_0000_0000_0800#;
   Window_Modal : constant Window_Flags := 16#0000_0000_0000_1000#;
   Window_High_Pixel_Density : constant Window_Flags := 16#0000_0000_0000_2000#;
   Window_Mouse_Capture : constant Window_Flags := 16#0000_0000_0000_4000#;
   Window_Mouse_Relative_Mode : constant Window_Flags := 16#0000_0000_0000_8000#;
   Window_Always_On_Top : constant Window_Flags := 16#0000_0000_0001_0000#;
   Window_Utility : constant Window_Flags := 16#0000_0000_0002_0000#;
   Window_Tooltip : constant Window_Flags := 16#0000_0000_0004_0000#;
   Window_Popup_Menu : constant Window_Flags := 16#0000_0000_0008_0000#;
   Window_Keyboard_Grabbed : constant Window_Flags := 16#0000_0000_0010_0000#;
   Window_Vulkan : constant Window_Flags := 16#0000_0000_1000_0000#;
   Window_Metal : constant Window_Flags := 16#0000_0000_2000_0000#;
   Window_Transparent : constant Window_Flags := 16#0000_0000_4000_0000#;
   Window_Not_Focusable : constant Window_Flags := 16#0000_0000_8000_0000#;

   type Window is new System.Address;
   type Renderer is new System.Address;

   procedure Create_Window_And_Renderer
     (Title    :     String;
      W, H     :     Integer;
      Flags    :     Window_Flags;
      Window   : out SDL3.Window;
      Renderer : out SDL3.Renderer);

   function Get_Window_Flags (Window : SDL3.Window) return Window_Flags
      with Import => True, Convention => C, External_Name => "SDL_GetWindowFlags";

   procedure Get_Window_Size (Window : SDL3.Window; Width, Height : out Integer);
   procedure Set_Window_Fullscreen (Window : SDL3.Window; Fullscreen : Boolean);

   type Renderer_Logical_Presentation is
     (Logical_Presentation_Disabled,
      Logical_Presentation_Stretch,
      Logical_Presentation_Letterbox,
      Logical_Presentation_Overscan,
      Logical_Presentation_Integer_Scale);

   for Renderer_Logical_Presentation use
     (Logical_Presentation_Disabled      => 0,
      Logical_Presentation_Stretch       => 1,
      Logical_Presentation_Letterbox     => 2,
      Logical_Presentation_Overscan      => 3,
      Logical_Presentation_Integer_Scale => 4);

   procedure Set_Render_Logical_Presentation
     (Renderer : SDL3.Renderer; W, H : Integer; Mode : Renderer_Logical_Presentation);

   type System_Theme is (System_Theme_Unknown, System_Theme_Dark, System_Theme_Light);
   function Get_System_Theme return System_Theme;

   procedure Set_Render_V_Sync (Renderer : SDL3.Renderer; V_Sync : Integer);

   type Event_Type is new Uint32;
   Event_First    : constant Event_Type := 16#000#;
   Event_Quit     : constant Event_Type := 16#100#;
   Event_Key_Down : constant Event_Type := 16#300#;
   Event_Key_Up   : constant Event_Type := 16#301#;

   -- NOTE: Just pad out the remaining 120 bytes of the 128 byte Event union.
   type Event_Padding is array (0 .. 14) of Uint64 with Component_Size => 64;

   type Basic_Event is record
      Kind : Event_Type;
      Pad  : Event_Padding;
   end record;

   type Keycode is new Uint32;
   Keycode_Return : constant Keycode := 16#0000_000d#;
   Keycode_Escape : constant Keycode := 16#0000_001b#;
   Keycode_Space  : constant Keycode := 16#0000_0020#;
   Keycode_Right  : constant Keycode := 16#4000_004f#;
   Keycode_Left   : constant Keycode := 16#4000_0050#;
   Keycode_Down   : constant Keycode := 16#4000_0051#;
   Keycode_Up     : constant Keycode := 16#4000_0052#;

   Keycode_0 : constant Keycode := 16#0000_0030#;
   Keycode_1 : constant Keycode := 16#0000_0031#;
   Keycode_2 : constant Keycode := 16#0000_0032#;
   Keycode_3 : constant Keycode := 16#0000_0033#;
   Keycode_4 : constant Keycode := 16#0000_0034#;
   Keycode_5 : constant Keycode := 16#0000_0035#;
   Keycode_6 : constant Keycode := 16#0000_0036#;
   Keycode_7 : constant Keycode := 16#0000_0037#;
   Keycode_8 : constant Keycode := 16#0000_0038#;
   Keycode_9 : constant Keycode := 16#0000_0039#;

   Keycode_A : constant Keycode := 16#0000_0061#;
   Keycode_B : constant Keycode := 16#0000_0062#;
   Keycode_C : constant Keycode := 16#0000_0063#;
   Keycode_D : constant Keycode := 16#0000_0064#;
   Keycode_E : constant Keycode := 16#0000_0065#;
   Keycode_F : constant Keycode := 16#0000_0066#;
   Keycode_G : constant Keycode := 16#0000_0067#;
   Keycode_H : constant Keycode := 16#0000_0068#;
   Keycode_I : constant Keycode := 16#0000_0069#;
   Keycode_J : constant Keycode := 16#0000_006a#;
   Keycode_K : constant Keycode := 16#0000_006b#;
   Keycode_L : constant Keycode := 16#0000_006c#;
   Keycode_M : constant Keycode := 16#0000_006d#;
   Keycode_N : constant Keycode := 16#0000_006e#;
   Keycode_O : constant Keycode := 16#0000_006f#;
   Keycode_P : constant Keycode := 16#0000_0070#;
   Keycode_Q : constant Keycode := 16#0000_0071#;
   Keycode_R : constant Keycode := 16#0000_0072#;
   Keycode_S : constant Keycode := 16#0000_0073#;
   Keycode_T : constant Keycode := 16#0000_0074#;
   Keycode_U : constant Keycode := 16#0000_0075#;
   Keycode_V : constant Keycode := 16#0000_0076#;
   Keycode_W : constant Keycode := 16#0000_0077#;
   Keycode_X : constant Keycode := 16#0000_0078#;
   Keycode_Y : constant Keycode := 16#0000_0079#;
   Keycode_Z : constant Keycode := 16#0000_007a#;

   type Keyboard_Event is record
      Kind      : Event_Type;
      Reserved  : Uint32;
      Timestamp : Uint64;
      Window_ID : Uint32;
      Which     : Uint32;
      Scancode  : Uint32;
      Key       : Keycode;
      Key_Mod   : Uint16;
      Raw       : Uint16;
      Down      : C.C_bool;
      Repeat    : C.C_bool;
   end record;

   type Event_Tag is (Basic, Keyboard);

   type Event (Tag : Event_Tag := Basic) is record
      case Tag is
         when Keyboard =>Key : Keyboard_Event;
         when others =>Basic : Basic_Event;
      end case;
   end record
   with Unchecked_Union;
   for Event'Size use 1_024;
   for Event'Alignment use 8;

   function Poll_Event (Event : out SDL3.Event) return Boolean;

   procedure Set_Render_Draw_Color (Renderer : SDL3.Renderer; R, G, B, A : Uint8);
   procedure Render_Clear (Renderer : SDL3.Renderer);
   procedure Render_Present (Renderer : SDL3.Renderer);

   type Texture is new System.Address;

   type Pixel_Format is new Uint32;
   Pixel_Format_RGBA8888 : constant Pixel_Format := 16#1646_2004#;
   Pixel_Format_ABGR8888 : constant Pixel_Format := 16#1676_2004#;

   type Texture_Access is (Static, Streaming, Target) with Convention => C;

   function Create_Texture (Renderer : SDL3.Renderer; W, H : Integer) return Texture;

   type Rect is record
      X, Y : Integer;
      W, H : Integer;
   end record
   with Convention => C;

   type FRect is record
      X, Y : Float;
      W, H : Float;
   end record
   with Convention => C;

   procedure Update_Texture
     (Texture : SDL3.Texture; Pixels : Pixel_Access; Pitch : Integer);

   procedure Render_Texture
     (Renderer : SDL3.Renderer; Texture : SDL3.Texture);

   type Scale_Mode is (Scale_Mode_Nearest, Scale_Mode_Linear);
   for Scale_Mode use (Scale_Mode_Nearest => 0, Scale_Mode_Linear => 1);

   procedure Set_Texture_Scale_Mode (Texture : SDL3.Texture; Mode : Scale_Mode);

end SDL3;
