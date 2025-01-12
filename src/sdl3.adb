--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

-- NOTE: The convention here is to define direct bindings to SDL3 functions with
-- the prefix SDL_. These are then wrapped with the non-prefixed versions that
-- map any C types to Ada primitives. Calls in usage code can then be prefixed
-- with the packge name, e.g. SDL3.Init (SDL3.Window_Fullscreen);

package body SDL3 is

   procedure Init (Flags : Init_Flags) is
      function SDL_Init (Flags : Init_Flags) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_Init";
   begin
      if SDL_Init (Flags) /= C.True then
         raise Initialization_Error;
      end if;
   end Init;

   ----------------------------------------------------------------------------
   procedure Create_Window_And_Renderer
     (Title    :     String;
      W, H     :     Integer;
      Flags    :     Window_Flags;
      Window   : out SDL3.Window;
      Renderer : out SDL3.Renderer)
   is
      function SDL_Create_Window_And_Renderer
        (Title        : C.char_array;
         W, H         : C.int;
         Flags        : Window_Flags;
         Window_Ptr   : System.Address;
         Renderer_Ptr : System.Address) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_CreateWindowAndRenderer";
   begin
      if SDL_Create_Window_And_Renderer
          (Title        => To_C (Title),
           W            => C.int (W),
           H            => C.int (H),
           Flags        => Flags,
           Window_Ptr   => Window'Address,
           Renderer_Ptr => Renderer'Address) /= C.True then
         raise Initialization_Error;
      end if;
   end Create_Window_And_Renderer;


   procedure Get_Window_Size
     (Window : SDL3.Window; Width, Height : out Integer) is
      function SDL_Get_Window_Size
        (Window : SDL3.Window; Width, Height : System.Address) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_GetWindowSize";
   begin
      if SDL_Get_Window_Size (Window, Width'Address, Height'Address) /= C.True then
         raise Initialization_Error;
      end if;
   end Get_Window_Size;

   procedure Set_Window_Fullscreen (Window : SDL3.Window; Fullscreen : Boolean) is
      function SDL_Set_Window_Fullscreen (Window : SDL3.Window; Fullscreen : C.C_bool) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_SetWindowFullscreen";
   begin
      if SDL_Set_Window_Fullscreen (Window, C.C_bool (Fullscreen)) /= C.True then
         raise Initialization_Error;
      end if;
   end Set_Window_Fullscreen;

   ------------------------------------------------------------------------
   procedure Set_Render_Logical_Presentation
     (Renderer : SDL3.Renderer; W, H : Integer; Mode : Renderer_Logical_Presentation) is

      -- TODO: Is there a portable way to determine the representation C will
      -- choose for the Mode enum? Maybe we just don't care, since we know the
      -- range is 0 to 4.

      function SDL_Set_Render_Logical_Presentation
        (Renderer : SDL3.Renderer; W, H : Integer; Mode : C.int) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_SetRenderLogicalPresentation";

      C_Mode : C.int;
   begin
      C_Mode := C.int (Renderer_Logical_Presentation'Enum_Rep (Mode));
      if SDL_Set_Render_Logical_Presentation (Renderer, W, H, C_Mode) /= C.True then
         raise Initialization_Error;
      end if;
   end Set_Render_Logical_Presentation;

   -------------------------------------------------------------------------
   function Get_System_Theme return System_Theme is
      function SDL_Get_System_Theme return Uint32
         with Import => True, Convention => C, External_Name => "SDL_GetSystemTheme";
   begin
      case SDL_Get_System_Theme is
         when 1 =>return System_Theme_Light;
         when 2 =>return System_Theme_Dark;
         when others =>return System_Theme_Unknown;
      end case;
   end Get_System_Theme;

   ----------------------------------------------------------------------------
   procedure Set_Render_V_Sync (Renderer : SDL3.Renderer; V_Sync : Integer) is
      function SDL_Set_Render_V_Sync (Renderer : SDL3.Renderer; V_Sync : C.int) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_SetRenderVSync";
   begin
      if SDL_Set_Render_V_Sync (Renderer, C.int (V_Sync)) /= C.True then
         null;
      end if;
   end Set_Render_V_Sync;

   ----------------------------------------------------------------------------
   function Poll_Event (Event : out SDL3.Event) return Boolean is
      function SDL_Poll_Event (Event_Ptr : System.Address) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_PollEvent";
   begin
      return SDL_Poll_Event (Event'Address) = C.True;
   end Poll_Event;

   ----------------------------------------------------------------------------
   procedure Set_Render_Draw_Color (Renderer : SDL3.Renderer; R, G, B, A : Uint8) is
      function SDL_Set_Render_Draw_Color (Renderer : SDL3.Renderer; R, G, B, A : Uint8) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_SetRenderDrawColor";
   begin
      if SDL_Set_Render_Draw_Color (Renderer, R, G, B, A) /= C.False then
         null; -- TODO: Do we care if this fails?
      end if;
   end Set_Render_Draw_Color;

   ----------------------------------------------------------------------------
   procedure Render_Clear (Renderer : SDL3.Renderer) is
      function SDL_Render_Clear (Renderer : SDL3.Renderer) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_RenderClear";
   begin
      if SDL_Render_Clear (Renderer) /= C.True then
         null; -- TODO: Do we care if this fails?
      end if;
   end Render_Clear;

   ----------------------------------------------------------------------------
   procedure Render_Present (Renderer : SDL3.Renderer) is
      function SDL_Render_Present (Renderer : SDL3.Renderer) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_RenderPresent";
   begin
      if SDL_Render_Present (Renderer) /= C.True then
         null; -- TODO: Do we care if this fails?
      end if;
   end Render_Present;

   ----------------------------------------------------------------------------
   function Create_Texture
     (Renderer : SDL3.Renderer;
      W, H     : Integer) return Texture is

      function SDL_Create_Texture
        (Renderer       : SDL3.Renderer;
         Format         : Pixel_Format;
         Access_Pattern : Texture_Access;
         W, H           : C.int) return Texture
         with Import => True, Convention => C, External_Name => "SDL_CreateTexture";

      Result : Texture;
   begin
      Result := SDL_Create_Texture (Renderer, Pixel_Format_RGBA8888, Target, C.int (W), C.int (H));
      if Result = Texture (System.Null_Address) then
         raise Initialization_Error;
      end if;

      return Result;
   end Create_Texture;

   ----------------------------------------------------------------------------
   procedure Update_Texture
     (Texture : SDL3.Texture;
      Pixels  : Pixel_Access;
      Pitch   : Integer) is

      function SDL_Update_Texture
        (Texture : SDL3.Texture;
         Rect    : System.Address;
         Pixels  : System.Address;
         Pitch   : C.int) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_UpdateTexture";
   begin
      if SDL_Update_Texture
          (Texture, System.Null_Address, Pixels.all'Address, C.int (Pitch)) /= C.True then
         null; -- TODO: Do we care if this fails?
      end if;
   end Update_Texture;

   ----------------------------------------------------------------------------
   procedure Render_Texture
     (Renderer : SDL3.Renderer; Texture : SDL3.Texture) is
      function SDL_Render_Texture
        (Renderer           : SDL3.Renderer;
         Texture            : SDL3.Texture;
         Src_Rect, Dst_Rect : System.Address) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_RenderTexture";
   begin
      if SDL_Render_Texture
          (Renderer, Texture, System.Null_Address, System.Null_Address) /= C.True then
         null; -- TODO: Do we care if this fails?
      end if;
   end Render_Texture;

   -------------------------------------------------------------------------
   procedure Set_Texture_Scale_Mode (Texture : SDL3.Texture; Mode : Scale_Mode) is

      -- TODO: Is there a portable way to determine the representation C will
      -- choose for the Mode enum? Maybe we just don't care, since we know the
      -- range is 0 to 1.

      function SDL_Set_Texture_Scale_Mode (Texture : SDL3.Texture; Mode : C.int) return C.C_bool
        with Import => True, Convention => C, External_Name => "SDL_SetTextureScaleMode";

      C_Mode : C.int;
   begin
      C_Mode := C.int (Scale_Mode'Enum_Rep (Mode));
      if SDL_Set_Texture_Scale_Mode (Texture, C_Mode) /= C.True then
         raise Initialization_Error;
      end if;
   end Set_Texture_Scale_Mode;

end SDL3;
