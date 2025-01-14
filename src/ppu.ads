--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Shared; use Shared;

package PPU is

   -- NOTE: PPU Registers:
   type Control_Register (As_Byte : Boolean := True) is record
      case As_Byte is
         when True =>
            Byte_Value : U8;
         when False =>
            Nametable_Base_Address           : U2;
            VRAM_Address_Increment           : Boolean;
            Sprite_Pattern_Table_Address     : Boolean;
            Background_Pattern_Table_Address : Boolean;
            Sprite_Size                      : Boolean;
            Master_Slave_Select              : Boolean;
            Enable_Vblank_NMI                : Boolean;
      end case;
   end record
   with Unchecked_Union => True, Size => 8;

   for Control_Register use record
      Byte_Value                       at 0 range 0 .. 7;
      --
      Nametable_Base_Address           at 0 range 0 .. 1;
      VRAM_Address_Increment           at 0 range 2 .. 2;
      Sprite_Pattern_Table_Address     at 0 range 3 .. 3;
      Background_Pattern_Table_Address at 0 range 4 .. 4;
      Sprite_Size                      at 0 range 5 .. 5;
      Master_Slave_Select              at 0 range 6 .. 6;
      Enable_Vblank_NMI                at 0 range 7 .. 7;
   end record;

   type Mask_Register (As_Byte : Boolean := True) is record
      case As_Byte is
         when True =>
            Byte_Value : U8;
         when False =>
            Greyscale                   : Boolean;
            Show_Leftmost_Background    : Boolean;
            Show_Leftmost_Sprites       : Boolean;
            Enable_Background_Rendering : Boolean;
            Enable_Sprite_Rendering     : Boolean;
            Emphasize_Red               : Boolean;
            Emphasize_Green             : Boolean;
            Emphasize_Blue              : Boolean;
      end case;
   end record
   with Unchecked_Union => True, Size => 8;

   for Mask_Register use record
      Byte_Value                  at 0 range 0 .. 7;
      --
      Greyscale                   at 0 range 0 .. 0;
      Show_Leftmost_Background    at 0 range 1 .. 1;
      Show_Leftmost_Sprites       at 0 range 2 .. 2;
      Enable_Background_Rendering at 0 range 3 .. 3;
      Enable_Sprite_Rendering     at 0 range 4 .. 4;
      Emphasize_Red               at 0 range 5 .. 5;
      Emphasize_Green             at 0 range 6 .. 6;
      Emphasize_Blue              at 0 range 7 .. 7;
   end record;

   type Status_Register (As_Byte : Boolean := True) is record
      case As_Byte is
         when True =>
            Byte_Value : U8;
         when False =>
            Open_Bus             : U5;
            Sprite_Overflow_Flag : Boolean;
            Sprite_0_Hit_Flag    : Boolean;
            Vblank_Flag          : Boolean;
      end case;
   end record
   with Unchecked_Union => True, Size => 8;

   for Status_Register use record
      Byte_Value           at 0 range 0 .. 7;
      --
      Open_Bus             at 0 range 0 .. 4;
      Sprite_Overflow_Flag at 0 range 5 .. 5;
      Sprite_0_Hit_Flag    at 0 range 6 .. 6;
      Vblank_Flag          at 0 range 7 .. 7;
   end record;

   PPU_CTRL    : Control_Register;
   PPU_MASK    : Mask_Register;
   PPU_STATUS  : Status_Register;
   OAM_ADDR    : U8;
   OAM_DATA    : U8;
   PPU_SCROLL  : U8;
   PPU_ADDRESS : U8;
   PPU_DATA    : U8;

   subtype Register_Address is U16 range 16#2000# .. 16#3FFF#;
   procedure Write_Register (Address : PPU.Register_Address; Value : U8);


   -- NOTE: PPU memory
   type Address_Space is array (U14) of U8;
   Memory : Address_Space;

   Pattern_Table0 : constant U14 := 16#0000#;
   Pattern_Table1 : constant U14 := 16#1000#;
   Name_Table0    : constant U14 := 16#2000#;
   Name_Table1    : constant U14 := 16#2400#;
   Name_Table3    : constant U14 := 16#2800#;

   function Read (Address : U14) return U8;
   procedure Write (Address : U14; Value : U8);

   procedure Render_Clear (Backbuffer : Texture; Color : U32);
   procedure Render_Pattern_Table(Backbuffer : Texture);

end PPU;
