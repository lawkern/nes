--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;

package Shared is
   type U8 is new Unsigned_8;
   type U16 is new Unsigned_16;
   type U32 is new Unsigned_32;
   type U64 is new Unsigned_64;

   type S8 is new Integer_8;
   type S16 is new Integer_16;
   type S32 is new Integer_32;
   type S64 is new Integer_64;

   subtype U2 is U8 range 0 .. 3;
   subtype U5 is U8 range 0 .. 31;
   subtype U14 is U16 range 0 .. 16#3FFF#;

   type Pixel_Buffer is array (Natural range <>) of U32;
   type Pixel_Access is access Pixel_Buffer;

   type Texture is record
      Width, Height : Integer;
      Pixels        : Pixel_Access;
   end record;

   package U8_IO is new Ada.Text_IO.Modular_IO (U8);
   package U16_IO is new Ada.Text_IO.Modular_IO (U16);

   function Merge (Low, High : U8) return U16;

   function Kilobytes (N : Natural) return Natural;
   function Megabytes (N : Natural) return Natural;

   procedure Put_Hex (Value : U8; Width : Positive := 2);
   procedure Put_Hex (Value : U16; Width : Positive := 4);
end Shared;
