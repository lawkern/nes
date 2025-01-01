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

   package U8_IO is new Ada.Text_IO.Modular_IO (U8);
   package U16_IO is new Ada.Text_IO.Modular_IO (U16);
end Shared;
