--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Strings.Fixed;

package body Shared is

   -- NOTE: This manually takes the substring of hex digits from an Ada-format
   -- 16#...# hex literal. This is just for debug output so we probably don't
   -- care, but maybe Ada has a smarter way to do this. It seems pretty silly
   -- that Ada has no built-in way to output hex/binary integers without the
   -- surrounding #'s.

   function Unformat_Hex (Value : U16; Width : Positive) return String is
      Prefix_Length : constant := 3; -- 16#
      Prefixed_Hex  : String (1 .. Prefix_Length + Width + 1);
      Start         : Natural;
      Result        : String (1 .. Width);
   begin
      U16_IO.Put (Prefixed_Hex, Value, 16);
      Start := Ada.Strings.Fixed.Index (Source => Prefixed_Hex, Pattern => "#");
      Ada.Strings.Fixed.Move
        (Source  => Prefixed_Hex (Start + 1 .. Prefixed_Hex'Last - 1),
         Target  => Result,
         Justify => Ada.Strings.Right,
         Pad     => '0');

      return Result;
   end Unformat_Hex;

   procedure Put_Hex (Value : U8; Width : Positive := 2) is
   begin
      Put (Unformat_Hex (U16 (Value), Width));
   end Put_Hex;

   procedure Put_Hex (Value : U16; Width : Positive := 4) is
   begin
      Put (Unformat_Hex (Value, Width));
   end Put_Hex;

end Shared;
