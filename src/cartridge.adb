--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

with CPU;    use CPU;
with Memory; use Memory;

package body Cartridge is

   procedure Load (Path : String) is
      package Header_IO is new Ada.Sequential_IO (Cartridge_Header);
      use Header_IO;

      package Content_IO is new Ada.Sequential_IO (U8);
      use Content_IO;

      Header       : Cartridge_Header;
      Header_File  : Header_IO.File_Type;
      Content_File : Content_IO.File_Type;

      Use_Trainer_Area           : Boolean := False;
      PRG_ROM_High, CHR_ROM_High : U8      := 0;
      PRG_ROM_Size, CHR_ROM_Size : Natural := 0;
   begin
      Put_Line ("Loading Cartridge: " & Path);

      Header_IO.Open (Header_File, In_File, Path);
      Header_IO.Read (Header_File, Header);

      if
        (Header.Identifier (1) /= 'N') or
        (Header.Identifier (2) /= 'E') or
        (Header.Identifier (3) /= 'S') or
        (Header.Identifier (4) /= EOF)
      then
         raise Invalid_Format;
      end if;

      Put_Line ("Cartridge Header: " & Header'Image);

      Use_Trainer_Area := (Header.Flags6 and 2#0000_0100#) /= 0;

      PRG_ROM_High := Header.PRG_CHR_ROM_Size_High and 2#0000_1111#;
      CHR_ROM_High := Shift_Right (Header.PRG_CHR_ROM_Size_High, 4);

      -- NOTE: If the high-byte for either ROM equals 0xF, then an
      -- exponent-multiplier notation is used. Otherwise the high and low bytes
      -- can be combined normally.

      if PRG_ROM_High = 16#F# then
         PRG_ROM_Size := 2**Natural (Shift_Right (Header.PRG_ROM_Size_Low, 2)) *
           (Natural (Header.PRG_ROM_Size_Low and 2#11#) * 2 + 1);
      else
         PRG_ROM_Size := Kilobytes (16) *
           Natural (Merge (Low => Header.PRG_ROM_Size_Low, High => PRG_ROM_High));
      end if;

      if CHR_ROM_High = 16#F# then
         CHR_ROM_Size := 2**Natural (Shift_Right (Header.CHR_ROM_Size_Low, 2)) *
           (Natural (Header.CHR_ROM_Size_Low and 2#11#) * 2 + 1);
      else
         CHR_ROM_Size := Kilobytes (8) *
           Natural (Merge (Low => Header.CHR_ROM_Size_Low, High => CHR_ROM_High));
      end if;

      Put_Line ("Use Trainer Area: " & Use_Trainer_Area'Image);
      Put_Line ("PRG ROM Size: " & PRG_ROM_Size'Image);
      Put_Line ("CHR ROM Size: " & CHR_ROM_Size'Image);

      Header_IO.Close (Header_File);
   end Load;

end Cartridge;
