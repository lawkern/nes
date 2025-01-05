--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

package body PPU is

   procedure Write_Register (Address : PPU.Register_Address; Value : U8) is
      Offset : U16 := Address and 2#0111#;
   begin
      case Offset is
         when 0 =>
            PPU_CTRL := Value;
         when 1 =>
            PPU_MASK := Value;
         when 2 =>
            PPU_STATUS := Value;
         when 3 =>
            OAM_ADDR := Value;
         when 4 =>
            OAM_DATA := Value;
         when 5 =>
            PPU_SCROLL := Value;
         when 6 =>
            PPU_ADDRESS := Value;
         when 7 =>
            PPU_DATA := Value;
         when others =>
            null;
      end case;
   end Write_Register;

   function Read (Address : U14) return U8 is
   begin
      return PPU.Memory (Address);
   end Read;

   procedure Write (Address : U14; Value : U8) is
   begin
      PPU.Memory (Address) := Value;
   end Write;

   procedure Render_Pattern_Table (Backbuffer : Texture) is
      procedure Render_Tile (Base : U14; Screen_X, Screen_Y : Integer) is
         Plane0, Plane1, Bit0, Bit1 : U8;
         Row : Natural := 0;
      begin
         for Y in 0 .. 7 loop
            Plane0 := PPU.Read (Base + 0 + U14 (Y));
            Plane1 := PPU.Read (Base + 8 + U14 (Y));

            Row := (Y + Screen_Y) * Backbuffer.Width;
            for X in 0 .. 7 loop
               Bit0 := Shift_Right (Plane0, X) and 2#01#;
               Bit1 := Shift_Left (Shift_Right (Plane1, X), 1) and 2#10#;

               case Bit0 or Bit1 is
                  when 1 =>Backbuffer.Pixels.all (Row + Screen_X + X) := 16#CCCC_CCFF#;
                  when 2 =>Backbuffer.Pixels.all (Row + Screen_X + X) := 16#8888_88FF#;
                  when 3 =>Backbuffer.Pixels.all (Row + Screen_X + X) := 16#3333_33FF#;
                  when others =>null;
               end case;
            end loop;
         end loop;
      end Render_Tile;
   begin
      for TileY in 0 .. 15 loop
         for TileX in 0 .. 15 loop
            declare
               Offset : U14 := U14 (16 * (TileX + TileY));
               ScreenX : Integer := 8 * TileX;
               ScreenY : Integer := 8 * TileY;
            begin
               Render_Tile (Pattern_Table0 + Offset, ScreenX, ScreenY);
               Render_Tile (Pattern_Table1 + Offset, ScreenX + (16 * 8), ScreenY);
            end;
         end loop;
      end loop;
   end Render_Pattern_Table;

end PPU;
