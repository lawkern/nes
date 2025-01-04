--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

package body PPU is

   procedure Write_Register (Address : PPU.Register_Address; Value : U8) is
      Offset : U16 := Address and 2#0111#;
   begin
      case Offset is
         when 0 =>PPU_CTRL := Value;
         when 1 =>PPU_MASK := Value;
         when 2 =>PPU_STATUS := Value;
         when 3 =>OAM_ADDR := Value;
         when 4 =>OAM_DATA := Value;
         when 5 =>PPU_SCROLL := Value;
         when 6 =>PPU_ADDRESS := Value;
         when 7 =>PPU_DATA := Value;
         when others =>null;
      end case;
   end Write_Register;

end PPU;
