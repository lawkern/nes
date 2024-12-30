--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

package body CPU is
   -----------------------------------------------------------------------------

   procedure Print_Registers is
   begin
      U8_IO.Default_Base  := 16;
      U16_IO.Default_Base := 16;

      Put ("PC : ");
      U16_IO.Put (Program_Counter);
      New_Line;

      Put ("SP : ");
      U8_IO.Put (Stack_Pointer);
      New_Line;

      Put ("A  : ");
      U8_IO.Put (Accumulator);
      New_Line;

      Put ("X  : ");
      U8_IO.Put (Index_Register_X);
      New_Line;

      Put ("Y  : ");
      U8_IO.Put (Index_Register_Y);
      New_Line (2);

      Put_Line ("Carry_Flag        : " & Carry_Flag'Image);
      Put_Line ("Zero_Flag         : " & Zero_Flag'Image);
      Put_Line ("Interrupt_Disable : " & Interrupt_Disable'Image);
      Put_Line ("Decimal_Mode      : " & Decimal_Mode'Image);
      Put_Line ("Break_Command     : " & Break_Command'Image);
      Put_Line ("Overflow_Flag     : " & Overflow_Flag'Image);
      Put_Line ("Negative_Flag     : " & Negative_Flag'Image);
   end Print_Registers;

   -----------------------------------------------------------------------------

   function Decode_And_Execute return Integer is
      procedure BRK is
      begin
         Put_Line ("BRK");
         Break_Command := True;
      end BRK;

      procedure LDA (Value : U8) is
      begin
         Put_Line ("LDA");
         Accumulator := Value;

         Zero_Flag := (Value = 0);
         Negative_Flag := (Shift_Right (Value, 7) and 1) = 1;
      end LDA;

      Instruction, Family, Mode,  Modifier : U8;

      Cycle_Count : Integer := 0;
      Instruction_Length : Integer := 1;
   begin
      Instruction := Memory (Program_Counter);

      Put_Line ("---------------------------------------------------");

      case Instruction is
         when 16#00# =>
            Instruction_Length := 2; -- NOTE: BRK skips the following byte on return.
            Cycle_Count        := 7;

            BRK;
         when 16#A9# =>
            Instruction_Length := 2;
            Cycle_Count        := 2;

            LDA (Memory (Program_Counter + 1));
         when others =>
            Put_Line ("*Unhandled Instruction*");
      end case;

      Put ("   Instruction     : ");
      U8_IO.Put (Instruction, Base => 16);
      New_Line;

      Family   := Shift_Right (Instruction, 5) and 2#000_0111#;
      Mode     := Shift_Right (Instruction, 2) and 2#000_0111#;
      Modifier := Shift_Right (Instruction, 0) and 2#000_0011#;

      Put ("   Family          : ");
      U8_IO.Put (Family, Base => 2);
      New_Line;

      Put ("   Addressing Mode : ");
      U8_IO.Put (Mode, Base => 2);
      New_Line;

      Put ("   Modifier        : ");
      U8_IO.Put (Modifier, Base => 2);
      New_Line (2);

      Program_Counter := Program_Counter + U16 (Instruction_Length);

      return Cycle_Count;
   end Decode_And_Execute;

   -----------------------------------------------------------------------------
end CPU;
