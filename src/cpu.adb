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

         Zero_Flag     := (Value = 0);
         Negative_Flag := (Shift_Right (Value, 7) and 1) = 1;
      end LDA;

      function Merge (Low, High : U8) return U16 is
      begin
         return Shift_Left (U16 (High), 8) + U16 (Low);
      end Merge;

      Instruction, Family, Mode, Modifier : U8;

      Cycle_Count        : Integer := 0;
      Instruction_Length : Integer := 1;

      Data1, Data2 : U8 := 0;
   begin
      Put_Line ("---------------------------------------------------");

      Instruction := Memory (Program_Counter);

      case Instruction is
         when 16#00# => -- BRK
            Instruction_Length := 2; -- NOTE: BRK skips the following byte on return.
            Cycle_Count        := 7;
            BRK;

         when 16#A9# => -- LDA #Immediate
            Instruction_Length := 2;
            Cycle_Count        := 2;

            Data1 := Memory (Program_Counter + 1);
            LDA (Data1);

         when 16#A5# => -- LDA Zero Page
            Instruction_Length := 2;
            Cycle_Count        := 3;

            Data1 := Memory (Program_Counter + 1);
            LDA (Memory (U16 (Data1)));

         when 16#B5# => -- LDA Zero Page,X
            Instruction_Length := 2;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            LDA (Memory (U16 (Data1 + Index_Register_X)));

         when 16#AD# => -- LDA Absolute
            Instruction_Length := 3;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            LDA (Memory (Merge (Low => Data1, High => Data2)));

         when 16#BD# => -- LDA Absolute,X
            Instruction_Length := 3;
            Cycle_Count        := 4; -- TODO: 5 if page is crossed.

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            LDA (Memory (Merge (Low => Data1, High => Data2) + U16 (Index_Register_X)));

         when 16#B9# => -- LDA Absolute,Y
            Instruction_Length := 3;
            Cycle_Count        := 4; -- TODO: 5 if page is crossed.

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            LDA (Memory (Merge (Low => Data1, High => Data2) + U16 (Index_Register_Y)));

         when 16#A1# => -- LDA (Indirect,X)
            Instruction_Length := 2;
            Cycle_Count        := 6;

            Data1 := Memory (Program_Counter + 1);
            declare
               Base : U16 := U16 (Data1) + U16 (Index_Register_X);
            begin
               LDA (Memory (Merge (Low => Memory (Base + 0), High => Memory (Base + 1))));
            end;

         when 16#B1# => -- LDA (Indirect),Y
            Instruction_Length := 2;
            Cycle_Count        := 5; -- TODO: 6 if page is crossed.

            Data1 := Memory (Program_Counter + 1);
            LDA (Memory (U16 (Memory (U16 (Data1))) + U16 (Index_Register_Y)));

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
