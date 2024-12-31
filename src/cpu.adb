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

   function Merge (Low, High : U8) return U16 is
   begin
      return Shift_Left (U16 (High), 8) + U16 (Low);
   end Merge;

   procedure Push8 (Value : U8) is
   begin
      Stack_Pointer := Stack_Pointer - 1;

      Memory (Stack_Top + U16 (Stack_Pointer)) := Value;
   end Push8;

   procedure Push16 (Value : U16) is
   begin
      Stack_Pointer := Stack_Pointer - 2;

      Memory (Stack_Top + U16 (Stack_Pointer) + 0) := U8 (Shift_Right (Value, 8) and 16#00FF#);
      Memory (Stack_Top + U16 (Stack_Pointer) + 1) := U8 (Shift_Right (Value, 0) and 16#00FF#);
   end Push16;

   function Pop8 return U8 is
      Result : U8;
   begin
      Result := Memory (Stack_Base - U16 (Stack_Pointer));

      Stack_Pointer := Stack_Pointer + 1;
      return Result;
   end Pop8;

   function Pop16 return U16 is
      High, Low : U8;
   begin
      Low  := Memory (Stack_Base - U16 (Stack_Pointer + 0));
      High := Memory (Stack_Base - U16 (Stack_Pointer + 1));

      Stack_Pointer := Stack_Pointer + 2;
      return Merge (High => High, Low => Low);
   end Pop16;

   -----------------------------------------------------------------------------

   function Decode_And_Execute return Integer is
      Unimplemented_Instruction : exception;

      Instruction, Family, Mode, Modifier : U8;

      Cycle_Count        : Integer := 0;
      Instruction_Length : Integer := 1;

      Data1, Data2 : U8 := 0;

      -------------------------------------------------------------------------
      procedure NOP is
      begin
         Put_Line ("NOP");
      end NOP;

      -------------------------------------------------------------------------
      procedure LDA (Value : U8) is
      begin
         Put_Line ("LDA");
         Accumulator := Value;

         Zero_Flag     := (Value = 0);
         Negative_Flag := (Shift_Right (Value, 7) and 1) = 1;
      end LDA;

      procedure LDX (Value : U8) is
      begin
         Put_Line ("LDX");
         Index_Register_X := Value;

         Zero_Flag     := (Value = 0);
         Negative_Flag := (Shift_Right (Value, 7) and 1) = 1;
      end LDX;

      procedure LDY (Value : U8) is
      begin
         Put_Line ("LDY");
         Index_Register_Y := Value;

         Zero_Flag     := (Value = 0);
         Negative_Flag := (Shift_Right (Value, 7) and 1) = 1;
      end LDY;

      procedure STA (Address : U16) is
      begin
         Put_Line ("STA");
         Memory (Address) := Accumulator;
      end STA;

      procedure STX (Address : U16) is
      begin
         Put_Line ("STX");
         Memory (Address) := Index_Register_X;
      end STX;

      procedure STY (Address : U16) is
      begin
         Put_Line ("STY");
         Memory (Address) := Index_Register_Y;
      end STY;

      ------------------------------------------------------------------------

      procedure TAX is
      begin
         Put_Line ("TAX");
         Index_Register_X := Accumulator;

         Zero_Flag     := (Index_Register_X = 0);
         Negative_Flag := (Shift_Right (Index_Register_X, 7) and 1) = 1;
      end TAX;

      procedure TAY is
      begin
         Put_Line ("TAY");
         Index_Register_Y := Accumulator;

         Zero_Flag     := (Index_Register_Y = 0);
         Negative_Flag := (Shift_Right (Index_Register_Y, 7) and 1) = 1;
      end TAY;

      procedure TSX is
      begin
         Put_Line ("TSX");
         Index_Register_X := Stack_Pointer;

         Zero_Flag     := (Index_Register_X = 0);
         Negative_Flag := (Shift_Right (Index_Register_X, 7) and 1) = 1;
      end TSX;

      procedure TXA is
      begin
         Put_Line ("TXA");
         Accumulator := Index_Register_X;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := (Shift_Right (Accumulator, 7) and 1) = 1;
      end TXA;

      procedure TXS is
      begin
         Put_Line ("TXS");
         Stack_Pointer := Index_Register_X;

         Zero_Flag     := (Stack_Pointer = 0);
         Negative_Flag := (Shift_Right (Stack_Pointer, 7) and 1) = 1;
      end TXS;

      procedure TYA is
      begin
         Put_Line ("TYA");
         Accumulator := Index_Register_Y;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := (Shift_Right (Accumulator, 7) and 1) = 1;
      end TYA;

      -------------------------------------------------------------------

      procedure JMP (Address : U16) is
      begin
         Put_Line ("JMP");
         Program_Counter := Address;
      end JMP;

      procedure JSR (Address : U16) is
      begin
         Put_Line ("JSR");
         Push16 (Program_Counter + 2);
         Program_Counter := Address;
      end JSR;

      procedure BRK is
      begin
         Put_Line ("BRK");
         Break_Command := True;
      end BRK;

      procedure RTS is
      begin
         Put_Line ("RTS");
         Program_Counter := Pop16 + 1;
      end RTS;

      procedure RTI is
      begin
         Put_Line ("RTI");

         declare
            Flags : U8;
         begin
            Flags := Pop8;

            Carry_Flag        := (Flags and Carry_Flag_Bit) /= 0;
            Zero_Flag         := (Flags and Zero_Flag_Bit) /= 0;
            Interrupt_Disable := (Flags and Interrupt_Disable_Bit) /= 0;
            Decimal_Mode      := (Flags and Decimal_Mode_Bit) /= 0;
            Overflow_Flag     := (Flags and Overflow_Flag_Bit) /= 0;
            Negative_Flag     := (Flags and Negative_Flag_Bit) /= 0;

            Program_Counter := Pop16;
         end;
      end RTI;

      -----------------------------------------------------------------

      procedure PHA is
      begin
         Put_Line ("PHA");
         Push8 (Accumulator);
      end PHA;

      procedure PHP is
         function Pack_Flags return U8 is
            Result : U8 := 2#0011_0000#;
         begin
            -- TODO: Pack the flag bits more intelligently.
            Result := (if Carry_Flag then (Result or Carry_Flag_Bit) else Result);
            Result := (if Zero_Flag then (Result or Zero_Flag_Bit) else Result);
            Result := (if Interrupt_Disable then (Result or Interrupt_Disable_Bit) else Result);
            Result := (if Decimal_Mode then (Result or Decimal_Mode_Bit) else Result);
            Result := (if Overflow_Flag then (Result or Overflow_Flag_Bit) else Result);
            Result := (if Negative_Flag then (Result or Negative_Flag_Bit) else Result);

            return Result;
         end Pack_Flags;
      begin
         Put_Line ("PHP");
         Push8 (Pack_Flags);
      end PHP;

      procedure PLA is
      begin
         Put_Line ("PLA");
         Accumulator := Pop8;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := (Shift_Right (Accumulator, 7) and 1) = 1;
      end PLA;

      procedure PLP is
         Flags : U8;
      begin
         Put_Line ("PLP");
         Flags := Pop8;

         Carry_Flag        := (Flags and Carry_Flag_Bit) /= 0;
         Zero_Flag         := (Flags and Zero_Flag_Bit) /= 0;
         Interrupt_Disable := (Flags and Interrupt_Disable_Bit) /= 0; -- TODO: Delay effect by one instruction.
         Decimal_Mode      := (Flags and Decimal_Mode_Bit) /= 0;
         Overflow_Flag     := (Flags and Overflow_Flag_Bit) /= 0;
         Negative_Flag     := (Flags and Negative_Flag_Bit) /= 0;
      end PLP;

      ---------------------------------------------------------------

      procedure Print_Instruction is
      begin
         Put ("   Instruction     : ");
         U8_IO.Put (Instruction, Base => 16);
         New_Line;

         Put ("   Family          : ");
         U8_IO.Put (Family, Base => 2);
         New_Line;

         Put ("   Addressing Mode : ");
         U8_IO.Put (Mode, Base => 2);
         New_Line;

         Put ("   Modifier        : ");
         U8_IO.Put (Modifier, Base => 2);
         New_Line;

         if Instruction_Length > 1 then
            Put ("   Data Byte 1     : ");
            U8_IO.Put (Data1, Base => 16);
            New_Line;
         end if;

         if Instruction_Length > 2 then
            Put ("   Data Byte 2     : ");
            U8_IO.Put (Data2, Base => 16);
            New_Line;
         end if;

         New_Line;
      end Print_Instruction;

      --------------------------------------------------------------------------
   begin
      Put_Line ("---------------------------------------------------");

      Instruction := Memory (Program_Counter);
      Family      := Shift_Right (Instruction, 5) and 2#000_0111#;
      Mode        := Shift_Right (Instruction, 2) and 2#000_0111#;
      Modifier    := Shift_Right (Instruction, 0) and 2#000_0011#;

      case Instruction is
         when 16#EA# => -- NOP
            Cycle_Count := 2;
            NOP;

            --------------------------------------------------------------------

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

            --------------------------------------------------------------------

         when 16#A2# => -- LDX #Immediate
            Instruction_Length := 2;
            Cycle_Count        := 2;

            Data1 := Memory (Program_Counter + 1);
            LDX (Data1);

         when 16#A6# => -- LDX Zero Page
            Instruction_Length := 2;
            Cycle_Count        := 3;

            Data1 := Memory (Program_Counter + 1);
            LDX (Memory (U16 (Data1)));

         when 16#B6# => -- LDX Zero Page,X
            Instruction_Length := 2;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            LDX (Memory (U16 (Data1 + Index_Register_X)));

         when 16#AE# => -- LDX Absolute
            Instruction_Length := 3;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            LDX (Memory (Merge (Low => Data1, High => Data2)));

         when 16#BE# => -- LDX Absolute,X
            Instruction_Length := 3;
            Cycle_Count        := 4; -- TODO: 5 if page is crossed.

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            LDX (Memory (Merge (Low => Data1, High => Data2) + U16 (Index_Register_X)));

            --------------------------------------------------------------------

         when 16#A0# => -- LDY #Immediate
            Instruction_Length := 2;
            Cycle_Count        := 2;

            Data1 := Memory (Program_Counter + 1);
            LDY (Data1);

         when 16#A4# => -- LDY Zero Page
            Instruction_Length := 2;
            Cycle_Count        := 3;

            Data1 := Memory (Program_Counter + 1);
            LDY (Memory (U16 (Data1)));

         when 16#B4# => -- LDY Zero Page,X
            Instruction_Length := 2;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            LDY (Memory (U16 (Data1 + Index_Register_X)));

         when 16#AC# => -- LDY Absolute
            Instruction_Length := 3;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            LDY (Memory (Merge (Low => Data1, High => Data2)));

         when 16#BC# => -- LDY Absolute,X
            Instruction_Length := 3;
            Cycle_Count        := 4; -- TODO: 5 if page is crossed.

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            LDY (Memory (Merge (Low => Data1, High => Data2) + U16 (Index_Register_X)));

            --------------------------------------------------------------------

         when 16#85# => -- STA Zero Page
            Instruction_Length := 2;
            Cycle_Count        := 3;

            Data1 := Memory (Program_Counter + 1);
            STA (U16 (Data1));

         when 16#95# => -- STA Zero Page,X
            Instruction_Length := 2;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            STA (U16 (Data1 + Index_Register_X));

         when 16#8D# => -- STA Absolute
            Instruction_Length := 3;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            STA (Merge (Low => Data1, High => Data2));

         when 16#9D# => -- STA Absolute,X
            Instruction_Length := 3;
            Cycle_Count        := 5;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            STA (Merge (Low => Data1, High => Data2) + U16 (Index_Register_X));

         when 16#99# => -- STA Absolute,Y
            Instruction_Length := 3;
            Cycle_Count        := 5;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            STA (Merge (Low => Data1, High => Data2) + U16 (Index_Register_Y));

         when 16#81# => -- STA (Indirect,X)
            Instruction_Length := 2;
            Cycle_Count        := 6;

            Data1 := Memory (Program_Counter + 1);
            declare
               Base : U16 := U16 (Data1) + U16 (Index_Register_X);
            begin
               STA (Merge (Low => Memory (Base + 0), High => Memory (Base + 1)));
            end;

         when 16#91# => -- STA (Indirect),Y
            Instruction_Length := 2;
            Cycle_Count        := 6;

            Data1 := Memory (Program_Counter + 1);
            STA (U16 (Memory (U16 (Data1))) + U16 (Index_Register_Y));

            --------------------------------------------------------------------

         when 16#86# => -- STX Zero Page
            Instruction_Length := 2;
            Cycle_Count        := 3;

            Data1 := Memory (Program_Counter + 1);
            STX (U16 (Data1));

         when 16#96# => -- STX Zero Page,Y
            Instruction_Length := 2;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            STX (U16 (Data1 + Index_Register_Y));

         when 16#8E# => -- STX Absolute
            Instruction_Length := 3;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            STX (Merge (Low => Data1, High => Data2));

            --------------------------------------------------------------------

         when 16#84# => -- STY Zero Page
            Instruction_Length := 2;
            Cycle_Count        := 3;

            Data1 := Memory (Program_Counter + 1);
            STY (U16 (Data1));

         when 16#94# => -- STY Zero Page,X
            Instruction_Length := 2;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            STY (U16 (Data1 + Index_Register_X));

         when 16#8C# => -- STY Absolute
            Instruction_Length := 3;
            Cycle_Count        := 4;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            STY (Merge (Low => Data1, High => Data2));

            --------------------------------------------------------------------

         when 16#AA# => -- TAX
            Instruction_Length := 1;
            Cycle_Count        := 2;
            TAX;

         when 16#A8# => -- TAY
            Instruction_Length := 1;
            Cycle_Count        := 2;
            TAY;

         when 16#BA# => -- TSX
            Instruction_Length := 1;
            Cycle_Count        := 2;
            TSX;

         when 16#8A# => -- TXA
            Instruction_Length := 1;
            Cycle_Count        := 2;
            TXA;

         when 16#9A# => -- TXS
            Instruction_Length := 1;
            Cycle_Count        := 2;
            TXS;

         when 16#98# => -- TYA
            Instruction_Length := 1;
            Cycle_Count        := 2;
            TYA;

            --------------------------------------------------------------------

         when 16#4C# => -- JMP Absolute
            Instruction_Length := 3;
            Cycle_Count        := 3;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            JMP (Merge (Low => Data1, High => Data2));

         when 16#6C# => -- JMP (Indirect)
            Instruction_Length := 3;
            Cycle_Count        := 5;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            declare
               Base : U16 := Merge (Low => Data1, High => Data2);
            begin
               JMP (Merge (Low => Memory (Base + 0), High => Memory (Base + 1)));
            end;

         when 16#20# => -- JSR
            Instruction_Length := 3;
            Cycle_Count        := 6;

            Data1 := Memory (Program_Counter + 1);
            Data2 := Memory (Program_Counter + 2);
            JSR (Merge (Low => Data1, High => Data2));

         when 16#60# => -- RTS
            Instruction_Length := 1;
            Cycle_Count        := 6;
            RTS;

         when 16#00# => -- BRK
            Instruction_Length := 2; -- NOTE: BRK skips the following byte on return.
            Cycle_Count        := 7;
            BRK;

         when 16#40# => -- RTI
            Instruction_Length := 1;
            Cycle_Count        := 6;
            RTI;

            --------------------------------------------------------------------

         when 16#48# => -- PHA
            Instruction_Length := 1;
            Cycle_Count        := 3;
            PHA;

         when 16#08# => -- PHP
            Instruction_Length := 1;
            Cycle_Count        := 3;
            PHP;

         when 16#68# => -- PLA
            Instruction_Length := 1;
            Cycle_Count        := 4;
            PLA;

         when 16#28# => -- PLP
            Instruction_Length := 1;
            Cycle_Count        := 4;
            PLP;

            --------------------------------------------------------------------

         when others =>
            Put_Line ("*Unhandled Instruction*");
            Print_Instruction;

            raise Unimplemented_Instruction;
      end case;

      Print_Instruction;
      Program_Counter := Program_Counter + U16 (Instruction_Length);

      return Cycle_Count;
   end Decode_And_Execute;

   -----------------------------------------------------------------------------
end CPU;
