--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

-- Official instructions to implement:
-- -----------------------------------
-- DONE: Access     LDA   STA   LDX   STX   LDY   STY
-- DONE: Transfer   TAX   TXA   TAY   TYA
-- DONE: Arithmetic ADC   SBC   INC   DEC   INX   DEX   INY   DEY
-- DONE: Shift      ASL   LSR   ROL   ROR
-- TODO: Bitwise    AND   ORA   EOR   BIT
-- DONE: Compare    CMP   CPX   CPY
-- TODO: Branch     BCC   BCS   BEQ   BNE   BPL   BMI   BVC   BVS
-- DONE: Jump       JMP   JSR   RTS   BRK   RTI
-- DONE: Stack      PHA   PLA   PHP   PLP   TXS   TSX
-- TODO: Flags      CLC   SEC   CLI   SEI   CLD   SED   CLV
-- DONE: Other      NOP


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

   function Negative (Value : U8) return Boolean is
   begin
      return (Value and Negative_Flag_Bit) /= 0;
   end Negative;

   -- NOTE: The following set of functions are intended to handle the different
   -- addressing modes used by 6502 instructions. They pull one or more
   -- additional bytes from the instruction stream and use them to address
   -- memory

   function Immediate return U8 is
   begin
      return Memory (Program_Counter + 1);
   end Immediate;

   function Zero_Page return U16 is
   begin
      return U16 (Memory (Program_Counter + 1));
   end Zero_Page;

   function Zero_Page_X return U16 is
   begin
      return U16 (Memory (Program_Counter + 1) + Index_Register_X);
   end Zero_Page_X;

   function Zero_Page_Y return U16 is
   begin
      return U16 (Memory (Program_Counter + 1) + Index_Register_Y);
   end Zero_Page_Y;

   function Absolute return U16 is
   begin
      return Merge (Low  => Memory (Program_Counter + 1),
                    High => Memory (Program_Counter + 2));
   end Absolute;

   function Absolute_X return U16 is
   begin
      return Merge (Low  => Memory (Program_Counter + 1),
                    High => Memory (Program_Counter + 2)) + U16 (Index_Register_X);
   end Absolute_X;

   function Absolute_Y return U16 is
      Data1, Data2 : U8;
   begin
      return Merge (Low  => Memory (Program_Counter + 1),
                    High => Memory (Program_Counter + 2)) + U16 (Index_Register_Y);
   end Absolute_Y;

   function Indirect return U16 is
      Base : U16;
   begin
      Base := Merge (Low => Memory (Program_Counter + 1),
                     High => Memory (Program_Counter + 2));

      return Merge (Low  => Memory (Base + 0),
                    High => Memory (Base + 1));
   end Indirect;

   function Indirect_X return U16 is
      Base : U16;
   begin
      Base := U16 (Memory (Program_Counter + 1)) + U16 (Index_Register_X);
      return Merge (Low => Memory (Base + 0), High => Memory (Base + 1));
   end Indirect_X;

   function Indirect_Y return U16 is
      Base : U8;
   begin
      Base := Memory (Program_Counter + 1);
      return Merge (Low => Base + 0, High => Base + 1) + U16 (Index_Register_Y);
   end Indirect_Y;

   -- NOTE: Stack helper functions.

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

      Cycles : Integer := 0;
      Bytes  : Integer := 1;

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
         Negative_Flag := Negative (Value);
      end LDA;

      procedure LDX (Value : U8) is
      begin
         Put_Line ("LDX");
         Index_Register_X := Value;

         Zero_Flag     := (Value = 0);
         Negative_Flag := Negative (Value);
      end LDX;

      procedure LDY (Value : U8) is
      begin
         Put_Line ("LDY");
         Index_Register_Y := Value;

         Zero_Flag     := (Value = 0);
         Negative_Flag := Negative (Value);
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
         Negative_Flag := Negative (Index_Register_X);
      end TAX;

      procedure TAY is
      begin
         Put_Line ("TAY");
         Index_Register_Y := Accumulator;

         Zero_Flag     := (Index_Register_Y = 0);
         Negative_Flag := Negative (Index_Register_Y);
      end TAY;

      procedure TSX is
      begin
         Put_Line ("TSX");
         Index_Register_X := Stack_Pointer;

         Zero_Flag     := (Index_Register_X = 0);
         Negative_Flag := Negative (Index_Register_X);
      end TSX;

      procedure TXA is
      begin
         Put_Line ("TXA");
         Accumulator := Index_Register_X;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end TXA;

      procedure TXS is
      begin
         Put_Line ("TXS");
         Stack_Pointer := Index_Register_X;

         Zero_Flag     := (Stack_Pointer = 0);
         Negative_Flag := Negative (Stack_Pointer);
      end TXS;

      procedure TYA is
      begin
         Put_Line ("TYA");
         Accumulator := Index_Register_Y;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end TYA;

      -------------------------------------------------------------------

      procedure ADC (Value : U8) is
         Result : U16;
      begin
         Put_Line ("ADC");
         Result := U16 (Accumulator) + U16 (Value);
         if Carry_Flag then
            Result := Result + 1;
         end if;

         Accumulator := U8 (Result mod 256);

         Carry_Flag    := (Result > 16#FF#);
         Zero_Flag     := (Result = 0);
         Overflow_Flag := ((Result xor U16 (Accumulator)) and (Result xor U16 (Value)) and 16#80#) /= 0;
         Negative_Flag := Negative (Accumulator);
      end ADC;

      procedure SBC (Value : U8) is
         Result : U16;
      begin
         Put_Line ("SBC");
         Result := U16 (Accumulator) - U16 (Value);
         if Carry_Flag then
            Result := Result - (not 1);
         end if;

         Carry_Flag    := (Result > U16 (Value));
         Zero_Flag     := (Result = 0);
         Overflow_Flag := ((Result xor U16 (Accumulator)) and (Result xor (not U16 (Value))) and 16#80#) /= 0;
         Negative_Flag := Negative (Accumulator);
      end SBC;

      procedure INC (Value : in out U8) is
      begin
         Put_Line ("INC");
         Value := Value + 1;

         Zero_Flag     := (Value = 0);
         Negative_Flag := Negative (Value);
      end INC;

      procedure DEC (Value : in out U8) is
      begin
         Put_Line ("DEC");
         Value := Value - 1;

         Zero_Flag     := (Value = 0);
         Negative_Flag := Negative (Value);
      end DEC;

      procedure INX is
      begin
         Put_Line ("INX");
         Index_Register_X := Index_Register_X + 1;

         Zero_Flag     := (Index_Register_X = 0);
         Negative_Flag := Negative (Index_Register_X);
      end INX;

      procedure DEX is
      begin
         Put_Line ("DEX");
         Index_Register_X := Index_Register_X - 1;

         Zero_Flag     := (Index_Register_X = 0);
         Negative_Flag := Negative (Index_Register_X);
      end DEX;

      procedure INY is
      begin
         Put_Line ("INY");
         Index_Register_Y := Index_Register_Y + 1;

         Zero_Flag     := (Index_Register_Y = 0);
         Negative_Flag := Negative (Index_Register_Y);
      end INY;

      procedure DEY is
      begin
         Put_Line ("DEY");
         Index_Register_Y := Index_Register_Y - 1;

         Zero_Flag     := (Index_Register_Y = 0);
         Negative_Flag := Negative (Index_Register_Y);
      end DEY;

      ---------------------------------------------------------------

      procedure ASL (Value : in out U8) is
         Result, Previous : U8;
      begin
         Previous := Value;
         Result := Shift_Left (Previous, 1);

         Value := Result;

         Carry_Flag    := (Previous and 2#1000_0000#) /= 0;
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end ASL;

      procedure LSR (Value : in out U8) is
         Result, Previous : U8;
      begin
         Previous := Value;
         Result := Shift_Right (Previous, 1);

         Value := Result;

         Carry_Flag    := (Previous and 2#1000_0000#) /= 0;
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end LSR;

      procedure ROL (Value : in out U8) is
         Result, Previous, C : U8;
      begin
         C := (if Carry_Flag then 2#0000_0001# else 2#0000_0000#);

         Previous := Value;
         Result := Shift_Left (Previous, 1);
         Result := Result or C;

         Value := Result;

         Carry_Flag    := (Previous and 2#1000_0000#) /= 0;
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end ROL;

      procedure ROR (Value : in out U8) is
         Result, Previous, C : U8;
      begin
         C := (if Carry_Flag then 2#1000_0000# else 2#0000_0000#);

         Previous := Value;
         Result := Shift_Right (Previous, 1);
         Result := Result or C;

         Value := Result;

         Carry_Flag    := (Previous and 2#0000_0001#) /= 0;
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end ROR;

      --------------------------------------------------------------

      procedure CMP (Value : U8) is
         Result : U8;
      begin
         Put_Line ("CMP");
         Result := Value - Accumulator;

         Carry_Flag    := (Accumulator >= Value);
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end CMP;

      procedure CPX (Value : U8) is
         Result : U8;
      begin
         Put_Line ("CPX");
         Result := Value - Index_Register_X;

         Carry_Flag    := (Accumulator >= Value);
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end CPX;

      procedure CPY (Value : U8) is
         Result : U8;
      begin
         Put_Line ("CPY");
         Result := Value - Index_Register_Y;

         Carry_Flag    := (Accumulator >= Value);
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end CPY;

      ----------------------------------------------------------------

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

         if Bytes > 1 then
            Put ("   Data Byte 1     : ");
            U8_IO.Put (Data1, Base => 16);
            New_Line;
         end if;

         if Bytes > 2 then
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
            Cycles := 2;
            NOP;

            --------------------------------------------------------------------

         when 16#A9# => -- LDA #Immediate
            Bytes  := 2;
            Cycles := 2;
            LDA (Immediate);
         when 16#A5# => -- LDA Zero Page
            Bytes  := 2;
            Cycles := 3;
            LDA (Memory (Zero_Page));
         when 16#B5# => -- LDA Zero Page,X
            Bytes  := 2;
            Cycles := 4;
            LDA (Memory (Zero_Page_X));
         when 16#AD# => -- LDA Absolute
            Bytes  := 3;
            Cycles := 4;
            LDA (Memory (Absolute));
         when 16#BD# => -- LDA Absolute,X
            Bytes  := 3;
            Cycles := 4; -- TODO: 5 if page is crossed.
            LDA (Memory (Absolute_X));
         when 16#B9# => -- LDA Absolute,Y
            Bytes  := 3;
            Cycles := 4; -- TODO: 5 if page is crossed.
            LDA (Memory (Absolute_Y));
         when 16#A1# => -- LDA (Indirect,X)
            Bytes  := 2;
            Cycles := 6;
            LDA (Memory (Indirect_X));
         when 16#B1# => -- LDA (Indirect),Y
            Bytes  := 2;
            Cycles := 5; -- TODO: 6 if page is crossed.
            LDA (Memory (Indirect_Y));

         when 16#A2# => -- LDX #Immediate
            Bytes  := 2;
            Cycles := 2;
            LDX (Immediate);
         when 16#A6# => -- LDX Zero Page
            Bytes  := 2;
            Cycles := 3;
            LDX (Memory (Zero_Page));
         when 16#B6# => -- LDX Zero Page,X
            Bytes  := 2;
            Cycles := 4;
            LDX (Memory (Zero_Page_X));
         when 16#AE# => -- LDX Absolute
            Bytes  := 3;
            Cycles := 4;
            LDX (Memory (Absolute));
         when 16#BE# => -- LDX Absolute,X
            Bytes  := 3;
            Cycles := 4; -- TODO: 5 if page is crossed.
            LDX (Memory (Absolute_X));

         when 16#A0# => -- LDY #Immediate
            Bytes  := 2;
            Cycles := 2;
            LDY (Immediate);
         when 16#A4# => -- LDY Zero Page
            Bytes  := 2;
            Cycles := 3;
            LDY (Memory (Zero_Page));
         when 16#B4# => -- LDY Zero Page,X
            Bytes  := 2;
            Cycles := 4;
            LDY (Memory (Zero_Page_X));
         when 16#AC# => -- LDY Absolute
            Bytes  := 3;
            Cycles := 4;
            LDY (Memory (Absolute));
         when 16#BC# => -- LDY Absolute,X
            Bytes  := 3;
            Cycles := 4; -- TODO: 5 if page is crossed.
            LDY (Memory (Absolute_X));

         when 16#85# => -- STA Zero Page
            Bytes  := 2;
            Cycles := 3;
            STA (Zero_Page);
         when 16#95# => -- STA Zero Page,X
            Bytes  := 2;
            Cycles := 4;
            STA (Zero_Page_X);
         when 16#8D# => -- STA Absolute
            Bytes  := 3;
            Cycles := 4;
            STA (Absolute);
         when 16#9D# => -- STA Absolute,X
            Bytes  := 3;
            Cycles := 5;
            STA (Absolute_X);
         when 16#99# => -- STA Absolute,Y
            Bytes  := 3;
            Cycles := 5;
            STA (Absolute_Y);
         when 16#81# => -- STA (Indirect,X)
            Bytes  := 2;
            Cycles := 6;
            STA (Indirect_X);
         when 16#91# => -- STA (Indirect),Y
            Bytes  := 2;
            Cycles := 6;
            STA (Indirect_Y);

         when 16#86# => -- STX Zero Page
            Bytes  := 2;
            Cycles := 3;
            STX (Zero_Page);
         when 16#96# => -- STX Zero Page,Y
            Bytes  := 2;
            Cycles := 4;
            STX (Zero_Page_Y);
         when 16#8E# => -- STX Absolute
            Bytes  := 3;
            Cycles := 4;
            STX (Absolute);

         when 16#84# => -- STY Zero Page
            Bytes  := 2;
            Cycles := 3;
            STY (Zero_Page);
         when 16#94# => -- STY Zero Page,X
            Bytes  := 2;
            Cycles := 4;
            STY (Zero_Page_X);
         when 16#8C# => -- STY Absolute
            Bytes  := 3;
            Cycles := 4;
            STY (Absolute);

            --------------------------------------------------------------------

         when 16#AA# => -- TAX
            Bytes  := 1;
            Cycles := 2;
            TAX;

         when 16#A8# => -- TAY
            Bytes  := 1;
            Cycles := 2;
            TAY;

         when 16#BA# => -- TSX
            Bytes  := 1;
            Cycles := 2;
            TSX;

         when 16#8A# => -- TXA
            Bytes  := 1;
            Cycles := 2;
            TXA;

         when 16#9A# => -- TXS
            Bytes  := 1;
            Cycles := 2;
            TXS;

         when 16#98# => -- TYA
            Bytes  := 1;
            Cycles := 2;
            TYA;

            --------------------------------------------------------------------

         when 16#69# => -- ADC #Immediate
            Bytes  := 2;
            Cycles := 2;
            ADC (Immediate);
         when 16#65# => -- ADC Zero Page
            Bytes  := 2;
            Cycles := 3;
            ADC (Memory (Zero_Page));
         when 16#75# => -- ADC Zero Page,X
            Bytes  := 2;
            Cycles := 4;
            ADC (Memory (Zero_Page_X));
         when 16#6D# => -- ADC Absolute
            Bytes  := 3;
            Cycles := 4;
            ADC (Memory (Absolute));
         when 16#7D# => -- ADC Absolute,X
            Bytes  := 3;
            Cycles := 4; -- TODO: 5 if page crossed.
            ADC (Memory (Absolute_X));
         when 16#79# => -- ADC Absolute,Y
            Bytes  := 3;
            Cycles := 4; -- TODO: 5 if page crossed.
            ADC (Memory (Absolute_Y));
         when 16#61# => -- ADC (Indirect,X)
            Bytes  := 2;
            Cycles := 6;
            ADC (Memory (Indirect_X));
         when 16#71# => -- ADC (Indirect),Y
            Bytes  := 2;
            Cycles := 5; -- TODO: 6 if page crossed.
            ADC (Memory (Indirect_Y));

         when 16#E9# => -- SBC #Immediate
            Bytes  := 2;
            Cycles := 2;
            SBC (Immediate);
         when 16#E5# => -- SBC Zero Page
            Bytes  := 2;
            Cycles := 3;
            SBC (Memory (Zero_Page));
         when 16#F5# => -- SBC Zero Page,X
            Bytes  := 2;
            Cycles := 4;
            SBC (Memory (Zero_Page_X));
         when 16#ED# => -- SBC Absolute
            Bytes  := 3;
            Cycles := 4;
            SBC (Memory (Absolute));
         when 16#FD# => -- SBC Absolute,X
            Bytes  := 3;
            Cycles := 4; -- TODO: 5 if page crossed.
            SBC (Memory (Absolute_X));
         when 16#F9# => -- SBC Absolute,Y
            Bytes  := 3;
            Cycles := 4; -- TODO: 5 if page crossed.
            SBC (Memory (Absolute_Y));
         when 16#E1# => -- SBC (Indirect,X)
            Bytes  := 2;
            Cycles := 6;
            SBC (Memory (Indirect_X));
         when 16#F1# => -- SBC (Indirect),Y
            Bytes  := 2;
            Cycles := 5; -- TODO: 6 if page crossed.
            SBC (Memory (Indirect_Y));

         when 16#E6# => -- INC Zero Page
            Bytes  := 2;
            Cycles := 5;
            INC (Memory (Zero_Page));
         when 16#F6# => -- INC Zero Page,X
            Bytes  := 2;
            Cycles := 6;
            INC (Memory (Zero_Page_X));
         when 16#EE# => -- INC Absolute
            Bytes  := 3;
            Cycles := 6;
            INC (Memory (Absolute));
         when 16#FE# => -- INC Absolute_X
            Bytes  := 3;
            Cycles := 7;
            INC (Memory (Absolute_X));

         when 16#C6# => -- DEC Zero Page
            Bytes  := 2;
            Cycles := 5;
            DEC (Memory (Zero_Page));
         when 16#D6# => -- DEC Zero Page,X
            Bytes  := 2;
            Cycles := 6;
            DEC (Memory (Zero_Page_X));
         when 16#CE# => -- DEC Absolute
            Bytes  := 3;
            Cycles := 6;
            DEC (Memory (Absolute));
         when 16#DE# => -- DEC Absolute,X
            Bytes  := 3;
            Cycles := 7;
            DEC (Memory (Absolute_X));

         when 16#E8# => -- INX
            Bytes  := 1;
            Cycles := 2;
            INX;

         when 16#CA# => -- DEX
            Bytes  := 1;
            Cycles := 2;
            DEX;

         when 16#C8# => -- INY
            Bytes  := 1;
            Cycles := 2;
            INY;

         when 16#88# => -- DEY
            Bytes  := 1;
            Cycles := 2;
            DEY;

            --------------------------------------------------------------------

         when 16#0A# => -- ASL Accumulator
            Bytes  := 1;
            Cycles := 2;
            ASL (Accumulator);
         when 16#06# => -- ASL Zero Page
            Bytes  := 2;
            Cycles := 5;
            ASL (Memory (Zero_Page));
         when 16#16# => -- ASL Zero Page,X
            Bytes  := 2;
            Cycles := 6;
            ASL (Memory (Zero_Page_X));
         when 16#0E# => -- ASL Absolute
            Bytes  := 3;
            Cycles := 6;
            ASL (Memory (Absolute));
         when 16#1E# => -- ASL Absolute,X
            Bytes  := 3;
            Cycles := 7;
            ASL (Memory (Absolute_X));

         when 16#4A# => -- LSR Accumulator
            Bytes  := 1;
            Cycles := 2;
            LSR (Accumulator);
         when 16#46# => -- LSR Zero Page
            Bytes  := 2;
            Cycles := 5;
            LSR (Memory (Zero_Page));
         when 16#56# => -- LSR Zero Page,X
            Bytes  := 2;
            Cycles := 6;
            LSR (Memory (Zero_Page_X));
         when 16#4E# => -- LSR Absolute
            Bytes  := 3;
            Cycles := 6;
            LSR (Memory (Absolute));
         when 16#5E# => -- LSR Absolute,X
            Bytes  := 3;
            Cycles := 7;
            LSR (Memory (Absolute_X));

         when 16#2A# => -- ROL Accumulator
            Bytes  := 1;
            Cycles := 2;
            ROL (Accumulator);
         when 16#26# => -- ROL Zero Page
            Bytes  := 2;
            Cycles := 5;
            ROL (Memory (Zero_Page));
         when 16#36# => -- ROL Zero Page,X
            Bytes  := 2;
            Cycles := 6;
            LSR (Memory (Zero_Page_X));
         when 16#2E# => -- ROL Absolute
            Bytes  := 3;
            Cycles := 6;
            LSR (Memory (Absolute));
         when 16#3E# => -- ROL Absolute,X
            Bytes  := 3;
            Cycles := 7;
            ROL (Memory (Absolute_X));

         when 16#6A# => -- ROR Accumulator
            Bytes  := 1;
            Cycles := 2;
            ROR (Accumulator);
         when 16#66# => -- ROR Zero Page
            Bytes  := 2;
            Cycles := 5;
            ROR (Memory (Zero_Page));
         when 16#76# => -- ROR Zero Page,X
            Bytes  := 2;
            Cycles := 6;
            LSR (Memory (Zero_Page_X));
         when 16#6E# => -- ROR Absolute
            Bytes  := 3;
            Cycles := 6;
            LSR (Memory (Absolute));
         when 16#7E# => -- ROR Absolute,X
            Bytes  := 3;
            Cycles := 7;
            ROR (Memory (Absolute_X));

         --------------------------------------------------------------------

         when 16#C9# => -- CMP #Immediate
            Bytes  := 2;
            Cycles := 2;
            CMP (Immediate);
         when 16#C5# => -- CMP Zero Page
            Bytes  := 2;
            Cycles := 3;
            CMP (Memory (Zero_Page));
         when 16#D5# => -- CMP Zero Page,X
            Bytes  := 2;
            Cycles := 4;
            CMP (Memory (Zero_Page_X));
         when 16#CD# => -- CMP Absolute
            Bytes  := 3;
            Cycles := 4;
            CMP (Memory (Absolute));
         when 16#DD# => -- CMP Absolute,X
            Bytes  := 3;
            Cycles := 4; -- TODO: 5 if page crossed.
            CMP (Memory (Absolute_X));
         when 16#D9# => -- CMP Absolute,Y
            Bytes  := 3;
            Cycles := 4; -- TODO: 5 if page crossed.
            CMP (Memory (Absolute_Y));
         when 16#C1# => -- CMP (Indirect,X)
            Bytes  := 2;
            Cycles := 6;
            CMP (Memory (Indirect_X));
         when 16#D1# => -- CMP (Indirect),Y
            Bytes  := 2;
            Cycles := 5; -- TODO: 6 if page crossed.
            CMP (Memory (Indirect_Y));

         when 16#E0# => -- CPX #Immediate
            Bytes  := 2;
            Cycles := 2;
            CPX (Immediate);
         when 16#E4# => -- CPX Zero Page
            Bytes  := 2;
            Cycles := 3;
            CPX (Memory (Zero_Page));
         when 16#EC# => -- CPX Absolute
            Bytes  := 3;
            Cycles := 4;
            CPX (Memory (Absolute));
         when 16#C0# => -- CPY #Immediate
            Bytes  := 2;
            Cycles := 2;
            CPY (Immediate);
         when 16#C4# => -- CPY Zero Page
            Bytes  := 2;
            Cycles := 3;
            CPY (Memory (Zero_Page));
         when 16#CC# => -- CPY Absolute
            Bytes  := 3;
            Cycles := 4;
            CPY (Memory (Absolute));

            --------------------------------------------------------------------

         when 16#4C# => -- JMP Absolute
            Bytes  := 3;
            Cycles := 3;
            JMP (Absolute);
         when 16#6C# => -- JMP (Indirect)
            Bytes  := 3;
            Cycles := 5;
            JMP (Indirect);

         when 16#20# => -- JSR
            Bytes  := 3;
            Cycles := 6;
            JSR (Absolute);

         when 16#60# => -- RTS
            Bytes  := 1;
            Cycles := 6;
            RTS;

         when 16#00# => -- BRK
            Bytes  := 2; -- NOTE: BRK skips the following byte on return.
            Cycles := 7;
            BRK;

         when 16#40# => -- RTI
            Bytes  := 1;
            Cycles := 6;
            RTI;

            --------------------------------------------------------------------

         when 16#48# => -- PHA
            Bytes  := 1;
            Cycles := 3;
            PHA;

         when 16#08# => -- PHP
            Bytes  := 1;
            Cycles := 3;
            PHP;

         when 16#68# => -- PLA
            Bytes  := 1;
            Cycles := 4;
            PLA;

         when 16#28# => -- PLP
            Bytes  := 1;
            Cycles := 4;
            PLP;

            --------------------------------------------------------------------

         when others =>
            Put_Line ("*Unhandled Instruction*");
            Print_Instruction;

            raise Unimplemented_Instruction;
      end case;

      Print_Instruction;
      Program_Counter := Program_Counter + U16 (Bytes);

      return Cycles;
   end Decode_And_Execute;

   -----------------------------------------------------------------------------
end CPU;
