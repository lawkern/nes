--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

package body CPU is

   procedure Power_On is
   begin
      Accumulator      := 0;
      Index_Register_X := 0;
      Index_Register_Y := 0;

      Program_Counter := 16#FFFC#;
      Stack_Pointer   := 16#FD#;

      Carry_Flag        := False;
      Zero_Flag         := False;
      Interrupt_Disable := True;
      Decimal_Mode      := False;
      Overflow_Flag     := False;
      Negative_Flag     := False;
   end Power_On;

   procedure Reset is
   begin
      Program_Counter   := 16#FFFC#;
      Stack_Pointer     := Stack_Pointer - 3;
      Interrupt_Disable := True;
   end Reset;

   -----------------------------------------------------------------------------

   procedure Print_Registers is
   begin
      U8_IO.Default_Base  := 16;
      U16_IO.Default_Base := 16;

      Put ("PC:");
      U16_IO.Put (Program_Counter);

      Put (" | SP: ");
      U8_IO.Put (Stack_Pointer);

      Put ("| A: ");
      U8_IO.Put (Accumulator);

      Put (" | X: ");
      U8_IO.Put (Index_Register_X);

      Put (" | Y: ");
      U8_IO.Put (Index_Register_Y);
      New_Line;

      Put ("CF:" & Boolean'Pos (Carry_Flag)'Image);
      Put (" | ZF:" & Boolean'Pos (Zero_Flag)'Image);
      Put (" | ID:" & Boolean'Pos (Interrupt_Disable)'Image);
      Put (" | DM:" & Boolean'Pos (Decimal_Mode)'Image);
      Put (" | BC:" & Boolean'Pos (Break_Command)'Image);
      Put (" | OF:" & Boolean'Pos (Overflow_Flag)'Image);
      Put (" | NF:" & Boolean'Pos (Negative_Flag)'Image);
      New_Line;
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

   function Overflow (Value : U8) return Boolean is
   begin
      return (Value and Overflow_Flag_Bit) /= 0;
   end Overflow;

   -- NOTE: The following set of functions are intended to handle the different
   -- addressing modes used by 6502 instructions. They pull one or more
   -- additional bytes from the instruction stream and use them to address
   -- memory

   function Immediate return U8 is
   begin
      return Memory (Program_Counter + 1);
   end Immediate;

   function Relative return U8 is
   begin
      return Memory (Program_Counter + 1);
   end Relative;

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
         null;
      end NOP;

      -------------------------------------------------------------------------
      procedure LDA (Value : U8) is
      begin
         Accumulator := Value;

         Zero_Flag     := (Value = 0);
         Negative_Flag := Negative (Value);
      end LDA;

      procedure LDX (Value : U8) is
      begin
         Index_Register_X := Value;

         Zero_Flag     := (Value = 0);
         Negative_Flag := Negative (Value);
      end LDX;

      procedure LDY (Value : U8) is
      begin
         Index_Register_Y := Value;

         Zero_Flag     := (Value = 0);
         Negative_Flag := Negative (Value);
      end LDY;

      procedure STA (Address : U16) is
      begin
         Memory (Address) := Accumulator;
      end STA;

      procedure STX (Address : U16) is
      begin
         Memory (Address) := Index_Register_X;
      end STX;

      procedure STY (Address : U16) is
      begin
         Memory (Address) := Index_Register_Y;
      end STY;

      ------------------------------------------------------------------------

      procedure TAX is
      begin
         Index_Register_X := Accumulator;

         Zero_Flag     := (Index_Register_X = 0);
         Negative_Flag := Negative (Index_Register_X);
      end TAX;

      procedure TAY is
      begin
         Index_Register_Y := Accumulator;

         Zero_Flag     := (Index_Register_Y = 0);
         Negative_Flag := Negative (Index_Register_Y);
      end TAY;

      procedure TSX is
      begin
         Index_Register_X := Stack_Pointer;

         Zero_Flag     := (Index_Register_X = 0);
         Negative_Flag := Negative (Index_Register_X);
      end TSX;

      procedure TXA is
      begin
         Accumulator := Index_Register_X;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end TXA;

      procedure TXS is
      begin
         Stack_Pointer := Index_Register_X;

         Zero_Flag     := (Stack_Pointer = 0);
         Negative_Flag := Negative (Stack_Pointer);
      end TXS;

      procedure TYA is
      begin
         Accumulator := Index_Register_Y;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end TYA;

      -------------------------------------------------------------------

      procedure ADC (Value : U8) is
         Result : U16;
      begin
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
         Value := Value + 1;

         Zero_Flag     := (Value = 0);
         Negative_Flag := Negative (Value);
      end INC;

      procedure DEC (Value : in out U8) is
      begin
         Value := Value - 1;

         Zero_Flag     := (Value = 0);
         Negative_Flag := Negative (Value);
      end DEC;

      procedure INX is
      begin
         Index_Register_X := Index_Register_X + 1;

         Zero_Flag     := (Index_Register_X = 0);
         Negative_Flag := Negative (Index_Register_X);
      end INX;

      procedure DEX is
      begin
         Index_Register_X := Index_Register_X - 1;

         Zero_Flag     := (Index_Register_X = 0);
         Negative_Flag := Negative (Index_Register_X);
      end DEX;

      procedure INY is
      begin
         Index_Register_Y := Index_Register_Y + 1;

         Zero_Flag     := (Index_Register_Y = 0);
         Negative_Flag := Negative (Index_Register_Y);
      end INY;

      procedure DEY is
      begin
         Index_Register_Y := Index_Register_Y - 1;

         Zero_Flag     := (Index_Register_Y = 0);
         Negative_Flag := Negative (Index_Register_Y);
      end DEY;

      ---------------------------------------------------------------

      procedure ANDA (Value : U8) is
      begin
         Accumulator := Accumulator and Value;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end ANDA;

      procedure ORA (Value : U8) is
      begin
         Accumulator := Accumulator or Value;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end ORA;

      procedure EOR (Value : U8) is
      begin
         Accumulator := Accumulator xor Value;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end EOR;

      procedure BIT (Value : U8) is
         Result : U8;
      begin
         Result := Accumulator xor Value;

         Zero_Flag     := (Result = 0);
         Overflow_Flag := Overflow (Result);
         Negative_Flag := Negative (Result);
      end BIT;

      --------------------------------------------------------------

      procedure ASL (Value : in out U8) is
         Result, Previous : U8;
      begin
         Previous := Value;
         Result   := Shift_Left (Previous, 1);

         Value := Result;

         Carry_Flag    := (Previous and 2#1000_0000#) /= 0;
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end ASL;

      procedure LSR (Value : in out U8) is
         Result, Previous : U8;
      begin
         Previous := Value;
         Result   := Shift_Right (Previous, 1);

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
         Result   := Shift_Left (Previous, 1);
         Result   := Result or C;

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
         Result   := Shift_Right (Previous, 1);
         Result   := Result or C;

         Value := Result;

         Carry_Flag    := (Previous and 2#0000_0001#) /= 0;
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end ROR;

      --------------------------------------------------------------

      procedure CMP (Value : U8) is
         Result : U8;
      begin
         Result := Value - Accumulator;

         Carry_Flag    := (Accumulator >= Value);
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end CMP;

      procedure CPX (Value : U8) is
         Result : U8;
      begin
         Result := Value - Index_Register_X;

         Carry_Flag    := (Accumulator >= Value);
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end CPX;

      procedure CPY (Value : U8) is
         Result : U8;
      begin
         Result := Value - Index_Register_Y;

         Carry_Flag    := (Accumulator >= Value);
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end CPY;

      ---------------------------------------------------------------

      procedure Branch (Offset : U8) is
         Signed_Offset : S8;
         Signed_Result : S16;
      begin
         Signed_Offset := S8'Val (Offset);
         Signed_Result := S16'Val (Program_Counter) + 2 + S16 (Signed_Offset);

         Program_Counter := U16'Val (Signed_Result);

         -- NOTE: Branches that are taken consume an extra cycle.
         Bytes := Bytes + 1;
      end Branch;

      procedure BCC (Offset : U8) is
      begin
         if not Carry_Flag then
            Branch (Offset);
         end if;
      end BCC;

      procedure BCS (Offset : U8) is
      begin
         if Carry_Flag then
            Branch (Offset);
         end if;
      end BCS;

      procedure BEQ (Offset : U8) is
      begin
         if Zero_Flag then
            Branch (Offset);
         end if;
      end BEQ;

      procedure BNE (Offset : U8) is
      begin
         if not Zero_Flag then
            Branch (Offset);
         end if;
      end BNE;

      procedure BPL (Offset : U8) is
      begin
         if not Negative_Flag then
            Branch (Offset);
         end if;
      end BPL;

      procedure BMI (Offset : U8) is
      begin
         if Negative_Flag then
            Branch (Offset);
         end if;
      end BMI;

      procedure BVC (Offset : U8) is
      begin
         if not Overflow_Flag then
            Branch (Offset);
         end if;
      end BVC;

      procedure BVS (Offset : U8) is
      begin
         if Overflow_Flag then
            Branch (Offset);
         end if;
      end BVS;

      ----------------------------------------------------------------

      procedure JMP (Address : U16) is
      begin
         Program_Counter := Address;
      end JMP;

      procedure JSR (Address : U16) is
      begin
         Push16 (Program_Counter + 2);
         Program_Counter := Address;
      end JSR;

      procedure BRK is
      begin
         Break_Command := True;
      end BRK;

      procedure RTS is
      begin
         Program_Counter := Pop16 + 1;
      end RTS;

      procedure RTI is
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
      end RTI;

      -----------------------------------------------------------------

      procedure PHA is
      begin
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
         Push8 (Pack_Flags);
      end PHP;

      procedure PLA is
      begin
         Accumulator := Pop8;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := (Shift_Right (Accumulator, 7) and 1) = 1;
      end PLA;

      procedure PLP is
         Flags : U8;
      begin
         Flags := Pop8;

         Carry_Flag        := (Flags and Carry_Flag_Bit) /= 0;
         Zero_Flag         := (Flags and Zero_Flag_Bit) /= 0;
         Interrupt_Disable := (Flags and Interrupt_Disable_Bit) /= 0; -- TODO: Delay effect by one instruction.
         Decimal_Mode      := (Flags and Decimal_Mode_Bit) /= 0;
         Overflow_Flag     := (Flags and Overflow_Flag_Bit) /= 0;
         Negative_Flag     := (Flags and Negative_Flag_Bit) /= 0;
      end PLP;

      ---------------------------------------------------------------

      procedure CLC is
      begin
         Carry_Flag := False;
      end CLC;
      procedure SEC is
      begin
         Carry_Flag := True;
      end SEC;

      procedure CLI is
      begin
         Interrupt_Disable := False;
      end CLI;
      procedure SEI is
      begin
         Interrupt_Disable := True;
      end SEI;

      procedure CLD is
      begin
         Decimal_Mode := False;
      end CLD;
      procedure SED is
      begin
         Decimal_Mode := True;
      end SED;

      procedure CLV is
      begin
         Overflow_Flag := False;
      end CLV;

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

         Put_Line ("   Cycles          : " & Cycles'Image);
         Put_Line ("   Bytes           : " & Bytes'Image);

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
      Instruction := Memory (Program_Counter);
      Family      := Shift_Right (Instruction, 5) and 2#000_0111#;
      Mode        := Shift_Right (Instruction, 2) and 2#000_0111#;
      Modifier    := Shift_Right (Instruction, 0) and 2#000_0011#;

      Cycles := Instructions (Instruction).Cycles;
      Bytes  := Instructions (Instruction).Bytes;

      case Instruction is
         when 16#EA# =>NOP;

         when 16#A9# =>LDA (Immediate);
         when 16#A5# =>LDA (Memory (Zero_Page));
         when 16#B5# =>LDA (Memory (Zero_Page_X));
         when 16#AD# =>LDA (Memory (Absolute));
         when 16#BD# =>LDA (Memory (Absolute_X));
         when 16#B9# =>LDA (Memory (Absolute_Y));
         when 16#A1# =>LDA (Memory (Indirect_X));
         when 16#B1# =>LDA (Memory (Indirect_Y));

         when 16#85# =>STA (Zero_Page);
         when 16#95# =>STA (Zero_Page_X);
         when 16#8D# =>STA (Absolute);
         when 16#9D# =>STA (Absolute_X);
         when 16#99# =>STA (Absolute_Y);
         when 16#81# =>STA (Indirect_X);
         when 16#91# =>STA (Indirect_Y);

         when 16#A2# =>LDX (Immediate);
         when 16#A6# =>LDX (Memory (Zero_Page));
         when 16#B6# =>LDX (Memory (Zero_Page_X));
         when 16#AE# =>LDX (Memory (Absolute));
         when 16#BE# =>LDX (Memory (Absolute_X));

         when 16#86# =>STX (Zero_Page);
         when 16#96# =>STX (Zero_Page_Y);
         when 16#8E# =>STX (Absolute);

         when 16#A0# =>LDY (Immediate);
         when 16#A4# =>LDY (Memory (Zero_Page));
         when 16#B4# =>LDY (Memory (Zero_Page_X));
         when 16#AC# =>LDY (Memory (Absolute));
         when 16#BC# =>LDY (Memory (Absolute_X));

         when 16#84# =>STY (Zero_Page);
         when 16#94# =>STY (Zero_Page_X);
         when 16#8C# =>STY (Absolute);

         when 16#AA# =>TAX;
         when 16#8A# =>TXA;
         when 16#A8# =>TAY;
         when 16#98# =>TYA;

         when 16#69# =>ADC (Immediate);
         when 16#65# =>ADC (Memory (Zero_Page));
         when 16#75# =>ADC (Memory (Zero_Page_X));
         when 16#6D# =>ADC (Memory (Absolute));
         when 16#7D# =>ADC (Memory (Absolute_X));
         when 16#79# =>ADC (Memory (Absolute_Y));
         when 16#61# =>ADC (Memory (Indirect_X));
         when 16#71# =>ADC (Memory (Indirect_Y));

         when 16#E9# =>SBC (Immediate);
         when 16#E5# =>SBC (Memory (Zero_Page));
         when 16#F5# =>SBC (Memory (Zero_Page_X));
         when 16#ED# =>SBC (Memory (Absolute));
         when 16#FD# =>SBC (Memory (Absolute_X));
         when 16#F9# =>SBC (Memory (Absolute_Y));
         when 16#E1# =>SBC (Memory (Indirect_X));
         when 16#F1# =>SBC (Memory (Indirect_Y));

         when 16#E6# =>INC (Memory (Zero_Page));
         when 16#F6# =>INC (Memory (Zero_Page_X));
         when 16#EE# =>INC (Memory (Absolute));
         when 16#FE# =>INC (Memory (Absolute_X));

         when 16#C6# =>DEC (Memory (Zero_Page));
         when 16#D6# =>DEC (Memory (Zero_Page_X));
         when 16#CE# =>DEC (Memory (Absolute));
         when 16#DE# =>DEC (Memory (Absolute_X));

         when 16#E8# =>INX;
         when 16#CA# =>DEX;
         when 16#C8# =>INY;
         when 16#88# =>DEY;

         when 16#29# =>ANDA (Immediate);
         when 16#25# =>ANDA (Memory (Zero_Page));
         when 16#35# =>ANDA (Memory (Zero_Page_X));
         when 16#2D# =>ANDA (Memory (Absolute));
         when 16#3D# =>ANDA (Memory (Absolute_X));
         when 16#39# =>ANDA (Memory (Absolute_Y));
         when 16#21# =>ANDA (Memory (Indirect_X));
         when 16#31# =>ANDA (Memory (Indirect_Y));

         when 16#09# =>ORA (Immediate);
         when 16#05# =>ORA (Memory (Zero_Page));
         when 16#15# =>ORA (Memory (Zero_Page_X));
         when 16#0D# =>ORA (Memory (Absolute));
         when 16#1D# =>ORA (Memory (Absolute_X));
         when 16#19# =>ORA (Memory (Absolute_Y));
         when 16#01# =>ORA (Memory (Indirect_X));
         when 16#11# =>ORA (Memory (Indirect_Y));

         when 16#49# =>EOR (Immediate);
         when 16#45# =>EOR (Memory (Zero_Page));
         when 16#55# =>EOR (Memory (Zero_Page_X));
         when 16#4D# =>EOR (Memory (Absolute));
         when 16#5D# =>EOR (Memory (Absolute_X));
         when 16#59# =>EOR (Memory (Absolute_Y));
         when 16#41# =>EOR (Memory (Indirect_X));
         when 16#51# =>EOR (Memory (Indirect_Y));

         when 16#24# =>BIT (Memory (Zero_Page));
         when 16#2C# =>BIT (Memory (Absolute));

         when 16#0A# =>ASL (Accumulator);
         when 16#06# =>ASL (Memory (Zero_Page));
         when 16#16# =>ASL (Memory (Zero_Page_X));
         when 16#0E# =>ASL (Memory (Absolute));
         when 16#1E# =>ASL (Memory (Absolute_X));

         when 16#4A# =>LSR (Accumulator);
         when 16#46# =>LSR (Memory (Zero_Page));
         when 16#56# =>LSR (Memory (Zero_Page_X));
         when 16#4E# =>LSR (Memory (Absolute));
         when 16#5E# =>LSR (Memory (Absolute_X));

         when 16#2A# =>ROL (Accumulator);
         when 16#26# =>ROL (Memory (Zero_Page));
         when 16#36# =>ROL (Memory (Zero_Page_X));
         when 16#2E# =>ROL (Memory (Absolute));
         when 16#3E# =>ROL (Memory (Absolute_X));

         when 16#6A# =>ROR (Accumulator);
         when 16#66# =>ROR (Memory (Zero_Page));
         when 16#76# =>ROR (Memory (Zero_Page_X));
         when 16#6E# =>ROR (Memory (Absolute));
         when 16#7E# =>ROR (Memory (Absolute_X));

         when 16#C9# =>CMP (Immediate);
         when 16#C5# =>CMP (Memory (Zero_Page));
         when 16#D5# =>CMP (Memory (Zero_Page_X));
         when 16#CD# =>CMP (Memory (Absolute));
         when 16#DD# =>CMP (Memory (Absolute_X));
         when 16#D9# =>CMP (Memory (Absolute_Y));
         when 16#C1# =>CMP (Memory (Indirect_X));
         when 16#D1# =>CMP (Memory (Indirect_Y));

         when 16#E0# =>CPX (Immediate);
         when 16#E4# =>CPX (Memory (Zero_Page));
         when 16#EC# =>CPX (Memory (Absolute));
         when 16#C0# =>CPY (Immediate);
         when 16#C4# =>CPY (Memory (Zero_Page));
         when 16#CC# =>CPY (Memory (Absolute));

         when 16#90# =>BCC (Relative);
         when 16#B0# =>BCS (Relative);
         when 16#F0# =>BEQ (Relative);
         when 16#D0# =>BNE (Relative);
         when 16#10# =>BPL (Relative);
         when 16#30# =>BMI (Relative);
         when 16#50# =>BVC (Relative);
         when 16#70# =>BVS (Relative);

         when 16#4C# =>JMP (Absolute);
         when 16#6C# =>JMP (Indirect);
         when 16#20# =>JSR (Absolute);
         when 16#60# =>RTS;
         when 16#00# =>BRK;
         when 16#40# =>RTI;

         when 16#48# =>PHA;
         when 16#08# =>PHP;
         when 16#68# =>PLA;
         when 16#28# =>PLP;
         when 16#BA# =>TSX;
         when 16#9A# =>TXS;

         when 16#18# =>CLC;
         when 16#38# =>SEC;
         when 16#58# =>CLI;
         when 16#78# =>SEI;
         when 16#D8# =>CLD;
         when 16#F8# =>SED;
         when 16#B8# =>CLV;

         when others =>raise Unimplemented_Instruction;
      end case;

      Put_Line (String (Instructions (Instruction).Symbol));
      Print_Instruction;

      Program_Counter := Program_Counter + U16 (Bytes);

      return Cycles;
   end Decode_And_Execute;

end CPU;
