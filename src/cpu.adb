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

      Total_Cycles_Elapsed := 7;
   end Power_On;

   procedure Reset is
   begin
      Program_Counter   := 16#FFFC#;
      Stack_Pointer     := Stack_Pointer - 3;
      Interrupt_Disable := True;

      Total_Cycles_Elapsed := 0;
   end Reset;

   ---------------------------------------------------------------------------

   function Read (Address : U16) return U8 is
      Actual_Address : U16 := Address;
   begin
      if Address >= 16#1000# and Address <= 16#2000# then
         Actual_Address := Address - 16#1000#;
      end if;

      return CPU.Memory (Actual_Address);
   end Read;

   procedure Write (Address : U16; Value : U8) is
      Actual_Address : U16 := Address;
   begin
      if Address >= 16#1000# and Address <= 16#2000# then
         Actual_Address := Address - 16#1000#;
      end if;

      CPU.Memory (Actual_Address) := Value;
   end Write;

   ---------------------------------------------------------------------------

   function Negative (Value : U8) return Boolean is
   begin
      return (Value and Negative_Flag_Bit) /= 0;
   end Negative;

   function Overflow (Value : U8) return Boolean is
   begin
      return (Value and Overflow_Flag_Bit) /= 0;
   end Overflow;

   -----------------------------------------------------------------------------

   procedure Print_Stack is
   begin
      Put_Line ("+-------");
      for Address in (Stack_Top + U16 (Stack_Pointer)) .. Stack_Base loop
         Put ("|");
         U16_IO.Put (Address, Base => 16);
         Put (": ");
         U8_IO.Put (Read (Address), Base => 16);
         New_Line;
      end loop;
      Put_Line ("+-------");
   end Print_Stack;

   procedure Push8 (Value : U8) is
   begin
      if Stack_Pointer = 0 then
         raise Stack_Overflow;
      end if;

      Write (Stack_Top + U16 (Stack_Pointer), Value);
      Stack_Pointer := Stack_Pointer - 1;
   end Push8;

   procedure Push16 (Value : U16) is
      High, Low : U8;
   begin
      High := U8 (Shift_Right (Value, 8));
      Low  := U8 (Value and 16#00FF#);

      Push8 (High);
      Push8 (Low);
   end Push16;

   function Pop8 return U8 is
      Result : U8;
   begin
      if Stack_Pointer = 16#FD# then
         raise Stack_Underflow;
      end if;

      Stack_Pointer := Stack_Pointer + 1;
      Result        := Read (Stack_Top + U16 (Stack_Pointer));

      return Result;
   end Pop8;

   function Pop16 return U16 is
      High, Low : U8;
   begin
      Low  := Pop8;
      High := Pop8;

      return Merge (High => High, Low => Low);
   end Pop16;

   -- NOTE: The following set of functions are intended to handle the different
   -- addressing modes used by 6502 instructions. They pull one or more
   -- additional bytes from the instruction stream and use them to address
   -- memory

   function Immediate return U8 is
   begin
      return Read (Program_Counter + 1);
   end Immediate;

   function Relative return U8 is
   begin
      return Read (Program_Counter + 1);
   end Relative;

   function Zero_Page return U16 is
   begin
      return U16 (Read (Program_Counter + 1));
   end Zero_Page;

   function Zero_Page_X return U16 is
   begin
      return U16 (Read (Program_Counter + 1) + Index_Register_X);
   end Zero_Page_X;

   function Zero_Page_Y return U16 is
   begin
      return U16 (Read (Program_Counter + 1) + Index_Register_Y);
   end Zero_Page_Y;

   function Absolute return U16 is
   begin
      return Merge (Low  => Read (Program_Counter + 1),
                    High => Read (Program_Counter + 2));
   end Absolute;

   function Absolute_X return U16 is
   begin
      return Merge (Low  => Read (Program_Counter + 1),
                    High => Read (Program_Counter + 2)) + U16 (Index_Register_X);
   end Absolute_X;

   function Absolute_Y return U16 is
      Data1, Data2 : U8;
   begin
      return Merge (Low  => Read (Program_Counter + 1),
                    High => Read (Program_Counter + 2)) + U16 (Index_Register_Y);
   end Absolute_Y;

   function Indirect return U16 is
      Address1, Address2 : U16;
   begin
      Address1 := Merge (Low => Read (Program_Counter + 1),
                         High => Read (Program_Counter + 2));

      -- NOTE: Due to a bug in the 6502 CPU, if the provided address crosses a
      -- page boundary (i.e. it ends in FF), the CPU will fail to increment the
      -- page for the second byte. For example, JMP (03FF) read bytes at 03FF
      -- and 0300, instead of 03FF and 0400.

      Address2 := Address1 + 1;
      if (Address1 and 16#FF#) = 16#FF# then
         Address2 := Address2 - 16#0100#;
      end if;

      return Merge (Low  => Read (Address1),
                    High => Read (Address2));
   end Indirect;

   function Indirect_X return U16 is
      Base : U8;
   begin
      Base := Read (Program_Counter + 1) + Index_Register_X;
      return Merge (Low => Read (U16 (Base + 0)), High => Read (U16 (Base + 1)));
   end Indirect_X;

   function Indirect_Y return U16 is
      Base : U8;
   begin
      Base := Read (Program_Counter + 1);
      return Merge (Low => Base + 0, High => Base + 1) + U16 (Index_Register_Y);
   end Indirect_Y;

   -----------------------------------------------------------------------------

   procedure Decode_And_Execute is
      Unimplemented_Instruction : exception;

      Instruction, Family, Mode, Modifier : U8;

      Cycles : Integer := 0;
      Bytes  : Integer := 1;

      Data1, Data2 : U8 := 0;

      Jump_Occurred        : Boolean := False;
      Page_Crossed         : Boolean := False;
      Next_Program_Counter : U16     := Program_Counter;

      -----------------------------------------------------------------------
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
         Write (Address, Accumulator);
      end STA;

      procedure STX (Address : U16) is
      begin
         Write (Address, Index_Register_X);
      end STX;

      procedure STY (Address : U16) is
      begin
         Write (Address, Index_Register_Y);
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
      end TXS;

      procedure TYA is
      begin
         Accumulator := Index_Register_Y;

         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end TYA;

      -------------------------------------------------------------------

      procedure ADC (Value : U8) is
         Result16 : U16;
         Result   : U8;
      begin
         Result16 := U16 (Accumulator) + U16 (Value);
         if Carry_Flag then
            Result16 := Result16 + 1;
         end if;
         Result := U8 (Result16 mod 256);

         Carry_Flag    := (Result16 > 16#FF#);
         Zero_Flag     := (Result = 0);
         Overflow_Flag := ((Result xor Accumulator) and (Result xor Value) and 16#80#) /= 0;
         Negative_Flag := Negative (Result);

         Accumulator := Result;
      end ADC;

      procedure SBC (Value : U8) is
         Result : U8;
      begin
         Result := Accumulator + (not Value);
         if Carry_Flag then
            Result := Result + 1;
         end if;

         Carry_Flag    := (not (Result > 127));
         Zero_Flag     := (Result = 0);
         Overflow_Flag := ((Result xor Accumulator) and (Result xor (not Value)) and 16#80#) /= 0;
         Negative_Flag := Negative (Result);

         Accumulator := Result;
      end SBC;

      procedure INC (Address : U16) is
         Value : U8;
      begin
         Value := Read (Address) + 1;
         Write (Address, Value);

         Zero_Flag     := (Value = 0);
         Negative_Flag := Negative (Value);
      end INC;

      procedure DEC (Address : U16) is
         Value : U8;
      begin
         Value := Read (Address) - 1;
         Write (Address, Value);

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
         Result := Accumulator and Value;

         Zero_Flag     := (Result = 0);
         Overflow_Flag := Overflow (Value);
         Negative_Flag := Negative (Value);
      end BIT;

      --------------------------------------------------------------

      -- TODO: De-duplicate instructions that need to update both addresses and
      -- the accumulator register.

      procedure ASL is
         Previous : U8;
      begin
         Previous    := Accumulator;
         Accumulator := Shift_Left (Previous, 1);

         Carry_Flag    := (Previous and 2#1000_0000#) /= 0;
         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end ASL;

      procedure ASL (Address : U16) is
         Result, Previous : U8;
      begin
         Previous := Read (Address);
         Result   := Shift_Left (Previous, 1);
         Write (Address, Result);

         Carry_Flag    := (Previous and 2#1000_0000#) /= 0;
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end ASL;

      procedure LSR is
         Previous : U8;
      begin
         Previous    := Accumulator;
         Accumulator := Shift_Right (Previous, 1);

         Carry_Flag    := (Previous and 2#0000_0001#) /= 0;
         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end LSR;

      procedure LSR (Address : U16) is
         Result, Previous : U8;
      begin
         Previous := Read (Address);
         Result   := Shift_Right (Previous, 1);
         Write (Address, Result);

         Carry_Flag    := (Previous and 2#1000_0000#) /= 0;
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end LSR;

      procedure ROL is
         Previous, C : U8;
      begin
         C := (if Carry_Flag then 2#0000_0001# else 2#0000_0000#);

         Previous    := Accumulator;
         Accumulator := Shift_Left (Previous, 1) or C;

         Carry_Flag    := (Previous and 2#1000_0000#) /= 0;
         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end ROL;

      procedure ROL (Address : U16) is
         Result, Previous, C : U8;
      begin
         C := (if Carry_Flag then 2#0000_0001# else 2#0000_0000#);

         Previous := Read (Address);
         Result   := Shift_Left (Previous, 1);
         Result   := Result or C;
         Write (Address, Result);

         Carry_Flag    := (Previous and 2#1000_0000#) /= 0;
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end ROL;

      procedure ROR is
         Previous, C : U8;
      begin
         C := (if Carry_Flag then 2#1000_0000# else 2#0000_0000#);

         Previous    := Accumulator;
         Accumulator := Shift_Right (Previous, 1) or C;

         Carry_Flag    := (Previous and 2#0000_0001#) /= 0;
         Zero_Flag     := (Accumulator = 0);
         Negative_Flag := Negative (Accumulator);
      end ROR;

      procedure ROR (Address : U16) is
         Result, Previous, C : U8;
      begin
         C := (if Carry_Flag then 2#1000_0000# else 2#0000_0000#);

         Previous := Read (Address);
         Result   := Shift_Right (Previous, 1) or C;
         Write (Address, Result);

         Carry_Flag    := (Previous and 2#0000_0001#) /= 0;
         Zero_Flag     := (Result = 0);
         Negative_Flag := Negative (Result);
      end ROR;

      --------------------------------------------------------------

      procedure CMP (Value : U8) is
         Result : U8;
      begin
         Result := Accumulator - Value;

         Carry_Flag    := (Accumulator >= Value);
         Zero_Flag     := (Accumulator = Value);
         Negative_Flag := Negative (Result);
      end CMP;

      procedure CPX (Value : U8) is
         Result : U8;
      begin
         Result := Index_Register_X - Value;

         Carry_Flag    := (Index_Register_X >= Value);
         Zero_Flag     := (Index_Register_X = Value);
         Negative_Flag := Negative (Result);
      end CPX;

      procedure CPY (Value : U8) is
         Result : U8;
      begin
         Result := Index_Register_Y - Value;

         Carry_Flag    := (Index_Register_Y >= Value);
         Zero_Flag     := (Index_Register_Y = Value);
         Negative_Flag := Negative (Result);
      end CPY;

      ---------------------------------------------------------------

      procedure Branch (Offset : U8) is
         Signed_Offset : S8;
         Signed_Result : S32;
      begin
         Signed_Offset := S8'Val (Offset);
         Signed_Result := S32'Val (Program_Counter) + 2 + S32 (Signed_Offset);

         Jump_Occurred        := True;
         Next_Program_Counter := U16'Val (Signed_Result);

         -- NOTE: Branches that are taken consume an extra cycle.
         Cycles := Cycles + 1;
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
         Jump_Occurred        := True;
         Next_Program_Counter := Address;
      end JMP;

      procedure JSR (Address : U16) is
      begin
         Push16 (Program_Counter + 2);

         Jump_Occurred        := True;
         Next_Program_Counter := Address;
      end JSR;

      procedure BRK is
      begin
         Break_Command := True;
      end BRK;

      procedure RTS is
      begin
         Jump_Occurred        := True;
         Next_Program_Counter := Pop16 + 1;
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

         Jump_Occurred        := True;
         Next_Program_Counter := Pop16;
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

      procedure Print_State is
         TAB : Character := Character'Val (9);

         Display_Asm       : Boolean := True;
         Display_Registers : Boolean := True;
         Display_Flags     : Boolean := True;
      begin
         Put_Hex (Program_Counter);
         Put ("  ");

         Put_Hex (Instruction);
         Put (" ");

         if Bytes > 1 then
            Put_Hex (Read (Program_Counter + 1));
         else
            Put ("  ");
         end if;
         Put (" ");

         if Bytes > 2 then
            Put_Hex (Read (Program_Counter + 2));
         else
            Put ("  ");
         end if;
         Put (" ");
         Put (TAB);

         if Display_Asm then
            -- TODO: This does not take addressing modes into account, it just
            -- displays the follow-up bytes verbatim.
            Put (String (Instructions (Instruction).Symbol));
            if Bytes = 2 then
               Put (" $");
               Put_Hex (Read (Program_Counter + 1));
               Put ("  ");
            elsif Bytes = 3 then
               Put (" $");
               Put_Hex (Read (Program_Counter + 2));
               Put_Hex (Read (Program_Counter + 1));
            else
               Put ("      ");
            end if;
         end if;

         if Display_Registers then
            Put ("                      ");

            Put (" A:");
            Put_Hex (Accumulator);

            Put (" X:");
            Put_Hex (Index_Register_X);

            Put (" Y:");
            Put_Hex (Index_Register_Y);

            Put (" P:");
            Put_Hex (U8 (16#00#));

            Put (" SP:");
            Put_Hex (Stack_Pointer);

            Put (" PPU:  0,  0");

            Put (" CYC:");
            Put (Total_Cycles_Elapsed'Image);
         end if;

         if Display_Flags then
            for I in 1 .. 15 - Total_Cycles_Elapsed'Image'Length loop
               Put (" ");
            end loop;

            Put ("CF:" & Boolean'Pos (Carry_Flag)'Image);
            Put (" ZF:" & Boolean'Pos (Zero_Flag)'Image);
            Put (" ID:" & Boolean'Pos (Interrupt_Disable)'Image);
            Put (" DM:" & Boolean'Pos (Decimal_Mode)'Image);
            Put (" BC:" & Boolean'Pos (Break_Command)'Image);
            Put (" OF:" & Boolean'Pos (Overflow_Flag)'Image);
            Put (" NF:" & Boolean'Pos (Negative_Flag)'Image);
         end if;

         New_Line;
      end Print_State;

      --------------------------------------------------------------------------
   begin
      Instruction := Read (Program_Counter);
      Family      := Shift_Right (Instruction, 5) and 2#000_0111#;
      Mode        := Shift_Right (Instruction, 2) and 2#000_0111#;
      Modifier    := Shift_Right (Instruction, 0) and 2#000_0011#;

      Cycles := Instructions (Instruction).Cycles;
      Bytes  := Instructions (Instruction).Bytes;

      Print_State;

      case Instruction is
         when 16#EA# =>NOP;

         when 16#A9# =>LDA (Immediate);
         when 16#A5# =>LDA (Read (Zero_Page));
         when 16#B5# =>LDA (Read (Zero_Page_X));
         when 16#AD# =>LDA (Read (Absolute));
         when 16#BD# =>LDA (Read (Absolute_X));
         when 16#B9# =>LDA (Read (Absolute_Y));
         when 16#A1# =>LDA (Read (Indirect_X));
         when 16#B1# =>LDA (Read (Indirect_Y));

         when 16#85# =>STA (Zero_Page);
         when 16#95# =>STA (Zero_Page_X);
         when 16#8D# =>STA (Absolute);
         when 16#9D# =>STA (Absolute_X);
         when 16#99# =>STA (Absolute_Y);
         when 16#81# =>STA (Indirect_X);
         when 16#91# =>STA (Indirect_Y);

         when 16#A2# =>LDX (Immediate);
         when 16#A6# =>LDX (Read (Zero_Page));
         when 16#B6# =>LDX (Read (Zero_Page_X));
         when 16#AE# =>LDX (Read (Absolute));
         when 16#BE# =>LDX (Read (Absolute_X));

         when 16#86# =>STX (Zero_Page);
         when 16#96# =>STX (Zero_Page_Y);
         when 16#8E# =>STX (Absolute);

         when 16#A0# =>LDY (Immediate);
         when 16#A4# =>LDY (Read (Zero_Page));
         when 16#B4# =>LDY (Read (Zero_Page_X));
         when 16#AC# =>LDY (Read (Absolute));
         when 16#BC# =>LDY (Read (Absolute_X));

         when 16#84# =>STY (Zero_Page);
         when 16#94# =>STY (Zero_Page_X);
         when 16#8C# =>STY (Absolute);

         when 16#AA# =>TAX;
         when 16#8A# =>TXA;
         when 16#A8# =>TAY;
         when 16#98# =>TYA;

         when 16#69# =>ADC (Immediate);
         when 16#65# =>ADC (Read (Zero_Page));
         when 16#75# =>ADC (Read (Zero_Page_X));
         when 16#6D# =>ADC (Read (Absolute));
         when 16#7D# =>ADC (Read (Absolute_X));
         when 16#79# =>ADC (Read (Absolute_Y));
         when 16#61# =>ADC (Read (Indirect_X));
         when 16#71# =>ADC (Read (Indirect_Y));

         when 16#E9# =>SBC (Immediate);
         when 16#E5# =>SBC (Read (Zero_Page));
         when 16#F5# =>SBC (Read (Zero_Page_X));
         when 16#ED# =>SBC (Read (Absolute));
         when 16#FD# =>SBC (Read (Absolute_X));
         when 16#F9# =>SBC (Read (Absolute_Y));
         when 16#E1# =>SBC (Read (Indirect_X));
         when 16#F1# =>SBC (Read (Indirect_Y));

         when 16#E6# =>INC (Zero_Page);
         when 16#F6# =>INC (Zero_Page_X);
         when 16#EE# =>INC (Absolute);
         when 16#FE# =>INC (Absolute_X);

         when 16#C6# =>DEC (Zero_Page);
         when 16#D6# =>DEC (Zero_Page_X);
         when 16#CE# =>DEC (Absolute);
         when 16#DE# =>DEC (Absolute_X);

         when 16#E8# =>INX;
         when 16#CA# =>DEX;
         when 16#C8# =>INY;
         when 16#88# =>DEY;

         when 16#29# =>ANDA (Immediate);
         when 16#25# =>ANDA (Read (Zero_Page));
         when 16#35# =>ANDA (Read (Zero_Page_X));
         when 16#2D# =>ANDA (Read (Absolute));
         when 16#3D# =>ANDA (Read (Absolute_X));
         when 16#39# =>ANDA (Read (Absolute_Y));
         when 16#21# =>ANDA (Read (Indirect_X));
         when 16#31# =>ANDA (Read (Indirect_Y));

         when 16#09# =>ORA (Immediate);
         when 16#05# =>ORA (Read (Zero_Page));
         when 16#15# =>ORA (Read (Zero_Page_X));
         when 16#0D# =>ORA (Read (Absolute));
         when 16#1D# =>ORA (Read (Absolute_X));
         when 16#19# =>ORA (Read (Absolute_Y));
         when 16#01# =>ORA (Read (Indirect_X));
         when 16#11# =>ORA (Read (Indirect_Y));

         when 16#49# =>EOR (Immediate);
         when 16#45# =>EOR (Read (Zero_Page));
         when 16#55# =>EOR (Read (Zero_Page_X));
         when 16#4D# =>EOR (Read (Absolute));
         when 16#5D# =>EOR (Read (Absolute_X));
         when 16#59# =>EOR (Read (Absolute_Y));
         when 16#41# =>EOR (Read (Indirect_X));
         when 16#51# =>EOR (Read (Indirect_Y));

         when 16#24# =>BIT (Read (Zero_Page));
         when 16#2C# =>BIT (Read (Absolute));

         when 16#0A# =>ASL;
         when 16#06# =>ASL (Zero_Page);
         when 16#16# =>ASL (Zero_Page_X);
         when 16#0E# =>ASL (Absolute);
         when 16#1E# =>ASL (Absolute_X);

         when 16#4A# =>LSR;
         when 16#46# =>LSR (Zero_Page);
         when 16#56# =>LSR (Zero_Page_X);
         when 16#4E# =>LSR (Absolute);
         when 16#5E# =>LSR (Absolute_X);

         when 16#2A# =>ROL;
         when 16#26# =>ROL (Zero_Page);
         when 16#36# =>ROL (Zero_Page_X);
         when 16#2E# =>ROL (Absolute);
         when 16#3E# =>ROL (Absolute_X);

         when 16#6A# =>ROR;
         when 16#66# =>ROR (Zero_Page);
         when 16#76# =>ROR (Zero_Page_X);
         when 16#6E# =>ROR (Absolute);
         when 16#7E# =>ROR (Absolute_X);

         when 16#C9# =>CMP (Immediate);
         when 16#C5# =>CMP (Read (Zero_Page));
         when 16#D5# =>CMP (Read (Zero_Page_X));
         when 16#CD# =>CMP (Read (Absolute));
         when 16#DD# =>CMP (Read (Absolute_X));
         when 16#D9# =>CMP (Read (Absolute_Y));
         when 16#C1# =>CMP (Read (Indirect_X));
         when 16#D1# =>CMP (Read (Indirect_Y));

         when 16#E0# =>CPX (Immediate);
         when 16#E4# =>CPX (Read (Zero_Page));
         when 16#EC# =>CPX (Read (Absolute));
         when 16#C0# =>CPY (Immediate);
         when 16#C4# =>CPY (Read (Zero_Page));
         when 16#CC# =>CPY (Read (Absolute));

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

      Page_Crossed := (Program_Counter and 16#FF00#) /= (Next_Program_Counter and 16#FF00#);
      if Jump_Occurred and Page_Crossed then
         Cycles := Cycles + Instructions (Instruction).Page_Cross_Penalty;
      end if;
      Total_Cycles_Elapsed := Total_Cycles_Elapsed + Cycles;

      if Jump_Occurred then
         Program_Counter := Next_Program_Counter;
      else
         Program_Counter := Program_Counter + U16 (Bytes);
      end if;

   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         Put_Line ("CPU state at failure:");
         Print_State;
         Break_Command := True;
   end Decode_And_Execute;

end CPU;
