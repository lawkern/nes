--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Shared; use Shared;
with PPU;    use PPU;

package CPU is

   -- NOTE: Registers.
   Program_Counter  : U16;
   Stack_Pointer    : U8;
   Accumulator      : U8;
   Index_Register_X : U8;
   Index_Register_Y : U8;

   -- NOTE: Processor Status.
   Carry_Flag        : Boolean;
   Zero_Flag         : Boolean;
   Interrupt_Disable : Boolean;
   Decimal_Mode      : Boolean;
   Break_Command     : Boolean;
   Overflow_Flag     : Boolean;
   Negative_Flag     : Boolean;

   Carry_Flag_Bit        : constant U8 := 2#0000_0001#;
   Zero_Flag_Bit         : constant U8 := 2#0000_0010#;
   Interrupt_Disable_Bit : constant U8 := 2#0000_0100#;
   Decimal_Mode_Bit      : constant U8 := 2#0000_1000#;
   Break_Command_Bit     : constant U8 := 2#0001_0000#;
   Overflow_Flag_Bit     : constant U8 := 2#0100_0000#;
   Negative_Flag_Bit     : constant U8 := 2#1000_0000#;

   Carry_Flag_Mask        : constant U8 := not Carry_Flag_Bit;
   Zero_Flag_Mask         : constant U8 := not Zero_Flag_Bit;
   Interrupt_Disable_Mask : constant U8 := not Interrupt_Disable_Bit;
   Decimal_Mode_Mask      : constant U8 := not Decimal_Mode_Bit;
   Break_Command_Mask     : constant U8 := not Break_Command_Bit;
   Overflow_Flag_Mask     : constant U8 := not Overflow_Flag_Bit;
   Negative_Flag_Mask     : constant U8 := not Negative_Flag_Bit;


   -- NOTE: CPU memory.
   type Address_Space is array (U16) of U8;
   Memory : Address_Space;

   -- NOTE: The 256-byte stack grows downward from 01FF to 0100, indexed by the
   -- 8-bit Stack_Pointer register.
   Stack_Base : constant U16 := 16#01FF#;
   Stack_Top  : constant U16 := 16#0100#;

   Stack_Overflow, Stack_Underflow : exception;

   function Read (Address : U16) return U8;
   procedure Write (Address : U16; Value : U8);


   -- NOTE: CPU instructions.
   type Instruction_Stream is array (Natural range <>) of U8;

   type Instruction_String is new String (1 .. 3);
   type Instruction_Info is record
      Symbol             : Instruction_String;
      Bytes, Cycles      : Natural;
      Page_Cross_Penalty : Natural := 0;
   end record;

   -- Official instructions:
   -- -----------------------------------
   -- DONE: Access     LDA   STA   LDX   STX   LDY   STY
   -- DONE: Transfer   TAX   TXA   TAY   TYA
   -- DONE: Arithmetic ADC   SBC   INC   DEC   INX   DEX   INY   DEY
   -- DONE: Shift      ASL   LSR   ROL   ROR
   -- DONE: Bitwise    AND   ORA   EOR   BIT
   -- DONE: Compare    CMP   CPX   CPY
   -- DONE: Branch     BCC   BCS   BEQ   BNE   BPL   BMI   BVC   BVS
   -- DONE: Jump       JMP   JSR   RTS   BRK   RTI
   -- DONE: Stack      PHA   PLA   PHP   PLP   TXS   TSX
   -- DONE: Flags      CLC   SEC   CLI   SEI   CLD   SED   CLV
   -- DONE: Other      NO

   type Instruction_Table is array (U8) of Instruction_Info;
   Instructions : constant Instruction_Table :=
     (16#EA# => ("NOP", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),

      -- NOTE: Access instructions.
      16#A9# => ("LDA", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#A5# => ("LDA", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#B5# => ("LDA", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#AD# => ("LDA", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),
      16#BD# => ("LDA", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#B9# => ("LDA", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#A1# => ("LDA", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#B1# => ("LDA", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 1),

      16#85# => ("STA", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#95# => ("STA", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#8D# => ("STA", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),
      16#9D# => ("STA", Bytes => 3, Cycles => 5, Page_Cross_Penalty => 0),
      16#99# => ("STA", Bytes => 3, Cycles => 5, Page_Cross_Penalty => 0),
      16#81# => ("STA", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#91# => ("STA", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),

      16#A2# => ("LDX", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#A6# => ("LDX", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#B6# => ("LDX", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#AE# => ("LDX", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),
      16#BE# => ("LDX", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),

      16#86# => ("STX", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#96# => ("STX", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#8E# => ("STX", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),

      16#A0# => ("LDY", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#A4# => ("LDY", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#B4# => ("LDY", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#AC# => ("LDY", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),
      16#BC# => ("LDY", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),

      16#84# => ("STY", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#94# => ("STY", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#8C# => ("STY", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),

      -- NOTE: Transfer instructions.
      16#AA# => ("TAX", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#8A# => ("TXA", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#A8# => ("TAY", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#98# => ("TYA", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),

      -- NOTE: Arithmetic instructions.
      16#69# => ("ADC", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#65# => ("ADC", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#75# => ("ADC", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#6D# => ("ADC", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),
      16#7D# => ("ADC", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#79# => ("ADC", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#61# => ("ADC", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#71# => ("ADC", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 1),

      16#E9# => ("SBC", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#E5# => ("SBC", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#F5# => ("SBC", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#ED# => ("SBC", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),
      16#FD# => ("SBC", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#F9# => ("SBC", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#E1# => ("SBC", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#F1# => ("SBC", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 1),

      16#E6# => ("INC", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 0),
      16#F6# => ("INC", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#EE# => ("INC", Bytes => 3, Cycles => 6, Page_Cross_Penalty => 0),
      16#FE# => ("INC", Bytes => 3, Cycles => 7, Page_Cross_Penalty => 0),

      16#C6# => ("DEC", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 0),
      16#D6# => ("DEC", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#CE# => ("DEC", Bytes => 3, Cycles => 6, Page_Cross_Penalty => 0),
      16#DE# => ("DEC", Bytes => 3, Cycles => 7, Page_Cross_Penalty => 0),

      16#E8# => ("INX", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#CA# => ("DEX", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#C8# => ("INY", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#88# => ("DEY", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),

      -- NOTE: Shift instructions.
      16#0A# => ("ASL", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#06# => ("ASL", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 0),
      16#16# => ("ASL", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#0E# => ("ASL", Bytes => 3, Cycles => 6, Page_Cross_Penalty => 0),
      16#1E# => ("ASL", Bytes => 3, Cycles => 7, Page_Cross_Penalty => 0),

      16#4A# => ("LSR", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#46# => ("LSR", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 0),
      16#56# => ("LSR", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#4E# => ("LSR", Bytes => 3, Cycles => 6, Page_Cross_Penalty => 0),
      16#5E# => ("LSR", Bytes => 3, Cycles => 7, Page_Cross_Penalty => 0),

      16#2A# => ("ROL", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#26# => ("ROL", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 0),
      16#36# => ("ROL", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#2E# => ("ROL", Bytes => 3, Cycles => 6, Page_Cross_Penalty => 0),
      16#3E# => ("ROL", Bytes => 3, Cycles => 7, Page_Cross_Penalty => 0),

      16#6A# => ("ROR", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#66# => ("ROR", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 0),
      16#76# => ("ROR", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#6E# => ("ROR", Bytes => 3, Cycles => 6, Page_Cross_Penalty => 0),
      16#7E# => ("ROR", Bytes => 3, Cycles => 7, Page_Cross_Penalty => 0),

      -- NOTE: Bitwise instructions.
      16#29# => ("AND", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#25# => ("AND", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#35# => ("AND", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#2D# => ("AND", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),
      16#3D# => ("AND", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#39# => ("AND", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#21# => ("AND", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#31# => ("AND", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 1),

      16#09# => ("ORA", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#05# => ("ORA", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#15# => ("ORA", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#0D# => ("ORA", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),
      16#1D# => ("ORA", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#19# => ("ORA", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#01# => ("ORA", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#11# => ("ORA", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 1),

      16#49# => ("EOR", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#45# => ("EOR", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#55# => ("EOR", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#4D# => ("EOR", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),
      16#5D# => ("EOR", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#59# => ("EOR", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#41# => ("EOR", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#51# => ("EOR", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 1),

      16#24# => ("BIT", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#2C# => ("BIT", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),

      -- NOTE: Compare instructions.
      16#C9# => ("CMP", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#C5# => ("CMP", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#D5# => ("CMP", Bytes => 2, Cycles => 4, Page_Cross_Penalty => 0),
      16#CD# => ("CMP", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),
      16#DD# => ("CMP", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#D9# => ("CMP", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 1),
      16#C1# => ("CMP", Bytes => 2, Cycles => 6, Page_Cross_Penalty => 0),
      16#D1# => ("CMP", Bytes => 2, Cycles => 5, Page_Cross_Penalty => 1),

      16#E0# => ("CPX", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#E4# => ("CPX", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#EC# => ("CPX", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),

      16#C0# => ("CPY", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 0),
      16#C4# => ("CPY", Bytes => 2, Cycles => 3, Page_Cross_Penalty => 0),
      16#CC# => ("CPY", Bytes => 3, Cycles => 4, Page_Cross_Penalty => 0),

      -- NOTE: Branch instructions.
      16#90# => ("BCC", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 1),
      16#B0# => ("BCS", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 1),
      16#F0# => ("BEQ", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 1),
      16#D0# => ("BNE", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 1),
      16#10# => ("BPL", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 1),
      16#30# => ("BMI", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 1),
      16#50# => ("BVC", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 1),
      16#70# => ("BVS", Bytes => 2, Cycles => 2, Page_Cross_Penalty => 1),

      -- NOTE: Jump instructions.
      16#4C# => ("JMP", Bytes => 3, Cycles => 3, Page_Cross_Penalty => 0),
      16#6C# => ("JMP", Bytes => 3, Cycles => 5, Page_Cross_Penalty => 0),
      16#20# => ("JSR", Bytes => 3, Cycles => 6, Page_Cross_Penalty => 0),
      16#60# => ("RTS", Bytes => 1, Cycles => 6, Page_Cross_Penalty => 0),
      16#00# => ("BRK", Bytes => 2, Cycles => 7, Page_Cross_Penalty => 0),
      16#40# => ("RTI", Bytes => 1, Cycles => 6, Page_Cross_Penalty => 0),

      -- NOTE: Stack instructions.
      16#48# => ("PHA", Bytes => 1, Cycles => 3, Page_Cross_Penalty => 0),
      16#08# => ("PHP", Bytes => 1, Cycles => 3, Page_Cross_Penalty => 0),
      16#68# => ("PLA", Bytes => 1, Cycles => 4, Page_Cross_Penalty => 0),
      16#28# => ("PLP", Bytes => 1, Cycles => 4, Page_Cross_Penalty => 0),
      16#BA# => ("TSX", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#9A# => ("TXS", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),

      -- NOTE: Flag instructions.
      16#18# => ("CLC", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#38# => ("SEC", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#58# => ("CLI", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#78# => ("SEI", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#D8# => ("CLD", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#F8# => ("SED", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),
      16#B8# => ("CLV", Bytes => 1, Cycles => 2, Page_Cross_Penalty => 0),

      others => ("???", Bytes => 1, Cycles => 0, Page_Cross_Penalty => 0));

   Total_Cycles_Elapsed : Natural := 0;

   procedure Power_On;
   procedure Reset;
   procedure Decode_And_Execute;
   procedure Print_Stack;

end CPU;
