-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2023 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Tomas Dolak   xdolak09@stud.fit.vutbr.cz
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
    
    -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti, ze 
    --   - nelze z vice procesu ovladat stejny signal,
    --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty, protoze pak
    --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET a 
    --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene signaly. 
    
    -- STATE AUTOMAT
    type STATE_FSM is 
    (
        StateStart,                     -- Firts State After Start of CPU
        StateInit,
        StateInitContinue,
        StateFetch,                     -- State for Fetch Instruction
        StateLoadInstruction,           -- Load Instruction from INPUT
        StateIncrementPtr,              -- State of Incrementation of Ptr
        StateDecrementPtr,              -- State of Decrementation of Ptr
        StateIncrementVal,              -- State of Incrementation of Value of Ptr
        StateIncrementValEnableData,    -- State of Incrementation of Value of Ptr, Phase 2, Just Enable DATA
        StateIncrementValWriteData,
        StateIncrementValFinish,
        StateDecrementVal,              -- State of Decrementation of Value of Ptr
        StateDecrementValEnableData,    -- State of Decrementation of Value of Ptr, Phase 2, Just Enable DATA
        StateDecrementValWriteData,
        StateDecrementValFinish, 
        StateJumpIfZero,                -- State if Value = 0 -> Jump To Next Instruction after ']'
        StateJumpItsZero,               -- Make a Jump
        StateJumpItsZeroInstructionLook,
        StateJumpItsZeroContinueOrNot,
        StateJumpIfNotZero,             -- State if Value != 0 -> Jump To Next Instruction after '['
        StateJumpIfNotZeroLook,
        StateJumpIfNotZeroLootAtCounter,
        StateBreak,                     -- State of End Of Cycle
        StatePrintVal,                  -- Print Value of Actual Cell
        StatePrintValFinish,
        StateLoadAndStore,              -- Load Value of and Store In Actual Cell
        StateDataValid,
        StateReturn                     -- Stop The Program
        );

        -- Actual State of Automat
        signal ActualState      : STATE_FSM := StateStart;
        -- Next State of Automat
        signal NextState        : STATE_FSM := StateStart;
        
    -- SIGNALS 
    -- CNT:
    signal cnt_Inc          : std_logic;
    signal cnt_Dec          : std_logic;
    signal CNT_OUT          : std_logic_vector(7 downto 0);
    signal cnt_load         : std_logic;
    -- MUX1:
    signal MX1_OUT          : std_logic_vector(12 downto 0) := (others => '0');
    signal MX1_Selection    : std_logic_vector(1 downto 0);
    -- MUX2:
    signal MX2_OUT          : std_logic_vector(7 downto 0) := (others => '0');
    signal MX2_Selection    : std_logic_vector(1 downto 0);
    -- PTR:
    signal ptr_adr          : std_logic_vector(12 downto 0);
    signal ptr_Inc          : std_logic;
    signal ptr_Dec          : std_logic;
    -- PC:
    signal ProgCountMX      : std_logic_vector(12 downto 0);
    signal PC_Inc           : std_logic;
    signal PC_Dec           : std_logic;
    -- END OF SIGNALS 
    
begin
    
-- COUNTER
CNT: process (RESET,CLK,cnt_Inc,cnt_Dec,cnt_load)
begin
    if (RESET = '1') then
        CNT_OUT <= (others => '0');

    elsif rising_edge(CLK) then
        if (cnt_Inc = '1') then 
            CNT_OUT <= CNT_OUT + 1;
        elsif(cnt_load = '1') then 
            CNT_OUT <= X"01";
        elsif (cnt_Dec = '1') then 
            CNT_OUT <= CNT_OUT - 1;
        end if;
    end if;
end process;
-- END OF COUNTER

-- STATE REGISTER
StateReg: process (CLK, RESET,EN)
begin
    if (RESET = '1') then 
        ActualState <= StateStart;
    elsif rising_edge(CLK) and (EN = '1') then
        ActualState <= NextState;
        
    end if;
end process;
-- END OF STATE REGISTER

-- MUX 1
MUX1: process(RESET,MX1_Selection, ptr_adr, ProgCountMX)
begin
    if (RESET = '1') then
        MX1_OUT <= (others => '0');
    else
        case MX1_Selection is
            when "01" =>
                MX1_OUT <= ptr_adr;
            when "10" =>
                MX1_OUT <= ProgCountMX;
            when others =>
                MX1_OUT <= (others => '0');
        end case;
    end if;
end process; 

    DATA_ADDR <= MX1_OUT;
-- END OF MUX 1

-- MUX 2
MUX2: process(RESET,MX2_Selection,IN_DATA)
begin
    if (RESET = '1') then
        MX2_OUT <= (others => '0');
    else
        case MX2_Selection is
            when "01" =>
                MX2_OUT <= DATA_RDATA + 1;
            
            when "10" =>
                MX2_OUT <= DATA_RDATA - 1;
        
            when "11" =>
                MX2_OUT <= IN_DATA;
            when others =>
                MX2_OUT <= (others => '0');
        end case;
    end if;
end process;

    DATA_WDATA <= MX2_OUT;
-- END OF MUX 2

-- Ptr
Pointer: process(CLK,RESET)
begin
    if (RESET = '1') then
        ptr_adr <= (others => '0');
    elsif rising_edge(CLK) then                         -- Circular Buffer
        if ptr_Inc = '1' and ptr_adr = X"1FFF" then     -- End -> Start 
            ptr_adr <= (others => '0');
        elsif ptr_Inc = '1' then
            ptr_adr <= ptr_adr + 1;
        elsif ptr_Dec = '1' and ptr_adr = X"0000" then  -- Start -> End
            ptr_adr <= (others => '1');
        elsif ptr_Dec = '1' then
            ptr_adr <= ptr_adr - 1;
        end if;
    end if;
end process; 
-- END OF Ptr

ProgramCounter: process (CLK,RESET,PC_Inc,PC_Dec)
begin
    if (RESET = '1') then
        ProgCountMX <= (others => '0');
    elsif rising_edge(CLK) then
        if PC_Inc = '1' then 
            ProgCountMX <= ProgCountMX + 1;
        elsif PC_Dec = '1' then 
            ProgCountMX <= ProgCountMX - 1;
        end if;
    end if;
end process; 
-- END OF Program Counter PC END

Automat: process(ActualState,IN_VLD,DATA_RDATA,CNT_OUT,OUT_BUSY) 
begin
    -- Initial Values (Default)
    -- Pointer
    ptr_Inc         <= '0';
    ptr_Dec         <= '0';
    -- Program Counter
    PC_Inc          <= '0';
    PC_Dec          <= '0';
    -- Multiplexors
    MX1_Selection   <= "00";
    MX2_Selection   <= "00";
    -- Program Counter - Multiplexors
    cnt_Inc         <= '0';
    cnt_Dec         <= '0';
    cnt_load        <= '0';        
    
    DATA_EN         <= '0';
    OUT_WE          <= '0'; 
    DONE            <= '0';
    DATA_RDWR       <= '0';

    case ActualState is 
    -- Start of Processing 
        when StateStart =>
            READY           <= '0';             -- Start Of CPU (Inicialization Process)
            IN_REQ          <= '0';
            NextState       <= StateInit;

        when StateInit =>
            DATA_EN         <= '1';
            DATA_RDWR       <= '0';
            MX1_Selection   <= "01";
            NextState       <= StateInitContinue;
        
        when StateInitContinue =>
            if (X"40" = DATA_RDATA) then
                READY       <= '1';
                ptr_Inc     <= '1';
                NextState   <= StateFetch;      -- Inicialization DONE -> Ready For First Fetch 
            else 
                ptr_Inc     <= '1';             -- Set ptr_adr 
                NextState   <= StateInit;
            end if;

        when StateFetch =>
            DATA_EN         <= '1'; -- Allow Processing
            MX1_Selection   <= "10";
            NextState       <= StateLoadInstruction;
            
        when StateLoadInstruction =>
            case DATA_RDATA is 
                when X"3E" =>   -- Instruction: ">"
                    NextState <= StateIncrementPtr;

                when X"3C" =>   -- Instruction: "<"
                    NextState <= StateDecrementPtr;

                when X"2B" =>   -- Instruction: "+"
                    NextState <= StateIncrementVal;
    
                when X"2D" =>   -- Instruction: "-"
                    NextState <= StateDecrementVal;

                when X"5B" =>   -- Instruction: "["
                    NextState <= StateJumpIfZero;

                when X"5D" =>   -- Instruction: "]"
                    NextState <= StateJumpIfNotZero;

                when X"7E" =>   -- Instruction: "~"
                    NextState <= StateBreak;

                when X"2E" =>   -- Instruction: "."
                    NextState <= StatePrintVal;

                when X"2C" =>   -- Instruction: ","
                    NextState <= StateLoadAndStore;

                when X"40" =>   -- Instruction: "@"
                    NextState <= StateReturn;
                when others =>
                    PC_Inc    <= '1';   -- Skip If It Was Something Other
                    NextState <= StateFetch;
            end case;
        
        -- Real Processing Of The Instruction 
        
        -- INCREMENT INSTRUCTIONS
        -- Instruction ">" - 0x3E   (ptr += 1;)
        when StateIncrementPtr =>
            ptr_Inc                 <= '1';
            MX1_Selection           <= "01";            -- Adress of Next Instruction To MX1_OUT
            
            NextState               <= StateFetch;      -- Fetch of Next Instruction
            PC_Inc                  <= '1';

        -- Instruction "+" - 0x2B   (*ptr += 1;)
        when StateIncrementVal =>            
            DATA_EN                 <= '1';             --  Enable Work With Data
            MX1_Selection           <= "01";            --  Make ptr_adr As Output of MX1_OUT
            NextState               <= StateIncrementValEnableData;

        when StateIncrementValEnableData =>
            DATA_EN                 <= '1';             -- If DATA_EN 1 It Will Be Possible To Write Into Mem. by DATA_WDATA
            NextState               <= StateIncrementValWriteData;  -- Write DATA Into Mem 

        when StateIncrementValWriteData =>
            MX2_Selection           <= "01";            -- Increment RDATA
            MX1_Selection           <= "01";            -- MX1_OUT <- ptr_adr (dej na vystup adresu ptr) 
            DATA_RDWR               <= '1';             -- WriteEnable
            DATA_EN                 <= '1';             -- Write Into Mem
            NextState               <= StateIncrementValFinish;

        when StateIncrementValFinish =>
            DATA_EN                 <= '0';
            DATA_RDWR               <= '0';
            
            NextState               <= StateFetch;      -- Fetch Another Instruction
            PC_Inc                  <= '1';

        -- DECREMENT INSTRUCTIONS
        -- Instruction "<" - 0x3C   (ptr -= 1;)
        when StateDecrementPtr =>
            ptr_Dec                 <= '1';             -- Decrement Value of Pointer Address
            MX1_Selection           <= "01";            -- Set MX1 As Output -> PtrAddress
            
            NextState               <= StateFetch;             
            PC_Inc                  <= '1';             -- Increment Prog. Counter for Next Instruction
            
        -- Instruction "-" - 0x2D      (*ptr -= 1;)
        when StateDecrementVal =>    
            DATA_EN                 <= '1';             -- Enable Work With Data
            MX1_Selection           <= "01";            -- Set MX1 As Output -> PtrAddress
            NextState               <= StateDecrementValEnableData;

        when StateDecrementValEnableData =>
            DATA_EN                 <= '1';             -- Enable Work With Data Again
            NextState               <= StateDecrementValWriteData;

        when StateDecrementValWriteData =>    
            MX2_Selection           <= "10";            -- Decrement RDATA
            MX1_Selection           <= "01";            -- MX1_OUT <- ptr_adr (dej na vystup adresu ptr) 
            DATA_RDWR               <= '1';             -- WriteEnable
            DATA_EN                 <= '1';             -- Write Into Mem
            NextState               <= StateDecrementValFinish;    
        when StateDecrementValFinish =>
            DATA_RDWR               <= '0';             -- For Reading
            DATA_EN                 <= '0';             -- Done With Data
            
            NextState               <= StateFetch;      -- Fetch Another Instruction
            PC_Inc                  <= '1';             -- Increment Prog. Counter for Next Instruction


        -- CONDITIONAL JUMP INSTRUCTIONS
        -- Instruction "[" - 0x5B   (while(*ptr){)
        when StateJumpIfZero =>
            DATA_EN                 <= '1';             -- Enable Work With Data
            MX1_Selection           <= "01";
            if ("01" = MX1_Selection) then              -- MX1 OUT -> DATA ADDRESS
                if (X"00" = DATA_RDATA) then            -- If Loaded DATA Has Value 0 Jump
                    cnt_Inc         <= '1';             -- CNT Is For Correct Define of START/END Of While Cycle 
                    NextState       <= StateJumpItsZero;
                else 
                    NextState       <= StateFetch;      -- Fetch Next Instruction
                end if;
                    PC_Inc          <= '1';             -- Move To Next Instruction
            end if;

        when StateJumpItsZero => 
            MX1_Selection           <= "10";            -- MX1 OUT -> INSTRUCTION ADDRESS
            DATA_EN                 <= '1';             -- Enable Work With Data
            -- In This State There Is No Need To Increment or Decrement Anything (signals PC and cnt are set...)
            NextState       <= StateJumpItsZeroInstructionLook;

        when StateJumpItsZeroInstructionLook =>
            case DATA_RDATA is
                when X"5B" =>
                    cnt_Inc         <= '1';             -- Another Loop
                    DATA_EN         <= '1';             -- Enable Data
                when X"5D" =>            
                    cnt_Dec         <= '1';             -- Decrement Val of Loops
                    DATA_EN         <= '1';             -- Enable Data
                when others =>  
                    -- Do Nothing 
            end case;
            NextState               <= StateJumpItsZeroContinueOrNot;

        when StateJumpItsZeroContinueOrNot =>
            PC_Inc                  <= '1';
            if (X"00" = CNT_OUT) then                       -- If (0 == CNT)
                NextState           <= StateFetch;          -- Fetch Next Instruction
            else 
                NextState           <= StateJumpItsZero;
            end if;

        -- Instruction "]" - 0x5D   (})
        when StateJumpIfNotZero =>
            DATA_EN                 <= '1';                 -- Enable Work With Data
            MX1_Selection           <= "10";                -- MX1 OUT <- PC Address
            if (X"00" = DATA_RDATA) then 
                NextState           <= StateFetch;
                PC_Dec              <= '0';
                PC_Inc              <= '0';
            else
                cnt_Inc             <= '1';
                PC_Dec              <= '1';
                NextState           <= StateJumpIfNotZeroLook;
            end if;
            
        when StateJumpIfNotZeroLook =>  
            DATA_EN                 <= '1';                 -- Enable Work With DATA
            MX1_Selection           <= "10";                -- Instruction
            NextState           <= StateJumpIfNotZeroLootAtCounter;

        when StateJumpIfNotZeroLootAtCounter =>
            DATA_EN                 <= '1';                 -- Enable Work With DATA   
                    
            if (X"5B" = DATA_RDATA) then                    -- Instruction "[" 
                cnt_Dec             <= '1';                 -- Decrement Counter
                cnt_Inc             <= '0';                 -- Don't Allow To Increment  
            elsif (X"5D" = DATA_RDATA) then                 -- Instruction "]" 
                cnt_Inc             <= '1';                 -- Increment Counter
                cnt_Dec             <= '0';                 -- Don't Allow To Decrement
            end if;

            if (X"00" = CNT_OUT) then                       -- If Counter = 0, Its Possible To Catch Next Instruction
                PC_Dec              <= '0';
                PC_Inc              <= '1';
                NextState           <= StateFetch;
            else 
                PC_Dec              <= '1';                 -- Else Continue In Jumping
                NextState           <= StateJumpIfNotZeroLook; 
            end if;

        
        -- Instruction "~" - 0x7E   (break;)
        when StateBreak   =>                                -- Same Logic Like In "[" With Hardcoded Set Condition mem[ptr] == 0 
            cnt_load                <= '1';                 -- Set CNT_OUT = 1
            NextState               <= StateJumpItsZero;    -- Continue With Logic Of Instruction "["
            
        -- PRINT AND GET CHAR INSTRUCTIONS
        -- Instruction "." - 0x2E   (putchar(*ptr);)
        when StatePrintVal =>
            DATA_EN                 <= '1';                 -- Enable Work With Data
            DATA_RDWR               <= '0';                 -- VAlue DATA_ADDR = DATA_WDATA
            MX1_Selection           <= "01";                -- ptr address 
            NextState               <= StatePrintValFinish;

        when StatePrintValFinish =>
            if ('0' /= OUT_BUSY) then                       -- If CPU Is Not Busy
                PC_Inc              <= '0';                 -- Do Nothing With Program Counter
                PC_Dec              <= '0';   
                NextState           <= StatePrintVal;
            else     
                OUT_DATA            <= DATA_RDATA;          -- Send Read Data Out
                OUT_WE              <= '1';                 -- if '1' DATA_OUT -> LCD
                PC_Inc              <= '1';                 -- Next Instruction
                PC_Dec              <= '0';                 -- Dont Decrement
                NextState           <= StateFetch;
            end if;

        -- Instruction "," - 0x2C   (*ptr = getchar();)
        when StateLoadAndStore =>
            IN_REQ                  <= '1';                 -- Make A Request
            DATA_EN                 <= '1';                 -- Enable Data 
            MX2_Selection           <= "11";                -- Set As MX2 Output Input Data
            NextState               <= StateDataValid;

        when StateDataValid => 
            if ('0' = IN_VLD) then                          -- If DATA Are Not Valid 
                MX2_Selection       <= "11";                -- Set As MX2 Output Input Data
                IN_REQ              <= '1';                 -- Make A Request For Writing Again 
                NextState           <= StateLoadAndStore;
            elsif ('1' = IN_VLD) then                       -- If DATA Are Valid
                IN_REQ              <= '0';                 -- Disable Request
                MX2_Selection       <= "11";                -- Set As MX2 Output Input Data
                DATA_EN             <= '1';                 -- Enable Mem      
                DATA_RDWR           <= '1';                 -- Set Operation Write
                MX1_Selection       <= "01";                -- Write Them Into The Mem
                PC_Inc              <= '1';                 -- Increment Program Counter
                NextState           <= StateFetch;          -- Fetch Next Instruction
            end if;

        -- Instruction "@" - 0x40   (return;) 
        when StateReturn =>
            ptr_Inc                 <= '1';                 -- Pointer Increment 
            DONE                    <= '1';                 -- Set Signal VALID To One
            NextState               <= StateReturn;         -- Back To State Return 

        -- Other Instructions 
        when others     =>
            PC_Inc              <= '1';                     -- Program Counter Move
            NextState           <= StateFetch;              -- Fetch Next Instruction

        end case;
end process; 


end behavioral;
