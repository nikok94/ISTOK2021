
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

library work;
use work.main_properties.all;

library work;
use work.antibounce;

entity ControlLogic is
  generic(
    c_clk_div_value     : integer := 100
  );
  port(
    clk                 : in std_logic;
    rst                 : in std_logic;
    Start_In            : in std_logic;
    ExtStart_In         : in std_logic;
    
    ShooterEnable_Out   : out std_logic;
    ExtStart_Out        : out std_logic;

    shooter_ready       : in std_logic;
    shooter_running     : in std_logic;

    hreg_bank           : in SPI_REG_RW_TYPE;

    status              : out std_logic_vector(1 downto 0)
  );
end ControlLogic;

architecture behav of ControlLogic is

    signal Ext_Start_Int        : std_logic;

    signal clk_div              : std_logic;
    signal clk_div_cnt          : integer;
    signal state                : std_logic_vector(7 downto 0);
    
    constant c_sync_num         : integer := 2;
    constant c_sync_stage       : integer := 3;
    type sync_reg_type is array (c_sync_num-1 downto 0) of std_logic_vector(c_sync_stage - 1 downto 0);
    signal shregs               : sync_reg_type;
    signal ExtStart_Sync        : std_logic;
    signal ExtStart_Sync_d      : std_logic;
    signal ExtStart_Sync_Edge   : std_logic;
    signal Start_Sync           : std_logic;
    signal Start_Sync_d         : std_logic;
    signal Start_Sync_Edge      : std_logic;
    signal TimeOutCnt           : std_logic_vector(31 downto 0);
    signal ext_start_ctrl       : std_logic_vector(1 downto 0);
    signal TimeOutSig              : std_logic;
    signal enable               : std_logic;
    signal TimeOutValue         : std_logic_vector(31 downto 0);

begin

  process(clk, rst)
    begin
      if (rst = '1') then
        clk_div_cnt <= 0;
        clk_div <= '0';
      elsif rising_edge(clk) then
        if (clk_div_cnt = 0) then
          clk_div <= '1';
        else 
          clk_div <= '0';
        end if;
        if (clk_div_cnt < c_clk_div_value) then
          clk_div_cnt <= clk_div_cnt + 1;
        else 
          clk_div_cnt <= 0;
        end if;
      end if ;
  end process;

----------------------------
-- starts metastab sync proc begin

shregs(0)(0) <= ExtStart_In;
shregs(1)(0) <= Start_In;

gen_proc : for i in 0 to c_sync_num - 1 generate
  process(clk, rst)
  begin
    if (rst = '1') then
      shregs(i)(c_sync_stage-1 downto 1) <= (others => '0');
    elsif rising_edge(clk) then
      shregs(i)(c_sync_stage - 1 downto 1) <= shregs(i)(c_sync_stage - 2 downto 0);
    end if;
  end process;
end generate;

ExtStart_Sync   <= shregs(0)(c_sync_num - 1);
Start_Sync      <= shregs(1)(c_sync_num - 1);
-- starts metastab sync proc end
----------------------------

delay_proc :
  process(clk, rst)
  begin
    if (rst = '1') then
      Start_Sync_d <= '0';
      ExtStart_Sync_d <= '0';
    elsif rising_edge(clk) then
      Start_Sync_d <= Start_Sync;
      ExtStart_Sync_d <= ExtStart_Sync;
      Start_Sync_Edge <= (not Start_Sync_d) and Start_Sync;
      ExtStart_Sync_Edge <= (not ExtStart_Sync_d) and ExtStart_Sync;
    end if;
  end process;

ext_start_ctrl <= hreg_bank(SPI_RW_REG_STRUCT'pos(CONTROL))(CONTROL_STRUCT'pos(EXT_START_GATE)) & hreg_bank(SPI_RW_REG_STRUCT'pos(CONTROL))(CONTROL_STRUCT'pos(EXT_START_EDGE));
TimeOutValue <= hreg_bank(SPI_RW_REG_STRUCT'pos(TIMEOUT));


main_state_proc :
  process(clk, rst)
  begin
    if (rst = '1') then
      state <= (others => '0');
      TimeOutSig <= '0';
      TimeOutCnt <= (others => '0');
      enable <= '0';
    elsif rising_edge(clk) then
      case state is
        when x"00" =>
          if (Start_Sync_Edge = '1') then
            if (ext_start_ctrl = 0) then
              state <= x"02";
            else
              TimeOutCnt <= (others => '0');
              state <= x"01";
            end if;
          end if;
          if (enable /= '0') then
            enable <= '0';
          end if;
          if (TimeOutSig /= '0') then
            TimeOutSig <= '0';
          end if;

        when x"01" =>
          if (ExtStart_Sync_Edge = '1') then
            state <= x"02";
          else
            if (TimeOutCnt < TimeOutValue) then
              if (clk_div = '1') then
                TimeOutCnt <= TimeOutCnt + 1;
              end if;
            else
              state <= x"03";
            end if;
          end if;
        when x"02" =>
          enable <= '1';
          if (shooter_running = '1') then
            state <= x"06";
          end if;
        when x"06" =>
          if (Start_Sync = '0') then
            state <= x"04";
          elsif (ext_start_ctrl(1) = '1') then
            if (ExtStart_Sync = '0') then
              state <= x"04";
            end if;
          elsif (shooter_running = '0') then
            state <= x"05";
          end if;
        when x"03" =>
          if (TimeOutSig = '0') then
            TimeOutSig <= '1';
          end if;
          if (Start_Sync = '0') then
            state <= x"00";
          end if;
        when x"04" =>
          if (shooter_running = '1') then
            if (shooter_ready = '1') then
              state <= x"05";
            end if;
          else
            state <= x"05";
          end if;
        when x"05" =>
          if (Start_Sync = '0') then
            state <= x"00";
          end if;
          if (enable /= '0') then
            enable <= '0';
          end if;
        when others =>
          state <= (others => '0');
      end case;
    end if;
  end process;

--l_antibounce_i : entity antibounce 
--  port map(
--    input => ExtStart_Sync,
--    output => Ext_Start_Int,
--    clk => clk
--  );
Ext_Start_Int <= ExtStart_Sync;

status(0) <= Ext_Start_Int;
status(1) <= TimeOutSig;

ShooterEnable_Out <= enable;
ExtStart_Out <= Ext_Start_Int;

end behav;
