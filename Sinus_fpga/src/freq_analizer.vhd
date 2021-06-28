LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.std_logic_1164.STD_LOGIC_VECTOR;
use IEEE.std_logic_unsigned.all;
USE ieee.numeric_std.all;
USE ieee.Math_real.all;


ENTITY freq_analyzer IS
    generic( 
      c_clk_freq_hz             : integer := 100_000_000;   -- С‡Р°СЃС‚РѕС‚Р° РІС…РѕРґРЅС‹С… С‚Р°РєС‚РѕРІ clk
      c_avar_val                : integer := 128;           -- РєРѕР»РёС‡РµСЃС‚РІРѕ Р·РЅР°С‡РµРЅРёР№ РґР»СЏ СѓСЃСЂРµРґРЅРµРЅРёСЏ
      c_sign_freq_width         : integer := 16;            -- СЂР°Р·РјРµСЂРЅРѕСЃС‚СЊ СЂРµР·СѓР»СЊС‚Р°С‚
      c_on_period               : boolean := false;         -- РјРµСЂРёС‚СЊ РїРµСЂРёРѕРґ РёР»Рё РґР»РёРЅСѓ РёРјРїСѓР»СЊСЃР°
      c_pulse_polarity          : integer := 1              -- РїРѕР»СЏСЂРЅРѕСЃС‚СЊ СЃРёРіРЅР°Р»Р°
    );
    port(
      clk                       : in std_logic;
      rst                       : in std_logic;
      signal_in                 : in std_logic;
      sign_freq_out             : out std_logic_vector(c_sign_freq_width-1 downto 0);
      valid                     : out std_logic
    );

END freq_analyzer;

ARCHITECTURE freq_analyzer_arh OF freq_analyzer IS
    constant c_enable_tresh     : std_logic_vector(c_sign_freq_width - 1 downto 0):= (others => '1');
    constant c_left_width_up    : integer := natural((log2(real(c_avar_val))));
    constant c_max_avar_count   : std_logic_vector(c_left_width_up-1 downto 0) := (others => '1');
    signal signal_in_d          : std_logic;
    signal signal_in_d1         : std_logic;
    signal signal_in_d2         : std_logic;
    signal signal_edge          : std_logic;
    signal signal_fall          : std_logic;
    signal clk_counter          : std_logic_vector(c_sign_freq_width-1 downto 0);
    signal edge_counter         : std_logic_vector(c_left_width_up - 1 downto 0):= (others => '0');
    signal zeros_vec            : std_logic_vector(c_left_width_up - 1 downto 0):= (others => '0');
    signal avar_reg             : std_logic_vector(c_left_width_up + c_sign_freq_width -1 downto 0);
    signal sum_vector           : std_logic_vector(c_left_width_up + c_sign_freq_width -1 downto 0);
    signal freq                 : std_logic_vector(c_sign_freq_width - 1 downto 0);
    signal freq_valid           : std_logic;
    signal enable               : std_logic;

begin
signal_in_dealay_proc :
    process(clk)
    begin
      if rising_edge(clk) then
        if rst = '1' then
          signal_in_d   <= '0';
          signal_in_d1  <= '0';
          signal_in_d2  <= '0';
        else
          signal_in_d   <= signal_in;
          signal_in_d1  <= signal_in_d;
          signal_in_d2  <= signal_in_d1;
        end if;
      end if;
    end process;

enable_process :
    process(clk)
    begin
      if rising_edge(clk) then
        if (rst = '1') or (clk_counter = c_enable_tresh) then
          enable <= '0';
        elsif (signal_edge = '1') then
          enable <= '1';
        end if;
      end if;
    end process;

rising_edge_generate : if c_pulse_polarity = 1 generate
    signal_edge <= signal_in_d1 and (not signal_in_d2);
    signal_fall <= signal_in_d2 and (not signal_in_d1);
end generate;

rising_edge_not_generate : if c_pulse_polarity = 0 generate
    signal_fall <= signal_in_d1 and (not signal_in_d2);
    signal_edge <= signal_in_d2 and (not signal_in_d1);
end generate;


clk_counter_proc:
  process(clk)
  begin
    if rising_edge(clk) then
      if (enable = '1') then
        if (rst = '1') or (signal_edge = '1') then
          clk_counter <= (0 => '1', others => '0');
        else
          clk_counter <= clk_counter + 1;
        end if;
      end if;
    end if;
  end process;

--edge_counter_proc :
--  process(clk)
--  begin
--    if rising_edge(clk) then
--      if (rst = '1') then
--        edge_counter <= (others => '0');
--      elsif (signal_edge = '1') then
--        if (edge_counter = c_max_avar_count) then
--          edge_counter <= (others => '0');
--        else 
--          edge_counter <= edge_counter + 1;
--        end if;
--      end if;
--    end if;
--  end process;

--valid_proc :
--  process(clk)
--  begin
--    if rising_edge(clk) then
--      if (rst = '1') then
--        valid <=  '0';
--      elsif (signal_edge = '1') then 
--        valid <= '1';
--      else 
--        valid <=  '0';
--      end if;
--    end if;
--  end process;

true_gerate_proc : if c_on_period = true generate
  avar_reg_process :
  process(clk)
  begin
    if rising_edge(clk) then
      if (rst = '1') then 
        freq <= (others => '0');
        avar_reg <= (others => '0');
        sum_vector <= (others => '0');
        freq_valid <= '0';
        edge_counter <= (others => '0');
      else
        if (signal_edge = '1') and (enable = '1') then
          if (edge_counter = c_max_avar_count) then
            sum_vector <= (avar_reg + (zeros_vec & clk_counter));
            avar_reg <= (others => '0');
            edge_counter <= (others => '0');
            freq_valid <= '1';
          else 
            edge_counter <= edge_counter + 1;
            avar_reg <= avar_reg + (zeros_vec & clk_counter);
          end if;
        else
            freq_valid <= '0';
        end if;
      end if;
    end if;
  end process;
end generate;

false_gerate_proc : if c_on_period = false generate
  avar_reg_process :
  process(clk)
  begin
    if rising_edge(clk) then
      if (rst = '1') then 
        freq <= (others => '0');
        avar_reg <= (others => '0');
        sum_vector <= (others => '0');
        freq_valid <= '0';
        edge_counter <= (others => '0');
      else
        if (signal_fall = '1') and (enable = '1') then
          if (edge_counter = c_max_avar_count) then
            sum_vector <= (avar_reg + (zeros_vec & clk_counter));
            avar_reg <= (others => '0');
            edge_counter <= (others => '0');
            freq_valid <= '1';
          else 
            edge_counter <= edge_counter + 1;
            avar_reg <= avar_reg + (zeros_vec & clk_counter);
          end if;
        else
            freq_valid <= '0';
        end if;
      end if;
    end if;
  end process;
end generate;

out_process :
  process(clk)
  begin
    if rising_edge(clk) then
      if (rst = '1') then 
        sign_freq_out <= (others => '0');
      elsif (freq_valid = '1') then
        sign_freq_out <= sum_vector((c_left_width_up + c_sign_freq_width - 1) downto c_left_width_up);
        valid <= '1';
      else
        valid <= '0';
      end if;
    end if;
  end process;


END freq_analyzer_arh;