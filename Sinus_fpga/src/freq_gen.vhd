
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity freq_gen is
  generic(
    pulse_duration   : integer := 1000
  );
  port
  (
    clk                     : in std_logic;
    n_reset                 : in std_logic;
    period                  : in std_logic_vector(31 downto 0);
    shots_number            : in std_logic_vector(31 downto 0);
    cur_shot                : out std_logic_vector(31 downto 0);
    freq_gen_running        : out std_logic;
    output                  : out std_logic
  );
end freq_gen;


--  Architecture Body

architecture freq_gen_arc of freq_gen is

  signal state              : std_logic_vector(7 downto 0);
  signal shots_cnt          : std_logic_vector(31 downto 0) := (others => '0');
  signal cnt                : std_logic_vector(31 downto 0);
  signal period_start       : std_logic;
  signal busy               : std_logic;

begin

output <= period_start;
cur_shot <= shots_cnt;
freq_gen_running <= busy;

main_state_proc :
  process(clk, n_reset)
  begin
    if (n_reset = '0') then
      state <= (others => '0');
      period_start <= '0';
      busy <= '0';
    elsif rising_edge(clk) then
      case (state) is
        when x"00" =>

          shots_cnt <= (others => '0');
          if (period > pulse_duration) then
            state <= x"01";
          end if;

          if (busy = '1') then
            busy <= '0';
          end if;
        when x"01" =>
          if (shots_cnt < shots_number) then
            state <= x"02";
            period_start <= '1';
            cnt <= (0 => '1', others => '0');
            shots_cnt <= shots_cnt + 1;
            if (busy /= '1') then
              busy <= '1';
            end if;
          else
            state <= x"03";
          end if;
        when x"02" =>
          if (period_start = '1') then
            if (cnt >= pulse_duration) then
              period_start <= '0';
            end if;
          end if;
          if (cnt < period) then
            cnt <= cnt + 1;
          else
            state <= x"01";
          end if;
        when x"03" =>
          if (busy /= '0') then
            busy <= '0';
          end if;
        when others =>
          state <= x"00";
      end case;
    end if;
  end process;

end freq_gen_arc;