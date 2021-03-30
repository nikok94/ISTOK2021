library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity timer is
  generic (
    pulse_duration : integer := 10
  );
  port
  (
    clk         : in std_logic;
    n_reset     : in std_logic;
    start       : in std_logic;
    value       : in std_logic_vector(31 downto 0);
    busy        : out std_logic;
    output      : out std_logic
  );
end timer;
--  Architecture Body

architecture timer_arc of timer is

  signal start_shift_reg        : std_logic_vector(3 downto 0);
  signal timer_start            : std_logic;
  signal state                  : std_logic_vector(3 downto 0);
  signal gen_pulse              : std_logic;
  signal cnt                    : std_logic_vector(31 downto 0);
  signal bsy                    : std_logic;

begin

busy <= bsy;

main_state_proc :
  process(clk, n_reset)
  begin
    if n_reset = '0' then
      start_shift_reg <= "0000";
      state <= (others => '0');
      gen_pulse <= '0';
      bsy <= '0';
    elsif rising_edge(clk) then
      start_shift_reg(3 downto 1) <= start_shift_reg(2 downto 0);
      start_shift_reg(0) <= start;
      timer_start <= ((not start_shift_reg(3)) and start_shift_reg(2));
      case (state) is
        when x"0" =>
          if (timer_start = '1') then
            state <= x"1";
            bsy <= '1';
            cnt <= (0 => '1', others => '0');
          end if;
          if (gen_pulse = '1') then
            gen_pulse <= '0';
          end if;
          if (bsy = '1') then
            bsy <= '0';
          end if;
        when x"1" =>
          if (cnt < value) then
            cnt <= cnt + 1;
          else
            state <= x"2";
            cnt <= (0 => '1', others => '0');
            gen_pulse <= '1';
          end if;
        when x"2" =>
          if (cnt < pulse_duration) then
            cnt <= cnt + 1;
          else
            state <= x"0";
          end if;
        when others =>
          state <= x"0";
      end case;
    end if;
  end process;

output <= gen_pulse;

end timer_arc;