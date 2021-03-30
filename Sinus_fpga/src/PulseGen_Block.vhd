library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.math_real.all;

library work;
use work.main_properties.all;

entity PulseGen_Block is
  port(
    clk                 : in std_logic;
    rst                 : in std_logic;
    start_in            : in std_logic;
    delay_config        : in SPI_REG_PULSE_OFFSET_TYPE;
    busy                : out std_logic;
    pulse               : out std_logic_vector(PULSE_NUMBER - 1 downto 0)
  );
end PulseGen_Block;

architecture arch of PulseGen_Block is

  constant c_pulse_duration    : integer := PULSE_DURATION;
  constant c_pulse_number      : integer := PULSE_NUMBER;
  constant main_counter_max : std_logic_vector(31 downto 0):= (others => '1');
  signal main_counter       : std_logic_vector(31 downto 0):= main_counter_max;
  signal start_vec          : std_logic_vector(c_pulse_number - 1 downto 0);
  type pulse_cnts_type is array(0 to c_pulse_number - 1) of std_logic_vector(natural(round(log2(real(c_pulse_duration))))-1 downto 0);
  signal pulse_range_counters   : pulse_cnts_type;
  signal pulse_vec              : std_logic_vector(c_pulse_number - 1 downto 0);
  signal busy_vec               : std_logic_vector(c_pulse_number - 1 downto 0);
  signal bsy                    : std_logic := '0';
  signal reg_offset             : SPI_REG_PULSE_OFFSET_TYPE;
  signal start_d_vec            : std_logic_vector(2 downto 0);
  signal start                  : std_logic;

begin

start_d_vec_proc :
  process(clk, rst)
  begin
    if (rst = '1') then
      start_d_vec <= (others => '0');
    elsif rising_edge(clk) then
      start_d_vec(0) <= start_in;
      start_d_vec(start_d_vec'length - 1 downto 1) <= start_d_vec(start_d_vec'length - 2 downto 0);
      start <= (not start_d_vec(start_d_vec'length - 1)) and start_d_vec(start_d_vec'length - 2);
    end if;
  end process;

p_parity_check : process (clk)
variable vparity           : std_logic;
begin
  if rising_edge(clk) then
    vparity := '0';
    l_parity : for k in 0 to busy_vec'length-1 loop
      vparity := vparity or busy_vec(k);
    end loop l_parity;

    bsy <= vparity;
  end if;
end process p_parity_check;

busy <= bsy;

pulse <= pulse_vec;

main_cntr_proc :
  process(clk, start, rst)
  begin
    if (rst = '1') then
      main_counter <= main_counter_max;
    elsif rising_edge(clk) then
      if (start = '1') then
        main_counter <= (others => '0');
        reg_offset <= delay_config;
      else
        if (main_counter < main_counter_max) then
          main_counter <= main_counter + 1;
        end if;
      end if;
    end if;
  end process;

main_gen : for i in 0 to c_pulse_number - 1 generate
  process(rst, clk)
  begin
    if (rst = '1') then
      pulse_range_counters(i) <= conv_std_logic_vector(c_pulse_duration, pulse_range_counters(i)'length) ;
      pulse_vec(i) <= '0';
      busy_vec(i) <= '0';
    elsif rising_edge(clk) then
      if (main_counter = reg_offset(i)) then
        pulse_range_counters(i) <= (others => '0');
        pulse_vec(i) <= '1';
      else
        if (pulse_range_counters(i) < c_pulse_duration) then
          pulse_range_counters(i) <= pulse_range_counters(i) + 1;
          pulse_vec(i) <= '1';
        else
          pulse_vec(i) <= '0';
        end if;
      end if;

      if (main_counter <= reg_offset(i)) then
        busy_vec(i) <= '1';
      else
        if (pulse_vec(i) = '0') then
          busy_vec(i) <= '0';
        end if;
      end if;

    end if;
  end process;

end generate;


end arch;
