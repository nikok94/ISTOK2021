library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.MATH_REAL.all;

entity antibounce is
  generic(
    n_count : INTEGER := 250 --number of antibounce counts
  );
  port (
  clk       : in std_logic;
  input     : in std_logic;
  output    : out std_logic
  );
end antibounce;

architecture behav of antibounce is
  constant sync_stage   : integer := 2;
  signal in_trig_vec    : std_logic_vector(sync_stage - 1 downto 0);
  signal cnt            : std_logic_vector(natural(ceil(log2(real(n_count)))) downto 0):= (others => '0');
  signal sync_value     : std_logic;
  signal old_sync_value : std_logic;
  signal rst            : std_logic;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      in_trig_vec(0) <= input;
      in_trig_vec(in_trig_vec'length - 1 downto 1) <= in_trig_vec(in_trig_vec'length - 2 downto 0);

      sync_value <= in_trig_vec(in_trig_vec'length - 1);
      old_sync_value <= sync_value;
      if (rst = '1') then
        cnt <= (others => '0');
      else
        if (cnt < n_count) then
          cnt <= cnt + 1;
          output <= input;
        else
          if (old_sync_value /= sync_value) then
            cnt <= (others => '0');
          else
            output <= sync_value;
          end if;
        end if;
      end if;
    end if;
  end process;

  process(clk)
    variable vparity           : boolean;
  begin
    if rising_edge(clk) then
      if (rst = '1') then
        rst <= '0';
      else
        vparity := false;
        l_parity : for i in 0 to cnt'length - 1 loop
          vparity := vparity or (not((cnt(i) = '0') or (cnt(i)= '1')));
        end loop l_parity;
        if vparity then 
          rst <= '1';
        else
          rst <= '0';
        end if;
      end if;
    end if;
  end process;

end behav;