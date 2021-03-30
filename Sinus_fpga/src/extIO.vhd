library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

library work;
use work.antibounce;

entity discreteI is 
  generic (
    c_num_ports         : integer := 8;
    c_antibounce_n_cnt  : integer := 50*250*2
  );
  port(
    dout        : out std_logic_vector(c_num_ports-1 downto 0);
    din         :  in std_logic_vector(c_num_ports-1 downto 0);
    clk         :  in std_logic
    );
  end discreteI;

architecture behav of discreteI is

begin
  l_antibounce:  for i in c_num_ports-1 downto 0 generate
    l_antibounce_i : entity antibounce
    generic map(
      n_count => c_antibounce_n_cnt
    )
    port map(
      input => din(i),
      output => dout(i),
      clk => clk
    );
  end generate;
end behav;
