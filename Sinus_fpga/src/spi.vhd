
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

library work;
--use work.spi_master;
use work.main_properties.all;


entity SPI_slave is
  generic( 
    c_d_width     : INTEGER := 16      --data width in bits
    );
  port(
    busy                      : out std_logic;
    busy_addr                 : out integer;
    data_in                   : in SPI_REG_TYPE;
    data_out                  : out SPI_REG_TYPE;
    --external ports
    SCK                       : in std_logic;
    CS                        : in std_logic;
    MOSI                      : in std_logic;
    MISO                      : inout std_logic
  );
end SPI_slave;

architecture behav of SPI_slave is
--  signal mode           : std_logic;
  signal ssck           : std_logic;
  
  signal wreg           : std_logic_vector(c_d_width - 1 downto 0):=(others => '0');
  signal w_bit_cnt      : integer:= 0;
  signal w_word_cnt     : integer:= 0;
  
  signal renable        : std_logic:='0';
  signal rreg           : std_logic_vector(c_d_width - 1 downto 0):=(others => 'Z');
  signal r_bit_cnt      : integer:= 0;
  signal r_word_cnt     : integer:= 0;
  signal addr           : integer:= 0;

  type BUFF_Type is array (2 downto 0) of std_logic_vector(c_d_width - 1 downto 0);
  signal wbuff          : BUFF_Type;
  signal rbuff          : BUFF_Type;
  signal wr_en          : std_logic:='0'; 
  signal data_to_out    : SPI_REG_TYPE:=
    ( 0 => (others => '0'),
      1 => (others => '0'),
      2 => (others => '0'),
      3 => (others => '0'),
      4 => (others => '0'),
      5 => (others => '0'),
      6 => (others => '0'),
      7 => (others => '0'),
      8 => (others => '0'),
      9 => (others => '0'),
     10 => (others => '0'),
     11 => (others => '0'),
     12 => (others => '0'),
     13 => (others => '0'),
     14 => (others => '0'),
     15 => (others => '0'),
     16 => (others => '0')
    );

begin

busy <= not CS;

data_out <= data_to_out;

MISO <= rreg(rreg'length-1) when CS = '0' else 'Z';

ssck <= SCK;
busy_addr <= addr;
addr <=  to_integer(unsigned(wbuff(0)));

process(ssck, CS)
begin
  if (CS = '1') then
    w_bit_cnt <= 0;
    w_word_cnt <= 0;
  elsif rising_edge(ssck) then
    if (w_bit_cnt < c_d_width-1) then
      w_bit_cnt <= w_bit_cnt + 1;
    else
      w_bit_cnt <= 0;
      if (w_word_cnt < 2) then
        w_word_cnt <= w_word_cnt + 1;
        if (w_word_cnt = 0) then
          wbuff(w_word_cnt) <= x"0FFF" and (wreg(wreg'length-2 downto 0) & MOSI);
          if (wreg(wreg'length-2 downto wreg'length-5) = x"0") then
            wr_en <= '1';
          else
            wr_en <= '0';
          end if;
        else
          wbuff(w_word_cnt) <= wreg(wreg'length-2 downto 0) & MOSI;
        end if;
      else
        if ((addr < SPI_REG_STRUCT'pos(CUR_SHOT)) and (wr_en = '1')) then
          data_to_out(addr) <= wbuff(1) & wreg(wreg'length-2 downto 0) & MOSI;
        end if;
        wbuff(w_word_cnt) <= wreg(wreg'length-2 downto 0) & MOSI;
        w_word_cnt <= 0;
      end if;
    end if;
    wreg(wreg'length-1 downto 1) <= wreg(wreg'length-2 downto 0);
    wreg(0) <= MOSI;
  end if;
end process;

process(ssck, CS)
begin
  if (CS = '1') then
    rreg <= (others => '0');
    r_bit_cnt <= 0;
    r_word_cnt <= 0;
  elsif falling_edge(ssck) then
    if (r_bit_cnt < c_d_width-1) then
      r_bit_cnt <= r_bit_cnt + 1;
      rreg(rreg'length-1 downto 1) <= rreg(rreg'length-2 downto 0);
      rreg(0) <= '0';
    else
      r_bit_cnt <= 0;
      if (r_word_cnt < 2) then
        r_word_cnt <= r_word_cnt + 1;
        if (r_word_cnt = 0) then
          if (addr < data_in'length) then
            rreg <= data_in(addr)(31 downto 16);
            rbuff(0) <= data_in(addr)(31 downto 16);
            rbuff(1) <= data_in(addr)(15 downto 0);
            rbuff(2) <= (others => '0');
          else
            rreg <= (others => '0');
            rbuff(0) <= (others => '0');
            rbuff(1) <= (others => '0');
            rbuff(2) <= (others => '0');
          end if;
        else
          rreg <= rbuff(r_word_cnt);
        end if;
      else
        r_word_cnt <= 0;
        rreg <= rbuff(r_word_cnt);
      end if;
    end if;
  end if;
end process;

end behav;
