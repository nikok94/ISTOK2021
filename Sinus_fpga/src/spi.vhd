
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

library work;
use work.spi_master;
use work.main_properties.all;


entity SPI_slave is
  generic( 
    cpol        : STD_LOGIC := '1';  --spi clock polarity mode
    cpha        : STD_LOGIC := '1';  --spi clock phase mode
    d_width     : INTEGER := 16      --data width in bits
    );
  port(
    -- internal ports:
    clk                     : in std_logic;
    spi_offset_regs_out     : out SPI_REG_PULSE_OFFSET_TYPE;
    spi_control_regs_out    : out SPI_REG_RW_TYPE;
    spi_status_regs_in      : in SPI_REG_R_TYPE;
    --external ports
    SCK                     : in std_logic;
    CS                      : in std_logic;
    MOSI                    : in std_logic;
    MISO                    : inout std_logic
  );
end SPI_slave;

architecture behav of SPI_slave is

  signal reg_adr_int              : integer range 0 to SPI_REG_NUMBER - 1;

  signal m_fcb_addr           : std_logic_vector(16 - 1 downto 0);
  signal m_fcb_wrdata         : std_logic_vector(32 - 1 downto 0);
  signal m_fcb_wrreq          : std_logic;
  signal m_fcb_wrack          : std_logic;
  signal m_fcb_rddata         : std_logic_vector(32 - 1 downto 0);
  signal m_fcb_rdreq          : std_logic;
  signal m_fcb_rdack          : std_logic;
  signal MISO_I               : std_logic;
  signal MISO_O               : std_logic;
  signal MISO_T               : std_logic;
  signal MOSI_I               : std_logic;
  signal MOSI_O               : std_logic;
  signal MOSI_T               : std_logic;
  signal offset_regs          : SPI_REG_PULSE_OFFSET_TYPE;
  signal control_regs         : SPI_REG_RW_TYPE;

begin

spi_offset_regs_out  <= offset_regs;
spi_control_regs_out <= control_regs;

SPI_MODUL_INST : ENTITY spi_master
    generic map(
      C_CPHA            => '1',
      C_CPOL            => '1',
      C_LSB_FIRST       => false
    )
    Port map( 
      SCK               => SCK, 
      CS                => CS,

      MISO_I            => MISO_I,
      MISO_O            => MISO_O,
      MISO_T            => MISO_T,
      
      MOSI_I            => MOSI_I,
      MOSI_O            => MOSI_O,
      MOSI_T            => MOSI_T,

      m_fcb_clk         => clk,
      m_fcb_areset      => '0',
      m_fcb_addr        => m_fcb_addr  ,
      m_fcb_wrdata      => m_fcb_wrdata,
      m_fcb_wrreq       => m_fcb_wrreq ,
      m_fcb_wrack       => m_fcb_wrack ,
      m_fcb_rddata      => m_fcb_rddata,
      m_fcb_rdreq       => m_fcb_rdreq ,
      m_fcb_rdack       => m_fcb_rdack 
    );

MOSI_I <= MOSI;

MISO <= MISO_O when MISO_T = '0' else 'Z';


  reg_adr_int <= conv_integer(m_fcb_addr);

  process(clk)
  begin
    if rising_edge(clk) then
      if (m_fcb_wrreq = '1') then
        if (reg_adr_int < SPI_REG_RW_NUMBER) then
          control_regs(reg_adr_int) <= m_fcb_wrdata;
        elsif (reg_adr_int < SPI_REG_RW_NUMBER + SPI_REG_PULSE_OFFSET_NUMBER) then
          offset_regs(reg_adr_int - SPI_REG_RW_NUMBER) <= m_fcb_wrdata;
        end if;
        m_fcb_wrack <= '1';
      else 
        m_fcb_wrack <= '0';
      end if;
      
      if (m_fcb_rdreq = '1') then
        if (reg_adr_int < SPI_REG_RW_NUMBER) then
          m_fcb_rddata <= control_regs(reg_adr_int);
        elsif (reg_adr_int < SPI_REG_RW_NUMBER + SPI_REG_PULSE_OFFSET_NUMBER) then
          m_fcb_rddata <= offset_regs(reg_adr_int - SPI_REG_RW_NUMBER);
        elsif (reg_adr_int < SPI_REG_NUMBER) then
          m_fcb_rddata <= spi_status_regs_in(reg_adr_int - SPI_REG_RW_NUMBER - SPI_REG_PULSE_OFFSET_NUMBER);
        end if;
        m_fcb_rdack <= '1';
      else 
        m_fcb_rdack <= '0';
      end if;
    end if;
  end process;

end behav;