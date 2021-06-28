library ieee;
use ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.math_real.all;

library altera; 
use altera.altera_primitives_components.all;

library work;
use work.freq_analyzer;
use work.PLL1;
use work.discreteI;
use work.spi_master;
use work.main_properties.all;
use work.data_recorder;
use work.QSPI_interconnect;
use work.PulseGen_Block;
use work.freq_gen;
use work.remoteUpdate_Module;

entity Top_Module is port(
    MISO                    : inout std_logic;
    MOSI                    : in std_logic;
    CS                      : in std_logic;
    SCK                     : in std_logic;
    --+++++++++++++++++++++ UFL ADC ++++++++++++++++++++++--
    ADC_CLK                 : out std_logic;
    OTR                     : in std_logic;
    D                       : in  std_logic_vector ( 11 downto 0);
    --+++++++++++++++++++++ QSPI ++++++++++++++++++++++++++--
    QSPI_NCS                : in std_logic;
    QSPI_CLK                : in std_logic;
    QSPI_IO0                : inout std_logic; -- QSPI_MOSI
    QSPI_IO1                : inout std_logic; -- QSPI_MISO
    QSPI_IO2                : inout std_logic;
    QSPI_IO3                : inout std_logic;	
    --++++++++++++++++++++++++++++++++++++++++++++++++++++++--
    EVENT8                  : out std_logic;
    pin_68                  : out std_logic;
    --++++++++++++++++++++++++++++++++++++++++++++++++++++++--
    clock                   : in std_logic;
    --clk_out                 : out std_logic;
    ------------------------------
    -- external signals
    Interlock_IN            : in std_logic;-- Interlock
    Trigatron_IN				 : in std_logic;-- Trigatron
    Slave_Ready_IN				: in std_logic; -- Slave Ready
    Ext_Start_Out        	: out std_logic;
    Ext_Start_IN            : in std_logic;
    Slave_Start_Out_Int     : in std_logic;
    Slave_Sync 				: out std_logic;
    
    ExtRst                  : in std_logic;
----------------------------------------------
    -- discrete INout interface
    inputs                  : in std_logic_vector (7 downto 0);
    pin_outputs             : out std_logic_vector (7 downto 0);
----------------------------------------------
    F_CH1_IN                : in std_logic;
----------------------------------------------
    --UARTS pins----------
    TX1_IN                  : in std_logic;
    RX1_IN                  : in std_logic;
    TX1_OUT                 : out std_logic;
    RX1_OUT                 : out std_logic;

    TX2_IN                  : in std_logic;
    RX2_IN                  : in std_logic;
    TX2_OUT                 : out std_logic;
    RX2_OUT                 : out std_logic;

    TX3_IN                  : in std_logic;
    RX3_IN                  : in std_logic;
    TX3_OUT                 : out std_logic;
    RX3_OUT                 : out std_logic;

    TX4_IN                  : in std_logic;
    RX4_IN                  : in std_logic;
    TX4_OUT                 : out std_logic;
    RX4_OUT                 : out std_logic;
    
    remBusy                 : out std_logic;
    remReconf               : in std_logic;
    
    pin_98                   : out std_logic;

----------------------------------------------
--QUADRATOR synchro signals--
    QuadStartOsc            : out std_logic;
    QuadGate                : out std_logic;
----------------------------------------------
    enable                  : in std_logic;
    checking                : out std_logic;
    Stop                    : in std_logic;
    aux_main                : out std_logic;
    timer_out               : out std_logic_vector((4 - 1) downto 0);
    aux_timer_out           : out std_logic_vector((6 - 1) downto 0)
    );
end Top_Module;

architecture arch of Top_Module is

  signal timers_trigger           : std_logic;
  signal n_reset                  : std_logic;
--  signal internal_timer_out       : std_logic_vector((TIMERS_NUMBER - 1) downto 0);
--  signal int_aux_timer_out        : std_logic_vector((AUX_TIMERS_NUMBER - 1) downto 0);
  signal ExtStart                 : std_logic;
  signal shooter_enable           : std_logic;
  signal StartByEdge              : std_logic;
  signal StopByEdge               : std_logic;

  signal clk200MHz                : std_logic;
  signal clk100MHz                : std_logic;
  signal clk50MHz                 : std_logic;
  signal clk20MHz                 : std_logic;
  signal locked                   : std_logic;
  
  signal rst_reg                  : std_logic_vector(7 downto 0);
  signal rst                      : std_logic;
  signal shooter_ready             : std_logic;
  signal shooter_running          : std_logic;
  signal ctrl_logic_ext_start     : std_logic;
  signal ctrl_logic_start         : std_logic;
  signal ufl_start                : std_logic;
  
  -- data recorder
  constant c_max_num_data                 : integer:= 2048;
  constant c_data_width                   : integer:= 12;
  signal data_recorder_num_data           : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := (others => '1');
  signal data_recorder_start_offset       : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := (others => '0');
  signal data_recorder_s_data             : std_logic_vector(c_data_width - 1 downto 0);
  signal data_recorder_s_valid            : std_logic;
  signal data_recorder_s_ready            : std_logic;
  signal data_recorder_m_data             : std_logic_vector(c_data_width - 1 downto 0);
  signal data_recorder_m_valid            : std_logic;
  signal data_recorder_m_ready            : std_logic;
  signal data_recorder_rst                : std_logic;
  signal data_recorder_interrupt          : std_logic;
  
  -- qspi interconnect 
  signal qspi_slave_x_cs_up               : std_logic_vector(1-1 downto 0);
  signal qspi_si                          : std_logic_vector(3 downto 0);
  signal qspi_so                          : std_logic_vector(3 downto 0);
  signal qspi_so_d                        : std_logic_vector(3 downto 0);
  signal qspi_t                           : std_logic_vector(3 downto 0);
  
  signal div_cnt                          : integer;
  signal clk_to_adc                       : std_logic;
  signal clk_to_adc_d                     : std_logic;
  constant dd_sync_stage                  : integer := 2;
  signal dd_vec                           : std_logic_vector(dd_sync_stage*D'length - 1 downto 0);
  
  signal qspi_reg                         : std_logic_vector(15 downto 0);
  signal qspi_sig                         : std_logic_vector(15 downto 0);
  signal qspi_cnt                         : integer;
  
  signal PulseGen_Block_start             : std_logic;
  signal PulseGen_Block_bsy               : std_logic;
  signal PulseGen_Block_rst               : std_logic;
  signal PulseGen_Block_pulse             : std_logic_vector(PULSE_NUMBER - 1 downto 0);
  
  signal freq_gen_output                  : std_logic;
  signal state                            : std_logic_vector(7 downto 0);
  signal master_mode                      : std_logic;
  signal ext_start_sync_vec               : std_logic_vector(3 downto 0);
  signal ext_start_rising_edge            : std_logic;
  signal stop_sync_vec                    : std_logic_vector(3 downto 0);
  signal stop_sync                        : std_logic;
  -- SPI REGS
  signal spi_regs             : SPI_REG_TYPE;
  
  signal reg_adr_int          : integer range 0 to SPI_REG_NUMBER - 1;

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
  
  signal timer_cfg            : SPI_REG_PULSE_OFFSET_TYPE;
  signal int_cur_shot         : std_logic_vector(31 downto 0);
  signal int_status           : std_logic_vector(31 downto 0);
  signal int_freq             : std_logic_vector(31 downto 0);
  signal quad_gate            : std_logic;
  signal qstartv              : std_logic_vector(2 downto 0);
  signal qstopv               : std_logic_vector(2 downto 0);
  signal ufl_start_vec        : std_logic_vector(2 downto 0);
  signal remoteUpdate_Module_busy : std_logic;
  signal Slave_Ready_IN_n     : std_logic;

begin

remoteUpdate_Module_inst : ENTITY remoteUpdate_Module
  PORT MAP
  (
    clock           => clock,
    param           => "000",
    read_param      => '0',
    read_source     => "00",
    reconfig        => remReconf,
    reset           => '0',
    reset_timer     => '0',
    busy            => remoteUpdate_Module_busy,
    data_out        => open
  );

remBusy <= locked and (not remoteUpdate_Module_busy);


checking <= PulseGen_Block_start;--freq_gen_output;
Ext_Start_Out <= Slave_Start_Out_Int;


SPI_MODUL_INST : ENTITY spi_master
    generic map(
      C_CPHA            => '1',
      C_CPOL            => '0',
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

      m_fcb_clk         => clk100MHz,
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

SPI_REGS_RW_PROC : process(clk100MHz)
begin
  if rising_edge(clk100MHz) then
    if (m_fcb_wrreq = '1') then
      m_fcb_wrack <= '1';
      if (reg_adr_int < SPI_REG_STRUCT'pos(CUR_SHOT)) then
        spi_regs(reg_adr_int) <= m_fcb_wrdata;
      end if;
    else
      m_fcb_wrack <= '0';
    end if;
    
    spi_regs(SPI_REG_STRUCT'pos(CUR_SHOT)) <= int_cur_shot;
    spi_regs(SPI_REG_STRUCT'pos(STATUS)) <= int_status;
    spi_regs(SPI_REG_STRUCT'pos(FREQCH12)) <= int_freq;
    
    if (m_fcb_rdreq = '1') then
      m_fcb_rdack <= '1';
      if (reg_adr_int < SPI_REG_STRUCT'pos(STRUCT_LENGTH)) then
        m_fcb_rddata <= spi_regs(reg_adr_int);
      else
        m_fcb_rddata <= (others => '1');
      end if;
    else
      m_fcb_rdack <= '0';
    end if;
  end if;
end process;

main_proc :
  process(clk100MHz, rst)
  begin
    if (rst = '1') then
      state <= (others => '0');
      shooter_enable <= '0';
      PulseGen_Block_rst <= '1';
      master_mode <= '0';
      int_status(STATUS_STRUCT'pos(SHOOTING)) <= '0';
    elsif rising_edge(clk100MHz) then
      case (state) is
        when x"00" =>
          shooter_enable <= '0';
          PulseGen_Block_rst <= '1';
          int_status(STATUS_STRUCT'pos(SHOOTING)) <= '0';
          
          if (stop_sync ='0') then
            if (spi_regs(SPI_REG_STRUCT'pos(CONTROL))(CONTROL_STRUCT'pos(SLAVE_ENABLE)) = '1') then
              state <= x"01";
              master_mode <= '0';
            else
              state <= x"02";
              master_mode <= '1';
            end if;
          end if;
        when x"01" =>
          PulseGen_Block_rst <= '0';
          if (stop_sync = '1') then
            state <= x"00";
          end if;
        when x"02" =>
          shooter_enable <= '1';
          PulseGen_Block_rst <= '0';
          if (shooter_running = '1') then
            int_status(STATUS_STRUCT'pos(SHOOTING)) <= '1';
            state <= x"03";
          end if;
        when x"03" =>
          if (stop_sync = '1') then
            state <= x"04";
          elsif (shooter_running = '0') then
            PulseGen_Block_rst <= '1';
            shooter_enable <= '0';
            state <= x"05";
            int_status(STATUS_STRUCT'pos(SHOOTING)) <= '0';
          end if;
        when x"04" =>
          if (shooter_running = '1') then
            if (PulseGen_Block_bsy = '0') then
              PulseGen_Block_rst <= '1';
              shooter_enable <= '0';
              state <= x"05";
            end if;
          else
            state <= x"05";
          end if;
        when x"05" =>
          if (stop_sync = '1') then
            state <= x"00";
          end if;
        when others =>
          state <= (others => '0');
      end case;
    end if;
  end process;

sync_vec_proc :
  process(clk100MHz)
  begin
    if rising_edge(clk100MHz) then
      ext_start_sync_vec(0) <= not Ext_Start_IN;
      ext_start_sync_vec(ext_start_sync_vec'length - 1 downto 1) <= ext_start_sync_vec(ext_start_sync_vec'length - 2 downto 0);
      ext_start_rising_edge <= (not ext_start_sync_vec(ext_start_sync_vec'length - 1)) and ext_start_sync_vec(ext_start_sync_vec'length - 2);
      
      stop_sync_vec(0) <= Stop;
      stop_sync_vec(stop_sync_vec'length - 1 downto 1) <= stop_sync_vec(stop_sync_vec'length - 2 downto 0);
      stop_sync <= stop_sync_vec(stop_sync_vec'length - 2);
    end if;
  end process;

PulseGen_Block_start <= freq_gen_output when (master_mode = '1') else ext_start_rising_edge;

pll_1 : entity PLL1 
port map
  (
    areset  => '0',
    inclk0  => clock,
    c0      => clk200MHz,
    c1      => clk100MHz,
    c2      => clk50MHz,
    c3      => clk20MHz,
    locked  => locked
  );

process(clk100MHz, locked)
begin
 if (locked = '0') then
   rst_reg <= (others => '1');
 elsif rising_edge(clk100MHz) then
   rst_reg(0) <= '0';
   rst_reg(7 downto 1) <= rst_reg(6 downto 0);
 end if;
end process;

rst <= rst_reg(7);
--n_reset <= not rst;


freq_gen_inst : entity freq_gen
  generic map(
    pulse_duration          => PULSE_DURATION
  )
  port map
  (
    clk                     => clk100MHz,
    n_reset                 => shooter_enable,
    period                  => spi_regs(SPI_REG_STRUCT'pos(SH_PERIOD)),
    shots_number            => spi_regs(SPI_REG_STRUCT'pos(SH_NUMBER)),
    cur_shot                => int_cur_shot,
    freq_gen_running        => shooter_running,
    output                  => freq_gen_output
  );

PulseGen_Block_inst : entity PulseGen_Block
  port map(
    clk                 => clk100MHz,
    rst                 => PulseGen_Block_rst,
    start_in            => PulseGen_Block_start,
    delay_config        => timer_cfg,
    busy                => PulseGen_Block_bsy,
    pulse               => PulseGen_Block_pulse
  );

quad_gate_proc : 
process(clk100MHz, PulseGen_Block_rst)
begin
  if (PulseGen_Block_rst = '1') then
    quad_gate <= '0';
    qstartv <= (others => '0');
    qstopv  <= (others => '0');
  elsif rising_edge(clk100MHz) then
    qstartv(0) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(AUX_TIMER3));
    qstopv(0) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(AUX_TIMER4));
    qstartv(qstartv'length - 1 downto 1) <= qstartv(qstartv'length - 2 downto 0);
    qstopv(qstartv'length - 1 downto 1) <= qstopv(qstartv'length - 2 downto 0);
    if ((qstartv(qstartv'length - 1) = '0') and (qstartv(qstartv'length - 2) = '1')) then
      quad_gate <= '1';
    elsif ((qstopv(qstopv'length - 1) = '0') and (qstopv(qstopv'length - 2) = '1')) then
      quad_gate <= '0';
    end if;
  end if;
end process;


gen_timer_cfg : for i in 0 to PULSE_NUMBER-1 generate
  timer_cfg(i) <= spi_regs(i);
end generate;

  pin_outputs <= spi_regs(SPI_REG_STRUCT'pos(OUTPUTS))(pin_outputs'length-1 downto 0);

  int_status(STATUS_STRUCT'pos(INTERLOCK))  <= not Interlock_IN; --tmp_inputs(7);
  int_status(STATUS_STRUCT'pos(TRIGATRON))  <= not Trigatron_IN;
  
  Slave_Ready_IN_n <= not Slave_Ready_IN;
  int_status(STATUS_STRUCT'pos(SLAVE_READY))  <= Slave_Ready_IN_n;
  
  pin_68 <= Slave_Ready_IN_n;

  timers_trigger <= '0';

ctrl_logic_ext_start <= (not Ext_Start_IN);
ctrl_logic_start <= (not Stop);

aux_main <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(TIMER0)) when enable = '1' else '0';

timer_out(0) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(TIMER0)) when enable = '1' else '0';  -- main
timer_out(1) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(TIMER1)) when enable = '1' else '0';  -- slave
timer_out(2) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(TIMER2)) when enable = '1' else '0';  -- trig
timer_out(3) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(TIMER3)) when enable = '1' else '0';  -- fast adc
Slave_Sync  <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(TIMER1)) when enable = '1' else '0';  -- slave

ufl_start_vec(0) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(AUX_TIMER0)) when enable = '1' else '0';  -- Ufl ADC   (internal)
aux_timer_out(0) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(AUX_TIMER0)) when enable = '1' else '0';  -- Ufl ADC   (internal)
aux_timer_out(1) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(AUX_TIMER1)) when enable = '1' else '0';  -- Urec adc           (to controller)
aux_timer_out(2) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(AUX_TIMER2)) when enable = '1' else '0';  -- QUAD oscillogram   (to controller)
aux_timer_out(3) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(AUX_TIMER3)) when enable = '1' else '0';  -- QUAD start         (to controller)
aux_timer_out(4) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(AUX_TIMER4)) when enable = '1' else '0';  -- QUAD stop          (to controller)
aux_timer_out(5) <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(AUX_TIMER5)) when enable = '1' else '0';  -- reserved           (to controller)

--pin_98 <= shooter_running;
--pin_98 <= PulseGen_Block_start;
pin_98 <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(TIMER1)) when enable = '1' else '0';  -- slave
--pin_98 <= enable;


QuadStartOsc <= PulseGen_Block_pulse(SPI_REG_STRUCT'pos(AUX_TIMER2)) when enable = '1' else '0';
QuadGate <= quad_gate;

----UFL ADC ----
data_recorder_rst <= rst or qspi_slave_x_cs_up(0);
ufl_start_vec_proc :
process(clk100MHz, rst)
begin
  if (rst = '1') then
    ufl_start <= '0';
  elsif rising_edge(clk100MHz) then
    ufl_start_vec(ufl_start_vec'length - 1 downto 1) <= ufl_start_vec(ufl_start_vec'length-2 downto 0);
	 ufl_start <= ufl_start_vec(ufl_start_vec'length - 2) and (not ufl_start_vec(ufl_start_vec'length - 1));
  end if;
end process;

process(clk100MHz)
begin
  if (rst = '1') then
    clk_to_adc <= '0';
    div_cnt <= 0;
  elsif rising_edge(clk100MHz) then
    if (div_cnt < 1) then
      div_cnt <= div_cnt + 1;
    else
      div_cnt <= 0;
      clk_to_adc <= not clk_to_adc;
    end if;
    clk_to_adc_d <= clk_to_adc;
    if ((clk_to_adc_d = '0') and (clk_to_adc = '1')) then
      data_recorder_s_data <= dd_vec((dd_sync_stage-1)*D'length + D'length - 1 downto (dd_sync_stage-1)*D'length);
      data_recorder_s_valid <= '1';
    else
      data_recorder_s_valid <= '0';
    end if;

    dd_vec(D'length - 1 downto 0) <= D;
    for i in 0 to dd_sync_stage - 2 loop
      dd_vec((i+1)*D'length + D'length - 1 downto (i+1)*D'length) <= dd_vec((i)*D'length + D'length - 1 downto (i)*D'length);
    end loop;
    
  end if;
end process;


process(clk100MHz, data_recorder_rst)
begin
  if (data_recorder_rst = '1') then
    data_recorder_interrupt <= '0';
  elsif rising_edge(clk100MHz) then
    if (data_recorder_interrupt = '0') then
      if (data_recorder_m_valid = '1') then
        data_recorder_interrupt <= '1';
      end if;
    end if;
  end if;
end process;

EVENT8 <= data_recorder_interrupt;

--process(clk100MHz)
--begin
--  if rising_edge(clk100MHz) then
--    if ((data_recorder_rst = '1') or (ufl_start = '1')) then
--      data_recorder_s_valid <= '0';
--      data_recorder_s_data <= (others => '0');
--    else
--      data_recorder_s_valid <= '1';
--      data_recorder_s_data <= data_recorder_s_data + 1;
--    end if;
--  end if;
--end process;


data_recorder_inst : entity data_recorder
    generic map(
      c_max_num_data            => c_max_num_data,
      c_data_width              => 12,
      c_start_delay             => 0
    )
    Port map( 
      rst                       => data_recorder_rst,
      clk                       => clk100MHz,

      start                     => ufl_start,
      num_data                  => data_recorder_num_data,
      start_offset              => data_recorder_start_offset,

      s_data                    => data_recorder_s_data,
      s_valid                   => data_recorder_s_valid,
      s_ready                   => data_recorder_s_ready,

      m_data                    => data_recorder_m_data ,
      m_valid                   => data_recorder_m_valid,
      m_ready                   => data_recorder_m_ready,
      
      compleat                  => open
    );

QSPI_interconnect_inst : entity QSPI_interconnect
  Generic map(
    c_num_slave_port    => 1,
    c_data_width        => 16,
    c_command_width     => 8,
    C_CPHA              => '1',
    C_CPOL              => '1',
    C_LSB_FIRST         => false
  )
  Port map(
    slave_x_clk(0)      => clk100MHz,
    slave_x_ready(0)    => data_recorder_m_ready,
    slave_x_data        => "0000" & data_recorder_m_data,
    slave_x_cs_up       => qspi_slave_x_cs_up,
    qspi_si             => qspi_si,
    qspi_so             => qspi_so,
    qspi_t              => qspi_t ,
    qspi_sck            => QSPI_CLK,
    qspi_cs             => QSPI_NCS
  );


ADC_CLK <= clk_to_adc;

qspi_si(0) <= QSPI_IO0;
qspi_si(1) <= QSPI_IO1;
qspi_si(2) <= QSPI_IO2;
qspi_si(3) <= QSPI_IO3;


QSPI_IO0 <= 'Z' when (qspi_t(0) = '1') else qspi_so(0);
QSPI_IO1 <= 'Z' when (qspi_t(1) = '1') else qspi_so(1);
QSPI_IO2 <= 'Z' when (qspi_t(2) = '1') else qspi_so(2);
QSPI_IO3 <= 'Z' when (qspi_t(3) = '1') else qspi_so(3);

 --------------------------------------
 --UARTS--
 --------------------------------------
  TX1_OUT <= not TX1_IN;
  RX1_OUT <=  RX1_IN; --when RX1_ENABLE = '0' else '1';

  TX2_OUT <= not TX2_IN;
  RX2_OUT <=  RX2_IN; --when RX2_ENABLE = '0' else '1';

  TX3_OUT <= not TX3_IN;
  RX3_OUT <=  RX3_IN; --when RX3_ENABLE = '0' else '1';

  TX4_OUT <=  TX4_IN;
  RX4_OUT <= not RX4_IN; --when RX4_ENABLE = '0' else '1';

 -------------------------------------
 -- FREQ ANALYZER
 -------------------------------------


  freq_meterCH1 : ENTITY freq_analyzer
    generic map( 
      c_avar_val          => 32,
      c_sign_freq_width   => 16,
      c_on_period         => false,
      c_pulse_polarity    => 0
    )
      port map(
        clk           => clk20MHz,
        rst           => rst,
        signal_in     => F_CH1_IN,
        sign_freq_out => int_freq(31 downto 16),
        valid         => open
      );
----------------------------
-- FREQ ANALYZER END
-----------------------------

I_DISCRETE_IO: entity discreteI
  generic map(
    c_num_ports         => inputs'length,
    c_antibounce_n_cnt  => 100*250
  )
  port map (
    din => inputs,
    dout => int_status(STATUS_STRUCT'pos(PININ0)+inputs'length-1 downto STATUS_STRUCT'pos(PININ0)),
    clk =>  clk100MHz
  );


 end arch;
