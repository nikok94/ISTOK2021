
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--use work.Sinus320D_Top;
use work.PulseGen_Block;

entity TestBench is

end TestBench;


--  Architecture Body

architecture TestBench_arc OF TestBench IS

  signal clk            : std_logic;
--  signal MISO                   :  std_logic;
--  signal MOSI                   : std_logic;
--  signal CS                     : std_logic;
--  signal SCK                    : std_logic;
--  signal ADC_CLK                :  std_logic;
--  signal OTR                    : std_logic;
--  signal D                      :  std_logic_vector ( 11 downto 0);
--  signal QSPI_NCS               : std_logic;
--  signal QSPI_CLK               : std_logic;
--  signal QSPI_IO0               :  std_logic; -- QSPI_MOSI
--  signal QSPI_IO1               :  std_logic; -- QSPI_MISO
--  signal QSPI_IO2               :  std_logic;
--  signal QSPI_IO3               :  std_logic;	
--  signal EVENT8                 : std_logic;
--  signal clock                  : std_logic;
--  signal clk_out                :  std_logic;
--  signal Interlock              : std_logic;					-- Interlock
--  signal Ext_Start_Out          :  std_logic;
--  signal Ext_Start_AUXOut       :  std_logic;
--  signal Ext_Start_IN           : std_logic;
--  signal inputs                 : std_logic_vector ( 7 downto 0);
--  signal outputs                :  std_logic_vector ( 7 downto 0);
--  signal F_CH1_IN               : std_logic;
--  signal F_CH2_IN               : std_logic;
--  signal F_CH3_IN               : std_logic;
--  signal F_CH4_IN               : std_logic; 
--  signal F_CH3_OUT              :  std_logic;
--  signal F_CH4_OUT              :  std_logic;
--  signal TX1_IN                 : std_logic;
--  signal RX1_IN                 : std_logic;
--  signal TX1_OUT                :  std_logic;
--  signal RX1_OUT                :  std_logic;
--  signal TX2_IN                 : std_logic;
--  signal RX2_IN                 : std_logic;
--  signal TX2_OUT                :  std_logic;
--  signal RX2_OUT                :  std_logic;
--  signal TX3_IN                 : std_logic;
--  signal RX3_IN                 : std_logic;
--  signal TX3_OUT                :  std_logic;
--  signal RX3_OUT                :  std_logic;
--  signal TX4_IN                 : std_logic;
--  signal RX4_IN                 : std_logic;
--  signal TX4_OUT                :  std_logic;
--  signal RX4_OUT                :  std_logic;
--  signal enable                 : std_logic;	
--  signal checking               :  std_logic;
--  signal Stop                   : std_logic;
--  signal aux_main               : std_logic;
--  signal timer_out              : std_logic_vector((4 - 1) downto 0);
--  signal aux_timer_out          : std_logic_vector((4 - 1) downto 0);
  
  constant c_pulse_number       : integer:= 4;
  signal rst                    : std_logic;
  signal pulse_gen_pulse        : std_logic_vector(c_pulse_number - 1 downto 0);
  signal pulse_gen_busy         : std_logic;
  signal pulse_gen_start        : std_logic;
  signal pulse_gen_delay_config : std_logic_vector(c_pulse_number*32 - 1 downto 0) := x"0000000A_00000008_00000005_00000001";

begin

clk_gen :
  process
  begin
    clk <= '0';
    wait for 20 ns/2;
    clk <= '1';
    wait for 20 ns/2;
  end process;
  
--  clock <= clk;
--
--main_proc :
--  process 
--  begin
--    Stop <= '1';
--    Ext_Start_IN <= '1';
--    inputs <= (others => '0');
--    wait for 300 ns;
--    inputs <= (others => '1');
--    Stop <= '0';
--    wait;
--  end process;
--
--sinus_inst : entity Sinus320D_Top 
-- port map(
--    MISO             => MISO             ,
--    MOSI             => MOSI             ,
--    CS               => CS               ,
--    SCK              => SCK              ,
--    ADC_CLK          => ADC_CLK          ,
--    OTR              => OTR              ,
--    D                => D                ,
--    QSPI_NCS         => QSPI_NCS         ,
--    QSPI_CLK         => QSPI_CLK         ,
--    QSPI_IO0         => QSPI_IO0         ,
--    QSPI_IO1         => QSPI_IO1         ,
--    QSPI_IO2         => QSPI_IO2         ,
--    QSPI_IO3         => QSPI_IO3         ,
--    EVENT8           => EVENT8           ,
--    clock            => clock            ,
--    clk_out          => clk_out          ,
--    Interlock        => Interlock        ,
--    Ext_Start_Out    => Ext_Start_Out    ,
--    Ext_Start_AUXOut => Ext_Start_AUXOut ,
--    Ext_Start_IN     => Ext_Start_IN     ,
--    inputs           => inputs           ,
--    outputs          => outputs          ,
--    F_CH1_IN         => F_CH1_IN         ,
--    F_CH2_IN         => F_CH2_IN         ,
--    F_CH3_IN         => F_CH3_IN         ,
--    F_CH4_IN         => F_CH4_IN         ,
--    F_CH3_OUT        => F_CH3_OUT        ,
--    F_CH4_OUT        => F_CH4_OUT        ,
--    TX1_IN           => TX1_IN           ,
--    RX1_IN           => RX1_IN           ,
--    TX1_OUT          => TX1_OUT          ,
--    RX1_OUT          => RX1_OUT          ,
--    TX2_IN           => TX2_IN           ,
--    RX2_IN           => RX2_IN           ,
--    TX2_OUT          => TX2_OUT          ,
--    RX2_OUT          => RX2_OUT          ,
--    TX3_IN           => TX3_IN           ,
--    RX3_IN           => RX3_IN           ,
--    TX3_OUT          => TX3_OUT          ,
--    RX3_OUT          => RX3_OUT          ,
--    TX4_IN           => TX4_IN           ,
--    RX4_IN           => RX4_IN           ,
--    TX4_OUT          => TX4_OUT          ,
--    RX4_OUT          => RX4_OUT          ,
--    enable           => enable           ,
--    checking         => checking         ,
--    Stop             => Stop             ,
--    aux_main         => aux_main         ,
--    timer_out        => timer_out        ,
--    aux_timer_out    => aux_timer_out    
--  );


rst_gen :
  process
  begin
    rst <= '1';
    wait for 100 ns;
    rst <= '0';
    wait;
  end process;

gen_start :
  process
  begin
    pulse_gen_start <= '0';
    wait for 1000 ns;
    pulse_gen_start <= '1';
    wait for 10 ns;
    pulse_gen_start <= '0';
    wait;
  end process;

PulseGen_Block_inst : entity PulseGen_Block
  generic map(
    c_pulse_number      => 4,
    c_pulse_duration    => 10
  )
  port map(
    clk                 => clk,
    rst                 => rst,
    start               => pulse_gen_start,
    delay_config        => pulse_gen_delay_config,
    busy                => pulse_gen_busy,
    pulse               => pulse_gen_pulse
  );


end TestBench_arc;