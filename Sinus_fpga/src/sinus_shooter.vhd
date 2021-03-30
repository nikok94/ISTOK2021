
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library altera; 
use altera.altera_primitives_components.all;

library work;
use work.freq_gen;
use work.timer;
use work.main_properties.all;


entity sinus_shooter is
  generic (
    pulse_duration   : integer := 1000
  );
  port (
    clk                     : in std_logic;
    n_reset                 : in std_logic;

    timers                  : in timer_bank_type;
    timer_masks             : in timer_mask_bank_type;
    aux_timers              : in aux_timer_bank_type;
    aux_timer_masks         : in aux_timer_mask_bank_type;
    
    shots_period            : in std_logic_vector(31 downto 0);
    
    curr_shots_period       : out std_logic_vector(31 downto 0);
    
    shots_number            : in std_logic_vector(31 downto 0);
    cur_shot                : out std_logic_vector(31 downto 0);

    shooter_running         : out std_logic;
    ready                   : out std_logic;
    outputs                 : out std_logic_vector((TIMERS_NUMBER - 1) downto 0);
    aux_outputs             : out std_logic_vector((AUX_TIMERS_NUMBER - 1) downto 0)
  );
end sinus_shooter;


--  Architecture Body

architecture sinus_shooter_arc OF sinus_shooter IS
    type inp_masks_type     is array ((TIMERS_NUMBER - 1) downto 0) of std_logic_vector(TMASK'pos(INP_EXT) downto 0);
    type inp_aux_masks_type is array ((AUX_TIMERS_NUMBER - 1) downto 0) of std_logic_vector(TMASK'pos(INP_EXT) downto 0);
    type out_masks_type     is array ((TIMERS_NUMBER - 1) downto 0) of std_logic_vector((TMASK'pos(OUT_T2) - TMASK'pos(OUT_T0)) downto 0);
    
    signal timer_outputs            : std_logic_vector((TIMERS_NUMBER - 1) downto 0);
    signal aux_timer_outputs        : std_logic_vector((AUX_TIMERS_NUMBER - 1) downto 0);
    
    signal masked_timer_outputs     : out_masks_type;
    signal masked_timer_starts      : inp_masks_type;
    signal masked_aux_timer_starts  : inp_aux_masks_type;
    
    signal timer_starts             : std_logic_vector((TIMERS_NUMBER - 1) downto 0);
    signal aux_timer_starts         : std_logic_vector((AUX_TIMERS_NUMBER - 1) downto 0);
    
    signal timer_starts_to_mask     : std_logic_vector(TMASK'pos(INP_EXT) downto 0);
    
    signal period_gen               : std_logic;
    signal freq_running             : std_logic;
    
    signal int_timers               : timer_bank_type;
    signal int_aux_timers           : aux_timer_bank_type;
    signal int_shots_period         : std_logic_vector(31 downto 0);
    signal ext_start_sig            : std_logic;
    signal ext_trigger              : std_logic;
    
    signal timer_busy               : std_logic_vector((TIMERS_NUMBER - 1) downto 0);
    signal aux_timer_busy           : std_logic_vector((AUX_TIMERS_NUMBER - 1) downto 0);
    
    signal timers_rdy               : std_logic;
    signal aux_timers_rdy           : std_logic;

begin

shooter_running <= freq_running;

ready <= timers_rdy and aux_timers_rdy;

parity_proc :
  process(clk)
    variable pbusy   : std_logic;
  begin
    if rising_edge(clk) then
      pbusy := '0';
      loop_i : for i in 0 to timer_busy'length - 1 loop
        pbusy := pbusy xor timer_busy(i);
      end loop;
      timers_rdy <= not pbusy;
    end if;
  end process;

aux_parity_proc :
  process(clk)
    variable aux_pbusy   : std_logic;
  begin
    if rising_edge(clk) then
      aux_pbusy := '0';
      aux_loop_i : for i in 0 to aux_timer_busy'length - 1 loop
        aux_pbusy := aux_pbusy xor aux_timer_busy(i);
      end loop;
      aux_timers_rdy <= not aux_pbusy;
    end if;
  end process;

-- Insert shooting period generator
l_shgen: entity freq_gen
  generic map(
    pulse_duration => pulse_duration
  )
  port map (
    clk                 => clk,
    n_reset             => n_reset,
    period              => shots_period,
    shots_number        => shots_number,
    cur_shot            => cur_shot,
    freq_gen_running    => freq_running,
    output              => period_gen
  );

curr_shots_period <= shots_period;

  l_outs: for i in (TIMERS_NUMBER - 1) downto 0 generate
    outputs(i) <= timer_outputs(i);
  end generate;

  -- Generate timer start pulses in basis of in_masks and timer_outputs
  l_mask_st: for i in (TIMERS_NUMBER - 1) downto 0 generate
    l_mask_sti: for j in TMASK'pos(INP_EXT) downto 0 generate
      masked_timer_starts(i)(j) <= timer_masks(i)(j) and timer_starts_to_mask(j);
      end generate;
  end generate;

ext_trigger <= '0';

  timer_starts_to_mask <= ext_trigger & aux_timer_outputs & timer_outputs & period_gen;

  l_starts: for i in (TIMERS_NUMBER - 1) downto 0 generate
    timer_starts(i) <= or_reduce(masked_timer_starts(i));
  end generate;
  
  l_timers: for i in (TIMERS_NUMBER - 1) downto 0 generate
    l_timers_i: entity timer 
      generic map(
        pulse_duration => pulse_duration
      )
      port map(
        clk => clk,
        n_reset => n_reset,
        start => timer_starts(i),
        busy => timer_busy(i),
        value => timers(i),
        output => timer_outputs(i)
      );
  end generate;
  
  -- Generate timer start pulses in basis of in_masks and timer_outputs
  l_mask_aux_st: for i in (AUX_TIMERS_NUMBER - 1) downto 0 generate
    l_mask_aux_sti: for j in TMASK'pos(INP_EXT) downto 0 generate
        masked_aux_timer_starts(i)(j) <= aux_timer_masks(i)(j) and timer_starts_to_mask(j);
      end generate;
  end generate;
      
  l_aux_starts: for i in (AUX_TIMERS_NUMBER - 1) downto 0 generate
    aux_timer_starts(i) <= or_reduce(masked_aux_timer_starts(i));
  end generate;
  
  l_aux_timers: for i in (AUX_TIMERS_NUMBER - 1) downto 0 generate
    l_aux_timers_i: entity timer 
      generic map(
        pulse_duration => pulse_duration
      )
      port map(
        clk => clk,
        n_reset => n_reset,
        start => aux_timer_starts(i),
        busy => aux_timer_busy(i),
        value => aux_timers(i),
        output => aux_timer_outputs(i)
      );
  end generate;
  
  aux_outputs <= aux_timer_outputs;
  
end sinus_shooter_arc;
