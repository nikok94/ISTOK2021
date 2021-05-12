
LIBRARY ieee;
USE ieee.std_logic_1164.all;

package main_properties is

  type SPI_REG_STRUCT is (
    -- read and write
    TIMER0, -- main
    TIMER1, -- slave
    TIMER2, -- trig
    TIMER3, -- fast adc
    AUX_TIMER0, -- Ufl ADC			(internal)
    AUX_TIMER1, -- Urec adc    (to controller)
    AUX_TIMER2, -- QUAD oscillogram 	(to controller)
    AUX_TIMER3, -- QUAD start       	(to controller)
    AUX_TIMER4, -- QUAD stop					(to controller)
    AUX_TIMER5, -- reserved					(to controller)

    CONTROL,
    SH_NUMBER,
    SH_PERIOD,

    OUTPUTS,

    -- read only
    CUR_SHOT,

    STATUS,
    FREQCH12,

    STRUCT_LENGTH
  );

  type CONTROL_STRUCT is (
    SLAVE_ENABLE,
    RST_ALL,
    STRUCT_LENGTH
  );

  type STATUS_STRUCT is (
    SHOOTING,
    INTERLOCK,
    TRIGATRON,
    SLAVE_READY,
    UNUSED0,
    UNUSED1,
    UNUSED2,
    UNUSED3,
    PININ0,
    STRUCT_LENGTH
  );

  constant PULSE_NUMBER                 : integer := SPI_REG_STRUCT'pos(CONTROL);
  constant PULSE_DURATION               : integer := 1000;
  constant SPI_REG_NUMBER               : integer := SPI_REG_STRUCT'pos(STRUCT_LENGTH);

  type SPI_REG_TYPE                     is array (0 to SPI_REG_STRUCT'pos(STRUCT_LENGTH)-1)                          of std_logic_vector(31 downto 0);
  type SPI_REG_PULSE_OFFSET_TYPE        is array (0 to PULSE_NUMBER-1)                                               of std_logic_vector(31 downto 0);

end main_properties;
