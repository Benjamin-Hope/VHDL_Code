library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--this code has no flaggs (almost flags)

--initially we will start by defining a no flag fifo;
entity FIFO is
  generic
  (
    g_width : natural := 8;
    g_depth : integer := 32
  );
  port
  (
    clk : in std_logic;
    rst : in std_logic;
    --FIFO write interface  
    i_wr_en   : in std_logic;
    i_wr_data : in std_logic_vector (g_width - 1 downto 0);
    o_full    : out std_logic;
    --FIFO read interface 
    i_rd_en   : in std_logic;
    o_rd_data : out std_logic_vector (g_width - 1 downto 0);
    o_emplty  : out std_logic
  );

end FIFO;

architecture rtl of FIFO is
  --define The fifo size 
  type t_FIFO is array (0 to g_depth) of std_logic_vector (g_width downto 0); --this is a 256 size fifo 
  signal s_fifo_data : t_FIFO := (others => (others => '0'));

  --Define the index or adress of the information in the FIFO 
  signal s_wr_index : integer range 0 to g_depth - 1 := 0;
  signal s_rd_index : integer range 0 to g_depth - 1 := 0;

  ---- # Words in FIFO, has extra range to allow for assert conditions
  signal s_FIFO_counter : integer range -1 to g_depth + 1 := 0;

  --create wires 
  signal sw_full  : std_logic;
  signal sw_empty : std_logic;

begin

  p_control : process (clk)
  begin
    if rising_edge (clk) then -- this is the sync of  the clock
      if rst = '1' then
        --reset 
        s_FIFO_counter <= 0;
        s_rd_index     <= 0;
        s_wr_index     <= 0;
      else
        -- keep track of the total amout of words in the fifo 
        if (i_wr_en = '1' and i_rd_en = '0') then
          s_FIFO_counter <= s_FIFO_counter + 1;
        elsif (i_wr_en = '0' and i_rd_en = '1') then
          s_FIFO_counter <= s_FIFO_counter - 1;
        end if;

        -- control of the overflow and write index 
        if (i_wr_en = '1' and sw_full = '0') then
          if (s_wr_index = g_depth - 1) then
            s_wr_index <= 0; --reset
          else
            s_wr_index <= s_wr_index + 1;
          end if;
        end if;

        -- control the read index 
        if (i_rd_en = '1' and sw_empty = '0') then
          if (s_rd_index = g_depth - 1) then
            s_rd_index <= 0;-- reset
          else
            s_rd_index <= s_rd_index + 1;
          end if;
        end if;

        --when write it high, register the information 
        if i_wr_en = '1' then
          s_fifo_data(s_wr_index) <= i_wr_data; -- writes the data in the empty position 
        end if;
        --
      end if;
    end if;
  end process p_control;

  --make the necessary data connections 
  o_full    <= sw_full;
  o_emplty  <= sw_empty;
  o_rd_data <= s_fifo_data(s_rd_index);

  sw_full <= '1' when s_FIFO_counter = g_depth else
    '0';
  sw_empty <= '1' when s_FIFO_counter = 0 else
    '0';

  --create warnings for when special events happen: 
  -- ASSERTION LOGIC - Not synthesized
  -- synthesis translate_off

  p_ASSERT : process (clk) is
  begin
    if rising_edge(clk) then
      if i_wr_en = '1' and sw_full = '1' then
        report "ASSERT FAILURE - MODULE_REGISTER_FIFO: FIFO IS FULL AND BEING WRITTEN " severity failure;
      end if;

      if i_rd_en = '1' and sw_empty = '1' then
        report "ASSERT FAILURE - MODULE_REGISTER_FIFO: FIFO IS EMPTY AND BEING READ " severity failure;
      end if;
    end if;
  end process p_ASSERT;

  -- synthesis translate_on

end rtl;