library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity darius2dcomp is
port(
	-- DARIUS II CONNECTIONS
	clk: in std_logic; -- ~12MHz clock. 74LS74 @ IC53 pin 5.
	
	-- Simultaneous RGB inputs for TC0070RGB #1 at IC58.
	a_in_r: in std_logic_vector(4 downto 0); -- DAC pins 16-12.
	a_in_g: in std_logic_vector(4 downto 0); -- DAC pins 11-7.
	a_in_b: in std_logic_vector(4 downto 0); -- DAC pins 6-2.
	-- ...and TC0070RGB #2 at IC41.
	b_in_r: in std_logic_vector(4 downto 0); -- DAC pins 16-12.
	b_in_g: in std_logic_vector(4 downto 0); -- DAC pins 11-7.
	b_in_b: in std_logic_vector(4 downto 0); -- DAC pins 6-2.
	
	hbl_in_n: in std_logic; -- Horizontal blank. DAC pin 18.
	vbl_in_n: in std_logic; -- Vertical blank. DAC pin 19.
	lat_in_n: in std_logic; -- Pixel latch, ~6MHz. DAC pin 20.
	
	csync_in_n: in std_logic; -- Composite video sync. Pin 13 of IC105
	
	sw_in: in std_logic_vector(17 downto 0); -- Switches.
	
	-- OUT
	vga_red_out: out std_logic_vector(9 downto 0);
	vga_green_out: out std_logic_vector(9 downto 0);
	vga_blue_out: out std_logic_vector(9 downto 0);
	vga_hsync_out: out std_logic;
	vga_vsync_out: out std_logic;
	vga_blank_out: out std_logic;
	vga_sog_out: out std_logic;
	vga_clk_out: out std_logic
	
);
end entity;

architecture behavioral of darius2dcomp is

signal buffer_en_0: std_logic := '0';
signal buffer_en_1: std_logic := '0';

-- Input signals for monitors A and B
signal buffer_in_a0: std_logic_vector(14 downto 0);
signal buffer_in_b0: std_logic_vector(14 downto 0);
signal buffer_in_a1: std_logic_vector(14 downto 0);
signal buffer_in_b1: std_logic_vector(14 downto 0);

signal buffer_sel: std_logic := '0';

signal hbl: std_logic := '0';
signal hbl_prev: std_logic := '0';
signal hbl_prev_2: std_logic := '0';

signal raw_in_a: std_logic_vector(14 downto 0);
signal raw_in_b: std_logic_vector(14 downto 0);

-- Output signals, pre-mux, from the monitor buffers
signal buffer_out_a0: std_logic_vector(14 downto 0);
signal buffer_out_a1: std_logic_vector(14 downto 0);
signal buffer_out_b0: std_logic_vector(14 downto 0);
signal buffer_out_b1: std_logic_vector(14 downto 0);

signal hcount: std_logic_vector(17 downto 0) := (others => '0');

signal routed_output: std_logic_vector(14 downto 0);

constant BACKPORCH: integer := 0;

constant SAMPLE_START: integer := 200;
constant SAMPLE_LEN: integer := 640;
constant SAMPLE_END: integer := SAMPLE_LEN + SAMPLE_START;

constant SCANOUT_START: integer := 128;
constant SCANOUT_LEN: integer := 640;
constant SCANOUT_END: integer := SCANOUT_LEN + SCANOUT_START;

begin

-- Monitor A input buffers
	buffer_a0: entity work.linebuffer(behavioral) port map (clk, buffer_en_0, buffer_in_a0, buffer_out_a0);
	buffer_a1: entity work.linebuffer(behavioral) port map (clk, buffer_en_1, buffer_in_a1, buffer_out_a1);
-- Monitor B input buffers
	buffer_b0: entity work.linebuffer(behavioral) port map (clk, buffer_en_0, buffer_in_b0, buffer_out_b0);
	buffer_b1: entity work.linebuffer(behavioral) port map (clk, buffer_en_1, buffer_in_b1, buffer_out_b1);

	raw_in_a <= a_in_r & a_in_g & a_in_b;
	raw_in_b <= b_in_r & b_in_g & b_in_b;
	
	edge_tracker: process(clk)
	begin
		if (rising_edge(clk)) then
			hbl_prev_2 <= hbl_prev;
			hbl_prev <= hbl;
			hbl <= hbl_in_n;
		end if;
	end process;
	
	line_count_proc: process(clk)
	begin
		if (rising_edge(clk)) then
			-- Vblank resets.
			if (hbl = '1' and hbl_prev = '0') then
				hcount <= (others => '0');
			-- Start of active raster.
			else
				hcount <= hcount + 1;
			end if;
		end if;
	end process;
	
	line_buffer_select: process(buffer_out_a0, buffer_out_b0, buffer_out_a1, buffer_out_b1, raw_in_a, raw_in_b, buffer_sel, hbl_in_n, hbl_prev_2, lat_in_n)
	begin
		if (buffer_sel = '0') then
			-- BUFFER 0 SCANOUT
			routed_output <= buffer_out_a0;
			buffer_in_a0 <= buffer_out_b0;
			buffer_in_b0 <= (others => '0');
			buffer_en_0 <= not hbl_prev_2;
			
			-- Buffer 1 should simply capture during this time.
			buffer_in_a1 <= raw_in_a;
			buffer_in_b1 <= raw_in_b;
			buffer_en_1 <= (not hbl_prev_2) or lat_in_n;
		else
			-- BUFFER 1 SCANOUT
			routed_output <= buffer_out_a1;
			buffer_in_a1 <= buffer_out_b1;
			buffer_in_b1 <= (others => '0');
			buffer_en_1 <= not hbl_prev_2;
			
			-- Buffer 0 should simply capture during this time.
			buffer_in_a0 <= raw_in_a;
			buffer_in_b0 <= raw_in_b;
			buffer_en_0 <= (not hbl_prev_2) or lat_in_n;
		end if;
	end process;
	
	buffer_switch: process(clk)
	begin
		if (rising_edge(clk)) then
			-- End of line swaps buffers.
			if (vbl_in_n = '0') then
				buffer_sel <= '0';
			elsif (hbl = '1' and hbl_prev = '0') then
				buffer_sel <= not buffer_sel;
			end if;
		end if;
	end process;
	
	vga_clk_out <= not clk;

	vga_output_set: process(clk)
	begin
		if (rising_edge(clk)) then
			vga_blank_out <= hbl_prev_2 and vbl_in_n;
			vga_sog_out <= '0';
			vga_hsync_out <= csync_in_n;
			vga_vsync_out <= '1';
			vga_red_out <= routed_output(14 downto 10) & routed_output(14 downto 10);
			vga_green_out <= routed_output(9 downto 5) & routed_output(9 downto 5);
			vga_blue_out <= routed_output(4 downto 0) & routed_output(4 downto 0);
			--vga_red_out <= (others => '0');
			--vga_green_out <= "0" & hbl_in_n & "00000000";
			--vga_blue_out <= a_in_b & a_in_b;
		end if;
	end process;
	
end architecture;