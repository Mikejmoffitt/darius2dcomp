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

signal buffer_en_a0: std_logic := '0';
signal buffer_en_a1: std_logic := '0';
signal buffer_en_b0: std_logic := '0';
signal buffer_en_b1: std_logic := '0';

-- Input signals for monitors A and B
signal buffer_in_a0: std_logic_vector(14 downto 0);
signal buffer_in_a1: std_logic_vector(14 downto 0);
signal buffer_in_b0: std_logic_vector(14 downto 0);
signal buffer_in_b1: std_logic_vector(14 downto 0);

signal buffer_sel: std_logic := '0';
signal buffer_sel_prev: std_logic := '0';

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

signal buffer_pretap_a0: std_logic_vector(14 downto 0);
signal buffer_pretap_a1: std_logic_vector(14 downto 0);
signal buffer_pretap_b0: std_logic_vector(14 downto 0);
signal buffer_pretap_b1: std_logic_vector(14 downto 0);

signal hcount: std_logic_vector(17 downto 0) := (others => '0');

signal routed_output: std_logic_vector(14 downto 0);

constant BACKPORCH: integer := 0;

constant RASTER_W: integer := 322;

constant HCOUNT_ADJ: integer := 206;

constant HCOUNT_CAP_START: integer := HCOUNT_ADJ;
constant HCOUNT_CAP_END: integer := HCOUNT_CAP_START + (2 * RASTER_W) + 2;

constant HCOUNT_A_START: integer := HCOUNT_ADJ + 2;
constant HCOUNT_A_END: integer := HCOUNT_A_START + RASTER_W;

constant HCOUNT_B_START: integer := HCOUNT_ADJ + RASTER_W;
constant HCOUNT_B_END: integer := HCOUNT_B_START + RASTER_W;

signal capture_en: std_logic := '0';
signal scanout_a_en: std_logic := '0';
signal scanout_b_en: std_logic := '0';
signal display_en: std_logic := '0';

signal debug_a_color: std_logic_vector(14 downto 0) := "111100111001110";
signal debug_b_color: std_logic_vector(14 downto 0) := "011100111011110";
signal debug_blank_color: std_logic_vector(14 downto 0) := "000000000000000";

begin

-- Monitor A input buffers
	buffer_a0: entity work.linebuffer(behavioral) generic map(RASTER_W) port map (clk, buffer_en_a0, buffer_in_a0, buffer_out_a0, buffer_pretap_a0);
	buffer_a1: entity work.linebuffer(behavioral) generic map(RASTER_W) port map (clk, buffer_en_a1, buffer_in_a1, buffer_out_a1, buffer_pretap_a1);
-- Monitor B input buffers                                                                              
	buffer_b0: entity work.linebuffer(behavioral) generic map(RASTER_W) port map (clk, buffer_en_b0, buffer_in_b0, buffer_out_b0, buffer_pretap_b0);
	buffer_b1: entity work.linebuffer(behavioral) generic map(RASTER_W) port map (clk, buffer_en_b1, buffer_in_b1, buffer_out_b1, buffer_pretap_b1);

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
			if (hbl = '0' and hbl_prev = '1') then
				hcount <= (others => '0');
			-- Start of active raster.
			else
				hcount <= hcount + 1;
			end if;
		end if;
	end process;
	
	buffer_signals: process(buffer_out_a0, buffer_out_b0, buffer_out_a1, buffer_out_b1,
	                            raw_in_a, raw_in_b, buffer_sel, hbl_in_n, hbl_prev,
										 hbl_prev_2, lat_in_n, sw_in, hcount,
										 buffer_pretap_b0, buffer_pretap_b1)
	begin
	-- (not hbl_prev or hbl_in_n) or lat_in_n
		if (hcount >= HCOUNT_CAP_START and hcount < HCOUNT_CAP_END) then
			capture_en <= '0' or hcount(0);
		else
			capture_en <= '1';
		end if;
		
		if (hcount >= HCOUNT_A_START and hcount < HCOUNT_A_END) then
			scanout_a_en <= not (hbl_prev or hbl_in_n);
		else
			scanout_a_en <= '1';
		end if;
		
		if (hcount >= HCOUNT_B_START and hcount < HCOUNT_B_END) then
			scanout_b_en <= not (hbl_prev or hbl_in_n);
		else
			scanout_b_en <= '1';
		end if;
		
		if (hcount >= HCOUNT_A_START and hcount <= HCOUNT_B_END) then
			display_en <= '1';
		else
			display_en <= '0';
		end if;
	end process;
	
	line_buffer_select: process(buffer_out_a0, buffer_out_b0, buffer_out_a1, buffer_out_b1,
	                            raw_in_a, raw_in_b, buffer_sel, hbl_in_n, hbl_prev,
										 hbl_prev_2, lat_in_n, sw_in, hcount, capture_en,
										 buffer_pretap_b0, buffer_pretap_b1,
										 scanout_a_en, scanout_b_en)
	begin
		if (buffer_sel = '0') then
			-- BUFFER 0 SCANOUT
			if (scanout_a_en = '0') then
				routed_output <= buffer_out_a0;
			elsif (scanout_b_en = '0') then
				routed_output <= buffer_out_b0;  
			else
				routed_output <= debug_blank_color;
			end if;
			buffer_in_a0 <= (others => '0');
			buffer_in_b0 <= (others => '0');
			buffer_en_a0 <= scanout_a_en;
			buffer_en_b0 <= scanout_b_en;
			
			-- Buffer 1 CAPTURE
			if (sw_in(0) = '1') then
				buffer_in_a1 <= debug_a_color;
			else
				buffer_in_a1 <= raw_in_a;
			end if;
			if (sw_in(1) = '1') then
				buffer_in_b1 <= debug_b_color;
			else
				buffer_in_b1 <= raw_in_b;
			end if;
				
			buffer_en_a1 <= capture_en;
			buffer_en_b1 <= capture_en;
		else
			-- BUFFER 1 SCANOUT
			if (scanout_a_en = '0') then
				routed_output <= buffer_out_a1;
			elsif (scanout_b_en = '0') then
				routed_output <= buffer_out_b1;  
			else
				routed_output <= debug_blank_color;
			end if;
			buffer_in_a1 <= (others => '0');
			buffer_in_b1 <= (others => '0');
			buffer_en_a1 <= scanout_a_en;
			buffer_en_b1 <= scanout_b_en;
			
			-- Buffer 0 CAPTURE
			if (sw_in(0) = '1') then
				buffer_in_a0 <= debug_a_color;
			else
				buffer_in_a0 <= raw_in_a;
			end if;
			if (sw_in(1) = '1') then
				buffer_in_b0 <= debug_b_color;
			else
				buffer_in_b0 <= raw_in_b;
			end if;
			
			buffer_en_a0 <= capture_en;
			buffer_en_b0 <= capture_en;
		end if;
	end process;
	
	buffer_switch: process(clk)
	begin
		if (rising_edge(clk)) then
			-- End of line swaps buffers.
			buffer_sel_prev <= buffer_sel;
			if (vbl_in_n = '0') then
				buffer_sel <= '0';
			elsif (hcount = 0) then
				buffer_sel <= not buffer_sel;
			end if;
		end if;
	end process;
	
	vga_clk_out <= not clk;

	vga_output_set: process(clk)
	begin
		if (rising_edge(clk)) then
			if (sw_in(7) = '1') then
				vga_blank_out <= display_en;
			elsif (sw_in(5) = '0') then
				vga_blank_out <= hbl_in_n and vbl_in_n;
			elsif (sw_in(6) = '0') then
				vga_blank_out <= '0';
			else
				vga_blank_out <= '1';
			end if;
			if (sw_in(2) = '1') then
				vga_sog_out <= csync_in_n;
			else
				vga_sog_out <= '0';
			end if;
			vga_hsync_out <= csync_in_n;
			vga_vsync_out <= '1';
			if (sw_in(3) = '1') then
				vga_red_out <= a_in_r & a_in_r;
				vga_green_out <= a_in_g & a_in_g;
				vga_blue_out <= a_in_b & a_in_b;
			elsif (sw_in(4) = '1') then
				vga_red_out <= b_in_r & b_in_r;
				vga_green_out <= b_in_g & b_in_g;
				vga_blue_out <= b_in_b & b_in_b;
			else
				vga_red_out <= routed_output(14 downto 10) & routed_output(14 downto 10);
				vga_green_out <= routed_output(9 downto 5) & routed_output(9 downto 5);
				vga_blue_out <= routed_output(4 downto 0) & routed_output(4 downto 0);
			end if;
		end if;
	end process;
	
end architecture;