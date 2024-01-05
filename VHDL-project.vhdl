library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


FOR 6 Register

entity Computer is
  port (
    clk      : in std_logic;
    address_bus : in std_logic_vector(15 downto 0);
    data_bus   : inout std_logic_vector(15 downto 0)
  );
end entity Computer;

architecture Structural of Computer is

  component Register
    port (
      clk      : in std_logic;
      load     : in std_logic;
      increase : in std_logic;
      clear    : in std_logic;
      data_in  : in std_logic_vector(15 downto 0);
      data_out : out std_logic_vector(15 downto 0)
    );
  end component;

  signal ar : std_logic_vector(15 downto 0);
  signal pc : std_logic_vector(15 downto 0);
  signal dr : std_logic_vector(15 downto 0);
  signal ac : std_logic_vector(15 downto 0);
  signal ir : std_logic_vector(15 downto 0);
  signal tr : std_logic_vector(15 downto 0);

begin

  ar_register : Register
  port map (
    clk => clk,
    load => load,
    increase => increase,
    clear => clear,
    data_in => address_bus,
    data_out => ar
  );

  pc_register : Register
  port map (
    clk => clk,
    load => load,
    increase => increase,
    clear => clear,
    data_in => ar,
    data_out => pc
  );

  dr_register : Register
  port map (
    clk => clk,
    load => load,
    increase => increase,
    clear => clear,
    data_in => data_bus,
    data_out => dr
  );

  ac_register : Register
  port map (
    clk => clk,
    load => load,
    increase => increase,
    clear => clear,
    data_in => data_bus,
    data_out => ac
  );

  ir_register : Register
  port map (
    clk => clk,
    load => load,
    increase => increase,
    clear => clear,
    data_in => data_bus,
    data_out => ir
  );

  tr_register : Register
  port map (
    clk => clk,
    load => load,
    increase => increase,
    clear => clear
	data_in => data_bus,
    data_out => tr
	);

end architecture Behavioral;

#FOR Atribute clear , load ,increase


entity Register16 is

	port(

	--inputs:

		input :in std_logic_vector(15 downto 0); -- comes from bus
		LD :in std_logic;
		CLR :in std_logic;
		INR :in std_logic;

	--outputs:
		output :out std_logic_vector(15 downto 0) -- goes to bus

	);

end Register16;


architecture behavioral of Register16 is 

signal val : std_logic_vector(15 downto 0);

begin

	output <= val ;

--load from bus
	load: Process(LD) is
	begin 
		if(LD) then
			val <= input;
		end if;
	end process load;


--increase value by one
	increase: Process(INR='1') is
	variable i :integer range 0 to 1;
	begin 
		if(INR='1') then
			i := 1;
			val <= std_logic_vector((to_unsigned(i,val''range) + unsigned(val));
		end if;
	end process increase;


--clear the register
	clear: Process(CLR) is
	begin 
		if(CLR='1') then
			val<="0000000000000000"
		end if;
	end process clear;

	
end behavioral;

	
--------------------------------------------------------------------

entity ALU is

  port (
    a         : in std_logic_vector(15 downto 0);
    b         : in std_logic_vector(15 downto 0);
   op (opcode): in std_logic_vector(3 downto 0);
    result    : out std_logic_vector(15 downto 0);
    carryout  : out std_logic
  );
end entity ALU;

architecture Behavioral of ALU is

  signal sum : std_logic_vector(15 downto 0);
  signal borrow : std_logic;
 begin

  adder : ALU
  port map (
    a => a,
    b => b,
    op => op,
    result => sum,
    carryout => carryout
  );

end architecture Behavioral;

####make another way


ENTITY ALU IS PORT (
   	clk : in std_logic;	
     
	A : in std_logic_vector(15 downto 0);
	B : in std_logic_vector(15 downto 0);
	output : out std_logic_vector (15 downto 0);
	
	ALU_Cin : in std_logic;
	ALU_Zin : in std_logic;
	
	ALU_Cout : out std_logic;
	ALU_Zout : out std_logic;

	B15to0 : in std_logic;
	AandB : in std_logic;
	AorB : in std_logic;
	Bshl : in std_logic;
  	Bshr : in std_logic;
	AcmpB : in std_logic;
	AaddB : in std_logic;
	AsubB : in std_logic;
	AxorB : in std_logic;
	Btws : in std_logic;
	AmulB : in std_logic;
	AdivB : in std_logic;
	Bsqr : in std_logic;
	rand : in std_logic;
	AnotB : in std_logic;
	sinB : in std_logic;
	cosB : in std_logic;
	tanB : in std_logic;
	cotB : in std_logic);  
END ALU;


ARCHITECTURE dataflow OF ALU IS 

component register is 
generic (n : integer := 16);

 port(
  
  inputA , inputB : in std_logic_vector(n - 1 downto 0);
  carryin : in std_logic;
  result  : out std_logic_vector(n downto 0));
end component;

	signal sum : std_logic_vector(16 downto 0);
	signal sub : std_logic_vector(16 downto 0);
	signal Asigned : signed(15 downto 0);
	signal Asigned_save : signed (15 downto 0);
	signal Bsigned : signed(15 downto 0);
	signal output_tmp : std_logic_vector(15 downto 0);
 	signal Btws_c : std_logic_vector (15 downto 0);
	type T is array (0 to 10) of integer;
	signal triangle_pos : T := (0,1,2,3,4,5,6,7,8,9,10);
	signal triangle_neg : T := (0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10);

BEGIN
   
   alu_adder: adder16 port map(inputA => A , inputB => B , carryin => ALU_Cin , result => sum);

   output <= output_tmp;
   Bsigned <= signed(B);
   Asigned <= signed(A); 


  process (clk) 

		variable  tmp : std_logic_vector(15 downto 0) := (others => '0');
		variable  BinInt : integer;
		variable  randreg : std_logic_vector(15 downto 0) := "0010101001000010";
		variable  endloop : std_logic := '0';
		variable  counter : std_logic_vector(15 downto 0) := (others => '0');
  begin
    
	  if clk'event and clk = '1' then
		randreg(10) := randreg(0) xor randreg(11);
		randreg(12) := randreg(0) xor randreg(13);
		randreg(13) := randreg(0) xor randreg(14);

		randreg := randreg(0) & randreg(15 downto 1);
		
		-- A and B (tested)                                                            
		if (AandB = '1') then                                                          
		   output_tmp <= (A and B);                                                        
		--------------------


	   -- AdivB (tested)
	   elsif (AdivB = '1') then
		   for i in 0 to to_integer(unsigned(A))+1 loop
			   if (endloop = '0') then
				   if (Asigned - Bsigned * signed(counter) >= Bsigned) then
					   counter := std_logic_vector(unsigned(counter) + 1);
				   elsif (Asigned - Bsigned * signed(counter) <= 0) then
					   endloop := '1';
				   end if;
			   end if;
			end loop;
			output_tmp <= counter;
	   ---------------------

	   	-- sqr (tested)
	   	elsif (Bsqr = '1') then
			if (Bsigned = 0 ) then
				output_tmp <= (others => '0' );
			elsif (Bsigned < 4 ) then
				 output_tmp <= "0000000000000001";
			elsif (Bsigned < 9 ) then
				 output_tmp <= "0000000000000010";
			elsif (Bsigned < 16 ) then
				 output_tmp <= "0000000000000011";
			elsif (Bsigned < 25 ) then
				 output_tmp <= "0000000000000100";
			elsif (Bsigned < 36 ) then
				 output_tmp <= "0000000000000101";
			elsif (Bsigned < 49 ) then
				 output_tmp <= "0000000000000110";
			elsif (Bsigned < 64 ) then
				 output_tmp <= "0000000000000111";
			elsif (Bsigned < 81 ) then
				 output_tmp <= "0000000000001000";
			elsif (Bsigned < 100 ) then
				 output_tmp <= "0000000000001001";
			elsif (Bsigned < 121 ) then
				 output_tmp <= "0000000000001010";
			elsif (Bsigned < 144 ) then
				 output_tmp <= "0000000000001011";
			elsif (Bsigned < 169 ) then
				 output_tmp <= "0000000000001100";
			elsif (Bsigned < 196 ) then
				 output_tmp <= "0000000000001101";
			elsif (Bsigned < 225 ) then
				 output_tmp <= "0000000000001110";
			elsif (Bsigned < 256 ) then
				 output_tmp <= "0000000000001111";
			elsif (Bsigned < 289 ) then
				 output_tmp <= "0000000000010000";
			elsif (Bsigned < 324 ) then
				 output_tmp <= "0000000000010001";
			elsif (Bsigned < 361 ) then
				 output_tmp <= "0000000000010010";
			elsif (Bsigned < 400 ) then
				 output_tmp <= "0000000000010011";
			elsif (Bsigned < 441 ) then
				 output_tmp <= "0000000000010100";
			elsif (Bsigned < 484 ) then
				 output_tmp <= "0000000000010101";
			elsif (Bsigned < 529 ) then
				 output_tmp <= "0000000000010110";
			elsif (Bsigned < 576 ) then
				 output_tmp <= "0000000000010111";
			elsif (Bsigned < 625 ) then
				 output_tmp <= "0000000000011000";
			elsif (Bsigned < 676 ) then
				 output_tmp <= "0000000000011001";
			elsif (Bsigned < 729 ) then
				 output_tmp <= "0000000000011010";
			elsif (Bsigned < 784 ) then
				 output_tmp <= "0000000000011011";
			elsif (Bsigned < 841 ) then
				 output_tmp <= "0000000000011100";
			elsif (Bsigned < 900 ) then
				 output_tmp <= "0000000000011101";
			elsif (Bsigned < 961 ) then
				 output_tmp <= "0000000000011110";
			elsif (Bsigned < 1024 ) then
				 output_tmp <= "0000000000011111";
			elsif (Bsigned < 1089 ) then
				 output_tmp <= "0000000000100000";
			elsif (Bsigned < 1156 ) then
				 output_tmp <= "0000000000100001";
			elsif (Bsigned < 1225 ) then
				 output_tmp <= "0000000000100010";
			elsif (Bsigned < 1296 ) then
				 output_tmp <= "0000000000100011";
			elsif (Bsigned < 1369 ) then
				 output_tmp <= "0000000000100100";
			elsif (Bsigned < 1444 ) then
				 output_tmp <= "0000000000100101";
			elsif (Bsigned < 1521 ) then
				 output_tmp <= "0000000000100110";
			elsif (Bsigned < 1600 ) then
				 output_tmp <= "0000000000100111";
			elsif (Bsigned < 1681 ) then
				 output_tmp <= "0000000000101000";
			elsif (Bsigned < 1764 ) then
				 output_tmp <= "0000000000101001";
			elsif (Bsigned < 1849 ) then
				 output_tmp <= "0000000000101010";
			elsif (Bsigned < 1936 ) then
				 output_tmp <= "0000000000101011";
			elsif (Bsigned < 2025 ) then
				 output_tmp <= "0000000000101100";
			elsif (Bsigned < 2116 ) then
				 output_tmp <= "0000000000101101";
			elsif (Bsigned < 2209 ) then
				 output_tmp <= "0000000000101110";
			elsif (Bsigned < 2304 ) then
				 output_tmp <= "0000000000101111";
			elsif (Bsigned < 2401 ) then
				 output_tmp <= "0000000000110000";
			elsif (Bsigned < 2500 ) then
				 output_tmp <= "0000000000110001";
			elsif (Bsigned < 2601 ) then
				 output_tmp <= "0000000000110010";
			elsif (Bsigned < 2704 ) then
				 output_tmp <= "0000000000110011";
			elsif (Bsigned < 2809 ) then
				 output_tmp <= "0000000000110100";
			elsif (Bsigned < 2916 ) then
				 output_tmp <= "0000000000110101";
			elsif (Bsigned < 3025 ) then
				 output_tmp <= "0000000000110110";
			elsif (Bsigned < 3136 ) then
				 output_tmp <= "0000000000110111";
			elsif (Bsigned < 3249 ) then
				 output_tmp <= "0000000000111000";
			elsif (Bsigned < 3364 ) then
				 output_tmp <= "0000000000111001";
			elsif (Bsigned < 3481 ) then
				 output_tmp <= "0000000000111010";
			elsif (Bsigned < 3600 ) then
				 output_tmp <= "0000000000111011";
			elsif (Bsigned < 3721 ) then
				 output_tmp <= "0000000000111100";
			elsif (Bsigned < 3844 ) then
				 output_tmp <= "0000000000111101";
			elsif (Bsigned < 3969 ) then
				 output_tmp <= "0000000000111110";
			elsif (Bsigned < 4096 ) then
				 output_tmp <= "0000000000111111";
			elsif (Bsigned < 4225 ) then
				 output_tmp <= "0000000001000000";
			elsif (Bsigned < 4356 ) then
				 output_tmp <= "0000000001000001";
			elsif (Bsigned < 4489 ) then
				 output_tmp <= "0000000001000010";
			elsif (Bsigned < 4624 ) then
				 output_tmp <= "0000000001000011";
			elsif (Bsigned < 4761 ) then
				 output_tmp <= "0000000001000100";
			elsif (Bsigned < 4900 ) then
				 output_tmp <= "0000000001000101";
			elsif (Bsigned < 5041 ) then
				 output_tmp <= "0000000001000110";
			elsif (Bsigned < 5184 ) then
				 output_tmp <= "0000000001000111";
			elsif (Bsigned < 5329 ) then
				 output_tmp <= "0000000001001000";
			elsif (Bsigned < 5476 ) then
				 output_tmp <= "0000000001001001";
			elsif (Bsigned < 5625 ) then
				 output_tmp <= "0000000001001010";
			elsif (Bsigned < 5776 ) then
				 output_tmp <= "0000000001001011";
			elsif (Bsigned < 5929 ) then
				 output_tmp <= "0000000001001100";
			elsif (Bsigned < 6084 ) then
				 output_tmp <= "0000000001001101";
			elsif (Bsigned < 6241 ) then
				 output_tmp <= "0000000001001110";
			elsif (Bsigned < 6400 ) then
				 output_tmp <= "0000000001001111";
			elsif (Bsigned < 6561 ) then
				 output_tmp <= "0000000001010000";
			elsif (Bsigned < 6724 ) then
				 output_tmp <= "0000000001010001";
			elsif (Bsigned < 6889 ) then
				 output_tmp <= "0000000001010010";
			elsif (Bsigned < 7056 ) then
				 output_tmp <= "0000000001010011";
			elsif (Bsigned < 7225 ) then
				 output_tmp <= "0000000001010100";
			elsif (Bsigned < 7396 ) then
				 output_tmp <= "0000000001010101";
			elsif (Bsigned < 7569 ) then
				 output_tmp <= "0000000001010110";
			elsif (Bsigned < 7744 ) then
				 output_tmp <= "0000000001010111";
			elsif (Bsigned < 7921 ) then
				 output_tmp <= "0000000001011000";
			elsif (Bsigned < 8100 ) then
				 output_tmp <= "0000000001011001";
			elsif (Bsigned < 8281 ) then
				 output_tmp <= "0000000001011010";
			elsif (Bsigned < 8464 ) then
				 output_tmp <= "0000000001011011";
			elsif (Bsigned < 8649 ) then
				 output_tmp <= "0000000001011100";
			elsif (Bsigned < 8836 ) then
				 output_tmp <= "0000000001011101";
			elsif (Bsigned < 9025 ) then
				 output_tmp <= "0000000001011110";
			elsif (Bsigned < 9216 ) then
				 output_tmp <= "0000000001011111";
			elsif (Bsigned < 9409 ) then
				 output_tmp <= "0000000001100000";
			elsif (Bsigned < 9604 ) then
				 output_tmp <= "0000000001100001";
			elsif (Bsigned < 9801 ) then
				 output_tmp <= "0000000001100010";
			elsif (Bsigned < 10000 ) then
				 output_tmp <= "0000000001100011";
			elsif (Bsigned < 10201 ) then
				 output_tmp <= "0000000001100100";
			elsif (Bsigned < 10404 ) then
				 output_tmp <= "0000000001100101";
			elsif (Bsigned < 10609 ) then
				 output_tmp <= "0000000001100110";
			elsif (Bsigned < 10816 ) then
				 output_tmp <= "0000000001100111";
			elsif (Bsigned < 11025 ) then
				 output_tmp <= "0000000001101000";
			elsif (Bsigned < 11236 ) then
				 output_tmp <= "0000000001101001";
			elsif (Bsigned < 11449 ) then
				 output_tmp <= "0000000001101010";
			elsif (Bsigned < 11664 ) then
				 output_tmp <= "0000000001101011";
			elsif (Bsigned < 11881 ) then
				 output_tmp <= "0000000001101100";
			elsif (Bsigned < 12100 ) then
				 output_tmp <= "0000000001101101";
			elsif (Bsigned < 12321 ) then
				 output_tmp <= "0000000001101110";
			elsif (Bsigned < 12544 ) then
				 output_tmp <= "0000000001101111";
			elsif (Bsigned < 12769 ) then
				 output_tmp <= "0000000001110000";
			elsif (Bsigned < 12996 ) then
				 output_tmp <= "0000000001110001";
			elsif (Bsigned < 13225 ) then
				 output_tmp <= "0000000001110010";
			elsif (Bsigned < 13456 ) then
				 output_tmp <= "0000000001110011";
			elsif (Bsigned < 13689 ) then
				 output_tmp <= "0000000001110100";
			elsif (Bsigned < 13924 ) then
				 output_tmp <= "0000000001110101";
			elsif (Bsigned < 14161 ) then
				 output_tmp <= "0000000001110110";
			elsif (Bsigned < 14400 ) then
				 output_tmp <= "0000000001110111";
			elsif (Bsigned < 14641 ) then
				 output_tmp <= "0000000001111000";
			elsif (Bsigned < 14884 ) then
				 output_tmp <= "0000000001111001";
			elsif (Bsigned < 15129 ) then
				 output_tmp <= "0000000001111010";
			elsif (Bsigned < 15376 ) then
				 output_tmp <= "0000000001111011";
			elsif (Bsigned < 15625 ) then
				 output_tmp <= "0000000001111100";
			elsif (Bsigned < 15876 ) then
				 output_tmp <= "0000000001111101";
			elsif (Bsigned < 16129 ) then
				 output_tmp <= "0000000001111110";
			elsif (Bsigned < 16384 ) then
				 output_tmp <= "0000000001111111";
			elsif (Bsigned < 16641 ) then
				 output_tmp <= "0000000010000000";
			elsif (Bsigned < 16900 ) then
				 output_tmp <= "0000000010000001";
			elsif (Bsigned < 17161 ) then
				 output_tmp <= "0000000010000010";
			elsif (Bsigned < 17424 ) then
				 output_tmp <= "0000000010000011";
			elsif (Bsigned < 17689 ) then
				 output_tmp <= "0000000010000100";
			elsif (Bsigned < 17956 ) then
				 output_tmp <= "0000000010000101";
			elsif (Bsigned < 18225 ) then
				 output_tmp <= "0000000010000110";
			elsif (Bsigned < 18496 ) then
				 output_tmp <= "0000000010000111";
			elsif (Bsigned < 18769 ) then
				 output_tmp <= "0000000010001000";
			elsif (Bsigned < 19044 ) then
				 output_tmp <= "0000000010001001";
			elsif (Bsigned < 19321 ) then
				 output_tmp <= "0000000010001010";
			elsif (Bsigned < 19600 ) then
				 output_tmp <= "0000000010001011";
			elsif (Bsigned < 19881 ) then
				 output_tmp <= "0000000010001100";
			elsif (Bsigned < 20164 ) then
				 output_tmp <= "0000000010001101";
			elsif (Bsigned < 20449 ) then
				 output_tmp <= "0000000010001110";
			elsif (Bsigned < 20736 ) then
				 output_tmp <= "0000000010001111";
			elsif (Bsigned < 21025 ) then
				 output_tmp <= "0000000010010000";
			elsif (Bsigned < 21316 ) then
				 output_tmp <= "0000000010010001";
			elsif (Bsigned < 21609 ) then
				 output_tmp <= "0000000010010010";
			elsif (Bsigned < 21904 ) then
				 output_tmp <= "0000000010010011";
			elsif (Bsigned < 22201 ) then
				 output_tmp <= "0000000010010100";
			elsif (Bsigned < 22500 ) then
				 output_tmp <= "0000000010010101";
			elsif (Bsigned < 22801 ) then
				 output_tmp <= "0000000010010110";
			elsif (Bsigned < 23104 ) then
				 output_tmp <= "0000000010010111";
			elsif (Bsigned < 23409 ) then
				 output_tmp <= "0000000010011000";
			elsif (Bsigned < 23716 ) then
				 output_tmp <= "0000000010011001";
			elsif (Bsigned < 24025 ) then
				 output_tmp <= "0000000010011010";
			elsif (Bsigned < 24336 ) then
				 output_tmp <= "0000000010011011";
			elsif (Bsigned < 24649 ) then
				 output_tmp <= "0000000010011100";
			elsif (Bsigned < 24964 ) then
				 output_tmp <= "0000000010011101";
			elsif (Bsigned < 25281 ) then
				 output_tmp <= "0000000010011110";
			elsif (Bsigned < 25600 ) then
				 output_tmp <= "0000000010011111";
			elsif (Bsigned < 25921 ) then
				 output_tmp <= "0000000010100000";
			elsif (Bsigned < 26244 ) then
				 output_tmp <= "0000000010100001";
			elsif (Bsigned < 26569 ) then
				 output_tmp <= "0000000010100010";
			elsif (Bsigned < 26896 ) then
				 output_tmp <= "0000000010100011";
			elsif (Bsigned < 27225 ) then
				 output_tmp <= "0000000010100100";
			elsif (Bsigned < 27556 ) then
				 output_tmp <= "0000000010100101";
			elsif (Bsigned < 27889 ) then
				 output_tmp <= "0000000010100110";
			elsif (Bsigned < 28224 ) then
				 output_tmp <= "0000000010100111";
			elsif (Bsigned < 28561 ) then
				 output_tmp <= "0000000010101000";
			elsif (Bsigned < 28900 ) then
				 output_tmp <= "0000000010101001";
			elsif (Bsigned < 29241 ) then
				 output_tmp <= "0000000010101010";
			elsif (Bsigned < 29584 ) then
				 output_tmp <= "0000000010101011";
			elsif (Bsigned < 29929 ) then
				 output_tmp <= "0000000010101100";
			elsif (Bsigned < 30276 ) then
				 output_tmp <= "0000000010101101";
			elsif (Bsigned < 30625 ) then
				 output_tmp <= "0000000010101110";
			elsif (Bsigned < 30976 ) then
				 output_tmp <= "0000000010101111";
			elsif (Bsigned < 31329 ) then
				 output_tmp <= "0000000010110000";
			elsif (Bsigned < 31684 ) then
				 output_tmp <= "0000000010110001";
			elsif (Bsigned < 32041 ) then
				 output_tmp <= "0000000010110010";
			elsif (Bsigned < 32400 ) then
				 output_tmp <= "0000000010110011";
			elsif (Bsigned < 32761 ) then
				 output_tmp <= "0000000010110100";
			end if;
		------------------------


		-- cos
	    elsif (cosB = '1') then
			if (Bsigned = 0) then
				output_tmp <= (others => '0');
			elsif (Bsigned < 26) then
				output_tmp <= "0000000000001001";
			elsif (Bsigned < 37) then
				output_tmp <= "0000000000001000";
			elsif (Bsigned < 46) then
				output_tmp <= "0000000000000111";
			elsif (Bsigned < 54) then
				output_tmp <= "0000000000000110";
			elsif (Bsigned < 61) then
				output_tmp <= "0000000000000101";
			elsif (Bsigned < 67) then
				output_tmp <= "0000000000000100";
			elsif (Bsigned < 73) then
				output_tmp <= "0000000000000011";
			elsif (Bsigned < 79) then
				output_tmp <= "0000000000000010";
			elsif (Bsigned < 85) then
				output_tmp <= "0000000000000001";
			elsif (Bsigned < 96) then
				output_tmp <= "0000000000000000";
			elsif (Bsigned < 102) then
				output_tmp <= "1111111111111111";
--			elsif (Bsigned < 108) then 
			end if;
			----------------------
		
		-- rand (tested)
		elsif (rand = '1') then
		   output_tmp <= randreg;
	    --------------------	   
		                                                                               
	  	-- shift B to right(tested)
		elsif (Bshr = '1') then
			output_tmp <= '0' &  B(15 downto 1);
		--------------------
	
		-- A not B(tested)
		elsif (AnotB = '1') then
			output_tmp <= not B;
		--------------------

		-- B 15 to 0(tested)
		elsif (B15to0 = '1') then
			output_tmp <= B;
		--------------------

		-- B two's complement (tested)
		elsif (Btws = '1') then
			output_tmp <= std_logic_vector(unsigned(not B) + 1);
		--------------------

		-- A multiply B (tested)
		elsif (AmulB = '1') then
			BinInt := to_integer(unsigned(B));
			for i in 1 to BinInt loop
				tmp := std_logic_vector(unsigned(tmp) + unsigned(A));
			end loop;
			output_tmp <= tmp;
		--------------------

		-- A or B (tested)                                                             
		elsif (AorB = '1') then
		  output_tmp <= A or B;
		--------------------
		
		-- shift B to left (tested)
		elsif (Bshl = '1') then 
		  output_tmp <= B(14 downto 0) & '0';
		--------------------
		
		-- compare B and A (tested)
		elsif (AcmpB = '1') then
		  if (A = B) then
		    ALU_Zout <= '1';
		  elsif (Asigned < Bsigned) then
		    ALU_Cout <= '1';
		  end if; 
		---------------------
		
		-- A + B + ALU_Cin with carry out (tested)
		elsif (AaddB = '1') then
		  output_tmp <= sum(15 downto 0);
		  ALU_Cout <= sum(16);
		 
		---------------------
		
		-- subtract (Tested) 
	  	elsif (AsubB = '1') then
			if (ALU_Cin = '1') then
				output_tmp <= std_logic_vector(Asigned - Bsigned - 1);
				if (Asigned < (Bsigned + 1)) then
					ALU_Cout <= '1';
				else
					ALU_Cout <= '0';
				end if;
			else
				output_tmp <= std_logic_vector(Asigned - Bsigned);
				if (Asigned < Bsigned) then
					ALU_Cout <= '1';
				else
					ALU_Cout <= '0';
				end if;
			end if;
		---------------------
		
		-- A xor B (tested)
		elsif (AxorB = '1') then 
		  for i in 0 to 15 loop
		    output_tmp(i) <= A(i) xor B(i);
		  end loop;
		---------------------
		end if;

		if (AcmpB = '0') then
		   ALU_Zout <= not(output_tmp(0) or output_tmp(1) or output_tmp(2) or output_tmp(3) 
				  or output_tmp(7) or output_tmp(6) or output_tmp(5) or output_tmp(4) or 
				  output_tmp(8) or output_tmp(9) or output_tmp(10) or output_tmp(11) or 
				  output_tmp(15) or output_tmp(14) or output_tmp(13) or output_tmp(12));
   		end if;
   		
 		elsif (clk'event and clk = '0') then
 		  tmp := (others => '0');
	end if;

    
  end process;

		  

END dataflow;







----------------------------------------------------------------------

entity Memory , INPR and OUTR is
  port (
    clk   : in std_logic;
    base  : in std_logic_vector(15 downto 0);
    data_in  : in std_logic_vector(15 downto 0);
    read    : in std_logic;
    write  : in std_logic;
    addr  : in std_logic_vector(15 downto 0);
    data_out  : out std_logic_vector(15 downto 0)
  );
end entity Memory;

architecture behavioral of memory is
	type mem is array (0 to blocksize - 1) of std_logic_vector (15 downto 0);
	signal milad: std_logic := '0';
	signal amir: std_logic := 'Z';

  signal data : std_logic_vector(15 downto 0);
 
begin

  data_r <= data;

  process (clk)
  begin
    if rising_edge(clk) then
      if wren = '1' then
        data_r <= data;
      end if;
    end if;
  end process;

  address <= address;

end architecture Behavioral;

#begin with test

begin
	process (clk)
		variable buffermem : mem := (others => (others => '0'));
		variable ad : integer;
		variable init : boolean := true;
	begin
		if init = true then
			-- cwp
			buffermem(0) := "0000000000000110";
			
			-- mil r0, 01011101
			buffermem(1) := "0010010000101100";
			
			-- mil r1, 00000101
			buffermem(2) := "0000000000000000";
			
			-- mul r1 r2, 00000001 ------ nop
			buffermem(3) := "1101011100110101";
			
			-- mih r1, 00000000
			buffermem(4) := "0010000100000001";
			
			-- add r1, r0
			buffermem(5) := "0000000000000101";

			-- some initiation
--			buffermem(0) := "0000000000000000";
			init := false;
		end if;

		databus <= (others => 'Z');

		if  clk'event and clk = '1' then
			ad := to_integer(unsigned(addressbus));
      
			if readmem = '1' then -- Readiing :)
				
				memdataready <= '0';
				if ad >= blocksize then
					databus <= (others => 'Z');
				else
				  milad <= '1';
					databus <= buffermem(ad);
				end if;
			elsif writemem = '1' then -- Writing :)
				memdataready <= '0';
				if ad < blocksize then
					buffermem(ad) := databus;
				end if;

			end if;
			memdataready <= '1';
		end if;
	end process;
end architecture behavioral;


-----------------------------------------------------------------


Entity bus is 
	port(

	--inputs:
		
		sel : in std_logic_vector(2 downto 0);

		ARin : in std_logic_vector(15 downto 0);	--1

		PCin : in std_logic_vector(15 downto 0);	--2

		DRin : in std_logic_vector(15 downto 0);	--3

		ACin : in std_logic_vector(15 downto 0);	--4

		IRin : in std_logic_vector(15 downto 0);	--5

		TRin : in std_logic_vector(15 downto 0);	--6

		Min : in std_logic_vector(15 downto 0);		--7

	--output :

		output : out std_logic_vectorI(15 downto 0);

	);

End bus;

Architecture behavioral of bus is

begin

	with  sel  select
		output <= 	"0000000000000000" when “000” ,
					ARin when “001” ,
					PCin when “010” ,
					DRin when “011” ,
					ACin when “100” ,
					IRin when “101” ,
					TRin when “110” ,
					Min when “111” ,

end behavioral;

--------------------------------------------------------------------------
for sc (select counter)

entity sc is	 
	port (
	--inputs:
		clk : in std_logic;
		INR : in std_logic;
		CLR : in std_logic;
	--outputs:
		count : out std_logic_vector(3 downto 0);
end sc;

architecture counter of sc is	

begin
	process (clk)
	variable cnt :integer range 0 to 15;
	begin 
		if ( (not(clk)) and INR) then 
			cnt:= cnt+1;
		end if;
		if(to_unsigned(cnt,5)="10000")then
			cnt:=0;
		end if;
		count<=to_unsigned(cnt,count'length);
	end process;

end counter;




------------------------------------------------------------------------------

PIADEH SAZI  BEGIN and test

FOR Register

begin

  process (clk)
  begin
    if clk'event and clk = '1' then
      if load = '1' then
        internal_data <= data_in;
      elsif increase = '1' then
        internal_data <= internal_data + 1;
      elsif clear = '1' then
        internal_data <= (others => '0');
      end if;
    end if;
  end process;

  data_out <= internal_data;

end architecture Behavioral;

-------

FOR ALU 

begin

  process (a, b, op)
  begin
    sum <= (others => '0');
    borrow <= '0';

    case op is
      when "0000" => -- ADD
        sum <= a + b;
        borrow <= '0';
      when "0001" => -- SUBTRACT
        if (a >= b) then
          sum <= a - b;
          borrow <= '0';
        else
          sum <= (others => '1');
          borrow <= '1';
        end if;
      when "1000" => -- AND
        sum <= a and b;
      when "1001" => -- OR
        sum <= a or b;
      when "1010" => -- NOT
        sum <= not a;
      when "1011" => -- XOR
        sum <= a xor b;
      when "1100" => -- SHIFT LEFT
        sum <= a << 1;
      when "1101" => -- SHIFT RIGHT
        sum <= a >> 1;
      when others =>
        -- Do nothing
    end case;

    if (carryin = '1' and sum(15) = '1') then
      overflow <= '1';
    else
      overflow <= '0';
    end if;
  end process;

  carryout <= sum(15);

  result <= sum;

end architecture Behavioral;

------------

MEMORY BALA PIADEH SHOD

-------------
FOR BUS

begin

  data_r <= data;

  process (clk)
  
  begin
    if rising_edge(clk) then
      if wren = '1' then
        data_r <= data;
      end if;
    end if;
  end process;

  address <= address;

end architecture Behavioral;


#wren = Write Enable

#--------------------------------------------------------------------------------------------

#main for table in memary



Entity main is 
port(

	--inputs:
	I : in std_logic; --comes from the last bit of IR

	D : in  std_logic_vector(7 downto 0); --the decoded instruction that comes from the dec3x8

	IR : in std_logic_vector(11 downto 0); -- the operand bits from IR

	T : in std_logic_vector(15 downto 0 ); -- time signal comming from the dec4x16 determinig the processing step we're going to perform

	clock : in std_logic; --clk from clock generator

	IO : in std_logic_vector(1 downto 0); -- IO[1]=fgi , IO[0]=fgo






	--outputs:

	
	INR,LD,CLR	: out std_logic_vector(6 downto 0); -- registers inputs --- ORDER : 6>Oreg, 5>IR,  4>AR,  3>PC,  2>TR,  1>AC,  0>DR

	s : out std_logic_vector(2 downto 0 ); -- bus line selector

	SCINR,SCCLR : out std_logic; -- to control the sequence counter

	MRead,MWrite : out std_logic; -- reading and writing from/to the memory

	FGICLR,FGOCLR,FGIINR,FGOINR : out std_logic ; --input/output control

	ALU : out std_logic_vector(3 downto 0);

	ECLR,EINR : out std_logic;

	
	--ALU OPS:
	--	"000"> DO NOTHING
	--	"001"> AND
	--	"010"> ADD
	--	"011"> DR -> AC INPUT
	--	"100"> CMA
	--	"101"> CME
	--	"110">
	--	"111">
	--END OF ALU OPS


);

End main;

Architecture behavioral of cumain is

	signal 
	IRINR,IRLD,IRCLR,
	OregINR,OregLD,OregCLR,
	ARIRN,ARLD,ARCLR,
	PCINR,PCLD,PCCLR,
	TRINR,TRLD,TRCLR,
	ACINR,ACLD,ACCLR,
	DRINR,DRLD,IRCLR : std_logic;

	signal
	IEN,R : std_logic;

	signal
	halt: std_logic; --halt the computer if set to 0
	
	Begin

		INR[0]<=IRINR;LD[0]<=IRLD;CLR[0]<=IRCLR;
		INR[1]<=ARINR;LD[1]<=ARLD;CLR[1]<=ARCLR;
		INR[2]<=PCINR;LD[2]<=PCLD;CLR[2]<=PCCLR;
		INR[3]<=TRINR;LD[3]<=TRLD;CLR[3]<=TRCLR;
		INR[4]<=ACINR;LD[4]<=ACLD;CLR[4]<=ACCLR;
		INR[5]<=DRINR;LD[5]<=DRLD;CLR[5]<=DRCLR;
		INR[6]<=OregINR;LD[6]<=OregLD;CLR[6]<=OregCLR;

		time:process(T,clock) is

			begin

			variable p,x: std_logic;
			p:= D[7] and IR[15] and T[3]; 				 --- if true then an i/o op is requested
			x:= D[7] and (not(IR[15])) and T[3];		 --- if true then a register op is requested



-------------------------T[0] to T[2]-------------------------
--------------------------------------------------------------Fetch and Decode

			if(T[0] and (not (R))) then
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					FGIINR<='0';
					FGICLR<='1';
					FGOINR<='0';
					FGOCLR<='1';
					EINR<='0';
					ECLR<='1';
					IEN<='0';
					R<='0';
					--ALU<="000";
					s<="010"
					SCINR<='1';
				else	
					ARLD<='1';

				end if;


			elsif(T[1] and (not (R))) then
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					PCINR<='1';
					MRead<='1';
					s<="111";
					SCINR<='1';
				else
					IRLD<='1';
				end if;


			elsif(T[2] and (not (R))) then
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					s<="101";
					SCINR<='1';
				else
					ARLD<='1';
				end if;

------------------------------------------------------------------Interrupt
			elsif(T[0] and T[1] and T[2] and IEN and (IO[1] or IO[0])) then
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					R<='1';
				else
				end if;

			elsif(T[0] and R) then
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					ARCLR<='1';
					s<="010";
				else
					TRLD<='1';
					ARCLR<='1';
				end if;

			elsif(T[1] and R) then
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					PCCLR<='1';
					s<="110";
					
				else
					MWrite<='1';
				end if;

			elsif(T[2] and R) then
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					IEN<='0';
					PCINR<='1';
					SCCLR<='1';
					R<='0';
				else
					ARLD<='1';
				end if;
-----------------------------T[3]----------------------------------
-----------------------------I/O OPS-------------------------------     

	--ZERO ---- NULL
			elsif(p) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';

				else
				
				end if;


	--INP
			elsif(p and IR[11]) then --requires alu
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					FGI<='0';
					ALU<="";
				else
				
				end if;
	--OUT
			elsif(p and IR[10]) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					s<="100";
				else
					OregLD<='1';
				end if;
	--SKI
			elsif(p and IR[9] and IO[1]) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					PCINR<='1';

				else
				
				end if;
	--SKO
			elsif(p and IR[8] and IO[0]) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					PCINR<='1';

				else
				
				end if;

	--ION
			elsif(p and IR[7]) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					IEN<='1';
				else
				
				end if;

	--IOF
			elsif(p and IR[6]) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					IEN<='0';

				else
				
				end if;

------------------------------------------------------------------END OF I/O

-----------------------------REGISTER OPS---------------------------------

	--CLA

			elsif(x and IR[11]) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					ACCLR<='1';
				else
				
				end if;

	--CLE
			elsif(x and IR[10]) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					ECLR<='1';
				else

				end if;

	--CMA			
			elsif(x and IR[9]) then -- requires alu
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					ALU<="100"

				else
					ACLD<='1';
				end if;
	--CME			
			elsif(x and IR[8]) then -- requires alu
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					ALU<="101";
				else
				end if;
	--CIR			
			elsif(x and IR[7]) then -- requires alu
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
				else
				end if;
	--CIL			
			elsif(x and IR[6]) then  -- requires alu
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
				else
				end if;
	--INC			
			elsif(x and IR[5]) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					ACINR<='1';
				else
				end if;
	--SPA			
			elsif(x and IR[4] and (not(AC[15])) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					PCINR<='1';
				else

				end if;
	--SNA			
			elsif(x and IR[3] and AC[15]) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					PCINR<='1';
				else
				end if;
	--SZA			
			elsif(x and IR[2] and AC="0000000000000000") then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					PCINR<='1';
				else
				end if;
	--SZE
			elsif(x and IR[1] and (not(E))) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					PCINR<='1';
				else
				end if;
	--HLT			
			elsif(x and IR[0]) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					halt<='0';
				else
				end if;


	--NOTHING		
			elsif(x) then 
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';

				else
				
				end if;

------------------------------------------------------END OF REGISTER OPS

------------------------------------------------------END OF T[3]

------------------------T[4] to T[6]---------------------


-------------------------MEMORY OPS----------------------

--AND:
			elsif(T[4] and D[0] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					MRead<='1';
					s<="111";

				else
					DRLD<='1';
				end if;


			elsif(T[5] and D[0]and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					ALU<="001";
				else
					ACLD<='1';
				end if;	


--ADD:
			elsif(T[4] and D[1] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					MRead<='1';
					s<="111";

				else
					DRLD<='1';
				end if;


			elsif(T[5] and D[1] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					ALU<="010";

				else
					ACLD<='1';
				end if;




--LDA:
			elsif(T[4] and D[2] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					MRead<='1';
					s<="111";


				else
					DRLD<='1';
				end if;


			elsif(T[5] and D[2] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					ALU<="011"; --PASS THE DR TO AC
					ACLD<='1';
				else
					NULL;
				end if;


--STA:
			elsif(T[4] and D[3] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					s<="100"
					ARLD<='1';

				else
					MWrite<='1';
				end if;
				

--BUN:
			elsif(T[4] and D[4] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					s<="001";
					PCLD<='1';
				else
					PCLD<='1';
				end if;

--BSA:

			elsif(T[4] and D[5] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					s<="010";


				else
					MWrite<='1';
					ARINR<='1';      -- COLLISION
				
				end if;


			elsif(T[5] and D[5] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';					
					s<="001";

				else
					PCLD<="1";
				end if;



--ISZ:
			elsif(T[4] and D[6] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					MRead<='1';
					s<="001";

				else
					DRLD<='1';

				end if;


			elsif(T[5] and D[6] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';										
					DRINR<='1';
				else
					null;
				end if;


			elsif(T[4] and D[6] and halt)
				if(clock='1') then
					LD<="0000000";
					INR<="0000000";
					CLR<="0000000";
					SCCLR<='1';
					s<="011";
					MWrite<='1';

					if(DR="000000000000") then
						PCINR<='1';
					end if;
				else
					null;
				end if;


			END IF;	
	end process time;

end behavioral;
