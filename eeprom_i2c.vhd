

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.function_conversion.all;


entity eeprom_i2c is
Generic ( fclk : positive := 50_000;           --system clock, 50MHz
            data_rate : positive := 100;        --standard mode- 100kbps
            write_time : positive := 5           --device write time,5ms(refer to datasheet)
            ); 
  Port ( clk, rst : in std_logic;
          data : in integer range 0 to 15;
          address: in std_logic_vector(3 downto 0);
          rd, wr : in std_logic;
          ssd : out std_logic_vector(6 downto 0);
          ack_error: out std_logic;
          --i2c signal
          scl : out std_logic;
          sda : inout std_logic 
           );
end eeprom_i2c;

architecture Behavioral of eeprom_i2c is
--CONSTANT AND SIGNAL
constant divider: integer := (fclk/8)/data_rate;             --
constant delay : integer := write_time * data_rate;
CONSTANT dev_addr_write: STD_LOGIC_VECTOR(7 DOWNTO 0):= "10100000";
CONSTANT dev_addr_read: STD_LOGIC_VECTOR(7 DOWNTO 0) := "10100001";
SIGNAL aux_clk, bus_clk, data_clk: STD_LOGIC;                     -- aux is 4x from data rate, bus clk for scl, data clk for sda
SIGNAL data_in, data_out: STD_LOGIC_VECTOR(7 DOWNTO 0);         
SIGNAL wr_flag, rd_flag: STD_LOGIC;
SIGNAL mem_addr: STD_LOGIC_VECTOR(7 DOWNTO 0);
SIGNAL ack: STD_LOGIC_VECTOR(2 DOWNTO 0); 
SIGNAL timer: integer range 0 to delay;
shared variable i : natural range 0 to delay;

--state machine signal
type state is (idle, start_wr, start_rd, dev_addr_wr, dev_addr_rd,
 wr_addr, wr_data, rd_data, stop, no_ack, ack1, ack2, ack3, ack4);
 signal pr_state,nxt_state: state;
 
begin

--general signals

data_out <= '0' & integer_to_ssd(data);
mem_addr <= "0000" & address;
ssd  <= data_in (6 downto 0);
ack_error <= ack(0) or ack(1) or ack(2);


--auxiliary clk
--freq=4*data_rate=400kHz
    PROCESS(clk)
        variable count: integer range 0 to divider;
        begin
            if (clk='1' and clk 'event) then
                count := count + 1;
            if (count=divider) then 
                aux_clk <= not aux_clk;
                count := 0;
             end if;
             end if;
     end process;
     
  -- data clk 

     process(aux_clk)
            variable count : integer range 0 to 3;
            begin 
                if (aux_clk ='1' and aux_clk 'event) then
                    count := count + 1;
                    if (count = 0 ) then
                    bus_clk <= '0';
                    elsif (count = 1) then 
                    data_clk <= '1';
                    elsif  (count = 2) then 
                    bus_clk <= '1';
                    else 
                    data_clk <= '0';
                 end if;
                 end if;
      end process;
      
      --lower section of fsm
      
      process (data_clk, rst)
      begin 
            if (rst='1') then
                pr_state <= idle;
                i := 0;
   --ENTER DATA FOR I2C BUS
            elsif (data_clk ='1' and data_clk 'event) then
                if (i = timer - 1) then 
                    pr_state <= nxt_state;
                    i:= 0;
                   else 
                        i := i +1;
                 end if;
                 
              elsif ( data_clk = '0' and data_clk 'event) then         --negative edge, from high to low, start the operation
      --store write/read flag
                    if (pr_state = idle) then 
                        wr_flag <= wr;
                        rd_flag <= rd;
                    end if;
                    
     --store ack signal during writing
     
                if ( pr_state = ack1) then
                    ack(0) <= sda;
                elsif (pr_state = ack2) then
                    ack(1) <= sda;
                elsif (pr_state = ack3) then
                    ack(2) <= sda;
                    
                end if; 
                    
    --store data read from memory
    
        if ( pr_state = rd_data) then
            data_in (7-i) <= sda;
         end if;
       end if;   
      end process;
      
  --upper section of fsm 
        
      process( pr_state, bus_clk, data_clk, wr_flag, rd_flag, data_out, mem_addr, sda)
      begin 
        Case pr_state is 
        when idle => 
                    scl <= '1';
                    sda <= '1';
                    timer <= delay;
                    if (wr_flag = '1' or rd_flag='1') then
                        nxt_state <= start_wr;
                     else 
                        nxt_state <= idle;
                     end if;
        when start_wr=> 
                        SCL <= '1';
                        sda <= data_clk;
                        timer <= 1;
                         nxt_state <= dev_addr_wr;
         WHEN dev_addr_wr =>
                         scl <= bus_clk;
                         sda <= dev_addr_write(7-i);
                          timer <= 8;
                         nxt_state <= ack1;
         WHEN ack1 =>
                        scl <= bus_clk;
                        sda <= 'Z';
                        timer <= 1;
                        nxt_state <= wr_addr;
         WHEN wr_addr =>
                        scl <= bus_clk;
                        sda <= mem_addr(7-i);
                        timer <= 8;
                        nxt_state <= ack2;
         WHEN ack2 =>
                        scl <= bus_clk;
                        sda <= 'Z';
                        timer <= 1;
                     IF (wr_flag='1') THEN
                        nxt_state <= wr_data;
                     ELSE
                        nxt_state <= start_rd;
                        END IF;
         WHEN wr_data =>
                    scl <= bus_clk;
                    sda <= data_out(7-i);
                    timer <= 8;
                    nxt_state <= ack3;
         WHEN ack3 =>
                    scl <= bus_clk;
                    sda <= 'Z';
                    timer <= 1;
                     nxt_state <= stop;
         WHEN start_rd =>
                    scl <= '1';
                    sda <= data_clk;
                    timer <= 1;
                    nxt_state <= dev_addr_rd;
         WHEN dev_addr_rd =>
                    scl <= bus_clk;
                    sda <= dev_addr_read(7-i);
                     timer <= 8;
                     nxt_state <= ack4;
         WHEN ack4 =>
                    scl <= bus_clk;
                     sda <= 'Z';
                    timer <= 1;
                    nxt_state <= rd_data;
         WHEN rd_data =>
                    scl <= bus_clk;
                    sda <= 'Z';
                    timer <= 8;
                    nxt_state <= no_ack;
         WHEN no_ack =>
                    scl <= bus_clk;
                    sda <= '1';
                    timer <= 1;
                     nxt_state <= stop;
         WHEN stop =>
                    scl <= '1';
                    sda <= NOT data_clk;
                     timer <= 1;
                     nxt_state <= idle;
         END CASE;
     END PROCESS;
       

end Behavioral;

