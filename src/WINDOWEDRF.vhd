library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.math_real.all;
use ieee.numeric_std.all;
use IEEE.std_logic_unsigned.all;
use WORK.all;

entity register_file is
generic (
    NBIT: integer:= 64;
    NREG: integer:= 16;
    NADDR: integer := 4;
    M: integer := 4;     --registers for global
    N: integer := 4;     --register in IN, OUT, LOCAL.
    F: integer := 4 );   --number of windows
    
port ( CLK: 	IN std_logic;
     RESET: 	IN std_logic;
	 ENABLE: 	IN std_logic;
	 RD1: 		IN std_logic;
	 RD2: 		IN std_logic;
	 WR: 		IN std_logic;
	 ADD_WR: 	IN std_logic_vector(NADDR-1 downto 0);
	 ADD_RD1: 	IN std_logic_vector(NADDR-1 downto 0);
	 ADD_RD2: 	IN std_logic_vector(NADDR-1 downto 0);
	 DATAIN: 	IN std_logic_vector(NBIT-1 downto 0);
	 SUBCALL:   IN std_logic; 
	 SUBRETURN: IN std_logic;
	 BUSIN:     IN std_logic_vector (NBIT-1 downto 0);
		
	 BUSOUT:    OUT std_logic_vector (NBIT-1 downto 0);
     OUT1: 	    OUT std_logic_vector(NBIT-1 downto 0);
	 OUT2: 		OUT std_logic_vector(NBIT-1 downto 0));
end register_file;

architecture A of register_file is

   --Physical Register is of size (F*2N)+M Registers (+ctrl registers). 
   constant numberOfReg: integer := F*(2*N);
   -- with the dafault values these are 4*4*2=32 registers for IN/LOCAL/OUT (windows), 4 (GLOBALS) addressed separately. 
   constant PhAddrSize: integer := integer(ceil(log2(real(numberOfReg)))); 
   --32 Registers means 6 bits to address them (PhAddrSize=6). 
     
    subtype REG_ADDR is natural range 0 to numberOfReg-1; -- using natural type
	type REG_ARRAY is array(REG_ADDR) of std_logic_vector(NBIT-1 downto 0);
	
    subtype REG_ADDR_GLOBAL is natural range 0 to M-1; -- using natural type
	type REG_ARRAY_GLOBAL is array(REG_ADDR_GLOBAL) of std_logic_vector(NBIT-1 downto 0);	
	 
	signal CWP,CWP_nxt,SWP,SWP_nxt,N_unsigned: unsigned (PhAddrSize-1 downto 0); --6 bits 
	--the pointer are of the size needed to address all IN/OUT/LOCALS in the physical register. 
	--signal CANSAVE,CANRESTORE: std_logic;
	
	type state_type is (init,setup,idle,spill,fill);
	signal state,state_nxt: state_type; 

	signal REGISTERS,REG_nxt : REG_ARRAY;
    signal GLOBAL_REG,GLOBAL_REG_nxt: REG_ARRAY_GLOBAL; 
    
    signal BUSOUTtmp,BUSOUT_nxt,OUT1tmp,OUT1_nxt,OUT2tmp,OUT2_nxt: std_logic_vector (NBIT-1 downto 0);	

    -- i need a counter to count the clock cycles (iterations) in spill and fill.
    signal cnt,cnt_nxt: unsigned (PhAddrSize-1 downto 0); 

begin 

process (clk,reset)
begin 
     if (rising_edge(clk))then 
       if (reset='1') then state<=init;  
       else state<=state_nxt; 
            REGISTERS<=reg_nxt;
            cwp<=cwp_nxt; 
            swp<=swp_nxt; 
            global_reg<=global_reg_nxt; 
            BUSOUTtmp<=BUSOUT_nxt; 
            OUT1tmp<=OUT1_nxt;
            OUT2tmp<=OUT2_nxt;
            CNT <= CNT_nxt;     
       end if; 
     end if; 
end process; 

process (RESET,ENABLE,RD1,RD2,WR,ADD_WR,ADD_RD1,ADD_RD2,DATAIN,SUBCALL,SUBRETURN,BUSIN,
         REGISTERS,GLOBAL_REG,state,CWP,SWP,cnt) 
variable CANSAVE,CANRESTORE: std_logic; 
begin
REG_nxt<=REGISTERS; 
state_nxt<=state;  
cwp_nxt<=cwp; 
swp_nxt<=swp; 
global_reg_nxt<=global_reg; 
cnt_nxt<=cnt; 

case state is 
when init => for I in 0 to (F*(2*N))-1 loop
              REG_nxt(I)<=(others=>'0');
             end loop;  
             for I in 0 to M-1 loop
              GLOBAL_REG_nxt(I)<=(others=>'0');
             end loop; 
             CWP_nxt<=(others=>'0');
             SWP_nxt<=(others=>'0');
             --cansave <= '1'; 
             --canrestore <= '0';
             cansave:='1'; 
             canrestore:='1';
             state_nxt<=idle; 
             cnt_nxt<=(others=>'0');
             

when idle => if (enable='1') then 
             --perform R/W operations 
             --rd1
             if(RD1 = '1')then
                if (ADD_RD1<(3*N)) then
				OUT1_nxt <= REGISTERS(to_integer(unsigned(ADD_RD1)+CWP));
				else OUT1_nxt <= GLOBAL_REG (to_integer (unsigned(ADD_RD1) - 3*N));
				end if; 
			  end if;
			  --rd2
			  if(RD2 = '1')then
			   if (ADD_RD2<(3*N)) then
				OUT2_nxt <= REGISTERS(to_integer(unsigned(ADD_RD2)+CWP));
			    else OUT2_nxt <= GLOBAL_REG (to_integer (unsigned(ADD_RD2) - 3*N));
				end if; 
				end if;
				--wr
			if WR = '1' then
			if (ADD_WR<(3*N)) then
				REG_nxt(to_integer(unsigned(ADD_WR)+CWP)) <= DATAIN;
			else GLOBAL_REG_nxt (to_integer (unsigned(ADD_WR) - 3*N)) <= DATAIN; 	
			end if;
			end if; 
			
			end if; 
			
			--compute canrestore: is 0 when SWP=CWP.
			if (CWP=SWP) then canrestore:='1'; 
			else canrestore:='0'; 
			end if;

			--compute cansave: is 0 only if SWP is 4N addresses ahead of CWP.  
			--if ((CWP+(N_unsigned(PhAddrSize-1 downto 1)&"00"))=SWP) then 
			if ((CWP+(4*N))=SWP) then 
			    CANSAVE:= '0'; 
			   else cansave := '1';  
			end if;
						
			--subcall. if there is space (cansave='1') move CWP by 2N addresses forward. else spill
			if (subcall='1') then
			 if (cansave='1') then
			   CWP_nxt <= CWP+2*N; 
			 else state_nxt<=spill; 
			 end if;
			end if; 

			--compute cansave: is 0 only if SWP is 4N addresses ahead of CWP.  
			--if ((CWP+(N_unsigned(PhAddrSize-1 downto 1)&"00"))=SWP) then 
			if ((CWP+(2*N))=SWP) then 
			    CANSAVE:= '0'; 
			   else cansave := '1';  
			end if; 
						
		    --return. if there is a window to return to move CWP by 2N addresses backward. else fill
			if (subreturn='1') then
			 if (canrestore='0') then
			   CWP_nxt <= CWP-(2*N);
			 else state_nxt<=fill; 
			 end if;
			end if;  
			
when spill => if ((CNT) < 2*N) then 
                   BUSOUT_nxt <= REGISTERS (to_integer(CWP+CNT+(4*N)));                    
                   REG_nxt (to_integer(CWP+CNT+(4*N))) <= (others=>'0'); 
                   CNT_nxt<=CNT+1; 
                   state_nxt<=spill;
              else state_nxt<=idle; 
                   CWP_nxt <= CWP + (2*N);
                   SWP_nxt <= SWP + (2*N);
                   CNT_nxt <= (others=>'0');
              end if;

when fill => if (CNT < 2*N) then 
                   REG_nxt (to_integer(CWP-CNT-1)) <= BUSIN; 
                   CNT_nxt<=CNT+1; 
              else state_nxt<=idle; 
                   CWP_nxt <= CWP - (2*N);
                   SWP_nxt <= SWP - (2*N);
                   CNT_nxt <= (others=>'0');
              end if; 
when others => state_nxt<=init;  
end case; 
end process; 

OUT1<=OUT1tmp; 
OUT2<=OUT2tmp; 
BUSOUT<=BUSOUTtmp; 

end A;


--configuration CFG_RF_BEH of register_file is
--  for A
--  end for;
--end configuration;

