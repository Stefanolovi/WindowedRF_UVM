// Code your testbench here
// or browse Examples

`include "uvm_macros.svh"

package RF_pkg; 
import uvm_pkg::*;

localparam int NBIT = 64;
localparam int NREG = 16;
localparam int NADDR = 4;
localparam int M = 4;
localparam int N = 4;
localparam int F = 4;
localparam int num_cycles = 500;

  class sqnc_item extends uvm_sequence_item;

    `uvm_object_utils(sqnc_item)
    bit RESET; 
    rand bit RD1;
    rand bit RD2;
    rand bit WR;
    rand logic [NADDR-1:0] ADD_WR;
    rand logic [NADDR-1:0] ADD_RD1;
    rand logic [NADDR-1:0] ADD_RD2;
    rand logic [NBIT-1:0] DATAIN;
    bit SUBCALL;
    bit SUBRETURN;
    rand logic [NBIT-1:0] BUSIN;
    logic [NBIT-1:0] BUSOUT;
    logic [NBIT-1:0] OUT1;
    logic [NBIT-1:0] OUT2;
    
    function new (string name = "");
      super.new(name);
    endfunction
    
    function string convert2string;
      return $sformatf("RESET = %b | RD1 = %b | RD2 = %b | WR = %b | ADD_WR = %h | ADD_RD1 = %h | ADD_RD2 = %h | \n SUBCALL = %b | SUBRETURN = %b | DATAIN = %h | BUSIN = %h | \n BUSOUT = %h | OUT1 = %h | OUT2 = %h |", 
      RESET, RD1, RD2, WR, ADD_WR, ADD_RD1, ADD_RD2, SUBCALL, SUBRETURN, DATAIN, BUSIN, BUSOUT, OUT1, OUT2 );
    endfunction
  endclass: sqnc_item


class RF_sequence extends uvm_sequence #(sqnc_item); 

  `uvm_object_utils(RF_sequence)

  function new (string name = ""); 
    super.new (name); 
  endfunction

  task body; 
    if (starting_phase != null)
    starting_phase.raise_objection(this); 
    repeat(num_cycles)
    // create the object, start it, randomize it, finish it. 
      begin
        req = sqnc_item::type_id::create("req");
        start_item(req);
        if( !req.randomize() )
          `uvm_error("", "Randomize failed")
        finish_item(req);
        `uvm_info ("SEQUENCE", $sformatf("SEQUENCE ITEM GENERATED! | %S", req.convert2string()), UVM_HIGH)
      end
    if (starting_phase != null)
    starting_phase.drop_objection(this);
  endtask 
endclass: RF_sequence


//driver writes on the virtual interface data generated from the squence.
class RF_driver extends uvm_driver #(sqnc_item); 
 `uvm_component_utils(RF_driver)

    virtual RF_if dut_vi; 

    function new (string name, uvm_component parent); 
        super.new (name, parent); 
    endfunction

function void build_phase (uvm_phase phase); 
      // Get interface reference from config database
      if( !uvm_config_db #(virtual RF_if)::get(this, "", "RF_if", dut_vi) )
        `uvm_error("", "uvm_config_db::get failed")
endfunction

task run_phase (uvm_phase phase); 
//get sqnc_item, drive it to IF at posedge, close it
forever
      begin
        seq_item_port.get_next_item(req);
        @(posedge dut_vi.CLK);
        `uvm_info ("DRIVER", $sformatf("DRIVING THE GENERATED PACKET"), UVM_HIGH)
        dut_vi.RD1  = req.RD1;
        dut_vi.RD2 = req.RD2;
        dut_vi.WR = req.WR; 
        dut_vi.ADD_RD1 = req.ADD_RD1; 
        dut_vi.ADD_RD2 = req.ADD_RD2; 
        dut_vi.ADD_WR = req.ADD_WR; 
        dut_vi.DATAIN = req.DATAIN;  
        dut_vi.BUSIN = req.BUSIN; 
        seq_item_port.item_done();
      end
endtask 
endclass: RF_driver


// use basic uvm_sequencer
class RF_sequencer extends uvm_sequencer #(sqnc_item); 
  `uvm_component_utils(RF_sequencer)

function new (string name, uvm_component parent); 
super.new (name, parent); 
endfunction
endclass: RF_sequencer


//monitor reads from the virtual interface and stores data in a local sqnc_item, then writes it on a port
class RF_monitor extends uvm_monitor; 
  
  `uvm_component_utils(RF_monitor)
  virtual RF_if dut_vi; 
  //semaphore sema4; 

  function new (string name, uvm_component parent); 
    super.new(name,parent);
  endfunction
  
  // instance analysis port
  uvm_analysis_port #(sqnc_item) mon_analysis_port; 

  function void build_phase (uvm_phase phase); 
      `uvm_info (get_type_name(), "START BUILDING PHASE", UVM_HIGH)

    //get if
    if( !uvm_config_db #(virtual RF_if)::get(this, "", "RF_if", dut_vi) )
    `uvm_error("", "uvm_config_db::get failed")
    //sema4 = new(1);
    //call analysys_port constructor
    mon_analysis_port = new ("mon_analaysis_port", this); 
  endfunction

  task run_phase (uvm_phase phase);
    //create an transaction object, assign the if value to it, write it on the port.
    sqnc_item data; 
    data = sqnc_item::type_id::create("data"); 
    //read at each positive edge of the clock
    forever begin 
      @(negedge dut_vi.CLK);
      data.RESET = dut_vi.RESET; 
      data.RD1 = dut_vi.RD1; 
      data.RD2 = dut_vi.RD2; 
      data.WR = dut_vi.WR; 
      data.ADD_WR = dut_vi.ADD_WR;  
      data.ADD_RD1 = dut_vi.ADD_RD1; 
      data.ADD_RD2 = dut_vi.ADD_RD2; 
      data.DATAIN = dut_vi.DATAIN; 
      data.SUBCALL = dut_vi.SUBCALL; 
      data.SUBRETURN = dut_vi.SUBRETURN; 
      data.BUSIN = dut_vi.BUSIN; 
      data.OUT1 = dut_vi.OUT1; 
      data.OUT2 = dut_vi.OUT2; 
      data.BUSOUT = dut_vi.BUSOUT; 
      `uvm_info(get_type_name(), $sformatf("\n TRANSACTION RECEIVED: %s \n", data.convert2string()), UVM_MEDIUM)
    //pass the data to subscribers thorugh analysis port  
    mon_analysis_port.write(data);
    end     
  endtask
endclass

//agent puts together (create and connect) driver, sequencer and monitor.
class RF_agent extends uvm_agent; 

  `uvm_component_utils(RF_agent)
  function new (string name, uvm_component parent); 
    super.new (name, parent); 
  endfunction

  RF_driver driver; 
  RF_sequencer sequencer; 
  RF_monitor monitor; 

  function void build_phase (uvm_phase phase); 
      `uvm_info (get_type_name(), "START BUILDING PHASE", UVM_HIGH)

    driver = RF_driver::type_id::create("driver", this);
    sequencer = RF_sequencer::type_id::create ("sequencer", this);
    monitor = RF_monitor::type_id::create("monitor", this); 
  endfunction

  virtual function void connect_phase (uvm_phase phase);
      `uvm_info (get_type_name(), "START CONNECT PHASE", UVM_HIGH)
    driver.seq_item_port.connect(sequencer.seq_item_export);
  endfunction
endclass


class RF_scoreboard extends uvm_scoreboard; 

  `uvm_component_utils(RF_scoreboard)

  logic [NBIT-1:0] regs [(N*F*2)];
  logic [NBIT-1:0] regs_nxt [(N*F*2)]; 
  logic [NBIT-1:0] glob [(M)];
  logic [NBIT-1:0] glob_nxt [(M)]; 
  logic [NBIT-1:0] read1 = 'h0; 
  logic [NBIT-1:0] read1_nxt = 'h0;  
  logic [NBIT-1:0] read2 = 'h0;
  logic [NBIT-1:0] read2_nxt = 'h0;  
  logic [NBIT-1:0] bus_out;
  logic [NBIT-1:0] bus_out_nxt;
  logic [$clog2(F*2*N)-1:0] cwp = 0; 
  logic [$clog2(F*2*N)-1:0] wp_tmp = 0;
  logic [$clog2(F*2*N)-1:0] cwp_nxt = 0;
  logic [$clog2(F*2*N)-1:0] swp = 0; 
  logic [$clog2(F*2*N)-1:0] swp_nxt = 0;
  logic cansave, canrestore, spill, fill; 
  int cnt = 0; 
  logic delay_1_cc; 

  function new (string name = "scoreboard", uvm_component parent = null);
    super.new(name, parent); 
  endfunction

  //get data from analysis_port, compute expected results, compare real results with expected. 
  uvm_analysis_imp #(sqnc_item, RF_scoreboard) ap_imp; 

  virtual function void build_phase (uvm_phase phase); 
      `uvm_info (get_type_name(), "START BUILDING PHASE", UVM_HIGH)
    super.build_phase(phase);
    ap_imp = new("ap_imp", this); 
  endfunction


  function void ref_model (sqnc_item t); 

     //similarly to the dut, this ref_model is synchronous: each negedge i compute the next value for the registers and other variables,
     //but I only update the regs variable at the next negedge. 
     regs = regs_nxt; 
     glob = glob_nxt;
     read1 = read1_nxt;  
     read2 = read2_nxt;  
     cwp = cwp_nxt; 
     swp = swp_nxt; 
     bus_out = bus_out_nxt;

    //reset procedure is synchronous and prioritary to other operations 
    if (t.RESET == 1) begin  
      cwp_nxt = 0; 
      swp_nxt = 0; 
      read1_nxt = 'h0; 
      read2_nxt = 'h0;
      cansave = 1; 
      canrestore = 0;  
      spill = 0; 
      fill = 0; 
      delay_1_cc = 1; 
      for (int i = 0; i < (N*F*2); i++) begin 
        regs_nxt [i] = 'h0;  
      end
      for (int i = 0; i < M; i++) begin 
        glob_nxt [i] = 'h0;  
      end
    end else if (delay_1_cc == 1) delay_1_cc = 0; 
            else  begin 

      //spill procedure
      if (spill == 1) begin
        if (cnt < 2*N) begin 
          wp_tmp = cwp + cnt + 4*N;
          bus_out_nxt = regs [wp_tmp]; 
          regs_nxt [wp_tmp] = 'h0; 
          cnt = cnt + 1;
        end
        else begin 
          cnt = 0; 
          spill = 0; 
          cwp_nxt = cwp + 2*N; 
          swp_nxt = swp + 2*N; 
        end
      end
      else begin

        if (fill == 1) begin
          `uvm_info(get_type_name(), $sformatf("FILLING"), UVM_MEDIUM)
          if (cnt < 2*N) begin 
            wp_tmp = cwp - cnt - 1;
            regs_nxt [wp_tmp] = t.BUSIN; 
            cnt = cnt + 1; 
          end
        else begin
          cwp_nxt = cwp - 2*N;
          swp_nxt = swp -2*N; 
          cnt = 0; 
          fill = 0; 
        end
        end else begin

          //write/read on mirrored register file (this happens synchronously, but results are only checked at next posedge)
          if (t.WR == 1) 
            if (t.ADD_WR < 3*N) regs_nxt [t.ADD_WR + cwp] = t.DATAIN; 
            else glob_nxt [t.ADD_WR - 3*N] = t.DATAIN;
          if (t.RD1 == 1) 
            if (t.ADD_RD1 < 3*N) read1_nxt = regs [t.ADD_RD1 + cwp];
            else  read1_nxt = glob [t.ADD_RD1 - 3*N];
          if (t.RD2 == 1)
            if (t.ADD_RD2 < 3*N) read2_nxt = regs [t.ADD_RD2 + cwp];
            else  read2_nxt = glob [t.ADD_RD2 - 3*N];
          
          //compute cansave
          wp_tmp = cwp + 4*N;  
          if ((wp_tmp) == swp) cansave = 0; 
          else begin 
            cansave = 1; 
          end
          //compute canrestore
          if (cwp == swp) canrestore = 0;
          else canrestore = 1;
          //`uvm_info(get_type_name(), $sformatf("CANRESTORE = %b", canrestore), UVM_MEDIUM)  
          
          //subcall
          if (t.SUBCALL == 1) 
            if (cansave == 1) begin  
              cwp_nxt = cwp + 2*N;
              `uvm_info(get_type_name(), $sformatf("CWP = %0d", cwp), UVM_MEDIUM)
            end
            else spill = 1;  

          //subreturn
          if (t.SUBRETURN == 1)  begin
            `uvm_info(get_type_name(), $sformatf("CWP = %0d", cwp), UVM_MEDIUM)
            if (canrestore == 1) cwp_nxt = cwp - 2*N; 
            else fill = 1;
          end
        end
      end 
      end

       `uvm_info (get_type_name(), $sformatf("READING PORT 1 | OUT1 (dut): %h, EXP: %h", t.OUT1, read1), UVM_MEDIUM)
       `uvm_info (get_type_name(), $sformatf("READING PORT 2 | OUT2 (dut): %h, EXP: %h \n", t.OUT2, read2), UVM_MEDIUM) 
         //check that R/W operations worked correctly
    if (fill == 0) begin 
      if (spill == 1) begin
        `uvm_info (get_type_name(), $sformatf("SPILLING OUT | BUSOUT (dut): %h, EXP: %h \n", t.BUSOUT, bus_out), UVM_MEDIUM)
        if (bus_out != t.BUSOUT) 
          `uvm_error (get_type_name(), $sformatf("ERROR WHILE SPILLING OUT DATA | BUSOUT (dut): %h, EXP: %h \n", t.BUSOUT, bus_out))  
        end
      else begin
          if (read1 != t.OUT1) 
            `uvm_error (get_type_name(), $sformatf("ERROR WHILE READING PORT 1 | OUT1 (dut): %h, EXP: %h \n", t.OUT1, read1))
          if (read2 != t.OUT2) 
            `uvm_error (get_type_name(), $sformatf("ERROR WHILE READING PORT 2 | OUT2 (dut): %h, EXP: %h \n", t.OUT2, read2) ) 
        end
    end
  //`uvm_info ("SCOREBOARD", $sformatf("REF_MODEL: EXPECTED_ITEM: %0d", expected_item.S), UVM_HIGH)
  endfunction

  //check wether expected output matches the actual one.
  virtual function void write (sqnc_item item); 
    ref_model (item); 
  endfunction 

endclass: RF_scoreboard


class RF_cov extends uvm_subscriber#(sqnc_item);

    `uvm_component_utils(RF_cov)

    sqnc_item item; 
    covergroup RF_cg;
        ADD_WR_cp: coverpoint item.ADD_WR; 
        ADD_RD1_cp: coverpoint item.ADD_RD1;
        ADD_RD2_cp: coverpoint item.ADD_RD2;
    endgroup: RF_cg

    function new (string name = "", uvm_component parent); 
      super.new(name, parent);
      RF_cg = new;
    endfunction

    function void build_phase (uvm_phase phase);
    `uvm_info ("COV", $sformatf("WRITE FUNCTION: start coverage"), UVM_HIGH)
    RF_cg.start(); 
    endfunction 

    function void extract_phase (uvm_phase phase); 
    `uvm_info ("COV", $sformatf("WRITE FUNCTION: stop coverage"), UVM_HIGH)
    RF_cg.stop(); 
    endfunction

  virtual function void write (sqnc_item t);
    `uvm_info ("COV", $sformatf("WRITE FUNCTION: sampling for coverage"), UVM_HIGH)
      item = t; 
      //t.do_copy(ite); 
      RF_cg.sample(); 
    endfunction
endclass: RF_cov



class RF_env extends uvm_env; 

`uvm_component_utils(RF_env)

RF_scoreboard scoreboard; 
RF_agent agent; 
RF_cov cov; 

function new (string name, uvm_component parent); 
super.new (name, parent);
endfunction

function void build_phase (uvm_phase phase); 
    `uvm_info (get_type_name(), "START BUILDING PHASE", UVM_HIGH)
scoreboard = RF_scoreboard::type_id::create("scoreboard",this);
agent = RF_agent::type_id::create("agent",this);
cov = RF_cov::type_id::create("cov",this);
endfunction

function void connect_phase (uvm_phase phase); 
agent.monitor.mon_analysis_port.connect(scoreboard.ap_imp);
agent.monitor.mon_analysis_port.connect(cov.analysis_export); 
endfunction
endclass: RF_env


class RF_test extends uvm_test;

  `uvm_component_utils(RF_test)
  //instance the interface to apply reset, subcall, subreturn. 
  RF_env env; 
  virtual RF_if dut_vi;

  function new (string name, uvm_component parent); 
    super.new (name, parent);
  endfunction

  //get if and create env component
  function void build_phase (uvm_phase phase); 
    `uvm_info (get_type_name(), "START BUILDING PHASE", UVM_MEDIUM)
    if( !uvm_config_db #(virtual RF_if)::get(this, "", "RF_if", dut_vi) )
        `uvm_error("", "uvm_config_db::get failed")
    env = RF_env::type_id::create("RF_env",this);
  endfunction

    //instance and create a sequence, apply reset, randomize squence and start it. 
    task run_phase(uvm_phase phase);
      fork 
        begin 
          apply_reset();
          _wait(15);  
          subcall();
          subcall();
          _wait(15);
          subcall(); 
          _wait(15); 
          subcall(); 
          _wait(15);
          subreturn();
          _wait(15);
          subreturn();
          _wait(15);
          subreturn();
          _wait(15);
          subreturn();
        end
        begin 
          RF_sequence seq;
          seq = RF_sequence::type_id::create("seq");
          if( !seq.randomize() ) 
            `uvm_error("", "Randomize failed")
          //setting the starting phase, allows sequence to execute it's body. 
          seq.starting_phase = phase;
          seq.start(env.agent.sequencer );
        end
      join
        endtask
        
      
    //reset proc
    virtual task apply_reset (); 
     `uvm_info (get_type_name(), "START RESET PROCEDURE...", UVM_MEDIUM)
      dut_vi.ENABLE = 1; 
      dut_vi.RESET = 1; 
      dut_vi.SUBCALL = 0; 
      dut_vi.SUBRETURN = 0;
      repeat(2) begin 
        @(posedge dut_vi.CLK);
        `uvm_info (get_type_name(), $sformatf("RESET = %0b", dut_vi.RESET), UVM_MEDIUM)
      end
      dut_vi.RESET = 0;
      `uvm_info (get_type_name(), "FINISH RESET PROCEDURE...", UVM_MEDIUM)
    endtask

    virtual task _wait (cycles); 
      repeat (15) begin 
        @(posedge dut_vi.CLK);
        dut_vi.SUBCALL = 0; 
        dut_vi.SUBRETURN = 0;
      end
    endtask 


    virtual task subcall (); 
    dut_vi.SUBCALL = 1;
    `uvm_info (get_type_name(), $sformatf("SUBCALL = %0b", dut_vi.SUBCALL), UVM_MEDIUM) 
     @(posedge dut_vi.CLK);
     dut_vi.SUBCALL = 0;
    endtask 

    virtual task subreturn (); 
      dut_vi.SUBRETURN = 1;
      `uvm_info (get_type_name(), $sformatf("SUBRETURN = %0b", dut_vi.SUBRETURN), UVM_MEDIUM) 
      @(posedge dut_vi.CLK);
      dut_vi.SUBRETURN = 0;
    endtask

endclass: RF_test

endpackage: RF_pkg



module top; 
  import uvm_pkg::*;
  import RF_pkg::*;
  
  //instance interface and wrap
  RF_if dut_if (); 
  RF_wrap dut_wrap(dut_if); 

  //clock
  initial
  begin
    dut_if.CLK = 0;
    forever #5 dut_if.CLK = ~dut_if.CLK;
  end
  
  initial
  begin
    //set if db
    uvm_config_db #(virtual RF_if)::set(null, "*", "RF_if", dut_if);
    uvm_top.finish_on_completion = 1;
    //verbosity level
    uvm_top.set_report_verbosity_level(UVM_MEDIUM);
    //run the test
    `uvm_info ("TOP", $sformatf("TEST STARTIG..."), UVM_MEDIUM)
    run_test("RF_test");
    `uvm_info ("TOP", $sformatf("TEST FINISHED"), UVM_MEDIUM)
  end

endmodule: top