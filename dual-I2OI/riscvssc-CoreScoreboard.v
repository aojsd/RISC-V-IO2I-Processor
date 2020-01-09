//=========================================================================
// IO2I 2-Wide RISC-V Scoreboard
//=========================================================================

`ifndef RISCV_CORE_SCOREBOARD_V
`define RISCV_CORE_SCOREBOARD_V

module riscv_CoreScoreboard
(
  input         clk,
  input         reset,

  input  [ 4:0] src00,            // Source 0 of ir0
  input         src00_en,
  input         src00_renamed,
  input  [ 4:0] src01,            // Source 1 of ir0
  input         src01_en,
  input         src01_renamed,
  input  [ 4:0] dst0,             // Instruction 0 destination register
  input         dst0_en,          // Write to destination register
  input  [ 1:0] func_ir0,         // Ir0 type
  input         ir0_issued,

  input  [ 4:0] src10,            // Source 0 of ir1
  input         src10_en,
  input         src10_renamed,
  input  [ 4:0] src11,            // Source 1 of ir1
  input         src11_en,
  input         src11_renamed,
  input  [ 4:0] dst1,             // Instruction 1 destination register
  input         dst1_en,          // Write to destination register
  input  [ 1:0] func_ir1,         // Ir1 type
  input         ir1_issued,

  input         steer_signal,     // Steering signal to determine pipelines
  input         inst_val_Ihl,

  input         stall_X0hl,
  input         stall_X1hl,
  input         stall_X2hl,
  input         stall_X3hl,
  input         stall_Whl,

  input  [ 4:0] rob_commit_slot_1,  // First committed slot in the ROB
  input         rob_commit_val_1,   // Ensure ROB slot 1 is valid
  input  [ 4:0] rob_commit_slot_2,  // Second committed slot in the ROB
  input         rob_commit_val_2,   // Ensure ROB slot 2 is valid

  output        stall_ir0,        // Src0 stall signal from scoreboard
  output        stall_ir1,        // Src1 stall signal from scoreboard

  output [ 3:0] op00_byp_mux_sel, // Ir0, src0 bypass signal
  output [ 3:0] op01_byp_mux_sel, // Ir0, src1 bypass signal
  output [ 3:0] op10_byp_mux_sel, // Ir1, src0 bypass signal
  output [ 3:0] op11_byp_mux_sel  // Ir1, src1 bypass signal

  // output [ 4:0] op00_byp_rob_slot,  // Ir0, src0 ROB slot
  // output [ 4:0] op01_byp_rob_slot,  // Ir0, src1 ROB slot
  // output [ 4:0] op10_byp_rob_slot,  // Ir1, src0 ROB slot
  // output [ 4:0] op11_byp_rob_slot   // Ir1, src1 ROB slot
);

  reg [31:0] pending;
  reg [31:0] pipeline;
  reg [ 4:0] reg_latency  [31:0];
  reg [ 4:0] rob_slots    [31:0];
  reg [ 1:0] func_unit    [31:0];

  reg [31:0] rt_pipeline;

  localparam alu_inst     = 2'b00;
  localparam mem_inst     = 2'b01;
  localparam muldiv_inst  = 2'b10;

  wire [4:0] stalls = { stall_X0hl,
                        stall_X1hl,
                        stall_X2hl,
                        stall_X3hl,
                        stall_Whl};

  wire [4:0] alu_st = { 3'b0, stall_X0hl, stall_Whl };

  integer i;
  always @ ( posedge clk ) begin
    if ( reset ) begin
      pending  <= 32'b0;
      pipeline <= 32'b0;

      for(i = 0; i < 32; i = i + 1) begin
        reg_latency[i] = 5'b00000;
        func_unit[i]   = 2'b00;
      end
    end else begin
      //----------------------------------------------------------------------
      // Issue Logic
      //----------------------------------------------------------------------

      // If an instruction is issued, update the scoreboard if the instruction
      // writes to the register file. Index into the scoreboard using the write
      // address of the instruction.
      // For all registers where a write-enabled instruction is not issued, set
      // the X0 scoreboard entry to 0.
      // The scoreboard should only issue if X0 is not stalled.
      // Pipeline 0 is A, pipeline 1 is B

      for ( i = 0; i < 32; i = i + 1 ) begin
        if ( ir0_issued && dst0_en && ( i == dst0 ) ) begin
          pipeline[i]   <= steer_signal;
          pending[i]    <= 1'b1;
          func_unit[i]  <= func_ir0;
          rob_slots[i]  <= i;

          if(steer_signal) begin
            reg_latency[i] <= 5'b00010;
          end
          else reg_latency[i] <= 5'b10000;
        end
        else if ( ir1_issued && dst1_en && ( i == dst1 ) ) begin
          pipeline[i]   <= !steer_signal;
          pending[i]    <= 1'b1;
          func_unit[i]  <= func_ir1;
          rob_slots[i]  <= i;

          if(!steer_signal) begin
            reg_latency[i] <= 5'b00010;
          end
          else reg_latency[i] <= 5'b10000;
        end

        //----------------------------------------------------------------------
        // Shift and Pending Logic
        //----------------------------------------------------------------------

        // Shift reg_latency based on the combined stall vector.
        // The pending bit is set to 1 if the instruction is still in the pipeline,
        // not necessarily if it cannot yet be bypassed.
        // Pending bit gets reset to 0 once the instruction is committed in the ROB
        else begin
          if(!pipeline[i]) begin
            reg_latency[i] <= ( reg_latency[i] & stalls ) |
                              ( ( reg_latency[i] & ~stalls ) >> 1 );            
          end
          else begin
            reg_latency[i] <= ( reg_latency[i] & alu_st ) |
                              ( ( reg_latency[i] & ~alu_st ) >> 1 );
          end

          if ( rob_commit_val_1 && rob_commit_slot_1 == i ) begin
            pending[i] <= 1'b0;
          end
          else if ( rob_commit_val_2 && rob_commit_slot_2 == i ) begin
            pending[i] <= 1'b0;
          end
        end
      end
    end
  end

  //----------------------------------------------------------------------
  // Bypassing Logic
  //----------------------------------------------------------------------

  // Bypass mux selects
  localparam byp_r0  = 4'd0;  // Use rdata0
  localparam byp_AX0 = 4'd1;  // Bypass from AX0
  localparam byp_AX1 = 4'd2;  // Bypass from AX1
  localparam byp_AX2 = 4'd3;  // Bypass from AX2
  localparam byp_AX3 = 4'd4;  // Bypass from AX3
  localparam byp_AW  = 4'd5;  // Bypass from AW
  localparam byp_BX0 = 4'd6;  // Bypass from BX0
  localparam byp_BX1 = 4'd7;  // Bypass from BX1
  localparam byp_BX2 = 4'd8;  // Bypass from BX2
  localparam byp_BX3 = 4'd9;  // Bypass from BX3
  localparam byp_BW  = 4'd10; // Bypass from BW
  localparam byp_ROB = 4'd11; // Bypass from reorder buffer

  wire rs00_bypA_cond_Dhl = !pipeline[src00] && src00_renamed && pending[src00];
  wire rs00_bypB_cond_Dhl = pipeline[src00]  && src00_renamed && pending[src00];

  wire rs01_bypA_cond_Dhl = !pipeline[src01] && src01_renamed && pending[src01];
  wire rs01_bypB_cond_Dhl = pipeline[src01]  && src01_renamed && pending[src01];

  wire rs10_bypA_cond_Dhl = !pipeline[src10] && src10_renamed && pending[src10];
  wire rs10_bypB_cond_Dhl = pipeline[src10]  && src10_renamed && pending[src10];

  wire rs11_bypA_cond_Dhl = !pipeline[src11] && src11_renamed && pending[src11];
  wire rs11_bypB_cond_Dhl = pipeline[src11]  && src11_renamed && pending[src11];

  // Operand Bypass Mux Select

  wire [3:0] op00_byp_mux_sel
    = (rs00_bypA_cond_Dhl && ( reg_latency[src00] == 5'b10000)) ? byp_AX0
    : (rs00_bypA_cond_Dhl && ( reg_latency[src00] == 5'b01000)) ? byp_AX1
    : (rs00_bypA_cond_Dhl && ( reg_latency[src00] == 5'b00100)) ? byp_AX2
    : (rs00_bypA_cond_Dhl && ( reg_latency[src00] == 5'b00010)) ? byp_AX3
    : (rs00_bypA_cond_Dhl && ( reg_latency[src00] == 5'b00001)) ? byp_AW

    : (rs00_bypB_cond_Dhl && ( reg_latency[src00] == 5'b00010)) ? byp_BX0
    : (rs00_bypB_cond_Dhl && ( reg_latency[src00] == 5'b00001)) ? byp_BW

    : (pending[src00] && src00_renamed && (reg_latency[src00] == 5'd0)) ? byp_ROB
    :                                                byp_r0;  

  wire [3:0] op01_byp_mux_sel
    = (rs01_bypA_cond_Dhl && ( reg_latency[src01] == 5'b10000)) ? byp_AX0
    : (rs01_bypA_cond_Dhl && ( reg_latency[src01] == 5'b01000)) ? byp_AX1
    : (rs01_bypA_cond_Dhl && ( reg_latency[src01] == 5'b00100)) ? byp_AX2
    : (rs01_bypA_cond_Dhl && ( reg_latency[src01] == 5'b00010)) ? byp_AX3
    : (rs01_bypA_cond_Dhl && ( reg_latency[src01] == 5'b00001)) ? byp_AW

    : (rs01_bypB_cond_Dhl && ( reg_latency[src01] == 5'b00010)) ? byp_BX0
    : (rs01_bypB_cond_Dhl && ( reg_latency[src01] == 5'b00001)) ? byp_BW

    : (pending[src01] && src01_renamed && (reg_latency[src01] == 5'd0)) ? byp_ROB
    :                                                byp_r0;

  wire [3:0] op10_byp_mux_sel
    = (rs10_bypA_cond_Dhl && ( reg_latency[src10] == 5'b10000)) ? byp_AX0
    : (rs10_bypA_cond_Dhl && ( reg_latency[src10] == 5'b01000)) ? byp_AX1
    : (rs10_bypA_cond_Dhl && ( reg_latency[src10] == 5'b00100)) ? byp_AX2
    : (rs10_bypA_cond_Dhl && ( reg_latency[src10] == 5'b00010)) ? byp_AX3
    : (rs10_bypA_cond_Dhl && ( reg_latency[src10] == 5'b00001)) ? byp_AW

    : (rs10_bypB_cond_Dhl && ( reg_latency[src10] == 5'b00010)) ? byp_BX0
    : (rs10_bypB_cond_Dhl && ( reg_latency[src10] == 5'b00001)) ? byp_BW

    : (pending[src10] && src10_renamed && (reg_latency[src10] == 5'd0)) ? byp_ROB
    :                                                byp_r0;

  wire [3:0] op11_byp_mux_sel
    = (rs11_bypA_cond_Dhl && ( reg_latency[src11] == 5'b10000)) ? byp_AX0
    : (rs11_bypA_cond_Dhl && ( reg_latency[src11] == 5'b01000)) ? byp_AX1
    : (rs11_bypA_cond_Dhl && ( reg_latency[src11] == 5'b00100)) ? byp_AX2
    : (rs11_bypA_cond_Dhl && ( reg_latency[src11] == 5'b00010)) ? byp_AX3
    : (rs11_bypA_cond_Dhl && ( reg_latency[src11] == 5'b00001)) ? byp_AW

    : (rs11_bypB_cond_Dhl && ( reg_latency[src11] == 5'b00010)) ? byp_BX0
    : (rs11_bypB_cond_Dhl && ( reg_latency[src11] == 5'b00001)) ? byp_BW

    : (pending[src11] && src11_renamed && (reg_latency[src11] == 5'd0)) ? byp_ROB
    :                                                byp_r0;

  //----------------------------------------------------------------------
  // Stall Logic
  //----------------------------------------------------------------------

  // Scoreboard-based stall signals
  wire op00_can_byp
    = (func_unit[src00] == alu_inst)    ? 1'b1
    : (func_unit[src00] == mem_inst)    ? reg_latency[src00] < 5'b10000
    : (func_unit[src00] == muldiv_inst) ? reg_latency[src00] < 5'b00100
    : (reg_latency[src00] == 5'b00000)  ? 1'b1
    : 1'b0;
  wire op01_can_byp
    = (func_unit[src01] == alu_inst)    ? 1'b1
    : (func_unit[src01] == mem_inst)    ? reg_latency[src01] < 5'b10000
    : (func_unit[src01] == muldiv_inst) ? reg_latency[src01] < 5'b00100
    : (reg_latency[src01] == 5'b00000)  ? 1'b1
    : 1'b0;
  wire op10_can_byp
    = (func_unit[src10] == alu_inst)    ? 1'b1
    : (func_unit[src10] == mem_inst)    ? reg_latency[src10] < 5'b10000
    : (func_unit[src10] == muldiv_inst) ? reg_latency[src10] < 5'b00100
    : (reg_latency[src10] == 5'b00000)  ? 1'b1
    : 1'b0;
  wire op11_can_byp
    = (func_unit[src11] == alu_inst)    ? 1'b1
    : (func_unit[src11] == mem_inst)    ? reg_latency[src11] < 5'b10000
    : (func_unit[src11] == muldiv_inst) ? reg_latency[src11] < 5'b00100
    : (reg_latency[src11] == 5'b00000)  ? 1'b1
    : 1'b0;

  wire stall_ir0 = ( ( !op00_can_byp && src00_en && src00_renamed )
                  || ( !op01_can_byp && src01_en && src01_renamed ) )
                          && inst_val_Ihl;
  wire stall_ir1 = ( ( !op10_can_byp && src10_en && src10_renamed )
                  || ( !op11_can_byp && src11_en && src11_renamed ) )
                          && inst_val_Ihl; 


  // initial begin
  //   for(i = 0; i < 32; i = i + 1) begin
  //     $dumpvars(0, rob_slots[i]);
  //     $dumpvars(0, reg_latency[i]);
  //   end
  // end

endmodule

`endif

