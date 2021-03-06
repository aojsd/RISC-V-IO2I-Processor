//=========================================================================
// 7-Stage RISCV Control Unit
//=========================================================================

`ifndef RISCV_CORE_CTRL_V
`define RISCV_CORE_CTRL_V

`include "riscvssc-InstMsg.v"
`include "riscvssc-CoreScoreboard.v"
`include "riscvssc-CoreReorderBuffer.v"
`include "riscvssc-CoreIssueQueue.v"

module riscv_CoreCtrl
(
  input clk,
  input reset,

  // Instruction Memory Port
  output        imemreq0_val,
  input         imemreq0_rdy,
  input  [31:0] imemresp0_msg_data,
  input         imemresp0_val,

  // Instruction Memory Port
  output        imemreq1_val,
  input         imemreq1_rdy,
  input  [31:0] imemresp1_msg_data,
  input         imemresp1_val,

  // Data Memory Port

  output        dmemreq_msg_rw,
  output  [1:0] dmemreq_msg_len,
  output        dmemreq_val,
  input         dmemreq_rdy,
  input         dmemresp_val,

  // Controls Signals (ctrl->dpath)

  output  [1:0] pc_mux_sel_Phl,
  output        steering_mux_sel_Ihl,
  output  [3:0] opA0_byp_mux_sel_Ihl,
  output  [1:0] opA0_mux_sel_Ihl,
  output  [3:0] opA1_byp_mux_sel_Ihl,
  output  [2:0] opA1_mux_sel_Ihl,
  output  [3:0] opB0_byp_mux_sel_Ihl,
  output  [1:0] opB0_mux_sel_Ihl,
  output  [3:0] opB1_byp_mux_sel_Ihl,
  output  [2:0] opB1_mux_sel_Ihl,
  output [31:0] instA_Ihl,
  output [31:0] instB_Ihl,
  output  [3:0] aluA_fn_X0hl,
  output  [3:0] aluB_fn_X0hl,
  output  [2:0] muldivreq_msg_fn_Ihl,
  output        muldivreq_val,
  input         muldivreq_rdy,
  input         muldivresp_val,
  output        muldivresp_rdy,
  output  [2:0] dmemresp_mux_sel_X1hl,
  output        dmemresp_queue_en_X1hl,
  output        dmemresp_queue_val_X1hl,
  output        muldiv_mux_sel_X3hl,
  output        execute_mux_sel_X3hl,
  output        memex_mux_sel_X1hl,
  output        stall_Fhl,
  output        stall_Dhl,
  output        stall_Ihl,
  output        stall_X0hl,
  output        stall_X1hl,
  output        stall_X2hl,
  output        stall_X3hl,
  output        stall_Whl,

  // Control Signals (dpath->ctrl)

  input         branch_cond_eq_X0hl,
  input         branch_cond_ne_X0hl,
  input         branch_cond_lt_X0hl,
  input         branch_cond_ltu_X0hl,
  input         branch_cond_ge_X0hl,
  input         branch_cond_geu_X0hl,
  input  [31:0] proc2csr_data_Whl,

  // Reorder Buffer Signals (ctrl->dpath)

  output [ 4:0] opA0_byp_rob_slot_Ihl,
  output [ 4:0] opA1_byp_rob_slot_Ihl,
  output [ 4:0] opB0_byp_rob_slot_Ihl,
  output [ 4:0] opB1_byp_rob_slot_Ihl,

  output        rob_fill_wen_A_Whl,
  output [ 4:0] rob_fill_slot_A_Whl,
  output        rob_fill_wen_B_Whl,
  output [ 4:0] rob_fill_slot_B_Whl,

  output        rob_commit_wen_1_Chl,
  output [ 4:0] rob_commit_slot_1_Chl,
  output [ 4:0] rob_commit_waddr_1_Chl,
  output        rob_commit_wen_2_Chl,
  output [ 4:0] rob_commit_slot_2_Chl,
  output [ 4:0] rob_commit_waddr_2_Chl,

  // iq signals
  output [ 4:0]  iq_slot_A_Dhl,
  output [ 4:0]  iq_slot_B_Dhl,
  output [ 4:0]  iq_slot_A_Ihl,
  output [ 4:0]  iq_slot_B_Ihl,
  // CSR Status

  output [31:0] csr_status
);

  //----------------------------------------------------------------------
  // PC Stage: Instruction Memory Request
  //----------------------------------------------------------------------

  // PC Mux Select

  assign pc_mux_sel_Phl
    = brj_taken_X0hl   ? pm_b
    : brj_taken_Ihl    ? pc_mux_sel_Ihl
    :                    pm_p;

  // Only send a valid imem request if not stalled

  wire   imemreq_val_Phl = reset || !stall_Phl;
  assign imemreq0_val     = imemreq_val_Phl;
  assign imemreq1_val     = imemreq_val_Phl;

  // Dummy Squash Signal

  wire squash_Phl = 1'b0;

  // Stall in PC if F is stalled

  wire stall_Phl = stall_Fhl;

  // Next bubble bit

  wire bubble_next_Phl = ( squash_Phl || stall_Phl );

  //----------------------------------------------------------------------
  // F <- P
  //----------------------------------------------------------------------

  reg imemreq_val_Fhl;

  reg bubble_Fhl;

  always @ ( posedge clk ) begin
    // Only pipeline the bubble bit if the next stage is not stalled
    if ( reset ) begin
      imemreq_val_Fhl <= 1'b0;

      bubble_Fhl <= 1'b0;
    end
    else if( !stall_Fhl ) begin 
      imemreq_val_Fhl <= imemreq_val_Phl;

      bubble_Fhl <= bubble_next_Phl;
    end
    else begin 
      imemreq_val_Fhl <= imemreq_val_Phl;
    end
  end

  //----------------------------------------------------------------------
  // Fetch Stage: Instruction Memory Response
  //----------------------------------------------------------------------

  // Is the current stage valid?

  wire inst_val_Fhl = ( !bubble_Fhl && !squash_Fhl );

  // Squash instruction in F stage if branch taken for a valid
  // instruction or if there was an exception in X stage

  wire squash_Fhl
    = ( inst_val_Ihl && brj_taken_Ihl )
   || ( inst_val_X0hl && brj_taken_X0hl )
   || (iq_contains_jmp);

  // Stall in F if D is stalled

  assign stall_Fhl = stall_Dhl;

  // Next bubble bit

  wire bubble_sel_Fhl  = ( squash_Fhl || stall_Fhl );
  wire bubble_next_Fhl = ( !bubble_sel_Fhl ) ? bubble_Fhl
                       : ( bubble_sel_Fhl )  ? 1'b1
                       :                       1'bx;

  //----------------------------------------------------------------------
  // Queue for instruction memory response
  //----------------------------------------------------------------------

  wire imemresp0_queue_en_Fhl = ( stall_Dhl && imemresp0_val );
  wire imemresp0_queue_val_next_Fhl
    = stall_Dhl && ( imemresp0_val || imemresp0_queue_val_Fhl );

  wire imemresp1_queue_en_Fhl = ( stall_Dhl && imemresp1_val );
  wire imemresp1_queue_val_next_Fhl
    = stall_Dhl && ( imemresp1_val || imemresp1_queue_val_Fhl );

  reg [31:0] imemresp0_queue_reg_Fhl;
  reg        imemresp0_queue_val_Fhl;

  reg [31:0] imemresp1_queue_reg_Fhl;
  reg        imemresp1_queue_val_Fhl;

  always @ ( posedge clk ) begin
    if ( imemresp0_queue_en_Fhl ) begin
      imemresp0_queue_reg_Fhl <= imemresp0_msg_data;
    end
    if ( imemresp1_queue_en_Fhl ) begin
      imemresp1_queue_reg_Fhl <= imemresp1_msg_data;
    end
    imemresp0_queue_val_Fhl <= imemresp0_queue_val_next_Fhl;
    imemresp1_queue_val_Fhl <= imemresp1_queue_val_next_Fhl;
  end

  //----------------------------------------------------------------------
  // Instruction memory queue mux
  //----------------------------------------------------------------------

  wire [31:0] imemresp0_queue_mux_out_Fhl
    = ( !imemresp0_queue_val_Fhl ) ? imemresp0_msg_data
    : ( imemresp0_queue_val_Fhl )  ? imemresp0_queue_reg_Fhl
    :                               32'bx;

  wire [31:0] imemresp1_queue_mux_out_Fhl
    = ( !imemresp1_queue_val_Fhl ) ? imemresp1_msg_data
    : ( imemresp1_queue_val_Fhl )  ? imemresp1_queue_reg_Fhl
    :                               32'bx;

  //----------------------------------------------------------------------
  // D <- F
  //----------------------------------------------------------------------

  reg [31:0] ir0_Dhl;
  reg [31:0] ir1_Dhl;
  reg        bubble_Dhl;

  always @ ( posedge clk ) begin
    if ( reset ) begin
      bubble_Dhl         <= 1'b1;
    end
    else if( !stall_Dhl ) begin
      ir0_Dhl    <= imemresp0_queue_mux_out_Fhl;
      ir1_Dhl    <= imemresp1_queue_mux_out_Fhl;

      bubble_Dhl <= bubble_next_Fhl;
    end

    // // Always send a bubble to the decode stage if a branch or jump is taken

    // if( brj_taken_Dhl || brj_taken_X0hl ) begin
    //   bubble_Dhl <= bubble_next_Fhl;
    // end    
  end

  //----------------------------------------------------------------------
  // Decode Stage: Constants
  //----------------------------------------------------------------------

  // Generic Parameters

  localparam n = 1'd0;
  localparam y = 1'd1;

  // Register specifiers

  localparam rx = 5'bx;
  localparam r0 = 5'd0;

  // Branch Type

  localparam br_x    = 3'bx;
  localparam br_none = 3'd0;
  localparam br_beq  = 3'd1;
  localparam br_bne  = 3'd2;
  localparam br_blt  = 3'd3;
  localparam br_bltu = 3'd4;
  localparam br_bge  = 3'd5;
  localparam br_bgeu = 3'd6;

  // PC Mux Select

  localparam pm_x   = 2'bx;  // Don't care
  localparam pm_p   = 2'd0;  // Use pc+4
  localparam pm_b   = 2'd1;  // Use branch address
  localparam pm_j   = 2'd2;  // Use jump address
  localparam pm_r   = 2'd3;  // Use jump register

  // Operand Bypass Mux Select

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
  localparam byp_rob = 4'd11; // Bypass from reorder buffer

  // Operand 0 Mux Select

  localparam am_x     = 2'bx;
  localparam am_rdat  = 2'd0; // Use output of bypass mux for rs1
  localparam am_pc    = 2'd1; // Use current PC
  localparam am_pc4   = 2'd2; // Use PC + 4
  localparam am_0     = 2'd3; // Use constant 0

  // Operand 1 Mux Select

  localparam bm_x      = 3'bx; // Don't care
  localparam bm_rdat   = 3'd0; // Use output of bypass mux for rs2
  localparam bm_shamt  = 3'd1; // Use shift amount
  localparam bm_imm_u  = 3'd2; // Use U-type immediate
  localparam bm_imm_sb = 3'd3; // Use SB-type immediate
  localparam bm_imm_i  = 3'd4; // Use I-type immediate
  localparam bm_imm_s  = 3'd5; // Use S-type immediate
  localparam bm_0      = 3'd6; // Use constant 0

  // ALU Function

  localparam alu_x    = 4'bx;
  localparam alu_add  = 4'd0;
  localparam alu_sub  = 4'd1;
  localparam alu_sll  = 4'd2;
  localparam alu_or   = 4'd3;
  localparam alu_lt   = 4'd4;
  localparam alu_ltu  = 4'd5;
  localparam alu_and  = 4'd6;
  localparam alu_xor  = 4'd7;
  localparam alu_nor  = 4'd8;
  localparam alu_srl  = 4'd9;
  localparam alu_sra  = 4'd10;

  // Muldiv Function

  localparam md_x    = 3'bx;
  localparam md_mul  = 3'd0;
  localparam md_div  = 3'd1;
  localparam md_divu = 3'd2;
  localparam md_rem  = 3'd3;
  localparam md_remu = 3'd4;

  // MulDiv Mux Select

  localparam mdm_x = 1'bx; // Don't Care
  localparam mdm_l = 1'd0; // Take lower half of 64-bit result, mul/div/divu
  localparam mdm_u = 1'd1; // Take upper half of 64-bit result, rem/remu

  // Execute Mux Select

  localparam em_x   = 1'bx; // Don't Care
  localparam em_alu = 1'd0; // Use ALU output
  localparam em_md  = 1'd1; // Use muldiv output

  // Memory Request Type

  localparam nr = 2'b0; // No request
  localparam ld = 2'd1; // Load
  localparam st = 2'd2; // Store

  // Subword Memop Length

  localparam ml_x  = 2'bx;
  localparam ml_w  = 2'd0;
  localparam ml_b  = 2'd1;
  localparam ml_h  = 2'd2;

  // Memory Response Mux Select

  localparam dmm_x  = 3'bx;
  localparam dmm_w  = 3'd0;
  localparam dmm_b  = 3'd1;
  localparam dmm_bu = 3'd2;
  localparam dmm_h  = 3'd3;
  localparam dmm_hu = 3'd4;

  // Writeback Mux 1

  localparam wm_x   = 1'bx; // Don't care
  localparam wm_alu = 1'd0; // Use ALU output
  localparam wm_mem = 1'd1; // Use data memory response

  //----------------------------------------------------------------------
  // Decode Stage: Logic
  //----------------------------------------------------------------------

  // Is the current stage valid?

  wire inst_val_Dhl = ( !bubble_Dhl && !squash_Dhl );

  // Parse instruction fields

  wire   [4:0] inst0_rs1_Dhl;
  wire   [4:0] inst0_rs2_Dhl;
  wire   [4:0] inst0_rd_Dhl;

  riscv_InstMsgFromBits inst0_msg_from_bits
  (
    .msg      (ir0_Dhl),
    .opcode   (),
    .rs1      (inst0_rs1_Dhl),
    .rs2      (inst0_rs2_Dhl),
    .rd       (inst0_rd_Dhl),
    .funct3   (),
    .funct7   (),
    .shamt    (),
    .imm_i    (),
    .imm_s    (),
    .imm_sb   (),
    .imm_u    (),
    .imm_uj   ()
  );

  wire   [4:0] inst1_rs1_Dhl;
  wire   [4:0] inst1_rs2_Dhl;
  wire   [4:0] inst1_rd_Dhl;

  riscv_InstMsgFromBits inst1_msg_from_bits
  (
    .msg      (ir1_Dhl),
    .opcode   (),
    .rs1      (inst1_rs1_Dhl),
    .rs2      (inst1_rs2_Dhl),
    .rd       (inst1_rd_Dhl),
    .funct3   (),
    .funct7   (),
    .shamt    (),
    .imm_i    (),
    .imm_s    (),
    .imm_sb   (),
    .imm_u    (),
    .imm_uj   ()
  );

  // Shorten register specifier name for table

  wire [4:0] rs00 = inst0_rs1_Dhl;
  wire [4:0] rs01 = inst0_rs2_Dhl;
  wire [4:0] rd0 = inst0_rd_Dhl;

  wire [4:0] rs10 = inst1_rs1_Dhl;
  wire [4:0] rs11 = inst1_rs2_Dhl;
  wire [4:0] rd1 = inst1_rd_Dhl;

  // Instruction Decode

  localparam cs_sz = 39;
  reg [cs_sz-1:0] cs0;
  reg [cs_sz-1:0] cs1;

  always @ (*) begin

    cs0 = {cs_sz{1'bx}}; // Default to invalid instruction

    casez ( ir0_Dhl )

      //                                j     br       pc      op0      rs1 op1       rs2 alu       md       md md     ex      mem  mem   memresp wb      rf      csr
      //                            val taken type     muxsel  muxsel   en  muxsel    en  fn        fn       en muxsel muxsel  rq   len   muxsel  muxsel  wen wa  wen
      `RISCV_INST_MSG_LUI     :cs0={ y,  n,    br_none, pm_p,   am_0,    n,  bm_imm_u, n,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_AUIPC   :cs0={ y,  n,    br_none, pm_p,   am_pc,   n,  bm_imm_u, n,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };

      `RISCV_INST_MSG_ADDI    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_ORI     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_or,   md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SLTI    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_lt,   md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SLTIU   :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_ltu,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_XORI    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_xor,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_ANDI    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_and,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SLLI    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_sll,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SRLI    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_srl,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SRAI    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_sra,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };

      `RISCV_INST_MSG_ADD     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SUB     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_sub,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SLL     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_sll,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SLT     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_lt,   md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SLTU    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_ltu,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_XOR     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_xor,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SRL     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_srl,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_SRA     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_sra,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_OR      :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_or,   md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_AND     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_and,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };

      `RISCV_INST_MSG_LW      :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_x,   ld,  ml_w, dmm_w,  wm_mem, y,  rd0, n   };
      `RISCV_INST_MSG_LB      :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_x,   ld,  ml_b, dmm_b,  wm_mem, y,  rd0, n   };
      `RISCV_INST_MSG_LH      :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_x,   ld,  ml_h, dmm_h,  wm_mem, y,  rd0, n   };
      `RISCV_INST_MSG_LBU     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_x,   ld,  ml_b, dmm_bu, wm_mem, y,  rd0, n   };
      `RISCV_INST_MSG_LHU     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_x,   ld,  ml_h, dmm_hu, wm_mem, y,  rd0, n   };
      `RISCV_INST_MSG_SW      :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_s, y,  alu_add,  md_x,    n, mdm_x, em_x,   st,  ml_w, dmm_w,  wm_mem, n,  r0, n   };
      `RISCV_INST_MSG_SB      :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_s, y,  alu_add,  md_x,    n, mdm_x, em_x,   st,  ml_b, dmm_b,  wm_mem, n,  r0, n   };
      `RISCV_INST_MSG_SH      :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_s, y,  alu_add,  md_x,    n, mdm_x, em_x,   st,  ml_h, dmm_h,  wm_mem, n,  r0, n   };

      `RISCV_INST_MSG_JAL     :cs0={ y,  y,    br_none, pm_j,   am_pc4,  n,  bm_0,     n,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_JALR    :cs0={ y,  y,    br_none, pm_r,   am_pc4,  y,  bm_0,     n,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };

      `RISCV_INST_MSG_BNE     :cs0={ y,  n,    br_bne,  pm_b,   am_rdat, y,  bm_rdat,  y,  alu_xor,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };
      `RISCV_INST_MSG_BEQ     :cs0={ y,  n,    br_beq,  pm_b,   am_rdat, y,  bm_rdat,  y,  alu_xor,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };
      `RISCV_INST_MSG_BLT     :cs0={ y,  n,    br_blt,  pm_b,   am_rdat, y,  bm_rdat,  y,  alu_sub,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };
      `RISCV_INST_MSG_BGE     :cs0={ y,  n,    br_bge,  pm_b,   am_rdat, y,  bm_rdat,  y,  alu_sub,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };
      `RISCV_INST_MSG_BLTU    :cs0={ y,  n,    br_bltu, pm_b,   am_rdat, y,  bm_rdat,  y,  alu_sub,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };
      `RISCV_INST_MSG_BGEU    :cs0={ y,  n,    br_bgeu, pm_b,   am_rdat, y,  bm_rdat,  y,  alu_sub,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };

      `RISCV_INST_MSG_MUL     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_x,    md_mul,  y, mdm_l, em_md,  nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_DIV     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_x,    md_div,  y, mdm_l, em_md,  nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_REM     :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_x,    md_rem,  y, mdm_u, em_md,  nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_DIVU    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_x,    md_divu, y, mdm_l, em_md,  nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };
      `RISCV_INST_MSG_REMU    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_x,    md_remu, y, mdm_u, em_md,  nr,  ml_x, dmm_x,  wm_alu, y,  rd0, n   };

      `RISCV_INST_MSG_CSRW    :cs0={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_0,     y,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, n,  r0, y   };

    endcase

  end

  always @ (*) begin

    cs1 = {cs_sz{1'bx}}; // Default to invalid instruction

    casez ( ir1_Dhl )

      //                                j     br       pc      op0      rs1 op1       rs2 alu       md       md md     ex      mem  mem   memresp wb      rf      csr
      //                            val taken type     muxsel  muxsel   en  muxsel    en  fn        fn       en muxsel muxsel  rq   len   muxsel  muxsel  wen wa  wen
      `RISCV_INST_MSG_LUI     :cs1={ y,  n,    br_none, pm_p,   am_0,    n,  bm_imm_u, n,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_AUIPC   :cs1={ y,  n,    br_none, pm_p,   am_pc,   n,  bm_imm_u, n,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };

      `RISCV_INST_MSG_ADDI    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_ORI     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_or,   md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SLTI    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_lt,   md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SLTIU   :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_ltu,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_XORI    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_xor,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_ANDI    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_and,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SLLI    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_sll,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SRLI    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_srl,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SRAI    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_sra,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };

      `RISCV_INST_MSG_ADD     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SUB     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_sub,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SLL     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_sll,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SLT     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_lt,   md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SLTU    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_ltu,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_XOR     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_xor,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SRL     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_srl,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_SRA     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_sra,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_OR      :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_or,   md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_AND     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_and,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };

      `RISCV_INST_MSG_LW      :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_x,   ld,  ml_w, dmm_w,  wm_mem, y,  rd1, n   };
      `RISCV_INST_MSG_LB      :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_x,   ld,  ml_b, dmm_b,  wm_mem, y,  rd1, n   };
      `RISCV_INST_MSG_LH      :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_x,   ld,  ml_h, dmm_h,  wm_mem, y,  rd1, n   };
      `RISCV_INST_MSG_LBU     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_x,   ld,  ml_b, dmm_bu, wm_mem, y,  rd1, n   };
      `RISCV_INST_MSG_LHU     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_i, n,  alu_add,  md_x,    n, mdm_x, em_x,   ld,  ml_h, dmm_hu, wm_mem, y,  rd1, n   };
      `RISCV_INST_MSG_SW      :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_s, y,  alu_add,  md_x,    n, mdm_x, em_x,   st,  ml_w, dmm_w,  wm_mem, n,  r0, n   };
      `RISCV_INST_MSG_SB      :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_s, y,  alu_add,  md_x,    n, mdm_x, em_x,   st,  ml_b, dmm_b,  wm_mem, n,  r0, n   };
      `RISCV_INST_MSG_SH      :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_imm_s, y,  alu_add,  md_x,    n, mdm_x, em_x,   st,  ml_h, dmm_h,  wm_mem, n,  r0, n   };

      `RISCV_INST_MSG_JAL     :cs1={ y,  y,    br_none, pm_j,   am_pc4,  n,  bm_0,     n,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_JALR    :cs1={ y,  y,    br_none, pm_r,   am_pc4,  y,  bm_0,     n,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };

      `RISCV_INST_MSG_BNE     :cs1={ y,  n,    br_bne,  pm_b,   am_rdat, y,  bm_rdat,  y,  alu_xor,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };
      `RISCV_INST_MSG_BEQ     :cs1={ y,  n,    br_beq,  pm_b,   am_rdat, y,  bm_rdat,  y,  alu_xor,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };
      `RISCV_INST_MSG_BLT     :cs1={ y,  n,    br_blt,  pm_b,   am_rdat, y,  bm_rdat,  y,  alu_sub,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };
      `RISCV_INST_MSG_BGE     :cs1={ y,  n,    br_bge,  pm_b,   am_rdat, y,  bm_rdat,  y,  alu_sub,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };
      `RISCV_INST_MSG_BLTU    :cs1={ y,  n,    br_bltu, pm_b,   am_rdat, y,  bm_rdat,  y,  alu_sub,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };
      `RISCV_INST_MSG_BGEU    :cs1={ y,  n,    br_bgeu, pm_b,   am_rdat, y,  bm_rdat,  y,  alu_sub,  md_x,    n, mdm_x, em_x,   nr,  ml_x, dmm_x,  wm_x,   n,  r0, n   };

      `RISCV_INST_MSG_MUL     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_x,    md_mul,  y, mdm_l, em_md,  nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_DIV     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_x,    md_div,  y, mdm_l, em_md,  nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_REM     :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_x,    md_rem,  y, mdm_u, em_md,  nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_DIVU    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_x,    md_divu, y, mdm_l, em_md,  nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };
      `RISCV_INST_MSG_REMU    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_rdat,  y,  alu_x,    md_remu, y, mdm_u, em_md,  nr,  ml_x, dmm_x,  wm_alu, y,  rd1, n   };

      `RISCV_INST_MSG_CSRW    :cs1={ y,  n,    br_none, pm_p,   am_rdat, y,  bm_0,     y,  alu_add,  md_x,    n, mdm_x, em_alu, nr,  ml_x, dmm_x,  wm_alu, n,  r0, y   };

    endcase

  end

  //----------------------------------------------------------------------
  // Instruction Decode
  //----------------------------------------------------------------------

  // Jump and Branch Controls

  wire j0_en_Dhl = cs0[`RISCV_INST_MSG_J_EN];
  wire j1_en_Dhl = cs1[`RISCV_INST_MSG_J_EN];

  wire [2:0] br0_sel_Dhl    = cs0[`RISCV_INST_MSG_BR_SEL];
  wire [2:0] br1_sel_Dhl    = cs1[`RISCV_INST_MSG_BR_SEL];

  // PC Mux Select

  wire [1:0] pc0_mux_sel_Dhl = cs0[`RISCV_INST_MSG_PC_SEL];
  wire [1:0] pc1_mux_sel_Dhl = cs1[`RISCV_INST_MSG_PC_SEL];

  // Source registers

  wire [4:0] rs00_addr_Dhl  = inst0_rs1_Dhl;
  wire [4:0] rs01_addr_Dhl  = inst0_rs2_Dhl;

  wire [4:0] rs10_addr_Dhl  = inst1_rs1_Dhl;
  wire [4:0] rs11_addr_Dhl  = inst1_rs2_Dhl;

  wire       rs00_en_Dhl    = cs0[`RISCV_INST_MSG_RS1_EN];
  wire       rs01_en_Dhl    = cs0[`RISCV_INST_MSG_RS2_EN];

  wire       rs10_en_Dhl    = cs1[`RISCV_INST_MSG_RS1_EN];
  wire       rs11_en_Dhl    = cs1[`RISCV_INST_MSG_RS2_EN];  

  // Operand Mux Select

  wire [1:0] op00_mux_sel_Dhl = cs0[`RISCV_INST_MSG_OP0_SEL];
  wire [2:0] op01_mux_sel_Dhl = cs0[`RISCV_INST_MSG_OP1_SEL];

  wire [1:0] op10_mux_sel_Dhl = cs1[`RISCV_INST_MSG_OP0_SEL];
  wire [2:0] op11_mux_sel_Dhl = cs1[`RISCV_INST_MSG_OP1_SEL];

  // ALU Function

  wire [3:0] alu0_fn_Dhl = cs0[`RISCV_INST_MSG_ALU_FN];
  wire [3:0] alu1_fn_Dhl = cs1[`RISCV_INST_MSG_ALU_FN];

  // Muldiv Function

  wire [2:0] muldivreq0_msg_fn_Dhl = cs0[`RISCV_INST_MSG_MULDIV_FN];
  wire [2:0] muldivreq1_msg_fn_Dhl = cs1[`RISCV_INST_MSG_MULDIV_FN];

  // Muldiv Controls

  wire muldivreq0_val_Dhl = cs0[`RISCV_INST_MSG_MULDIV_EN];
  wire muldivreq1_val_Dhl = cs1[`RISCV_INST_MSG_MULDIV_EN];

  // Muldiv Mux Select

  wire muldiv0_mux_sel_Dhl = cs0[`RISCV_INST_MSG_MULDIV_SEL];
  wire muldiv1_mux_sel_Dhl = cs1[`RISCV_INST_MSG_MULDIV_SEL];

  // Execute Mux Select

  wire execute0_mux_sel_Dhl = cs0[`RISCV_INST_MSG_MULDIV_EN];
  wire execute1_mux_sel_Dhl = cs1[`RISCV_INST_MSG_MULDIV_EN];

  wire       dmemreq0_msg_rw_Dhl  = ( cs0[`RISCV_INST_MSG_MEM_REQ] == st );
  wire [1:0] dmemreq0_msg_len_Dhl = cs0[`RISCV_INST_MSG_MEM_LEN];
  wire       dmemreq0_val_Dhl     = ( cs0[`RISCV_INST_MSG_MEM_REQ] != nr );

  wire       dmemreq1_msg_rw_Dhl  = ( cs1[`RISCV_INST_MSG_MEM_REQ] == st );
  wire [1:0] dmemreq1_msg_len_Dhl = cs1[`RISCV_INST_MSG_MEM_LEN];
  wire       dmemreq1_val_Dhl     = ( cs1[`RISCV_INST_MSG_MEM_REQ] != nr );

  // Memory response mux select

  wire [2:0] dmemresp0_mux_sel_Dhl = cs0[`RISCV_INST_MSG_MEM_SEL];
  wire [2:0] dmemresp1_mux_sel_Dhl = cs1[`RISCV_INST_MSG_MEM_SEL];

  // Writeback Mux Select

  wire memex0_mux_sel_Dhl = cs0[`RISCV_INST_MSG_WB_SEL];
  wire memex1_mux_sel_Dhl = cs1[`RISCV_INST_MSG_WB_SEL];

  // Register Writeback Controls

  wire rf0_wen_Dhl         = cs0[`RISCV_INST_MSG_RF_WEN];
  wire [4:0] rf0_waddr_Dhl = cs0[`RISCV_INST_MSG_RF_WADDR];

  wire rf1_wen_Dhl         = cs1[`RISCV_INST_MSG_RF_WEN];
  wire [4:0] rf1_waddr_Dhl = cs1[`RISCV_INST_MSG_RF_WADDR];

  // CSR register write enable

  wire csr0_wen_Dhl = cs0[`RISCV_INST_MSG_CSR_WEN];
  wire csr1_wen_Dhl = cs1[`RISCV_INST_MSG_CSR_WEN];

  // CSR register address

  wire [11:0] csr0_addr_Dhl  = ir0_Dhl[31:20];
  wire [11:0] csr1_addr_Dhl  = ir1_Dhl[31:20];

  //----------------------------------------------------------------------
  // Decode Stage: Reorder Buffer Logic
  //----------------------------------------------------------------------

  wire rob_req_rdy_Dhl;

  wire src00_renamed;
  wire src01_renamed;
  wire src10_renamed;
  wire src11_renamed;

  wire [4:0] src00_slot;
  wire [4:0] src01_slot;
  wire [4:0] src10_slot;
  wire [4:0] src11_slot;

  wire [4:0] rob_resp_slot_1;
  wire [4:0] rob_resp_slot_2;

  wire rob_commit_val_1;
  wire rob_commit_val_2;

  wire raw_hazard0_Dhl = ( inst_val_Dhl && rf0_wen_Dhl )
                       && ( ( rf0_waddr_Dhl == rs10_addr_Dhl ) && rs10_en_Dhl );

  wire raw_hazard1_Dhl = ( inst_val_Dhl && rf0_wen_Dhl )
                       && ( ( rf0_waddr_Dhl == rs11_addr_Dhl ) && rs11_en_Dhl );

  // Logic to mark instructions as speculative
  reg spec_Dhl;
  always @(posedge clk) begin
    if( reset ) begin
      spec_Dhl <= 1'b0;
    end
    else begin
      // Set the speculative bit to 1 on the next cycle if a branch is in D,
      // and the decode stage is not stalled
      if( inst_val_Dhl &&
        ( (br0_sel_Dhl && !squash_ir0_Dhl && !stall_agg_Dhl) ||
          (br1_sel_Dhl && !stall_Dhl) )
        ) begin
        spec_Dhl <= 1'b1;
      end
    end
  end
  always @(*) begin
    // The speculative bit should remain at 1 until the branch is resolved
    if( inst_val_X0hl && brj_resolved_X0hl ) begin
      spec_Dhl <= 1'b0;
    end
  end

  // Handle the case of two branches in D at the same time
  //    When D can issue, issue the first branch and squash it afterwards
  //    Later allocate rob entry and issue the second instruction only
  //    Senc information as to whether either instruction is squashed to I stage
  reg squash_ir0_Dhl;
  wire double_br_Dhl = br0_sel_Dhl && br1_sel_Dhl &&
                      !squash_ir0_Dhl && inst_val_Dhl && !stall_agg_Dhl;

  // Allocate the first entry if I isn't stalled and the instructions are valid.
  // Allocate the second entry if the first is not a jump, and if it is not a
  //    double branch case.
  wire rob_req_val_1_Dhl = inst_val_Dhl && !stall_agg_Dhl && !squash_ir0_Dhl;
  wire rob_req_val_2_Dhl = inst_val_Dhl && !stall_Dhl && !j0_en_Dhl && !double_br_Dhl;

  wire rob_req_spec_1_Dhl = spec_Dhl && !(brj_resolved_X0hl);
  wire rob_req_spec_2_Dhl = (spec_Dhl || (br0_sel_Dhl != br_none && !squash_ir0_Dhl))&& !(brj_resolved_X0hl);

  reg rs00_renamed_Dhl;
  reg rs01_renamed_Dhl;
  reg rs10_renamed_Dhl;
  reg rs11_renamed_Dhl;

  reg [4:0] rs00_rt_slot_Dhl;
  reg [4:0] rs01_rt_slot_Dhl;
  reg [4:0] rs10_rt_slot_Dhl;
  reg [4:0] rs11_rt_slot_Dhl;

  reg [4:0] rob_fill_slot_0_Dhl;
  reg [4:0] rob_fill_slot_1_Dhl;

  always @(*) begin
    rs00_renamed_Dhl = src00_renamed;
    rs01_renamed_Dhl = src01_renamed;
    rs10_renamed_Dhl = src10_renamed;
    rs11_renamed_Dhl = src11_renamed;

    rs00_rt_slot_Dhl = src00_slot;
    rs01_rt_slot_Dhl = src01_slot;
    rs10_rt_slot_Dhl = src10_slot;
    rs11_rt_slot_Dhl = src11_slot;

    rob_fill_slot_0_Dhl = rob_resp_slot_1;
    rob_fill_slot_1_Dhl = rob_resp_slot_2;
  end

  riscv_CoreReorderBuffer rob
  (
    .clk                         (clk),
    .reset                       (reset),

    .brj_taken_X0hl              (brj_taken_X0hl),
    .brj_resolved_X0hl           (brj_resolved_X0hl),

    .rob_alloc_req_rdy           (rob_req_rdy_Dhl),

    .rob_alloc_req_val_1         (rob_req_val_1_Dhl),
    .rob_alloc_req_wen_1         (rf0_wen_Dhl),
    .rob_alloc_req_spec_1        (rob_req_spec_1_Dhl),
    .rob_alloc_req_preg_1        (rf0_waddr_Dhl),
    .rob_fill_val_1              (rob_fill_val_A_Whl),
    .rob_fill_slot_1             (rob_fill_slot_A_Whl),
    .rob_commit_slot_1           (rob_commit_slot_1_Chl),
    .rob_commit_wen_1            (rob_commit_wen_1_Chl),
    .rob_commit_rf_waddr_1       (rob_commit_waddr_1_Chl),
    .rob_commit_val_1            (rob_commit_val_1),
    .rob_commit_spec_1           (rob_commit_spec_1),

    .rob_alloc_req_val_2         (rob_req_val_2_Dhl),
    .rob_alloc_req_wen_2         (rf1_wen_Dhl),
    .rob_alloc_req_spec_2        (rob_req_spec_2_Dhl),
    .rob_alloc_req_preg_2        (rf1_waddr_Dhl),
    .rob_fill_val_2              (rob_fill_val_B_Whl),
    .rob_fill_slot_2             (rob_fill_slot_B_Whl),
    .rob_commit_slot_2           (rob_commit_slot_2_Chl),
    .rob_commit_wen_2            (rob_commit_wen_2_Chl),
    .rob_commit_rf_waddr_2       (rob_commit_waddr_2_Chl),
    .rob_commit_val_2            (rob_commit_val_2),
    .rob_commit_spec_2           (rob_commit_spec_2),

    .rob_alloc_resp_slot_1       (rob_resp_slot_1),
    .rob_alloc_resp_slot_2       (rob_resp_slot_2),

    .raw_hazard0                 (raw_hazard0_Dhl),
    .raw_hazard1                 (raw_hazard1_Dhl),

    .src00                       (rs00_addr_Dhl),
    .src01                       (rs01_addr_Dhl),
    .src10                       (rs10_addr_Dhl),
    .src11                       (rs11_addr_Dhl),

    .src00_renamed               (src00_renamed),
    .src01_renamed               (src01_renamed),
    .src10_renamed               (src10_renamed),
    .src11_renamed               (src11_renamed),

    .src00_slot                  (src00_slot),
    .src01_slot                  (src01_slot),
    .src10_slot                  (src10_slot),
    .src11_slot                  (src11_slot)
  );

  //----------------------------------------------------------------------
  // Decode Stage: Stall Logic
  //----------------------------------------------------------------------

  // Squash instruction in D stage if branch taken for a valid
  // instruction or if there was an exception in X stage

  wire squash_Dhl
    = ( inst_val_Ihl && brj_taken_Ihl )
   || ( inst_val_X0hl && brj_taken_X0hl )
   || iq_contains_jmp;

  // Stall in D if the ROB is full, if a second branch is seen before the first
  // resolved, or if I is stalled
  wire stall_rob_Dhl = !rob_req_rdy_Dhl;

  wire stall_br_Dhl = spec_Dhl && (br0_sel_Dhl || br1_sel_Dhl);

  wire stall_agg_Dhl = !iq_enqueue_rdy || ( (stall_rob_Dhl || stall_br_Dhl) && inst_val_Dhl);

  // if we are doing a store, then we stall if it is speculative (can't do this!)
  wire stall_spec_st = spec_Dhl && (cs0[`RISCV_INST_MSG_MEM_REQ] ==  st || cs1[`RISCV_INST_MSG_MEM_REQ] == st);
  wire stall_Dhl = stall_agg_Dhl || double_br_Dhl || stall_spec_st;

  // Next bubble bit
  // Send a bubble bit if no ROB entries are allocated
  wire bubble_sel_Dhl  = ( !rob_req_val_1_Dhl && !rob_req_val_2_Dhl );
  wire bubble_next_Dhl = ( !bubble_sel_Dhl ) ? bubble_Dhl
                       : ( bubble_sel_Dhl )  ? 1'b1
                       :                       1'bx;

  //----------------------------------------------------------------------
  // IQ <- D
  //----------------------------------------------------------------------
  wire iq_enqueue_val0 = rob_req_val_1_Dhl;//inst_val_Dhl && !squash_ir0_Dhl && !stall_Dhl;
  wire iq_enqueue_val1 = rob_req_val_2_Dhl;//inst_val_Dhl && !squash_Dhl && !stall_Dhl;
  wire iq_enqueue_spec0= rob_req_spec_1_Dhl;
  wire iq_enqueue_spec1= rob_req_spec_2_Dhl;
  wire iq_dequeue_rdy0 = !stall_Ihl;
  wire iq_dequeue_rdy1 = !stall_Ihl;
  wire iq_enqueue_rdy;
  wire dequeue_val0;
  wire dequeue_val1;
  wire iq_contains_jmp;
  riscv_CoreIssueQueue issue_queue(
    .clk (clk),
    .reset (reset),

    .brj_taken_X0hl (brj_taken_X0hl),
    .brj_resolved_X0hl (brj_resolved_X0hl),
    .squash_first_I_inst_Ihl (squash_first_I_inst_Ihl),
    .iq_contains_jmp    (iq_contains_jmp),

    .iq_enqueue_val0    (iq_enqueue_val0),
    .iq_enqueue_val1    (iq_enqueue_val1),
    .iq_enqueue_rdy     (iq_enqueue_rdy),

    .iq_enqueue_spec0   (iq_enqueue_spec0),
    .iq_enqueue_spec1   (iq_enqueue_spec1),
    .iq_dequeue_rdy0    (iq_dequeue_rdy0),
    .iq_dequeue_rdy1    (iq_dequeue_rdy1),
    .iq_dequeue_val0    (dequeue_val0),
    .iq_dequeue_val1    (dequeue_val1),
    .iq_enqueue_slot0   (iq_slot_A_Dhl),
    .iq_enqueue_slot1   (iq_slot_B_Dhl),
    .iq_dequeue_slot0   (iq_slot_A_Ihl),
    .iq_dequeue_slot1   (iq_slot_B_Ihl),
    .sb_src_ready (sb_src_ready),

    
    .iq_ir0_Dhl (ir0_Dhl),
    .iq_ir0_squashed_Dhl (!rob_req_val_1_Dhl),
    .iq_rf0_wen_Dhl (rf0_wen_Dhl),
    .iq_rf0_waddr_Dhl (rf0_waddr_Dhl),
    .iq_alu0_fn_Dhl (alu0_fn_Dhl),
    .iq_br0_sel_Dhl (br0_sel_Dhl),
    .iq_j0_en_Dhl (j0_en_Dhl),
    .iq_pc0_mux_sel_Dhl (pc0_mux_sel_Dhl),
    .iq_muldivreq0_val_Dhl (muldivreq0_val_Dhl),
    .iq_muldivreq0_msg_fn_Dhl (muldivreq0_msg_fn_Dhl),
    .iq_muldiv0_mux_sel_Dhl (muldiv0_mux_sel_Dhl),
    .iq_execute0_mux_sel_Dhl (execute0_mux_sel_Dhl),
    .iq_dmemreq0_msg_rw_Dhl (dmemreq0_msg_rw_Dhl),
    .iq_dmemreq0_msg_len_Dhl (dmemreq0_msg_len_Dhl),
    .iq_dmemreq0_val_Dhl (dmemreq0_val_Dhl),
    .iq_dmemresp0_mux_sel_Dhl (dmemresp0_mux_sel_Dhl),
    .iq_memex0_mux_sel_Dhl (memex0_mux_sel_Dhl),
    .iq_csr0_wen_Dhl (csr0_wen_Dhl),
    .iq_csr0_addr_Dhl (csr0_addr_Dhl),
    .iq_ir1_Dhl (ir1_Dhl),

    .iq_ir1_squashed_Dhl (!rob_req_val_2_Dhl),
    .iq_rf1_wen_Dhl (rf1_wen_Dhl),
    .iq_rf1_waddr_Dhl (rf1_waddr_Dhl),
    .iq_alu1_fn_Dhl (alu1_fn_Dhl),
    .iq_br1_sel_Dhl (br1_sel_Dhl),
    .iq_j1_en_Dhl (j1_en_Dhl),
    .iq_pc1_mux_sel_Dhl (pc1_mux_sel_Dhl),
    .iq_muldivreq1_val_Dhl (muldivreq1_val_Dhl),
    .iq_muldivreq1_msg_fn_Dhl (muldivreq1_msg_fn_Dhl),
    .iq_muldiv1_mux_sel_Dhl (muldiv1_mux_sel_Dhl),
    .iq_execute1_mux_sel_Dhl (execute1_mux_sel_Dhl),
    .iq_dmemreq1_msg_rw_Dhl (dmemreq1_msg_rw_Dhl),
    .iq_dmemreq1_msg_len_Dhl (dmemreq1_msg_len_Dhl),
    .iq_dmemreq1_val_Dhl (dmemreq1_val_Dhl),
    .iq_dmemresp1_mux_sel_Dhl (dmemresp1_mux_sel_Dhl),
    .iq_memex1_mux_sel_Dhl (memex1_mux_sel_Dhl),
    .iq_csr1_wen_Dhl (csr1_wen_Dhl),
    .iq_csr1_addr_Dhl (csr1_addr_Dhl),
    .iq_rs00_addr_Dhl (rs00_addr_Dhl),
    .iq_rs01_addr_Dhl (rs01_addr_Dhl),
    .iq_rs10_addr_Dhl (rs10_addr_Dhl),
    .iq_rs11_addr_Dhl (rs11_addr_Dhl),
    .iq_rs00_en_Dhl (rs00_en_Dhl),
    .iq_rs01_en_Dhl (rs01_en_Dhl),
    .iq_rs10_en_Dhl (rs10_en_Dhl),
    .iq_rs11_en_Dhl (rs11_en_Dhl),
    .iq_rob_fill_slot_0_Dhl (rob_fill_slot_0_Dhl),
    .iq_rob_fill_slot_1_Dhl (rob_fill_slot_1_Dhl),
    .iq_rs00_renamed_Dhl (rs00_renamed_Dhl),
    .iq_rs01_renamed_Dhl (rs01_renamed_Dhl),
    .iq_rs10_renamed_Dhl (rs10_renamed_Dhl),
    .iq_rs11_renamed_Dhl (rs11_renamed_Dhl),
    .iq_rs00_rt_slot_Dhl (rs00_rt_slot_Dhl),
    .iq_rs01_rt_slot_Dhl (rs01_rt_slot_Dhl),
    .iq_rs10_rt_slot_Dhl (rs10_rt_slot_Dhl),
    .iq_rs11_rt_slot_Dhl (rs11_rt_slot_Dhl),
    .iq_op00_mux_sel_Dhl (op00_mux_sel_Dhl),
    .iq_op01_mux_sel_Dhl (op01_mux_sel_Dhl),
    .iq_op10_mux_sel_Dhl (op10_mux_sel_Dhl),
    .iq_op11_mux_sel_Dhl (op11_mux_sel_Dhl),

    //outputs
    
    .iq_ir0_Ihl  (ir0_Ihl),
    .iq_ir0_squashed_Ihl (ir0_squashed_Ihl),
    .iq_rf0_wen_Ihl  (rf0_wen_Ihl),
    .iq_rf0_waddr_Ihl  (rf0_waddr_Ihl),
    .iq_alu0_fn_Ihl  (alu0_fn_Ihl),
    .iq_br0_sel_Ihl  (br0_sel_Ihl),
    .iq_j0_en_Ihl  (j0_en_Ihl),
    .iq_pc0_mux_sel_Ihl  (pc0_mux_sel_Ihl),
    .iq_muldivreq0_val_Ihl (muldivreq0_val_Ihl),
    .iq_muldivreq0_msg_fn_Ihl  (muldivreq0_msg_fn_Ihl),
    .iq_muldiv0_mux_sel_Ihl  (muldiv0_mux_sel_Ihl),
    .iq_execute0_mux_sel_Ihl (execute0_mux_sel_Ihl),
    .iq_dmemreq0_msg_rw_Ihl  (dmemreq0_msg_rw_Ihl),
    .iq_dmemreq0_msg_len_Ihl (dmemreq0_msg_len_Ihl),
    .iq_dmemreq0_val_Ihl (dmemreq0_val_Ihl),
    .iq_dmemresp0_mux_sel_Ihl  (dmemresp0_mux_sel_Ihl),
    .iq_memex0_mux_sel_Ihl (memex0_mux_sel_Ihl),
    .iq_csr0_wen_Ihl (csr0_wen_Ihl),
    .iq_csr0_addr_Ihl  (csr0_addr_Ihl),

    .iq_ir1_Ihl  (ir1_Ihl),
    .iq_ir1_squashed_Ihl (ir1_squashed_Ihl),
    .iq_rf1_wen_Ihl  (rf1_wen_Ihl),
    .iq_rf1_waddr_Ihl  (rf1_waddr_Ihl),
    .iq_alu1_fn_Ihl  (alu1_fn_Ihl),
    .iq_br1_sel_Ihl  (br1_sel_Ihl),
    .iq_j1_en_Ihl  (j1_en_Ihl),
    .iq_pc1_mux_sel_Ihl  (pc1_mux_sel_Ihl),
    .iq_muldivreq1_val_Ihl (muldivreq1_val_Ihl),
    .iq_muldivreq1_msg_fn_Ihl  (muldivreq1_msg_fn_Ihl),
    .iq_muldiv1_mux_sel_Ihl  (muldiv1_mux_sel_Ihl),
    .iq_execute1_mux_sel_Ihl (execute1_mux_sel_Ihl),
    .iq_dmemreq1_msg_rw_Ihl  (dmemreq1_msg_rw_Ihl),
    .iq_dmemreq1_msg_len_Ihl (dmemreq1_msg_len_Ihl),
    .iq_dmemreq1_val_Ihl (dmemreq1_val_Ihl),
    .iq_dmemresp1_mux_sel_Ihl  (dmemresp1_mux_sel_Ihl),
    .iq_memex1_mux_sel_Ihl (memex1_mux_sel_Ihl),
    .iq_csr1_wen_Ihl (csr1_wen_Ihl),
    .iq_csr1_addr_Ihl  (csr1_addr_Ihl),
    .iq_rs00_addr_Ihl  (rs00_addr_Ihl),
    .iq_rs01_addr_Ihl  (rs01_addr_Ihl),
    .iq_rs10_addr_Ihl  (rs10_addr_Ihl),
    .iq_rs11_addr_Ihl  (rs11_addr_Ihl),
    .iq_rs00_en_Ihl  (rs00_en_Ihl),
    .iq_rs01_en_Ihl  (rs01_en_Ihl),
    .iq_rs10_en_Ihl  (rs10_en_Ihl),
    .iq_rs11_en_Ihl  (rs11_en_Ihl),
    .iq_rob_fill_slot_0_Ihl  (rob_fill_slot_0_Ihl),
    .iq_rob_fill_slot_1_Ihl  (rob_fill_slot_1_Ihl),
    .iq_rs00_renamed_Ihl (rs00_renamed_Ihl),
    .iq_rs01_renamed_Ihl (rs01_renamed_Ihl),
    .iq_rs10_renamed_Ihl (rs10_renamed_Ihl),
    .iq_rs11_renamed_Ihl (rs11_renamed_Ihl),
    .iq_rs00_rt_slot_Ihl (rs00_rt_slot_Ihl),
    .iq_rs01_rt_slot_Ihl (rs01_rt_slot_Ihl),
    .iq_rs10_rt_slot_Ihl (rs10_rt_slot_Ihl),
    .iq_rs11_rt_slot_Ihl (rs11_rt_slot_Ihl),
    .iq_op00_mux_sel_Ihl (op00_mux_sel_Ihl),
    .iq_op01_mux_sel_Ihl (op01_mux_sel_Ihl),
    .iq_op10_mux_sel_Ihl (op10_mux_sel_Ihl),
    .iq_op11_mux_sel_Ihl (op11_mux_sel_Ihl)
  );
  //----------------------------------------------------------------------
  // I <- D
  //----------------------------------------------------------------------

  reg         bubble_Ihl;

  wire [31:0]  ir0_Ihl;
  wire         ir0_squashed_Ihl;
  wire         rf0_wen_Ihl;
  wire  [4:0]  rf0_waddr_Ihl;
  wire  [3:0]  alu0_fn_Ihl;
  wire  [2:0]  br0_sel_Ihl;
  wire         j0_en_Ihl;
  wire  [1:0]  pc0_mux_sel_Ihl;
  wire         muldivreq0_val_Ihl;
  wire  [2:0]  muldivreq0_msg_fn_Ihl;
  wire         muldiv0_mux_sel_Ihl;
  wire         execute0_mux_sel_Ihl;
  wire         dmemreq0_msg_rw_Ihl;
  wire  [1:0]  dmemreq0_msg_len_Ihl;
  wire         dmemreq0_val_Ihl;
  wire  [2:0]  dmemresp0_mux_sel_Ihl;
  wire         memex0_mux_sel_Ihl;
  wire         csr0_wen_Ihl;
  wire [11:0]  csr0_addr_Ihl;

  wire [31:0]  ir1_Ihl;
  wire         ir1_squashed_Ihl;
  wire         rf1_wen_Ihl;
  wire  [4:0]  rf1_waddr_Ihl;
  wire  [3:0]  alu1_fn_Ihl;
  wire  [2:0]  br1_sel_Ihl;
  wire         j1_en_Ihl;
  wire  [1:0]  pc1_mux_sel_Ihl;
  wire         muldivreq1_val_Ihl;
  wire  [2:0]  muldivreq1_msg_fn_Ihl;
  wire         muldiv1_mux_sel_Ihl;
  wire         execute1_mux_sel_Ihl;
  wire         dmemreq1_msg_rw_Ihl;
  wire  [1:0]  dmemreq1_msg_len_Ihl;
  wire         dmemreq1_val_Ihl;
  wire  [2:0]  dmemresp1_mux_sel_Ihl;
  wire         memex1_mux_sel_Ihl;
  wire         csr1_wen_Ihl;
  wire [11:0]  csr1_addr_Ihl;

  wire  [4:0]  rs00_addr_Ihl;
  wire  [4:0]  rs01_addr_Ihl;
  wire  [4:0]  rs10_addr_Ihl;
  wire  [4:0]  rs11_addr_Ihl;
  wire         rs00_en_Ihl;
  wire         rs01_en_Ihl;
  wire         rs10_en_Ihl;
  wire         rs11_en_Ihl;
  wire  [4:0]  rob_fill_slot_0_Ihl;
  wire  [4:0]  rob_fill_slot_1_Ihl;
  wire         rob_fill_val_0_Ihl;
  wire         rob_fill_val_1_Ihl;

  wire         rs00_renamed_Ihl;
  wire         rs01_renamed_Ihl;
  wire         rs10_renamed_Ihl;
  wire         rs11_renamed_Ihl;
  wire  [4:0]  rs00_rt_slot_Ihl;
  wire  [4:0]  rs01_rt_slot_Ihl;
  wire  [4:0]  rs10_rt_slot_Ihl;
  wire  [4:0]  rs11_rt_slot_Ihl;

  wire  [1:0]  op00_mux_sel_Ihl;
  wire  [2:0]  op01_mux_sel_Ihl;
  wire  [1:0]  op10_mux_sel_Ihl;
  wire  [2:0]  op11_mux_sel_Ihl;

  always @ (posedge clk) begin
    if( reset ) begin
      bubble_Ihl      <= 1'b1;
      squash_ir0_Dhl  <= 1'b0;
    end
    else if( !stall_Ihl ) begin
      bubble_Ihl <= 1'b0;//bubble_next_Dhl;

      if( double_br_Dhl ) begin
        squash_ir0_Dhl  <= 1'b1;
      end
      else if( !stall_Dhl ) begin
        squash_ir0_Dhl  <= 1'b0;
      end
    end
  end

  // Is the current stage valid?

  wire inst_val_Ihl = ( !bubble_Ihl && !squash_Ihl );

  // PC Mux Select

  wire [1:0] pc_mux_sel_Ihl  = ( !steering_mux_sel_Ihl ) ? pc0_mux_sel_Ihl
                             : (  steering_mux_sel_Ihl ) ? pc1_mux_sel_Ihl
                             :                           2'bx;

  //----------------------------------------------------------------------
  // Issue Stage: Jump and Branch Controls
  //----------------------------------------------------------------------

  wire       brj0_taken_Ihl = ( inst_val_Ihl && j0_en_Ihl );
  wire       brj1_taken_Ihl = ( inst_val_Ihl && j1_en_Ihl );

  wire       brj_taken_Ihl  = ( !steering_mux_sel_Ihl ) ? brj0_taken_Ihl && ir0_issued_Ihl
                            : (  steering_mux_sel_Ihl ) ? brj1_taken_Ihl && ir1_issued_Ihl
                            :                           1'bx;

  //----------------------------------------------------------------------
  // Issue Stage: Steering Logic
  //----------------------------------------------------------------------

  reg [31:0] irA_Ihl;
  reg        rfA_wen_Ihl;
  reg  [4:0] rfA_waddr_Ihl;
  reg  [3:0] aluA_fn_Ihl;
  reg  [4:0] rob_fill_slot_A_Ihl;
  reg        rob_fill_val_A_Ihl;
  

  reg [31:0] irB_Ihl;
  reg        rfB_wen_Ihl;
  reg  [4:0] rfB_waddr_Ihl;
  reg  [3:0] aluB_fn_Ihl;
  reg  [4:0] rob_fill_slot_B_Ihl;
  reg        rob_fill_val_B_Ihl;

  reg  [2:0] br_sel_Ihl;
  reg        muldivreq_val_Ihl;
  reg  [2:0] muldivreq_msg_fn_Ihl;
  reg        muldiv_mux_sel_Ihl;
  reg        execute_mux_sel_Ihl;
  reg        dmemreq_msg_rw_Ihl;
  reg  [1:0] dmemreq_msg_len_Ihl;
  reg        dmemreq_val_Ihl;
  reg  [2:0] dmemresp_mux_sel_Ihl;
  reg        memex_mux_sel_Ihl;
  reg        csr_wen_Ihl;
  reg [11:0] csr_addr_Ihl;

  reg        rsA0_renamed_Ihl;
  reg        rsA1_renamed_Ihl;
  reg  [4:0] rsA0_rt_slot_Ihl;
  reg  [4:0] rsA1_rt_slot_Ihl;
  reg        rsA0_en_Ihl;
  reg        rsA1_en_Ihl;

  reg        rsB0_renamed_Ihl;
  reg        rsB1_renamed_Ihl;
  reg  [4:0] rsB0_rt_slot_Ihl;
  reg  [4:0] rsB1_rt_slot_Ihl;
  reg        rsB0_en_Ihl;
  reg        rsB1_en_Ihl;


  // For disassembly in dpath

  assign instA_Ihl            = irA_Ihl;
  assign instB_Ihl            = irB_Ihl;

  // Logic to stall the second instruction if there is a hazard
  // A RAW hazard exists if instruction 0 writes the register file
  // and the write destination is the same as a source register being
  // used for instruction 1.

  wire ir0_squash_total_Ihl = squash_first_I_inst_Ihl || ir0_squashed_Ihl;

  wire stall_1_RAW0_Ihl        = ( inst_val_Ihl && rf0_wen_Ihl )
                             && ( ( rf0_waddr_Ihl == rs10_addr_Ihl ) && rs10_en_Ihl )
                             && ( !ir0_squash_total_Ihl )
                             && ( !ir1_squashed_Ihl );

  wire stall_1_RAW1_Ihl        = ( inst_val_Ihl && rf0_wen_Ihl )
                             && ( ( rf0_waddr_Ihl == rs11_addr_Ihl ) && rs11_en_Ihl )
                             && ( !ir0_squash_total_Ihl )
                             && ( !ir1_squashed_Ihl );

  // A structural hazard exists if both instructions are not simple
  // ALU instructions. Alternatively, both instructions are loads, stores,
  // muldivs, jumps, branches, or CSRWs.

  wire reqA_0_Ihl             = ( inst_val_Ihl && !squash_first_I_inst_Ihl )
                             && ( muldivreq0_val_Ihl
                               || dmemreq0_val_Ihl
                               || brj0_taken_Ihl
                               || ( br0_sel_Ihl != br_none )
                               || csr0_wen_Ihl
                                );

  wire reqA_1_Ihl             = ( inst_val_Ihl )
                             && ( muldivreq1_val_Ihl
                               || dmemreq1_val_Ihl
                               || brj1_taken_Ihl
                               || ( br1_sel_Ihl != br_none )
                               || csr1_wen_Ihl
                                );

  wire stall_1_structural_Ihl = reqA_0_Ihl && reqA_1_Ihl;

  // Aggregate hazards for steering instruction 1

  wire stall_1_steer_Ihl = ( stall_1_RAW0_Ihl || stall_1_RAW1_Ihl || stall_1_structural_Ihl );

  // Signals to indicate when both instructions are issued, or if only
  // instruction 0 was issued
  reg squash_first_I_inst_Ihl;
  reg ir0_issued_Ihl = 1'b0;
  reg ir1_issued_Ihl = 1'b0;

  // Steering signal calculation
  // A value of 0 indicates instruction 0 being steered to A
  // A value of 1 indicates instruction 1 being steered to A
  reg steer_signal_Ihl;
  assign steering_mux_sel_Ihl = steer_signal_Ihl;

  always @(*) begin

    // If the first instruction was already issued or squashed, steer the second
    // into pipeline A.
    if( ir0_squash_total_Ihl ) begin
      steer_signal_Ihl = 1'b1;
    end

    // If the second instruction requires A, and the first does not,
    // steer the second to A.
    else if ( !reqA_0_Ihl && reqA_1_Ihl && !stall_1_Ihl && !ir1_squashed_Ihl ) begin
      steer_signal_Ihl = 1'b1;
    end

    // Steer the first instruction to A in all other cases
    else begin
      steer_signal_Ihl = 1'b0;
    end
  end

  // Steer operand and mux select signals
  assign opA0_mux_sel_Ihl
    = ( steering_mux_sel_Ihl == 1'b0 ) ? op00_mux_sel_Ihl
    : ( steering_mux_sel_Ihl == 1'b1 ) ? op10_mux_sel_Ihl
    :                                    2'bx;
  assign opA1_mux_sel_Ihl
    = ( steering_mux_sel_Ihl == 1'b0 ) ? op01_mux_sel_Ihl
    : ( steering_mux_sel_Ihl == 1'b1 ) ? op11_mux_sel_Ihl
    :                                    2'bx;
  assign opB0_mux_sel_Ihl
    = ( steering_mux_sel_Ihl == 1'b0 ) ? op10_mux_sel_Ihl
    : ( steering_mux_sel_Ihl == 1'b1 ) ? op00_mux_sel_Ihl
    :                                    2'bx;
  assign opB1_mux_sel_Ihl
    = ( steering_mux_sel_Ihl == 1'b0 ) ? op11_mux_sel_Ihl
    : ( steering_mux_sel_Ihl == 1'b1 ) ? op01_mux_sel_Ihl
    :                                    2'bx;
  assign opA0_byp_rob_slot_Ihl
    = ( steering_mux_sel_Ihl == 1'b0 ) ? rs00_rt_slot_Ihl
    : ( steering_mux_sel_Ihl == 1'b1 ) ? rs10_rt_slot_Ihl
    :                                    5'bx;
  assign opA1_byp_rob_slot_Ihl
    = ( steering_mux_sel_Ihl == 1'b0 ) ? rs01_rt_slot_Ihl
    : ( steering_mux_sel_Ihl == 1'b1 ) ? rs11_rt_slot_Ihl
    :                                    5'bx;
  assign opB0_byp_rob_slot_Ihl
    = ( steering_mux_sel_Ihl == 1'b0 ) ? rs10_rt_slot_Ihl
    : ( steering_mux_sel_Ihl == 1'b1 ) ? rs00_rt_slot_Ihl
    :                                    5'bx;
  assign opB1_byp_rob_slot_Ihl
    = ( steering_mux_sel_Ihl == 1'b0 ) ? rs11_rt_slot_Ihl
    : ( steering_mux_sel_Ihl == 1'b1 ) ? rs01_rt_slot_Ihl
    :                                    5'bx;

  always @(*) begin
    if ( steering_mux_sel_Ihl == 1'b0 ) begin

      // Issue the first instruction (ir0) to A
      irA_Ihl              = ir0_Ihl;
      rfA_wen_Ihl          = rf0_wen_Ihl;
      rfA_waddr_Ihl        = rf0_waddr_Ihl;
      aluA_fn_Ihl          = alu0_fn_Ihl;
      rob_fill_slot_A_Ihl  = rob_fill_slot_0_Ihl;

      br_sel_Ihl           = br0_sel_Ihl;
      muldivreq_val_Ihl    = muldivreq0_val_Ihl;
      muldivreq_msg_fn_Ihl = muldivreq0_msg_fn_Ihl;
      muldiv_mux_sel_Ihl   = muldiv0_mux_sel_Ihl;
      execute_mux_sel_Ihl  = execute0_mux_sel_Ihl;
      dmemreq_msg_rw_Ihl   = dmemreq0_msg_rw_Ihl;
      dmemreq_msg_len_Ihl  = dmemreq0_msg_len_Ihl;
      dmemreq_val_Ihl      = dmemreq0_val_Ihl;
      dmemresp_mux_sel_Ihl = dmemresp0_mux_sel_Ihl;
      memex_mux_sel_Ihl    = memex0_mux_sel_Ihl;
      csr_wen_Ihl          = csr0_wen_Ihl;
      csr_addr_Ihl         = csr0_addr_Ihl;

      rsA0_renamed_Ihl     = rs00_renamed_Ihl;
      rsA1_renamed_Ihl     = rs01_renamed_Ihl;
      rsA0_rt_slot_Ihl     = rs00_rt_slot_Ihl;
      rsA1_rt_slot_Ihl     = rs01_rt_slot_Ihl;
      rsA0_en_Ihl          = rs00_en_Ihl;
      rsA1_en_Ihl          = rs01_en_Ihl;

      if( !stall_0_Ihl ) begin
        ir0_issued_Ihl     = 1'b1;
        rob_fill_val_A_Ihl = 1'b1;
      end
      else begin
        ir0_issued_Ihl     = 1'b0;
        rob_fill_val_A_Ihl = 1'b0;
      end

      // Issue ir1 to B if ir1, ir0, and X0 are not stalled and if
      // ir0 is not a jump instruction
      if( !stall_1_Ihl && !stall_0_Ihl && !brj_taken_Ihl && !ir1_squashed_Ihl ) begin
        irB_Ihl              = ir1_Ihl;
        rfB_wen_Ihl          = rf1_wen_Ihl;
        rfB_waddr_Ihl        = rf1_waddr_Ihl;
        aluB_fn_Ihl          = alu1_fn_Ihl;
        rob_fill_slot_B_Ihl  = rob_fill_slot_1_Ihl;
        rob_fill_val_B_Ihl   = 1'b1;

        rsB0_renamed_Ihl     = rs10_renamed_Ihl;
        rsB1_renamed_Ihl     = rs11_renamed_Ihl;
        rsB0_rt_slot_Ihl     = rs10_rt_slot_Ihl;
        rsB1_rt_slot_Ihl     = rs11_rt_slot_Ihl;
        rsB0_en_Ihl          = rs10_en_Ihl;
        rsB1_en_Ihl          = rs11_en_Ihl;

        ir1_issued_Ihl       = 1'b1;
      end

      // Send an invalid instruction down B if ir1 is stalled
      // Set ir1_issued to 0 to indicate that only ir0 has been issued
      else begin
        irB_Ihl              = 32'b0;
        rfB_wen_Ihl          = 1'b0;

        ir1_issued_Ihl       = 1'b0;
        rob_fill_val_B_Ihl   = 1'b0;
      end
    end

    else if ( steering_mux_sel_Ihl == 1'b1 ) begin

      // Issue the second instruction (ir1) to A
      irA_Ihl              = ir1_Ihl;
      rfA_wen_Ihl          = rf1_wen_Ihl;
      rfA_waddr_Ihl        = rf1_waddr_Ihl;
      aluA_fn_Ihl          = alu1_fn_Ihl;
      rob_fill_slot_A_Ihl  = rob_fill_slot_1_Ihl;

      br_sel_Ihl           = br1_sel_Ihl;
      aluA_fn_Ihl          = alu1_fn_Ihl;
      muldivreq_val_Ihl    = muldivreq1_val_Ihl;
      muldivreq_msg_fn_Ihl = muldivreq1_msg_fn_Ihl;
      muldiv_mux_sel_Ihl   = muldiv1_mux_sel_Ihl;
      execute_mux_sel_Ihl  = execute1_mux_sel_Ihl;
      dmemreq_msg_rw_Ihl   = dmemreq1_msg_rw_Ihl;
      dmemreq_msg_len_Ihl  = dmemreq1_msg_len_Ihl;
      dmemreq_val_Ihl      = dmemreq1_val_Ihl;
      dmemresp_mux_sel_Ihl = dmemresp1_mux_sel_Ihl;
      memex_mux_sel_Ihl    = memex1_mux_sel_Ihl;
      csr_wen_Ihl          = csr1_wen_Ihl;
      csr_addr_Ihl         = csr1_addr_Ihl;

      rsA0_renamed_Ihl     = rs10_renamed_Ihl;
      rsA1_renamed_Ihl     = rs11_renamed_Ihl;
      rsA0_rt_slot_Ihl     = rs10_rt_slot_Ihl;
      rsA1_rt_slot_Ihl     = rs11_rt_slot_Ihl;
      rsA0_en_Ihl          = rs10_en_Ihl;
      rsA1_en_Ihl          = rs11_en_Ihl;

      // If i1 and X0 are not stalled, both instructions will issued,
      // set ir0_issued_only to 0, and ir1_issued to 1
      if( !stall_1_Ihl ) begin
        ir1_issued_Ihl       = 1'b1;
        rob_fill_val_A_Ihl   = 1'b1;
      end
      else begin
        ir1_issued_Ihl       = 1'b0;
        rob_fill_val_A_Ihl   = 1'b0;
      end

      // If ir0 has not been issued, send it to path B
      if( !ir0_squash_total_Ihl ) begin
        irB_Ihl              = ir0_Ihl;
        rfB_wen_Ihl          = rf0_wen_Ihl;
        rfB_waddr_Ihl        = rf0_waddr_Ihl;
        aluB_fn_Ihl          = alu0_fn_Ihl;
        rob_fill_slot_B_Ihl  = rob_fill_slot_0_Ihl;
        rob_fill_val_B_Ihl   = 1'b1;

        rsB0_renamed_Ihl     = rs00_renamed_Ihl;
        rsB1_renamed_Ihl     = rs01_renamed_Ihl;
        rsB0_rt_slot_Ihl     = rs00_rt_slot_Ihl;
        rsB1_rt_slot_Ihl     = rs01_rt_slot_Ihl;
        rsB0_en_Ihl          = rs00_en_Ihl;
        rsB1_en_Ihl          = rs01_en_Ihl;

        ir0_issued_Ihl       = 1'b1;
      end

      // Otherwise send an invalid instruction down path B
      else begin
        irB_Ihl              = 32'b0;
        rfB_wen_Ihl          = 1'b0;

        ir0_issued_Ihl       = 1'b0;
        rob_fill_val_B_Ihl   = 1'b0;
      end
    end

    // Do not issue if X0 is stalled or instructions are invalid
    if ( !inst_val_Ihl || stall_X0hl ) begin
      ir0_issued_Ihl         = 1'b0;
      ir1_issued_Ihl         = 1'b0;
      rob_fill_val_A_Ihl     = 1'b0;
      rob_fill_val_B_Ihl     = 1'b0;
    end
  end

  //----------------------------------------------------------------------
  // Scoreboard Logic
  //----------------------------------------------------------------------

  wire [1:0] func_irA_Ihl = {muldivreq_val_Ihl, dmemreq_val_Ihl};

  wire A_issued_Ihl = ( !steering_mux_sel_Ihl && ir0_issued_Ihl ) ||
                      (  steering_mux_sel_Ihl && ir1_issued_Ihl );
  wire B_issued_Ihl = ( !steering_mux_sel_Ihl && ir1_issued_Ihl ) ||
                      (  steering_mux_sel_Ihl && ir0_issued_Ihl );

  wire stall_0_sb_Ihl = ( (!sb_src_ready[rs00_rt_slot_Ihl] && rs00_en_Ihl && rs00_renamed_Ihl)
                       || (!sb_src_ready[rs01_rt_slot_Ihl] && rs01_en_Ihl && rs01_renamed_Ihl) )
                       && inst_val_Ihl;
  wire stall_1_sb_Ihl = ( (!sb_src_ready[rs10_rt_slot_Ihl] && rs10_en_Ihl && rs10_renamed_Ihl)
                       || (!sb_src_ready[rs11_rt_slot_Ihl] && rs11_en_Ihl && rs11_renamed_Ihl) )
                       && inst_val_Ihl;

  wire [31:0] sb_src_ready;

  riscv_CoreScoreboard scoreboard
  (
    .clk                (clk),
    .reset              (reset),

    .srcA0              (rsA0_rt_slot_Ihl),
    .srcA0_en           (rsA0_en_Ihl),
    .srcA0_renamed      (rsA0_renamed_Ihl),
    .srcA1              (rsA1_rt_slot_Ihl),
    .srcA1_en           (rsA1_en_Ihl),
    .srcA1_renamed      (rsA1_renamed_Ihl),
    .dstA               (rob_fill_slot_A_Ihl),
    .dstA_en            (rob_fill_val_A_Ihl),
    .func_irA           (func_irA_Ihl),
    .A_issued           (A_issued_Ihl),

    .srcB0              (rsB0_rt_slot_Ihl),
    .srcB0_en           (rsB0_en_Ihl),
    .srcB0_renamed      (rsB0_renamed_Ihl),
    .srcB1              (rsB1_rt_slot_Ihl),
    .srcB1_en           (rsB1_en_Ihl),
    .srcB1_renamed      (rsB1_renamed_Ihl),
    .dstB               (rob_fill_slot_B_Ihl),
    .dstB_en            (rob_fill_val_B_Ihl),
    .B_issued           (B_issued_Ihl),

    .stall_X0hl         (stall_X0hl),
    .stall_X1hl         (stall_X1hl),
    .stall_X2hl         (stall_X2hl),
    .stall_X3hl         (stall_X3hl),
    .stall_Whl          (stall_Whl),

    .rob_commit_slot_1  (rob_commit_slot_1_Chl),
    .rob_commit_val_1   (rob_commit_val_1),
    .rob_commit_slot_2  (rob_commit_slot_2_Chl),
    .rob_commit_val_2   (rob_commit_val_2),

    .opA0_byp_mux_sel   (opA0_byp_mux_sel_Ihl),
    .opA1_byp_mux_sel   (opA1_byp_mux_sel_Ihl),
    .opB0_byp_mux_sel   (opB0_byp_mux_sel_Ihl),
    .opB1_byp_mux_sel   (opB1_byp_mux_sel_Ihl),

    .src_ready          (sb_src_ready)
  );

  //----------------------------------------------------------------------
  // Squash and Stall Logic
  //----------------------------------------------------------------------
  
  // Squash instruction in D if a valid branch in X is taken
  wire squash_Ihl = ( inst_val_X0hl && brj_taken_X0hl ) || (!dequeue_val0 && !dequeue_val1);

  // Aggregate Stall Signal
  wire stall_0_Ihl = ( stall_0_sb_Ihl && !ir0_squash_total_Ihl );

  wire stall_1_Ihl = ( stall_1_sb_Ihl || stall_1_steer_Ihl && !ir1_squashed_Ihl );

  wire stall_Ihl   = ( stall_X0hl || stall_0_Ihl || stall_1_Ihl )
                   && !brj_taken_Ihl && !brj_taken_X0hl;



  // Send a bubble to the next stage if either ir0 is valid and stalled,
  // or if ir0 is invalid and i1 is stalled
  wire bubble_sel_Ihl  = ( squash_Ihl || stall_X0hl || stall_0_Ihl
                      || ( stall_1_Ihl && squash_first_I_inst_Ihl ) );
  wire bubble_next_Ihl = ( !bubble_sel_Ihl ) ? bubble_Ihl
                       : ( bubble_sel_Ihl )  ? 1'b1
                       :                       1'bx;

  //----------------------------------------------------------------------
  // X0 <- I
  //----------------------------------------------------------------------

  reg [31:0] irA_X0hl;
  reg  [3:0] aluA_fn_X0hl;
  reg        rfA_wen_X0hl;
  reg  [4:0] rfA_waddr_X0hl;
  reg  [4:0] rob_fill_slot_A_X0hl;
  reg        rob_fill_val_A_X0hl;

  reg [31:0] irB_X0hl;
  reg  [3:0] aluB_fn_X0hl;
  reg        rfB_wen_X0hl;
  reg  [4:0] rfB_waddr_X0hl;
  reg  [4:0] rob_fill_slot_B_X0hl;
  reg        rob_fill_val_B_X0hl;

  reg  [2:0] br_sel_X0hl;
  reg        muldivreq_val_X0hl;
  reg  [2:0] muldivreq_msg_fn_X0hl;
  reg        muldiv_mux_sel_X0hl;
  reg        execute_mux_sel_X0hl;
  reg        memex_mux_sel_X0hl;
  reg        dmemreq_msg_rw_X0hl;
  reg  [1:0] dmemreq_msg_len_X0hl;
  reg        dmemreq_val_X0hl;
  reg  [2:0] dmemresp_mux_sel_X0hl;
  reg        csr_wen_X0hl;
  reg [11:0] csr_addr_X0hl;

  reg        bubble_X0hl;

  // Pipeline Controls

  always @ ( posedge clk ) begin
    if ( reset ) begin
      bubble_X0hl             <= 1'b1;
      squash_first_I_inst_Ihl <= 1'b0;
    end
    else if( !stall_X0hl ) begin
      irA_X0hl              <= irA_Ihl;
      aluA_fn_X0hl          <= aluA_fn_Ihl;
      rfA_wen_X0hl          <= rfA_wen_Ihl;
      rfA_waddr_X0hl        <= rfA_waddr_Ihl;
      rob_fill_slot_A_X0hl  <= rob_fill_slot_A_Ihl;
      rob_fill_val_A_X0hl   <= rob_fill_val_A_Ihl && !bubble_next_Ihl;

      irB_X0hl              <= irB_Ihl;
      aluB_fn_X0hl          <= aluB_fn_Ihl;
      rfB_wen_X0hl          <= rfB_wen_Ihl;
      rfB_waddr_X0hl        <= rfB_waddr_Ihl;
      rob_fill_slot_B_X0hl  <= rob_fill_slot_B_Ihl;
      rob_fill_val_B_X0hl   <= rob_fill_val_B_Ihl && !bubble_next_Ihl;

      br_sel_X0hl           <= br_sel_Ihl;
      muldivreq_val_X0hl    <= muldivreq_val_Ihl;
      muldivreq_msg_fn_X0hl <= muldivreq_msg_fn_Ihl;
      muldiv_mux_sel_X0hl   <= muldiv_mux_sel_Ihl;
      execute_mux_sel_X0hl  <= execute_mux_sel_Ihl;
      memex_mux_sel_X0hl    <= memex_mux_sel_Ihl;
      dmemreq_msg_rw_X0hl   <= dmemreq_msg_rw_Ihl;
      dmemreq_msg_len_X0hl  <= dmemreq_msg_len_Ihl;
      dmemreq_val_X0hl      <= dmemreq_val_Ihl;
      dmemresp_mux_sel_X0hl <= dmemresp_mux_sel_Ihl;
      csr_wen_X0hl          <= csr_wen_Ihl;
      csr_addr_X0hl         <= csr_addr_Ihl;

      bubble_X0hl           <= bubble_next_Ihl;

      // Squash the first I instruction if only instruction 0 is issued
      // and it is not a jump

      if( ir0_issued_Ihl && !ir1_issued_Ihl && !brj_taken_Ihl && dequeue_val1) begin
        squash_first_I_inst_Ihl <= 1'b1;
      end
      else if( ir1_issued_Ihl || brj_taken_Ihl || brj_taken_X0hl ) begin
        squash_first_I_inst_Ihl <= 1'b0;
      end
    end
  end

  //----------------------------------------------------------------------
  // Execute Stage
  //----------------------------------------------------------------------

  // Is the current stage valid?

  wire inst_val_X0hl = ( !bubble_X0hl && !squash_X0hl );

  // Muldiv request

  assign muldivreq_val = muldivreq_val_Ihl && inst_val_Ihl && (!stall_X0hl);
  assign muldivresp_rdy = !stall_X3hl;

  // Only send a valid dmem request if not stalled and not speculative

  assign dmemreq_msg_rw  = dmemreq_msg_rw_X0hl;
  assign dmemreq_msg_len = dmemreq_msg_len_X0hl;
  assign dmemreq_val     = ( inst_val_X0hl && !stall_X0hl && dmemreq_val_X0hl );

  // Resolve Branch

  wire bne_taken_X0hl  = ( ( br_sel_X0hl == br_bne ) && branch_cond_ne_X0hl );
  wire beq_taken_X0hl  = ( ( br_sel_X0hl == br_beq ) && branch_cond_eq_X0hl );
  wire blt_taken_X0hl  = ( ( br_sel_X0hl == br_blt ) && branch_cond_lt_X0hl );
  wire bltu_taken_X0hl = ( ( br_sel_X0hl == br_bltu) && branch_cond_ltu_X0hl);
  wire bge_taken_X0hl  = ( ( br_sel_X0hl == br_bge ) && branch_cond_ge_X0hl );
  wire bgeu_taken_X0hl = ( ( br_sel_X0hl == br_bgeu) && branch_cond_geu_X0hl);

  wire brj_resolved_X0hl = ( inst_val_X0hl && (br_sel_X0hl != br_none) && !stall_X0hl );


  wire any_br_taken_X0hl
    = ( beq_taken_X0hl
   ||   bne_taken_X0hl
   ||   blt_taken_X0hl
   ||   bltu_taken_X0hl
   ||   bge_taken_X0hl
   ||   bgeu_taken_X0hl );

  wire brj_taken_X0hl = ( inst_val_X0hl && any_br_taken_X0hl && !stall_X0hl );

  // Dummy Squash Signal

  wire squash_X0hl = 1'b0;

  // Dummy muldiv stall signal

  wire stall_muldiv_X0hl = 1'b0;

  // Stall in X if imem is not ready

  wire stall_imem_X0hl = !imemreq0_rdy || !imemreq1_rdy;

  // Stall in X if dmem is not ready and there was a valid request

  wire stall_dmem_X0hl = ( dmemreq_val_X0hl && inst_val_X0hl && !dmemreq_rdy );

  // Aggregate Stall Signal

  assign stall_X0hl = ( stall_X1hl || stall_muldiv_X0hl || stall_imem_X0hl || stall_dmem_X0hl );

  // Next bubble bit

  wire bubble_sel_X0hl  = ( squash_X0hl || stall_X0hl );
  wire bubble_next_X0hl = ( !bubble_sel_X0hl ) ? bubble_X0hl
                       : ( bubble_sel_X0hl )  ? 1'b1
                       :                       1'bx;

  //----------------------------------------------------------------------
  // X1 <- X0
  //----------------------------------------------------------------------

  reg [31:0] irA_X1hl;
  reg        rfA_wen_X1hl;
  reg  [4:0] rfA_waddr_X1hl;
  reg  [4:0] rob_fill_slot_A_X1hl;
  reg        rob_fill_val_A_X1hl;

  reg        dmemreq_val_X1hl;
  reg  [2:0] dmemresp_mux_sel_X1hl;
  reg        memex_mux_sel_X1hl;
  reg        execute_mux_sel_X1hl;
  reg        muldiv_mux_sel_X1hl;
  reg        csr_wen_X1hl;
  reg  [4:0] csr_addr_X1hl;

  reg        bubble_X1hl;

  // Pipeline Controls

  always @ ( posedge clk ) begin
    if ( reset ) begin
      dmemreq_val_X1hl <= 1'b0;

      bubble_X1hl <= 1'b1;
    end
    else if( !stall_X1hl ) begin
      irA_X1hl              <= irA_X0hl;
      rfA_wen_X1hl          <= rfA_wen_X0hl;
      rfA_waddr_X1hl        <= rfA_waddr_X0hl;
      rob_fill_slot_A_X1hl  <= rob_fill_slot_A_X0hl;
      rob_fill_val_A_X1hl   <= rob_fill_val_A_X0hl && !bubble_next_X0hl;

      dmemreq_val_X1hl      <= dmemreq_val;
      dmemresp_mux_sel_X1hl <= dmemresp_mux_sel_X0hl;
      memex_mux_sel_X1hl    <= memex_mux_sel_X0hl;
      execute_mux_sel_X1hl  <= execute_mux_sel_X0hl;
      muldiv_mux_sel_X1hl   <= muldiv_mux_sel_X0hl;
      csr_wen_X1hl          <= csr_wen_X0hl;
      csr_addr_X1hl         <= csr_addr_X0hl;

      bubble_X1hl           <= !(rob_fill_val_A_X0hl && !bubble_next_X0hl);
    end
  end

  //----------------------------------------------------------------------
  // X1 Stage
  //----------------------------------------------------------------------

  // Is current stage valid?

  wire inst_val_X1hl = ( !bubble_X1hl && !squash_X1hl );

  // Data memory queue control signals

  assign dmemresp_queue_en_X1hl = ( stall_X1hl && dmemresp_val );
  wire   dmemresp_queue_val_next_X1hl
    = stall_X1hl && ( dmemresp_val || dmemresp_queue_val_X1hl );

  // Dummy Squash Signal

  wire squash_X1hl = 1'b0;

  // Stall in X1 if memory response is not returned for a valid request

  wire stall_dmem_X1hl
    = ( !reset && dmemreq_val_X1hl && inst_val_X1hl && !dmemresp_val && !dmemresp_queue_val_X1hl );
  wire stall_imem_X1hl
    = ( !reset && imemreq_val_Fhl && inst_val_Fhl && !imemresp0_val && !imemresp0_queue_val_Fhl )
   || ( !reset && imemreq_val_Fhl && inst_val_Fhl && !imemresp1_val && !imemresp1_queue_val_Fhl );

  // Aggregate Stall Signal

  wire stall_X1hl = ( stall_X2hl || stall_imem_X1hl || stall_dmem_X1hl );

  // Next bubble bit

  wire bubble_sel_X1hl  = ( squash_X1hl || stall_X1hl );
  wire bubble_next_X1hl = ( !bubble_sel_X1hl ) ? bubble_X1hl
                       : ( bubble_sel_X1hl )  ? 1'b1
                       :                       1'bx;

  //----------------------------------------------------------------------
  // X2 <- X1
  //----------------------------------------------------------------------

  reg [31:0] irA_X2hl;
  reg        rfA_wen_X2hl;
  reg  [4:0] rfA_waddr_X2hl;
  reg  [4:0] rob_fill_slot_A_X2hl;
  reg        rob_fill_val_A_X2hl;

  reg        dmemresp_queue_val_X1hl;
  reg        csr_wen_X2hl;
  reg  [4:0] csr_addr_X2hl;
  reg        execute_mux_sel_X2hl;
  reg        muldiv_mux_sel_X2hl;

  reg        bubble_X2hl;

  // Pipeline Controls

  always @ ( posedge clk ) begin
    if ( reset ) begin
      bubble_X2hl <= 1'b1;
    end
    else if( !stall_X2hl ) begin
      irA_X2hl              <= irA_X1hl;
      rfA_wen_X2hl          <= rfA_wen_X1hl;
      rfA_waddr_X2hl        <= rfA_waddr_X1hl;
      rob_fill_slot_A_X2hl  <= rob_fill_slot_A_X1hl;
      rob_fill_val_A_X2hl   <= rob_fill_val_A_X1hl && !bubble_next_X1hl;

      muldiv_mux_sel_X2hl   <= muldiv_mux_sel_X1hl;
      csr_wen_X2hl          <= csr_wen_X1hl;
      csr_addr_X2hl         <= csr_addr_X1hl;
      execute_mux_sel_X2hl  <= execute_mux_sel_X1hl;

      bubble_X2hl           <= !(rob_fill_val_A_X1hl && !bubble_next_X1hl);
    end
    dmemresp_queue_val_X1hl <= dmemresp_queue_val_next_X1hl;
  end

  //----------------------------------------------------------------------
  // X2 Stage
  //----------------------------------------------------------------------

  // Is current stage valid?

  wire inst_val_X2hl = ( !bubble_X2hl && !squash_X2hl );

  // Dummy Squash Signal

  wire squash_X2hl = 1'b0;

  // Dummy Stall Signal

  wire stall_X2hl = 1'b0;

  // Next bubble bit

  wire bubble_sel_X2hl  = ( squash_X2hl || stall_X2hl );
  wire bubble_next_X2hl = ( !bubble_sel_X2hl ) ? bubble_X2hl
                       : ( bubble_sel_X2hl )  ? 1'b1
                       :                       1'bx;

  //----------------------------------------------------------------------
  // X3 <- X2
  //----------------------------------------------------------------------

  reg [31:0] irA_X3hl;
  reg        rfA_wen_X3hl;
  reg  [4:0] rfA_waddr_X3hl;
  reg  [4:0] rob_fill_slot_A_X3hl;
  reg        rob_fill_val_A_X3hl;

  reg        csr_wen_X3hl;
  reg  [4:0] csr_addr_X3hl;
  reg        execute_mux_sel_X3hl;
  reg        muldiv_mux_sel_X3hl;

  reg        bubble_X3hl;

  // Pipeline Controls

  always @ ( posedge clk ) begin
    if ( reset ) begin
      bubble_X3hl <= 1'b1;
    end
    else if( !stall_X3hl ) begin
      irA_X3hl              <= irA_X2hl;
      rfA_wen_X3hl          <= rfA_wen_X2hl;
      rfA_waddr_X3hl        <= rfA_waddr_X2hl;
      rob_fill_slot_A_X3hl  <= rob_fill_slot_A_X2hl;
      rob_fill_val_A_X3hl   <= rob_fill_val_A_X2hl && !bubble_next_X2hl;

      muldiv_mux_sel_X3hl   <= muldiv_mux_sel_X2hl;
      csr_wen_X3hl          <= csr_wen_X2hl;
      csr_addr_X3hl         <= csr_addr_X2hl;
      execute_mux_sel_X3hl  <= execute_mux_sel_X2hl;

      bubble_X3hl           <= !(rob_fill_val_A_X2hl && !bubble_next_X2hl);
    end
  end

  //----------------------------------------------------------------------
  // X3 Stage
  //----------------------------------------------------------------------

  // Is current stage valid?

  wire inst_val_X3hl = ( !bubble_X3hl && !squash_X3hl );

  // Dummy Squash Signal

  wire squash_X3hl = 1'b0;

  // Dummy Stall Signal

  wire stall_X3hl = 1'b0;

  // Next bubble bit

  wire bubble_sel_X3hl  = ( squash_X3hl || stall_X3hl );
  wire bubble_next_X3hl = ( !bubble_sel_X3hl ) ? bubble_X3hl
                       : ( bubble_sel_X3hl )  ? 1'b1
                       :                       1'bx;

  //----------------------------------------------------------------------
  // W <- X3
  //----------------------------------------------------------------------

  reg [31:0] irA_Whl;
  reg        rfA_wen_Whl;
  reg  [4:0] rfA_waddr_Whl;
  reg  [4:0] rob_fill_slot_A_Whl;
  reg        rob_fill_val_A_Whl;

  reg [31:0] irB_Whl;
  reg        rfB_wen_Whl;
  reg  [4:0] rfB_waddr_Whl;
  reg  [4:0] rob_fill_slot_B_Whl;
  reg        rob_fill_val_B_Whl;

  reg        csr_wen_Whl;
  reg  [4:0] csr_addr_Whl;
  reg        bubble_Whl;

  // Pipeline Controls

  always @ ( posedge clk ) begin
    if ( reset ) begin
      bubble_Whl <= 1'b1;
    end
    else if( !stall_Whl ) begin
      irA_Whl             <= irA_X3hl;
      rfA_wen_Whl         <= rfA_wen_X3hl;
      rfA_waddr_Whl       <= rfA_waddr_X3hl;
      rob_fill_slot_A_Whl <= rob_fill_slot_A_X3hl;
      rob_fill_val_A_Whl  <= rob_fill_val_A_X3hl && !bubble_next_X3hl;

      irB_Whl             <= irB_X0hl;
      rfB_wen_Whl         <= rfB_wen_X0hl;
      rfB_waddr_Whl       <= rfB_waddr_X0hl;
      rob_fill_slot_B_Whl <= rob_fill_slot_B_X0hl;
      rob_fill_val_B_Whl  <= rob_fill_val_B_X0hl && !bubble_next_X0hl;

      csr_wen_Whl         <= csr_wen_X3hl;
      csr_addr_Whl        <= csr_addr_X3hl;

      bubble_Whl          <= !(rob_fill_val_B_X0hl && !bubble_next_X0hl) &&
                             !(rob_fill_val_A_X3hl && !bubble_next_X3hl);
    end
  end

  //----------------------------------------------------------------------
  // Writeback Stage
  //----------------------------------------------------------------------

  // Is current stage valid?

  wire inst_val_Whl = ( !bubble_Whl && !squash_Whl );

  // Only set register file wen if instruction is valid

  wire rfA_wen_out_Whl = ( rob_fill_val_A_Whl && !stall_Whl && rfA_wen_Whl );
  wire rfB_wen_out_Whl = ( rob_fill_val_B_Whl && !stall_Whl && rfB_wen_Whl );

  // Dummy squash and stall signals

  wire squash_Whl = 1'b0;
  wire stall_Whl  = 1'b0;

  // Reorder Buffer signals

  wire rob_fill_wen_A_Whl = rfA_wen_out_Whl && rob_fill_val_A_Whl;
  wire rob_fill_wen_B_Whl = rfB_wen_out_Whl && rob_fill_val_B_Whl;

  //----------------------------------------------------------------------
  // Debug registers for instruction disassembly
  //----------------------------------------------------------------------

  reg [31:0] irA_debug;
  reg [31:0] irB_debug;
  reg        inst_val_debug;

  always @ ( posedge clk ) begin
    irA_debug       <= irA_Whl;
    inst_val_debug <= inst_val_Whl;
    irB_debug       <= irB_Whl; // FIXME: fix this when you can have two instructions issued per cycle!
  end

  //----------------------------------------------------------------------
  // CSR register
  //----------------------------------------------------------------------

  reg  [31:0] csr_status;
  reg         csr_stats;

  always @ ( posedge clk ) begin
    if ( csr_wen_Whl && rob_fill_val_A_Whl ) begin
      case ( csr_addr_Whl )
        12'd10 : csr_stats  <= proc2csr_data_Whl[0];
        12'd21 : csr_status <= proc2csr_data_Whl;
      endcase
    end
  end

//========================================================================
// Disassemble instructions
//========================================================================

  `ifndef SYNTHESIS

  riscv_InstMsgDisasm inst0_msg_disasm_D
  (
    .msg ( ir0_Dhl )
  );

  riscv_InstMsgDisasm instA_msg_disasm_X0
  (
    .msg ( irA_X0hl )
  );

  riscv_InstMsgDisasm instA_msg_disasm_X1
  (
    .msg ( irA_X1hl )
  );

  riscv_InstMsgDisasm instA_msg_disasm_X2
  (
    .msg ( irA_X2hl )
  );

  riscv_InstMsgDisasm instA_msg_disasm_X3
  (
    .msg ( irA_X3hl )
  );

  riscv_InstMsgDisasm instA_msg_disasm_W
  (
    .msg ( irA_Whl )
  );

  riscv_InstMsgDisasm instA_msg_disasm_debug
  (
    .msg ( irA_debug )
  );

  riscv_InstMsgDisasm inst1_msg_disasm_D
  (
    .msg ( ir1_Dhl )
  );

  riscv_InstMsgDisasm instB_msg_disasm_X0
  (
    .msg ( irB_X0hl )
  );

  riscv_InstMsgDisasm instB_msg_disasm_X1
  (
    .msg ( irB_X0hl )
  );

  riscv_InstMsgDisasm instB_msg_disasm_X2
  (
    .msg ( irB_X0hl )
  );

  riscv_InstMsgDisasm instB_msg_disasm_X3
  (
    .msg ( irB_X0hl )
  );

  riscv_InstMsgDisasm instB_msg_disasm_W
  (
    .msg ( irB_Whl )
  );

  riscv_InstMsgDisasm instB_msg_disasm_debug
  (
    .msg ( irB_debug )
  );

  `endif

//========================================================================
// Assertions
//========================================================================
// Detect illegal instructions and terminate the simulation if multiple
// illegal instructions are detected in succession.

  `ifndef SYNTHESIS

  reg overload = 1'b0;

  always @ ( posedge clk ) begin
    if (( !cs0[`RISCV_INST_MSG_INST_VAL] && !reset ) 
     || ( !cs1[`RISCV_INST_MSG_INST_VAL] && !reset )) begin
      $display(" RTL-ERROR : %m : Illegal instruction!");

      if ( overload == 1'b1 ) begin
        $finish;
      end

      overload = 1'b1;
    end
    else begin
      overload = 1'b0;
    end
  end

  `endif

//========================================================================
// Stats
//========================================================================

  `ifndef SYNTHESIS

  reg [31:0] num_inst    = 32'b0;
  reg [31:0] num_cycles  = 32'b0;
  reg        stats_en    = 1'b0; // Used for enabling stats on asm tests

  wire rob_commit_spec_1;
  wire rob_commit_spec_2;

  wire count0 = rob_commit_val_1 && !rob_commit_spec_1;
  wire count1 = rob_commit_val_2 && !rob_commit_spec_2;

  always @( posedge clk ) begin
    if ( !reset ) begin

      // Count cycles if stats are enabled

      if ( stats_en || csr_stats ) begin
        num_cycles = num_cycles + 1;

        // Count instructions that reach writeback
        if ( count0 && count1 ) begin
          num_inst = num_inst + 2;
        end
        else if ( count0 || count1 ) begin
          num_inst = num_inst + 1;
        end
      end

    end
  end

  `endif

endmodule

`endif

// vim: set textwidth=0 ts=2 sw=2 sts=2 :
