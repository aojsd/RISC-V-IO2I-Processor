//=========================================================================
// 5-Stage RISCV Core
//=========================================================================

`ifndef RISCV_CORE_V
`define RISCV_CORE_V

`include "vc-MemReqMsg.v"
`include "vc-MemRespMsg.v"
`include "riscvio2i-CoreCtrl.v"
`include "riscvio2i-CoreDpath.v"

module riscv_Core
(
  input         clk,
  input         reset,

  // Instruction Memory Request Port

  output [`VC_MEM_REQ_MSG_SZ(32,32)-1:0] imemreq_msg,
  output                                 imemreq_val,
  input                                  imemreq_rdy,

  // Instruction Memory Response Port

  input [`VC_MEM_RESP_MSG_SZ(32)-1:0] imemresp_msg,
  input                               imemresp_val,

  // Data Memory Request Port

  output [`VC_MEM_REQ_MSG_SZ(32,32)-1:0] dmemreq_msg,
  output                                 dmemreq_val,
  input                                  dmemreq_rdy,

  // Data Memory Response Port

  input [`VC_MEM_RESP_MSG_SZ(32)-1:0] dmemresp_msg,
  input                               dmemresp_val,

  // CSR Status Register Output to Host

  output [31:0] csr_status
);

  wire [31:0] imemreq_msg_addr;
  wire [31:0] imemresp_msg_data;

  wire        dmemreq_msg_rw;
  wire  [1:0] dmemreq_msg_len;
  wire [31:0] dmemreq_msg_addr;
  wire [31:0] dmemreq_msg_data;
  wire [31:0] dmemresp_msg_data;

  wire  [1:0] pc_mux_sel_Phl;
  wire  [2:0] op0_byp_mux_sel_Ihl;
  wire  [3:0] op0_byp_rob_slot_Ihl;
  wire  [1:0] op0_mux_sel_Ihl;
  wire  [2:0] op1_byp_mux_sel_Ihl;
  wire  [3:0] op1_byp_rob_slot_Ihl;
  wire  [2:0] op1_mux_sel_Ihl;
  wire [31:0] inst_Ihl;
  wire  [3:0] alu_fn_Xhl;
  wire  [2:0] muldivreq_msg_fn_Ihl;
  wire        muldivreq_val;
  wire        muldivreq_rdy;
  wire        muldivresp_val;
  wire        muldivresp_rdy;
  wire        muldiv_mux_sel_X3hl;
  wire  [2:0] dmemresp_mux_sel_Mhl;
  wire        dmemresp_queue_en_Mhl;
  wire        dmemresp_queue_val_Mhl;
  wire  [1:0] wb_mux_sel_Whl;
  wire        rf_wen_Whl;
  wire  [4:0] rf_waddr_Whl;
  wire        rob_fill_wen_Whl;
  wire [ 3:0] rob_fill_slot_Whl;
  wire        rob_commit_wen_Chl;
  wire [ 3:0] rob_commit_slot_Chl;
  wire [ 4:0] rob_commit_waddr_Chl;
  wire        stall_Fhl;
  wire        stall_Dhl;
  wire        stall_Ihl;
  wire        stall_Xhl;
  wire        stall_Mhl;
  wire        stall_Whl;

  wire        branch_cond_eq_Xhl;
  wire        branch_cond_ne_Xhl;
  wire        branch_cond_lt_Xhl;
  wire        branch_cond_ltu_Xhl;
  wire        branch_cond_ge_Xhl;
  wire        branch_cond_geu_Xhl;
  wire [31:0] proc2csr_data_Whl;

  wire [31:0] pc_Dhl;
  wire [31:0] pc_plus4_Dhl;
  wire [31:0] pc_Ihl;
  wire [31:0] pc_plus4_Ihl;
  //----------------------------------------------------------------------
  // Pack Memory Request Messages
  //----------------------------------------------------------------------

  vc_MemReqMsgToBits#(32,32) imemreq_msg_to_bits
  (
    .type (`VC_MEM_REQ_MSG_TYPE_READ),
    .addr (imemreq_msg_addr),
    .len  (2'd0),
    .data (32'bx),
    .bits (imemreq_msg)
  );

  vc_MemReqMsgToBits#(32,32) dmemreq_msg_to_bits
  (
    .type (dmemreq_msg_rw),
    .addr (dmemreq_msg_addr),
    .len  (dmemreq_msg_len),
    .data (dmemreq_msg_data),
    .bits (dmemreq_msg)
  );

  //----------------------------------------------------------------------
  // Unpack Memory Response Messages
  //----------------------------------------------------------------------

  vc_MemRespMsgFromBits#(32) imemresp_msg_from_bits
  (
    .bits (imemresp_msg),
    .type (),
    .len  (),
    .data (imemresp_msg_data)
  );

  vc_MemRespMsgFromBits#(32) dmemresp_msg_from_bits
  (
    .bits (dmemresp_msg),
    .type (),
    .len  (),
    .data (dmemresp_msg_data)
  );

  //----------------------------------------------------------------------
  // Control Unit
  //----------------------------------------------------------------------

  riscv_CoreCtrl ctrl
  (
    .clk                    (clk),
    .reset                  (reset),

    // Instruction Memory Port

    .imemreq_val            (imemreq_val),
    .imemreq_rdy            (imemreq_rdy),
    .imemresp_msg_data      (imemresp_msg_data),
    .imemresp_val           (imemresp_val),

    // Data Memory Port

    .dmemreq_msg_rw         (dmemreq_msg_rw),
    .dmemreq_msg_len        (dmemreq_msg_len),
    .dmemreq_val            (dmemreq_val),
    .dmemreq_rdy            (dmemreq_rdy),
    .dmemresp_val           (dmemresp_val),

    // Controls Signals (ctrl->dpath)

    .pc_mux_sel_Phl         (pc_mux_sel_Phl),
    .op0_byp_mux_sel_Ihl    (op0_byp_mux_sel_Ihl),
    .op0_byp_rob_slot_Ihl   (op0_byp_rob_slot_Ihl),
    .op0_mux_sel_Ihl        (op0_mux_sel_Ihl),
    .op1_byp_mux_sel_Ihl    (op1_byp_mux_sel_Ihl),
    .op1_byp_rob_slot_Ihl   (op1_byp_rob_slot_Ihl),
    .op1_mux_sel_Ihl        (op1_mux_sel_Ihl),
    .inst_Ihl               (inst_Ihl),
    .alu_fn_Xhl             (alu_fn_Xhl),
    .muldivreq_msg_fn_Ihl   (muldivreq_msg_fn_Ihl),
    .muldivreq_val          (muldivreq_val),
    .muldivreq_rdy          (muldivreq_rdy),
    .muldivresp_val         (muldivresp_val),
    .muldivresp_rdy         (muldivresp_rdy),
    .muldiv_mux_sel_X3hl    (muldiv_mux_sel_X3hl),
    .dmemresp_mux_sel_Mhl   (dmemresp_mux_sel_Mhl),
    .dmemresp_queue_en_Mhl  (dmemresp_queue_en_Mhl),
    .dmemresp_queue_val_Mhl (dmemresp_queue_val_Mhl),
    .wb_mux_sel_Whl         (wb_mux_sel_Whl),
    .rf_wen_out_Whl         (rf_wen_Whl),
    .rf_waddr_Whl           (rf_waddr_Whl),
    .rob_fill_wen_Whl       (rob_fill_wen_Whl),
    .rob_fill_slot_Whl      (rob_fill_slot_Whl),
    .rob_commit_wen_Chl     (rob_commit_wen_Chl),
    .rob_commit_slot_Chl    (rob_commit_slot_Chl),
    .rob_commit_waddr_Chl   (rob_commit_waddr_Chl),
    .stall_Fhl              (stall_Fhl),
    .stall_Dhl              (stall_Dhl),
    .stall_Ihl              (stall_Ihl),
    .stall_Xhl              (stall_Xhl),
    .stall_Mhl              (stall_Mhl),
    .stall_Whl              (stall_Whl),
    .pc_Ihl                  (pc_Ihl),
    .pc_plus4_Ihl            (pc_plus4_Ihl),
    

    // Control Signals (dpath->ctrl)
    .pc_Dhl                 (pc_Dhl),
    .pc_plus4_Dhl           (pc_plus4_Dhl),
    .branch_cond_eq_Xhl	    (branch_cond_eq_Xhl),
    .branch_cond_ne_Xhl	    (branch_cond_ne_Xhl),
    .branch_cond_lt_Xhl	    (branch_cond_lt_Xhl),
    .branch_cond_ltu_Xhl	  (branch_cond_ltu_Xhl),
    .branch_cond_ge_Xhl	    (branch_cond_ge_Xhl),
    .branch_cond_geu_Xhl	  (branch_cond_geu_Xhl),
    .proc2csr_data_Whl      (proc2csr_data_Whl),

    // CSR Status

    .csr_status             (csr_status)
  );

  //----------------------------------------------------------------------
  // Datapath
  //----------------------------------------------------------------------

  riscv_CoreDpath dpath
  (
    .clk                     (clk),
    .reset                   (reset),

    // Instruction Memory Port

    .imemreq_msg_addr        (imemreq_msg_addr),

    // Data Memory Port

    .dmemreq_msg_addr        (dmemreq_msg_addr),
    .dmemreq_msg_data        (dmemreq_msg_data),
    .dmemresp_msg_data       (dmemresp_msg_data),

    // Controls Signals (ctrl->dpath)

    .pc_mux_sel_Phl          (pc_mux_sel_Phl),
    .op0_byp_mux_sel_Ihl     (op0_byp_mux_sel_Ihl),
    .op0_byp_rob_slot_Ihl    (op0_byp_rob_slot_Ihl),
    .op0_mux_sel_Ihl         (op0_mux_sel_Ihl),
    .op1_byp_mux_sel_Ihl     (op1_byp_mux_sel_Ihl),
    .op1_byp_rob_slot_Ihl    (op1_byp_rob_slot_Ihl),
    .op1_mux_sel_Ihl         (op1_mux_sel_Ihl),
    .inst_Ihl                (inst_Ihl),
    .alu_fn_Xhl              (alu_fn_Xhl),
    .muldivreq_msg_fn_Ihl    (muldivreq_msg_fn_Ihl),
    .muldivreq_val           (muldivreq_val),
    .muldivreq_rdy           (muldivreq_rdy),
    .muldivresp_val          (muldivresp_val),
    .muldivresp_rdy          (muldivresp_rdy),
    .muldiv_mux_sel_X3hl     (muldiv_mux_sel_X3hl),
    .dmemresp_mux_sel_Mhl    (dmemresp_mux_sel_Mhl),
    .dmemresp_queue_en_Mhl   (dmemresp_queue_en_Mhl),
    .dmemresp_queue_val_Mhl  (dmemresp_queue_val_Mhl),
    .wb_mux_sel_Whl          (wb_mux_sel_Whl),
    .rf_wen_Whl              (rf_wen_Whl),
    .rf_waddr_Whl            (rf_waddr_Whl),
    .rob_fill_wen_Whl       (rob_fill_wen_Whl),
    .rob_fill_slot_Whl      (rob_fill_slot_Whl),
    .rob_commit_wen_Chl     (rob_commit_wen_Chl),
    .rob_commit_slot_Chl    (rob_commit_slot_Chl),
    .rob_commit_waddr_Chl   (rob_commit_waddr_Chl),
    .stall_Fhl               (stall_Fhl),
    .stall_Dhl               (stall_Dhl),
    .stall_Ihl               (stall_Ihl),
    .stall_Xhl               (stall_Xhl),
    .stall_Mhl               (stall_Mhl),
    .stall_Whl               (stall_Whl),
    .pc_Ihl                  (pc_Ihl),
    .pc_plus4_Ihl            (pc_plus4_Ihl),
    

    // Control Signals (dpath->ctrl)
    .pc_Dhl                  (pc_Dhl),
    .pc_plus4_Dhl            (pc_plus4_Dhl),
    .branch_cond_eq_Xhl	     (branch_cond_eq_Xhl),
    .branch_cond_ne_Xhl	     (branch_cond_ne_Xhl),
    .branch_cond_lt_Xhl	     (branch_cond_lt_Xhl),
    .branch_cond_ltu_Xhl	 (branch_cond_ltu_Xhl),
    .branch_cond_ge_Xhl	     (branch_cond_ge_Xhl),
    .branch_cond_geu_Xhl	 (branch_cond_geu_Xhl),
    .proc2csr_data_Whl       (proc2csr_data_Whl)
  );

endmodule

`endif

// vim: set textwidth=0 ts=2 sw=2 sts=2 :