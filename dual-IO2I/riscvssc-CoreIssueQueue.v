//=========================================================================
// IO2I 2-Wide RISC-V Reorder Buffer and Rename Table
//=========================================================================

`ifndef RISCV_CORE_ISSUEQUEUE_V
`define RISCV_CORE_ISSUEQUEUE_V

module riscv_CoreIssueQueue
(
  input         clk,
  input         reset,

  input         brj_taken_X0hl,
  input         brj_resolved_X0hl,
  input         squash_first_I_inst_Ihl,
  input  [31:0] sb_src_ready, // signal from sb detailing which inst are ready to be issued 
  output        iq_contains_jmp,
  // enqueue values
  input         iq_enqueue_spec0, // determines if newly enqueued val is spec
  input         iq_enqueue_spec1, 

  input         iq_enqueue_val0, // determines if input is valid
  input         iq_enqueue_val1, // determines if input is valid
  output        iq_enqueue_rdy,
  output [4:0]  iq_enqueue_slot0,
  output [4:0]  iq_enqueue_slot1,

  // dequeue values
  output        iq_dequeue_val0,
  output        iq_dequeue_val1,
  input         iq_dequeue_rdy0,
  input         iq_dequeue_rdy1,
  output [4:0]  iq_dequeue_slot0,
  output [4:0]  iq_dequeue_slot1,
  
  // State inputs
  // IR0
  input [31:0]  iq_ir0_Dhl,
  input         iq_ir0_squashed_Dhl,
  input         iq_rf0_wen_Dhl,
  input  [4:0]  iq_rf0_waddr_Dhl,
  input  [3:0]  iq_alu0_fn_Dhl,
  input  [2:0]  iq_br0_sel_Dhl,
  input         iq_j0_en_Dhl,
  input  [1:0]  iq_pc0_mux_sel_Dhl,
  input         iq_muldivreq0_val_Dhl,
  input  [2:0]  iq_muldivreq0_msg_fn_Dhl,
  input         iq_muldiv0_mux_sel_Dhl,
  input         iq_execute0_mux_sel_Dhl,
  input         iq_dmemreq0_msg_rw_Dhl,
  input  [1:0]  iq_dmemreq0_msg_len_Dhl,
  input         iq_dmemreq0_val_Dhl,
  input  [2:0]  iq_dmemresp0_mux_sel_Dhl,
  input         iq_memex0_mux_sel_Dhl,
  input         iq_csr0_wen_Dhl,
  input [11:0]  iq_csr0_addr_Dhl,

  input  [4:0]  iq_rs00_addr_Dhl,
  input  [4:0]  iq_rs01_addr_Dhl,

  input         iq_rs00_en_Dhl,
  input         iq_rs01_en_Dhl,

  input  [4:0]  iq_rob_fill_slot_0_Dhl,

  input         iq_rs00_renamed_Dhl,
  input         iq_rs01_renamed_Dhl,

  input  [4:0]  iq_rs00_rt_slot_Dhl,
  input  [4:0]  iq_rs01_rt_slot_Dhl,

  input  [1:0]  iq_op00_mux_sel_Dhl,
  input  [2:0]  iq_op01_mux_sel_Dhl,

  // IR1
  input [31:0]  iq_ir1_Dhl,
  input         iq_ir1_squashed_Dhl,
  input         iq_rf1_wen_Dhl,
  input  [4:0]  iq_rf1_waddr_Dhl,
  input  [3:0]  iq_alu1_fn_Dhl,
  input  [2:0]  iq_br1_sel_Dhl,
  input         iq_j1_en_Dhl,
  input  [1:0]  iq_pc1_mux_sel_Dhl,
  input         iq_muldivreq1_val_Dhl,
  input  [2:0]  iq_muldivreq1_msg_fn_Dhl,
  input         iq_muldiv1_mux_sel_Dhl,
  input         iq_execute1_mux_sel_Dhl,
  input         iq_dmemreq1_msg_rw_Dhl,
  input  [1:0]  iq_dmemreq1_msg_len_Dhl,
  input         iq_dmemreq1_val_Dhl,
  input  [2:0]  iq_dmemresp1_mux_sel_Dhl,
  input         iq_memex1_mux_sel_Dhl,
  input         iq_csr1_wen_Dhl,
  input [11:0]  iq_csr1_addr_Dhl,

  input  [4:0]  iq_rs10_addr_Dhl,
  input  [4:0]  iq_rs11_addr_Dhl,

  input         iq_rs10_en_Dhl,
  input         iq_rs11_en_Dhl,

  input  [4:0]  iq_rob_fill_slot_1_Dhl,

  input         iq_rs10_renamed_Dhl,
  input         iq_rs11_renamed_Dhl,

  input  [4:0]  iq_rs10_rt_slot_Dhl,
  input  [4:0]  iq_rs11_rt_slot_Dhl,

  input  [1:0]  iq_op10_mux_sel_Dhl,
  input  [2:0]  iq_op11_mux_sel_Dhl,



  // State outputs
  // IR0
  output [31:0]  iq_ir0_Ihl,
  output         iq_ir0_squashed_Ihl,
  output         iq_rf0_wen_Ihl,
  output  [4:0]  iq_rf0_waddr_Ihl,
  output  [3:0]  iq_alu0_fn_Ihl,
  output  [2:0]  iq_br0_sel_Ihl,
  output         iq_j0_en_Ihl,
  output  [1:0]  iq_pc0_mux_sel_Ihl,
  output         iq_muldivreq0_val_Ihl,
  output  [2:0]  iq_muldivreq0_msg_fn_Ihl,
  output         iq_muldiv0_mux_sel_Ihl,
  output         iq_execute0_mux_sel_Ihl,
  output         iq_dmemreq0_msg_rw_Ihl,
  output  [1:0]  iq_dmemreq0_msg_len_Ihl,
  output         iq_dmemreq0_val_Ihl,
  output  [2:0]  iq_dmemresp0_mux_sel_Ihl,
  output         iq_memex0_mux_sel_Ihl,
  output         iq_csr0_wen_Ihl,
  output [11:0]  iq_csr0_addr_Ihl,

  output  [4:0]  iq_rs00_addr_Ihl,
  output  [4:0]  iq_rs01_addr_Ihl,

  output         iq_rs00_en_Ihl,
  output         iq_rs01_en_Ihl,

  output  [4:0]  iq_rob_fill_slot_0_Ihl,

  output         iq_rs00_renamed_Ihl,
  output         iq_rs01_renamed_Ihl,

  output  [4:0]  iq_rs00_rt_slot_Ihl,
  output  [4:0]  iq_rs01_rt_slot_Ihl,

  output  [1:0]  iq_op00_mux_sel_Ihl,
  output  [2:0]  iq_op01_mux_sel_Ihl,

  // IR1
  output [31:0]  iq_ir1_Ihl,
  output         iq_ir1_squashed_Ihl,
  output         iq_rf1_wen_Ihl,
  output  [4:0]  iq_rf1_waddr_Ihl,
  output  [3:0]  iq_alu1_fn_Ihl,
  output  [2:0]  iq_br1_sel_Ihl,
  output         iq_j1_en_Ihl,
  output  [1:0]  iq_pc1_mux_sel_Ihl,
  output         iq_muldivreq1_val_Ihl,
  output  [2:0]  iq_muldivreq1_msg_fn_Ihl,
  output         iq_muldiv1_mux_sel_Ihl,
  output         iq_execute1_mux_sel_Ihl,
  output         iq_dmemreq1_msg_rw_Ihl,
  output  [1:0]  iq_dmemreq1_msg_len_Ihl,
  output         iq_dmemreq1_val_Ihl,
  output  [2:0]  iq_dmemresp1_mux_sel_Ihl,
  output         iq_memex1_mux_sel_Ihl,
  output         iq_csr1_wen_Ihl,
  output [11:0]  iq_csr1_addr_Ihl,

  output  [4:0]  iq_rs10_addr_Ihl,
  output  [4:0]  iq_rs11_addr_Ihl,

  output         iq_rs10_en_Ihl,
  output         iq_rs11_en_Ihl,

  output  [4:0]  iq_rob_fill_slot_1_Ihl,

  output         iq_rs10_renamed_Ihl,
  output         iq_rs11_renamed_Ihl,

  output  [4:0]  iq_rs10_rt_slot_Ihl,
  output  [4:0]  iq_rs11_rt_slot_Ihl,

  output  [1:0]  iq_op10_mux_sel_Ihl,
  output  [2:0]  iq_op11_mux_sel_Ihl
);
  
  //SETUP ENQUEUE RDY SIGNAL
  // enqueue is rdy if the tail is not head OR tail is head, but tail is invalid (i.e. empty iq)
  wire         iq_empty           = (iq_head == iq_tail) && !(iq_valid[iq_tail]);
  wire         iq_has_two_empty   = (iq_head != iq_tail+1);

  wire         iq_enqueue_rdy     = (( iq_head != iq_tail && iq_has_two_empty) ||
                                    ( iq_empty ) );
  reg         iq_contains_jmp;
  integer n;
  always @(posedge reset) begin
    iq_contains_jmp <= 1'b0;
  end
  always @(posedge clk) begin
    if ((iq_enqueue_val0 && iq_j0_en_Dhl) || (iq_enqueue_val1 && iq_j1_en_Dhl)) begin
      iq_contains_jmp <= 1'b1;
    end
    if ( (iq_dequeue_val0 && iq_j0_en_Ihl && iq_dequeue_rdy0) || (iq_dequeue_val1 && iq_j1_en_Ihl && iq_dequeue_rdy1)) begin
      iq_contains_jmp <= 1'b0;
    end
  end


  
  //state storage
  reg [31:0] iq_valid;
  reg [31:0] iq_spec;

  reg [31:0]  iq_ir [31:0];
  reg         iq_ir_squashed [31:0];
  reg         iq_rf_wen [31:0];
  reg  [4:0]  iq_rf_waddr [31:0];
  reg  [3:0]  iq_alu_fn [31:0];
  reg  [2:0]  iq_br_sel [31:0];
  reg         iq_j_en [31:0];
  reg  [1:0]  iq_pc_mux_sel [31:0];
  reg         iq_muldivreq_val [31:0];
  reg  [2:0]  iq_muldivreq_msg_fn [31:0];
  reg         iq_muldiv_mux_sel [31:0];
  reg         iq_execute_mux_sel [31:0];
  reg         iq_dmemreq_msg_rw [31:0];
  reg  [1:0]  iq_dmemreq_msg_len [31:0];
  reg         iq_dmemreq_val [31:0];
  reg  [2:0]  iq_dmemresp_mux_sel [31:0];
  reg         iq_memex_mux_sel [31:0];
  reg         iq_csr_wen [31:0];
  reg [11:0]  iq_csr_addr [31:0];

  reg  [4:0]  iq_rs0_addr [31:0];
  reg  [4:0]  iq_rs1_addr [31:0];
  reg         iq_rs0_en [31:0];
  reg         iq_rs1_en [31:0];
  reg  [4:0]  iq_rob_fill_slot [31:0];

  reg         iq_rs0_renamed [31:0];
  reg         iq_rs1_renamed [31:0];
  reg  [4:0]  iq_rs0_rt_slot [31:0];
  reg  [4:0]  iq_rs1_rt_slot [31:0];
  reg  [1:0]  iq_op0_mux_sel [31:0];
  reg  [2:0]  iq_op1_mux_sel [31:0];

  reg [4:0] iq_head;
  reg [4:0] iq_tail;
  wire[4:0] iq_head_next = iq_head + 1;
  wire[4:0] iq_tail_next = iq_tail + 1;

  integer i;

  //----------------------------------------------------------------------  
  // Reset Case
  //----------------------------------------------------------------------

  always @(posedge clk) begin
    if (reset) begin
      iq_head <= 5'b00000;
      iq_tail <= 5'b00000;
      for (i = 0; i < 32; i = i + 1) begin
        iq_valid[i] <= 1'b0;
        iq_spec[i] <= 1'b0;

        iq_ir[i]   <= 32'b0;
        iq_ir_squashed[i]    <= 1'b0;
        iq_rf_wen[i]   <= 1'b0;
        iq_rf_waddr[i]   <= 5'b0;
        iq_alu_fn[i]   <= 4'b0;
        iq_br_sel[i]   <= 3'b0;
        iq_j_en[i]   <= 1'b0;
        iq_pc_mux_sel[i]   <= 2'b0;
        iq_muldivreq_val[i]    <= 1'b0;
        iq_muldivreq_msg_fn[i]   <= 3'b0;
        iq_muldiv_mux_sel[i]   <= 1'b0;
        iq_execute_mux_sel[i]    <= 1'b0;
        iq_dmemreq_msg_rw[i]   <= 1'b0;
        iq_dmemreq_msg_len[i]    <= 2'b0;
        iq_dmemreq_val[i]    <= 1'b0;
        iq_dmemresp_mux_sel[i]   <= 3'b0;
        iq_memex_mux_sel[i]    <= 1'b0;
        iq_csr_wen[i]    <= 1'b0;
        iq_csr_addr[i]   <= 12'b0;

        iq_rs0_addr[i]   <= 5'b0;
        iq_rs1_addr[i]   <= 5'b0;
        iq_rs0_en[i]   <= 1'b0;
        iq_rs1_en[i]   <= 1'b0;
        iq_rob_fill_slot[i]   <= 5'b0;

        iq_rs0_renamed[i]    <= 1'b0;
        iq_rs1_renamed[i]    <= 1'b0;
        iq_rs0_rt_slot[i]    <= 5'b0;
        iq_rs1_rt_slot[i]    <= 5'b0;

        iq_op0_mux_sel[i]    <= 2'b0;
        iq_op1_mux_sel[i]    <= 3'b0;
      end
    end
  end

  
  // IQ Enqueue
  //----------------------------------------------------------------------

  always @(posedge clk) begin
    //ir0
    if (iq_enqueue_val0) begin
      iq_valid[iq_tail]              <= 1'b1;
      iq_spec[iq_tail]               <= iq_enqueue_spec0;

      iq_ir[iq_tail]                 <= iq_ir0_Dhl;
      iq_ir_squashed[iq_tail]        <= iq_ir0_squashed_Dhl;
      iq_rf_wen[iq_tail]             <= iq_rf0_wen_Dhl;
      iq_rf_waddr[iq_tail]           <= iq_rf0_waddr_Dhl;
      iq_alu_fn[iq_tail]             <= iq_alu0_fn_Dhl;
      iq_br_sel[iq_tail]             <= iq_br0_sel_Dhl;
      iq_j_en[iq_tail]               <= iq_j0_en_Dhl;
      iq_pc_mux_sel[iq_tail]         <= iq_pc0_mux_sel_Dhl;
      iq_muldivreq_val[iq_tail]      <= iq_muldivreq0_val_Dhl;
      iq_muldivreq_msg_fn[iq_tail]   <= iq_muldivreq0_msg_fn_Dhl;
      iq_muldiv_mux_sel[iq_tail]     <= iq_muldiv0_mux_sel_Dhl;
      iq_execute_mux_sel[iq_tail]    <= iq_execute0_mux_sel_Dhl;
      iq_dmemreq_msg_rw[iq_tail]     <= iq_dmemreq0_msg_rw_Dhl;
      iq_dmemreq_msg_len[iq_tail]    <= iq_dmemreq0_msg_len_Dhl;
      iq_dmemreq_val[iq_tail]        <= iq_dmemreq0_val_Dhl;
      iq_dmemresp_mux_sel[iq_tail]   <= iq_dmemresp0_mux_sel_Dhl;
      iq_memex_mux_sel[iq_tail]      <= iq_memex0_mux_sel_Dhl;
      iq_csr_wen[iq_tail]            <= iq_csr0_wen_Dhl;
      iq_csr_addr[iq_tail]           <= iq_csr0_addr_Dhl;

      iq_rs0_addr[iq_tail]           <= iq_rs00_addr_Dhl;
      iq_rs1_addr[iq_tail]           <= iq_rs01_addr_Dhl;
      iq_rs0_en[iq_tail]             <= iq_rs00_en_Dhl;
      iq_rs1_en[iq_tail]             <= iq_rs01_en_Dhl;
      iq_rob_fill_slot[iq_tail]      <= iq_rob_fill_slot_0_Dhl;

      iq_rs0_renamed[iq_tail]        <= iq_rs00_renamed_Dhl;
      iq_rs1_renamed[iq_tail]        <= iq_rs01_renamed_Dhl;
      iq_rs0_rt_slot[iq_tail]        <= iq_rs00_rt_slot_Dhl;
      iq_rs1_rt_slot[iq_tail]        <= iq_rs01_rt_slot_Dhl;

      iq_op0_mux_sel[iq_tail]        <= iq_op00_mux_sel_Dhl;
      iq_op1_mux_sel[iq_tail]        <= iq_op01_mux_sel_Dhl;
    end
    // ir1
    if (iq_enqueue_val1) begin
      iq_valid[iq_tail_next]             <= 1'b1;
      iq_spec[iq_tail_next]              <= iq_enqueue_spec1;

      iq_ir[iq_tail_next]                <= iq_ir1_Dhl;
      iq_ir_squashed[iq_tail_next]       <= iq_ir1_squashed_Dhl;
      iq_rf_wen[iq_tail_next]            <= iq_rf1_wen_Dhl;
      iq_rf_waddr[iq_tail_next]          <= iq_rf1_waddr_Dhl;
      iq_alu_fn[iq_tail_next]            <= iq_alu1_fn_Dhl;
      iq_br_sel[iq_tail_next]            <= iq_br1_sel_Dhl;
      iq_j_en[iq_tail_next]              <= iq_j1_en_Dhl;
      iq_pc_mux_sel[iq_tail_next]        <= iq_pc1_mux_sel_Dhl;
      iq_muldivreq_val[iq_tail_next]     <= iq_muldivreq1_val_Dhl;
      iq_muldivreq_msg_fn[iq_tail_next]  <= iq_muldivreq1_msg_fn_Dhl;
      iq_muldiv_mux_sel[iq_tail_next]    <= iq_muldiv1_mux_sel_Dhl;
      iq_execute_mux_sel[iq_tail_next]   <= iq_execute1_mux_sel_Dhl;
      iq_dmemreq_msg_rw[iq_tail_next]    <= iq_dmemreq1_msg_rw_Dhl;
      iq_dmemreq_msg_len[iq_tail_next]   <= iq_dmemreq1_msg_len_Dhl;
      iq_dmemreq_val[iq_tail_next]       <= iq_dmemreq1_val_Dhl;
      iq_dmemresp_mux_sel[iq_tail_next]  <= iq_dmemresp1_mux_sel_Dhl;
      iq_memex_mux_sel[iq_tail_next]     <= iq_memex1_mux_sel_Dhl;
      iq_csr_wen[iq_tail_next]           <= iq_csr1_wen_Dhl;
      iq_csr_addr[iq_tail_next]          <= iq_csr1_addr_Dhl;
      
      iq_rs0_addr[iq_tail_next]          <= iq_rs10_addr_Dhl;
      iq_rs1_addr[iq_tail_next]          <= iq_rs11_addr_Dhl;
      iq_rs0_en[iq_tail_next]            <= iq_rs10_en_Dhl;
      iq_rs1_en[iq_tail_next]            <= iq_rs11_en_Dhl;
      iq_rob_fill_slot[iq_tail_next]     <= iq_rob_fill_slot_1_Dhl;
      
      iq_rs0_renamed[iq_tail_next]       <= iq_rs10_renamed_Dhl;
      iq_rs1_renamed[iq_tail_next]       <= iq_rs11_renamed_Dhl;
      iq_rs0_rt_slot[iq_tail_next]       <= iq_rs10_rt_slot_Dhl;
      iq_rs1_rt_slot[iq_tail_next]       <= iq_rs11_rt_slot_Dhl;

      iq_op0_mux_sel[iq_tail_next]       <= iq_op10_mux_sel_Dhl;
      iq_op1_mux_sel[iq_tail_next]       <= iq_op11_mux_sel_Dhl;
    end
    
    // increment tail
    if(iq_enqueue_val0 && iq_enqueue_val1) begin
      iq_tail <= iq_tail+2;
    end
    else if(iq_enqueue_val0 || iq_enqueue_val1) begin
      iq_tail <= iq_tail+1;
    end
  end

  //----------------------------------------------------------------------
  // IQ DEQUEUE
  //----------------------------------------------------------------------  

  wire         iq_dequeue_val0 = iq_valid[j] && j!=k; // determines if input is valid
  wire [4:0]   iq_enqueue_slot0 = iq_tail;
  wire [4:0]   iq_dequeue_slot0 = j;
  wire [31:0]  iq_ir0_Ihl = iq_ir[j];
  wire         iq_ir0_squashed_Ihl = iq_ir_squashed[j];
  wire         iq_rf0_wen_Ihl = iq_rf_wen[j];
  wire  [4:0]  iq_rf0_waddr_Ihl = iq_rf_waddr[j];
  wire  [3:0]  iq_alu0_fn_Ihl = iq_alu_fn[j];
  wire  [2:0]  iq_br0_sel_Ihl = iq_br_sel[j];
  wire         iq_j0_en_Ihl = iq_j_en[j];
  wire  [1:0]  iq_pc0_mux_sel_Ihl = iq_pc_mux_sel[j];
  wire         iq_muldivreq0_val_Ihl = iq_muldivreq_val[j];
  wire  [2:0]  iq_muldivreq0_msg_fn_Ihl = iq_muldivreq_msg_fn[j];
  wire         iq_muldiv0_mux_sel_Ihl = iq_muldiv_mux_sel[j];
  wire         iq_execute0_mux_sel_Ihl = iq_execute_mux_sel[j];
  wire         iq_dmemreq0_msg_rw_Ihl = iq_dmemreq_msg_rw[j];
  wire  [1:0]  iq_dmemreq0_msg_len_Ihl = iq_dmemreq_msg_len[j];
  wire         iq_dmemreq0_val_Ihl = iq_dmemreq_val[j];
  wire  [2:0]  iq_dmemresp0_mux_sel_Ihl = iq_dmemresp_mux_sel[j];
  wire         iq_memex0_mux_sel_Ihl = iq_memex_mux_sel[j];
  wire         iq_csr0_wen_Ihl = iq_csr_wen[j];
  wire [11:0]  iq_csr0_addr_Ihl = iq_csr_addr[j];

  wire  [4:0]  iq_rs00_addr_Ihl = iq_rs0_addr[j];
  wire  [4:0]  iq_rs01_addr_Ihl = iq_rs1_addr[j];

  wire         iq_dequeue_val1 = iq_valid[k] && j!=k; // determines if input is valid
  wire [4:0]   iq_enqueue_slot1 = iq_tail_next;
  wire [4:0]   iq_dequeue_slot1 = k;

  wire [31:0]  iq_ir1_Ihl = iq_ir[k];
  wire         iq_ir1_squashed_Ihl = iq_ir_squashed[k];
  wire         iq_rf1_wen_Ihl = iq_rf_wen[k];
  wire  [4:0]  iq_rf1_waddr_Ihl = iq_rf_waddr[k];
  wire  [3:0]  iq_alu1_fn_Ihl = iq_alu_fn[k];
  wire  [2:0]  iq_br1_sel_Ihl = iq_br_sel[k];
  wire         iq_j1_en_Ihl = iq_j_en[k];
  wire  [1:0]  iq_pc1_mux_sel_Ihl = iq_pc_mux_sel[k];
  wire         iq_muldivreq1_val_Ihl = iq_muldivreq_val[k];
  wire  [2:0]  iq_muldivreq1_msg_fn_Ihl = iq_muldivreq_msg_fn[k];
  wire         iq_muldiv1_mux_sel_Ihl = iq_muldiv_mux_sel[k];
  wire         iq_execute1_mux_sel_Ihl = iq_execute_mux_sel[k];
  wire         iq_dmemreq1_msg_rw_Ihl = iq_dmemreq_msg_rw[k];
  wire  [1:0]  iq_dmemreq1_msg_len_Ihl = iq_dmemreq_msg_len[k];
  wire         iq_dmemreq1_val_Ihl = iq_dmemreq_val[k];
  wire  [2:0]  iq_dmemresp1_mux_sel_Ihl = iq_dmemresp_mux_sel[k];
  wire         iq_memex1_mux_sel_Ihl = iq_memex_mux_sel[k];
  wire         iq_csr1_wen_Ihl = iq_csr_wen[k];
  wire [11:0]  iq_csr1_addr_Ihl = iq_csr_addr[k];
  


  wire  [4:0]  iq_rs10_addr_Ihl = iq_rs0_addr[k];
  wire  [4:0]  iq_rs11_addr_Ihl = iq_rs1_addr[k];
  
  wire         iq_rs00_en_Ihl = iq_rs0_en[j];
  wire         iq_rs01_en_Ihl = iq_rs1_en[j];
  
  wire         iq_rs10_en_Ihl = iq_rs0_en[k];
  wire         iq_rs11_en_Ihl = iq_rs1_en[k];
  
  wire  [4:0]  iq_rob_fill_slot_0_Ihl = iq_rob_fill_slot[j];
  wire  [4:0]  iq_rob_fill_slot_1_Ihl = iq_rob_fill_slot[k];
  
  wire         iq_rs00_renamed_Ihl = iq_rs0_renamed[j];
  wire         iq_rs01_renamed_Ihl = iq_rs1_renamed[j];
  
  wire         iq_rs10_renamed_Ihl = iq_rs0_renamed[k];
  wire         iq_rs11_renamed_Ihl = iq_rs1_renamed[k];
  
  wire  [4:0]  iq_rs00_rt_slot_Ihl = iq_rs0_rt_slot[j];
  wire  [4:0]  iq_rs01_rt_slot_Ihl = iq_rs1_rt_slot[j];
  
  wire  [4:0]  iq_rs10_rt_slot_Ihl = iq_rs0_rt_slot[k];
  wire  [4:0]  iq_rs11_rt_slot_Ihl = iq_rs1_rt_slot[k];
  
  wire  [1:0]  iq_op00_mux_sel_Ihl = iq_op0_mux_sel[j];
  wire  [2:0]  iq_op01_mux_sel_Ihl = iq_op1_mux_sel[j];
  
  wire  [1:0]  iq_op10_mux_sel_Ihl = iq_op0_mux_sel[k];
  wire  [2:0]  iq_op11_mux_sel_Ihl = iq_op1_mux_sel[k];

  
  // update head
  always @(posedge clk) begin
    if (!iq_valid[iq_head] && !iq_valid[iq_head_next] && iq_head != iq_tail && iq_head_next != iq_tail) begin
      iq_head <= iq_head + 2;
    end
  end

  wire sb_src_read_head = sb_src_ready[iq_rob_fill_slot[iq_head]];
  // verilog sucks 
  wire [4:0] j =  (0%2 == 0 && iq_valid[(iq_head+0)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+0)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+0)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+1)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+1)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+1)%32])) ))?
                  (iq_head+0)%32:
                  (1%2 == 0 && iq_valid[(iq_head+1)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+1)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+1)%32])) ) && iq_valid[(iq_head+2)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+2)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+2)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+1)%32:
                  (2%2 == 0 && iq_valid[(iq_head+2)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+2)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+2)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+3)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+3)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+3)%32])) ))?
                  (iq_head+2)%32:
                  (3%2 == 0 && iq_valid[(iq_head+3)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+3)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+3)%32])) ) && iq_valid[(iq_head+4)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+4)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+4)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+3)%32:
                  (4%2 == 0 && iq_valid[(iq_head+4)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+4)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+4)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+5)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+5)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+5)%32])) ))?
                  (iq_head+4)%32:
                  (5%2 == 0 && iq_valid[(iq_head+5)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+5)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+5)%32])) ) && iq_valid[(iq_head+6)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+6)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+6)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+5)%32:
                  (6%2 == 0 && iq_valid[(iq_head+6)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+6)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+6)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+7)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+7)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+7)%32])) ))?
                  (iq_head+6)%32:
                  (7%2 == 0 && iq_valid[(iq_head+7)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+7)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+7)%32])) ) && iq_valid[(iq_head+8)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+8)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+8)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+7)%32:
                  (8%2 == 0 && iq_valid[(iq_head+8)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+8)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+8)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+9)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+9)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+9)%32])) ))?
                  (iq_head+8)%32:
                  (9%2 == 0 && iq_valid[(iq_head+9)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+9)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+9)%32])) ) && iq_valid[(iq_head+10)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+10)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+10)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+9)%32:
                  (10%2 == 0 && iq_valid[(iq_head+10)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+10)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+10)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+11)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+11)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+11)%32])) ))?
                  (iq_head+10)%32:
                  (11%2 == 0 && iq_valid[(iq_head+11)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+11)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+11)%32])) ) && iq_valid[(iq_head+12)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+12)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+12)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+11)%32:
                  (12%2 == 0 && iq_valid[(iq_head+12)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+12)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+12)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+13)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+13)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+13)%32])) ))?
                  (iq_head+12)%32:
                  (13%2 == 0 && iq_valid[(iq_head+13)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+13)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+13)%32])) ) && iq_valid[(iq_head+14)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+14)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+14)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+13)%32:
                  (14%2 == 0 && iq_valid[(iq_head+14)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+14)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+14)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+15)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+15)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+15)%32])) ))?
                  (iq_head+14)%32:
                  (15%2 == 0 && iq_valid[(iq_head+15)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+15)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+15)%32])) ) && iq_valid[(iq_head+16)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+16)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+16)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+15)%32:
                  (16%2 == 0 && iq_valid[(iq_head+16)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+16)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+16)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+17)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+17)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+17)%32])) ))?
                  (iq_head+16)%32:
                  (17%2 == 0 && iq_valid[(iq_head+17)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+17)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+17)%32])) ) && iq_valid[(iq_head+18)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+18)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+18)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+17)%32:
                  (18%2 == 0 && iq_valid[(iq_head+18)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+18)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+18)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+19)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+19)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+19)%32])) ))?
                  (iq_head+18)%32:
                  (19%2 == 0 && iq_valid[(iq_head+19)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+19)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+19)%32])) ) && iq_valid[(iq_head+20)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+20)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+20)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+19)%32:
                  (20%2 == 0 && iq_valid[(iq_head+20)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+20)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+20)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+21)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+21)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+21)%32])) ))?
                  (iq_head+20)%32:
                  (21%2 == 0 && iq_valid[(iq_head+21)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+21)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+21)%32])) ) && iq_valid[(iq_head+22)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+22)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+22)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+21)%32:
                  (22%2 == 0 && iq_valid[(iq_head+22)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+22)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+22)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+23)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+23)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+23)%32])) ))?
                  (iq_head+22)%32:
                  (23%2 == 0 && iq_valid[(iq_head+23)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+23)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+23)%32])) ) && iq_valid[(iq_head+24)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+24)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+24)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+23)%32:
                  (24%2 == 0 && iq_valid[(iq_head+24)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+24)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+24)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+25)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+25)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+25)%32])) ))?
                  (iq_head+24)%32:
                  (25%2 == 0 && iq_valid[(iq_head+25)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+25)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+25)%32])) ) && iq_valid[(iq_head+26)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+26)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+26)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+25)%32:
                  (26%2 == 0 && iq_valid[(iq_head+26)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+26)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+26)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+27)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+27)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+27)%32])) ))?
                  (iq_head+26)%32:
                  (27%2 == 0 && iq_valid[(iq_head+27)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+27)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+27)%32])) ) && iq_valid[(iq_head+28)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+28)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+28)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+27)%32:
                  (28%2 == 0 && iq_valid[(iq_head+28)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+28)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+28)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+29)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+29)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+29)%32])) ))?
                  (iq_head+28)%32:
                  (29%2 == 0 && iq_valid[(iq_head+29)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+29)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+29)%32])) ) && iq_valid[(iq_head+30)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+30)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+30)%32])) || squash_first_I_inst_Ihl))?
                  (iq_head+29)%32:
                  (30%2 == 0 && iq_valid[(iq_head+30)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+30)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+30)%32])) || squash_first_I_inst_Ihl) && iq_valid[(iq_head+31)%32] && ((sb_src_ready[iq_rob_fill_slot[(iq_head+31)%32]] && !(brj_resolved_X0hl && brj_taken_X0hl && iq_spec[(iq_head+31)%32])) ))?
                  (iq_head+30)%32:
                  5'b00000;

  wire [4:0] k = (j+1)%32;

  always @(posedge clk) begin
    if( iq_valid[j] && sb_src_ready[iq_rob_fill_slot[j]] && iq_valid[k] && sb_src_ready[iq_rob_fill_slot[k]] && iq_dequeue_rdy0 && iq_dequeue_rdy1) begin
      iq_valid[j] <= 1'b0;
      iq_spec[j] <= 1'b0;
      iq_ir[j]   <= 32'b0;
      iq_ir_squashed[j]    <= 1'b0;
      iq_rf_wen[j]   <= 1'b0;
      iq_rf_waddr[j]   <= 5'b0;
      iq_alu_fn[j]   <= 4'b0;
      iq_br_sel[j]   <= 3'b0;
      iq_j_en[j]   <= 1'b0;
      iq_pc_mux_sel[j]   <= 2'b0;
      iq_muldivreq_val[j]    <= 1'b0;
      iq_muldivreq_msg_fn[j]   <= 3'b0;
      iq_muldiv_mux_sel[j]   <= 1'b0;
      iq_execute_mux_sel[j]    <= 1'b0;
      iq_dmemreq_msg_rw[j]   <= 1'b0;
      iq_dmemreq_msg_len[j]    <= 2'b0;
      iq_dmemreq_val[j]    <= 1'b0;
      iq_dmemresp_mux_sel[j]   <= 3'b0;
      iq_memex_mux_sel[j]    <= 1'b0;
      iq_csr_wen[j]    <= 1'b0;
      iq_csr_addr[j]   <= 12'b0;

      iq_rs0_addr[j]   <= 5'b0;
      iq_rs1_addr[j]   <= 5'b0;
      iq_rs0_en[j]   <= 1'b0;
      iq_rs1_en[j]   <= 1'b0;
      iq_rob_fill_slot[j]   <= 5'b0;

      iq_rs0_renamed[j]    <= 1'b0;
      iq_rs1_renamed[j]    <= 1'b0;
      iq_rs0_rt_slot[j]    <= 5'b0;
      iq_rs1_rt_slot[j]    <= 5'b0;

      iq_op0_mux_sel[j]    <= 2'b0;
      iq_op1_mux_sel[j]    <= 3'b0;


      iq_valid[k] <= 1'b0;
      iq_spec[k] <= 1'b0;
      iq_ir[k]   <= 32'b0;
      iq_ir_squashed[k]    <= 1'b0;
      iq_rf_wen[k]   <= 1'b0;
      iq_rf_waddr[k]   <= 5'b0;
      iq_alu_fn[k]   <= 4'b0;
      iq_br_sel[k]   <= 3'b0;
      iq_j_en[k]   <= 1'b0;
      iq_pc_mux_sel[k]   <= 2'b0;
      iq_muldivreq_val[k]    <= 1'b0;
      iq_muldivreq_msg_fn[k]   <= 3'b0;
      iq_muldiv_mux_sel[k]   <= 1'b0;
      iq_execute_mux_sel[k]    <= 1'b0;
      iq_dmemreq_msg_rw[k]   <= 1'b0;
      iq_dmemreq_msg_len[k]    <= 2'b0;
      iq_dmemreq_val[k]    <= 1'b0;
      iq_dmemresp_mux_sel[k]   <= 3'b0;
      iq_memex_mux_sel[k]    <= 1'b0;
      iq_csr_wen[k]    <= 1'b0;
      iq_csr_addr[k]   <= 12'b0;

      iq_rs0_addr[k]   <= 5'b0;
      iq_rs1_addr[k]   <= 5'b0;
      iq_rs0_en[k]   <= 1'b0;
      iq_rs1_en[k]   <= 1'b0;
      iq_rob_fill_slot[k]   <= 5'b0;

      iq_rs0_renamed[k]    <= 1'b0;
      iq_rs1_renamed[k]    <= 1'b0;
      iq_rs0_rt_slot[k]    <= 5'b0;
      iq_rs1_rt_slot[k]    <= 5'b0;

      iq_op0_mux_sel[k]    <= 2'b0;
      iq_op1_mux_sel[k]    <= 3'b0;
    end
  end   

  //----------------------------------------------------------------------
  // IQ Speculation
  //----------------------------------------------------------------------  

  // When the branch resolves, if it is not taken, invalidate speculative bits,
  integer m;
  always @(posedge clk) begin
    if( brj_resolved_X0hl ) begin
      for(m = 0; m < 32; m = m + 1 ) begin
        if (iq_valid[m] && iq_spec[m] && brj_taken_X0hl) begin
          iq_valid[m] = 1'b0;
        end
        iq_spec[m]     = 1'b0;
      end
    end
  end

endmodule

`endif

