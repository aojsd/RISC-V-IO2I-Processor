//=========================================================================
// 5-Stage RISCV Scoreboard
//=========================================================================

`ifndef RISCV_CORE_ISSUEQUEUE_V
`define RISCV_CORE_ISSUEQUEUE_V

`include "riscvio2i-InstMsg.v"

module riscv_CoreIssueQueue
(
  input         clk,
  input         reset,

  input         iq_enqueue_val,
  output        iq_enqueue_rdy,
  
  input [4:0]       rs1_Dhl,
  input [4:0]       rs2_Dhl,
  input [4:0]       rd_Dhl,
  input [31:0]      ir_Dhl,
  input [`RISCV_INST_MSG_CS_SZ-1:0] cs_Dhl,
  input [31:0] pc_Dhl,
  input [31:0] pc_plus4_Dhl,
  input [3:0] rob_fill_slot_Dhl,

  output        iq_dequeue_val,
  input         iq_dequeue_rdy,
  input         iq_is_currently_issueable,
  output [5:0]  iq_dequeue_slot,
  input [5:0]   iq_jmp_clear_slot,
  input         iq_jump_clear_val,

  output [4:0]       rs1_Ihl,
  output [4:0]       rs2_Ihl,
  output [4:0]       rd_Ihl,
  output [31:0]      ir_Ihl,
  output [31:0]      pc_Ihl,
  output [31:0]      pc_plus4_Ihl,
  output [3:0] rob_fill_slot_Ihl,
  
  output [`RISCV_INST_MSG_CS_SZ-1:0] cs_Ihl
);
  localparam NUM_SLOTS = 6'b111111;
  // ** ROB STATE BEGIN ** \\
  reg[5:0] iq_head_ptr = 4'b0; // Always commit from head first
  reg[5:0] iq_tail_ptr = 4'b0; // Always alloc  from tail first
  reg iq_val[0:NUM_SLOTS]; 

  reg[4:0] iq_rs1[0:NUM_SLOTS];
  reg[4:0] iq_rs2[0:NUM_SLOTS];
  reg[4:0] iq_rd[0:NUM_SLOTS];
  reg[32:0] iq_ir[0:NUM_SLOTS]; // FIXME: FIGURE OUT WHY ONLY 33+ bits WORKS FOR THIS?? (Theoretically, 32 bits/entry should be enough??)
  reg[`RISCV_INST_MSG_CS_SZ:0] iq_cs[0:NUM_SLOTS];
  reg[32:0] iq_pc[0:NUM_SLOTS];
  reg[32:0] iq_pc_plus4[0:NUM_SLOTS];
  reg[4:0] iq_rob_fill_slot_Dhl[0:NUM_SLOTS];

  // ** HELPER WIRES ** \\
  wire is_iq_full = (iq_val[iq_tail_ptr]);
  wire is_iq_enqueue = (iq_enqueue_val && iq_enqueue_rdy);
  wire is_iq_dequeue = (iq_dequeue_rdy && (iq_head_ptr != iq_tail_ptr || !iq_val[iq_head_ptr]));
  // ** INPUT AND OUTPUT WIRES ** \\
  wire iq_enqueue_rdy     = !is_iq_full;

  // ** RESET ** \\
  integer i = 0;
  always @(posedge reset) begin
    iq_head_ptr <= 4'b0000;
    iq_tail_ptr <= 4'b0000;
    for (i = 0; i <= NUM_SLOTS; i = i+1) begin
      iq_val[i] <= 1'b0;
      iq_rs1[i] <= 5'b00000;
      iq_rs2[i] <= 5'b00000;
      iq_rd[i] <= 5'b00000;
      iq_ir[i] <= {32{1'bx}};
      iq_cs[i] <= {`RISCV_INST_MSG_CS_SZ{1'bx}};
      iq_pc[i] <= {32{1'b0}};
      iq_pc_plus4[i] <= {32{1'b0}};
      iq_rob_fill_slot_Dhl[i] <= 4'b1111;
    end
  end

  integer j = 0;
  integer finished = 0;
  wire [4:0] rs1_Ihl = iq_rs1[j];
  wire [4:0] rs2_Ihl = iq_rs2[j];
  wire [4:0] rd_Ihl  = iq_rd[j];
  wire [31:0] ir_Ihl  = iq_ir[j];
  wire [`RISCV_INST_MSG_CS_SZ-1:0] cs_Ihl  = iq_cs[j];
  wire iq_dequeue_val= iq_val[j];
  wire [31:0] pc_Ihl = iq_pc[j];
  wire [31:0] pc_plus4_Ihl = iq_pc_plus4[j];
  wire [3:0] rob_fill_slot_Ihl = iq_rob_fill_slot_Dhl[j];

  wire [5:0] iq_dequeue_slot = j;
  wire iq_val_head = iq_val[iq_head_ptr];
  always @(posedge clk) begin
    // On new alloc request
    if (is_iq_enqueue) begin
      iq_val[iq_tail_ptr]     <= 1'b1;
      iq_rs1[iq_tail_ptr]     <= rs1_Dhl;
      iq_rs2[iq_tail_ptr]     <= rs2_Dhl;
      iq_rd[iq_tail_ptr]      <= rd_Dhl;
      iq_ir[iq_tail_ptr]      <= ir_Dhl;
      iq_cs[iq_tail_ptr]      <= cs_Dhl;
      iq_pc[iq_tail_ptr]      <= pc_Dhl;
      iq_pc_plus4[iq_tail_ptr] <= pc_plus4_Dhl;
      iq_rob_fill_slot_Dhl[iq_tail_ptr] <= rob_fill_slot_Dhl;
      // increment tail ptr (should automatically handle modulo)
      iq_tail_ptr <= iq_tail_ptr + 1'b1;
    end

    if (iq_jump_clear_val) begin
      // clear iq slots after jump slot
      for (j = iq_jmp_clear_slot ; j != iq_head_ptr ; j = (j+1) % 64) begin
        iq_val[j] <= 1'b0;
      end
    end
    // Commit head of iq (we implicitly assume that commit never stalls - this should hold true)
    if (is_iq_dequeue) begin
      finished = 0;
      //scan iq for dequeable entries
      for (j = iq_head_ptr; j != iq_tail_ptr && finished == 0; j = (j+1) % 64) begin
        `define is_j_inbetween ((iq_head_ptr < iq_jmp_clear_slot) && (j>= iq_head_ptr && j < iq_jmp_clear_slot)) || ((iq_head_ptr > iq_jmp_clear_slot) && (j >= iq_head_ptr || j < iq_jmp_clear_slot))
        if(iq_val[j] && iq_is_currently_issueable && (!iq_jump_clear_val || (`is_j_inbetween))) begin
          iq_val[j] <= 1'b0;
          finished = 1;
          // increment head pointer
          if (j == iq_head_ptr || !iq_val[iq_head_ptr]) begin
            iq_head_ptr <= iq_head_ptr + 1'b1;
          end
        end
        // $display("head_ptr %x c %x jval: %d isfinished %d",iq_head_ptr, pc_Dhl, j,finished);
      end
      
    end
    // if (finished == 0) begin
    //   finished = 2;
    // end
  end
endmodule

`endif

