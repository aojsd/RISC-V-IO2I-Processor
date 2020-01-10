//=========================================================================
// 5-Stage RISCV Scoreboard
//=========================================================================

`ifndef RISCV_CORE_REORDERBUFFER_V
`define RISCV_CORE_REORDERBUFFER_V

module riscv_CoreReorderBuffer
(
  input         clk,
  input         reset,

  input         rob_alloc_req_val,
  output        rob_alloc_req_rdy,
  input  [ 4:0] rob_alloc_req_preg,
  
  output [ 3:0] rob_alloc_resp_slot,

  input         rob_fill_val,
  input  [ 3:0] rob_fill_slot,

  output        rob_commit_wen,
  output [ 3:0] rob_commit_slot,
  output [ 4:0] rob_commit_rf_waddr
);

  // ** ROB STATE BEGIN ** \\
  reg[3:0] rob_head_ptr = 4'b0; // Always commit from head first
  reg[3:0] rob_tail_ptr = 4'b0; // Always alloc  from tail first
  reg[4:0] rob_waddr[0:16]; 
  reg rob_val[0:16]; 
  reg rob_pending[0:16]; 


  // ** HELPER WIRES ** \\
  wire is_rob_commit = (rob_val[rob_head_ptr] && !rob_pending[rob_head_ptr]);
  wire is_rob_alloc  = (rob_alloc_req_val && rob_alloc_req_rdy);
  // ** INPUT AND OUTPUT WIRES ** \\
  wire rob_alloc_req_rdy   = !rob_val[rob_tail_ptr]; // As long as the tail is not valid (i.e. empty), we are able to accept new alloc
  wire rob_alloc_resp_slot = rob_tail_ptr; // We always alloc tail for new entries
  wire rob_commit_wen      = is_rob_commit; // We only say wen when the head is commitable
  wire rob_commit_rf_waddr = rob_waddr[rob_head_ptr]; 
  wire rob_commit_slot     = rob_head_ptr;

  // ** RESET ** \\
  integer i;
  always @(posedge reset) begin
    rob_head_ptr <= 4'b0000;
    rob_tail_ptr <= 4'b0000;
    for (i = 0; i <= 4'b1111; i = i+1) begin
      rob_waddr[i] <= 5'b00000;
      rob_val[i] <= 1'b0;
      rob_pending[i] <= 1'b0;
    end
    clk_cnt <= 10'b0;
  end

  reg[10:0] clk_cnt;
  always @(posedge clk) begin
    clk_cnt <= clk_cnt + 1;
    // On new alloc request
    if (is_rob_alloc) begin
      // Update rob state
      rob_waddr[rob_tail_ptr]   <= rob_alloc_req_preg;
      rob_val[rob_tail_ptr]     <= 1'b1;
      rob_pending[rob_tail_ptr] <= 1'b1;

      // increment tail ptr (should automatically handle modulo)
      rob_tail_ptr <= rob_tail_ptr + 1'b1;
    end

    // Fill in rob 
    if (rob_fill_val) begin
      rob_pending[rob_fill_slot] <= 1'b0;
    end

    // Commit head of rob (we implicitly assume that commit never stalls - this should hold true)
    if (is_rob_commit) begin
      // Clear rob state
      rob_val[rob_head_ptr]     <= 1'b0;
      rob_pending[rob_head_ptr] <= 1'b0;
      rob_waddr[rob_head_ptr]   <= 5'b00000;

      // Increment head ptr
      rob_head_ptr <= rob_head_ptr + 1'b1;
    end
  end
endmodule

`endif

