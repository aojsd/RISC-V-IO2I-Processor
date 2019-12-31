//=========================================================================
// IO2I 2-Wide RISC-V Reorder Buffer
//=========================================================================

`ifndef RISCV_CORE_REORDERBUFFER_V
`define RISCV_CORE_REORDERBUFFER_V

module riscv_CoreReorderBuffer
(
  input         clk,
  input         reset,

  input         rob_alloc_req_val_1,
  input  [ 4:0] rob_alloc_req_preg_1,
  input         rob_alloc_req_val_2,
  input  [ 4:0] rob_alloc_req_preg_2,

  output        rob_alloc_req_rdy_1,
  output        rob_alloc_req_rdy_2,
  
  output [ 3:0] rob_alloc_resp_slot_1,
  output [ 3:0] rob_alloc_resp_slot_2,

  input         rob_fill_val_1,
  input  [ 3:0] rob_fill_slot_1,
  input         rob_fill_val_2,
  input  [ 3:0] rob_fill_slot_2,

  output        rob_commit_wen_1,
  output [ 3:0] rob_commit_slot_1,
  output [ 4:0] rob_commit_rf_waddr_1,
  output        rob_commit_wen_2,
  output [ 3:0] rob_commit_slot_2,
  output [ 4:0] rob_commit_rf_waddr_2
);

  reg rob_alloc_req_rdy_1;
  reg rob_alloc_resp_slot_1;
  reg rob_commit_wen_1;
  reg rob_commit_rf_waddr_1;
  reg rob_commit_slot_1;

  reg rob_alloc_req_rdy_2;
  reg rob_alloc_resp_slot_2;
  reg rob_commit_wen_2;
  reg rob_commit_rf_waddr_2;
  reg rob_commit_slot_2;

  reg       rob_valid   [15:0];
  reg       rob_pending [15:0];
  reg       rob_wen     [15:0];
  reg [4:0] rob_preg    [15:0];

  reg [3:0] rob_head;
  reg [3:0] rob_tail;
  integer i;

  //----------------------------------------------------------------------  
  // Reset Case
  //----------------------------------------------------------------------

  always @(posedge clk) begin
    if (reset) begin
      rob_alloc_req_rdy_1   <= 1'b1;
      rob_alloc_resp_slot_1 <= 4'b0;
      rob_commit_wen_1      <= 1'b0;
      rob_commit_slot_1     <= 4'b0;
      rob_commit_rf_waddr_1 <= 5'b0;

      rob_alloc_req_rdy_2   <= 1'b1;
      rob_alloc_resp_slot_2 <= 4'b0;
      rob_commit_wen_2      <= 1'b0;
      rob_commit_slot_2     <= 4'b0;
      rob_commit_rf_waddr_2 <= 5'b0;

      rob_head    <= 4'b0;
      rob_tail    <= 4'b0;

      rob_entry_allocated_1 <= 1'b0;
      rob_entry_allocated_2 <= 1'b0;

      rob_entry_committed_1 <= 1'b0;
      rob_entry_committed_2 <= 1'b0;

      for (i = 0; i < 16; i = i + 1) begin
        rob_valid[i]    <= 1'b0;
        rob_pending[i]  <= 1'b0;  
        rob_preg[i]     <= 5'b0;
      end
    end
  end

  //----------------------------------------------------------------------
  // ROB Alloc
  //----------------------------------------------------------------------

  // On a ROB allocation, the ROB slot must be returned before the end of
  // the clock cycle. The actual changes to the entry may be changed on
  // the next clock edge.
  // The ROB is only not ready for allocation if the head and tail pointers
  // are equal and the entry they point to is valid.

  reg rob_entry_allocated_1;
  reg rob_entry_allocated_2;

  always @(*) begin

    // One slot is open if the head is not equal to the tail pointer, or if 
    // they are equal, but the entry at the head pointer is invalid.
    // The second slot is open only if the first slot is open, and there is
    // another open slot.

    rob_alloc_req_rdy_1 = (rob_head != rob_tail) ||
                          ( (rob_head == rob_tail) && !rob_valid[rob_head] );
    rob_alloc_req_rdy_2 = rob_alloc_req_rdy_1 && (rob_head != rob_tail + 1);

    // Request 2 should only be valid if two allocations are being done this
    // cycle
    if(rob_alloc_req_val_2 && rob_alloc_req_rdy_2) begin
      rob_alloc_resp_slot_1 = rob_tail;
      rob_alloc_resp_slot_2 = rob_tail + 1;

      rob_entry_allocated_1 = 1'b0;
      rob_entry_allocated_2 = 1'b1;
    end
    else if(rob_alloc_req_val_1 && rob_alloc_req_rdy_1) begin
      rob_alloc_resp_slot_1 = rob_tail;

      rob_entry_allocated_1 = 1'b1;
      rob_entry_allocated_2 = 1'b0;
    end
    else begin
      rob_alloc_resp_slot_1 = 4'b0;
      rob_alloc_resp_slot_2 = 4'b0;
      
      rob_entry_allocated_1 = 1'b0;
      rob_entry_allocated_2 = 1'b0;
    end
  end

  always @(posedge clk) begin
    if (rob_entry_allocated_2) begin
      rob_valid[rob_tail]   <= 1'b1;
      rob_pending[rob_tail] <= 1'b1;
      rob_preg[rob_tail]    <= rob_alloc_req_preg_1;

      rob_valid[rob_tail]   <= 1'b1;
      rob_pending[rob_tail] <= 1'b1;
      rob_preg[rob_tail]    <= rob_alloc_req_preg_2;

      rob_tail              <= rob_tail + 2;
    end
    else if (rob_entry_allocated_1) begin
      rob_valid[rob_tail]   <= 1'b1;
      rob_pending[rob_tail] <= 1'b1;
      rob_preg[rob_tail]    <= rob_alloc_req_preg_1;

      rob_tail              <= rob_tail + 1;
    end
  end

  //----------------------------------------------------------------------
  // ROB Fill
  //----------------------------------------------------------------------

  // When a result is written to the ROB, flip the pending bit of that slot
  // on the next clock edge.

  always @(posedge clk) begin
    if(rob_fill_val_1) begin
      rob_pending[rob_fill_slot_1] <= 1'b0;
    end
    if(rob_fill_val_2) begin
      rob_pending[rob_fill_slot_2] <= 1'b0;
    end
  end

  //----------------------------------------------------------------------
  // ROB Commit
  //----------------------------------------------------------------------  

  // A result in the ROB is ready to be committed when the entry at the
  // head pointer is both valid and not pending. The output information
  // should be set before the end of the clock cycle, but the head pointer
  // can be iterated at the next clock edge.

  reg rob_entry_committed_1;
  reg rob_entry_committed_2;

  always @(*) begin

    // Since commits occur in order, check the head pointer first, checking
    // the entry after only if the first entry is committed

    if(rob_valid[rob_head] && !rob_pending[rob_head]) begin
      rob_commit_slot_1     = rob_head;
      rob_commit_rf_waddr_1 = rob_preg[rob_head];
      rob_commit_wen_1      = 1'b1;

      // Check to commit second instruction as well

      if(rob_valid[rob_head + 1] && !rob_pending[rob_head + 1]) begin
        rob_commit_slot_2     = rob_head + 1;
        rob_commit_rf_waddr_2 = rob_preg[rob_head + 1];
        rob_commit_wen_2      = 1'b1;

        rob_entry_committed_1 = 1'b0;
        rob_entry_committed_2 = 1'b1;
      end
      else begin
        rob_commit_slot_2     = 4'b0;
        rob_commit_rf_waddr_2 = 5'b0;
        rob_commit_wen_2      = 1'b0;

        rob_entry_committed_1 = 1'b1;
        rob_entry_committed_2 = 1'b0;
      end
    end
    else begin
      rob_commit_slot_1     = 4'b0;
      rob_commit_rf_waddr_1 = 5'b0;
      rob_commit_wen_1      = 1'b0;

      rob_commit_slot_2     = 4'b0;
      rob_commit_rf_waddr_2 = 5'b0;
      rob_commit_wen_2      = 1'b0;
    end
  end

  always @(posedge clk) begin
    if(rob_entry_committed_2) begin
      rob_valid[rob_head]     <= 1'b0;
      rob_valid[rob_head + 1] <= 1'b0;

      rob_head                <= rob_head + 2;
    end
    else if(rob_entry_committed_1) begin
      rob_valid[rob_head] <= 1'b0;
      rob_head            <= rob_head + 1;
    end
  end

endmodule

`endif

