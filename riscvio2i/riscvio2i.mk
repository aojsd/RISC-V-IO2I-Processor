#=========================================================================
# riscvio2i Subpackage
#=========================================================================

riscvio2i_deps = \
  vc \
  imuldiv \
  pcache \

riscvio2i_srcs = \
  riscvio2i-CoreDpath.v \
  riscvio2i-CoreDpathRegfile.v \
  riscvio2i-CoreDpathAlu.v \
  riscvio2i-CoreScoreboard.v \
  riscvio2i-CoreReorderBuffer.v \
  riscvio2i-CoreIssueQueue.v \
  riscvio2i-CoreCtrl.v \
  riscvio2i-Core.v \
  riscvio2i-InstMsg.v \

riscvio2i_test_srcs = \
  riscvio2i-InstMsg.t.v \

riscvio2i_prog_srcs = \
  riscvio2i-sim.v \
  riscvio2i-randdelay-sim.v \

