#=======================================================================
# Makefile for Verilog simulation w/ VCS
#-----------------------------------------------------------------------
# Yunsup Lee (yunsup@cs.berkeley.edu)
#
# This makefile will build a rtl simulator and run various tests to
# verify proper functionality.
#

default: verilog devicetree bd

fpga_dir = $(abspath ..)
base_dir = $(fpga_dir)/..
build_dir = $(fpga_dir)/build
generated_dir = $(build_dir)/generated-src
dts_dir  = dts

MODEL ?= TestHarness
BOARD ?= zcu102
NAME = TraceFPGA
CONFIG = $(NAME)Config$(BOARD)
TOP = $(NAME)Top

#--------------------------------------------------------------------
# Rocket-chip verilog source generation
#--------------------------------------------------------------------

-include $(base_dir)/Makefrag

gen_rtl = $(generated_dir)/$(long_name).v
srams_rtl = $(generated_dir)/$(long_name).behav_srams.v
other_rtl = $(base_dir)/src/main/resources/vsrc/AsyncResetReg.v \
            $(base_dir)/src/main/resources/vsrc/plusarg_reader.v
dts       = pardcore-$(BOARD)-top.dts
dtb       = $(dts:.dts=.dtb)
bd        = bd/pardcore.tcl


$(gen_rtl):
	$(MAKE) verilog -C $(base_dir)/vsim CONFIG=$(CONFIG) MODEL=$(MODEL) generated_dir=$(generated_dir)

$(srams_rtl): $(gen_rtl)

rocketchip_rtl = rtl/rocket/rocketchip_board_$(BOARD).v

$(rocketchip_rtl): $(gen_rtl) $(srams_rtl) $(other_rtl)
	cat $^ > $@
	sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@

verilog: $(rocketchip_rtl)

$(build_dir)/%.dtb: $(dts_dir)/%.dts
	dtc -i $(generated_dir) -I dts -O dtb $< -o $@

devicetree: $(build_dir)/$(dtb)

bd: $(bd)
$(bd): $(bd).in
	rm -f $@
	echo "# WARNING THIS FILE IS MACHIME GENERATED. DO NOT EDIT!" > $@
	sed 's/@TOPNAME@/$(TOP)/g' $< >> $@


.PHONY: $(gen_rtl) verilog devicetree bd

#--------------------------------------------------------------------
# Cleaning
#--------------------------------------------------------------------

clean:
	-rm -rf $(generated_dir)/freechips.rocketchip.system.$(CONFIG)*
	-rm -rf $(rocketchip_rtl)
	-rm -rf $(build_dir)/$(dtb)
	-rm -rf $(bd)

.PHONY: default clean
