# Default Vivado board & project names
BOARD ?= zcu102
PRJ ?= myproject
PRJ_FULL = $(PRJ)-$(BOARD)

include Makefile.check

default: project

VIVADO_FLAG = -nolog -nojournal -notrace

#--------------------------------------------------------------------
# Project building and implementation
#--------------------------------------------------------------------

PARDCORE_RTL = pardcore/rtl/rocket/rocketchip_board_$(BOARD).v
PRJ_ROOT = board/$(BOARD)/build/$(PRJ_FULL)
XPR_FILE = $(PRJ_ROOT)/$(PRJ_FULL).xpr
BIT_FILE = $(PRJ_ROOT)/$(PRJ_FULL).runs/impl_1/system_top.bit
HWDEF_FILE = $(abspath $(PRJ_ROOT)/$(PRJ_FULL).sdk/system_top.hdf)

pardcore_rtl_deps = \
	$(shell find ../src/main/scala -name *.scala) \
	pardcore/dts/pardcore-$(BOARD)-top.dts
$(PARDCORE_RTL): $(pardcore_rtl_deps)
	make -C pardcore BOARD=$(BOARD)

xpr_file_deps = \
	board/common.tcl \
	$(shell find board/$(BOARD) -maxdepth 2 -type f -regex ".*\.\(tcl\|v|\xdc\)") \
	pardcore/bd/pardcore.tcl \
	$(PARDCORE_RTL)
$(XPR_FILE): $(xpr_file_deps)
	vivado $(VIVADO_FLAG) -mode batch -source board/$(BOARD)/mk.tcl -tclargs $(PRJ_FULL)

$(BIT_FILE): board/build.tcl $(XPR_FILE)
	vivado $(VIVADO_FLAG) -mode batch -source board/build.tcl -tclargs $(PRJ_FULL)

$(HWDEF_FILE): $(BIT_FILE)

pardcore: $(PARDCORE_RTL)

project: $(XPR_FILE)

bitstream: $(BIT_FILE)

bootgen: $(HWDEF_FILE)
	cd boot && hsi $(VIVADO_FLAG) -source mk.tcl -tclargs $(HWDEF_FILE) $(PRJ_FULL)

vivado: $(XPR_FILE)
	vivado $(VIVADO_FLAG) $(XPR_FILE) &

clean:
	make -C pardcore clean BOARD=$(BOARD)
	rm -rf $(PRJ_ROOT)
	rm -rf boot/build/$(PRJ_FULL)

.PHONY: default project vivado bootgen pardcore bitstream

include Makefile.sw
