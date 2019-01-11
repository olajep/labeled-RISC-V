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

PRJ_ROOT = board/$(BOARD)/build/$(PRJ_FULL)
XPR_FILE = $(PRJ_ROOT)/$(PRJ_FULL).xpr
$(XPR_FILE): pardcore
	vivado $(VIVADO_FLAG) -mode batch -source board/$(BOARD)/mk.tcl -tclargs $(PRJ_FULL)

bitstream: $(XPR_FILE)
	vivado $(VIVADO_FLAG) -mode batch -source board/build.tcl -tclargs $(PRJ_FULL)

pardcore:
	make -C pardcore BOARD=$(BOARD)

project: $(XPR_FILE)

bootgen:
	cd boot && hsi $(VIVADO_FLAG) -source mk.tcl -tclargs $(abspath $(PRJ_ROOT)/$(PRJ_FULL).sdk/system_top.hdf) $(PRJ_FULL)

vivado: $(XPR_FILE)
	vivado $(VIVADO_FLAG) $(XPR_FILE) &

.PHONY: default project vivado bootgen pardcore bitstream

include Makefile.sw
