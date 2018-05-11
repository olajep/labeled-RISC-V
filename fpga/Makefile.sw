# check RISCV environment variable
ifndef RISCV
$(error Please set environment variable RISCV. Please take a look at README)
endif

build_dir = $(realpath ./build)
SW_PATH = $(abspath ../../sw)

$(SW_PATH):
	@echo "Do you want to put all software repos under $(SW_PATH) (You can modify 'SW_PATH' in Makefile.sw)? [y/n]"
	@read r; test $$r = "y"
	mkdir -p $(SW_PATH)

#--------------------------------------------------------------------
# Build tools
#--------------------------------------------------------------------

RISCV_PREFIX=riscv64-unknown-linux-gnu-
CC = $(RISCV_PREFIX)gcc
LD = $(RISCV_PREFIX)ld
RISCV_COPY = $(RISCV_PREFIX)objcopy
RISCV_COPY_FLAGS = --set-section-flags .bss=alloc,contents --set-section-flags .sbss=alloc,contents -O binary

#--------------------------------------------------------------------
# BBL variables
#--------------------------------------------------------------------

BBL_REPO_PATH = $(SW_PATH)/riscv-pk
BBL_BUILD_COMMIT = 9da7d98e851570e42488573697daca762ad01029

BBL_BUILD_PATH = $(BBL_REPO_PATH)/build
BBL_ELF_BUILD = $(BBL_BUILD_PATH)/bbl

BBL_PAYLOAD = $(LINUX_ELF)
BBL_CONFIG = --host=riscv64-unknown-linux-gnu --with-payload=$(BBL_PAYLOAD) --enable-logo
BBL_CFLAGS = -msoft-float -march=RV64IMAC

BBL_ELF = $(build_dir)/bbl.elf
BBL_BIN = $(build_dir)/linux.bin

#--------------------------------------------------------------------
# Linux variables
#--------------------------------------------------------------------

LINUX_REPO_PATH = $(SW_PATH)/riscv-linux
LINUX_BUILD_COMMIT = b38a9a276851e2d34de99393ad0ef3faa89458af

LINUX_ELF_BUILD = $(LINUX_REPO_PATH)/vmlinux
LINUX_ELF = $(build_dir)/vmlinux

ROOTFS_PATH = $(LINUX_REPO_PATH)/arch/riscv/rootfs

#--------------------------------------------------------------------
# BBL rules
#--------------------------------------------------------------------

bbl: $(BBL_BIN)

$(BBL_BIN): $(BBL_ELF)
	$(RISCV_COPY) $(RISCV_COPY_FLAGS) $< $@
	truncate -s \%8 $@

$(BBL_ELF): $(BBL_ELF_BUILD)
	ln -sf $(abspath $<) $@

$(BBL_REPO_PATH): | $(SW_PATH)
	mkdir -p $@
	git clone https://github.com/LvNA-system/riscv-pk.git $@

$(BBL_BUILD_PATH): $(BBL_PAYLOAD) | $(BBL_REPO_PATH)
	mkdir -p $@
	cd $@ && \
		echo git checkout $(BBL_BUILD_COMMIT) && \
		($(BBL_REPO_PATH)/configure $(BBL_CONFIG) || (echo git checkout @{-1}; false)) && \
		echo git checkout @{-1}

$(BBL_ELF_BUILD): | $(BBL_BUILD_PATH)
	cd $(@D) && \
		echo git checkout $(BBL_BUILD_COMMIT) && \
		(CFLAGS="$(BBL_CFLAGS)" $(MAKE) || (echo git checkout @{-1}; false)) && \
		echo git checkout @{-1}

bbl-clean:
	-rm $(BBL_ELF) $(BBL_BIN)
	-$(MAKE) clean -C $(BBL_BUILD_PATH)

.PHONY: bbl bbl-clean $(BBL_ELF_BUILD)

#--------------------------------------------------------------------
# Linux rules
#--------------------------------------------------------------------

$(LINUX_REPO_PATH): | $(SW_PATH)
	mkdir -p $@
	git clone https://github.com/LvNA-system/riscv-linux.git $@
	cd $@ && (curl -L https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.6.2.tar.xz | tar -xJ --strip-components=1) && echo git checkout . && cp arch/riscv/configs/riscv64_pard .config && make ARCH=riscv menuconfig

linux: $(LINUX_ELF)

$(LINUX_ELF): $(LINUX_ELF_BUILD)
	ln -sf $(abspath $<) $@

$(LINUX_ELF_BUILD): | $(LINUX_REPO_PATH) 
	cd $(@D) && \
		echo git checkout $(LINUX_BUILD_COMMIT) && \
		(($(MAKE) -C $(ROOTFS_PATH) && $(MAKE) ARCH=riscv vmlinux) || (echo git checkout @{-1}; false)) && \
		echo git checkout @{-1}


linux-clean:
	-rm $(LINUX_ELF)
	-$(MAKE) clean -C $(LINUX_REPO_PATH)

.PHONY: linux linux-clean $(LINUX_ELF_BUILD)


#--------------------------------------------------------------------
# Software top-level rules
#--------------------------------------------------------------------

sw: bbl

sw-clean: bbl-clean linux-clean
	-$(MAKE) -C $(ROOTFS_PATH) clean

.PHONY: sw sw-clean
