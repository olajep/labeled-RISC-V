# usage: hsi -nojournal -nolog -source [this tcl file] -tclargs [hdf file] [prj-brd]

if {[llength $argv] > 0} {
  set hdf_file [lindex $argv 0]
  set project_name [lindex $argv 1]
} else {
  puts "hdf file path is not given!"
  return 1
}

set s [split $project_name -]
set prj [lindex $s 0]
set brd [lindex $s 1]

set script_dir [file normalize [file dirname [info script]]]
set build_dir ${script_dir}/build/${project_name}

if {$brd == "ultraZ"} {
  set device_tree_repo_path "/home/yzh/xilinx/ultraZ/device-tree-xlnx"
} else {
  set device_tree_repo_path ${script_dir}/device-tree-xlnx
}

set hw_design [open_hw_design ${hdf_file}]

switch -regexp -- $brd {
  zedboard {
    set processor ps7_cortexa9_0
    set brd_version zedboard
    set arch zynq
  }
  zcu102|sidewinder|ultraZ {
    set processor psu_cortexa53_0
    set brd_version zcu102-rev1.0
    set arch zynqmp

    generate_app -hw $hw_design -os standalone -proc psu_pmu_0 -app zynqmp_pmufw -compile -sw pmufw -dir ${build_dir}/pmufw
    exec mkdir -p ${script_dir}/build/${arch}
    exec ln -sf ${build_dir}/pmufw/executable.elf ${script_dir}/build/${arch}/pmufw.elf
  }
  default {
    puts "Unsupported board $brd"
    return 1
  }
}

generate_app -hw $hw_design -os standalone -proc $processor -app ${arch}_fsbl -compile -sw fsbl -dir ${build_dir}/fsbl

exec mkdir -p ${script_dir}/build/${arch}
exec ln -sf ${build_dir}/fsbl/executable.elf ${script_dir}/build/${arch}/fsbl.elf
exec bootgen -arch ${arch} -image ${script_dir}/bootgen-${arch}.bif -w -o i ${build_dir}/BOOT.BIN

#device tree
set_repo_path ${device_tree_repo_path}
create_sw_design device-tree -os device_tree -proc $processor
if {$brd != "ultraZ"} {
  set_property CONFIG.periph_type_overrides "{BOARD ${brd_version}}" [get_os]
}
generate_target -dir ${build_dir}/dts
exec -ignorestderr dtc -i ${build_dir}/dts -I dts -O dtb ${script_dir}/dts/${brd}.dts -o ${build_dir}/system.dtb

exit
