if {[llength $argv] > 0} {
  set project_name [lindex $argv 0]
  set s [split $project_name -]
  set prj [lindex $s 0]
  set brd [lindex $s 1]
} else {
  puts "project full name is not given!"
  return 1
}

set script_dir  [file dirname [info script]]
set project_dir ${script_dir}/${brd}/build/$project_name

###########################################################
# OPEN PROJECT
###########################################################
open_project ${project_dir}/${project_name}.xpr
reset_project

###########################################################
# SYNTHESIS
###########################################################
set_property strategy "Flow_RuntimeOptimized" [get_runs synth_1]
launch_runs synth_1
wait_on_run synth_1
open_run synth_1
report_timing_summary -file ${project_dir}/timing_synth.log

###########################################################
# PLACE AND ROUTE
###########################################################
set_property strategy "Flow_RuntimeOptimized" [get_runs impl_1]
launch_runs impl_1
wait_on_run impl_1
open_run impl_1
report_timing_summary -file ${project_dir}/timing_impl.log

###########################################################
# GENERATE BITSTREAM
###########################################################
set_property STEPS.WRITE_BITSTREAM.ARGS.BIN_FILE true [get_runs impl_1]
launch_runs impl_1 -to_step write_bitstream
wait_on_run impl_1

###########################################################
# EXPORT PROJECT TO SDK
###########################################################
set bit_filename [lindex [glob -dir "${project_dir}/${project_name}.runs/impl_1" *.bit] 0]
set bit_filename_only [lindex [split ${bit_filename} /] end]
set top_module_name [lindex [split ${bit_filename_only} .] 0]
set export_dir "${project_dir}/${project_name}.sdk"
file mkdir ${export_dir}
write_sysdef -force \
  -hwdef "${project_dir}/${project_name}.runs/impl_1/${top_module_name}.hwdef" \
  -bitfile "${project_dir}/${project_name}.runs/impl_1/${top_module_name}.bit" \
  -meminfo "${project_dir}/${project_name}.runs/impl_1/${top_module_name}.mmi" \
  ${export_dir}/${top_module_name}.hdf
