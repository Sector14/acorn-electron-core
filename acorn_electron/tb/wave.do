onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_clk_sys
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_ena_sys
add wave -noupdate -format Literal /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_cph_sys
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_rst_sys
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_clk_ram
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_rst_ram
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_clk_vid
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_rst_vid
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_halt
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Literal -expand /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/o_vid_sync
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/o_disk_led
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/o_pwr_led
add wave -noupdate -format Literal /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_joy_a_l
add wave -noupdate -format Literal /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_joy_b_l
add wave -noupdate -format Logic /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_kb_ps2_we
add wave -noupdate -format Literal /a_replay_example_tb/u_replay_tb_wrap/u_replay/u_core/u_core/i_kb_ps2_data
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {5320786 ps} 0}
configure wave -namecolwidth 340
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 2
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
update
WaveRestoreZoom {0 ps} {33400576 ps}
