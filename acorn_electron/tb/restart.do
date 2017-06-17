proc myrestart {} {
variable cur
quietly set cur [wave cursortime]
puts $cur
restart -force


run $cur
run 1 us
wave cursortime -time $cur
wave zoomfull
wave zoomcursor 20
}

view wave
formatTime +bestunits

_add_menu .main_pane.mdi.interior.cs.vm.paneset.cli_0.wf.clip.cs controls right SystemButtonFace red MyRun myrestart
