puts "Reading core tcl file"

# Core will run out of M9Ks without this due to amount needed
# for the current 65k 4bit RAM
set_global_assignment -name ALLOW_ANY_ROM_SIZE_FOR_RECOGNITION OFF
set_global_assignment -name AUTO_ROM_RECOGNITION OFF
