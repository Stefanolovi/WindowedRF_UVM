This is repo includes Windowed RF source files and Testbench files. To navigate through this project:   


- SRC: includes all .vhd files necessary to compile my version of the RF. 

- TB  : includes test files. I have decided to include all test components in a single package in a single file. I know this is not good for reusability but I find it more convenient for the writing code process. I can change this in the near future if necessary. 
    - RF_if_wrap includes the interface and the wrapper of the DUT. 
    - RF_testbench includes the package with all test components as well as the top module that runs the test. 
- SIM contains the work library and necessary files for simulation, coverage etc.


# How to compile and Simulate  
1. Compile the DUT's source file: vcom -F compile_vhd.f
2. Compile the test files: vlog -F compile_sv.f
3. Simulate the test: vsim -c -sv_seed random -do sim.do
