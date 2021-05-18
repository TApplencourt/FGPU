#  Mandatory Boilerplate
source utils/set_env.bash
teardown() {
 load ${DGA_TESTBED}/utils/common_functions.bash
 test_exit
 }
setup(){
  load ${DGA_TESTBED}/utils/common_functions.bash
  load_oneapi
}
 
#  User code
@test "$_self" {
  test_init
  
  ifx -fiopenmp -fopenmp-targets=spir64 t.f90
 
}
