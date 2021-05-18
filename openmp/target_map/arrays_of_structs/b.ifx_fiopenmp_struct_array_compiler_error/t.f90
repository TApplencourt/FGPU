module prim
  implicit none

  type prim_type
    double precision, allocatable, dimension(:) :: v1
  end type prim_type

end module prim

program testmap
  use prim
  implicit none
    
  integer :: id = 1
  type(prim_type) :: prim_data(1:1)

  ! Allocate the data
  allocate(prim_data(id)%v1(10))
  
  ! Set data on host
  prim_data(id)%v1(1) = 9.99D0
  
  !$omp target enter data map(to:prim_data(id)%v1)

  ! modify data on device 
  !$omp target
  prim_data(id)%v1(1) = 1.0D0
  !$omp end target

end program testmap


