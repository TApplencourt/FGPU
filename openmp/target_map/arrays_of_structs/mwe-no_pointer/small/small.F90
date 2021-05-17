module prim
  implicit none
  integer :: nx = 10

  type prim_type
    double precision, allocatable, dimension(:) :: v1
  contains 
    procedure :: setup => setup_prim
  end type prim_type

contains

  subroutine setup_prim(prim_data_i)
  class(prim_type), intent(OUT) :: prim_data_i
    allocate(prim_data_i%v1(nx))
  end subroutine setup_prim

end module prim


module objects
  use prim
  type(prim_type)  ::    prim_data(1:1)
end module objects


program testmap
  use objects
  implicit none
    
  integer :: id = 1

  ! Allocate the data
  call prim_data(id)%setup()

  ! Set data on host
  prim_data(id)%v1(1) = 9.99D0
  
  ! set data on device 
  !$omp target
  prim_data(id)%v1(1) = 1.0D0
  !$omp end target

end program testmap


