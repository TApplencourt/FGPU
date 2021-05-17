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
  type(prim_type), target  ::    prim_data(1:1)
  type(prim_type), pointer ::    prim_ptr

  contains
    subroutine point_to_objects(id)
      implicit none
      integer, intent(in) :: id
      prim_ptr   =>   prim_data(id)
    end subroutine point_to_objects
end module objects


program testmap
  use objects
  implicit none
    
  integer :: id = 1

  call point_to_objects(id)
  ! Allocate the data
  call prim_ptr%setup()

  ! Set data on host
  prim_ptr%v1(1) = 9.99D0
  
  ! set data on device 
  !$omp target
  prim_ptr%v1(1) = 1.0D0
  !$omp end target
  
  !$omp target exit data map(from:prim_ptr%v1)

  WRITE(*,*)  'prim_ptr%v1(1) (should be 1.0)', prim_ptr%v1(1)

end program testmap


