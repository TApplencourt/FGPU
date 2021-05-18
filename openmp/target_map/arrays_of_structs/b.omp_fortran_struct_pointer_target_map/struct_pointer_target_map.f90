function almost_equal(x, ref, tol) result(b)
  implicit none
  double precision, intent(in) :: x, ref, tol
  logical                      :: b
  b = ( ref * (1-tol) <= x ).and.( x <= ref * (1+tol) )
end function almost_equal
  

module prim
  implicit none
  type prim_type
    double precision, allocatable, dimension(:) :: v1
  end type prim_type
end module prim


program testmap
  ! modified from github.com/LLNL/FGPU/tree/master/openmp/target_map/arrays_of_structs
  use prim
  implicit none
  logical :: almost_equal  
  type(prim_type), target  ::    prim_data(1:4)
  type(prim_type), pointer ::    prim_ptr
    
  ! set prim_ptr to point to first element of prim_data
  prim_ptr => prim_data(1)

  ! Allocate the data
  allocate(prim_ptr%v1(1))

  ! Set data on host
  prim_ptr%v1(1) = 9.99D0
 
  ! map data to device
  !$omp target enter data map(to:prim_ptr%v1)

  ! modify data on device 
  !$omp target data use_device_ptr(prim_ptr) 
  prim_ptr%v1(1) = 1.0D0
  !$omp end target data
  
  ! map data to host
  !$omp target exit data map(from:prim_ptr%v1)
  
  if (.not.almost_equal(prim_ptr%v1(1),1.0D0,0.1D0)) then
    WRITE(*,*)  'prim_ptr%v1(1) (should be 1.0)', prim_ptr%v1(1)
    stop 112
  endif

end program testmap
