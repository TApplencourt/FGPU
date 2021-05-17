FUNCTION almost_equal(x, gold, tol) RESULT(b)
  implicit none
  DOUBLE PRECISION,  intent(in) :: x
  DOUBLE PRECISION,  intent(in) :: gold
  REAL,  intent(in) :: tol
  LOGICAL              :: b
  b = ( gold * (1 - tol)  <= x ).AND.( x <= gold * (1+tol) )
END FUNCTION almost_equal


module prim
  use, intrinsic :: iso_c_binding
  implicit none
  integer :: nx = 10
  integer :: ny = 10
  integer :: nz = 10
  
  type prim_type
     real(c_double), allocatable, dimension(:,:,:) :: v1
     real(c_double), allocatable, dimension(:,:,:) :: v2
     real(c_double), allocatable, dimension(:,:,:) :: v3
   contains 
     procedure :: setup => setup_prim
  end type prim_type
  
contains
  subroutine setup_prim(prim_data_i)
    class(prim_type), intent(OUT) :: prim_data_i
    
    allocate(prim_data_i%v1(nx,ny,nz))
    allocate(prim_data_i%v2(nx,ny,nz))
    allocate(prim_data_i%v3(nx,ny,nz))
    
  end subroutine setup_prim
  
end module prim

module objects
  use prim
  type(prim_type)  ::    prim_data(1:10)
end module objects


subroutine setDeviceData(val,id)
  use objects
  use, intrinsic :: iso_c_binding
  implicit none
  real(c_double), intent(in) :: val
  integer, intent(in) :: id
  integer :: i,j,k

  !$omp target teams distribute parallel do collapse(3)
  do i=1,10
     do j=1,10
        do k=1,10
           prim_data(id)%v3(i,j,k) = val
        end do
     end do
  end do
  !$omp end target teams distribute parallel do

end subroutine setDeviceData
module ompdata

  use objects
  implicit none


  contains

    subroutine ompdata_prim_to(id)
      implicit none
      integer, intent(in) :: id
      !$omp target enter data map(to:prim_data(id)%v1,prim_data(id)%v2,prim_data(id)%v3)
    end subroutine ompdata_prim_to

    subroutine ompdata_prim_from(id)
      implicit none
      integer, intent(in) :: id
      !$omp target exit data map(from:prim_data(id)%v1,prim_data(id)%v2,prim_data(id)%v3)
    end subroutine ompdata_prim_from
    
    subroutine ompdata_prim_delete(id)
      implicit none
      integer, intent(in) :: id
      !$omp target exit data map(delete:prim_data(id)%v1,prim_data(id)%v2,prim_data(id)%v3)
    end subroutine ompdata_prim_delete        

  
end module ompdata

program testmap

  use, intrinsic :: iso_c_binding
  use objects
  use ompdata
  implicit none
    
  integer :: id = 1
  LOGICAL :: almost_equal
  LOGICAL :: r
  external :: setDeviceData

  ! Allocate the data
  call prim_data(id)%setup()

  ! Set data on host
  prim_data(id)%v3(1,1,1) = 9.99D0
  
  ! Map to device
  !$omp target enter data map(to:prim_data(id)%v1,prim_data(id)%v2,prim_data(id)%v3)
  
  call setDeviceData(3.0D0,id)

  
  ! This should be 9.99 on host and 3 on device
#ifdef _OPENMP
  r = almost_equal(prim_data(id)%v3(1,1,1), 9.99D0, 0.1)  
#else
  r = almost_equal(prim_data(id)%v3(1,1,1), 3.0D0, 0.1)
#endif
  IF ( .NOT.r) THEN
    WRITE(*,*)  '1/ Wrong value for prim_data(',id,')%v3(1,1,1)', prim_data(id)%v3(1,1,1)
    STOP 112
  ENDIF

  ! set v1(1,1,1) to 1.0 on device
  !!$omp target
  prim_data(id)%v1(1,1,1) = 1.0D0
  !!$omp end target

  !$omp target exit data map(from:prim_data(id)%v1)

  !V1 shoud have the corret value
  IF (.NOT.almost_equal(prim_data(id)%v1(1,1,1),1.0D0,0.1)) THEN
    WRITE(*,*)  '2/ Wrong value for prim_data(',id,')%v1(1,1,1)', prim_data(id)%v1(1,1,1)
    STOP 112
  ENDIF

  !V3 didn't change
#ifdef _OPENMP
  r = almost_equal(prim_data(id)%v3(1,1,1), 9.99D0, 0.1)
#else
  r = almost_equal(prim_data(id)%v3(1,1,1), 3.0D0, 0.1)
#endif
  IF ( .NOT.r) THEN
    WRITE(*,*)  '3/ Wrong value for prim_data(',id,')%v3(1,1,1)', prim_data(id)%v3(1,1,1)
    STOP 112
  ENDIF

  !$omp target enter data map(to:prim_data(id)%v1)
  !$ call ompdata_prim_from(id)

  IF (.NOT.almost_equal(prim_data(id)%v3(1,1,1), 3.d0,0.1)) THEN
    WRITE(*,*)  '4/ Wrong value for prim_data(',id,')%v3(1,1,1)', prim_data(id)%v3(1,1,1)
    STOP 112
  ENDIF

end program testmap


