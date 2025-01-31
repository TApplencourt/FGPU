module base_types
  implicit none

! Define base derived types

!== Type-bound procedure ========================! 
	type base_type
		real(kind=8), dimension(:,:), pointer :: array
		real(kind=8) :: scalar
		integer :: nx, ny				! Array dimensions
		integer :: openmp				! Flag for different openmp implementations
		contains
		procedure :: setup => setup_values
        procedure :: remove => remove_values
    end type base_type

	contains
	
!== Type-bound procedures =========================!
	subroutine setup_values(op,nx,ny,i)
		implicit none
		class(base_type) :: op		! derived type holding values for performing operations
		integer :: nx, ny, i		! size of arrays
		
		! Allocate and assign values to components
		allocate( op%array(nx,ny))
		op%array = i
		op%scalar = i
		op%nx = nx; op%ny = ny
        !$omp target enter data map(to:op%array) map(to:op%scalar) map(to:op%nx) map(to:op%ny)
	end subroutine setup_values

    subroutine remove_values(op)
        class(base_type) :: op
        !$omp target exit data map(from:op%array) map(from:op%scalar) map(from:op%nx) map(from:op%ny)
     end subroutine remove_values

end module base_types
