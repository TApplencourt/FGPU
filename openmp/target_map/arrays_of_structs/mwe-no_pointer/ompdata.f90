module ompdata

  use, intrinsic :: iso_c_binding
  use objects
  implicit none


  contains

    subroutine ompdata_prim_to(id)
      implicit none
      integer(c_int), intent(in) :: id
      !$omp target enter data map(to:prim_data(id)%v1,prim_data(id)%v2,prim_data(id)%v3)
    end subroutine ompdata_prim_to

    subroutine ompdata_prim_from(id)
      implicit none
      integer(c_int), intent(in) :: id
      !$omp target exit data map(from:prim_data(id)%v1,prim_data(id)%v2,prim_data(id)%v3)
    end subroutine ompdata_prim_from
    
    subroutine ompdata_prim_delete(id)
      implicit none
      integer(c_int), intent(in) :: id
      !$omp target exit data map(delete:prim_data(id)%v1,prim_data(id)%v2,prim_data(id)%v3)
    end subroutine ompdata_prim_delete        

  
end module ompdata
