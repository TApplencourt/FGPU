module objects
  use prim

  type(prim_type)  ::    prim_data

  contains

    subroutine setup_objects()
      use, intrinsic :: iso_c_binding
      implicit none

      call prim_data%setup()

    end subroutine setup_objects

   ! subroutine point_to_objects(id)
   !   implicit none
   !   integer(c_int), intent(in) :: id
   !   prim_ptr   =>   prim_data(id)
   ! end subroutine point_to_objects


end module objects
