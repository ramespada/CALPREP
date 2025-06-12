module utils_module
! UTILS

contains

integer function atoi(str)            !string -> int
  implicit none
  character(len=*), intent(in) :: str
  read(str,*) atoi
end function
character(len=16) function itoa(i)    !int -> string
   implicit none
   integer, intent(in) :: i
   write(itoa, '(i0)') i
   itoa = adjustl(itoa)
end function
character(len=16) function rtoa(r)    !real -> string
   implicit none
   real, intent(in) :: r
   write(rtoa, '(F16.3)') r
   rtoa = adjustl(rtoa)
end function

end module
