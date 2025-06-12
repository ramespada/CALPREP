module PROJ
  !
  !  Wrapper for PROJ Library
  !

  use, intrinsic :: ISO_C_BINDING, only: c_int, c_char,c_double, c_ptr, c_null_ptr, c_associated, c_size_t, c_loc, c_funptr, c_null_funptr,c_sizeof
  
  implicit none
  private
  public proj_trans     !(generalized)
 
  interface
     function proj_context_create() bind(C,name='proj_context_create')
       use iso_c_binding
       implicit none
       type(c_ptr) :: proj_context_create
     end function
     subroutine proj_context_destroy(ctx) bind(C,name='proj_context_destroy')
       use iso_c_binding
       implicit none
       type(c_ptr),value :: ctx
     end subroutine
     function proj_create_crs_to_crs(ctx,s_srs,t_srs,area) bind(C,name='proj_create_crs_to_crs')
       use iso_c_binding
       implicit none
       type(C_PTR)            :: proj_create_crs_to_crs
       character(kind=C_CHAR) :: s_srs(*)
       character(kind=C_CHAR) :: t_srs(*)
       type(C_PTR), value     :: ctx, area
     end function
     subroutine proj_destroy(PJ) bind(C,name='proj_destroy')
       use iso_c_binding
       implicit none
       type(c_ptr), value :: PJ
     end subroutine
     function proj_trans_generic(pj,dir,x,sx,nx,y,sy,ny,z,sz,nz,t,st,nt) bind(C,name='proj_trans_generic')
       use iso_c_binding
       implicit none
       integer(c_int)       :: proj_trans_generic
       type(c_ptr), value   :: pj
       integer(c_int),value :: dir
       type(c_ptr),value    :: x,y,z,t
       integer(c_int),value :: sx,sy,sz,st
       integer(c_int),value :: nx,ny,nz,nt
     end function
  end interface

  interface PROJ_TRANS
     module procedure proj_trans_multipt, proj_trans_pt
  end interface  PROJ_TRANS

  !character (len=*), public, parameter :: lonlat_proj4="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" !EPSG:4326
  
contains

subroutine proj_trans_multipt(s_srs,t_srs, x, y) !, np)
  !
  !"Any 2 any" coordinate system transofmration:
  ! Projects IN-PLACE arrays of coordinates from one projection to another.
  !
  implicit none
  !real(8), dimension(:),intent(inout) :: x, y        !coordinates to transform (rank-1 array form)
  character(len=*), intent(in) :: s_srs,t_srs           ! source & target spatial reference system (srs) (proj4str, epsgs, wkt, etc.)
  !integer, intent(in) :: np                             ! number of points (length of x or y)
  !integer, intent(in) :: direction                     ! 1=forward, backwards
  real(8), dimension (:), target, intent(inout) :: x, y ! coordinates to transform

  character (len=*), parameter :: sub_name='proj_trans'
    
  type(c_ptr) :: CTX = c_null_ptr
  type(c_ptr) ::  PJ = c_null_ptr
  type(c_ptr) :: area= c_null_ptr
  integer     :: dir=1, iStat
  integer     :: sp, np             !size (in bytes of coordinates datatype), number of points

  CTX=proj_context_create()
  PJ=proj_create_crs_to_crs(CTX, trim(s_srs)//achar(0),           &
                           &     trim(t_srs)//achar(0), C_NULL_PTR)

  sp=c_sizeof(x(1))       !Step length (bytes) between consecutive elements of the array
  np=size(x)! c_sizeof(x)/sp!

  iStat=proj_trans_generic( PJ, dir,             &
                          & c_loc(x)   , sp, np, &
                          & c_loc(y)   , sp, np, &
                          & C_NULL_PTR ,  0, 0 , &
                          & C_NULL_PTR ,  0, 0  )
  if (iStat /= np) print*,"Number of transformations failed = ",np-iStat

  !Clean:
  call proj_destroy(PJ)
  call proj_context_destroy(CTX)
end subroutine

subroutine proj_trans_pt(s_srs, t_srs, x, y)
    !      
    ! same that proj_trans but for a single value
    implicit none
    character(len=*), intent(in) :: s_srs, t_srs  !source & target spatial reference system (srs) (proj4str, epsgs, wkt, etc.)
    real(8), target, intent(inout) :: x, y !coordinates to transform
    real(8), dimension(1) :: xa, ya        !coordinates to transform (rank-1 array form)
    character (len=*), parameter :: sub_name='ll2proj_pt'
    xa=x;ya=y
    call proj_trans_multipt(s_srs,t_srs,xa,ya)!,1)
    x=xa(1);y=ya(1)
 end subroutine

end module
