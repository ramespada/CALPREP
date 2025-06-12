module INTERP

 use PROJ              !subroutines for coordinate transformations

 implicit none
 private
 public interp_2d
 public interp_grid
 public remap_freq

 type interp_grid
   character(len=256) :: proj    !string defining projection of grid
   integer            :: nx,ny   !grid size X-Y
   real(8)            :: dx,dy   !grid cell size X-Y     (in units of the proj crs)
   real(8)            :: xc,yc   !center X-Y coordinates (in units of the proj crs)
   real(8)            :: xmin,ymin,xmax,ymax  !grid boundaries coords (in units of the proj crs)
 end type

 interface interp_2d
    module procedure interp_2d_int, interp_2d_float
 end interface  interp_2d

contains

!main INTERPOLATION/REMAP function:
subroutine interp_2d_float(g0,z0, g1,z1)  
   !
   ! PURPOSE: 2D Interpolation/Remap driver
   implicit none
   type(interp_grid), intent(in) :: g0,g1
   real, intent(in)              :: z0(:,:)
   real, intent(inout)           :: z1(:,:)
   real :: scale_X, scale_Y

   print '("2D Interpolation for continuous variables (floats)")'
   !Check array dimensions match grid definitions
   if (size(z0,1) /= g0%nx .or. size(z0,2) /= g0%ny) then
    !print*,"debug: ",size(z0,1),size(z0,2), g0%nx, g0%ny
     error stop "INTERP: Source array dimensions don't match source grid"
   end if
   if (size(z1,1) /= g1%nx .or. size(z1,2) /= g1%ny ) then
           !print*,"debug: ",size(z1,1),size(z1,2), g1%nx, g1%ny
     error stop "INTERP: Target array dimensions don't match target grid"
   end if

   !Check which grid is coarser:
   call get_scale_factor(g0,g1,scale_X,scale_Y)

   !print*,"SCALE factor:",scale_X,scale_Y

   !Decide which method to call:
   if ( scale_X > 1.5 .or. scale_Y > 1.5 ) then
           call remap_avg(g0,z0,g1,z1)
   else
           call interp_bilinear(g0,z0,g1,z1)
   end if
end subroutine

subroutine interp_2d_int(g0,z0,  g1,z1)  
   !
   ! PURPOSE: 2D Interpolation/Remap driver
   implicit none
   type(interp_grid), intent(in) :: g0,g1
   integer, intent(in)           :: z0(:,:)
   integer, intent(inout)        :: z1(:,:)
   real :: scale_X, scale_Y

   print '("2D Interpolation for discrete variables (integers)")'

   !Check array dimensions match grid definitions
   if (size(z0,1) /= g0%nx .or. size(z0,2) /= g0%ny) then
   !print*,"debug:",shape(z0), g0%nx, g0%ny
     error stop "INTERP: Source array dimensions don't match source grid"
   end if

   if (size(z1,1) /= g1%nx .or. size(z1,2) /= g1%ny) then
    ! print*,"debug:",shape(z1), g1%nx, g1%ny
     error stop "INTERP: Target array dimensions don't match target grid"
   end if

   !Check which grid is coarser:
   call get_scale_factor(g0,g1,scale_X,scale_Y)

   print*,"SCALE factor:",scale_X,scale_Y

   !Decide which method to call:
   if ( scale_X > 1.5 .or. scale_Y > 1.5 ) then
      call remap_mode(g0,z0,g1,z1)
   else
      call interp_nearest(g0,z0,g1,z1)
   end if
end subroutine

!
! INTERPOLATION subroutines:
!
! > when source grid is coarser than target grid.

subroutine interp_bilinear(g0,z0, g1,z1)  
   !
   !PURPOSE: Performs "Bilinear Interpolation" for a 2D real array.
   !
   implicit none
   type(interp_grid), intent(in) :: g0,g1
   real, intent(in)              :: z0(:,:)
   real, intent(inout)           :: z1(:,:)
   real(8) :: px,py, x1,x2,y1,y2,p11,p12,p21,p22, w11,w12,w21,w22
   integer :: i,j, i1,j1,i2,j2

   print*,"  Bilinear method."

   ! Check array dimensions match grid definitions
   if (size(z0,1) /= g0%ny .or. size(z0,2) /= g0%nx) then
     !error stop "INTERP: Source array dimensions don't match source grid"
   end if
   if (size(z1,1) /= g1%ny .or. size(z1,2) /= g0%nx) then
     !error stop "INTERP: Target array dimensions don't match target grid"
   end if

   do i=1,g1%nx
      do j=1,g1%ny
          !Position where to interpolate
          px=g1%xmin+g1%dx*i                           !target coordinate-x
          py=g1%ymin+g1%dy*j                           !target coordinate-y

          call proj_trans(g1%proj,g0%proj, px, py)  !i want px,py to be in source proj

          !closest indices on source grid:
          i1=floor( (px-g0%xmin) / g0%dx );  i2=i1+1
          j1=floor( (py-g0%ymin) / g0%dy );  j2=j1+1
          if ( i1 > 1 .and. i2 <= g0%nx .and. j1 > 1 .and. j2 <= g0%ny ) then !if point inside grid.
             !points (coordinates)    !   p12(i1,j2)    p22(i2,j2)
             x1=g0%xmin+g0%dx*i1      !       *- - - - - -*            
             x2=g0%xmin+g0%dx*i2      !       |           |           
             y1=g0%ymin+g0%dy*j1      !       |           |           
             y2=g0%ymin+g0%dy*j2      !       |           |           
             !points (values)         !       |           |           
             p11=z0(i1,j1)            !       *- - - - - -*                                    
             p12=z0(i1,j2)            !   p11(i1,j1)    p21(i2,j1)
             p21=z0(i2,j1)
             p22=z0(i2,j2)
             !weights:
             w11 =(x2 - px)*(y2 - py)/(g0%dx*g0%dy)
             w12 =(x2 - px)*(py - y1)/(g0%dx*g0%dy)
             w21 =(px - x1)*(y2 - py)/(g0%dx*g0%dy)
             w22 =(px - x1)*(py - y1)/(g0%dx*g0%dy)
             !Bilineal formula:
             z1(i,j)= p11*w11 + p12*w12 + p21*w21 + p22*w22 ! DOT_PRODUCT(p,w)
          else                                                                                         
             z1(i,j)=0.0
          endif
      enddo
   enddo
end subroutine


subroutine interp_nearest(g0,z0, g1,z1)  
   !
   !PURPOSE: Perform "neares neighboor" interpolation for a 2D integer array.
   !
   implicit none
   type(interp_grid), intent(in) :: g0,g1
   integer, intent(in)           :: z0(:,:)
   integer, intent(inout)        :: z1(:,:)
   integer :: i,j,i1,j1
   real(8)    :: px,py

   print*,"  Nearest neighboor method."
   do i=1,g1%nx
      do j=1,g1%ny
          px=g1%xmin+g1%dx*i  !target coordinate-x
          py=g1%ymin+g1%dy*j  !target coordinate-y
                                                                                      
          call proj_trans(g1%proj,g0%proj, px, py)  !i want px,py to be in source proj

          ! Find nearest neighbor in source grid
          i1 = nint((px - g0%xmin)/g0%dx) + 1
          j1 = nint((py - g0%ymin)/g0%dy) + 1

          ! Handle edge cases (points outside source grid)
          i1 = max(1, min(i1, g0%nx))
          j1 = max(1, min(j1, g0%ny))

          ! Assign value
          z1(i,j) = z0(i1,j1)
      enddo
   enddo
end subroutine


!----------------------------------------------------------
!
! REMAP subroutines:
!
! > when target grid is coarser than source grid.


subroutine remap_avg(g0,z0, g1,z1)  
   !
   !PURPOSE: Perform an average remaping for a 2D real array.
   !
   implicit none
   type(interp_grid), intent(in) :: g0, g1
   real, intent(in)              :: z0(:,:)
   real, intent(inout)           :: z1(:,:)

   integer :: i,j,i1,j1,i2,j2,count
   real(8) :: px,py,pxmin,pxmax,pymin,pymax
   real(8) :: xmin0,ymin0,dx0,dy0
   print*,"  Average remap method."

   do i=1,g1%nx
      do j=1,g1%ny

        !Position where to interpolate
        px=g1%xmin+g1%dx*i;  py=g1%ymin+g1%dy*j        !target cell coordinates
        !Cell boundaries:
        pxmin=px-g1%dx*0.5;  pymin=py-g1%dy*0.5
        pxmax=px+g1%dx*0.5;  pymax=py+g1%dy*0.5

        call proj_trans(g1%proj,g0%proj, pxmin, pymin)  !(!) this is an aproximation. It should be 
        call proj_trans(g1%proj,g0%proj, pxmax, pymax)  !(!) evaluated at least on the 4 corners of the poly.

        ! Find overlapping source cells
        i1 =  floor( (pxmin-g0%xmin)/g0%dx) + 1! 
        i2 =ceiling( (pxmax-g0%xmin)/g0%dx) + 1! 
        j1 =  floor( (pymin-g0%ymin)/g0%dy) + 1! 
        j2 =ceiling( (pymax-g0%ymin)/g0%dy) + 1! 

        ! Clamp to source grid boundaries
        i1 = max(1,     i1); j1 = max(1,     j1)
        i2 = min(g0%nx, i2); j2 = min(g0%ny, j2)

        ! Average all source cells that fall within target cell
        count=((i2-i1+1)*(j2-j1+1))
        if ( count > 0 ) then
           z1(i,j) = sum(z0(i1:i2,j1:j2))/count  !average
        else
           z1(i,j) = 0.0                         ! or some missing value
        end if
      enddo
   enddo
end subroutine

subroutine remap_mode(g0,z0, g1,z1)  
   !
   !PURPOSE: Perform a "mode" remap for a 2D integer array.
   !
   implicit none
   type(interp_grid), intent(in) :: g0,g1
   integer, intent(in)           :: z0(:,:)
   integer, intent(inout)        :: z1(:,:)
   integer                       :: i,j,i1,j1,i2,j2,count
   real(8)                       :: px,py,pxmin,pxmax,pymin,pymax

   print*,"  Mode/vote remap method."
   do j=1,g1%ny
   do i=1,g1%nx
                                                                                
        !Position where to interpolate
        px=g1%xmin+g1%dx*i;  py=g1%ymin+g1%dy*j        !target cell coordinates
        !Cell boundaries:
        pxmin=px-g1%dx*0.5;  pymin=py-g1%dy*0.5
        pxmax=px+g1%dx*0.5;  pymax=py+g1%dy*0.5
                                                                                                              
        call proj_trans(g1%proj,g0%proj, pxmin, pymin)  !(!) this is an aproximation. It should be 
        call proj_trans(g1%proj,g0%proj, pxmax, pymax)  !(!) evaluated at least on the 4 corners of the poly.
                                                                                                              
        ! Find overlapping source cells
        i1 =  floor( (pxmin-g0%xmin)/g0%dx) + 1! 
        i2 =ceiling( (pxmax-g0%xmin)/g0%dx) + 1! 
        j1 =  floor( (pymin-g0%ymin)/g0%dy) + 1! 
        j2 =ceiling( (pymax-g0%ymin)/g0%dy) + 1! 
                                                                                                              
        ! Clamp to source grid boundaries
        i1 = max(1,     i1); j1 = max(1,     j1)
        i2 = min(g0%nx, i2); j2 = min(g0%ny, j2)
                                                                                                         
         ! Average all source cells that fall within target cell
         count=((i2-i1+1)*(j2-j1+1))
         if ( count > 0 ) then
            z1(i,j) = mode(z0(i1:i2,j1:j2))       !"vote"/mode
         else
            z1(i,j) = 0                           ! or some missing value
         end if
   enddo
   enddo

end subroutine


!Mode (most_frequent) function:
function mode(arr2d) result(most_frequent)
    implicit none
    integer, intent(in) :: arr2d(:,:)
    integer :: n,nx,ny,most_frequent

    integer, allocatable :: keys(:), counts(:)
    integer :: i,j, key_index, num_keys, max_count

    n=size(arr2d)
    nx=size(arr2d,1)
    ny=size(arr2d,2)
    ! Allocate hash table
    allocate(keys(n), counts(n))
    counts = 0
    num_keys = 0
    max_count = 0
    most_frequent = arr2d(1,1)

    ! Hashing method: Open addressing
    do i = 1, nx
    do j = 1, ny
        key_index = find_key_index(keys, counts, num_keys, arr2d(i,j))
        counts(key_index) = counts(key_index) + 1
        ! Update most frequent value
        if (counts(key_index) > max_count) then
            max_count = counts(key_index)
            most_frequent = keys(key_index)
        end if
    end do
    end do
    ! Cleanup
    deallocate(keys, counts)

contains
  function find_key_index(keys, counts, num_keys, value) result(index)
      integer, intent(inout) :: keys(:), counts(:)
      integer, intent(inout) :: num_keys
      integer, intent(in) :: value
      integer :: index, i

      ! Search for existing key
      do i = 1, num_keys
          if ( keys(i) == value ) then
              index = i
              return
          end if
      end do

      ! If not found, add new key
      num_keys = num_keys + 1
      keys(num_keys) = value
      index = num_keys
  end function find_key_index
end function


subroutine get_scale_factor(g0,g1,scaleX,scaleY)
   implicit none

   type(interp_grid),intent(in) :: g0,g1
   real, intent(inout)          :: scaleX,scaleY
   real(8) :: xmin,ymin,xmax,ymax,DX,DY
   real(8) :: corners(8,2)
    
   corners(1,1)= g1%xmin; corners(1,2)= g1%ymin  !Corners of g1:
   corners(2,1)= g1%xmin; corners(2,2)= g1%yc    !        4
   corners(3,1)= g1%xmin; corners(3,2)= g1%ymax  !   3x---x---x5
   corners(4,1)= g1%xc  ; corners(4,2)= g1%ymax  !    |       |
   corners(5,1)= g1%xmax; corners(5,2)= g1%ymax  !   2x       x6
   corners(6,1)= g1%xmax; corners(6,2)= g1%yc    !    |       |
   corners(7,1)= g1%xmax; corners(7,2)= g1%ymin  !   1x---x---x7
   corners(8,1)= g1%xc  ; corners(8,2)= g1%ymin  !        8
                                                               
   call proj_trans(g1%proj,g0%proj, corners(:,1), corners(:,2) ) 
   
   xmin=minval(corners(:,1)); ymin=minval(corners(:,2))
   xmax=maxval(corners(:,1)); ymax=maxval(corners(:,2))

   DX=abs(xmax-xmin)/g1%nx  !average cell X-size of grid1
   DY=abs(ymax-ymin)/g1%ny  !average cell Y-size of grid1

   !Compute scale:
   scaleX= DX/g0%dx  !how many g0 cells enters on a g1 cell
   scaleY= DY/g0%dy  !how many g0 cells enters on a g1 cell

end subroutine


!=============================================================
subroutine remap_freq(g0,z0, g1,f, cat)  
   !
   !PURPOSE: Perform a frequency remap for a 2D integer array.
   !
   implicit none
   type(interp_grid), intent(in) :: g0,g1
   integer, intent(in)           :: z0(:,:)
   real   , intent(inout)        :: f(:,:,:) !rel. freq. array: [ncat, nx, ny]
   integer, intent(in)           :: cat(:)
   integer :: i,j,k,i1,j1,i2,j2,cells
   real(8) :: px,py,pxmin,pxmax,pymin,pymax
   real :: frec

   print*,"  Compute relative frequency ..."

   do j=1,g1%ny
   do i=1,g1%nx
        !Position where to interpolate
        px=g1%xmin+g1%dx*i;  py=g1%ymin+g1%dy*j         !target cell coordinates
        !Cell boundaries:
        pxmin=px-g1%dx*0.5;  pymin=py-g1%dy*0.5
        pxmax=px+g1%dx*0.5;  pymax=py+g1%dy*0.5
                                                                                                              
        call proj_trans(g1%proj,g0%proj, pxmin, pymin)  !(!) this is an aproximation. It should be 
        call proj_trans(g1%proj,g0%proj, pxmax, pymax)  !(!) evaluated at least on the 4 corners of the poly.
                                                                                                              
        ! Find overlapping source cells
        i1 =  floor( (pxmin-g0%xmin)/g0%dx) + 1! 
        i2 =ceiling( (pxmax-g0%xmin)/g0%dx) + 1! 
        j1 =  floor( (pymin-g0%ymin)/g0%dy) + 1! 
        j2 =ceiling( (pymax-g0%ymin)/g0%dy) + 1! 
                                                                                                              
        ! Clamp to source grid boundaries
        i1 = max(1,     i1); j1 = max(1,     j1)
        i2 = min(g0%nx, i2); j2 = min(g0%ny, j2)
                                                                                                         
        ! Compute frequency of every category on this subdomain
        cells=((i2-i1+1)*(j2-j1+1))
        if ( cells > 0) then
           do k=1, size(cat)
             frec=real( count( z0(i1:i2,j1:j2) == cat(k) )) / real(cells)
             f(k,i,j) = frec
             !print*,"k:",k," cells:",cells,"count:",count( z0(i1:i2,j1:j2) == cat(k) ),"frec:",frec
           enddo
        end if
   enddo
   enddo

end subroutine

end module

