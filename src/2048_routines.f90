! Basic 2048 moves
module routines2048
  implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Create new tile in random empty spot
  subroutine new_tile(m)
    integer, intent(inout), dimension(:,:) :: m
!    integer, dimension(:), allocatable :: empty_tiles
    integer, dimension(16) :: empty_tiles
    integer :: nempty
    integer :: newtile_pos
    double precision :: rnd
    integer :: i
    call random_number(rnd)
    nempty = count(m==0)
!    allocate(empty_tiles(nempty))
    
    empty_tiles = 0
    empty_tiles(1:nempty) = pack(m,m==0)
    
    i = int(rnd*nempty)+1
    call random_number(rnd)
    if (rnd < 0.9d0) then
       empty_tiles(i) = 2
    else 
       empty_tiles(i) = 4
    end if
    m = unpack(empty_tiles(1:nempty),m==0,m)
!    deallocate(empty_tiles)
  end subroutine new_tile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Slide tile
  subroutine slide(m,d,score)
    integer, intent(inout), dimension(:,:) :: m
    integer, intent(inout) :: score
    integer, dimension(4,4) :: m_temp
    integer, dimension(4) :: vec
    integer i, j, d
!!! up = 1, left = 2, down = 3, right = 4
!!! default is up
    if (d == 2) then
       m = transpose(m)
    else if (d == 3) then
       m = m(4:1:-1,:)
    else if (d == 4) then
       m = transpose(m)
       m = m(4:1:-1,:)
    end if
    do j = 1, 4
       m(:,j) = reshape(pack(m(:,j),m(:,j)/=0),(/4/),pad=(/0,0,0,0/))
    end do
    do j = 1, 4
       do i = 1, 3
          if (m(i,j)==m(i+1,j)) then
             m(i,j) = m(i,j) + m(i+1,j)
             score = score + m(i,j)
             m((i+1):4,j) = eoshift(m((i+1):4,j),1)
          end if
       end do
    end do
    if (d == 2) then
       m = transpose(m)
    else if (d == 3) then
       m = m(4:1:-1,:)
    else if (d == 4) then
       m = m(4:1:-1,:)
       m = transpose(m)
    end if
  end subroutine slide

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Test possible moves
  function test_moves(m)
    integer, intent(in), dimension(:,:) :: m
    integer, dimension(size(m,1),size(m,2)) :: m_temp
    integer, dimension(4) :: test_moves
    integer i, s
    s = 0
    test_moves = 0
    do i = 1, 4
       m_temp = m
       call slide(m_temp,i,s)
       if(.not. all(m==m_temp)) test_moves(i) = i
    end do
  end function test_moves

end module routines2048
