! Testing code
! gfortran -o my2048 rand_init.f90 2048_routines.f90 2048_autoplay.f90 2048_main.f90
program my2048
  use routines2048
  use rand_init
  use autoplay2048
  implicit none

  integer, dimension(4,4) :: a 
  integer, dimension(4,4) :: b, b2
  integer, dimension(4) :: moves_left, moves_sort
  integer i, move, score, j, depth, ntry, nzeros
  double precision, dimension(7,4) :: autoplay
  double precision, dimension(4) :: moves_eval
  integer t1, t2, rate
  double precision dt, minloss
  integer ii, kk, n, dscore
  double precision, dimension(4) :: lev2_eval
  integer, dimension(4) :: empty_tiles
  integer dummy_score

  call system_clock(t1,rate)
  call init_random_seed()


  a = 0
  score = 0
  do i = 1, 4
     write(*,*) a(i,:)
  end do
  write(*,*)
  
  do i = 1,2
     call new_tile(a)
  end do
  do i = 1, 4
     write(*,*) a(i,:)
  end do
  write(*,*)

  j = 0
  do while (any(moves_left/=0))
     j = j+1
     depth = min(max(10,j/10),12)
     ntry = min(max(100,j/2),300)
     autoplay = best_move(a,ntry,depth)
     moves_eval = autoplay(1,:)

     do i = 1, 4
        dummy_score = 0
        b = a
        call slide(b,i,dummy_score)
        empty_tiles(i) = count(b==0)
     end do
     moves_eval = moves_eval+moves_eval*dble(empty_tiles)/32.d0

     ! select move that loses the least
     ! minloss = huge(minloss)
     ! moves_sort = moves_eval
     ! do i = 1, 4
     !    moves_sort(i) = maxloc(moves_eval,dim=1,mask=autoplay(1,:)<minloss)
     !    minloss = moves_eval(moves_sort(i))
     ! end do
     ! move = moves_sort(minloc(autoplay(7,moves_sort),dim=1))

     ! moves_eval = autoplay(7,:)
     
     moves_eval = moves_eval/max(0.00001,autoplay(7,:))
    
     ! move = minloc(moves_eval,dim=1)
     move = maxloc(moves_eval,dim=1)
     call slide(a,move,score)
     call new_tile(a)
     write(*,*) "turn: " ,j, "move: ",move!,"max tile: ",maxval(a),"score: ", score
     write(*,*) "max tile: ",maxval(a),"score: ", score
     write(*,*) "loss fraction: ",autoplay(7,move)
     write(*,*) "score gain: ",autoplay(1,move)
     write(*,*) "move loss: ",autoplay(7,:)
     write(*,*) "move score: ",autoplay(1,:)
     write(*,*) "move eval: ",moves_eval
     ! write(*,*) "move order: ",moves_sort
     write(*,*)
!     write(*,*) "metric: ", moves_eval
     do i = 1, 4
        write(*,*) a(i,:)!, autoplay(i,:)
     end do
     write(*,*)
     moves_left = test_moves(a)
  end do

  call system_clock(t2)
  dt = dble(t2-t1)/dble(rate)

  write(*,*)
  write(*,*)
  write(*,*) "GAME OVER!"
  write(*,*) "move: ",move,"max tile: ",maxval(a),"score: ", score
  do i = 1, 4
     write(*,*) a(i,:)
  end do
  write(*,*)
  write(*,*) "Time elapsed (s): ", dt
  write(*,*) "Turns played: ", j
  write(*,*) "moves/sec: ", dble(j)/dt
  write(*,*)



end program my2048
