! Play automatically with random moves
module autoplay2048
  use routines2048
  implicit none

contains

  function best_move(mat,ntry,depth)
    integer, intent(in), dimension(:,:) :: mat
    integer, intent(in) :: ntry, depth
    integer, dimension(size(mat,1),size(mat,2)) :: m
    integer i, d, k, score, dscore, move, Nmoves, k2
    integer, dimension(4) :: avail_moves, start_move
    double precision rnd, delta
    double precision mean, sse
    integer maxscore, minscore, maxtile, mintile
    double precision, dimension(7,4) :: best_move
    !!! row: "mean","sd","max","min","max.tile","min.tile"
!!! col: up, left, down, right
    integer playcount, playloss

    best_move = 0.d0
    start_move = test_moves(mat)
    do i = 1, 4
       if ( start_move(i)==0 ) then
          best_move(7,i) = 2.d0
          CYCLE
       end if
       playloss = 0
       ! 
       m = mat
       score = 0
       move = i
       playcount = 1
       call slide(m,move,score)
       call new_tile(m)
       avail_moves = test_moves(m)
       Nmoves = count(avail_moves/=0)
       if(Nmoves==0) then
          score = score/depth/2
       end if
       do d = 2, depth
          if(Nmoves==0) then
             score = score*(min(d,depth/2))/(depth/2)
             playloss = playloss + depth - d
             playcount = playcount + depth - d - 1
             EXIT
          end if
          avail_moves = reshape(pack(avail_moves,avail_moves/=0),(/4/),pad=(/0,0,0,0/))
          call random_number(rnd)
          move = avail_moves(int(rnd*Nmoves)+1)
!!$          call slide(m,move,score)
          dscore = 0
          playcount = playcount + 1
          call slide(m,move,dscore)
          if (d > depth/2) dscore = dscore/(d-depth/2)
          score = score + dscore
         call new_tile(m)
          avail_moves = test_moves(m)
          Nmoves = count(avail_moves/=0)        
       end do
       mean = score
       sse = 0.d0
       maxscore = score
       minscore = score
       maxtile = maxval(m)
       mintile = maxtile
       do k = 2, ntry
          m = mat
          score = 0
          move = i
          playcount = playcount + 1
          call slide(m,move,score)
          call new_tile(m)
          avail_moves = test_moves(m)
          Nmoves = count(avail_moves/=0)
          if(Nmoves==0) score = score/(depth/2)
          do d = 2, depth
             if(Nmoves==0) then
                score = score*(min(d,depth/2))/(depth/2)
                playloss = playloss + depth - d
                playcount = playcount + depth - d - 1
                EXIT
             end if
             avail_moves = reshape(pack(avail_moves,avail_moves/=0),(/4/),pad=(/0,0,0,0/))
             call random_number(rnd)
             move = avail_moves(int(rnd*Nmoves)+1)
             playcount = playcount + 1
             call slide(m,move,score)
             dscore = 0
             call slide(m,move,dscore)
             if (d > depth/2) dscore = dscore/(d-depth/2)
             score = score + dscore
             call new_tile(m)
             avail_moves = test_moves(m)
             Nmoves = count(avail_moves/=0)     
          end do
          delta = score - mean
          mean = mean + delta/ dble(k)
          sse = sse + delta*(score-mean)
          maxscore = max(maxscore,score)
          minscore = min(minscore,score)
          maxtile = max(maxtile,maxval(m))
          mintile = min(mintile,minval(m))
       end do
       ! Testing various options for "best" moves
       best_move(1,i) = mean
       best_move(2,i) = sqrt(sse/dble(k-1))
       best_move(3,i) = dble(maxscore)
       best_move(4,i) = dble(minscore)
       best_move(5,i) = dble(maxtile)
       best_move(6,i) = dble(mintile)
       !!!
       best_move(7,i) = dble(playloss)/dble(playcount)
       !!!
       ! best_move(1,i) = mean+dble(minscore)+mean*dble(maxtile)/2048.d0
!!$       best_move(1,i) = (mean+dble(minscore))*dble(maxtile)/2048.d0
       !!!
    end do
  end function best_move

end module autoplay2048
