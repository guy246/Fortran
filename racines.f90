program TP3exo1
    implicit none
    integer :: i
    integer,parameter :: n=5
    real,dimension(n) :: u(5)=(/1.0,1.0,1.0,1.0,2.0/)
    real :: r
    !complex :: r
    i=1
    r = sqrt(u(n))
    do i=n-1,1,-1
        r = sqrt(u(i) + r)
    end do
    write(*,"(f5.2)")r
    !write(*,"('2f5.2')")r
    stop "Racines"
    end program TP3exo1