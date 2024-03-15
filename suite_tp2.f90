program TP2exo1
    implicit none
    integer :: N,i
    real :: u0,un1
    !effacer l'ecran : call system ("clear")
    !<><><>Lire la limite
    write (*,"(/,'Veuillez indiquer le rang N : ')",advance = 'no')
    read *,N
    !u0=3
    un1 = sqrt(3.0)
    i = 1
    if (N.eq.0) then
        write (*,"('. u0 = 3')")
    else
        if (N.eq.1) then
            write (*,"('. u0 = 3')")
        end if
        do while (i.ne.N) 
            u0=un1
            un1=(0.5*u0)+sqrt(1+cos(u0-1))
            write(*,"(3x,'iteration',2x,i4,'  :',3x,g14.6)")i,u0
            i=i+1
        end do
        write (*,"('. Le'i3'eme terme vaut : 'g14.6'')")N,un1
    end if
    stop "Fin recurrence"
    end program TP2exo1