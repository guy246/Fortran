program Nb_Hamming
    implicit none
    integer :: lim,i,j,cpt
    write (*,"('. Bienvenue dans lister Hammings')")
    !effacer l'ecran : call system ("clear")
    !<><><>Lire la limite
    write (*,"(/,'Veuillez indiquer votre entier limite : ')",advance = 'no')
    read *,lim
    write (*,"('. [La liste]: ')")
    cpt = 0
    do i=1,lim,1
        j=i
        do while (mod(j,2).eq.0)
            j=j/2
        end do
        do while (mod(j,3).eq.0)
            j=j/3
        end do
        do while (mod(j,5).eq.0)
            j=j/5
        end do
        if (j.eq.1) then
            cpt = cpt +1
            write(*,"(4x,i4,6x,i4)")cpt,i
        end if
    end do
    stop "Fin lister Hamming"
    end program Nb_Hamming