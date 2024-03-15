program Hamming
    implicit none
    integer :: h
    write (*,"('. Bienvenue dans est hamming')")
    !effacer l'ecran : call system ("clear")
    !<><><>Lire l'entier
    write (*,"(/,'Veuillez indiquer votre entier : ')",advance = 'no')
    read *,h
    do while (mod(h,2).eq.0)
        h = h/2
    end do
    do while (mod(h,3).eq.0)
        h = h/3
    end do
    do while (mod(h,5).eq.0)
        h = h/5
    end do
    if (h.eq.1) then
        write(*,"('. [Est Hamming ?] : OUI')")
    else
        write(*,"('. [Est Hamming ?] : NON')")
    end if
    stop "Fin Hamming"
end program Hamming