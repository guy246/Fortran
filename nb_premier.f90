program estPremier
    implicit none
    integer :: p,i,y
    write (*,"('. Bienvenue dans est premier')")
    !effacer l'ecran : call system ("clear")
    !<><><>Lire la valeur p
    write (*,"(/,'Veuillez indiquer votre entier : ')",advance = 'no')
    read *,p
    if (p<=1) then
        write(*,"('. [Est premier ?] : NON')")
    else
        do i=2,p,1
            if (mod(p,i).EQ.0) then
                y=0
            end if
        end do
        if (y==0) then
            write(*,"('. [Est premier ?] : NON')")
        else
            write(*,"('. [Est premier ?] : OUI')")
        end if
    end if
    stop "Fin primes"
end program estPremier