program EntiersPremiers
    implicit none
    integer :: limite,i,j,y,cpt
    write (*,"('. Bienvenue dans lister premiers')")
    !effacer l'ecran : call system ("clear")
    !<><><>Lire la valeur limite
    write (*,"(/,'Veuillez indiquer votre entier limite : ')",advance = 'no')
    read *,limite
    cpt=0
    if (limite==1) then
        write (*,"('. [1 nest pas premier]')")
    else
        write (*,"('. [La liste]: ')")
        do i=2,limite,1
                y=1
                do j=2,i-1,1
                    if (mod(i,j).EQ.0) then
                        y=0
                    end if
                end do
                if (y.ne.0) then
                    cpt = cpt+1
                    write(*,"(4x,i4,6x,i4)")cpt,i
                end if
        end do
    end if
    stop "Fin primes"
end program EntiersPremiers