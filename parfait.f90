program nombre_parfait
    implicit none
    integer :: N,sommediv,i,j
    !effacer l'ecran : call system ("clear")
    !<><><>Lire la valeur N
    write (*,"(/,'Veuillez indiquer la limite:')",advance = 'no')
    read *,N
    sommediv = 0
    do i=2,N,1
        do j=1,i-1,1
            if (mod(i,j).EQ.0) then
            sommediv = sommediv + j
            end if
        end do
        !write(*,*)sommediv
        if ((sommediv).EQ.i) then
            write(*,"('.'i4' est somme de ses diviseurs')")i
        end if
        sommediv = 0
    end do
    stop "Fin entiers parfaits"
end program nombre_parfait