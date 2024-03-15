program TP2_exo4
    implicit none
    integer :: i,o,n,k
    integer,dimension(:),allocatable :: u
    integer,dimension(:),allocatable :: v
    write (*,"('. Bienvenue dans le comparateur lexinf')")
    write (*,"(/,'Définir la taille du vecteur n : ')",advance = 'no')
    read *,n
    !On alloue la taille necessaire pour u et v
    allocate(u(n))
    allocate(v(n))
    i=1
    !On demande au user d'entrer les valeurs de u
    write (*,"(/,'Vecteur u : ')",advance = 'no')
    do while (i<=n)
        read *,k
        u(i) = k
        i = i+1
    end do
    i=1
    !On demande au user d'entrer les valeurs de v
    write (*,"(/,'Vecteur v : ')",advance = 'no')
    do while (i<=n)
        read *,k
        v(i) = k
        i = i+1
    end do
    !Affichage sur une seule ligne de u puis de v
    write (*,"(/,'Vecteur u : ')",advance = 'no')
    do i=1,n
        write (*,"(x,i4)",advance = 'no')u(i)
    end do
    write(*,*)
    write (*,"(/,'Vecteur v : ')",advance = 'no')
    do i=1,n
        write (*,"(x,i4)",advance = 'no')v(i)
    end do
    write(*,*)
    write(*,*)
    !Test si u <lexico v
    do i=1,n
        if (u(i)<v(i)) then
            o=1
        end if
    end do
    if (o.eq.0) then
        write(*,"(' [u lexinf v ?] : NON')")
    else
        write(*,"(' [u lexinf v ?] : OUI')")
    end if
    write(*,*)
    stop "Fin lexorder"
    end program TP2_exo4
