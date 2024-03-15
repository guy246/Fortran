program TP3exo3
    implicit none
    integer :: i,j,L
    !pballocation
    integer,dimension(:,:),allocatable :: matrix
    integer,dimension(:,:),allocatable :: atmatrix
    !effacer l'ecran : call system ("clear")
    write (*,"('Bienvenue dans anti-transposeur version 1.0 :')")
    !On demande la taille de la matrice
    write (*,"(/,'Ordre de la matrice (matrice carrée) : ')",advance = 'no')
    read *,L
    !On alloue la taille lue en entrée
    allocate(matrix(L,L))
    allocate(atmatrix(L,L))
    i = 1
    j = 1
    !On remplit la matrice avec rand()
    do while (i<=L)
        j=1
        do while (j<=L)
            matrix(i,j)=floor((rand()*49)+1)
            j = j+1
        end do
        i = i+1
    end do
    !On affiche la matrice entiere
    write (*,"(/,'La matrice : ')")
    write(*,*)
    do i=1,L
        do j=1,L
            write (*,"(4x,i4)",advance = 'no')matrix(i,j)
        end do
        write(*,*)
    end do
    write (*,*)
    !On calcule l'anti-transposee de la matrice
    i=1
    do while (i<=L)
        j=1
        do while (j<=L)
           atmatrix(i,j)=matrix(L-j+1,L-i+1)
            j = j+1
        end do
        i = i+1
    end do
    !On affiche l'anti-transposee
    write (*,"(/,'Son anti-transposee : ')")
    write(*,*)
    do i=1,L
        do j=1,L
            write (*,"(4x,i4)",advance = 'no')atmatrix(i,j)
        end do
        write(*,*)
    end do
    write (*,*)
    stop "Fin anti-transposee"
    end program TP3exo3