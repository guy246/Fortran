program TP2exo22
    implicit none
    integer :: L,C,i,j,k
    !pballocation
    integer,dimension(:,:),allocatable :: matrix
    !effacer l'ecran : call system ("clear")
    write (*,"('. Taille de la matrice :')")
    !On demande le nombre de lignes et de colonnes
    write (*,"(/,'Saisir le nombre de lignes L et de colonnes C : ')",advance = 'no')
    read *,L,C
    !On alloue la taille lue ne entrée
    allocate(matrix(L,C))
    write (*,"(/,' Merci de saisir (ligne par ligne) les entrées de la matrice :')")
    i = 1
    j = 1
    !On remplit la matrice en demanant a l'user
    do while (i<=L)
        write (*,"(/,2x,' Ligne :'i4' ')")i
        j=1
        do while (j<=C)
            k=i
            write (*,"(4x,'Col'i4' : ')",advance = 'no')k
            read *,k
            matrix(i,j) = k
            j = j+1
        end do
        i = i+1
    end do
    !On affiche la matrice entiere
    write (*,"(/,'Voici les entrees de la matrice : ')")
    write(*,*)
    do i=1,L
        do j=1,C
            write (*,"(4x,i4)",advance = 'no')matrix(i,j)
        end do
        write(*,*)
    end do
    write (*,*)
    deallocate(matrix)
    stop "Write and Read Matrices..."
    end program TP2exo22