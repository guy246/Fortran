program TP2exo2
    implicit none
    integer :: L,C,i,j,k
    !pballocation
    integer,dimension(:,:),allocatable :: matrix
    !effacer l'ecran : call system ("clear")
    write (*,"('. Taille de la matrice :')")
    !On demande le nombre de lignes et de colonnes
    write (*,"(/,'Saisir le nombre de lignes L et de colonnes C : ')",advance = 'no')
    read *,L,C
    !On alloue la taille lue en entrée pour la matrice
    allocate(matrix(L,C))
    write (*,"(/,' Merci de saisir (ligne par ligne) les entrées de la matrice :')")
    i = 1
    j = 1
    !On remplit la matrice en demandant a l'user
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
    stop "Write and Read Matrices..."
    end program TP2exo2