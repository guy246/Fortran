program TP3exo2
    implicit none
    integer :: L,C,i,j,k,tr
    real :: M
    !pballocation
    integer,dimension(:,:),allocatable :: matrix
    integer,dimension(:,:),allocatable :: tmatrix
    integer,dimension(:,:),allocatable :: prodmatrix
    !effacer l'ecran : call system ("clear")
    write (*,"('. Taille de la matrice :')")
    !On demande le nombre de lignes et de colonnes
    write (*,"(/,'Saisir le nombre de lignes L et de colonnes C : ')",advance = 'no')
    read *,L,C
    !On alloue la taille lue ne entrée
    allocate(matrix(L,C))
    allocate(tmatrix(L,C))
    allocate(prodmatrix(L,C))
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
    !write (*,"(/,'Voici les entrees de la matrice : ')")
    !write(*,*)
    !do i=1,L
    !    do j=1,C
    !        write (*,"(4x,i4)",advance = 'no')matrix(i,j)
    !    end do
    !    write(*,*)
    !end do
    !write (*,*)
    tmatrix=transpose(matrix)
    !On affiche la matrice transposee
    write (*,"(/,'Voici les entrees de la matrice transposee : ')")
    write(*,*)
    do i=1,L
        do j=1,C
            write (*,"(4x,i4)",advance = 'no')tmatrix(i,j)
        end do
        write(*,*)
    end do
    write (*,*)
    !On realise et on affiche le produit de la matrice et de sa transposee
    prodmatrix=matmul(matrix,tmatrix)
    write (*,"(/,'Voici les entrees du produit : ')")
    write(*,*)
    do i=1,L
        do j=1,C
            write (*,"(4x,i4)",advance = 'no')prodmatrix(i,j)
        end do
        write(*,*)
    end do
    write (*,*)
    !On determine la trace du produit
    i=1
    do while (i<=L)
        tr=tr+prodmatrix(i,i)
        i=i+1
    end do
    !On calcule la norme de Frobenius
    M=sqrt(real(tr))
    write (*,"('La norme de Frobenius de la matrice est : 'g14.6'')")M
    !On desalloue la memoire
    deallocate(matrix)
    deallocate(tmatrix)
    deallocate(prodmatrix)
    stop "Norme de Frobenius"
    end program TP3exo2