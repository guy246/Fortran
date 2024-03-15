program TP2exo3
    implicit none
    integer :: i,j,k,L,C
    integer,dimension(:,:),allocatable :: A
    integer,dimension(:),allocatable :: minimum
    write (*,"('. Taille de la matrice :')")
    write (*,"(/,'Saisir le nombre de lignes L et de colonnes C : ')",advance = 'no')
    read *,L,C
    allocate(A(L,C))
    allocate(minimum(L))
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
            A(i,j) = k
            j = j+1
        end do
        i = i+1
    end do
    !On affiche la matrice entiere
    write (*,"(/,'Voici les entrees de la matrice : ')")
    write(*,*)
    do i=1,L
        do j=1,C
            write (*,"(4x,i4)",advance = 'no')A(i,j)
        end do
        write(*,*)
    end do
    write (*,*)
    !trouver le minimum sur chaque ligne de A
    do i=1,L
        minimum(i)=minval(A(i,:))
    end do
    write (*,"(/,'Voici les entrees de minimum : ')")
    write(*,*)
    do i=1,L
        write (*,"(4x,i4)",advance = 'no')minimum(i)
    end do
    write (*,*)
    write(*,*)
    !On trouve la valeur maximum dans le vecteur 'minimum' 
    write (*,"('. Le maximum des minimum des lignes de A vaut : 'i4'')")maxval(minimum)
    stop "Max et Min..."
    end program TP2exo3