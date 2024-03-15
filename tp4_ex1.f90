program TP4exo1
    implicit none
    !----------------------------------------------------------------------
    !On définit le type pour stocker les matrices triangulaires supérieures
    !----------------------------------------------------------------------
    type TMatrice
    sequence
    integer :: taille
    character (1) :: nom
    integer, dimension(:), allocatable :: t
    end type TMatrice
    type (TMatrice)::TMat

    integer :: Mincoef,Maxcoef,pballocation,i,j,pi,modif,tr,k
    !----------------------------------------------------------------------
    !On lit en entrée la taille de la matrice
    !----------------------------------------------------------------------
    write(*,"(/,'Saisir lordre de la matrice : ')",advance='no')
    read *,TMat%taille
    !----------------------------------------------------------------------
    !On alloue la matrice TMat
    !----------------------------------------------------------------------
    allocate(TMat%t((TMat%taille*((TMat%taille)+1))/2),stat=pballocation)
    if (pballocation>0) then
        stop "Erreur : Probleme de memoire"
    end if
    !write(*,"('Taille de la matrice :')")TMat%taille

    !----------------------------------------------------------------------
    !On lit en entrée l'intervalle de remplissage
    !----------------------------------------------------------------------
    write(*,"(/,'Saisir lintervalle à respecter pour remplir la matrice :')",advance='no')
    read *,Mincoef,Maxcoef
    !----------------------------------------------------------------------
    !Remplissage du vecteur t aléatoirement
    !----------------------------------------------------------------------
    do i=1,(TMat%taille*((TMat%taille)+1))/2
        TMat%t(i)=floor((rand()*(Maxcoef-Mincoef))+Mincoef)
    end do
    !----------------------------------------------------------------------
    !Affichage matrice triangulaire supérieure
    !----------------------------------------------------------------------
    write(*,'("La matrice obtenue est : ")')
    write(*,*)
    do i=1,TMat%taille
        do j=1,TMat%taille
            if (j>i) then
                write(*,'(i5)',advance='no') 0
            else
                write(*,'(i5)',advance='no') TMat%t((i-1)*i/2+j)
            end if
        end do
        write(*,*)
    end do
    !----------------------------------------------------------------------
    !Proposer de modifier certains termes de TMat: nombre, indices, nv valeur
    !----------------------------------------------------------------------
    write(*,'("Saisir  le nombre de termes a modifier : ")',advance='no')
    read*,modif
    !----------------------------------------------------------------------
    !Modifier les termes à l'aide de l'expression de pi(i,j)
    !----------------------------------------------------------------------
    do i=1,modif
        pi=(i-1)*TMat%taille-((i-2)*(i-1))/2
        k=i
        write(*,'("Saisir lindice de la ligne :")',advance='no')
        read*,k
        write(*,'("Saisir lindice de la colonne :")',advance='no')
        read*,j
        write(*,'("Saisir la nouvelle valeur :")',advance='no')
        read*,TMat%t(pi+j-k+1)
    end do
    write(*,*)
    !---------------------------------------------------------------------
    !Afficher la matrice modifiée
    !---------------------------------------------------------------------
    write(*,'("La matrice modifiee est : ")')
    write(*,*)
    do i=1,TMat%taille
        do j=1,TMat%taille
            if (j>i) then
                write(*,'(i5)',advance='no') 0
            else
                write(*,'(i5)',advance='no') TMat%t((i-1)*i/2+j)
            end if
        end do
        write(*,*)
    end do
    write(*,*)
    !---------------------------------------------------------------------
    !Afficher la solvabilite ou non d'une equation de type Ax=b, b un vecteur quelconque
    !---------------------------------------------------------------------
    do i=1,Tmat%taille
        tr=tr+TMat%t((i-1)*i-2+i)
    end do
    if (tr==0) then
        write(*,'("Lequation Ax=b est solvable.")')
    else
        write(*,'("Lequation Ax=b nest pas solvable.")')
    end if
    !---------------------------------------------------------------------
    !On desalloue la memoire
    !---------------------------------------------------------------------
    deallocate(TMat%t)
    end program TP4exo1