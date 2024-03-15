program TP5exo1
    implicit none
    integer :: i,n
    real :: alpha,beta,mmax,mmin,moym,moys,moyms,moym2,a,b,eclin
    real, dimension(:),allocatable :: m,s,ss
    alpha=2
    beta=5
    !--------------------------------------------------------
    !Lecture de l''etendue et du nombre de points
    !--------------------------------------------------------
    write(*,"(/,'Saisir l''etendue de mesure : ')",advance='no')
    read *,mmin,mmax
    write(*,"(/,'Saisir le nombre de points de mesure n : ')",advance='no')
    read *,n
    write(*,*)
    !--------------------------------------------------------
    !Allocation de la memoire pour toutes les matrices
    !--------------------------------------------------------
    allocate(m(n))
    allocate(s(n))
    allocate(ss(n))
    !---------------------------------------------------------
    !Definition des vecteurs m,s,ms et m2
    !---------------------------------------------------------
    m=(/(mmin+(((i-1)*(mmax-mmin))/(n-1)),i=1,n)/)
    write(*,"('Voici le vecteur m :')");write(*,*)m
    s=alpha*m+beta+(rand()*2)-1
    write(*,*)
    write(*,"('Voici le vecteur s :')");write(*,*)s
    !----------------------------------------------------------
    !Moyenne de m,s,ms et m2
    !----------------------------------------------------------
    moym=moyenne(m)
    moys=moyenne(s)
    moyms=moyenne(m*s)
    moym2=moyenne(m*m)
    !---------------------------------------------------------
    !Definiton de a et b
    !---------------------------------------------------------
    a= (moyms-(moys*moym))/(moym2-(moym*moym))
    b= (moys*moym2-(moyms*moym))/(moym2-(moym*moym))
    write (*,"('a='f8.6)",advance = 'no')a
    write(*,*)
    write (*,"('b='f8.6)",advance = 'no')b
    !--------------------------------------------------------
    !Definition de la droite aux moindres carres
    !--------------------------------------------------------
    ss=a*m+b
    !--------------------------------------------------------
    !Ecart de linearite
    !--------------------------------------------------------
    eclin=maxval(s-ss)/maxval(s)
    write(*,*)
    write (*,"('L''ecart de linearite vaut :'f14.6)",advance = 'no')eclin
    write(*,*)
    !--------------------------------------------------------
    !On desalloue la memoire des vecteurs
    !--------------------------------------------------------
    deallocate(m)
    deallocate(s)
    deallocate(ss)
    stop "DroiteAuxMoindresCarres"
    !--------------------------------------------------------
    !Definition de la fonction interne de la moyenne
    !--------------------------------------------------------
    contains
    function moyenne(tab)
        real,dimension(:),intent(in) :: tab
        real :: moyenne
        moyenne=sum(tab)/size(tab)
    end function moyenne
end program TP5exo1