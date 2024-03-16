program Moment_magnetique
    implicit none
    integer :: i,n,tmax,tmin
    real :: mmax,mmin,a,b,moym,moys,moyms,moym2
    real, dimension(:), allocatable :: m,ss
    integer,dimension(:),allocatable :: s
    !--------------------------------------
    !Etendue et nombre de points de mesure
    !--------------------------------------
    mmin = 7.7
    mmax = 8.32
    n = 25
    tmin = 2020
    tmax = 1900
    !--------------------------------------
    !Allocation de la memoire
    !--------------------------------------
    allocate(m(n))
    allocate(s(n))
    allocate(ss(n))
    !--------------------------------------
    !Definiton des vecteurs m(points de mesure) et t(années)
    !--------------------------------------
    m=(/(mmin+(((i-1)*(mmax-mmin))/(n-1)),i=1,n)/)
    write(*,"('Voici le vecteur m : ')");write(*,*)m
    s=(/(tmin+(((i-1)*(tmax-tmin))/(n-1)),i=1,n)/)
    write(*,*)
    write(*,"('Voici le vecteur s :'f4.2)");write(*,*)s
    !--------------------------------------
    !Definition des moyennes des vecteurs
    !--------------------------------------
    moym=sum(m)/size(m)
    moys=sum(s)/size(s)
    moyms=sum(m*s)/size(m*s)
    moym2=sum(m*m)/size(m*m)
    !--------------------------------------
    !Calcul de a et de b + moment = 0
    !--------------------------------------
    call coef_ab(m,s)
    !---------------------------------------
    !Application de la droite aux moindres carrés
    !---------------------------------------
    ss=a*m+b
    !---------------------------------------
    !Ecart de linearite
    !---------------------------------------
    write(*,*)
    write (*,"('L''ecart de linearite vaut :'f14.6)",advance = 'no')ecart_lin(s,ss)
    write(*,*)
    !--------------------------------------------------------
    !On desalloue la memoire des vecteurs
    !--------------------------------------------------------
    deallocate(m)
    deallocate(s)
    deallocate(ss)
    stop "Moment Magnetique Terrestre"
    !--------------------------------------------------------
    !Definition de la subroutine pour determiner a et b
    !--------------------------------------------------------
    contains
    subroutine coef_ab(entree,sortie)
        real,dimension(:),intent(in) :: entree
        integer,dimension(:),intent(in) :: sortie
        real :: moye,moys,moyes,moye2,aa,bb
        moye=sum(entree)/size(entree)
        moys=sum(sortie)/size(sortie)
        moyes=sum(entree*sortie)/size(entree*sortie)
        moye2=sum(entree*entree)/size(entree*entree)

        aa=(moyes-(moys*moye))/(moye2-(moye*moye))
        bb=(moys*moye2-(moyes*moye))/(moye2-(moye*moye))
        
        write(*,"('a = 'f14.6)")aa
        write(*,"('b = 'f14.6)")bb
        write(*,"(/'On peut predire que le moment magnétique terrestre s''annulera en : 'i4)")int(bb)
    end subroutine coef_ab
    !--------------------------------------------------------
    !Definition de la fonction interne de l'ecart lineaire
    !--------------------------------------------------------
    real function ecart_lin(sortie,moindres_carres) result(eclin)
        real,dimension(:),intent(in) :: moindres_carres
        integer,dimension(:),intent(in) :: sortie
        eclin = maxval(sortie-moindres_carres)/maxval(sortie)
    end function ecart_lin
end program Moment_magnetique