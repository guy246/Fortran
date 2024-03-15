!-----------------------------------------------------------
!Définition module (avant fonction)
!-----------------------------------------------------------
module m_moyenne
    contains
    function moyenne(tab)
    real,dimension(:),intent(in) :: tab
    real :: moyenne
    moyenne=sum(tab)/size(tab)
    end function moyenne
end module m_moyenne
program TP5exo22
    !--------------------------------------------------------
    !Definition fonction moyenne externe a la fonction
    !--------------------------------------------------------
    use m_moyenne
    !-------------------------------------------------------
    !Definition des variables
    !-------------------------------------------------------
    integer :: i,n
    real :: B1,T0,R0,Rref,Ru,E,Tmax0,Tmin0,Tmin,Tmax,moyT,moyVS,moyTVS,moyT2,a,b,eclin
    real, dimension(:),allocatable :: T,Rc,VS,VSS
    B1=3300!Kelvin-1
    T0=300!Kelvin
    R0=2000!Ohm
    Rref=5936!Ohm
    Ru=614!Ohm
    E=10!V
    !--------------------------------------------------------
    !Lecture de l''etendue et du nombre de points
    !--------------------------------------------------------
    write(*,"(/,'Saisir l''etendue de mesure : ')",advance='no')
    read *,Tmin0,Tmax0
    write(*,"(/,'Saisir le nombre de temperatures de mesure n : ')",advance='no')
    read *,n
    write(*,*)
    Tmin=Tmin0+273.15
    Tmax=Tmax0+273.15
    !--------------------------------------------------------
    !Allocation de la memoire pour toutes les matrices
    !--------------------------------------------------------
    allocate(T(n))
    allocate(Rc(n))
    allocate(VS(n))
    allocate(VSS(n))
    !---------------------------------------------------------
    !Definition des vecteurs T,VS
    !---------------------------------------------------------
    T=(/(Tmin+(((i-1)*(Tmax-Tmin))/(n-1)),i=1,n)/)
    write(*,"('Voici le vecteur T :')");write(*,*)T
    Rc=R0*exp(B1*((1/T)-(1/T0)))
    VS=(10/6.32)*E*((Rref/(Ru+Rref))-(Rc/(Ru+Rc)))
    write(*,*)
    write(*,"('Voici le vecteur Vs :')");write(*,*)VS
    !----------------------------------------------------------
    !Moyenne de T,VS,T*VS et T²
    !----------------------------------------------------------
    moyT=moyenne(T)
    moyVS=moyenne(VS)
    moyTVS=moyenne(T*VS)
    moyT2=moyenne(T*T)
    !---------------------------------------------------------
    !Definiton de a et b
    !---------------------------------------------------------
    a= (moyTVS-(moyVS*moyT))/(moyT2-(moyT*moyT))
    b= (moyVS*moyT2-(moyTVS*moyT))/(moyT2-(moyT*moyT))
    write (*,"('a='f14.6)",advance = 'no')a
    write(*,*)
    write (*,"('b='f14.6)",advance = 'no')b
    !--------------------------------------------------------
    !Definition de la droite aux moindres carres
    !--------------------------------------------------------
    VSS=a*T+b
    !--------------------------------------------------------
    !Ecart de linearite
    !--------------------------------------------------------
    eclin=maxval(VS-VSS)/maxval(VS)
    write(*,*)
    write (*,"('L''ecart de linearite vaut :'f14.6)",advance = 'no')eclin
    write(*,*)
    !--------------------------------------------------------
    !On desalloue la memoire des vecteurs
    !--------------------------------------------------------
    deallocate(T)
    deallocate(Rc)
    deallocate(VS)
    deallocate(VSS)
    stop "DroiteAuxMoindresCarres"
end program TP5exo22
