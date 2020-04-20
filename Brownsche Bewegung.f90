!  BrownscheBewegung.f90 
!
!  FUNCTIONS:
!  BrownscheBewegung - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: BrownscheBewegung
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program BrownscheBewegung

    implicit none
    
    integer, parameter :: anz_sim = 100
    integer :: n, i,k
    double precision :: position1d = 0d0
    double precision :: position2d(2) = (/0d0, 0d0 /)
    double precision :: position3d(3) = (/0d0, 0d0, 0d0 /)
    double precision :: mittelwert1d = 0d0, mittelwert2d = 0d0, mittelwert3d = 0d0
    
    open(unit= 1,file="random_walk1d.dat",action="write") 
    open(unit= 2,file="random_walk2d.dat",action="write") 
    open(unit= 3,file="random_walk3d.dat",action="write") 
    call RANDOM_SEED()
    
    do n= 3, 300 !Für diese Schrittzahl n simuliert
        do k = 1, anz_sim !Anzahl an Pfaden
            do i = 1, n !Ein kompletter Walk
                call move1d(position1d)
                call move2d(position2d)
                call move3d(position3d)
            end do
            mittelwert1d = mittelwert1d + dsqrt(position1d**2) !damit wir kein negatives Vorzeichen bekommen
            mittelwert2d = mittelwert2d + dsqrt(sum(position2d**2))
            mittelwert3d = mittelwert3d + dsqrt(sum(position3d**2))
            
            position1d = 0d0
            position2d = (/0d0 , 0d0 /)
            position3d = (/0d0, 0d0, 0d0 /)
            
        end do
        mittelwert1d = mittelwert1d / anz_sim
        mittelwert2d = mittelwert2d / anz_sim
        mittelwert3d = mittelwert3d / anz_sim
        write(1, *) n , mittelwert1d
        write(2, *) n , mittelwert2d
        write(3, *) n , mittelwert3d
        mittelwert1d = 0d0
        mittelwert2d = 0d0
        mittelwert3d = 0d0
    end do
    
    close(1)
    close(2)
    close(3)
    end program BrownscheBewegung

    subroutine move1d(pos1d)
        implicit none
        double precision, intent(inout) :: pos1d
        real :: r
        call RANDOM_NUMBER(r)
        if (r > 0.5e0) then
            pos1d = pos1d - 1d0
        else
            pos1d = pos1d + 1d0
        end if     
    end subroutine move1d
    subroutine move2d(pos2d)
        implicit none
        double precision, dimension(2), intent(inout) :: pos2d
        real :: angle
        call RANDOM_NUMBER(angle)
        pos2d(1) = pos2d(1) + cos(2 * acos(-1d0) * angle)
        pos2d(2) = pos2d(2) + sin(2 * acos(-1d0) * angle)
        
    end subroutine move2d
    
    subroutine move3d(pos3d)
        implicit none
        double precision, dimension(3), intent(inout) :: pos3d
        real :: angle1, angle2
        call RANDOM_NUMBER(angle1)
        call RANDOM_NUMBER(angle2)
        pos3d(1) = pos3d(1) + cos(2 * acos(-1d0) * angle1) * sin(2 * acos(-1d0) * angle2)
        pos3d(2) = pos3d(2) + sin(2 * acos(-1d0) * angle1) * sin(2 * acos(-1d0) * angle2)
        pos3d(3) = pos3d(3) + cos(2 * acos(-1d0) * angle2)
        
    end subroutine move3d
    
