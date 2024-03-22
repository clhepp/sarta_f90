program main

! main program to read nonLTE and compute delta radiance
!

use incFTC 

implicit none 
!
real(4),dimension(NCHNTE) :: IP_YMAX, IP_YMIN, B2, OP_YMAX, OP_YMIN, OP_XMAX, OP_XMIN,FCHAN
real(4),dimension(NCHNTE,4) :: IP_XMAX, IP_XMIN
real(4),dimension(NCHNTE,10) :: B1, LW
real(4),dimension(NCHNTE,10,4):: IW
real(4),dimension(MXCHAN) :: DRAD
integer,dimension(NCHNTE) :: ICHAN
!
integer :: IPROF,IERR,ich                     ! Atmospheric Profile counter
! for read_r49_regdata
real(4),dimension(NCHNTE,5358) :: DELTAR
real(4),dimension(4,5358) :: PREDNTE


!    
call read_nlte_ann(IP_YMAX, IP_YMIN, IP_XMAX, IP_XMIN,&
 b1, b2, IW, LW, OP_YMAX, OP_YMIN, OP_XMAX, OP_XMIN,ICHAN,FCHAN ) 

call read_r49_regdata(PREDNTE, DELTAR)
write(6,*) 'main:DELTAR(1,1) = ',DELTAR(1,1)

IPROF = 1

call calxnte(ICHAN,FCHAN,IP_YMAX, IP_YMIN, IP_XMAX, IP_XMIN,&
 b1, b2, IW, LW, OP_YMAX, OP_YMIN, OP_XMAX, OP_XMIN,IPROF,&
 PREDNTE, DRAD)
write(6,*) 'main:DRAD(1) = ',DRAD(1)
!
! write one spectrum out to a file
    OPEN(UNIT=IOUN,FILE='./data/out.txt',FORM='FORMATTED',STATUS='NEW',IOSTAT=IERR)
    IF (IERR .NE. 0) THEN
     WRITE(6,1020) IERR, './data/out.txt'
    STOP
    ENDIF
 1020     FORMAT('Error ',I5,' opening output file:',/,A80)

do ich=1,560
    write(IOUN,fmt='(8(E11.4,3X))') DRAD(ich)
enddo

close(IOUN)


end program main