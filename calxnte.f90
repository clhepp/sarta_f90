subroutine calxnte(ICHAN,FCHAN,IP_YMAX,IP_YMIN,IP_XMAX,IP_XMIN,b1,b2, &
        IW,LW,OP_YMAX,OP_YMIN,OP_XMAX,OP_XMIN, IPROF,PREDNTE, DRAD)

! Adjust a LTE atmospheric radiance for a non-LTE upper atmosphere.


!CALL PROTOCOL:
!    CALNTE( INDCHN, TEMP, SUNCOS, SCOS1, VSEC1,
!           NCHNTE, CLISTN, COEFN, CO2TOP, RAD)

! included files
USE incFTC

implicit NONE

! Inputs
integer,dimension(NCHNTE) :: ICHAN
real(4),dimension(NCHNTE) :: IP_YMAX, IP_YMIN, B2, OP_YMAX, OP_YMIN, OP_XMAX, OP_XMIN, FCHAN
real(4),dimension(NCHNTE,4) :: IP_XMAX, IP_XMIN
real(4),dimension(NCHNTE,10) :: B1, LW
real(4),dimension(NCHNTE,10,4) :: IW
real(4),dimension(10,4) :: IW1
real(4),dimension(10) :: LW1, B21
real :: sangle, vangle, ptemp1, ptemp2
real(4),dimension(4,5358) :: PREDNTE
integer :: ich, IPROF 
! Input/Output
REAL, dimension(NCHNTE) :: DRAD
! Local
real :: x1, x2, x3, x4
real, dimension(4) :: x0
real, dimension(10) :: y1, y1a 
real :: y2, res1, res2, res3, res4

! Test case [cos(sangle), vangle, ptemp1, ptemp2]
real, dimension(4) :: input = [1.0000, 0.0,  216.9349,  263.5031];
!input = PREDNTE(:,IPROF);
write(6,*) 'PREDNTE(:,1)= ', PREDNTE(:,1)
write( *, * ) 'Press Enter to continue'
      read( *, * ) 
! Assemble independant variables:
!x1 = sangle
!x2 = vangle
!x3 = ptemp1     ! mean(tprof(1:5,:),1);
!x4 = ptemp2     ! mean(tprof(6:9,:),1);
!x1 = cos(sangle*pi/180);
!x0 = [x1, x2, x3, x4]

! Pass through the ANN matrix multiplication - assumes tansig nn
! transfer function (net.layers{}.transferFcn).
do ich = 1,NCHNTE
   IW1 = RESHAPE(IW(ich,:,:), (/10, 4/))
   LW1 = LW(ich,:)     !   RESHAPE(LW(ich,:), (/1, 10/) )

!do i=1,10
!   y1a(i) = IW1(i,:) * input(:)
!enddo
   y1a = ( matmul(IW1,input) );    ! tanh(IW * input + b1)
!   print*, y1a
   y1 = tanh(y1a + B1(ich,:)) 

   y2 = dot_product(LW1,y1)    !   dot_product(LW1,y1) + B2;
!   write(6,*), y2
!  (y2-OP_YMIN).* (OP_XMAX-OP_XMIN) /(OP_YMAX-OP_YMIN) + OP_XMIN;
   res1 = (y2 - OP_YMIN(ich)) * (OP_XMAX(ich) - OP_XMIN(ich))
   DRAD(ich) = res1/(OP_YMAX(ich)-OP_YMIN(ich)) + OP_XMIN(ich)
!
enddo
print*, DRAD(1)
!DRAD(1) = res/1000.0          ! convert to Watts.cm-1.sr-1

!write( *, * ) 'Press Enter to continue'
!   read( *, * ) 
end subroutine calxnte