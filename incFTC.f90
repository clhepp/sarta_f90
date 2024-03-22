MODULE incFTC
! include file
! 
! 
! 

! include variables and constants.
real, PARAMETER :: pi = 3.1415926535

! Bandpass
integer, PARAMETER :: MXCHAN = 560          ! 

! nonLTE parameters
integer, parameter :: NCHNTE = 560


! Coefficient file names
character(len=80), PARAMETER :: FNCOFN='./data/nlte_ann_model_v3.txt'
!character(len=80) FNCOFN 
!PARAMETER(FNCOFN='./data/nlte_ann_model.txt')

!      ----------------
!      I/O unit numbers
!     ----------------
!      Note: these units are not explicitly openned by the sarta code,
!      they should be set to standard I/O units for your compiler
!       INTEGER IOINFO  ! unit number for non-error info messages (6)
!       INTEGER IOERR   ! unit number for error messages (2 or 6)
!       PARAMETER( IOINFO = 6 )
!       PARAMETER( IOERR = 0 )
!
!    unit IOUN: used by routines RDCOEF and RDPROF.
       INTEGER,  PARAMETER :: IOUN = 11         ! I/O unit number


end MODULE incFTC