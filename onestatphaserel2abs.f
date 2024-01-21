C
C StG 20230104
C This program needs to take three arguments
C stat phase cmode
C cmode is Bayesloc/Replace/Display/Residual/Allpairs
C Bayesloc - writes out 
C --------------------------
C ev_id     sta_id  phase time
C D00000001 NS.VAGH P1    1699408241.439
C D00000001 NS.VAGH S1    1699408242.969
C ...
C --------------------------
C Replace the epoch times in the measurements with the inverted times
C ..........................
C Residual - simply output the same times again but append the obs minus predicted residuals
C --------------------------
C Display adds dble( iabsvl ) to the final absolute values
C and then writes out the relative times based on these values
C to make a plot
C --------------------------
C Allpairs - loops around I and J for all events and writes out
C a line for all I.ne.J with the absolute times - 
C In the output file there will be no repetition and there will
C be a line even for combinations where no measurements exist.
C --------------------------
C 
C
C DMEDET( NABSVM ) - the median epoch time for this event
C We set NRELVL to zero
C INDEV1( NRELVM )
C INDEV2( NRELVM )
C DEPOT1( NRELVM ) - the true epoch time for event 1 of this measurement
C DEPOT2( NRELVM ) - the true epoch time for event 2 of this measurement
C DEOUT1( NRELVM ) - the "epoch time" to output for event 1 of this measurement
C DEOUT2( NRELVM ) - the "epoch time" to output for event 2 of this measurement
C DCCARR( NRELVM )
C DEP1DA( NRELVM ) - DEPOT1( NRELVM ) - dmedet( iev1 )
C DEP2DA( NRELVM ) - DEPOT2( NRELVM ) - dmedet( iev2 )
C
C it then reads in many lines from standard input
C ev1 ev2 ut1 ut2 cstat cphase cc
C if cstat != stat or cphase != phase
C then hop over.
C 
C We set up a character*8 array CEVARR
C and initiate NABSVL = 0
C we identify the index of CEVARR containing ev1 iev1.
C if ev1 is not found then we increase NABSVL by 1 and
C set CEVARR( nabsvl ) to ev1 and INDEV1( NRELVM ) to nabsvl
C
C set 
      PROGRAM onestatphaserel2abs
      IMPLICIT NONE
C
      INTEGER     NIARGS
      INTEGER     IARGC
      INTEGER     IARG
C
      CHARACTER*(24)  CHUTMI
      CHARACTER*(24)  CHUTMJ
      CHARACTER*(200) CHARG
C for the Finland dataset we need 55 events and over 30000 relative readings.
      INTEGER     NABSVM
      PARAMETER ( NABSVM = 60 )
      INTEGER     NRELVM
      PARAMETER ( NRELVM = 50000 )
      INTEGER     LDWRK1
      PARAMETER ( LDWRK1 = NABSVM )
      INTEGER     NABSVL
      INTEGER     NRELVL
      INTEGER     IFIXVL
C
      INTEGER     IERR
C
C NTUSED is number of times used
      INTEGER     NTUSED( NABSVM )
      REAL*8      DMEDET( NABSVM )
      INTEGER     INDEV1( NRELVM )
      INTEGER     INDEV2( NRELVM )
      REAL*8      DEPOT1( NRELVM )
      REAL*8      DEPOT2( NRELVM )
      REAL*8      DEOUT1( NRELVM )
      REAL*8      DEOUT2( NRELVM )
      REAL*8      DCCARR( NRELVM )
      REAL*8      DEP1DA( NRELVM )
      REAL*8      DEP2DA( NRELVM )
      REAL*8      DCCVAL
      REAL*8      DTMPAR( NRELVM )
      REAL*8      DVAL
C 
C Variables for SAC routines
C
      INTEGER     ISACTM( 6 )
      INTEGER     IMON
      INTEGER     IDOM
      INTEGER     IJUL
      INTEGER     IYYYY
      REAL*8      DSECS
C
C Variables for the routine CSARGM
C
      INTEGER     ILEN
      INTEGER     I0
      INTEGER     I1
      INTEGER     I2
      INTEGER     NARGM
      PARAMETER ( NARGM = 7 )
      INTEGER     NARGS, CSLEN, IARGL( NARGM, 2 )
C
      CHARACTER*(20) CEVARR( NABSVM )
      INTEGER        LEVARR( NABSVM )
      INTEGER        LEVMAX
C
      INTEGER        IEV1
      INTEGER        IEV2
      REAL*8         DTIME1
      REAL*8         DTIME2
C
      INTEGER     IABSVL( NRELVM )
      INTEGER     JABSVL( NRELVM )
C
      REAL*8      DRELVA( NRELVM )
      REAL*8      DRELVC( NRELVM )
C DRELVC is a corrected relative time for the adjustment of the absolute base
      REAL*8      DWGHTA( NRELVM )
      REAL*8      DRELRA( NRELVM )
C
      REAL*8      DWORK1( NRELVM )
      REAL*8      DOLDRV( NRELVM )
C
      REAL*8      DABSVA( NABSVM )
      REAL*8      DABSUT( NABSVM )
      REAL*8      DABSCR( NABSVM )
C DABSUT is the absolute time to be output
C DABSCR is the correction to be applied to the relative times
C where we add the subtracted value
C
      REAL*8      DWRKM1( LDWRK1, LDWRK1 )
C
      REAL*8      DSHIFT
C
      INTEGER     N
      INTEGER     IRELVL
      INTEGER     I
      INTEGER     J
      INTEGER     MXITER
      REAL*8      DTOL
C CTARGS is the target station name - LTARGS is the length
      CHARACTER*8 CTARGS
      INTEGER     LTARGS
C CTARGP is the target phase name - LTARGP is the length
      CHARACTER*8 CTARGP
      INTEGER     LTARGP
C     imode is the output mode
      INTEGER     IMODE
C
      MXITER  = 1000
      DTOL    = 0.0001d0
      NIARGS  = IARGC()
      IF ( NIARGS.NE.3 ) THEN
        WRITE (6,*) 'Usage: CTARGS    CTARGP    MODE    '
        WRITE (6,*) '        PDAR         P     Bayesloc     '
        WRITE (6,*) '        PDAR         P     Replace     '
        WRITE (6,*) '        PDAR         P     Residual     '
        WRITE (6,*) '        PDAR         P     Display     '
        CALL EXIT(1)
      ENDIF
C
      CHARG  = ' '
      IARG   = 1
      CALL GETARG( IARG, CHARG )
      LTARGS = 8
      DO I = 2, 8
        IF ( CHARG(I:I).EQ.' ' .AND. LTARGS.EQ.8 ) LTARGS = I-1
      ENDDO
      CTARGS           = '        '
      CTARGS(1:LTARGS) = CHARG(1:LTARGS)
C
      CHARG  = ' '
      IARG   = 2
      CALL GETARG( IARG, CHARG )
      LTARGP = 8
      DO I = 2, 8
        IF ( CHARG(I:I).EQ.' ' .AND. LTARGP.EQ.8 ) LTARGP = I-1
      ENDDO
      CTARGP           = '        '
      CTARGP(1:LTARGP) = CHARG(1:LTARGP)
C
      IMODE  = 0
      CHARG  = ' '
      IARG   = 3
      CALL GETARG( IARG, CHARG )
      IF ( CHARG(1:9).EQ.'Bayesloc ' ) IMODE = 1
      IF ( CHARG(1:9).EQ.'Replace  ' ) IMODE = 2
      IF ( CHARG(1:9).EQ.'Residual ' ) IMODE = 3
      IF ( CHARG(1:9).EQ.'Display  ' ) IMODE = 4
      IF ( CHARG(1:9).EQ.'Allpairs ' ) IMODE = 5
C
      PRINT *, CTARGS, CTARGP, LTARGS, LTARGP, IMODE
      IF ( IMODE.EQ.0 ) THEN
        WRITE (6,*) 'IMODE not selected.'
        WRITE (6,*) 'Choose 1: Bayesloc'
        WRITE (6,*) '       2: Replace'
        WRITE (6,*) '       3: Residual'
        WRITE (6,*) '       4: Display'
        CALL EXIT(1)
      ENDIF
C
      NARGS  = 7
      CSLEN  = 200
      NABSVL = 0
      NRELVL = 0
      LEVMAX = 0
 50   CONTINUE
      CHARG = ' '
      READ (5,'(A)',ERR=50,END=60) CHARG
      IF ( CHARG(1:1).EQ.'*' ) GOTO 50
      IF ( CHARG(1:1).EQ.'#' ) GOTO 50
C
C This line should contain 7 terms
C CEV1
C CEV2
C CHTIM1
C CHTIM2
C CSTAT
C CPHASE
C CCV
C
      CALL CSARGM( CHARG, NARGS, NARGM, CSLEN, IARGL, IERR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'CSARGM returned IERR = ', IERR
        GOTO 99
      ENDIF
C
C We read in all items!
C So make an early exit of this line if the station or phase is not
C the target.
C
      IARG   = 5
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILEN   = I2 - I1 + 1
      IF ( ILEN.NE.LTARGS ) GOTO 50
      IF ( CHARG(I1:I2).NE.CTARGS(1:LTARGS)  )   GOTO 50
C So we know this line corresponds to the right station
      IARG   = 6
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILEN   = I2 - I1 + 1 
      IF ( ILEN.NE.LTARGP ) GOTO 50
      IF ( CHARG(I1:I2).NE.CTARGP(1:LTARGP)  )   GOTO 50
C So we know this line corresponds to the right phase
C Now we have to identify the event number 1
c     WRITE (6,'(A)') CHARG
C
C Need to find IEV1
C
      IARG   = 1
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILEN   = I2 - I1 + 1 
      IF ( ILEN.GT.20 ) THEN
        WRITE (6,*) 'Invalid length for event name ', CHARG(I1:I2)
        GOTO 99
      ENDIF
      IEV1   = 0
      DO I = 1, NABSVL
        IF ( ILEN.EQ.LEVARR( I ) ) THEN
          IF ( CHARG(I1:I2).EQ.CEVARR(I)(1:ILEN) ) IEV1 = I
        ENDIF
      ENDDO
      IF ( IEV1.EQ.0 ) THEN
C       .
C       . event IEV1 not found. Make a new
C       .
        IF ( NABSVL.EQ.NABSVM ) THEN
          WRITE (6,*) 'Error: unable to increase NABSVL'
          GOTO 99
        ENDIF
C       .
        NABSVL                   = NABSVL + 1
        NTUSED( NABSVL )         = 0
        CEVARR( NABSVL )         = ' '
        CEVARR( NABSVL )(1:ILEN) = CHARG(I1:I2)
        LEVARR( NABSVL )         = ILEN
        IF ( ILEN.GT.LEVMAX ) LEVMAX = ILEN
        IEV1                     = NABSVL
C       .
      ENDIF
C     .
C Need to find IEV2
C
      IARG   = 2
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILEN   = I2 - I1 + 1 
      IF ( ILEN.GT.20 ) THEN
        WRITE (6,*) 'Invalid length for event name ', CHARG(I1:I2)
        GOTO 99
      ENDIF
      IEV2   = 0
      DO I = 1, NABSVL
        IF ( ILEN.EQ.LEVARR( I ) ) THEN
          IF ( CHARG(I1:I2).EQ.CEVARR(I)(1:ILEN) ) IEV2 = I
        ENDIF
      ENDDO
      IF ( IEV2.EQ.0 ) THEN
C       .
C       . event IEV2 not found. Make a new
C       .
        IF ( NABSVL.EQ.NABSVM ) THEN
          WRITE (6,*) 'Error: unable to increase NABSVL'
          GOTO 99
        ENDIF 
C       .
        NABSVL                   = NABSVL + 1
        NTUSED( NABSVL )         = 0
        CEVARR( NABSVL )         = ' '
        CEVARR( NABSVL )(1:ILEN) = CHARG(I1:I2)
        LEVARR( NABSVL )         = ILEN
        IF ( ILEN.GT.LEVMAX ) LEVMAX = ILEN
        IEV2                     = NABSVL
C       .
      ENDIF
C     .
C     . We shouldn't have a case of ev1 = ev2 - if we do, just jump out
C     .
      IF ( IEV1.EQ.IEV2 ) GOTO 50
      NTUSED( IEV1 ) = NTUSED( IEV1 ) + 1
      NTUSED( IEV2 ) = NTUSED( IEV2 ) + 1
C     .
C     . Now need to try to read the epoch time for event 1
C     .
      IARG   = 3
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILEN   = I2 - I1 + 1
      IF ( ILEN.LT.23 ) THEN
        WRITE (6,*) 'Invalid string for time ', CHARG(I1:I2)
        GOTO 99
      ENDIF
      I0     = I1 - 1
      READ ( CHARG(I0+ 1:I0+ 4), '(I4)', END=99, ERR=99 ) ISACTM( 1 )
      READ ( CHARG(I0+ 6:I0+ 7), '(I2)', END=99, ERR=99 ) IMON
      READ ( CHARG(I0+ 9:I0+10), '(I2)', END=99, ERR=99 ) IDOM
      IYYYY  = ISACTM( 1 )
      CALL MD2DOY( IYYYY, IMON, IDOM, IJUL, IERR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Error from MD2DOY: ', IYYYY, IMON, IDOM, IJUL
        GOTO 99
      ENDIF
      ISACTM( 2 ) = IJUL
      READ ( CHARG(I0+12:I0+13), '(I2)', END=99, ERR=99 ) ISACTM( 3 )
      READ ( CHARG(I0+15:I0+16), '(I2)', END=99, ERR=99 ) ISACTM( 4 )
      READ ( CHARG(I0+18:I0+24), *,      END=99, ERR=99 ) DSECS
      ISACTM( 5 ) = INT( DSECS )
      ISACTM( 6 ) = INT( (DSECS - DBLE( INT( DSECS ) ))*1000.0d0 )
      CALL SACH2E( ISACTM, DTIME1 )
c     WRITE (6,'(f20.4)') DTIME1
C
C     . Now need to try to read the epoch time for event 2
C     .
      IARG   = 4
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILEN   = I2 - I1 + 1
      IF ( ILEN.LT.23 ) THEN
        WRITE (6,*) 'Invalid string for time ', CHARG(I1:I2)
        GOTO 99
      ENDIF
      I0     = I1 - 1
      READ ( CHARG(I0+ 1:I0+ 4), '(I4)', END=99, ERR=99 ) ISACTM( 1 )
      READ ( CHARG(I0+ 6:I0+ 7), '(I2)', END=99, ERR=99 ) IMON
      READ ( CHARG(I0+ 9:I0+10), '(I2)', END=99, ERR=99 ) IDOM
      IYYYY  = ISACTM( 1 )
      CALL MD2DOY( IYYYY, IMON, IDOM, IJUL, IERR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Error from MD2DOY: ', IYYYY, IMON, IDOM, IJUL
        GOTO 99
      ENDIF
      ISACTM( 2 ) = IJUL
      READ ( CHARG(I0+12:I0+13), '(I2)', END=99, ERR=99 ) ISACTM( 3 )
      READ ( CHARG(I0+15:I0+16), '(I2)', END=99, ERR=99 ) ISACTM( 4 )
      READ ( CHARG(I0+18:I0+24), *,      END=99, ERR=99 ) DSECS
      ISACTM( 5 ) = INT( DSECS )
      ISACTM( 6 ) = INT( (DSECS - DBLE( INT( DSECS ) ))*1000.0d0 )
      CALL SACH2E( ISACTM, DTIME2 )
c     WRITE (6,'(f20.4)') DTIME2
C
C Finally, read the weight in the 7th column
C
      IARG   = 7
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      READ ( CHARG(I1:I2), *, ERR=99, END=99 ) DCCVAL
C
      PRINT *, IEV1, IEV2, DTIME1, DTIME2, DCCVAL
C
C OK. We have read in all the parameters from this line
C So we add the values to the arrays.
C
c     READ ( CHARG, *, ERR=50, END=50 ) I, J, DRELVL, DWEIGH
      NRELVL = NRELVL + 1
      IF ( NRELVL.GT.NRELVM ) THEN
        WRITE (6,*) 'NRELVL = ', NRELVL
        WRITE (6,*) 'NRELVM = ', NRELVM
        CALL EXIT( 1 )
      ENDIF
      INDEV1( NRELVL ) = IEV1
      INDEV2( NRELVL ) = IEV2
      DEPOT1( NRELVL ) = DTIME1
      DEPOT2( NRELVL ) = DTIME2
      DCCARR( NRELVL ) = DCCVAL
      GOTO 50
 60   CONTINUE
C
C Now we calculate the median times for each of the events
C and subtract them from DEPOT1/2 to give DEP1DA/DEP2DA
C
      print *,' nasbvl = ', nabsvl
      DO I = 1, NABSVL
        N       = 0
        DO IRELVL = 1, NRELVL
C          print *,' hello ', INDEV1( IRELVL ), INDEV2( IRELVL )
          IF ( INDEV1( IRELVL ).EQ.I ) THEN
            N           = N + 1
            DTMPAR( N ) = DEPOT1( IRELVL )
          ENDIF
          IF ( INDEV2( IRELVL ).EQ.I ) THEN
            N           = N + 1
            DTMPAR( N ) = DEPOT2( IRELVL )
          ENDIF
        ENDDO
        IF ( N.NE.NTUSED( I ) ) THEN
          WRITE (6,*) 'Error: N = ', N
          WRITE (6,*) 'NTUSED   = ', NTUSED( I )
          CALL EXIT(1)
        ENDIF
        CALL DMEDVL( IERR, N, DTMPAR, DVAL )
        DMEDET( I ) = DVAL
        print *,' median time = ', DMEDET( I )
C       .
C       . Now put the "demedianed" times in
C       . DEP1DA and DEP2DA
C       .
        IF ( IMODE.EQ.4 ) THEN
          DSHIFT = DBLE(I)
        ELSE
          DSHIFT = 0.0d0
        ENDIF
        DO IRELVL = 1, NRELVL
          IF ( INDEV1( IRELVL ).EQ.I ) THEN
            DEP1DA( IRELVL ) = DEPOT1( IRELVL ) - DMEDET( I ) + DSHIFT
          ENDIF
          IF ( INDEV2( IRELVL ).EQ.I ) THEN
            DEP2DA( IRELVL ) = DEPOT2( IRELVL ) - DMEDET( I ) + DSHIFT
          ENDIF
        ENDDO
C       .
      ENDDO
C     .
      DO IRELVL = 1, NRELVL
        IABSVL( IRELVL ) = INDEV1( IRELVL )
        JABSVL( IRELVL ) = INDEV2( IRELVL )
        DRELVA( IRELVL ) = DEP2DA( IRELVL ) - DEP1DA( IRELVL )
        DWGHTA( IRELVL ) = DCCARR( IRELVL )
      ENDDO
C
      WRITE (6,*) 'Relative value weights before'
      DO I = 1, NRELVL
        WRITE (6,81) I, DWGHTA( I )
      ENDDO
      CALL MDPVUN( NRELVL, DWGHTA )
      WRITE (6,*) 'Relative value weights after'
      DO I = 1, NRELVL
        WRITE (6,81) I, DWGHTA( I )
      ENDDO
      CALL MDPVUN( NRELVL, DWGHTA )
      WRITE (6,*) 'Relative value weights and again'
      DO I = 1, NRELVL
        WRITE (6,81) I, DWGHTA( I )
      ENDDO

      WRITE (6,*) ' CALLING IRLSRA '

      IFIXVL = 0
      CALL IRLSRA( IERR, NABSVL, NRELVL, LDWRK1,         IFIXVL,
     1             IABSVL, JABSVL, DRELVA, DWGHTA, DRELRA,
     2             DABSVA, DWRKM1,
     3             MXITER, DTOL, DWORK1, DOLDRV )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'IRLSRA gave IERR = ', IERR
        CALL EXIT(1)
      ENDIF
C
      WRITE (6,*) 'Absolute values'
      DO I = 1, NABSVL
        WRITE (6,81) I, DABSVA( I )
 81   FORMAT(I3,1X,f20.6)
      ENDDO
      WRITE (6,*) 'Relative values + residuals'
      DO I = 1, NRELVL
        WRITE (6,82) I, DRELVA( I ), DRELRA( I )
 82   FORMAT(I3,1X,f20.6,1X,f20.6)
      ENDDO
C
      DO I = 1, NABSVL
        DO J = 1, NABSVL
          WRITE (6,83) I, J, DWRKM1(I,J)
        ENDDO
      ENDDO
 83   FORMAT('DataCov ',I3,1X,I3,1X,f20.6)
C
C So now we reconstruct the absolute values of the epoch times
C If we want to display we want these to be 1.0d0, 2.0d0 etc.
C
      IF ( IMODE.EQ.4 ) THEN
C       .
C       . Now DABSVA( I ) for 1 to NABSVL contain the 
C       . least squares absolute values
C       .
C       .
        DO I = 1, NABSVL
          DABSUT( I ) = DBLE( I )
          DABSCR( I ) = DABSVA( I ) - DABSUT( I )
          WRITE ( 6, 41 ) CEVARR( I )(1:LEVMAX),
     1                      DABSUT( I ), DABSCR( I )
        ENDDO
        DO IRELVL = 1, NRELVL
          I      = IABSVL( IRELVL )
          J      = JABSVL( IRELVL )
          DRELVC( IRELVL ) = DRELVA( IRELVL ) -
     1                          DABSCR( J ) + DABSCR( I )
          DEOUT1( IRELVL ) = DABSUT( I ) 
          DEOUT2( IRELVL ) = DEOUT1( IRELVL ) + DRELVC( IRELVL )
          WRITE ( 6, 42 ) CEVARR( I )(1:LEVMAX), CEVARR( J )(1:LEVMAX), 
     1                    DEOUT1( IRELVL ), DEOUT2( IRELVL ),
     2                    CTARGS(1:LTARGS), CTARGP(1:LTARGP),
     3                    DCCARR( IRELVL ), DRELRA( IRELVL )
        ENDDO
 41   FORMAT (A,1X,f20.8,1X,f20.8)
 42   FORMAT (A,1X,A,1X,f20.5,1X,f20.5,1X,A,1X,A,1X,f6.3,1X,f10.5)
      ENDIF
C
C Bayesloc - we want to write out an arrival file for Bayesloc
C
      IF ( IMODE.EQ.1 ) THEN
C       .
C       . Want to output the epoch times as the median
C       . times plus DABSVA( I ) - DABSVA( 1 )
C       .
Cdonotwritethisline        WRITE ( 6, 11 ) ' ev.id   sta.id   phase    time '
        DO I = 1, NABSVL
          DABSUT( I ) = DMEDET( I ) + DABSVA( I ) - DABSVA( 1 )
          WRITE ( 6, 12 ) CEVARR( I )(1:LEVMAX),
     1                    CTARGS(1:LTARGS), CTARGP(1:LTARGP),
     2                    DABSUT( I )
        ENDDO
C       .
c11   FORMAT('XXXBAYES ',A)
 12   FORMAT('XXXBAYES ',A,1X,A,1X,A,1X,f20.5)
      ENDIF
C
C Replace - we want to write out a new file but replacing
C the relative times with the absolute times 
C (as given by DMEDET( I ) + DABSVA( I ) - DABSVA( 1 ) )
C
      IF ( IMODE.EQ.2 ) THEN
        DO I = 1, NABSVL
          DABSUT( I ) = DMEDET( I ) + DABSVA( I ) - DABSVA( 1 )
        ENDDO
        DO IRELVL = 1, NRELVL
          I      = IABSVL( IRELVL )
          J      = JABSVL( IRELVL )
          DEOUT1( IRELVL ) = DABSUT( I )
          DEOUT2( IRELVL ) = DABSUT( J )
          CALL E2UTMS( DEOUT1( IRELVL ), CHUTMI )
          CALL E2UTMS( DEOUT2( IRELVL ), CHUTMJ )
          WRITE ( 6, 22 ) CEVARR( I )(1:LEVMAX), CEVARR( J )(1:LEVMAX),
     1                    CHUTMI,           CHUTMJ,
     2                    CTARGS(1:LTARGS), CTARGP(1:LTARGP),
     3                    DCCARR( IRELVL ), DRELRA( IRELVL )
        ENDDO
 22   FORMAT (A,1X,A,1X,A,1X,A,1X,A,1X,A,1X,f6.3,1X,f10.5)
      ENDIF
C
C Residual - we want to write out a new file but with the old
C times with only the residuals appended to the lines.
C
      IF ( IMODE.EQ.3 ) THEN
        DO IRELVL = 1, NRELVL
          I      = IABSVL( IRELVL )
          J      = JABSVL( IRELVL )
          CALL E2UTMS( DEPOT1( IRELVL ), CHUTMI )
          CALL E2UTMS( DEPOT2( IRELVL ), CHUTMJ )
          WRITE ( 6, 32 ) CEVARR( I )(1:LEVMAX), CEVARR( J )(1:LEVMAX),
     1                    CHUTMI,           CHUTMJ,          
     2                    CTARGS(1:LTARGS), CTARGP(1:LTARGP),
     3                    DCCARR( IRELVL ), DRELRA( IRELVL )
        ENDDO
 32   FORMAT (A,1X,A,1X,A,1X,A,1X,A,1X,A,1X,f6.3,1X,f10.5)
      ENDIF
C
      IF ( IMODE.EQ.5 ) THEN
        DSHIFT = 1.0d0
        DO I = 1, NABSVL
          DABSUT( I ) = DMEDET( I ) + DABSVA( I ) - DABSVA( 1 )
        ENDDO
        DO I = 1, NABSVL
          CALL E2UTMS( DABSUT( I ), CHUTMI )
          DO J = 1, NABSVL
            CALL E2UTMS( DABSUT( J ), CHUTMJ )
            IF ( I.NE.J ) THEN
              WRITE ( 6, 52 ) CEVARR(I)(1:LEVMAX), CEVARR(J)(1:LEVMAX),
     1                    CHUTMI,           CHUTMJ,          
     2                    CTARGS(1:LTARGS), CTARGP(1:LTARGP),
     3                    DSHIFT
            ENDIF
          ENDDO
        ENDDO
 52   FORMAT (A,1X,A,1X,A,1X,A,1X,A,1X,A,1X,f6.3)
      ENDIF
C
      CALL EXIT(0)
C
 99   CONTINUE
      WRITE (6,*) 'Error'
      CALL EXIT(1)
      END
C
C*********************************************************************
C Subroutine Character String ARGument Map ***************************
C            -         -      ---      -   ***************************
C Steve Gibbons August 2001 (University of Leeds)                    C
C____________________________________________________________________C
C                                                                    C
C Takes in a character string, LINE, a number of arguments, NARGS,   C
C and a maximum line length. Assuming the arguments to be separated  C
C by spaces, CSARGM will fill the integer array IARGL with the       C
C character positions of the start and end of the argument.          C
C                                                                    C
C____________________________________________________________________C
C                                                                    C
C Input Variables :-                                                 C
C ===============                                                    C
C                                                                    C
C  Character                                                         C
C  ---------                                                         C
C                                                                    C
C     LINE      : String (*) containing input arguments.             C
C                                                                    C
C  Integer                                                           C
C  -------                                                           C
C                                                                    C
C     NARGS     : Number of arguments to be searched for.            C
C     NARGM     : Maximum no. of arguments (i.e. dimension for IARGL C
C     CSLEN     : Length of character string.                        C
C                                                                    C
C Output Variables :-                                                C
C ================                                                   C
C                                                                    C
C     IARGL     : Array dim ( NARGM, 2 ).                            C
C                 On exit, IARGL( I, 1 ) contains the character      C
C                 position where argument I begins: IARGL( I, 2 )    C
C                 contains the character position where arg I ends.  C
C                                                                    C
C     IERR      : =0 all NARGS are found and located.                C
C                 >0 CSLEN was exceeded with only IERR arguments     C
C                 being located.                                     C
C                 IERR = -1 means that no arguments were found.      C
C                 IERR = -2 means that the last argument was not     C
C                 terminated, indicating a possible string longer    C
C                 than was anticipated.                              C
C____________________________________________________________________C
C
C*********************************************************************
      SUBROUTINE CSARGM( LINE, NARGS, NARGM, CSLEN, IARGL, IERR )
      IMPLICIT NONE
C____________________________________________________________________C
C Variable declarations - Parameters ................................C
C
      CHARACTER *(*) LINE
      INTEGER        NARGS, NARGM, CSLEN, IARGL( NARGM, 2 ), IERR
C____________________________________________________________________C
C Variable declarations - Working variables .........................C
C
      INTEGER        I, IARGF
      LOGICAL        OWORDP, OWORDC
C_____________________________________________________________________
C                  **************************************************C
C START OF PROGRAM **************************************************C
C____________________________________________________________________C
C
      IF ( NARGS.LT.0 .OR. NARGS.GT.NARGM ) THEN
        PRINT *,' Subroutine CSARGM.'
        PRINT *,' NARGS = ', NARGS
        PRINT *,' NARGM = ', NARGM
        PRINT *,' Program aborted.'
        STOP
      ENDIF
C
      DO IARGF = 1, NARGM
        IARGL( IARGF, 1 ) = -1
        IARGL( IARGF, 2 ) = -1
      ENDDO
C
      IARGF  = 0
      OWORDP = .FALSE.
      OWORDC = .FALSE.
      DO I = 1, CSLEN
        IF ( LINE(I:I).EQ.' ' ) THEN
          OWORDC = .FALSE.
        ELSE
          OWORDC = .TRUE.
        ENDIF
        IF ( I.EQ.1 ) OWORDP = .FALSE.
C
C Treat the case of us starting a new word
C
        IF ( .NOT. OWORDP .AND. OWORDC ) THEN
          IARGF = IARGF + 1
          IARGL( IARGF, 1 ) = I
        ENDIF
C
C Treat the case of us ending a word
C
        IF ( OWORDP .AND. .NOT. OWORDC ) IARGL( IARGF, 2 ) = I - 1
C
        IF ( IARGF.EQ.NARGS .AND. IARGL( IARGF, 2 ).GT.0 ) THEN
          IERR = 0
          RETURN
        ENDIF
C
        OWORDP = OWORDC
C
      ENDDO
C
      IF ( IARGF.EQ.0 ) THEN
        IERR = -1
        RETURN
      ENDIF
c     print *,' iargf = ', iargf
      IF ( IARGF.LT.NARGS ) IERR = IARGF
      IF ( IARGF.EQ.NARGS .AND. IARGL( IARGF, 2 ).LT.0 ) THEN
        IERR = -2
        IARGL( IARGF, 2 ) = CSLEN
      ENDIF
C
      RETURN
      END
C*********************************************************************
C
C StG dmedvl 
C Written by ChatGPT.
C Edited and checked by me.
C
C IERR   - error flag - zero for good output
C N      - number of points in array
C DARR   - real*8 array (N)
C DMEDN  - median value output
C
      SUBROUTINE DMEDVL( IERR, N, DARR, DMEDN )
      IMPLICIT NONE
C
      INTEGER   IERR
      INTEGER   N
      REAL*8    DARR( N )
      REAL*8    DMEDN
C
      INTEGER   I
      INTEGER   J
      INTEGER   MID
      REAL*8    DTMP
C
      IERR   = 0
      IF ( N.LT.1 ) THEN
        IERR   = 1
        RETURN
      ENDIF
C
C Quick escapes for 1 and 2
C
      IF ( N.EQ.1 ) THEN
        DMEDN = DARR( 1 )
        RETURN
      ENDIF
C
      IF ( N.EQ.2 ) THEN
        DMEDN = 0.5d0*( DARR( 1 ) + DARR( 2 ) )
        RETURN
      ENDIF
C
C Sort the array using bubble sort
      DO I = 1, N-1
        DO J = 1, N-i
          IF ( DARR(J) > DARR(J+1) ) THEN
            DTMP      = DARR(J)
            DARR(J)   = DARR(J+1)
            DARR(J+1) = DTMP
          ENDIF
        ENDDO
      ENDDO
C
      MID = N / 2
      IF ( MOD(N, 2) == 1) THEN
        DMEDN = DARR(MID + 1)
      ELSE
        DMEDN = 0.5d0*( DARR(MID) + DARR(MID+1) )
      ENDIF
C
      RETURN
      END
C
C
C mdpvun
C Make Double Precision Vector Unit Norm.
C
C We have a double precision vector DV with N elements
C
C We want it such that the norm calculated with the
C BLAS routine DNRM2 is 1.0d0
C Returns leaving vector unchanged if it is too small
C
      SUBROUTINE MDPVUN( N, DV )
      IMPLICIT NONE
C
      INTEGER     N
      REAL*8      DV( N )
C
      INTEGER     I
      REAL*8      DSCALE
      REAL*8      DLOW
      PARAMETER ( DLOW = 1.0d-9 )
      INTEGER     INCX
      PARAMETER ( INCX = 1      )
C
      REAL*8      DNRM2
C
      DSCALE = DNRM2( N, DV, INCX )
      IF ( DSCALE.LT.DLOW ) RETURN
C
      DSCALE = 1.0d0/DSCALE
C
      DO I = 1, N
        DV( I ) = DV( I )*DSCALE
      ENDDO
C
      RETURN
      END
C
C Takes 3 integer input parameters, IYYYY (year), IMONTH (1=Jan,
C 2=Feb, 3=Mar etc.) and day of month, IDOM.             
C Returns the day-of-year (IDOY) and error flag IERR.   
C                                                      
      SUBROUTINE MD2DOY( IYYYY, IMONTH, IDOM, IDOY, IERR )
      IMPLICIT NONE
C
C Variable declarations - Parameters
      INTEGER IYYYY, IMONTH, IDOM, IDOY, IERR
C
C Variable declarations - Working variables
      INTEGER IM, MONLEN( 12 )
      LOGICAL OLEAP
C
      IERR        = 0
C
      MONLEN(  1 ) = 31
      MONLEN(  2 ) = 28
      MONLEN(  3 ) = 31
      MONLEN(  4 ) = 30
      MONLEN(  5 ) = 31
      MONLEN(  6 ) = 30
      MONLEN(  7 ) = 31
      MONLEN(  8 ) = 31
      MONLEN(  9 ) = 30
      MONLEN( 10 ) = 31
      MONLEN( 11 ) = 30
      MONLEN( 12 ) = 31
C
      OLEAP = .FALSE.
C
      IF (     (IYYYY - (IYYYY/4)*4).EQ.0    .AND.
     1         (IYYYY - (IYYYY/100)*100).NE.0      ) OLEAP = .TRUE.
C
      IF (     (IYYYY - (IYYYY/400)*400).EQ.0      ) OLEAP = .TRUE.
C
      IF ( OLEAP ) MONLEN( 2 ) = 29
C
      IF ( IMONTH.LT.1 .OR. IMONTH.GT.12 ) THEN
        PRINT *,' Subroutine MD2DOY'
        PRINT *,' IMONTH = ', IMONTH
        IERR   = 1
        RETURN
      ENDIF
C
      IF ( IDOM.LT.1 .OR. IDOM.GT.MONLEN( IMONTH ) ) THEN
        PRINT *,' Subroutine MD2DOY'
        PRINT *,' IDOM = ', IDOM
        PRINT *,' Month ', IMONTH,' has only ',MONLEN( IMONTH ),' days.'
        IERR   = 2
        RETURN
      ENDIF
C
      IDOY   = IDOM
      IF ( IMONTH.EQ.1 ) RETURN
      DO IM = 1, IMONTH - 1
        IDOY   = IDOY   + MONLEN( IM )
      ENDDO
C
      RETURN
      END
C*********************************************************************
C
C Steve Gibbons
C NGI
C
C SAC human time 2 epoch time
C
C ISACTM( 1 ) = year
C ISACTM( 2 ) = Julian day
C ISACTM( 3 ) = hour
C ISACTM( 4 ) = minute
C ISACTM( 5 ) = second
C ISACTM( 6 ) = millisecond
C
      SUBROUTINE SACH2E( ISACTM, SACEPT )
      IMPLICIT NONE
C
      INTEGER ISACTM( 6 )
      REAL*8  SACEPT
C
      INTEGER NDAYS
      INTEGER IY
      INTEGER LEAPYR
C
      NDAYS  = 0
C
      IF ( ISACTM(1).LT.1970 ) THEN
        DO IY = ISACTM(1), 1969
          NDAYS   = NDAYS - 365
          CALL ISLEAP( IY, LEAPYR )
          NDAYS   = NDAYS - LEAPYR
        ENDDO
      ENDIF
C
      IF ( ISACTM(1).GT.1970 ) THEN
        DO IY = 1970, ISACTM(1) - 1
          NDAYS   = NDAYS + 365
          CALL ISLEAP( IY, LEAPYR )
          NDAYS   = NDAYS + LEAPYR
        ENDDO
      ENDIF
C
      NDAYS   = NDAYS + ISACTM( 2 ) - 1
      SACEPT  = 86400.0d0*DBLE( NDAYS )
      SACEPT  = SACEPT + 3600.0d0*DBLE( ISACTM(3) )
      SACEPT  = SACEPT +   60.0d0*DBLE( ISACTM(4) )
      SACEPT  = SACEPT +          DBLE( ISACTM(5) )
      SACEPT  = SACEPT +  0.001d0*DBLE( ISACTM(6) )
C
      RETURN
      END
C
C
C Steve Gibbons
C NGI 2023-09-18
C
C      isleap.f
C
C Inputs a year IYEAR and outputs LEAPYR = 0 if not leap year
C and LEAPYR = 1 if it is.
C
      SUBROUTINE ISLEAP( IYEAR, LEAPYR )
      IMPLICIT NONE
C
      INTEGER IYEAR
      INTEGER LEAPYR
C
      INTEGER ITERM1
      INTEGER ITERM2
      INTEGER ITERM3
C
      LEAPYR = 0
      ITERM1 = IYEAR - (IYEAR/4)*4
      ITERM2 = IYEAR - (IYEAR/100)*100
      ITERM3 = IYEAR - (IYEAR/400)*400
      IF ( ITERM1.EQ.0 .AND. ITERM2.NE.0 .OR. ITERM3.EQ.0 ) LEAPYR = 1
C
      RETURN
      END
C
C--------------
C Subroutine E to UTMS
C Input epoch time E output
C  123456789012345678901234
C 'yyyy-mm-ddThh:mm:ss:mmmm'
C--------------

      SUBROUTINE E2UTMS( DEPOCH, CHUTMS )
      IMPLICIT NONE
C
      REAL*8         DEPOCH
      CHARACTER*(24) CHUTMS
C
      INTEGER        ISACTM( 6 )
      INTEGER        IYEAR
      INTEGER        IJUL
      INTEGER        IMONTH
      INTEGER        IDOM
      INTEGER        IHOUR
      INTEGER        IMINUTE
      INTEGER        ISECOND
      INTEGER        I10000
C
      INTEGER        IERR
C
      CALL SACE2X( DEPOCH, ISACTM )
      IYEAR   = ISACTM( 1 )
      IJUL    = ISACTM( 2 )
      IHOUR   = ISACTM( 3 )
      IMINUTE = ISACTM( 4 )
      ISECOND = ISACTM( 5 )
      I10000  = ISACTM( 6 )
C
      CALL JUL2MD( IYEAR, IJUL, IMONTH, IDOM, IERR )
C
      WRITE ( CHUTMS( 1: 4), '(I4.4)'  ) IYEAR
      CHUTMS( 5: 5)                    = '-'
      WRITE ( CHUTMS( 6: 7), '(I2.2)'  ) IMONTH
      CHUTMS( 8: 8)                    = '-'
      WRITE ( CHUTMS( 9:10), '(I2.2)'  ) IDOM
      CHUTMS(11:11)                    = 'T'
      WRITE ( CHUTMS(12:13), '(I2.2)'  ) IHOUR
      CHUTMS(14:14)                    = ':'
      WRITE ( CHUTMS(15:16), '(I2.2)'  ) IMINUTE
      CHUTMS(17:17)                    = ':'
      WRITE ( CHUTMS(18:19), '(I2.2)'  ) ISECOND
      CHUTMS(20:20)                    = '.'
      WRITE ( CHUTMS(21:24), '(I4.4)'  ) I10000
C
      RETURN
      END
C

C
C Steve Gibbons
C NGI
C
C SAC epoch time 2 human time
C (EXCEPT THAT it is 10000ths of a second - not millisecond)
C
C ISACTM( 1 ) = year
C ISACTM( 2 ) = Julian day
C ISACTM( 3 ) = hour
C ISACTM( 4 ) = minute
C ISACTM( 5 ) = second
C ISACTM( 6 ) = 10000ths of a second
C
      SUBROUTINE SACE2X( SACEPT, ISACTM )
      IMPLICIT NONE
C
      REAL*8  SACEPT
      INTEGER ISACTM( 6 )
C
      INTEGER NDAYIY
      INTEGER NDAYOR
      INTEGER NDAYS
      INTEGER IY
      INTEGER LEAPYR
      REAL*8  DSEC
C
      NDAYS  = INT( SACEPT/86400.0d0 )
      IF ( SACEPT.LT.0 ) NDAYS = NDAYS - 1
      ISACTM( 3 ) = SACEPT/3600.0d0 - 24.0d0*DBLE( NDAYS )
      ISACTM( 4 ) = SACEPT/60.0d0 - 1440.0d0*DBLE( NDAYS ) -
     1                60.0d0*DBLE( ISACTM( 3 ) )
      DSEC        = SACEPT - 86400.0d0*DBLE( NDAYS ) -
     1                        3600.0d0*DBLE( ISACTM( 3 ) ) -
     2                          60.0d0*DBLE( ISACTM( 4 ) )
      ISACTM( 5 ) = INT( DSEC )
      ISACTM( 6 ) = NINT( 10000.0d0*DSEC ) - 10000*ISACTM( 5 )
      IF ( ISACTM( 6 ).GE.10000 ) ISACTM( 6 ) = 9999
      IF ( ISACTM( 6 ).LT.0    ) ISACTM( 6 ) = 0
C
      NDAYOR = NDAYS
      IF ( NDAYOR.LT.0 ) THEN
        ISACTM( 1 ) = 1969
 72     CONTINUE
        CALL ISLEAP(  ISACTM( 1 ), LEAPYR )
        NDAYIY = 365 + LEAPYR
        NDAYS  = NDAYS + NDAYIY
        IF ( NDAYS.GE.0 ) GOTO 73
        ISACTM( 1 ) = ISACTM( 1 ) - 1
        GOTO 72
 73     CONTINUE
      ENDIF
C
      IF ( NDAYOR.GE.0 ) THEN
        ISACTM( 1 ) = 1970
 82     CONTINUE
        CALL ISLEAP(  ISACTM( 1 ), LEAPYR )
        NDAYIY = 365 + LEAPYR
        IF ( NDAYS.LT.NDAYIY ) GOTO 83
        ISACTM( 1 ) = ISACTM( 1 ) + 1
        NDAYS  = NDAYS - NDAYIY
        GOTO 82
 83     CONTINUE
      ENDIF
C
      ISACTM( 2 ) = NDAYS + 1
C
      RETURN
      END
C
C
C Steve Gibbons
C NGI 2023-09-18
C
C JUL2MD
C 
C Julian day to Month and Day
C
      SUBROUTINE JUL2MD( IYEAR, JUL, IMON, IDOM, IERR )
      IMPLICIT NONE
C
      INTEGER IYEAR, IMON, IDOM, IERR
C
      INTEGER IM, NODIM( 12 ), NODIY, JUL2, JUL3, JUL
      INTEGER LEAPYR
C
      IERR   = 0
      NODIM(  1 ) = 31
      NODIM(  2 ) = 28
      NODIM(  3 ) = 31
      NODIM(  4 ) = 30
      NODIM(  5 ) = 31
      NODIM(  6 ) = 30
      NODIM(  7 ) = 31
      NODIM(  8 ) = 31
      NODIM(  9 ) = 30
      NODIM( 10 ) = 31
      NODIM( 11 ) = 30
      NODIM( 12 ) = 31
C
      CALL ISLEAP( IYEAR, LEAPYR )
      NODIY       = 365        + LEAPYR
      NODIM( 2 )  = NODIM( 2 ) + LEAPYR
C
      IF ( JUL.LT.1 .OR. JUL.GT.NODIY ) THEN
        PRINT *,' Subroutine JUL2MD'
        PRINT *,' JUL = ', JUL
        IERR = 1
        RETURN
      ENDIF
C
      JUL2 = 0
      DO IM = 1, 12
        JUL3 = JUL2 + NODIM( IM )
        IF ( JUL.GT.JUL2 .AND. JUL.LE.JUL3 ) THEN
          IMON = IM
          GOTO 91
        ENDIF
        JUL2 = JUL2 + NODIM( IM )
      ENDDO
 91   CONTINUE
C
      IDOM = JUL
      IF ( IMON.EQ.1 ) RETURN
C
      DO IM = 1, IMON - 1
        IDOM = IDOM - NODIM( IM )
      ENDDO
C
      RETURN
      END
C
C
C Steve Gibbons
C 2023-12-07
C
C Iteratively Reweighted Least Squares Relative to Absolute.
C
C The routine VDCR2A solves for NABSVL absolute values in the
C array DABSVA given NRELVL relative values in the array DRELVA
C The array DWGHTA contains an inital set of weights (with high
C weight indicating good quality)
C
C Here we call VDCR2A repeatedly with a weight
C
C DWORK1( I ) = 1.0d0/( DEPS + DSQRT( DABS( DRESID(I) ) ) )
C 
C where DRESID(I) is the residual from the previous iteration.
C
C We stop when ITER exceeds MXITER or when the
C residual from the previous iteration is identical to the residual
C from the current iteration.
C
C See VDCR2A for details of the inputs.
C
C We have the additional integer variable MXITER
C the additional real*8 variables DTOL
C and 2 work arrays of length NRELVL:
C  DWORK1, DOLDRV
C
      SUBROUTINE IRLSRA( IERR, NABSVL, NRELVL, LDATCM,         IFIXVL,
     1                   IABSVL, JABSVL, DRELVA, DWGHTA, DRELRA,
     2                   DABSVA, DATCVM,
     3                   MXITER, DTOL, DWORK1, DOLDRV )
      IMPLICIT NONE
C
      INTEGER     IERR
      INTEGER     NABSVL
      INTEGER     NRELVL
      INTEGER     LDATCM
      INTEGER     IFIXVL
C
      INTEGER     IABSVL( NRELVL )
      INTEGER     JABSVL( NRELVL )
C
      REAL*8      DRELVA( NRELVL )
      REAL*8      DWGHTA( NRELVL )
      REAL*8      DRELRA( NRELVL )
C
      REAL*8      DABSVA( NABSVL )
C
      REAL*8      DATCVM( LDATCM, LDATCM )
C
C ---------
      INTEGER     MXITER
      REAL*8      DTOL
      REAL*8      DWORK1( NRELVL )
      REAL*8      DOLDRV( NRELVL )
C ---------
      INTEGER     I
      INTEGER     NITER
      INTEGER     IWGHTF
C
      INTEGER     INCX
      PARAMETER ( INCX = 1      )
C
      REAL*8      DEPS
      PARAMETER ( DEPS = 1.0d-4 )
C
      REAL*8      DDIFF
C
      REAL*8      DNRM2
C
      IWGHTF = 1
      IERR   = 0
C
C First simply copy the original weights into DWORK1
C and then normalize and then solve for the initial set of
C residuals in DRELRA
C
      DO I = 1, NRELVL
        DWORK1( I ) = DWGHTA( I )
      ENDDO
      CALL MDPVUN( NRELVL, DWORK1 )
C
      CALL VDCR2A( IERR, NABSVL, NRELVL, LDATCM, IWGHTF, IFIXVL,
     1             IABSVL, JABSVL, DRELVA, DWORK1, DRELRA,
     2             DABSVA, DATCVM )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine IRLSRA'
        WRITE (6,*) 'VDCR2A returned IERR = ', IERR
        RETURN
      ENDIF
C
C Now copy the vector of residuals into DOLDRV
C
      DO I = 1, NRELVL
        DOLDRV( I ) = DRELRA( I )
      ENDDO
C
C Now start our loop around the iterations
C
      DO NITER = 1, MXITER
C       .
C       . First construct the vector of weights
C       . from the previous set of residuals
C       .
        DO I = 1, NRELVL
          DWORK1( I ) = 1.0d0/( DEPS + DSQRT( DABS( DOLDRV(I) ) ) )
        ENDDO
        CALL MDPVUN( NRELVL, DWORK1 )
C       .
        CALL VDCR2A( IERR, NABSVL, NRELVL, LDATCM, IWGHTF, IFIXVL,
     1               IABSVL, JABSVL, DRELVA, DWORK1, DRELRA,
     2               DABSVA, DATCVM )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'Subroutine IRLSRA'
          WRITE (6,*) 'VDCR2A returned IERR = ', IERR
          RETURN
        ENDIF
C       .
C       . Put the difference between the old residuals and the
C       . new into DWORK1 as we do not need this array right now
C       .
        DO I = 1, NRELVL
          DWORK1( I ) = DRELRA( I ) - DOLDRV( I )
        ENDDO
        DDIFF = DNRM2( NRELVL, DWORK1, INCX )

        WRITE (6,81) NITER, DDIFF
 81     FORMAT('Iteration ',I8,' : resid_diff = ', f20.6)
        IF ( DDIFF.LT.DTOL ) RETURN
C       .
C       . Now need to copy the new residuals into the old
C       . for the next iteration.
C       .
        DO I = 1, NRELVL
          DOLDRV( I ) = DRELRA( I )
        ENDDO
C       .
      ENDDO
C
      WRITE (6,*) 'ILRSRA: MXITER exceeded without convergence'
C
      RETURN
      END
C
C
C  Steve Gibbons
C  Van Decar and Crosson Relative 2 Absolute routine
C  2023-12-06
C
C  Algorithm based upon that of
C
C title = {Determination of teleseismic relative phase arrival
C times using multi-channel cross-correlation and least squares},
C journal = {Bulletin of the Seismological Society of America},
C author = {van Decar, J C and Crosson, R S},
C year = {1990},
C volume = {80},
C pages = {150--169},
C
C hereafter labelled VDC.
C
C The only principal difference is that VDC add a constraint of zero
C mean whereas I will set (optionally) a single specified value to zero.
C
C We have NABSVL - the number of absolute values wanted.
C                  These can be e.g. times or distances.
C                    These are V(1), V(2), ... , V( NABSVL )
C                  These are the numbers we want to solve for.
C
C         NRELVL - number of relative value measurements.
C                  These are estimates of the values
C                      V( J ) - V( I )
C                  obtained using some or other procedure.
C
C         IABSVL - Integer array with dimension NRELVL
C                  such that 
C                  IABSVL( K ) is a value between 1 and NABSVL.
C         JABSVL - Integer array with dimension NRELVL
C                  such that 
C                  JABSVL( K ) is a value between 1 and NABSVL.
C                   For all K (1 to NRELVL) IABSVL(K) and JABSVL(K)
C                    must be different.
C
C         DRELVA - real*8 array of dimension NRELVL such that
C                  DRELVA( k ) contains the estimate of the value
C                   VJ(k) - VI(k) where I and J are given by
C                      IABSVL and JABSVL.
C
C         DABSVA - real*8 array of dimension NABSVL
C                  Returned with estimates of V(1) --- V( NABSVL )
C
C         DWGHTA - real*8 array of dimension NRELVL
C                  DWGHTA( k ) contains a weighting for the
C                  relative value measurement k.
C         IWGHTF - weight flag.
C                  IF IWGHTF is zero then all measurements are
C                    weighed equally.
C                  IF IWGHTF is above zero then we have set
C                    DWGHTA(k) to a large value if the measurement
C                     is to be weighted highly.
C                  IF IWGHTF is below zero then we have set
C                    DWGHTA(k) to a small value if the measurement
C                     is to be weighted highly.
C                  (i.e. IWGHTF.eq.+1 --> DWGHTA(k) indicator of goodness
C                        IWGHTF.eq.-1 --> DWGHTA(k) indicator of badness )
C
C         IFIXVL - integer between 1 and NABSVL indicating which value
C                   should be fixed to zero.
C                  If IFIXVL is 0 then we do not fix any element to zero.
C                 
C         DRELRA - real*8 array of dimension NRELVL such that
C                   DRELRA( k ) contains the observed minus predicted
C                    value residual i.e.
C                     (v(J)-V(I))_obs - ( v_pred(J) - V_pred(I) )
C                  DRELVA( k ) contains the estimate of the value
C
C         LDATCM leading dimension of work array DATCVM
C                 must be at least equal to NABSVL STEVE
C
C         DATCVM - real*8 array ( LDATCM * LDATCM )
C                  Output with data covariance matrix.
C                  Used as a work array internally.
C                  Is not set on input (contents overwritten).
C
C         IERR   - error flag
C
      SUBROUTINE VDCR2A( IERR, NABSVL, NRELVL, LDATCM, IWGHTF, IFIXVL,
     1                   IABSVL, JABSVL, DRELVA, DWGHTA, DRELRA,
     2                   DABSVA, DATCVM )
      IMPLICIT NONE
C
      INTEGER     IERR
      INTEGER     NABSVL
      INTEGER     NRELVL
      INTEGER     LDATCM
      INTEGER     IWGHTF
      INTEGER     IFIXVL
C
      INTEGER     IABSVL( NRELVL )
      INTEGER     JABSVL( NRELVL )
C
      REAL*8      DRELVA( NRELVL )
      REAL*8      DWGHTA( NRELVL )
      REAL*8      DRELRA( NRELVL )
C
      REAL*8      DABSVA( NABSVL )
C
      REAL*8      DATCVM( LDATCM, LDATCM )
C
C ---------
C
      CHARACTER *(1)   UPLO
      PARAMETER      ( UPLO = 'U' )
      INTEGER          NRHS
      PARAMETER      ( NRHS = 1 )
      REAL*8           DLOW
      PARAMETER      ( DLOW = 1.0d-7 )
C
      INTEGER          IABSV
      INTEGER          IRELVL
      INTEGER          I
      INTEGER          J
      INTEGER          IC
      INTEGER          IR
      INTEGER          INFO
C
      REAL*8           DVAL
      REAL*8           DPREDR
C
C
C --- start executable part of code
C
      IERR   = 0
C
C Return with error if IFIXVL is not valid
C
      IF ( IFIXVL.LT.0 .OR. IFIXVL.GT.NABSVL ) THEN
        WRITE (6,*) 'Subroutine VDCR2A'
        WRITE (6,*) 'NABSVL = ', NABSVL
        WRITE (6,*) 'IFIXVL = ', IFIXVL
        IERR  = 99
        RETURN
      ENDIF
C
C Return with error if we have more unknowns than relative
C observations
C
      IF ( NABSVL.GE.NRELVL ) THEN
        WRITE (6,*) 'Subroutine VDCR2A'
        WRITE (6,*) 'NABSVL = ', NABSVL
        WRITE (6,*) 'NRELVL = ', NRELVL
        IERR  = 99
        RETURN
      ENDIF
C
C Return with error if the DATCVM is too small
C
      IF ( NABSVL.GT.LDATCM ) THEN
        WRITE (6,*) 'Subroutine VDCR2A'
        WRITE (6,*) 'NABSVL = ', NABSVL
        WRITE (6,*) 'LDATCM = ', LDATCM
        IERR  = 99
        RETURN
      ENDIF
C
C Initialize the work matrix
C
      DO J = 1, LDATCM
        DO I = 1, J
          DATCVM( I, J ) = 1.0d0
        ENDDO
      ENDDO
C
C Set array DABSVA to zero
C
      DO IABSV  = 1, NABSVL
        DABSVA( IABSV  ) = 0.0d0
      ENDDO
C
C But now use DABSVA to check that we have at least
C one relative measurement that relates to this absolute
C measurement.
C
      DO IRELVL = 1, NRELVL
        I      = IABSVL( IRELVL )
        J      = JABSVL( IRELVL )
        IF (        I.LT.1       .OR.
     1              J.LT.1       .OR.
     2              I.GT.NABSVL  .OR.
     3              J.GT.NABSVL  .OR.
     4              I.EQ.J           ) THEN
          WRITE (6,*) 'Subroutine VDCR2A'
          WRITE (6,*) 'IRELVL  = ', IRELVL
          WRITE (6,*) 'NABSVL  = ', NABSVL
          WRITE (6,*) 'I       = ', I
          WRITE (6,*) 'J       = ', J
          IERR   = 99
          RETURN
        ENDIF
        DABSVA( I ) = DABSVA( I ) + 0.5d0
        DABSVA( J ) = DABSVA( J ) + 0.5d0
      ENDDO
      DO IABSV  = 1, NABSVL
        IF ( DABSVA( IABSV  ).LT.DLOW ) THEN
          WRITE (6,*) 'Subroutine VDCR2A'
          WRITE (6,*) 'No measurement for abs val ', IABSV 
          IERR   = 99
          RETURN
        ENDIF
        DABSVA( IABSV  ) = 0.0d0
      ENDDO
C
C We now want to make sure that DWGHTA contains
C only positive weights and that the weights are larger
C if the variable is to be weighed heavily.
C
      DO IRELVL = 1, NRELVL
        IF ( DWGHTA( IRELVL ).LT.DLOW ) IWGHTF = 0
      ENDDO
C     .
C     . Invert the weights if they are "upside down"
C     .
      IF ( IWGHTF.LT.0 ) THEN
        DO IRELVL = 1, NRELVL
          DWGHTA( IRELVL ) = 1.0d0/DWGHTA( IRELVL )
        ENDDO
      ENDIF
C
C Set to unity if we do not want to weigh
C
      IF ( IWGHTF.EQ.0 ) THEN
        DO IRELVL = 1, NRELVL
          DWGHTA( IRELVL ) = 1.0d0
        ENDDO
      ENDIF
C
C Fill Right Hand Side and matrix elements
C
      DO IRELVL = 1, NRELVL
        DVAL        = DWGHTA( IRELVL )*DRELVA( IRELVL )
        I           = IABSVL( IRELVL )
        J           = JABSVL( IRELVL )
        DABSVA( I ) = DABSVA( I ) - DVAL
        DABSVA( J ) = DABSVA( J ) + DVAL
C       .
C       . Now replace DVAL with the weight for that measurement
C       .
        DVAL        = DWGHTA( IRELVL )
        IC          = MAX( I, J )
        IR          = MIN( I, J )
        DATCVM(  I,  I ) = DATCVM(  I,  I )  + DVAL
        DATCVM(  J,  J ) = DATCVM(  J,  J )  + DVAL
        DATCVM( IR, IC ) = DATCVM( IR, IC )  - DVAL
C       .
      ENDDO
C
      CALL DPOSV( UPLO, NABSVL, NRHS, DATCVM, LDATCM, DABSVA,
     1            NABSVL, INFO )
      IF ( INFO.NE.0 ) THEN
        WRITE ( 6, * ) 'Subroutine VDCR2A'
        WRITE ( 6, * ) 'DPOSV gave INFO = ', INFO
        IERR   = 99
        RETURN
      ENDIF
C
C Now calculate the predicted relative values
C and the residuals for each of the observations
C
      DO IRELVL = 1, NRELVL
        I                = IABSVL( IRELVL )
        J                = JABSVL( IRELVL )
        DPREDR           = DABSVA( J ) - DABSVA( I )
        DRELRA( IRELVL ) = DRELVA( IRELVL ) - DPREDR
      ENDDO
C
C Make sure that the fixed absolute value is set to zero
C
      IF ( IFIXVL.GT.0 ) THEN
        DVAL   = DABSVA( IFIXVL )
        DO IABSV = 1, NABSVL
          DABSVA( IABSV ) = DABSVA( IABSV ) - DVAL
        ENDDO
      ENDIF
C
C Now calculate the data Covariance matrix.
C We need to recalculate the matrix
C
      DO J = 1, LDATCM
        DO I = 1, J
          DATCVM( I, J ) = 1.0d0
        ENDDO
      ENDDO
      DO IRELVL = 1, NRELVL
        I           = IABSVL( IRELVL )
        J           = JABSVL( IRELVL )
        DVAL        = DWGHTA( IRELVL )
        IC          = MAX( I, J )
        IR          = MIN( I, J )
        DATCVM(  I,  I ) = DATCVM(  I,  I )  + DVAL
        DATCVM(  J,  J ) = DATCVM(  J,  J )  + DVAL
        DATCVM( IR, IC ) = DATCVM( IR, IC )  - DVAL
C       .
      ENDDO
C     ... LAPACK Cholesky factorization
      CALL DPOTRF( UPLO, NABSVL, DATCVM, LDATCM, INFO )
      IF ( INFO.NE.0 ) THEN
        WRITE ( 6, * ) 'Subroutine VDCR2A'
        WRITE ( 6, * ) 'DPOTRF gave INFO = ', INFO
        IERR = 6
        RETURN
      ENDIF
C     ... LAPACK inversion
      CALL DPOTRI( UPLO, NABSVL, DATCVM, LDATCM, INFO )
      IF ( INFO.NE.0 ) THEN
        WRITE ( 6, * ) 'Subroutine VDCR2A'
        WRITE ( 6, * ) 'DPOTRI gave INFO = ', INFO
        IERR = 7
        RETURN
      ENDIF
      DO J = 1, NABSVL
        DO I = 1, NABSVL
          IF ( J.LT.I ) DATCVM( I, J ) = DATCVM( J, I )
        ENDDO
      ENDDO
C
      RETURN
      END
C
