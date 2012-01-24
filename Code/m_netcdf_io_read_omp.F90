!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: m_netcdf_io_read_omp
!
! !DESCRIPTION: Module containing wrapper routines for parallelizing 
!  netCDF data reads with OpenMP.  Each CPU will be given a subset of the
!  global data array to read.
!\\
!\\
! !INTERFACE:
!
MODULE m_netcdf_io_read_omp

!
! !USES:
!
  USE OMP_LIB
  USE m_netcdf_io_read

  IMPLICIT NONE
  PRIVATE

# include "netcdf.inc"
!
! !PUBLIC MEMBER FUNCTIONS:
!
  ! Public interface
  PUBLIC :: NcRd_Omp

  ! Private methods overloaded by public interface
  INTERFACE NcRd_Omp
     MODULE PROCEDURE Ncrd_Omp_3d_R8
     MODULE PROCEDURE Ncrd_Omp_3d_R4
     MODULE PROCEDURE Ncrd_Omp_3d_Int
     MODULE PROCEDURE Ncrd_Omp_4d_R8
     MODULE PROCEDURE Ncrd_Omp_4d_R4
     MODULE PROCEDURE Ncrd_Omp_4d_Int
     !MODULE PROCEDURE Ncrd_Omp_5d_R8   ! add these later
     !MODULE PROCEDURE Ncrd_Omp_5d_R4
     !MODULE PROCEDURE Ncrd_Omp_6d_R8
     !MODULE PROCEDURE Ncrd_Omp_6d_R4
  END INTERFACE
!
! !REMARKS:
!  The routines in this module parallelize over the 1st dimension of the
!  data array.  This is usually longitude, which is typically the largest
!  dimension in the file.  Assumes that the first dimension is divisible by
!  the number of threads on your system.
!                                                                             .
!  Don't bother parallelizing netCDF reads for scalars, 1D, or 2D arrays.  
!  We typically won't gain enough computational advantage to make it
!  worthwhile.
!                                                                             .
!  If you  get an error of the type:
!                                                                             .
!     TestNcPar.x: posixio.c:325: px_rel: Assertion `pxp->bf_offset <= offset 
!      && offset < pxp->bf_offset + (off_t) pxp->bf_extent' failed.
!                                                                             .
!  Then this means that you probably do not have enough on-board memory to
!  read the data in parallel.  Reducing the number of threads (by setting the
!  OMP_NUM_THREADS environment variable) will usually fix the problem.
!
! !REVISION HISTORY:
!  17 Jan 2012 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ncrd_omp_3d_r8
!
! !DESCRIPTION: OpenMP wrapper for routine NcRd.  Reads a 3D REAL*8 data 
!  array from the netCDF file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NcRd_Omp_3d_R8( data, fId, varName )
!
! !INPUT PARAMETERS:
!
    INTEGER,          INTENT(IN)  :: fId              ! netCDF file Id       
    CHARACTER(LEN=*), INTENT(IN)  :: varName          ! Variable name to read
!
! !OUTPUT PARAMETERS:
!
    REAL*8,           INTENT(OUT) :: data(:,:,:)      ! Data array
!
! !REVISION HISTORY:
!  17 Jan 2012 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER :: N, nCpu, div, cpu, I1, J1, T1, I2, J2, T2

    ! Arrays
    INTEGER :: st(3), ct(3)

    !=======================================================================
    ! NcRd_Omp_3d_R8 begins here!
    !=======================================================================

    ! Maximum # of CPU's available
    nCpu = OMP_Get_Max_Threads()

!$OMP PARALLEL DO                                           &  
!$OMP DEFAULT( SHARED )                                     &
!$OMP PRIVATE( N, cpu, div, I1, I2, J1, J2, T1, T2, st, ct )
    DO N = 1, nCpu

       ! # of the CPU we are on
       cpu  = OMP_Get_Thread_Num()

       ! # of grid boxes in 1st dimension to be handled by each CPU
       div  = SIZE( data, 1 ) / nCpu

       ! Starting indices and ending indices: 1st dimension
       I1   = ( cpu * div ) + 1
       I2   = I1 + div - 1

       ! Starting & ending indices: 2nd dimension
       J1  = 1
       J2  = SIZE( data, 2 )
    
       ! Starting & ending indices: 3rd dimension
       T1 = 1
       T2  = SIZE( data, 3 )

       ! Start and count indices for netCDF
       st   = (/ I1,      J1,      T1      /)
       ct   = (/ I2-I1+1, J2-J1+1, T2-T1+1 /)

       ! Read the chunk of data on this CPU
       CALL NcRd( data( I1:I2, J1:J2, T1:T2 ), fId, varName, st, ct )

    ENDDO
!$OMP END PARALLEL DO
       
  END SUBROUTINE NcRd_Omp_3d_R8
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ncrd_omp_3d_r4
!
! !DESCRIPTION: OpenMP wrapper for routine NcRd.  Reads a 3D REAL*4 data 
!  array from the netCDF file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NcRd_Omp_3d_R4( data, fId, varName )
!
! !INPUT PARAMETERS:
!
    INTEGER,          INTENT(IN)  :: fId              ! netCDF file Id       
    CHARACTER(LEN=*), INTENT(IN)  :: varName          ! Variable name to read
!
! !OUTPUT PARAMETERS:
!
    REAL*4,           INTENT(OUT) :: data(:,:,:)      ! Data array
!
! !REVISION HISTORY:
!  17 Jan 2012 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER :: N, nCpu, div, cpu, I1, J1, T1, I2, J2, T2

    ! Arrays
    INTEGER :: st(3), ct(3)

    !=======================================================================
    ! NcRd_Omp_3d_R4 begins here!
    !=======================================================================

    ! Maximum # of CPU's available
    nCpu = OMP_Get_Max_Threads()

!$OMP PARALLEL DO                                           &  
!$OMP DEFAULT( SHARED )                                     &
!$OMP PRIVATE( N, cpu, div, I1, I2, J1, J2, T1, T2, st, ct )
    DO N = 1, nCpu

       ! # of the CPU we are on
       cpu  = OMP_Get_Thread_Num()

       ! # of grid boxes in 1st dimension to be handled by each CPU
       div  = SIZE( data, 1 ) / nCpu

       ! Starting indices and ending indices: 1st dimension
       I1   = ( cpu * div ) + 1
       I2   = I1 + div - 1

       ! Starting & ending indices: 2nd dimension
       J1  = 1
       J2  = SIZE( data, 2 )
    
       ! Starting & ending indices: 3rd dimension
       T1 = 1
       T2  = SIZE( data, 3 )

       ! Start and count indices for netCDF
       st   = (/ I1,      J1,      T1      /)
       ct   = (/ I2-I1+1, J2-J1+1, T2-T1+1 /)

       ! Read the chunk of data on this CPU
       CALL NcRd( data( I1:I2, J1:J2, T1:T2 ), fId, varName, st, ct )

    ENDDO
!$OMP END PARALLEL DO
       
  END SUBROUTINE NcRd_Omp_3d_R4
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ncrd_omp_3d_int
!
! !DESCRIPTION: OpenMP wrapper for routine NcRd.  Reads a 3D INTEGER data 
!  array from the netCDF file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NcRd_Omp_3d_Int( data, fId, varName )
!
! !INPUT PARAMETERS:
!
    INTEGER,          INTENT(IN)  :: fId              ! netCDF file Id       
    CHARACTER(LEN=*), INTENT(IN)  :: varName          ! Variable name to read
!
! !OUTPUT PARAMETERS:
!
    INTEGER,  TARGET, INTENT(OUT) :: data(:,:,:)      ! Data array
!
! !REVISION HISTORY:
!  17 Jan 2012 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER :: N, nCpu, div, cpu, I1, J1, T1, I2, J2, T2

    ! Arrays
    INTEGER :: st(3), ct(3)

    !=======================================================================
    ! NcRd_Omp_3d_R4 begins here!
    !=======================================================================

    ! Maximum # of CPU's available
    nCpu = OMP_Get_Max_Threads()

!$OMP PARALLEL DO                                           &  
!$OMP DEFAULT( SHARED )                                     &
!$OMP PRIVATE( N, cpu, div, I1, I2, J1, J2, T1, T2, st, ct )
    DO N = 1, nCpu

       ! # of the CPU we are on
       cpu  = OMP_Get_Thread_Num()

       ! # of grid boxes in 1st dimension to be handled by each CPU
       div  = SIZE( data, 1 ) / nCpu

       ! Starting indices and ending indices: 1st dimension
       I1   = ( cpu * div ) + 1
       I2   = I1 + div - 1

       ! Starting & ending indices: 2nd dimension
       J1  = 1
       J2  = SIZE( data, 2 )
    
       ! Starting & ending indices: 3rd dimension
       T1 = 1
       T2  = SIZE( data, 3 )

       ! Start and count indices for netCDF
       st   = (/ I1,      J1,      T1      /)
       ct   = (/ I2-I1+1, J2-J1+1, T2-T1+1 /)

       ! Read the chunk of data on this CPU
       CALL NcRd( data( I1:I2, J1:J2, T1:T2 ), fId, varName, st, ct )

    ENDDO
!$OMP END PARALLEL DO
       
  END SUBROUTINE NcRd_Omp_3d_Int
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ncrd_omp_4d_r8
!
! !DESCRIPTION: OpenMP wrapper for routine NcRd.  Reads a 4D REAL*8 data 
!  array from the netCDF file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NcRd_Omp_4d_R8( data, fId, varName )
!
! !INPUT PARAMETERS:
!
    INTEGER,          INTENT(IN)  :: fId              ! netCDF file Id       
    CHARACTER(LEN=*), INTENT(IN)  :: varName          ! Variable name to read
!
! !OUTPUT PARAMETERS:
!
    REAL*8,   TARGET, INTENT(OUT) :: data(:,:,:,:)    ! Data array
!
! !REVISION HISTORY:
!  17 Jan 2012 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER         :: N, nCpu, div, cpu, I1, J1, Z1, T1, I2, J2, Z2, T2

    ! Arrays
    INTEGER         :: st(4), ct(4)
    REAL*8, POINTER :: ptr(:,:,:,:)

    !=======================================================================
    ! NcRd_Omp_3d_R8 begins here!
    !=======================================================================

    ! Maximum # of CPU's available
    nCpu = OMP_Get_Max_Threads()

!$OMP PARALLEL DO                                                        &  
!$OMP DEFAULT( SHARED )                                                  &
!$OMP PRIVATE( N, cpu, div, I1, I2, J1, J2, Z1, Z2, T1, T2, st, ct, ptr )
    DO N = 1, nCpu

       ! # of the CPU we are on
       cpu  = OMP_Get_Thread_Num()

       ! # of grid boxes in 1st dimension to be handled by each CPU
       div  = SIZE( data, 1 ) / nCpu

       ! Starting indices and ending indices: 1st dimension
       I1   = ( cpu * div ) + 1
       I2   = I1 + div - 1

       ! Starting & ending indices: 2nd dimension
       J1  = 1
       J2  = SIZE( data, 2 )
    
       ! Starting & ending indices: 3rd dimension
       Z1 = 1
       Z2  = SIZE( data, 3 )

       ! Starting & ending indices: 3rd dimension
       T1 = 1
       T2  = SIZE( data, 4 )

       ! Start and count indices for netCDF
       st   = (/ I1,      J1,      Z1,      T1      /)
       ct   = (/ I2-I1+1, J2-J1+1, Z2-Z1+1, T2-T1+1 /)

       ! Point to the slice of data
       ptr => data( I1:I2, J1:J2, Z1:Z2, T1:T2 )

       ! Read the chunk of data on this CPU
       CALL NcRd( ptr, fId, varName, st, ct )

       ! Free the pointer
       NULLIFY( ptr )

    ENDDO
!$OMP END PARALLEL DO
       
  END SUBROUTINE NcRd_Omp_4d_R8
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ncrd_omp_4d_r4
!
! !DESCRIPTION: OpenMP wrapper for routine NcRd.  Reads a 4D REAL*4 data 
!  array from the netCDF file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NcRd_Omp_4d_R4( data, fId, varName )
!
! !INPUT PARAMETERS:
!
    INTEGER,          INTENT(IN)  :: fId              ! netCDF file Id       
    CHARACTER(LEN=*), INTENT(IN)  :: varName          ! Variable name to read
!
! !OUTPUT PARAMETERS:
!
    REAL*4,   TARGET, INTENT(OUT) :: data(:,:,:,:)    ! Data array
!
! !REVISION HISTORY:
!  17 Jan 2012 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER         :: N, nCpu, div, cpu, I1, J1, Z1, T1, I2, Z2, J2, T2

    ! Arrays
    INTEGER         :: st(4), ct(4)
    REAL*4, POINTER :: ptr(:,:,:,:)

    !=======================================================================
    ! NcRd_Omp_3d_R4 begins here!
    !=======================================================================

    ! Maximum # of CPU's available
    nCpu = OMP_Get_Max_Threads()

!$OMP PARALLEL DO                                                        &  
!$OMP DEFAULT( SHARED )                                                  &
!$OMP PRIVATE( N, cpu, div, I1, I2, J1, J2, Z1, Z2, T1, T2, st, ct, ptr )
    DO N = 1, nCpu

       ! # of the CPU we are on
       cpu  = OMP_Get_Thread_Num()

       ! # of grid boxes in 1st dimension to be handled by each CPU
       div  = SIZE( data, 1 ) / nCpu

       ! Starting indices and ending indices: 1st dimension
       I1   = ( cpu * div ) + 1
       I2   = I1 + div - 1

       ! Starting & ending indices: 2nd dimension
       J1  = 1
       J2  = SIZE( data, 2 )

       ! Starting & ending indices: 3rd dimension
       Z1  = 1
       Z2  = SIZE( data, 3 )

       ! Starting & ending indices: 4th dimension
       T1 = 1
       T2  = SIZE( data, 4 )

       ! Start and count indices for netCDF
       st   = (/ I1,      J1,      Z1,      T1      /)
       ct   = (/ I2-I1+1, J2-J1+1, Z2-Z1+1, T2-T1+1 /)
     
!$OMP CRITICAL  
       print*, '###################'
       print*, cpu
       print*, st
       print*, ct
!$OMP END CRITICAL

       ! Pointer to the slice of data
       ptr => data( I1:I2, J1:J2, Z1:Z2, T1:T2 )

       print*, size( ptr )

       ! Read the chunk of data on this CPU
       CALL NcRd( ptr, fId, varName, st, ct )

       ! Free the pointer
       NULLIFY( ptr )

    ENDDO
!$OMP END PARALLEL DO
       
  END SUBROUTINE NcRd_Omp_4d_R4
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ncrd_omp_4d_int
!
! !DESCRIPTION: OpenMP wrapper for routine NcRd.  Reads a 4D INTEGER data 
!  array from the netCDF file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NcRd_Omp_4d_Int( data, fId, varName )
!
! !INPUT PARAMETERS:
!
    INTEGER,          INTENT(IN)  :: fId            ! netCDF file Id       
    CHARACTER(LEN=*), INTENT(IN)  :: varName        ! Variable name to read
!
! !OUTPUT PARAMETERS:
!
    INTEGER,  TARGET, INTENT(OUT) :: data(:,:,:,:)  ! Data array
!
! !REVISION HISTORY:
!  17 Jan 2012 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER          :: N, nCpu, div, cpu, I1, J1, Z1, T1, I2, J2, Z2, T2

    ! Arrays
    INTEGER          :: st(4), ct(4)
    INTEGER, POINTER :: ptr(:,:,:,:)

    !=======================================================================
    ! NcRd_Omp_3d_R4 begins here!
    !=======================================================================

    ! Maximum # of CPU's available
    nCpu = OMP_Get_Max_Threads()

!$OMP PARALLEL DO                                                        &  
!$OMP DEFAULT( SHARED )                                                  &
!$OMP PRIVATE( N, cpu, div, I1, I2, J1, J2, Z1, Z2, T1, T2, st, ct, ptr )
    DO N = 1, nCpu

       ! # of the CPU we are on
       cpu  = OMP_Get_Thread_Num()

       ! # of grid boxes in 1st dimension to be handled by each CPU
       div  = SIZE( data, 1 ) / nCpu

       ! Starting indices and ending indices: 1st dimension
       I1   = ( cpu * div ) + 1
       I2   = I1 + div - 1

       ! Starting & ending indices: 2nd dimension
       J1  = 1
       J2  = SIZE( data, 2 )

       ! Starting & ending indices: 3rd dimension
       Z1  = 1
       Z2  = SIZE( data, 2 )
    
       ! Starting & ending indices: 3rd dimension
       T1 = 1
       T2  = SIZE( data, 3 )

       ! Start and count indices for netCDF
       st   = (/ I1,      J1,      Z1,      T1      /)
       ct   = (/ I2-I1+1, J2-J1+1, Z2-Z1+1, T2-T1+1 /)

       ! Point to slice of data
       ptr => data( I1:I2, J1:J2, Z1:Z2, T1:T2 )

       ! Read the chunk of data on this CPU
       CALL NcRd( ptr, fId, varName, st, ct )

       ! Free the pointer
       NULLIFY( ptr )

    ENDDO
!$OMP END PARALLEL DO
       
  END SUBROUTINE NcRd_Omp_4d_Int
!EOC
END MODULE m_netcdf_io_read_omp
