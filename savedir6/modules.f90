! --- This group of modules takes on the role of data declarations and
! --- included common files;  initializations are also done here using
! --- data statements much like a BLOCK DATA structure.
! --- Individual modules are:
!      module mroad1
!      module mroad2
!      module mspray1
!      module mspray2
!      module mqscale
!-----------------------------------------------------------------------
      module mroad1
!-----------------------------------------------------------------------
! --- CALPUFF    Version: 7.3.0    Level: 141201           |MROAD1|
! ---            Constant/Scaled Road-source data
!-----------------------------------------------------------------------
      integer              :: nrd1,nrdseg1,nsfrds
      integer, allocatable :: nptrd1(:)                                 ! (nrd1)
      integer, allocatable :: iroad1(:),newrd1(:)                       ! (nrdseg1)
      integer, allocatable :: idsfrds(:,:)                              ! (mxspec,nrd1)
      integer, allocatable :: ixrefrds(:)                               ! (nsfrds)
      character(len=16), allocatable :: srcnamrd1(:)                    ! (nrd1)
      character(len=40), allocatable :: csfrds(:)                       ! (nsfrds)
      real, allocatable    :: htrd1(:),sz0rd1(:),sy0rd1(:)              ! (nrd1)
      real, allocatable    :: qrd1(:,:)                                 ! (mxspec,nrd1)
      real, allocatable    :: rdlen1(:)                                 ! (nrdseg1)
      real, allocatable    :: xrd1grd(:,:),yrd1grd(:,:),elrd1(:,:)      ! (2,nrdseg1)
! --- Variables:
! ---------------
!
! --- Variables for named roads
!                     NRD1 - integer - Number of roads
!          SRCNAMRD1(nrd1) - char*16 - Road names
!              HTRD1(nrd1) - real    - Effective release height (m)
!             SZ0RD1(nrd1) - real    - Initial sigma z (m)
!             SY0RD1(nrd1) - real    - Initial sigma y (m)
!        QRD1(mxspec,nrd1) - real    - Emission rate (g/s/m) for each
!                                      pollutant
!             NPTRD1(nrd1) - real    - Number of points defining road
!
! --- Variables for road-species pairs with scaled emissions
!                   NSFRDS - integer - Number of road-species pairs
!                                      with emissions scaling factors
!     IDSFRDS(mxspec,nrd1) - integer - Pointer to road-species pair
!                                      index, 0 to NSFRDS
!                                      (0 if no scaling)
!           CSFRDS(nsfrds) - char*40 - List of scale-factor table names
!                                      for road-species pairs
!         IXREFRDS(nsfrds) - integer - Cross-reference pointer from
!                                      road-species pairs to
!                                      scale-factor tables
!
! --- Variables for road segments that emit puffs/slugs
!                  NRDSEG1 - integer - Number of emitting road segments
!                                      (Total over all roads)
!          IROAD1(nrdseg1) - integer - Road number for this segment
!          RDLEN1(nrdseg1) - real    - Road length (m) for this segment
!       XRD1GRD(2,nrdseg1) - real    - X coordinate of the ends of road
!                                      segments in grid units
!                                      (i.e., origin at (0.0,0.0))
!       YRD1GRD(2,nrdseg1) - real    - Y coordinate of the ends of road
!                                      segments in grid units
!                                      (i.e., origin at (0.0,0.0))
!         ELRD1(2,nrdseg1) - real    - Ground elevation of the ends of
!                                      road segments (m MSL)
!          NEWRD1(nrdseg1) - integer - Number of puffs/slugs released
!                                      by each road during the current
!                                      step
!-----------------------------------------------------------------------
      end module mroad1
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      module mroad2
!-----------------------------------------------------------------------
! --- CALPUFF    Version: 7.3.0    Level: 141201           |MROAD2|
! ---            Time-varying Road-source data
!-----------------------------------------------------------------------
      integer              :: nrd2,nrdseg2,nse7,nrddat
      integer, allocatable :: nrdseg2b(:),nrdseg2e(:)                   ! (nrddat)
      integer, allocatable :: ixrem7(:)                                 ! (mxspec)
      integer, allocatable :: nptrd2(:)                                 ! (nrd2)
      integer, allocatable :: iroad2(:),newrd2(:)                       ! (nrdseg2)
      character(len=12), allocatable :: cslst7(:)                       ! (mxspec)
      character(len=16), allocatable :: cid7(:)                         ! (nrd2)
      real,    allocatable :: xmwem7(:)                                 ! (mxspec)
      real,    allocatable :: rdlen2(:)                                 ! (nrdseg2)
      real,    allocatable :: xrd2grd(:,:),yrd2grd(:,:),elrd2(:,:)      ! (2,nrdseg2)
      real,    allocatable :: htrd2(:,:),sz0rd2(:,:),sy0rd2(:,:)        ! (mxqstep,nrd2)
      real,    allocatable :: qrd2(:,:,:)                               ! (mxspec,mxqstep,nrd2)
! --- Arrays for data stored for each RDEMARB.DAT file (nrddat files)
      integer, allocatable :: ibsrc7(:),iesrc7(:),ibdathr7(:),ibsec7(:) ! (nrddat)
      integer, allocatable :: iedathr7(:),iesec7(:)                     ! (nrddat)
      integer, allocatable :: iutmznrd2(:)                              ! (nrddat)
      integer, allocatable :: nstep7(:),mfrd2(:)                        ! (nrddat)
      integer, allocatable :: ndhrqb7(:,:),nsecqb7(:,:)                 ! (mxqstep,nrddat)
      integer, allocatable :: ndhrqe7(:,:),nsecqe7(:,:)                 ! (mxqstep,nrddat)
      real,    allocatable :: xtz7(:),t2btz7(:)                         ! (nrddat)
      real,    allocatable :: feastrd2(:),fnorthrd2(:)                  ! (nrddat)
      real,    allocatable :: rnlat0rd2(:),relon0rd2(:)                 ! (nrddat)
      real,    allocatable :: rnlat1rd2(:),rnlat2rd2(:)                 ! (nrddat)
      character(len=8),  allocatable :: pmaprd2(:),datumrd2(:)          ! (nrddat)
      character(len=4),  allocatable :: utmhemrd2(:),xyunitrd2(:)       ! (nrddat)
      character(len=12), allocatable :: datenrd2(:)                     ! (nrddat)
      character(len=16), allocatable :: verrdarb(:)                     ! (nrddat)
      character(len=132),allocatable :: rddat(:)                        ! (nrddat)
! --- Variables:
! ---------------
!
! --- Variables for named roads
!             NSE7 - integer  - Number of emitted species
!   CSLST7(mxspec) - char*12  - Species identifiers
!   XMWEM7(mxspec) - real     - Molecular weight for each species
!   IXREM7(mxspec) - integer  - Cross referencing array of NSE7
!                               values relating species ordering
!                               in the emissions file to the
!                               ordering in the main conc. array
!             NRD2 - integer  - Total number of roads
!       CID7(nrd2) - char*16  - Road names
!     NPTRD2(nrd2) - real     - Number of points defining road
!
! --- Variables for each file
!           NRDDAT - integer  - Total number of RDEMARB.DAT files
!  NRDSEG2B(nrddat) - integer - Beginning index of road segments for a road
!                               variable emission file
!  NRDSEG2E(nrddat) - integer - Ending index of road segments for a road
!                               variable emission file
!    RDDAT(nrddat) - char*132 - Path & filename for the input CALPUFF
!                               file(s) containing ROAD sources with
!                               arbitrarily-varying location and
!                               emissions
!                               (default: RDEMARB.DAT, for 1 file)
!    MFRD2(nrddat) - integer  - Flag for file type
!                                 0: UNFORMATTED (not supported!)
!                                 1: FORMATTED
! VERRDARB(nrddat) - char*16  - Version of the input CALPUFF
!                               file(s) containing road sources
!                               with arbitrarily-varying location and
!                               emissions
!                               (RDEMARB.DAT)
!   IBSRC7(nrddat) - integer  - Index for first source in a RDEMARB.DAT
!                               file
!   IESRC7(nrddat) - integer  - Index for last source in a RDEMARB.DAT
!                               file
!  IBDATHR7(nrddat)- integer  - Date/hour at beginning of period for
!                               the first data record in the file
!                               (YYYYJJJHH, where YYYY=year,
!                               JJJ=Julian day, HH=hour [00-23 LST])
!   IBSEC7(nrddat) - integer  - Seconds of the first data record in the
!                               file  (0000-3599)
!  IEDATHR7(nrddat)- integer  - Date/hour at end of period for
!                               the last data record in the file
!                               (YYYYJJJHH, where YYYY=year,
!                               JJJ=Julian day, HH=hour [00-23 LST])
!   IESEC7(nrddat) - integer  - Seconds of the last data record in the
!                               file  (0000-3599)
!     XTZ7(nrddat) - real     - Time zone (UTC=LST+XTZ7)
!   T2BTZ7(nrddat) - real     - Hours to ADD to Local Time to obtain
!                               Base Time (xtz7-xbtz)
!
! --- MAP Projection
!   PMAPRD2(nrddat) -char*8    - Character code for map projection
!                                 UTM :  Universal Transverse Mercator
!                                 LCC :  Lambert Conformal Conic
!                                 PS  :  Polar Stereographic
!                                 EM  :  Equatorial Mercator
!                                 LAZA:  Lambert Azimuthal Equal Area
!                                 TTM :  Tangential Transverse Mercator
! UTMHEMRD2(nrddat) -char*4    - Base hemisphere for UTM projection
!                                (S=southern, N=northern)
!  DATUMRD2(nrddat) -char*8    - Datum-Region for grid coordinates
!  DATENRD2(nrddat) -char*12   - NIMA date for datum parameters
!                                 (MM-DD-YYYY  )
! XYUNITRD2(nrddat) -char*4    - Units for coordinates (e.g., KM)
!
!  IUTMZNRD2(nrddat) -integer  - UTM zone for UTM projection
!  FEASTRD2(nrddat)  -real     - False Easting (km) at projection origin
!  FNORTHRD2(nrddat) -real     - False Northing (km) at projection origin
!  RNLAT0RD2(nrddat) -real     - N. latitude & E. longitude of x=0 and y=0
!  RELON0RD2(nrddat)  (deg)      of map projection (Used only if PMAP =
!                                LCC, PS, EM, TTM or LAZA) 
!                                NOTE: longitude neg in western hemisphere
!  RNLAT1RD2(nrddat) - real    - Matching N. latitude(s) for projection
!  RNLAT2RD2(nrddat)  (deg)      (Used only if PMAP3= LCC, PS, or EM)
!                            LCC :  Projection cone slices through
!                                   Earth's surface at XLAT1 and XLAT2
!                            PS  :  Projection plane slices through
!                                   Earth at XLAT1
!                            EM  :  Projection cylinder slices through
!                                   Earth at [+/-] XLAT1
!
! --- Variables for road-segments that emit puffs/slugs
! --- (other properties are taken from the (nrd2) arrays)
!                  NRDSEG2 - integer - Number of emitting road segments
!                                      (Total over all roads)
!          IROAD2(nrdseg2) - integer - Road number for this segment
!          RDLEN2(nrdseg2) - real    - Road length (m) for this segment
!       XRD2GRD(2,nrdseg2) - real    - X coordinate of the ends of road
!                                      segments in grid units
!                                      (i.e., origin at (0.0,0.0))
!       YRD2GRD(2,nrdseg2) - real    - Y coordinate of the ends of road
!                                      segments in grid units
!                                      (i.e., origin at (0.0,0.0))
!         ELRD2(2,nrdseg2) - real    - Ground elevation of the ends of
!                                      road segments (m MSL)
!
! ---  Variable data  ---
!
!    NSTEP7(nrddat)  - integer  - Number of emission steps in
!                                 current timestep for each file
! NDHRQB7(mxqstep,nrddat) & NSECQB7(mxqstep,nrddat)
!                    - integer  - Starting time for which
!                                 emissions data in current set of
!                                 records is valid
!                                 (YYYYJJJHH & SSSS)
! NDHRQE7(mxqstep,nrddat) & NSECQE7(mxqstep,nrddat)
!                    - integer  - Ending time for which
!                                 emissions data in current set of
!                                 records is valid
!                                 (YYYYJJJHH & SSSS)
!    HTRD2(mxqstep,nrd2) - real - Effective height (mAGL)
!   SY0RD2(mxqstep,nrd2) - real - Initial sigma y (m)
!   SZ0RD2(mxqstep,nrd2) - real - Initial sigma z (m)
!  QRD2(mxspec,mxqstep,nrd2)
!                        - real - Emission rate (g/m/s) for each
!                                 pollutant
!    NEWRD2(nrdseg2) - integer  - Number of puffs/slugs released
!                                 by each road during the current
!                                 step
!
!-----------------------------------------------------------------------
      end module mroad2
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      module mspray1
!-----------------------------------------------------------------------
! --- CALPUFF    Version: 7.3.0    Level: 150918           |MSPRAY1|
! ---            Constant/Scaled Road-source data
!-----------------------------------------------------------------------
      integer              :: nsp1,nspseg1,nsfsps
      integer, allocatable :: nptsp1(:)                                 ! (nsp1)
      integer, allocatable :: ispray1(:),newsp1(:)                       ! (nspseg1)
      integer, allocatable :: idsfsps(:,:)                              ! (mxspec,nsp1)
      integer, allocatable :: ixrefsps(:)                               ! (nsfsps)
      character(len=16), allocatable :: srcnamsp1(:)                    ! (nsp1)
      character(len=40), allocatable :: csfsps(:)                       ! (nsfsps)
      real, allocatable    :: htsp1(:),sz0sp1(:),sy0sp1(:)              ! (nsp1)  
      real, allocatable    :: diasp1(:),dcutsp1(:)                      ! (nsp1) (**** ADD DIAMETER, DCUT ****)
      real, allocatable    :: qsp1(:,:)                                 ! (mxspec,nsp1)
      real, allocatable    :: splen1(:)                                 ! (nspseg1)
      real, allocatable    :: xsp1grd(:,:),ysp1grd(:,:),elsp1(:,:)      ! (2,nspseg1)
      real                 :: scspray(2)
! --- Variables:
! ---------------
!
! --- Variables for named roads
!                     NSP1 - integer - Number of spray lines
!          SRCNAMSP1(nsp1) - char*16 - Spray line names
!              HTSP1(nsp1) - real    - Effective release height (m)
!             SZ0SP1(nsp1) - real    - Initial sigma z (m)
!             SY0SP1(nsp1) - real    - Initial sigma y (m)
!             DIASP1(nsp1) - real    - Initial droplet diameter (m)
!            DCUTSP1(nsp1) - real    - Cut-off diameter when evaporation stops (m)
!        QSP1(mxspec,nsp1) - real    - Emission rate (g/s/m) for each
!                                      pollutant
!             NPTSP1(nsp1) - real    - Number of points defining spray line
!
! --- Variables for road-species pairs with scaled emissions
!                   NSFSPS - integer - Number of road-species pairs
!                                      with emissions scaling factors
!     IDSFSPS(mxspec,nsp1) - integer - Pointer to spray line-species pair
!                                      index, 0 to NSFSPS
!                                      (0 if no scaling)
!           CSFSPS(nsfsps) - char*40 - List of scale-factor table names
!                                      for spray line-species pairs
!         IXREFSPS(nsfsps) - integer - Cross-reference pointer from
!                                      spray line-species pairs to
!                                      scale-factor tables
!
! --- Variables for road segments that emit puffs/slugs
!                  NSPSEG1 - integer - Number of emitting spray line segments
!                                      (Total over all spray lines)
!         ISPRAY1(nspseg1) - integer - Spray line number for this segment
!          SPLEN1(nspseg1) - real    - Spray line length (m) for this segment
!       XSP1GRD(2,nspseg1) - real    - X coordinate of the ends of spray line
!                                      segments in grid units
!                                      (i.e., origin at (0.0,0.0))
!       YSP1GRD(2,nspseg1) - real    - Y coordinate of the ends of spray line
!                                      segments in grid units
!                                      (i.e., origin at (0.0,0.0))
!         ELSP1(2,nspseg1) - real    - Ground elevation of the ends of
!                                      spray line segments (m MSL)
!          NEWSP1(nspseg1) - integer - Number of puffs/slugs released
!                                      by each spray line during the current
!                                      step
!-----------------------------------------------------------------------
      end module mspray1
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      module mspray2
!-----------------------------------------------------------------------
! --- CALPUFF    Version: 7.3.0    Level: 150918           |MSPRAY2|
! ---            Time-varying Spray line-source data                              
!-----------------------------------------------------------------------
      integer              :: nsp2,nspseg2,nse8,nspdat
      integer, allocatable :: ixrem8(:)                                 ! (mxspec)
      integer, allocatable :: nptsp2(:)                                 ! (nsp2)
      integer, allocatable :: ispray2(:),newsp2(:),iqtspray2(:)         ! (nspseg2)
      character(len=12), allocatable :: cslst8(:)                       ! (mxspec)
      character(len=16), allocatable :: cid8(:)                         ! (nsp2)
      real,    allocatable :: xmwem8(:)                                 ! (mxspec)
      real,    allocatable :: splen2(:)                                 ! (nspseg2)
      real,    allocatable :: xsp2grd(:,:),ysp2grd(:,:),elsp2(:,:)      ! (2,nspseg2)
      real,    allocatable :: htsp2(:,:),sz0sp2(:,:),sy0sp2(:,:)     ! (mxqstep,nsp2)
      real,    allocatable :: diasp2(:,:),dcutsp2(:,:)                  ! (mxqstep, nsp2
      real,    allocatable :: qsp2(:,:,:)                               ! (mxspec,mxqstep,nsp2)
! --- Arrays for data stored for each SPEMARB.DAT file (nspdat files)
      integer, allocatable :: ibsrc8(:),iesrc8(:),ibdathr8(:),ibsec8(:) ! (nspdat)
      integer, allocatable :: iedathr8(:),iesec8(:)                     ! (nspdat)
      integer, allocatable :: iutmznsp2(:)                              ! (nspdat)
      integer, allocatable :: nstep8(:),mfsp2(:)                        ! (nspdat)
      integer, allocatable :: ndhrqb8(:,:),nsecqb8(:,:)                 ! (mxqstep,nspdat)
      integer, allocatable :: ndhrqe8(:,:),nsecqe8(:,:)                 ! (mxqstep,nspdat)
      real,    allocatable :: xtz8(:),t2btz8(:)                         ! (nspdat)
      real,    allocatable :: feastsp2(:),fnorthsp2(:)                  ! (nspdat)
      real,    allocatable :: rnlat0sp2(:),relon0sp2(:)                 ! (nspdat)
      real,    allocatable :: rnlat1sp2(:),rnlat2sp2(:)                 ! (nspdat)
      character(len=8),  allocatable :: pmapsp2(:),datumsp2(:)          ! (nspdat)
      character(len=4),  allocatable :: utmhemsp2(:),xyunitsp2(:)       ! (nspdat)
      character(len=12), allocatable :: datensp2(:)                     ! (nspdat)
      character(len=16), allocatable :: versparb(:)                     ! (nspdat)
      character(len=132),allocatable :: spdat(:)                        ! (nspdat)
! --- Variables:
! ---------------
!
! --- Variables for named roads
!             NSE8 - integer  - Number of emitted species
!   CSLST8(mxspec) - char*12  - Species identifiers
!   XMWEM8(mxspec) - real     - Molecular weight for each species
!   IXREM8(mxspec) - integer  - Cross referencing array of NSE8
!                               values relating species ordering
!                               in the emissions file to the
!                               ordering in the main conc. array   ( Index road values up 1 for spray sources )
!             NSP2 - integer  - Total number of roads
!       CID8(nsp2) - char*16  - Spray names
!     NPTSP2(nsp2) - real     - Number of points defining spray line
!
! --- Variables for each file
!           NSPDAT - integer  - Total number of SPEMARB.DAT files
!    SPDAT(nspdat) - char*132 - Path & filename for the input CALPUFF
!                               file(s) containing SRAY sources with
!                               arbitrarily-varying location and
!                               emissions
!                               (default: SPEMARB.DAT, for 1 file)
!    MFSP2(nspdat) - integer  - Flag for file type
!                                 0: UNFORMATTED (not supported!)
!                                 1: FORMATTED
! VERSPARB(nspdat) - char*16  - Version of the input CALPUFF
!                               file(s) containing road sources
!                               with arbitrarily-varying location and
!                               emissions
!                               (SPEMARB.DAT)
!   IBSRC8(nspdat) - integer  - Index for first source in a SPEMARB.DAT
!                               file
!   IESRC8(nspdat) - integer  - Index for last source in a SPEMARB.DAT
!                               file
!  IBDATHR8(nspdat)- integer  - Date/hour at beginning of period for
!                               the first data record in the file
!                               (YYYYJJJHH, where YYYY=year,
!                               JJJ=Julian day, HH=hour [00-23 LST])
!   IBSEC8(nspdat) - integer  - Seconds of the first data record in the
!                               file  (0000-3599)
!  IEDATHR8(nspdat)- integer  - Date/hour at end of period for
!                               the last data record in the file
!                               (YYYYJJJHH, where YYYY=year,
!                               JJJ=Julian day, HH=hour [00-23 LST])
!   IESEC8(nspdat) - integer  - Seconds of the last data record in the
!                               file  (0000-3599)
!     XTZ8(nspdat) - real     - Time zone (UTC=LST+XTZ7)
!   T2BTZ8(nspdat) - real     - Hours to ADD to Local Time to obtain
!                               Base Time (xtz7-xbtz)
!
! --- MAP Projection
!   PMAPSP2(nspdat) -char*8    - Character code for map projection
!                                 UTM :  Universal Transverse Mercator
!                                 LCC :  Lambert Conformal Conic
!                                 PS  :  Polar Stereographic
!                                 EM  :  Equatorial Mercator
!                                 LAZA:  Lambert Azimuthal Equal Area
!                                 TTM :  Tangential Transverse Mercator
! UTMHEMSP2(nspdat) -char*4    - Base hemisphere for UTM projection
!                                (S=southern, N=northern)
!  DATUMSP2(nspdat) -char*8    - Datum-Region for grid coordinates
!  DATENSP2(nspdat) -char*12   - NIMA date for datum parameters
!                                 (MM-DD-YYYY  )
! XYUNITSP2(nspdat) -char*4    - Units for coordinates (e.g., KM)
!
!  IUTMZNSP2(nspdat) -integer  - UTM zone for UTM projection
!  FEASTSP2(nspdat)  -real     - False Easting (km) at projection origin
!  FNORTHSP2(nspdat) -real     - False Northing (km) at projection origin
!  RNLAT0SP2(nspdat) -real     - N. latitude & E. longitude of x=0 and y=0
!  RELON0SP2(nspdat)  (deg)      of map projection (Used only if PMAP =
!                                LCC, PS, EM, TTM or LAZA) 
!                                NOTE: longitude neg in western hemisphere
!  RNLAT1SP2(nspdat) - real    - Matching N. latitude(s) for projection
!  RNLAT2SP2(nspdat)  (deg)      (Used only if PMAP3= LCC, PS, or EM)
!                            LCC :  Projection cone slices through
!                                   Earth's surface at XLAT1 and XLAT2
!                            PS  :  Projection plane slices through
!                                   Earth at XLAT1
!                            EM  :  Projection cylinder slices through
!                                   Earth at [+/-] XLAT1
!
! --- Variables for spray line-segments that emit puffs/slugs
! --- (other properties are taken from the (nsp2) arrays)
!                  NSPSEG2 - integer - Number of emitting spray line segments
!                                      (Total over all spray lines)
!         ISPRAY2(nspseg2) - integer - Spray line number for this segment
!          SPLEN2(nspseg2) - real    - Spray line length (m) for this segment
!       XSP2GRD(2,nspseg2) - real    - X coordinate of the ends of spray line
!                                      segments in grid units
!                                      (i.e., origin at (0.0,0.0))
!       YSP2GRD(2,nspseg2) - real    - Y coordinate of the ends of spray line
!                                      segments in grid units  
!                                      (i.e., origin at (0.0,0.0))
!         ELSP2(2,nspseg2) - real    - Ground elevation of the ends of
!                                      spray line segments (m MSL)
!
! ---  Variable data  ---
!
!    NSTEP8(nspdat)  - integer  - Number of emission steps in
!                                 current timestep for each file
! NDHRQB8(mxqstep,nspdat) & NSECQB8(mxqstep,nspdat)
!                    - integer  - Starting time for which
!                                 emissions data in current set of
!                                 records is valid
!                                 (YYYYJJJHH & SSSS)
! NDHRQE8(mxqstep,nspdat) & NSECQE8(mxqstep,nspdat)
!                    - integer  - Ending time for which
!                                 emissions data in current set of
!                                 records is valid
!                                 (YYYYJJJHH & SSSS)
!    HTSP2(mxqstep,nsp2) - real - Effective height (mAGL)
!   SY0SP2(mxqstep,nsp2) - real - Initial sigma y (m)
!   SZ0SP2(mxqstep,nsp2) - real - Initial sigma z (m)
!   DIASP2(mxqstep,nsp2) - real - Initial droplet diameter (m)
!  DCUTSP2(mxqstep,nsp2) - real - Cut off diameter for evaporation (m)
!  QSP2(mxspec,mxqstep,nsp2)
!                        - real - Emission rate (g/m/s) for each
!                                 pollutant
!    NEWSP2(nspseg2) - integer  - Number of puffs/slugs released
!                                 by each road during the current
!                                 step
!
!-----------------------------------------------------------------------
      end module mspray2
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      module mqscale
!-----------------------------------------------------------------------
! --- CALPUFF    Version: 7.3.0    Level: 141201          |MQSCALE|
! ---            Emission-Rate Scaling Factors (control file sources)
!-----------------------------------------------------------------------
      integer, parameter   :: nqsftype = 9
      integer              :: nqsfval(nqsftype)
      integer              :: nqsfcol(nqsftype),nqsfrow(nqsftype)
      integer              :: mapivary(6)
      character(len=24)    :: cqsftype(nqsftype)
      real                 :: wqsf(5,13),tqsf(11,13)
      integer              :: nqsftab
      integer, allocatable :: iqsftype(:)                                ! (nqsftab)
      real, allocatable    :: qsftab(:,:)                                ! (mxqsf,nqsftab)
      character(len=40), allocatable :: cqsfname(:)                      ! (nqsftab)
! --- Assignments:
! -----------------
      data nqsfval/1,   12,   7,&
                   24, 168, 288,&
                   6,   36,  12/
      data nqsfcol/1,   12,   7,&
                   24,  24,  24,&
                   6,    6,  12/
      data nqsfrow/1,    1,   1,&
                   1,    7,  12,&
                   1,    6,   1/
      data mapivary/1, 4, 2, 6, 8, 9/
      data cqsftype/                'CONSTANT1               ',&
         'MONTH12                 ','DAY7                    ',&
         'HOUR24                  ','HOUR24_DAY7             ',&
         'HOUR24_MONTH12          ','WSP6                    ',&
         'WSP6_PGCLASS6           ','TEMPERATURE12           '/
! --- NOTE ---------
!           CONSTANT1        1   scaling factor
!           MONTH12          12  scaling factors: months 1-12
!           DAY7             7   scaling factors: days 1-7
!                              [SUNDAY,MONDAY, ... FRIDAY,SATURDAY]
!           HOUR24           24  scaling factors: hours 1-24
!           HOUR24_DAY7      168 scaling factors: hours 1-24,
!                              repeated  7 times:
!                              [SUNDAY,MONDAY, ... FRIDAY,SATURDAY]
!           HOUR24_MONTH12   288 scaling factors: hours 1-24,
!                              repeated 12 times: months 1-12
!           WSP6             6   scaling factors: wind speed classes 1-6
!                              [speed classes (WSCAT)]
!           WSP6_PGCLASS6    36  scaling factors: wind speed classes 1-6
!                              repeated  6 times: PG classes A,B,C,D,E,F
!                              [speed classes (WSCAT)]
!           TEMPERATURE12    12  scaling factors: temp(K) classes 1-12
!                              [temperature classes (TKCAT)]
! -----------------
!
! --- Variables:
! ---------------
!
! --- Variables for defining emission-rate scaling factors
!                 NQSFTAB - integer - Number of tables of
!                                     emissions scaling factors
!       IQSFTYPE(nqsftab) - integer - Index of scale-factor type of
!                                     each table
!       CQSFNAME(nqsftab) - char*40 - Name of each scale-factor table
!   QSFTAB(mxqsf,nqsftab) - real    - Emission scale-factors
!                NQSFTYPE - integer - Number of types of
!                                     emissions scaling factors
!      CQSFTYPE(nqsftype) - char*24 - Name of each scale-factor type
!             MAPIVARY(6) - integer - Map pointer from the 6 IVARY 
!                                     choices to the corresponding
!                                     CQSFTYPE() index
!       NQSFVAL(nqsftype) - integer - Number of scaling factors for
!                                     each type
!                                     (Max must = MXQSF in /params/)
!       NQSFCOL(nqsftype) - integer - Number of print columns for each
!       NQSFROW(nqsftype) - integer - Number of print rows for each
!
! --- Temperature and wind speed classes by source type (13)
!              WQSF(5,13) - real    - Wind speed class boundaries (m/s)
!                                     (boundary is upper limit of class)
!             TQSF(11,13) - real    - Temperature class boundaries (K)
!                                     (boundary is upper limit of class)
!     Source Types are:
!            1 = Point         Constant Emissions
!            2 = Point         Variable Emissions (no WS/T class used)
!            3 = Poly. Area    Constant Emissions
!            4 = Poly. Area    Variable Emissions (no WS/T class used)
!            5 = Line          Constant Emissions
!            6 = Line          Variable Emissions (no WS/T class used)
!            7 = Volume        Constant Emissions
!            8 = Grid Volume   Variable Emissions (no WS/T class used)
!            9 = Boundary Condition
!          (10)= Flare         Constant Emissions
!           11 = Flare         Variable Emissions (no WS/T class used)
!           12 = Road          Constant Emissions
!           13 = Road          Variable Emissions (no WS/T class used)
!
!-----------------------------------------------------------------------
      end module mqscale
!-----------------------------------------------------------------------
