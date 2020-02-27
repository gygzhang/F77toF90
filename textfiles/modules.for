c --- This group of modules takes on the role of data declarations and
c --- included common files;  initializations are also done here using
c --- data statements much like a BLOCK DATA structure.
c --- Individual modules are:
c      module mroad1
c      module mroad2
c      module mspray1
c      module mspray2
c      module mqscale

c-----------------------------------------------------------------------
      module mroad1
c-----------------------------------------------------------------------
c --- CALPUFF    Version: 7.3.0    Level: 141201           |MROAD1|
c ---            Constant/Scaled Road-source data
c-----------------------------------------------------------------------
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

c --- Variables:
c ---------------
c
c --- Variables for named roads
c                     NRD1 - integer - Number of roads
c          SRCNAMRD1(nrd1) - char*16 - Road names
c              HTRD1(nrd1) - real    - Effective release height (m)
c             SZ0RD1(nrd1) - real    - Initial sigma z (m)
c             SY0RD1(nrd1) - real    - Initial sigma y (m)
c        QRD1(mxspec,nrd1) - real    - Emission rate (g/s/m) for each
c                                      pollutant
c             NPTRD1(nrd1) - real    - Number of points defining road
c
c --- Variables for road-species pairs with scaled emissions
c                   NSFRDS - integer - Number of road-species pairs
c                                      with emissions scaling factors
c     IDSFRDS(mxspec,nrd1) - integer - Pointer to road-species pair
c                                      index, 0 to NSFRDS
c                                      (0 if no scaling)
c           CSFRDS(nsfrds) - char*40 - List of scale-factor table names
c                                      for road-species pairs
c         IXREFRDS(nsfrds) - integer - Cross-reference pointer from
c                                      road-species pairs to
c                                      scale-factor tables
c
c --- Variables for road segments that emit puffs/slugs
c                  NRDSEG1 - integer - Number of emitting road segments
c                                      (Total over all roads)
c          IROAD1(nrdseg1) - integer - Road number for this segment
c          RDLEN1(nrdseg1) - real    - Road length (m) for this segment
c       XRD1GRD(2,nrdseg1) - real    - X coordinate of the ends of road
c                                      segments in grid units
c                                      (i.e., origin at (0.0,0.0))
c       YRD1GRD(2,nrdseg1) - real    - Y coordinate of the ends of road
c                                      segments in grid units
c                                      (i.e., origin at (0.0,0.0))
c         ELRD1(2,nrdseg1) - real    - Ground elevation of the ends of
c                                      road segments (m MSL)
c          NEWRD1(nrdseg1) - integer - Number of puffs/slugs released
c                                      by each road during the current
c                                      step
c-----------------------------------------------------------------------
      end module mroad1
c-----------------------------------------------------------------------


c-----------------------------------------------------------------------
      module mroad2
c-----------------------------------------------------------------------
c --- CALPUFF    Version: 7.3.0    Level: 141201           |MROAD2|
c ---            Time-varying Road-source data
c-----------------------------------------------------------------------
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

c --- Arrays for data stored for each RDEMARB.DAT file (nrddat files)

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

c --- Variables:
c ---------------
c
c --- Variables for named roads
c             NSE7 - integer  - Number of emitted species
c   CSLST7(mxspec) - char*12  - Species identifiers
c   XMWEM7(mxspec) - real     - Molecular weight for each species
c   IXREM7(mxspec) - integer  - Cross referencing array of NSE7
c                               values relating species ordering
c                               in the emissions file to the
c                               ordering in the main conc. array
c             NRD2 - integer  - Total number of roads
c       CID7(nrd2) - char*16  - Road names
c     NPTRD2(nrd2) - real     - Number of points defining road
c
c --- Variables for each file
c           NRDDAT - integer  - Total number of RDEMARB.DAT files
c  NRDSEG2B(nrddat) - integer - Beginning index of road segments for a road
c                               variable emission file
c  NRDSEG2E(nrddat) - integer - Ending index of road segments for a road
c                               variable emission file
c    RDDAT(nrddat) - char*132 - Path & filename for the input CALPUFF
c                               file(s) containing ROAD sources with
c                               arbitrarily-varying location and
c                               emissions
c                               (default: RDEMARB.DAT, for 1 file)
c    MFRD2(nrddat) - integer  - Flag for file type
c                                 0: UNFORMATTED (not supported!)
c                                 1: FORMATTED
c VERRDARB(nrddat) - char*16  - Version of the input CALPUFF
c                               file(s) containing road sources
c                               with arbitrarily-varying location and
c                               emissions
c                               (RDEMARB.DAT)
c   IBSRC7(nrddat) - integer  - Index for first source in a RDEMARB.DAT
c                               file
c   IESRC7(nrddat) - integer  - Index for last source in a RDEMARB.DAT
c                               file
c  IBDATHR7(nrddat)- integer  - Date/hour at beginning of period for
c                               the first data record in the file
c                               (YYYYJJJHH, where YYYY=year,
c                               JJJ=Julian day, HH=hour [00-23 LST])
c   IBSEC7(nrddat) - integer  - Seconds of the first data record in the
c                               file  (0000-3599)
c  IEDATHR7(nrddat)- integer  - Date/hour at end of period for
c                               the last data record in the file
c                               (YYYYJJJHH, where YYYY=year,
c                               JJJ=Julian day, HH=hour [00-23 LST])
c   IESEC7(nrddat) - integer  - Seconds of the last data record in the
c                               file  (0000-3599)
c     XTZ7(nrddat) - real     - Time zone (UTC=LST+XTZ7)
c   T2BTZ7(nrddat) - real     - Hours to ADD to Local Time to obtain
c                               Base Time (xtz7-xbtz)
c
c --- MAP Projection
c   PMAPRD2(nrddat) -char*8    - Character code for map projection
c                                 UTM :  Universal Transverse Mercator
c                                 LCC :  Lambert Conformal Conic
c                                 PS  :  Polar Stereographic
c                                 EM  :  Equatorial Mercator
c                                 LAZA:  Lambert Azimuthal Equal Area
c                                 TTM :  Tangential Transverse Mercator
c UTMHEMRD2(nrddat) -char*4    - Base hemisphere for UTM projection
c                                (S=southern, N=northern)
c  DATUMRD2(nrddat) -char*8    - Datum-Region for grid coordinates
c  DATENRD2(nrddat) -char*12   - NIMA date for datum parameters
c                                 (MM-DD-YYYY  )
c XYUNITRD2(nrddat) -char*4    - Units for coordinates (e.g., KM)
c
c  IUTMZNRD2(nrddat) -integer  - UTM zone for UTM projection
c  FEASTRD2(nrddat)  -real     - False Easting (km) at projection origin
c  FNORTHRD2(nrddat) -real     - False Northing (km) at projection origin
c  RNLAT0RD2(nrddat) -real     - N. latitude & E. longitude of x=0 and y=0
c  RELON0RD2(nrddat)  (deg)      of map projection (Used only if PMAP =
c                                LCC, PS, EM, TTM or LAZA) 
c                                NOTE: longitude neg in western hemisphere
c  RNLAT1RD2(nrddat) - real    - Matching N. latitude(s) for projection
c  RNLAT2RD2(nrddat)  (deg)      (Used only if PMAP3= LCC, PS, or EM)
c                            LCC :  Projection cone slices through
c                                   Earth's surface at XLAT1 and XLAT2
c                            PS  :  Projection plane slices through
c                                   Earth at XLAT1
c                            EM  :  Projection cylinder slices through
c                                   Earth at [+/-] XLAT1
c
c --- Variables for road-segments that emit puffs/slugs
c --- (other properties are taken from the (nrd2) arrays)
c                  NRDSEG2 - integer - Number of emitting road segments
c                                      (Total over all roads)
c          IROAD2(nrdseg2) - integer - Road number for this segment
c          RDLEN2(nrdseg2) - real    - Road length (m) for this segment
c       XRD2GRD(2,nrdseg2) - real    - X coordinate of the ends of road
c                                      segments in grid units
c                                      (i.e., origin at (0.0,0.0))
c       YRD2GRD(2,nrdseg2) - real    - Y coordinate of the ends of road
c                                      segments in grid units
c                                      (i.e., origin at (0.0,0.0))
c         ELRD2(2,nrdseg2) - real    - Ground elevation of the ends of
c                                      road segments (m MSL)
c
c ---  Variable data  ---
c
c    NSTEP7(nrddat)  - integer  - Number of emission steps in
c                                 current timestep for each file
c NDHRQB7(mxqstep,nrddat) & NSECQB7(mxqstep,nrddat)
c                    - integer  - Starting time for which
c                                 emissions data in current set of
c                                 records is valid
c                                 (YYYYJJJHH & SSSS)
c NDHRQE7(mxqstep,nrddat) & NSECQE7(mxqstep,nrddat)
c                    - integer  - Ending time for which
c                                 emissions data in current set of
c                                 records is valid
c                                 (YYYYJJJHH & SSSS)
c    HTRD2(mxqstep,nrd2) - real - Effective height (mAGL)
c   SY0RD2(mxqstep,nrd2) - real - Initial sigma y (m)
c   SZ0RD2(mxqstep,nrd2) - real - Initial sigma z (m)
c  QRD2(mxspec,mxqstep,nrd2)
c                        - real - Emission rate (g/m/s) for each
c                                 pollutant
c    NEWRD2(nrdseg2) - integer  - Number of puffs/slugs released
c                                 by each road during the current
c                                 step
c
c-----------------------------------------------------------------------
      end module mroad2
c-----------------------------------------------------------------------

c-----------------------------------------------------------------------
      module mspray1
c-----------------------------------------------------------------------
c --- CALPUFF    Version: 7.3.0    Level: 150918           |MSPRAY1|
c ---            Constant/Scaled Road-source data
c-----------------------------------------------------------------------
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

c --- Variables:
c ---------------
c
c --- Variables for named roads
c                     NSP1 - integer - Number of spray lines
c          SRCNAMSP1(nsp1) - char*16 - Spray line names
c              HTSP1(nsp1) - real    - Effective release height (m)
c             SZ0SP1(nsp1) - real    - Initial sigma z (m)
c             SY0SP1(nsp1) - real    - Initial sigma y (m)
c             DIASP1(nsp1) - real    - Initial droplet diameter (m)
c            DCUTSP1(nsp1) - real    - Cut-off diameter when evaporation stops (m)
c        QSP1(mxspec,nsp1) - real    - Emission rate (g/s/m) for each
c                                      pollutant
c             NPTSP1(nsp1) - real    - Number of points defining spray line
c
c --- Variables for road-species pairs with scaled emissions
c                   NSFSPS - integer - Number of road-species pairs
c                                      with emissions scaling factors
c     IDSFSPS(mxspec,nsp1) - integer - Pointer to spray line-species pair
c                                      index, 0 to NSFSPS
c                                      (0 if no scaling)
c           CSFSPS(nsfsps) - char*40 - List of scale-factor table names
c                                      for spray line-species pairs
c         IXREFSPS(nsfsps) - integer - Cross-reference pointer from
c                                      spray line-species pairs to
c                                      scale-factor tables
c
c --- Variables for road segments that emit puffs/slugs
c                  NSPSEG1 - integer - Number of emitting spray line segments
c                                      (Total over all spray lines)
c         ISPRAY1(nspseg1) - integer - Spray line number for this segment
c          SPLEN1(nspseg1) - real    - Spray line length (m) for this segment
c       XSP1GRD(2,nspseg1) - real    - X coordinate of the ends of spray line
c                                      segments in grid units
c                                      (i.e., origin at (0.0,0.0))
c       YSP1GRD(2,nspseg1) - real    - Y coordinate of the ends of spray line
c                                      segments in grid units
c                                      (i.e., origin at (0.0,0.0))
c         ELSP1(2,nspseg1) - real    - Ground elevation of the ends of
c                                      spray line segments (m MSL)
c          NEWSP1(nspseg1) - integer - Number of puffs/slugs released
c                                      by each spray line during the current
c                                      step
c-----------------------------------------------------------------------
      end module mspray1
c-----------------------------------------------------------------------


c-----------------------------------------------------------------------
      module mspray2
c-----------------------------------------------------------------------
c --- CALPUFF    Version: 7.3.0    Level: 150918           |MSPRAY2|
c ---            Time-varying Spray line-source data                              
c-----------------------------------------------------------------------
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

c --- Arrays for data stored for each SPEMARB.DAT file (nspdat files)

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

c --- Variables:
c ---------------
c
c --- Variables for named roads
c             NSE8 - integer  - Number of emitted species
c   CSLST8(mxspec) - char*12  - Species identifiers
c   XMWEM8(mxspec) - real     - Molecular weight for each species
c   IXREM8(mxspec) - integer  - Cross referencing array of NSE8
c                               values relating species ordering
c                               in the emissions file to the
c                               ordering in the main conc. array   ( Index road values up 1 for spray sources )
c             NSP2 - integer  - Total number of roads
c       CID8(nsp2) - char*16  - Spray names
c     NPTSP2(nsp2) - real     - Number of points defining spray line
c
c --- Variables for each file
c           NSPDAT - integer  - Total number of SPEMARB.DAT files
c    SPDAT(nspdat) - char*132 - Path & filename for the input CALPUFF
c                               file(s) containing SRAY sources with
c                               arbitrarily-varying location and
c                               emissions
c                               (default: SPEMARB.DAT, for 1 file)
c    MFSP2(nspdat) - integer  - Flag for file type
c                                 0: UNFORMATTED (not supported!)
c                                 1: FORMATTED
c VERSPARB(nspdat) - char*16  - Version of the input CALPUFF
c                               file(s) containing road sources
c                               with arbitrarily-varying location and
c                               emissions
c                               (SPEMARB.DAT)
c   IBSRC8(nspdat) - integer  - Index for first source in a SPEMARB.DAT
c                               file
c   IESRC8(nspdat) - integer  - Index for last source in a SPEMARB.DAT
c                               file
c  IBDATHR8(nspdat)- integer  - Date/hour at beginning of period for
c                               the first data record in the file
c                               (YYYYJJJHH, where YYYY=year,
c                               JJJ=Julian day, HH=hour [00-23 LST])
c   IBSEC8(nspdat) - integer  - Seconds of the first data record in the
c                               file  (0000-3599)
c  IEDATHR8(nspdat)- integer  - Date/hour at end of period for
c                               the last data record in the file
c                               (YYYYJJJHH, where YYYY=year,
c                               JJJ=Julian day, HH=hour [00-23 LST])
c   IESEC8(nspdat) - integer  - Seconds of the last data record in the
c                               file  (0000-3599)
c     XTZ8(nspdat) - real     - Time zone (UTC=LST+XTZ7)
c   T2BTZ8(nspdat) - real     - Hours to ADD to Local Time to obtain
c                               Base Time (xtz7-xbtz)
c
c --- MAP Projection
c   PMAPSP2(nspdat) -char*8    - Character code for map projection
c                                 UTM :  Universal Transverse Mercator
c                                 LCC :  Lambert Conformal Conic
c                                 PS  :  Polar Stereographic
c                                 EM  :  Equatorial Mercator
c                                 LAZA:  Lambert Azimuthal Equal Area
c                                 TTM :  Tangential Transverse Mercator
c UTMHEMSP2(nspdat) -char*4    - Base hemisphere for UTM projection
c                                (S=southern, N=northern)
c  DATUMSP2(nspdat) -char*8    - Datum-Region for grid coordinates
c  DATENSP2(nspdat) -char*12   - NIMA date for datum parameters
c                                 (MM-DD-YYYY  )
c XYUNITSP2(nspdat) -char*4    - Units for coordinates (e.g., KM)
c
c  IUTMZNSP2(nspdat) -integer  - UTM zone for UTM projection
c  FEASTSP2(nspdat)  -real     - False Easting (km) at projection origin
c  FNORTHSP2(nspdat) -real     - False Northing (km) at projection origin
c  RNLAT0SP2(nspdat) -real     - N. latitude & E. longitude of x=0 and y=0
c  RELON0SP2(nspdat)  (deg)      of map projection (Used only if PMAP =
c                                LCC, PS, EM, TTM or LAZA) 
c                                NOTE: longitude neg in western hemisphere
c  RNLAT1SP2(nspdat) - real    - Matching N. latitude(s) for projection
c  RNLAT2SP2(nspdat)  (deg)      (Used only if PMAP3= LCC, PS, or EM)
c                            LCC :  Projection cone slices through
c                                   Earth's surface at XLAT1 and XLAT2
c                            PS  :  Projection plane slices through
c                                   Earth at XLAT1
c                            EM  :  Projection cylinder slices through
c                                   Earth at [+/-] XLAT1
c
c --- Variables for spray line-segments that emit puffs/slugs
c --- (other properties are taken from the (nsp2) arrays)
c                  NSPSEG2 - integer - Number of emitting spray line segments
c                                      (Total over all spray lines)
c         ISPRAY2(nspseg2) - integer - Spray line number for this segment
c          SPLEN2(nspseg2) - real    - Spray line length (m) for this segment
c       XSP2GRD(2,nspseg2) - real    - X coordinate of the ends of spray line
c                                      segments in grid units
c                                      (i.e., origin at (0.0,0.0))
c       YSP2GRD(2,nspseg2) - real    - Y coordinate of the ends of spray line
c                                      segments in grid units  
c                                      (i.e., origin at (0.0,0.0))
c         ELSP2(2,nspseg2) - real    - Ground elevation of the ends of
c                                      spray line segments (m MSL)
c
c ---  Variable data  ---
c
c    NSTEP8(nspdat)  - integer  - Number of emission steps in
c                                 current timestep for each file
c NDHRQB8(mxqstep,nspdat) & NSECQB8(mxqstep,nspdat)
c                    - integer  - Starting time for which
c                                 emissions data in current set of
c                                 records is valid
c                                 (YYYYJJJHH & SSSS)
c NDHRQE8(mxqstep,nspdat) & NSECQE8(mxqstep,nspdat)
c                    - integer  - Ending time for which
c                                 emissions data in current set of
c                                 records is valid
c                                 (YYYYJJJHH & SSSS)
c    HTSP2(mxqstep,nsp2) - real - Effective height (mAGL)
c   SY0SP2(mxqstep,nsp2) - real - Initial sigma y (m)
c   SZ0SP2(mxqstep,nsp2) - real - Initial sigma z (m)
c   DIASP2(mxqstep,nsp2) - real - Initial droplet diameter (m)
c  DCUTSP2(mxqstep,nsp2) - real - Cut off diameter for evaporation (m)
c  QSP2(mxspec,mxqstep,nsp2)
c                        - real - Emission rate (g/m/s) for each
c                                 pollutant
c    NEWSP2(nspseg2) - integer  - Number of puffs/slugs released
c                                 by each road during the current
c                                 step
c
c-----------------------------------------------------------------------
      end module mspray2
c-----------------------------------------------------------------------


c-----------------------------------------------------------------------
      module mqscale
c-----------------------------------------------------------------------
c --- CALPUFF    Version: 7.3.0    Level: 141201          |MQSCALE|
c ---            Emission-Rate Scaling Factors (control file sources)
c-----------------------------------------------------------------------
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

c --- Assignments:
c -----------------
      data nqsfval/1,   12,   7,
     &             24, 168, 288,
     &             6,   36,  12/
      data nqsfcol/1,   12,   7,
     &             24,  24,  24,
     &             6,    6,  12/
      data nqsfrow/1,    1,   1,
     &             1,    7,  12,
     &             1,    6,   1/
      data mapivary/1, 4, 2, 6, 8, 9/
      data cqsftype/                'CONSTANT1               ',
     &   'MONTH12                 ','DAY7                    ',
     &   'HOUR24                  ','HOUR24_DAY7             ',
     &   'HOUR24_MONTH12          ','WSP6                    ',
     &   'WSP6_PGCLASS6           ','TEMPERATURE12           '/
c --- NOTE ---------
c           CONSTANT1        1   scaling factor
c           MONTH12          12  scaling factors: months 1-12
c           DAY7             7   scaling factors: days 1-7
c                              [SUNDAY,MONDAY, ... FRIDAY,SATURDAY]
c           HOUR24           24  scaling factors: hours 1-24
c           HOUR24_DAY7      168 scaling factors: hours 1-24,
c                              repeated  7 times:
c                              [SUNDAY,MONDAY, ... FRIDAY,SATURDAY]
c           HOUR24_MONTH12   288 scaling factors: hours 1-24,
c                              repeated 12 times: months 1-12
c           WSP6             6   scaling factors: wind speed classes 1-6
c                              [speed classes (WSCAT)]
c           WSP6_PGCLASS6    36  scaling factors: wind speed classes 1-6
c                              repeated  6 times: PG classes A,B,C,D,E,F
c                              [speed classes (WSCAT)]
c           TEMPERATURE12    12  scaling factors: temp(K) classes 1-12
c                              [temperature classes (TKCAT)]
c -----------------
c
c --- Variables:
c ---------------
c
c --- Variables for defining emission-rate scaling factors
c                 NQSFTAB - integer - Number of tables of
c                                     emissions scaling factors
c       IQSFTYPE(nqsftab) - integer - Index of scale-factor type of
c                                     each table
c       CQSFNAME(nqsftab) - char*40 - Name of each scale-factor table
c   QSFTAB(mxqsf,nqsftab) - real    - Emission scale-factors
c                NQSFTYPE - integer - Number of types of
c                                     emissions scaling factors
c      CQSFTYPE(nqsftype) - char*24 - Name of each scale-factor type
c             MAPIVARY(6) - integer - Map pointer from the 6 IVARY 
c                                     choices to the corresponding
c                                     CQSFTYPE() index
c       NQSFVAL(nqsftype) - integer - Number of scaling factors for
c                                     each type
c                                     (Max must = MXQSF in /params/)
c       NQSFCOL(nqsftype) - integer - Number of print columns for each
c       NQSFROW(nqsftype) - integer - Number of print rows for each
c
c --- Temperature and wind speed classes by source type (13)
c              WQSF(5,13) - real    - Wind speed class boundaries (m/s)
c                                     (boundary is upper limit of class)
c             TQSF(11,13) - real    - Temperature class boundaries (K)
c                                     (boundary is upper limit of class)
c     Source Types are:
c            1 = Point         Constant Emissions
c            2 = Point         Variable Emissions (no WS/T class used)
c            3 = Poly. Area    Constant Emissions
c            4 = Poly. Area    Variable Emissions (no WS/T class used)
c            5 = Line          Constant Emissions
c            6 = Line          Variable Emissions (no WS/T class used)
c            7 = Volume        Constant Emissions
c            8 = Grid Volume   Variable Emissions (no WS/T class used)
c            9 = Boundary Condition
c          (10)= Flare         Constant Emissions
c           11 = Flare         Variable Emissions (no WS/T class used)
c           12 = Road          Constant Emissions
c           13 = Road          Variable Emissions (no WS/T class used)
c
c-----------------------------------------------------------------------
      end module mqscale
c-----------------------------------------------------------------------
