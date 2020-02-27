subroutine wrfiles(npt2,nar2,nln2,nvl2,nfl2,&
	idryflg,mhill)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 150918           WRFILES
	!                J. Scire
	!
	! --- PURPOSE:  Write a table of the input and output file names
	!               for the current run
	!
	! --- UPDATE
	!
	! --- V7.2.1-v7.3.0 150918       : add ROAD and SPRAY source types
	! --- V6.302-TNG-7.0.0 140913    : add FLARE source
	! --- V6.302-V6.42_x1.1 140521   : revise format for AUX filename list
	!
	! --- V6.301-V6.302 100917  (DGS): add NH3Z.DAT file of vertical NH3
	!                                  concentrations for each month
	!                                  (mchem=6,7)
	!                   100917  (DGS): add liquid water content file
	!                                  extension
	! --- V6.3-V6.301   100827  (DGS): NMETDAT converted to array
	! --- V6.267-V6.3   100212  (DGS): add nested CALMET grid
	! --- V6.26-V6.267  090710  (DGS): add PFTRAK.DAT for puff tracking
	!                                  output
	! --- V6.11-V6.26   080430  (DGS): add RISE.DAT: numerical rise output
	! --- V5.725-V6.11  060309  (DGS): change filenames from c*70 to c*132
	! --- V5.4-V5.725   050128  (DGS): add TK2D.DAT for 2D temperature  
	!                   050128  (DGS): add RHO2D.DAT for 2D density
	! --- V5.4-V5.74    040715  (DGS): add METFM=5 (AERMET)
	! --- V5.4-V5.4     000602_3(DGS): add H2O2.DAT for aqueous chemistry
	! --- V5.3-V5.4     000602  (DGS): multiple PTEMARB, BAEMARB, and
	!                                  VOLEMARB files
	!                   000602  (DGS): include FOG.DAT file
	! --- V5.3-V5.3     991222a (DGS): use of PROFILE.DAT for turbulence
	!                                  when PLMMET.DAT option is used
	! --- V5.2-V5.3     991222  (DGS): add BCON.DAT file
	! --- V5.0-V5.1     990625b (DGS): drop mmodel='AUSPUFF' distinction
	! --- V5.0-V5.0     990228d (DGS): add mass balance file
	! --- V5.0-V5.0     990228c (DGS): add mass flux files
	! --- V5.0-V5.0     990228a (DGS): add multiple CALMET.DAT files
	! --- V5.0-V5.0     980918  (DGS): include MCHEM=4
	! --- V5.0-V5.0     980515  (DGS): add COASTLN.DAT file
	! --- V5.0-V5.0     980304  (DGS): add RESTART file
	! --- V4.0-V5.0     971107  (DGS): add LNEMARB.DAT file
	!
	! --- INPUTS:
	!               NPT2 - integer - Number of point sources with variable
	!                                emissions
	!                                (PTEMARB.DAT - arbitrary emissions)
	!               NAR2 - integer - Number of buoyant area sources with
	!                                variable location and emissions
	!                                (BAEMARB.DAT - arbitrary emissions)
	!               NLN2 - integer - Number of buoyant line sources with
	!                                variable location and emissions
	!                                (LNEMARB.DAT - arbitrary emissions)
	!               NVL2 - integer - Number of volume sources with
	!                                variable location and emissions
	!                                (VOLEMARB.DAT - arbitrary emissions)
	!               NFL2 - integer - Number of FLARE sources with 
	!                                variable location and emissions
	!                                (FLEMARB.DAT - arbitrary emissions)
	!     IDRYFLG(mxspec) - integer - Array of dry deposition flags for
	!                       array     each pollutant
	!                                   0 = No deposition
	!                                   1 = Resistance model - gas
	!                                   2 = Resistance model - particle
	!                                   3 = User-specified dep. velocities
	!               MHILL - integer - Flag controlling use of CTDM-format
	!                                 hill information files (for CTSG)
	!                                   0 = No file (CTSG option not used)
	!                                   1 = HILL.DAT & HILLRCT.DAT files
	!                                   2 = No files (hill data from OPTHILL
	!                                       are supplied in Subgroup 6b,
	!                                       and CTSG receptors are
	!                                       supplied in Subgroup 6c)
	!
	!     Common block /FILNAM/ variables:
	!         PUFINP, METDATL, METDOML, ISCDAT, PLMDAT,
	!         PTDAT, ARDAT, VOLDAT, FLDAT, LNDAT,
	!         VDDAT, OZDAT, CHEMDAT, HILDAT, RCTDAT, DEBUG, PUFLST,
	!         CONDAT, DFDAT, WFDAT, VISDAT, T2DDAT, RHODAT, PRFDAT, SFCDAT,
	!         RSTARTB, RSTARTE, CSTDAT, BDYDAT, FLXDAT, BALDAT, BCNDAT,
	!         H2O2DAT, RISDAT, TRKDAT, NH3ZDAT, AUXEXT, NMETDAT, NMETDOM,
	!         NPTDAT, NARDAT, NVOLDAT, NFLDAT
	!     Common block /CHEMDAT/ variables:
	!           MOZ, MH2O2, MNH3
	!     Common block /FLAGS/ variables:
	!           MCHEM, MDRY, MDISP, MCTSG, MTURBVW, MSGTIBL, MBCON,
	!           MAQCHEM, MLWC
	!     Common block /GEN/ variables:
	!           METFM, NSPEC, MRESTART
	!     Common block /OUTPT/ variables:
	!           ICON, IVIS, IT2D, IRHO, IDRY, IWET, IMFLX, IMBAL, IFOG,
	!           INRISE, IPFTRAK, LDEBUG
	! ---    Module |MROAD2| variables:
	!           nrddat,rddat
	! ---    Module |MRSPRAY2| variables:
	!           nspdat,spdat
	!
	!     Parameters: IO6, IO7, IO8, IO9, IO10, IO11, IO12, IO13, IO14, IO15,
	!                 IO19, IO20, IO22, IO23, IO24, IO25, IO28, IO29, IO30,
	!                 IO31, IO32, IO35, IO36, IO37, IO38, IO40, MXSPEC,
	!                 IOPT2, IOAR2, IOVOL, IOFL2, IOTRK, MXMETDAT, MXMETDOM
	!
	! --- OUTPUT:  none
	!
	! --- WRFILES called by:  SETUP
	! --- WRFILES calls:      none
	!----------------------------------------------------------------------
	!
	! --- Modules
	use mroad2
	use mspray2
	! --- Include parameter statements
	include 'params.puf'
	!
	! --- Include common blocks
	include 'filnam.puf'
	include 'chemdat.puf'
	include 'flags.puf'
	include 'gen.puf'
	include 'outpt.puf'
	!
	integer idryflg(mxspec)
	!
	! --------------------------------------------------
	! --- Write the list of INPUT files used in this run
	! --------------------------------------------------
	write(io6,10)
	10    format(//1x,13('----------')/10x,'INPUT FILES'//&
	1x,'Default Name',5x,'Unit No.',5x,'File Name and Path'/&
	1x,'------------',5x,'--------',5x,'------------------')
	!
	! --- CALPUFF.INP
	write(io6,12)'CALPUFF.INP',io5,pufinp
	12    format(1x,a12,7x,i3,8x,a132)
	!
	! --- Restart file for continuation run
		if(mrestart.EQ.1 .OR. mrestart.EQ.3) &
	write(io6,12)'RESTARTB.DAT',io3,rstartb
	!
	! --- Meteorological data file(s)
	if(metfm.EQ.1) then
		! ---    CALMET meteorological data file   (CALMET.DAT)
		do k=1,nmetdom
			iometgrd=io7+(k-1)
			write(io6,*)'(CALMET Domain:',k,')  '//TRIM(metdoml(k))
			do i=1,nmetdat(k)
				if(k.EQ.1 .AND. i.EQ.1) then
					write(io6,12)'CALMET.DAT',iometgrd,metdatl(1,k)
				else
					write(io6,12)'  (----)  ',iometgrd,metdatl(i,k)
				endif
			enddo
		enddo
		! ---    CALMET 3D gridded liquid water content files
		if(maqchem.EQ.1 .AND. mlwc.EQ.1) then
			! ---       CALMET 3D water content data file   (CALMET.DAT.AUX)
			do k=1,nmetdom
				iometgrd=io7+nmetdom+(k-1)
				write(io6,*)'(CALMET Domain:',k,')  '//TRIM(metdoml(k))
				do i=1,nmetdat(k)
					! --- 6.42_x1.1 140521
					write(io6,'(1x,a12,7x,i3,8x,a)')'  (.AUX)  ',&
					iometgrd,TRIM(metdatl(i,k))//TRIM(auxext)
				enddo
			enddo
		endif
	elseif(metfm.EQ.2) then
		! ---    ISC meteorological data file      (ISCMET.DAT)
		write(io6,12)'ISCMET.DAT',io7,iscdat
	elseif(metfm.EQ.3) then
		! ---    PLUME meteorological data file    (PLMMET.DAT)
		write(io6,12)'PLMMET.DAT',io7,plmdat
	elseif(metfm.EQ.4 .OR. metfm.EQ.5) then
		! ---    Tower data file              (PROFILE.DAT)
		write(io6,12)'PROFILE.DAT',io31,prfdat
		! ---    Surface parameter file       (SURFACE.DAT)
		write(io6,12)'SURFACE.DAT',io32,sfcdat
	endif
	!
	! --- BOUNDARY CONCENTRATION file (BCON.DAT)
	! --- (all information about both constant and variable boundary data)
	if(mbcon.gt.0)write(io6,12)'BCON.DAT',io15,bcndat
	!
	! --- POINT SOURCE emissions file #2 (PTEMARB.DAT)
	! --- (stationary point sources with arbitrary variation in emissions)
	if(npt2.gt.0) then
		do i=1,nptdat
			io=iopt2+i-1
			write(io6,12)'PTEMARB.DAT',io,ptdat(i)
		enddo
	endif
	!
	! --- BUOYANT AREA SOURCE file  (BAEMARB.DAT)
	! --- (area sources with arbitrary variation in location & emissions)
	if(nar2.gt.0) then
		do i=1,nardat
			io=ioar2+i-1
			write(io6,12)'BAEMARB.DAT',io,ardat(i)
		enddo
	endif
	!
	! --- BUOYANT LINE SOURCE file  (LNEMARB.DAT)
	! --- (line sources with arbitrary variation in location & emissions)
	if(nln2.gt.0)write(io6,12)'LNEMARB.DAT',io19,lndat
	!
	! --- VOLUME SOURCE emissions file (VOLEMARB.DAT)
	! --- (volume sources with arbitrary variation in location & emissions)
	if(NVL2.gt.0) then
		do iv=1,nvoldat
			io=iovol+iv-1
			write(io6,12)'VOLEMARB.DAT',io,voldat(iv)
		enddo
	endif
	!
	! --- FLARE SOURCE emissions file (FLEMARB.DAT)
	! --- (FLARE sources with arbitrary variation in location & emissions)
	if(NFL2.GT.0) then
		do i=1,nfldat
			io=iofl2+i-1
			write(io6,12)'FLEMARB.DAT',io,fldat(i)
		enddo
	endif
	! --- ROAD SOURCE emissions file (RDEMARB.DAT)
	! --- (ROAD sources with arbitrary variation in location & emissions)
	if(NRD2.GT.0) then
		do i=1,nrddat
			io=iord2+i-1
			write(io6,12)'RDEMARB.DAT',io,rddat(i)
		enddo
	endif
	! --- SPRAY SOURCE emissions file (SPEMARB.DAT)
	! --- (SPRAY sources with arbitrary variation in location & emissions)
	if(NSP2.GT.0) then
		do i=1,nspdat
			io=iosp2+i-1
			write(io6,12)'SPEMARB.DAT',io,spdat(i)
		enddo
	endif
	!
	! --- DEPOSITION VELOCITY file (VD.DAT) (if user-specified
	!     deposition velocities are used for any species AND computation
	!     of dry deposition is requested
	if(mdry.eq.1)then
		do 20 i=1,nspec
			if(idryflg(i).eq.3)then
				write(io6,12)'VD.DAT',io20,vddat
				exit ! add by @creaqi break the loop goto 22 in wrfiles with len equal 1
			endif
			20       continue
			22       continue
		endif
		!
		! --- OZONE data file (OZONE.DAT) (if MESOPUFF II chemical
		!     scheme is used AND hourly ozone input requested)
			if((mchem.NE.0 .AND. mchem.NE.2 .AND. mchem.NE.5) .AND. moz.EQ.1)&
		write(io6,12)'OZONE.DAT',io22,ozdat
		!
		! --- H2O2 data file (H2O2.DAT) (if aqueous phase chemical
		!     scheme is used AND hourly H2O2 input requested)
			if(maqchem.EQ.1 .AND. mh2o2.EQ.1)&
		write(io6,12)'H2O2.DAT',io23,h2o2dat
		!
		! --- CHEMICAL TRANSFORMATION file (CHEM.DAT) (if user-specified
		!     chemical transformation rates are used)
		if(mchem.eq.2)write(io6,12)'CHEM.DAT',io24,chemdat
		!
		! --- Vertical NH3 concentration profiles (for MCHEM=6,7)
			if(mnh3.EQ.1 .AND. (mchem.EQ.6 .OR. mchem.EQ.7))&
		write(io6,12)'NH3Z.DAT',io40,nh3zdat
		!
		! --- TURBULENCE data file
		if(mdisp.eq.1 .OR. mdisp.EQ.5) then
			if(metfm.LT.4 .AND. mturbvw.LT.4) then
				write(io6,12)'PROFILE.DAT',io31,prfdat
			endif
		endif
		!
		! --- CTSG hill information files (HILL.DAT, HILLRCT.DAT)
		!     (if CTDM processors are used to create them)
		if(mctsg.eq.1.and.mhill.eq.1) then
			write(io6,12)'HILL.DAT',io28,hildat
			write(io6,12)'HILLRCT.DAT',io29,rctdat
		endif
		!
		! --- Sub-Grid TIBL COAST LINE file (COASTLN.DAT) (if 
		!     MSGTIBL option is selected)
		if(msgtibl.eq.1)write(io6,12)'COASTLN.DAT',io25,cstdat
		!
		! --- Mass flux BOUNDARY file (FLUXBDY.DAT)
		if(imflx.eq.1)write(io6,12)'FLUXBDY.DAT',io35,bdydat
		!
		! ---------------------------------------------------
		! --- Write the list of OUTPUT files used in this run
		! ---------------------------------------------------
		write(io6,30)
		30    format(//1x,13('----------')/10x,'OUTPUT FILES'//&
		1x,'Default Name',5x,'Unit No.',5x,'File Name and Path'/&
		1x,'------------',5x,'--------',5x,'------------------')
		!
		! --- CALPUFF.LST
		write(io6,12)'CALPUFF.LST',io6,puflst
		!
		! --- Output concentration file (CONC.DAT)
		if(ICON.eq.1)write(io6,12)'CONC.DAT',io8,condat
		!
		! --- Output dry flux file (DFLX.DAT)
		if(IDRY.eq.1)write(io6,12)'DFLX.DAT',io9,dfdat
		!
		! --- Output wet flux file (WFLX.DAT)
		if(IWET.eq.1)write(io6,12)'WFLX.DAT',io10,wfdat
		!
		! --- Visibility-related file (VISB.DAT)
		if(IVIS.eq.1)write(io6,12)'VISB.DAT',io11,visdat
		!
		! --- 2D Temperature file (TK2D.DAT)
		if(IT2D.eq.1)write(io6,12)'TK2D.DAT',io13,t2ddat
		!
		! --- 2D Density file (RHO2D.DAT)
		if(IRHO.eq.1)write(io6,12)'RHO2D.DAT',io14,rhodat
		!
		! --- Mass flux file (MASSFLX.DAT)
		if(imflx.eq.1)write(io6,12)'MASSFLX.DAT',io36,flxdat
		!
		! --- Mass balance file (MASSBAL.DAT)
		if(imbal.eq.1)write(io6,12)'MASSBAL.DAT',io37,baldat
		!
		! --- Fog plume data file
		if(IFOG.eq.1)write(io6,12)'FOG.DAT',io12,fogdat
		!
		! --- Numerical rise data file
		if(INRISE.eq.1)write(io6,12)'RIS.DAT',io38,risdat
		!
		! --- Puff-tracking output file
		if(IPFTRAK.GT.0)write(io6,12)'PFTRAK.DAT',iotrk,trkdat
		!
		! --- Restart file
		if(mrestart.GE.2) write(io6,12)'RESTARTE.DAT',io4,rstarte
		!
		! --- Puff/Slug TRACKING file (DEBUG.DAT) (if in DEBUG mode)
		if(LDEBUG)write(io6,12)'DEBUG.DAT',io30,debug
		!
		return
end
	!----------------------------------------------------------------------
subroutine stktip(hs,diam,fluxm,vexit,wsstk,hdw)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 991222b           STKTIP
	! ---            J. Scire, SRC
	!
	! --- PURPOSE:  Calculate the plume height adjustment to account for
	!               stack-tip downwash effects
	!
	! --- UPDATE
	! --- V5.3-V5.3     991222b (DGS): add momentum flux to argument list
	!                                  and test for zero (exit velocity may
	!                                  not be vertical)
	!
	! --- INPUTS:
	!              HS - real         - Stack height (m)
	!            DIAM - real         - Stack diameter (m)
	!           FLUXM - real         - Vertical momentum flux (m**4/s**2)
	!           VEXIT - real         - Stack gas exit velocity (m/s)
	!           WSSTK - real         - Stack height wind speed (m/s)
	!
	! --- OUTPUT:
	!             HDW - real         - Plume height DECREASE (m)
	!
	! --- STKTIP called by:  POINTS1, POINTS2, PUFRECS, SLGRECS. PLGRECS
	! --- STKTIP calls:      none
	!----------------------------------------------------------------------
	!
	! --- Compute adjustment in effective stack height due to stack-tip
	!     downwash effects.  Note that the adjustment cannot be greater
	!     than the physical stack height!  The adjustment is POSITIVE, and
	!     is SUBTRACTED from, not added to, the stack height.
	if(fluxm.EQ.0.0) then
		hdw=AMIN1(hs,(3.*diam))
	elseif(vexit.LT.(1.5*wsstk)) then
		vbyws=vexit/wsstk
		hdw=AMIN1(hs,(2.*diam*(1.5-vbyws)))
	else
		hdw=0.0
	endif
	return
end
!----------------------------------------------------------------------
subroutine prb(fb,wsstk,x,xfinb,zfinb,zb)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 941215               PRB
	! ---            J. Scire, SRC
	!
	! --- PURPOSE:  Calculate transitional buoyant plume rise
	!               (excluding building downwash effects).
	!               This routine is used for both neutral/unstable and
	!               stable conditions.
	!
	! --- INPUTS:
	!              FB - real         - Buoyancy flux (m**4/s**3)
	!           WSSTK - real         - Stack height wind speed (m/s)
	!               X - real         - Downwind distance (m) from the
	!                                  stack to the receptor
	!           XFINB - real         - Distance (m) to final buoyant
	!                                  plume rise for neutral/unstable
	!                                  conditions
	!           ZFINB - real         - Final buoyant plume rise (m)
	!                                  (during stable conditions, taken as
	!                                  the min. of vertical and bent-over
	!                                  plume equations)
	!
	! --- OUTPUT:
	!              ZB - real         - Transitional plume rise (m) due
	!                                  to plume buoyancy
	!
	! --- PRB called by:  GRISE
	! --- PRB calls:      none
	!----------------------------------------------------------------------
	!
	! --- Check for case of no buoyant rise
	if(x.le.0.0.or.fb.le.0.0)then
		zb=0.0
	else
		! ---    Compute transitional buoyant plume rise
		if(x.lt.xfinb .AND. wsstk.gt.0.0)then
			zb=(1.6*(fb*x*x)**0.333333)/wsstk
			!
			! ---       Final plume height in stable conditions is based on minimum
			! ---       of bent-over and vertical plume equations -- ensure plume
			! ---       height does not exceed final height
			zb=amin1(zb,zfinb)
		else
			zb=zfinb
		endif
	endif
	return
end
!----------------------------------------------------------------------
subroutine prm(diam,vexit,fm,wsstk,istab,sqrts,x,xfinm,zfinm,zm)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 950715               PRM
	! ---            J. Scire, SRC
	!
	! --- PURPOSE:  Calculate transitional momentum plume rise
	!               (excluding building downwash effects).
	!               This routine is used for both neutral/unstable and
	!               stable conditions.
	!
	! --- INPUTS:
	!            DIAM - real         - Stack diameter (m)
	!           VEXIT - real         - Stack gas exit velocity (m/s)
	!              FM - real         - Momentum flux (m**4/s**2)
	!           WSSTK - real         - Stack height wind speed (m/s)
	!           ISTAB - integer      - Stability class (1=A,...6=F)
	!           SQRTS - real         - Square root of stability parameter,s
	!                                  (sqrt(s)=(g*(dtheta/dz)/tair)**0.5)
	!               X - real         - Downwind distance (m) from the
	!                                  stack to the receptor
	!           XFINM - real         - Distance (m) to final momentum
	!                                  plume rise
	!           ZFINM - real         - Final momentum plume rise (m)
	!                                  (during stable conditions, taken as
	!                                  the min. of vertical and bent-over
	!                                  plume equations)
	!
	! --- OUTPUT:
	!              ZM - real         - Transitional plume rise (m) due
	!                                  to momentum
	!
	! --- PRM called by:  GRISE, POINTS1
	! --- PRM calls:      none
	!----------------------------------------------------------------------
	!
	! --- Check for case of no momentum rise
	if(vexit.le.0.0.or.fm.le.0.0.or.x.le.0.0)then
		zm=0.0
	else
		! ---    Compute transitional momentum plume rise
		if(x.lt.xfinm .AND. wsstk.gt.0.0)then
			!
			! ---       Compute the momentum entrainment coefficient
			betaj=0.333333+(wsstk/vexit)
			! ---       Compute neutral/unstable final rise
			zmfnu=3.*diam*vexit/wsstk
			!
			if(istab.le.4)then
				!
				! ---          Neutral/unstable conditions
				zm=(3.*fm*x/(betaj*betaj*wsstk*wsstk))**0.333333
				! ---          Test zm against neutral/unstable final rise (as in ISC2)
				zm=amin1(zm,zmfnu)
			else
				!
				! ---          Stable conditions
				zm=(3.*fm*sin(sqrts*x/wsstk)/(betaj*betaj*wsstk*&
				sqrts))**0.333333
				!
				! ---          Should make sure that zm is no greater than zfinm,
				! ---          which is the minimum of final momentum rise for either
				! ---          stable or non-stable conditions
				!
				!              zm=amin1(zm,zfinm)
				!
				! ---          BUT, to be consistent with ISC2, only test zm against
				! ---          neutral/unstable final rise (3.*diam*vexit/wsstk)
				zm=amin1(zm,zmfnu)
			endif
		else
			!
			! ---       Beyond distance to final rise
			zm=zfinm
		endif
	endif
	return
end
!----------------------------------------------------------------------
subroutine prfin(vexit,diam,fm,fb,wsstk,istab,sqrts,&
	xfinm,xfinb,xfin,zfinm,zfinb,zfin)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 991222b            PRFIN
	! ---            J. Scire, SRC
	!
	! --- PURPOSE:  Calculate distance to final rise and height of final
	!               plume rise
	!
	! --- UPDATE
	! --- V5.3-V5.3     991222b (DGS): Account for zero momentum flux that
	!                                  is caused by FMFAC=0
	!
	! --- INPUTS:
	!           VEXIT - real         - Stack gas exit velocity (m/s)
	!            DIAM - real         - Stack diameter (m)
	!              FM - real         - Momentum flux (m**4/s**2)
	!              FB - real         - Buoyancy flux (m**4/s**3)
	!           WSSTK - real         - Stack height wind speed (m/s)
	!           ISTAB - integer      - Stability class (1=A,...6=F)
	!           SQRTS - real         - Square root of stability parameter,s
	!                                  (sqrt(s)=(g*(dtheta/dz)/tair)**0.5)
	!
	! --- OUTPUTS:
	!           XFINM - real         - Distance (m) to final momentum
	!                                  plume rise
	!           XFINB - real         - Distance (m) to final buoyant
	!                                  plume rise
	!            XFIN - real         - Larger of XFINM and XFINB
	!           ZFINM - real         - Final momentum plume rise (m)
	!           ZFINB - real         - Final buoyant plume rise (m)
	!            ZFIN - real         - Larger of ZFINM and ZFINB
	!
	! --- PRFIN called by:  POINTS1, POINTS2
	! --- PRFIN calls:      none
	!----------------------------------------------------------------------
	!
	! --- Set minimum allowed wind speed at stack-top (wsstk0).  If wind
	! --- speed is less than wsstk0, set distance to final rise equal to
	! --- zero, and use the "calm rise" formula for stable conditions, or
	! --- the neutral/unstable rise formulas with wsstk=wsstk0.
	data wsstk0/1.0/
	!
	! ------------------------------------------
	! --- Compute distance, height of final rise
	! ------------------------------------------
	!
	if(wsstk.GE.wsstk0) then
		! ---    "non-CALM" conditions ------------------------
		!
		! ---    Buoyant plume rise
		if(istab.le.4)then
			!
			! ---       Neutral/unstable conditions
			if(fb.ge.55.)then
				xfinb=119.*fb**0.4
				zfinb=38.71*fb**0.6/wsstk
			else if(fb.gt.0.0)then
				xfinb=49.*fb**0.625
				zfinb=21.425*fb**0.75/wsstk
			else
				!
				! ---          XFINB with FB le 0.0 follows ISC2 convention
				vexit2=amax1(vexit,1.0e-10)
				xfinb=4.*diam*(vexit2+3.*wsstk)**2/(vexit2*wsstk)
				fb2=amax1(fb,1.0e-10)
				zfinb=21.425*fb2**0.75/wsstk
			endif
		else
			!
			! ---       Stable conditions
			xfinb=2.0715*wsstk/sqrts
			s=sqrts*sqrts
			fb2=amax1(fb,1.0e-10)
			!
			! ---       Bent-over plume equation
			zfinb=2.6*(fb2/(wsstk*s))**0.333333
			!
			! ---       Vertical plume equation
			zfinv=4.*fb2**0.25/s**0.375
			!
			! ---       Final stable rise is lower of bent-over and vertical plume
			! ---       values
			zfinb=amin1(zfinb,zfinv)
		endif
		!
		! ---    Momentum plume rise
		if(istab.le.4)then
			!
			! ---       Neutral/unstable conditions
			vexit2=amax1(vexit,1.0e-10)
			xfinm=4.*diam*(vexit2+3.*wsstk)**2/(vexit2*wsstk)
			zfinm=3.*diam*vexit/wsstk
		else
			!
			! ---       Stable conditions
			! ---       (Constant 1.57080 = pi/2.)
			xfinm=1.57080*wsstk/sqrts
			fm2=amax1(fm,1.0e-10)
			zfinm=1.5*(fm2/(wsstk*sqrts))**0.333333
			!
			! ---       Maximum stable momentum rise is restricted to be less than
			! ---       final neutral/unstable momentum rise
			zfinm=amin1(zfinm,3.*diam*vexit/wsstk)
		endif
		! ---    Set xfinm,zfinm to zero explicitly when FM is zero
		if(fm.EQ.0.0) then
			xfinm=0.0
			zfinm=0.0
		endif
		!
	else
		! ---    "CALM" conditions ----------------------------
		!
		! ---    Buoyant plume rise
		if(istab.le.4)then
			!
			! ---       Neutral/unstable conditions
			xfinb=0.0
			if(fb.ge.55.)then
				zfinb=38.71*fb**0.6/wsstk0
			else
				fb2=amax1(fb,1.0e-10)
				zfinb=21.425*fb2**0.75/wsstk0
			endif
		else
			!
			! ---       Stable conditions
			xfinb=0.0
			s=sqrts*sqrts
			fb2=amax1(fb,1.0e-10)
			!
			! ---       Vertical plume equation
			zfinb=4.*fb2**0.25/s**0.375
		endif
		!
		! ---    Momentum plume rise
		if(istab.le.4)then
			!
			! ---       Neutral/unstable conditions
			vexit2=amax1(vexit,1.0e-10)
			xfinm=0.0
			zfinm=3.*diam*vexit/wsstk0
		else
			!
			! ---       Stable conditions
			xfinm=0.0
			fm2=amax1(fm,1.0e-10)
			zfinm=1.5*(fm2/(wsstk0*sqrts))**0.333333
			!
			! ---       Maximum stable momentum rise is restricted to be less than
			! ---       final neutral/unstable momentum rise
			zfinm=amin1(zfinm,3.*diam*vexit/wsstk0)
		endif
		! ---    Set xfinm,zfinm to zero explicitly when FM is zero
		if(fm.EQ.0.0) then
			xfinm=0.0
			zfinm=0.0
		endif
	endif
	!
	! --- Set overall maximum distance to final plume rise and maximum
	! --- plume rise
	xfin=amax1(xfinb,xfinm)
	zfin=amax1(zfinb,zfinm)
	!
	return
end
!----------------------------------------------------------------------
subroutine grise(x,htgrise,risefac)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 141230             GRISE
	! ---            D. Strimaitis
	!
	! --- PURPOSE:  Calculate height of gradual plume rise at position "x"
	!
	! --- UPDATE
	! --- TNG-7.0.0 - TNG-7.1.0  141230  (DGS)
	!                                : Apply adjustment for final rise that
	!                                  may have been reduced by wind shear
	!                                  and/or partial penetration
	! --- V6.265 - TNG-7.0.0  140913  (DGS)
	!                                : Add Flare source (istype=10,11)
	! --- V6.262-V6.265 090612  (DGS): Set initial method=0 for all sources
	!                                  since it is actively defined for use
	!                                  with point sources and not others
	!                                  (LHEIGHT branch checks it for all
	!                                   sources)
	! --- V6.261-V6.262 080725  (DGS): Refine test for invalid negative
	!                                  rise factor that halted a valid run
	!                                  instead of returning a zero factor
	!                   080725  (DGS): Change rise method from 2 (SS
	!                                  downwash) to 3 (tabulated rise) for
	!                                  point sources not subject to downwash
	!                                  that are modeled with numerical plume
	!                                  rise.
	! --- V6.26-V6.261  080520  (DGS): Replace individual rise tables with
	!                                  /SRCTAB/ arrays from Direct Access
	!                                  file
	!                   080520  (DGS): Add call to PRSS to treat SS downwash
	! --- V6.23-V6.26   080430  (DGS): Add numerical rise for point sources
	!                                  (not subject to downwash)
	!                                  Use source type alone for 1st-level
	!                                  logic branch, and consolidate 2nd-
	!                                  level branch with a METHOD flag
	! --- V6.22-V6.23   080204  (DGS): Apply stack-tip downwash adjustment
	!                                  here instead of after call
	! --- V6.1-V6.22    070921  (DGS): Check for imet outside valid range
	!                                  of 1 to MXMETSAV+1
	! --- V5.725-V6.1   050915  (DGS): add emission step argument to
	!                                  numerical rise arrays (PT2,AR2)
	! --- V5.7-V5.725   050128  (DGS): fix gradual rise factor for the PRIME
	!                                  module.  The full rise (without 
	!                                  streamline depression in a building
	!                                  wake) was divided by the final rise
	!                                  (with rather than without streamline
	!                                  depression).
	!                   050128  (DGS): add check for valid risefac
	! --- V5.0-V5.7     030402  (DGS): introduce rise tables for point
	!                                  sources with PRIME downwash
	!                   030402  (DGS): use IMET in /CURRENT/ to select
	!                                  appropriate tabulated values
	! --- V5.0-V5.0     980821  (DGS): replace use of IAGE for AREA-source
	!                                  tables since IAGE=1 for all puffs
	! --- V4.0-V5.0     971107  (DGS): add variable line source treatment
	!
	! --- INPUTS:
	!               X - real     - Downwind dist. from  "pt." source (m)
	!
	!     Common block /CURRENT/ variables:
	!           IPNUM, ISNUM, ISTYPE, ZFRISE, XSHIFT, IMET,
	!           IQSTEP
	!     Common block /PUFF/ variables:
	!           DIAM0(mxpuff), EXITW0(mxpuff), HT0(mxpuff), PLEXP0(mxpuff),
	!           WS0(mxpuff), ISTAB0(mxpuff), SQRTS0(mxpuff),
	!           FM(mxpuff), XMFIN(mxpuff), ZMFIN(mxpuff),
	!           FB(mxpuff), XBFIN(mxpuff), ZBFIN(mxpuff),
	!           ZLY0(mxpuff), R0(mxpuff), STIPDW(mxpuff)
	!     Common block /FLAGS/ variables:
	!           MSHEAR, MRISE, MRISE_FL
	!     Common block /SRCTAB/ variables:
	!           NTR,XTR(mxrise),ZTR(mxrise),HTR(mxrise)
	!
	! --- OUTPUTS:
	!         HTGRISE - real         - Plume height (m) (stack+plume rise)
	!         RISEFAC - real         - Corresponding plume rise as a
	!                                  fraction of final rise
	!
	! --- GRISE called by:  PUFRECS, SLGRECS, RECSPEC0, RISEWIND
	! --- GRISE calls:      PRM, PRB, PRBL, PRSHEAR, PRSS, PRTAB, INTERTAB
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	include 'current.puf'
	include 'puff.puf'
	include 'flags.puf'
	include 'srctab.puf'
	! --- Flag determines if height needs to be computed from rise
	logical lheight
	! --- Initialize full rise factor as the default
	risefac=1.0
	! --- Retain IMET for compatibility with previous treatment
	! --- Validate imet index
	if(imet.LT.1 .OR. imet.GT.mxmetsav+1) then
		write(io6,*)
		write(io6,*) 'FATAL ERROR in GRISE: bad met period index'
		write(io6,*) 'Expected IMET = 1 to MXMETSAV+1'
		write(io6,*) 'Found    IMET = ',imet
		write(io6,*) '     MXMETSAV = ',mxmetsav
		write(io6,*)
		write(*,*)
		stop 'Halted in GRISE -- See list file'
	endif
	! --- Set rise calculation METHOD for Point sources
	! ---          0 =  No Rise
	! ---          1 =  Briggs Rise
	! ---          2 =  Briggs rise modified for SS downwash
	! ---          3 =  Tabulated Rise
	method=0
	if(istype.EQ.1 .OR. istype.EQ.2) then
		if(idw0(ipnum).EQ.4) then
			! ---       PRIME downwash active - Cavity plume (no rise)
			method=0
		elseif(idw0(ipnum).EQ.3) then
			! ---       PRIME downwash active - Primary plume (tabulated rise)
			method=3
		elseif(idw0(ipnum).EQ.2) then
			! ---       SS downwash active (modified Briggs rise)
			method=2
		elseif(idw0(ipnum).EQ.1) then
			! ---       HS downwash active (Briggs rise)
			method=1
		elseif(idw0(ipnum).EQ.0) then
			if(mrise.EQ.1) then
				! ---          No downwash active, Briggs rise option
				method=1
			elseif(mrise.EQ.2) then
				! ---          No downwash active, Numerical rise option
				method=3
			else
				write(io6,*)
				write(io6,*) 'FATAL ERROR in GRISE: invalid MRISE'
				write(io6,*) 'Expected MRISE = 1 or 2'
				write(io6,*) 'Found MRISE = ',mrise
				write(io6,*)
				write(*,*)
				stop 'Halted in GRISE -- See list file'
			endif
		else
			write(io6,*)
			write(io6,*) 'FATAL ERROR in GRISE: invalid IDW0'
			write(io6,*) 'Expected IDW0 = 0,1,2,3, or 4'
			write(io6,*) 'Found IDW0 = ',idw0(ipnum)
			write(io6,*)
			write(*,*)
			stop 'Halted in GRISE -- See list file'
		endif
	endif
	! --- Process gradual rise by source type
	if(istype.EQ.1 .OR. istype.EQ.2) then
		! ---    POINT source
		! ---    ------------
		! ---    Control-file (1) or Variable Emissions File (2)
		if(method.EQ.0) then
			! ---       No rise
			htgrise=zfinal(ipnum)
			lheight=.FALSE.
		elseif(method.EQ.1) then
			! ---       Briggs Rise
			! ---       Get gradual momentum rise
			call prm(diam0(ipnum),exitw0(ipnum),fm(ipnum),ws0(ipnum),&
			istab0(ipnum),sqrts0(ipnum),x,xmfin(ipnum),&
			zmfin(ipnum),risem)
			! ---       Get gradual buoyant rise
			call prb(fb(ipnum),ws0(ipnum),x,xbfin(ipnum),zbfin(ipnum),&
			riseb)
			! ---       Compute effects of vertical wind shear
			if(mshear.eq.1)then
				call prshear(fb(ipnum),ws0(ipnum),plexp0(ipnum),&
				ht0(ipnum),x,xbfin(ipnum),zbfin(ipnum),risesh)
				! ---          Take the minimum of the Briggs rise and shear rise
				riseb=amin1(riseb,risesh)
			endif
			! ---       Pick the larger of the buoyant rise & momentum rise
			rise=AMAX1(risem,riseb)
			lheight=.TRUE.
		elseif(method.EQ.2) then
			! ---       Schulman-Scire building downwash
			call prss(x,zly0(ipnum),r0(ipnum),ws0(ipnum),&
			istab0(ipnum),sqrts0(ipnum),diam0(ipnum),&
			exitw0(ipnum),fm(ipnum),fb(ipnum),&
			xmfin(ipnum),xbfin(ipnum),rise_SS)
			! ---       Possible limit to final rise is stored
			rise=AMAX1(zbfin(ipnum),zmfin(ipnum))
			rise=AMIN1(rise,rise_SS)
			lheight=.TRUE.
		elseif(method.EQ.3) then
			! ---       Tabulated Rise
			! ---       Can be used if puff was released during current
			! ---       met. period: IMET must be 1 for this puff
			if(imet.EQ.1) then
				! ---          Check array size
				if(ntr.LE.0) then
					write(io6,*)
					write(io6,*) 'FATAL ERROR in GRISE: invalid array'
					write(io6,*) 'Expected NTR greater than 0'
					write(io6,*) 'Found NTR = ',ntr
					write(io6,*)
					write(*,*)
					stop 'Halted in GRISE -- See list file'
				endif
				call INTERTAB(x,ntr,xtr,i1,i2,factor)
				htgrise=ztr(i1)+factor*(ztr(i2)-ztr(i1))
				rise=htr(i1)+factor*(htr(i2)-htr(i1))
				! ---          Look at full final rise without streamline depression
				! !!!          if(zfrise.GT.0.0) risefac=rise/zfrise
				riselast=htr(ntr)
				if(riselast.GT.0.0) risefac=rise/riselast
				lheight=.FALSE.
			else
				htgrise=zfinal(ipnum)
				lheight=.FALSE.
			endif
		endif
	elseif(istype.EQ.4) then
		! ---    BUOYANT AREA source (interpolate in rise table)
		! ---    -------------------
		! ---    Tabulated rise can be used if puff was released during current
		! ---    met. period: IMET must be 1 for this puff
		if(imet.EQ.1) then
			! ---       Check array size
			if(ntr.LE.0) then
				write(io6,*)
				write(io6,*) 'FATAL ERROR in GRISE: invalid array'
				write(io6,*) 'Expected NTR greater than 0'
				write(io6,*) 'Found NTR = ',ntr
				write(io6,*)
				write(*,*)
				stop 'Halted in GRISE -- See list file'
			endif
			! ---       Get rise
			call prtab(x,ht0(ipnum),ntr,xtr,ztr,rise)
		else
			rise=zfrise
		endif
		lheight=.TRUE.
	elseif(istype.EQ.5 .OR. istype.EQ.6) then
		! ---    LINE source
		! ---    -----------
		! ---    Tabulated rise can be used if puff was released during current
		! ---    or previous met. period: IMET must be 1 or 2 for this puff
		if(imet.GT.2) then
			rise=zfrise
		else
			! ---       Check array size
			if(ntr.LE.0) then
				write(io6,*)
				write(io6,*) 'FATAL ERROR in GRISE: invalid array'
				write(io6,*) 'Expected NTR greater than 0'
				write(io6,*) 'Found NTR = ',ntr
				write(io6,*)
				write(*,*)
				stop 'Halted in GRISE -- See list file'
			endif
			! ---       Get rise
			call prbl(x,xshift,ntr,xtr,ztr,rise)
		endif
		lheight=.TRUE.
	elseif(istype.EQ.10 .OR. istype.EQ.11) then
		! ---    FLARE source
		! ---    ------------
		! ---    Control-file (10) or Variable Emissions File (11)
		if(mrise_fl.EQ.1) then
			! ---       Briggs Rise
			! ---       Get gradual momentum rise
			call prm(diam0(ipnum),exitw0(ipnum),fm(ipnum),ws0(ipnum),&
			istab0(ipnum),sqrts0(ipnum),x,xmfin(ipnum),&
			zmfin(ipnum),risem)
			! ---       Get gradual buoyant rise
			call prb(fb(ipnum),ws0(ipnum),x,xbfin(ipnum),zbfin(ipnum),&
			riseb)
			! ---       Compute effects of vertical wind shear
			if(mshear.eq.1)then
				call prshear(fb(ipnum),ws0(ipnum),plexp0(ipnum),&
				ht0(ipnum),x,xbfin(ipnum),zbfin(ipnum),risesh)
				! ---          Take the minimum of the Briggs rise and shear rise
				riseb=amin1(riseb,risesh)
			endif
			! ---       Pick the larger of the buoyant rise & momentum rise
			rise=AMAX1(risem,riseb)
			lheight=.TRUE.
		elseif(mrise_fl.EQ.2) then
			! ---       Tabulated Rise
			! ---       Can be used if puff was released during current
			! ---       met. period: IMET must be 1 for this puff
			if(imet.EQ.1) then
				! ---          Check array size
				if(ntr.LE.0) then
					write(io6,*)
					write(io6,*) 'FATAL ERROR in GRISE: invalid array'
					write(io6,*) 'Expected NTR greater than 0'
					write(io6,*) 'Found NTR = ',ntr
					write(io6,*)
					write(*,*)
					stop 'Halted in GRISE -- See list file'
				endif
				call INTERTAB(x,ntr,xtr,i1,i2,factor)
				htgrise=ztr(i1)+factor*(ztr(i2)-ztr(i1))
				rise=htr(i1)+factor*(htr(i2)-htr(i1))
				riselast=htr(ntr)
				if(riselast.GT.0.0) risefac=rise/riselast
				lheight=.FALSE.
			endif
		else
			write(io6,*)
			write(io6,*) 'FATAL ERROR in GRISE: flag'
			write(io6,*) 'Expected MRISE_FL = 1,2'
			write(io6,*) 'Found MRISE_FL    = ',mrise_fl
			write(io6,*)
			write(*,*)
			stop 'Halted in GRISE -- See list file'
		endif
	else
		rise=0.0
		lheight=.TRUE.
	endif
	if(LHEIGHT) then
		! ---    Set plume height (release ht. + rise - stipdw) and
		! ---    fractional rise factor
		! ---    (Note: the stack-tip DW adjustment is non-zero only for
		! ---     applicable source types when the MTIP option is selected)
		htgrise=ht0(ipnum)+rise-stipdw(ipnum)
		htgrise=AMAX1(0.0,htgrise)
		if(zfrise.GT.0.) then
			risefac=rise/zfrise
		else
			risefac=1.0
		endif
		if(method.EQ.2) then
			! ---       Additional conditioning for SS downwash result (method=2):
			! ---       Cubic solution for small distances or very small fluxes may
			! ---       result in a very small negative rise due to single precision
			! ---       so condition rise factor to positive values here
			! ---       (note:  rise factor for BID is not used for these sources)
			risefac=AMAX1(risefac,0.0)
		endif
	endif
	! --- Check for valid risefac
	if(risefac.LT.-0.0001) then
		! ---    Potential problem.  Negative rise factor is more
		! ---    negative than expected from precision.
		! ---    Report problem and stop
		write(io6,*)
		write(io6,*)'Fatal Error in GRISE'
		write(io6,*)'Computed risefac is less than 0.0'
		write(io6,*)'   x,htgrise      = ',x,htgrise
		write(io6,*)'   rise,zfrise    = ',rise,zfrise
		write(io6,*)'   risefac        = ',risefac
		stop 'Halted in GRISE -- see list file'
	else
		! ---    Reset small negative rise factor to zero
		risefac=AMAX1(risefac,0.0)
	endif
	return
end
!----------------------------------------------------------------------
subroutine prss(xarg,zly,r0,ws,istab,sqrts,diam,vexit,&
	fluxm,fluxb,xfinm,xfinb,ssrise)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 941215              PRSS
	! ---            Adapted from ISC2:DHPSS
	!
	! --- PURPOSE:  Calculates distance-dependent plume rise for
	!               Schulman-Scire downwash algorithm, as coded in
	!               ISC2.
	!
	! --- INPUTS:
	!          XARG - real     - Downwind distance (m)
	!           ZLY - real     - Effective line length based on sigma-y
	!                            at 3*HL
	!            R0 - real     - Effective source radius based on sigma-z
	!                            at 3*HL
	!            WS - real     - Stack height wind speed (m/s)
	!         ISTAB - integer  - Stability class
	!         SQRTS - real     - Square root of stability parameter
	!                            (sqrt(s)=(g*(dtheta/dz)/tair)**0.5)
	!          DIAM - real     - Stack diameter (m)
	!         VEXIT - real     - Stack gas exit velocity (m/s)
	!         FLUXM - real     - Momentum flux (m**4/s**2)
	!         FLUXB - real     - Buoyancy flux (m**4/s**3)
	!         XFINM - real     - Distance (m) to final momentum
	!                            plume rise
	!         XFINB - real     - Distance (m) to final buoyant
	!                            plume rise
	!
	! --- OUTPUT:
	!        SSRISE - real     - Schulman-Scire plume rise (m)
	!
	! --- PRSS called by:  POINTS1, POINTS2, GRISE
	! --- PRSS calls:      SSCM, SSCB, ROOT3
	!----------------------------------------------------------------------
	real rt(3)
	data pi/3.1415927/, beta/0.6/
	! --- Determine coefficients A, B of cubic equation
	a = (3./beta)*(zly/pi + r0)
	b = (3.*r0/beta**2)*(2.*zly/pi + r0)
	! --- Compute coefficient C for MOMENTUM rise (CM)
	call sscm(xarg,ws,vexit,xfinm,istab,fluxm,sqrts,diam,cm)
	! --- Compute coefficient C for BUOYANT rise (CB)
	call sscb(xarg,xfinb,istab,fluxb,ws,sqrts,cb)
	! --- Solve cubic equation with buoyant rise (CB) and momentum rise (CM)
	! --- and select the larger of the two as the gradual plume rise.  Take
	! --- the largest of the real roots in each case.
	call root3(1.0,a,b,cb,nroots,rt)
	zb=rt(1)
	if(nroots.GT.1) then
		do iroot=2,nroots
			zb=AMAX1(zb,rt(iroot))
		enddo
	endif
	call root3(1.0,a,b,cm,nroots,rt)
	zm=rt(1)
	if(nroots.GT.1) then
		do iroot=2,nroots
			zm=AMAX1(zm,rt(iroot))
		enddo
	endif
	ssrise=AMAX1(zb,zm)
	return
end
!----------------------------------------------------------------------
subroutine sscb(xarg,xfinb,istab,fluxb,ws,sqrts,cb)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 941215              SSCB
	! ---            Adapted from ISC2:BLPCB
	!
	! --- PURPOSE:  Calculates C coefficient for BUOYANT rise used in
	!               Schulman-Scire downwash algorithm, as coded in ISC2
	!
	! --- INPUTS:
	!            XARG - real         - Distance from stack (m)
	!           XFINB - real         - Distance (m) to final buoyant
	!                                  plume rise
	!           ISTAB - integer      - Stability class
	!           FLUXB - real         - Buoyancy flux (m**4/s**3)
	!              WS - real         - Stack height wind speed (m/s)
	!           SQRTS - real         - Square root of stability parameter
	!                                  (sqrt(s)=(g*(dtheta/dz)/tair)**0.5)
	!
	! --- OUTPUT:
	!              CB - real         - C coefficient in cubic eqn for
	!                                  buoyant rise
	!
	! --- SSCB called by:  PRSS
	! --- SSCB calls:      none
	!----------------------------------------------------------------------
	!
	data betasq/0.36/
	! --- Limit distance to that for final buoyant rise
	x=AMIN1(xarg,xfinb)
	if(istab.LE.4) then
		cb=-3.*fluxb*x*x/(2.*betasq*ws*ws*ws)
	else
		cbs=6.*fluxb/(betasq*ws*sqrts*sqrts)
		! ---    Compare stable term to neutral term
		cbn=3.*fluxb*x*x/(2.*betasq*ws*ws*ws)
		! ---    Select minimum of stable and neutral term
		cb=-1.*AMIN1(cbs,cbn)
	endif
	return
end
!----------------------------------------------------------------------
subroutine sscm(xarg,ws,vexit,xfinm,istab,fluxm,sqrts,diam,cm)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 941215              SSCM
	! ---            Adapted from ISC2:BLPCM
	!
	! --- PURPOSE:  Calculates C coefficient for MOMENTUM rise used in
	!               Schulman-Scire downwash algorithm, as coded in ISC2
	!
	! --- INPUTS:
	!            XARG - real         - Distance from stack (m)
	!              WS - real         - Stack height wind speed (m/s)
	!           VEXIT - real         - Stack gas exit velocity (m/s)
	!           XFINM - real         - Distance (m) to final momentum
	!                                  plume rise
	!           ISTAB - integer      - Stability class
	!           FLUXM - real         - Momentum flux (m**4/s**2)
	!           SQRTS - real         - Square root of stability parameter
	!                                  (sqrt(s)=(g*(dtheta/dz)/tair)**0.5)
	!            DIAM - real         - Stack diameter (m)
	!
	! --- OUTPUT:
	!              CM - real         - C coefficient in cubic eqn for
	!                                  momentum rise
	!
	! --- SSCM called by:  PRSS
	! --- SSCM calls:      none
	!----------------------------------------------------------------------
	!
	! --- Calculate entrainment coefficient
	betaj = 0.333333 + ws/vexit
	! --- Limit distance to that for final momentum rise
	x=AMIN1(xarg,xfinm)
	! --- Calculate C
	if(istab.LE.4) then
		cm=-3.*fluxm*x/(betaj*ws)**2
	else
		cms=3.*fluxm*SIN(sqrts*x/ws)/(betaj*betaj*ws*sqrts)
		! ---    Calculate neutral term for comparison
		xfmn=4.*diam*(vexit+3.*ws)**2/(vexit*ws)
		x=AMIN1(xarg,xfmn)
		cmn=3.*fluxm*x/(betaj*ws)**2
		! ---    Select minimum of stable and neutral term
		cm=-1.*AMIN1(cms,cmn)
	endif
	return
end
!----------------------------------------------------------------------
subroutine prbl(xarg,xshift,ntab,xtab,ztab,blrise)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 950715              PRBL
	! ---            D. Strimaitis, SRC
	!
	! --- PURPOSE:  Calculates distance-dependent plume rise for
	!               Buoyant Line sources from precalculated tabulation
	!
	! --- INPUTS:
	!          XARG - real     - Downwind dist. from  "pt." source (m)
	!        XSHIFT - real     - Dist. to "pt." source from upwind edge
	!                            of the block of line sources (m)
	!          NTAB - integer  - Number of points in XTAB and ZTAB arrays
	!          XTAB - real arr.- Tabulated distances from upwind edge of
	!                            the block of line sources to positions
	!                            located between the "full buoyancy" point
	!                            and the point of final rise (m)
	!          ZTAB - real arr.- Rise tabulated at each distance in XTAB,
	!                            for a point located at the most upwind
	!                            point in the block of line sources (m)
	!
	! --- OUTPUT:
	!        BLRISE - real     - Plume rise (m)
	!
	! --- PRBL called by:  GRISE
	! --- PRBL calls:      PRTAB
	!----------------------------------------------------------------------
	real xtab(ntab),ztab(ntab)
	data zero/0.0/
	if(xtab(1).GE.xtab(ntab)) then
		!
		! ---    Case 1:  Final rise reached before "full buoyancy" distance
		size=1.0-xshift/xtab(1)
		xfrise=xtab(ntab)*size
		zfrise=ztab(ntab)*size
		if(xarg.GE.xfrise) then
			z=zfrise
		else
			z=xarg*zfrise/xfrise
		endif
		!
	else
		!
		! ---    Case 2:  Final rise reached beyond "full buoyancy" distance
		!
		! ---    Add the distance shift to xarg to "jump" to plume rise curve
		! ---    for the point of the block of line sources furthest upwind
		xeff=amax1(0.0,xarg+xshift)
		!
		! ---    Calculate the rise
		if(xeff.EQ.0.0) then
			! ---       No rise
			z=0.0
		elseif(xeff.GE.xtab(ntab)) then
			! ---       Final rise
			z=ztab(ntab)
		elseif(xeff.LE.xtab(1)) then
			! ---       Interpolate between zero and first entry in table
			z=xeff*ztab(1)/xtab(1)
		else
			! ---       Interpolate within table
			call prtab(xeff,zero,ntab,xtab,ztab,z)
		endif
		!
		! ---    "Drop" back to plume rise curve for pt. source element
		if(xtab(1).GT.0.0) then
			z=z-xshift*ztab(1)/xtab(1)
		endif
		!
	endif
	!
	! --- Make sure rise is not negative
	blrise=amax1(z,zero)
	return
end
!----------------------------------------------------------------------
subroutine prshear(fb,wsstk,p,hs,x,xfinb,zfinsh,zbsh)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 090612           PRSHEAR
	! ---            J. Scire
	!
	! --- PURPOSE:- Calculate transitional buoyant plume rise using plume
	!               rise equations with WIND SHEAR effects
	!               (Scire and Schulman, 1980).
	!             - Used for both neutral/unstable and stable conditions.
	!             - Result must be compared to Briggs plume rise w/o
	!               shear & minimum of the two values used.
	!
	! --- UPDATE
	! --- V5.0-V6.265   090612  (DGS): add check for invalid power-law exp
	!
	! --- INPUTS:
	!              FB - real         - Buoyancy flux (m**4/s**3)
	!           WSSTK - real         - Stack height wind speed (m/s)
	!               P - real         - Wind shear exponent to power
	!                                  law equation.
	!              HS - real         - Stack top height (m)
	!               X - real         - Downwind distance (m) from the
	!                                  stack to the receptor
	!           XFINB - real         - Distance (m) to final buoyant
	!                                  plume rise for neutral/unstable
	!                                  conditions
	!          ZFINSH - real         - Final buoyant plume rise (m) with
	!                                  wind shear
	!
	! --- OUTPUT:
	!            ZBSH - real         - Transitional plume rise (m) due
	!                                  to plume buoyancy with WIND SHEAR
	!                                  effects
	!
	! --- PRSHEAR called by:  GRISE
	! --- PRSHEAR calls:      none
	!----------------------------------------------------------------------
	! --- Trap invalid power-law exponent
	if(p.LE.0.0) then
		write(*,*)
		write(*,*)'PRSHEAR:  Invalid power-law exponent found'
		write(*,*)'          P = ',p
		stop
	endif
	!
	! --- Check for case of no buoyant rise
	if(x.le.0.0.or.fb.le.0.0)then
		zbsh=0.0
	else
		! ---    Compute transitional buoyant plume rise
		if(x.lt.xfinb .AND. wsstk.gt.0.0)then
			!
			! ---       Protect against zero or small stack ht.
			if(hs.gt.1.0)then
				expt=hs**(3.*p)
			else
				expt=1.0
			endif
			!
			ep=3.+3.*p
			! ---       Constant 0.72 = 2.*beta*beta with beta=0.6
			zbsh=((ep*ep)/(0.72*(3.+p))*(fb*expt/(wsstk**3)))**(1./ep)*&
			x**(2./ep)
			!
			! ---       Final plume height in stable conditions is based on minimum
			! ---       of bent-over and vertical plume equations -- ensure plume
			! ---       height does not exceed final height
			zbsh=amin1(zbsh,zfinsh)
		else
			!
			! ---       Plume has reached final rise (x > xfinb) or winds are calm
			zbsh=zfinsh
		endif
	endif
	!
	return
end
!----------------------------------------------------------------------
subroutine prfinsh(fb,wsstk,p,hs,xfinb,istab,sqrts,zfinsh)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 090612           PRFINSH
	! ---            J. Scire
	!
	! --- PURPOSE:- Calculate FINAL buoyant plume rise using plume rise eqns
	!               with WIND SHEAR effects (Scire and Schulman, 1980).
	!             - Equations for neutral/unstable and stable conditions.
	!             - Result must be compared to Briggs plume rise w/o
	!               shear & minimum of the two values used.
	!
	! --- UPDATE
	! --- V5.0-V6.265   090612  (DGS): add check for invalid power-law exp
	!
	! --- INPUTS:
	!              FB - real         - Buoyancy flux (m**4/s**3)
	!           WSSTK - real         - Stack height wind speed (m/s)
	!               P - real         - Wind shear exponent to power
	!                                  law equation.
	!              HS - real         - Stack top height (m)
	!           XFINB - real         - Distance (m) to final buoyant
	!                                  plume rise for neutral/unstable
	!                                  conditions
	!           ISTAB - integer      - Stability class (1=A,...6=F)
	!           SQRTS - real         - Square root of stability parameter,s
	!                                  (sqrt(s)=(g*(dtheta/dz)/tair)**0.5)
	!
	! --- OUTPUT:
	!          ZFINSH - real         - Final plume rise (m) due to
	!                                  plume buoyancy with WIND SHEAR
	!                                  effects
	!
	! --- PRFINSH called by:  POINTS1, POINTS2
	! --- PRFINSH calls:      none
	!----------------------------------------------------------------------
	! --- Trap invalid power-law exponent
	if(p.LE.0.0) then
		write(*,*)
		write(*,*)'PRFINSH:  Invalid power-law exponent found'
		write(*,*)'          P = ',p
		stop
	endif
	!
	! --- Check for case of no buoyant rise
	if(fb.le.0.0)then
		zfinsh=0.0
	else if(istab.le.4)then
		! ---    NEUTRAL/UNSTABLE final buoyant plume rise
		!
		! ---    Protect against zero or small stack ht. and wind speeds
		wssaf=amax1(wsstk,0.1)
		if(hs.gt.1.0)then
			expt=hs**(3.*p)
		else
			expt=1.0
		endif
		!
		ep=3.+3.*p
		! ---   Constant 0.72 = 2.*beta*beta with beta=0.6
		zfinsh=((ep*ep)/(0.72*(3.+p))*(fb*expt/(wssaf**3)))**(1./ep)*&
		xfinb**(2./ep)
	else
		!
		! ---   STABLE final rise with WIND SHEAR
		!
		! ---    Protect against zero or small stack ht. and wind speeds
		wssaf=amax1(wsstk,0.1)
		if(hs.gt.1.0)then
			expt=hs**p
		else
			expt=1.0
		endif
		ep=3.+p
		! ---    Constant 5.555556 = 2./(beta*beta) with beta=0.6
		zfinsh=(5.555556*ep*expt*fb/(wssaf*sqrts*sqrts))**(1./ep)
	endif
	! DBG ***************
	!      write(*,*)
	!      write(*,*)'PRFINSH:     Power-law exp = ',p
	!      write(*,*)'wsstk,istab,sqrts,fb,xfinb = ',wsstk,istab,sqrts,
	!     &                                          fb,xfinb
	!      write(*,*)'                    zfinsh = ',zfinsh
	!      write(*,*)
	return
end
!----------------------------------------------------------------------
subroutine prfpp(zfin,fb,wsstk,hs,hmix,ta,ptginv,ti,fmix,hmax)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 971107             PRFPP
	! ---            D. Strimaitis, SRC
	!
	! --- PURPOSE:  Revise height of final rise due to Partial Penetration
	!               of an elevated inversion (after Manins: Atm.Env. 18,
	!               p. 2339-2344; with modifications from MESOPUFF II)
	!               (used for buoyant rise in convective mixed layer)
	!
	! --- UPDATE
	! --- V4.0-V5.0     971107  (DGS): add temperature jump at inversion to
	!                                  calling arguments
	!
	! --- INPUTS:
	!            ZFIN - real         - Final rise (m) without inversion
	!              FB - real         - Buoyancy flux (m**4/s**3)
	!           WSSTK - real         - Stack height wind speed (m/s)
	!              HS - real         - Stack Height (m)
	!            HMIX - real         - Current mixing depth (m)
	!              TA - real         - Ambient temperature (mixed layer) (K)
	!          PTGINV - real         - Potential temperature gradient above
	!                                  inversion (K/m)
	!              TI - real         - Potential temperature gradient jump
	!                                  across inversion (K)
	!
	! --- OUTPUTS:
	!            ZFIN - real         - Final rise in mixed layer (m)
	!            FMIX - real         - Fraction of mass in mixed layer
	!            HMAX - real         - Final rise for mass above
	!                                  inversion (m)
	!
	! --- PRFPP called by:  POINTS1, POINTS2
	! --- PRFPP calls:      none
	!----------------------------------------------------------------------
	!
	data g/9.80665/
	! --- Set minimum allowed wind speed at stack-top (wsstk0)
	data wsstk0/1.0/
	!
	! --- If wind speed is less than wsstk0, reset to wsstk0.
	u=wsstk
	if(u.LT.wsstk0) u=wsstk0
	!
	! --- Set height of inversion above stack-top (m)
	zi=hmix-hs
	rise0=0.67*zi
	!
	if(zi.LE.0.0) then
		! ---    Top of stack is above mixed layer, so no mass lies below
		fmix=0.0
		! ---    Use stable rise equation
		! ---    S aloft is (g/T)*d(THETA)/dz
		saloft=g*ptginv/ta
		if(saloft.GT.0.0) then
			rise1=2.6*(fb/(u*saloft) )**0.3333333
		else
			rise1=zfin
		endif
		rise2=rise1
		!
	elseif(zfin.LE.rise0) then
		! ---    No modifications if final rise is LE rise0
		rise1=zfin
		rise2=zi
		fmix=1.0
		!
	else
		! ---    Inversion may alter rise
		! ---    Compute penetration parameter and its inverse (reciprocal)
		peninv=u*(g*ti/ta)*zi*zi/fb
		pen=1./peninv
		!
		! ---    Set fraction of plume remaining in mixed layer
		if(pen.LT.0.08) then
			! ---       All mass remains in mixed layer
			fmix=1.0
		elseif(pen.GT.0.3) then
			! ---       No mass remains in mixed layer
			fmix=0.0
		else
			fmix=0.08*peninv-pen+0.08
		endif
		!
		! ---    Set effective plume heights
		if(fmix.GT.0.0) then
			rise1=zi*(1.-0.33*fmix)
			rise2=zi*(2.-fmix)
		else
			! ---       All mass penetrates inversion: use MESOPUFF rise
			! ---       S aloft is (g/T)*d(THETA)/dz
			saloft=g*ptginv/ta
			if(saloft.GT.0.0) then
				rise1=( 1.8*zi*zi*zi+ 18.75*fb/(u*saloft) )**0.3333333
			else
				rise1=zfin
			endif
			rise2=rise1
		endif
	endif
	!
	! --- Alter the final plume rise passed as argument
	zfin=AMIN1(zfin,rise1)
	!
	! --- Turn rise2 into an effective "past maximum mixing height"
	hmax=hmix+2.*(rise2-zi)
	!
	return
end
!----------------------------------------------------------------------
subroutine prtab(xarg,zstart,nt,x,z,rise)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 950610             PRTAB
	! ---            D. Strimaitis, SRC
	!
	! --- PURPOSE:  Calculates distance-dependent plume rise by
	!               interpolating in plume height table.  Note that height
	!               in table is assumed to include the release height.  If
	!               table contains just the rise, then argument ZSTART
	!               should be entered as 0.0 in the call!
	!
	! --- INPUTS:
	!          XARG - real     - Downwind dist. from source (m)
	!        ZSTART - real     - Plume height at start of rise (m)
	!            NT - integer  - Number of points in table
	!         X(nt) - real     - Tabulated along-wind distance (m)
	!         Z(nt) - real     - Tabulated plume height (m)
	!
	! --- OUTPUT:
	!          RISE - real     - Interpolated plume rise (m)
	!
	! --- PRTAB called by:  GRISE
	! --- PRTAB calls:      none
	!----------------------------------------------------------------------
	real x(nt),z(nt)
	! --- Find index for point in table just beyond given xarg
	ind=0
	do i=1,nt
		if(x(i).LT.xarg) ind=i
	enddo
	indp1=ind+1
	! --- Compute interpolated rise at xarg
	if(indp1.EQ.1) then
		xfac=xarg/x(indp1)
		rise=(z(indp1)-zstart)*xfac
	else
		xfac=(xarg-x(ind))/(x(indp1)-x(ind))
		rise=z(ind)-zstart + (z(indp1)-z(ind))*xfac
	endif
	return
end
!----------------------------------------------------------------------
subroutine setpuf(ii,tsamp,distm,istab,el,sigv,sigw,bvf,ws,dpbl,&
	xnew,ynew,iru,ixmet,iymet,mdmet,ldbhr,uavg,&
	lcalm)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 141230            SETPUF
	!                D. Strimaitis
	!
	! --- PURPOSE:  Set distances/times for puff, and update stored puff
	!               data.
	!
	! --- UPDATE
	! --- V6.269 - TNG-7.1.0  141230  (DGS)
	!                                : Allow roughness adjustment (MROUGH)
	!                                  of RURAL PG sigmas to be used with
	!                                  CALMET gridded fields
	! --- V6.268-V6.269 100430  (DGS): Reinstate modification made in v6.2
	!                                  that uses PRIME wake table to get
	!                                  sigmas for puffs at travel distances
	!                                  that fall within the limits of the
	!                                  table.  PUFRECS change allows this
	!                                  treatment to be used while matching
	!                                  plume sigmas between the wake and
	!                                  the ambient dispersion regime
	!                                  further downwind.
	! --- V6.22-V6.268  100308  (DGS): Revert to using a single virtual
	!                                  time/distance for puffs starting
	!                                  within a PRIME wake.  Modifications
	!                                  made in v6.2 can result in a 
	!                                  mismatch in plume sigmas between the
	!                                  wake and the ambient dispersion
	!                                  regime further downwind.
	!                   100308  (DGS): Remove approximation to BID-altered
	!                                  sigmas
	! --- V6.2-V6.22    070921  (DGS): Check for imet outside valid range
	!                                  of 1 to MXMETSAV+1
	! --- V6.1-V6.2     070629  (DGS): move CALM re-assignment of IDW0()
	!                                  from PUFRECS so downwash effects are
	!                                  turned off as soon as a puff reaches
	!                                  a calm area.
	!                   070629  (DGS): add LCALM as output argument for
	!                                  subsequent use
	!                   070629  (DGS): use PRIME wake table to get sigmas
	!                                  for puffs at travel distances that
	!                                  fall within the limits of the table
	!                                  (previously used virtual distance 
	!                                  calc that could generate sigmas 
	!                                  different from those in table if
	!                                  wind speed changed)
	! --- V5.75-V6.1    050915  (DGS): Add IQSTEP arg in call to WAKE_TAB
	! --- V5.73-V5.75   050225  (DGS): Add DPBL arg to pass on to SETCSIG
	!                                  for TAULY
	! --- V5.7-V5.73    040611  (DGS): Gravitational settling adjustment
	!                                  to puff height at end of step;
	!                                  Add tb1,te1,vsetl to /CURRENT/
	! --- V5.3-V5.7     030402  (DGS): Make puff sigmas consistent with
	!                                  values at end of tables in
	!                                  wake region (PRIME downwash)
	! --- V5.2-V5.3     991222  (DGS): IPUFID already decoded in COMP
	! --- V5.0-V5.0     980918  (DGS): add area-source sigy in quadrature
	! --- V5.0-V5.0     980731  (DGS): when CALM, set min sigmas in /PUFF/
	!                                  to mid-pt values for current step
	! --- V4.0-V5.0     971107  (DGS): bug - ratio2 used for s_e1 instead
	!                                  of ratio3
	!                   971107  (DGS): ICODE obtained from /CURRENT/
	! --- V4.0-V5.0     971107  (DGS): adjust puff ht for MCTADJ=2
	!                   971107  (DGS): replace ZFINAL array with ZPB array
	!                   971107  (DGS): add WALLTRAP call to constrain sigy
	!                                  in valleys
	!                   971107  (DGS): include tip DW in ZFRISE
	!
	! --- INPUTS:
	!          II - integer   - Puff number
	!       TSAMP - real      - Sampling time step (sec)
	!       DISTM - real      - Advection distance (m) for time-step
	!       ISTAB - integer   - PGT stability class at puff
	!          EL - real      - Current Monin-Obukhov length (m)
	!        SIGV - real      - Current sigma-v velocity (m/s)
	!        SIGW - real      - Current sigma-w velocity (m/s)
	!         BVF - real      - Current Brunt-Vaisala freq (1/s)
	!          WS - real      - Current wind speed (m/s)
	!        DPBL - real      - Current depth of PBL (m)
	!        XNEW - real      - New puff x-position (Met Grid Units)
	!        YNEW - real      - New puff y-position (Met Grid Units)
	!         IRU - integer   - Rural cell indicator (rural=0 ; urban=1)
	!   IXMET,IYMET,MDMET
	!             - integers  - Grid cell location
	!       LDBHR - logical   - Debug write logical
	!
	!       Common block /COMPARM/ variables:
	!             SYMIN, SZMIN, WSCALM
	!       Common block /CURRENT/ variables:
	!             IDOPTY, IDOPTZ, ICODE,
	!             IPNUM, ISNUM, ISTYPE, IMET, IQSTEP
	!       Common block /DRYPART/ variables:
	!             VGRAV(mxint,mxpdep)
	!       Common block /FLAGS/ variables:
	!             MHFTSZ, MCTADJ, MTILT
	!       Common block /GRID/ variables:
	!             DGRID
	!       Common block /PUFF/ variables:
	!             SIGYB(mxpuff), SIGZB(mxpuff), ZPB(mxpuff),
	!             TMTOTB(mxpuff), XTOTB(mxpuff), BIDFNL(mxpuff),
	!             IPUFCD(mxpuff), STIPDW(mxpuff)
	!             SY0(mxpuff), IDW0(mxpuff)
	!       Common block /CSIGMA/ variables:
	!             SYH
	!       Parameters:
	!             IO6
	!
	! --- OUTPUT:
	!        UAVG - real      - Mean transport speed (m/s)
	!       LCALM - logical   - CALM logical
	!       Common block /CSIGMA/ variables:
	!             THFTY, THFTZ, SZH
	!       Common block /CURRENT/ variables:
	!             xb1,yb1,zb1,syb1,szb1,
	!             xe1,ye1,ze1,sye1,sze1,
	!             vtyb1,vtzb1,vtye1,vtze1,
	!             vdyb1,vdzb1,vdye1,vdze1,
	!             xttb1,xtte1,xshift,
	!             tb1,te1,vsetl,
	!             sym1,szm1,zm1,fracz1,lup1
	!             ixmc,iymc,mdmc,
	!             ipnum,iage,speedi,srat,temis,
	!             bidsq,xfrise,zfrise,sy0sq
	!       Common block /PUFF/ variables:
	!             XPB(mxpuff), YPB(mxpuff), SIGYB(mxpuff), SIGZB(mxpuff),
	!             TMTOTB(mxpuff), XTOTB(mxpuff), ZPB(mxpuff)
	!
	! --- SETPUF called by:  COMP
	! --- SETPUF calls:      SETCSIG, SIGTY, SIGTZ, HEFTRAN, CTADJ2,
	!                        WALLTRAP, WAKE_TAB
	!
	!----------------------------------------------------------------------
	!
	! --- Include parameters
	include 'params.puf'
	!
	! --- Include common blocks
	include 'csigma.puf'
	include 'comparm.puf'
	include 'current.puf'
	include 'drypart.puf'
	include 'flags.puf'
	include 'grid.puf'
	include 'puff.puf'
	logical ldbhr,lcalm
	data zero/0.0/, half/0.5/, one/1.0/
	data dm2km/0.001/
	! --- Validate imet index
	if(imet.LT.1) then
		write(io6,*)
		write(io6,*) 'FATAL ERROR in SETPUF: bad met period index'
		write(io6,*) 'Expected IMET = 1 to MXMETSAV+1'
		write(io6,*) 'Found    IMET = ',imet
		write(io6,*) '     MXMETSAV = ',mxmetsav
		write(io6,*)
		write(*,*)
		stop 'Halted in SETPUF -- See list file'
	endif
	! --- Initialize variables associated with terrain option MCTADJ=2
	strain=one
	fracz1=zero
	lup1=.FALSE.
	! --- Pass cell location into /CURRENT/ variables for use now and later
	ixmc=ixmet
	iymc=iymet
	mdmc=mdmet
	! --- Total travel distance (m) at beginning and end of step
	xttb1=xtotb(ii)
	xtte1=xttb1+distm
	! --- Total travel time (s) at beginning and end of step
	tb1=tmtotb(ii)
	te1=tb1+tsamp
	! --- Distance travelled this step by puff to midpoint (km)
	dmidkm=0.5*distm*dm2km
	! --- Time travelled by puff to midpoint (sec)
	tmid=0.5*tsamp
	! --- Set mean transport speed for step for use in sigma calc.
	uavg=ws
	ukps=ws*dm2km
	! --- Set dispersion option ("calms" use time-based sigmas)
	lcalm=.FALSE.
	if(ws.LT.wscalm) then
		lcalm=.TRUE.
		idopty=1
		idoptz=1
		! ---    Turn off any downwash and plume rise calculations if CALM
		idw0(ii)=0
		xfinal(ii)=zero
	endif
	!
	! --- Set selected data in /CSIGMA/ for sigma calls
	call SETCSIG(idopty,idoptz,iru,uavg,istab,el,bvf,&
	sigv,sigw,symin,szmin,zpb(ii),dpbl,ixmc,iymc,mdmc)
	!
	! --- Assign puff number
	ipnum=ii
	! --- Set sigma-y squared for initial size of area source
	if(istype.EQ.3 .OR. istype.EQ.4) then
		sy0sq=sy0(ii)*sy0(ii)
	else
		sy0sq=zero
	endif
	! ----------------------------------------------------------------
	! --- Calculate the virtual times and sigmas at Heffter transition
	! ----------------------------------------------------------------
	! --- Process sigma-z only if Gaussian in vertical and Heffter is used
	if(mhftsz.EQ.0 .OR. mod(icode,2).NE.1)then
		szh=syh
		call sigty(syh,zero,zero,dum,thfty,dhfty)
	else
		call heftran(1,zpb(ii),sigyb(ii),sigzb(ii),&
		zero,zero,zero,zero)
	endif
	! ----------------------------------------------------------------------
	! --- Calculate the virtual times and sigmas at start/end/midpoint
	! ----------------------------------------------------------------------
	! --- Initialize control variable for PRIME treatment
	! --- ( 0: PRIME arrays NOT used   1: PRIME arrays used)
	iprime=0
	if(idw0(ipnum).EQ.3 .OR. idw0(ipnum).EQ.4 .AND. imet.EQ.1) then
		! ---    Special case of PRIME building downwash ---
		!        Sigmas are tabulated from the source to the point where the
		!        turbulence reaches ambient.  WAKE_TAB is used to interpolate
		!        sigmas within the tabulated range, using the appropriate
		!        source arrays.  Make the virtuals here consistent with the
		!        sigmas at the end of the tabulated range, for the current
		!        met data, when the puff starts in the wake.
		! ---    Try to process sigmas at start of step
		call WAKE_TAB(istype,isnum,iqstep,idw0(ipnum),xttb1,&
		syb1,szb1,hgr,rise,xlast,iprime)
		if(iprime.EQ.1) then
			! ---       OK, puff starts in wake region; get virtuals at XLAST
			call WAKE_TAB(istype,isnum,iqstep,idw0(ipnum),xlast,&
			sylast,szlast,hgr,rise,xlast2,idone)
			call SIGTY(sylast,zero,zero,sydum,vtylast,vdylast)
			call SIGTZ(szlast,zero,zero,zpb(ii),szdum,vtzlast,vdzlast)
			vdy0km=(vdylast-xlast*dm2km)
			vdz0km=(vdzlast-xlast*dm2km)
			! ---       Process puff at start (within tabulated region)
			syb1=AMAX1(syb1,symin)
			szb1=AMAX1(szb1,szmin)
			call SIGTY(syb1,zero,zero,sydum,vtyb1,vdyb1)
			call SIGTZ(szb1,zero,zero,zpb(ii),szdum,vtzb1,vdzb1)
			! ---       Process midpoint
			xttm1=0.5*(xttb1+xtte1)
			call WAKE_TAB(istype,isnum,iqstep,idw0(ipnum),xttm1,&
			sym1,szm1,hgr,rise,xlast,idone)
			if(idone.EQ.1) then
				! ---          Point within tabulated region
				sym1=AMAX1(sym1,symin)
				szm1=AMAX1(szm1,szmin)
				call SIGTY(sym1,zero,zero,sydum,vtym1,vdym1)
				call SIGTZ(szm1,zero,zero,zpb(ii),szdum,vtzm1,vdzm1)
			else
				! ---          Point beyond tabulated region
				dxkm=AMAX1(0.0,xttm1*dm2km+vdy0km)
				dts=dxkm/ukps
				call SIGTY(zero,dxkm,dts,sym1,vtym1,vdym1)
				dxkm=AMAX1(0.0,xttm1*dm2km+vdz0km)
				dts=dxkm/ukps
				call SIGTZ(zero,dxkm,dts,zpb(ii),szm1,vtzm1,vdzm1)
				sym1=AMAX1(sym1,symin)
				szm1=AMAX1(szm1,szmin)
			endif
			! ---       Process endpoint
			call WAKE_TAB(istype,isnum,iqstep,idw0(ipnum),xtte1,&
			sye1,sze1,hgr,rise,xlast,idone)
			if(idone.EQ.1) then
				! ---          Point within tabulated region
				sye1=AMAX1(sye1,symin)
				sze1=AMAX1(sze1,szmin)
				call SIGTY(sye1,zero,zero,sydum,vtye1,vdye1)
				call SIGTZ(sze1,zero,zero,zpb(ii),szdum,vtze1,vdze1)
			else
				! ---          Point beyond tabulated region
				dxkm=AMAX1(0.0,xtte1*dm2km+vdy0km)
				dts=dxkm/ukps
				call SIGTY(zero,dxkm,dts,sye1,vtye1,vdye1)
				dxkm=AMAX1(0.0,xtte1*dm2km+vdz0km)
				dts=dxkm/ukps
				call SIGTZ(zero,dxkm,dts,zpb(ii),sze1,vtze1,vdze1)
				sye1=AMAX1(sye1,symin)
				sze1=AMAX1(sze1,szmin)
			endif
		endif
	endif
	if(iprime.EQ.0) then
		! ---    Obtain sigmas and virtuals from puff properties at start
		! ---    SIGMA-Y:
		! ---    Beginning of step (syb1,vtyb1,vdyb1)
		call sigty(sigyb(ii),zero,zero,syb1,vtyb1,vdyb1)
		! ---    Midpoint of step (sym1,vtym1,vdym1)
		tm=vtyb1+tmid
		dm=vdyb1+dmidkm
		call sigty(zero,dm,tm,sym1,vtym1,vdym1)
		! ---    End of step (sye1,vtye1,vdye1)
		te=tm+tmid
		de=dm+dmidkm
		call sigty(zero,de,te,sye1,vtye1,vdye1)
		!
		! ---    If Gaussian in vertical, do same for SIGMA-Z:
		szb1=sigzb(ii)
		szm1=sigzb(ii)
		sze1=sigzb(ii)
		if(mod(icode,2).EQ.1)then
			! ---       Beginning of step (szb1,vtzb1,vdzb1)
			call sigtz(sigzb(ii),zero,zero,zpb(ii),szb1,vtzb1,vdzb1)
			! ---       Midpoint of step (szm1,vtzm1,vdzm1)
			tm=vtzb1+tmid
			dm=vdzb1+dmidkm
			call sigtz(zero,dm,tm,zpb(ii),szm1,vtzm1,vdzm1)
			! ---       End of step (sze1,vtze1,vdze1)
			te=tm+tmid
			de=dm+dmidkm
			call sigtz(zero,de,te,zpb(ii),sze1,vtze1,vdze1)
		endif
	endif
	! --- Final Adjustments
	! ---------------------
	! -------------
	! --- SIGMA-Y
	! -------------
	! --- Apply sidewall constraint to sigma-y for stable layer in
	! --- valleys if MCTADJ=2 option is selected
	if(mctadj.EQ.2) then
		! ---    Midpoint of step
		xmid=half*(xpb(ii)+xnew)
		ymid=half*(ypb(ii)+ynew)
		call walltrap(ldbhr,syb1,sym1,zpb(ii),xpb(ii),ypb(ii),&
		xmid,ymid,bvf,ws,itrap)
		if(itrap.EQ.1) then
			syadj=sym1
			call sigty(syadj,zero,zero,sym1,vtym1,vdym1)
			te=vtym1+tmid
			de=vdym1+dmidkm
			call sigty(zero,de,te,sye1,vtye1,vdye1)
		endif
		! ---    End of step
		call walltrap(ldbhr,sym1,sye1,zpb(ii),xmid,ymid,&
		xnew,ynew,bvf,ws,itrap)
		if(itrap.EQ.1) then
			syadj=sye1
			call sigty(syadj,zero,zero,sye1,vtye1,vdye1)
		endif
	endif
	!
	! --- Allow for precision problems;  sigmas must grow with time
	if(syb1.GT.sym1) sym1=syb1
	if(sym1.GT.sye1) sye1=sym1
	!
	! --- Update stored sigma for start of next step (without BID & SY0)
	sigyb(ii)=sye1
	!
	! --- Add quadrature contribution to sigmas in /CURRENT/
	sysq=bidfnl(ii)+sy0sq
	if(sysq.GT.zero) then
		syb1=SQRT(syb1*syb1+sysq)
		sym1=SQRT(sym1*sym1+sysq)
		sye1=SQRT(sye1*sye1+sysq)
	endif
	!      if(sysq.GT.zero) then
	!         ratio1=sysq/(syb1*syb1)
	!         ratio2=sysq/(sym1*sym1)
	!         ratio3=sysq/(sye1*sye1)
	!         if(ratio1.LE.0.2) then
	!c ---       Approximate square root
	!            syb1=syb1*(one+half*ratio1)
	!            sym1=sym1*(one+half*ratio2)
	!            sye1=sye1*(one+half*ratio3)
	!         else
	!            syb1=syb1*sqrt(one+ratio1)
	!            sym1=sym1*sqrt(one+ratio2)
	!            sye1=sye1*sqrt(one+ratio3)
	!         endif
	!      endif
	!
	! --- Update minimum sigma (with BID) for subsequent sampling,
	!     to equal current mid-point value (active only during calms)
	if(LCALM) then
		sy0(ii)=sym1
	endif
	! -------------
	! --- SIGMA-Z
	! -------------
	! --- If Gaussian in vertical, do same for SIGMA Z
	zb1=zpb(ii)
	zm1=zpb(ii)
	ze1=zpb(ii)
	if(mod(icode,2).EQ.1)then
		! ---    Apply strain adjustment to sigmas and adjust puff height
		! ---    for stable layers if MCTADJ=2 option is selected
		if(mctadj.EQ.2) then
			!
			! ---       Midpoint of step
			r=sigzb(ii)/szm1
			xmid=half*(xpb(ii)+xnew)
			ymid=half*(ypb(ii)+ynew)
			call ctadj2(r,zpb(ii),xpb(ii),ypb(ii),xmid,ymid,&
			sigzb(ii),bvf,ws,ldbhr,strain,zm1,fracz1,lup1)
			szadj=szm1*strain
			call sigtz(szadj,zero,zero,zpb(ii),szm1,vtzm1,vdzm1)
			! ---       End of step
			r=sigzb(ii)/sze1
			call ctadj2(r,zpb(ii),xpb(ii),ypb(ii),xnew,ynew,&
			sigzb(ii),bvf,ws,ldbhr,strain,ze1,fracz1,lup1)
			szadj=sze1*strain
			call sigtz(szadj,zero,zero,zpb(ii),sze1,vtze1,vdze1)
		endif
		!
		! --- Allow for precision problems;  sigmas must grow with time
		if(szb1.GT.szm1) szm1=szb1
		if(szm1.GT.sze1) sze1=szm1
		!
		! ---    Update stored sigma for start of next step (without BID)
		sigzb(ii)=sze1
		!
		! ---    Add contribution of buoyancy-enhancement to sigmas in /CURRENT/
		if(bidfnl(ii).GT.zero) then
			szb1=SQRT(szb1*szb1+bidfnl(ii))
			szm1=SQRT(szm1*szm1+bidfnl(ii))
			sze1=SQRT(sze1*sze1+bidfnl(ii))
		endif
		!         if(bidfnl(ii).GT.zero) then
		!            ratio1=bidfnl(ii)/(szb1*szb1)
		!            ratio2=bidfnl(ii)/(szm1*szm1)
		!            ratio3=bidfnl(ii)/(sze1*sze1)
		!            if(ratio1.LT.0.2) then
		!c ---          Approximate square root
		!               szb1=szb1*(one+half*ratio1)
		!               szm1=szm1*(one+half*ratio2)
		!               sze1=sze1*(one+half*ratio3)
		!            else
		!               szb1=szb1*sqrt(one+ratio1)
		!               szm1=szm1*sqrt(one+ratio2)
		!               sze1=sze1*sqrt(one+ratio3)
		!            endif
		!         endif
		!
		! ---    Update minimum sigma (with BID) for subsequent sampling,
		!        to equal current mid-point value (active only during calms)
		if(LCALM) then
			sz0(ii)=szm1
		endif
	endif
	! ----------------------------------------
	! --- Store remaining /CURRENT/ variables
	! ----------------------------------------
	!
	! --- Puff coordinates in /CURRENT/ are relative to the origin of the
	! --- meteorological grid (0.0, 0.0) = LL corner of cell (1,1) --
	! --- units: meters
	xb1=xpb(ii)*dgrid
	yb1=ypb(ii)*dgrid
	xe1=xnew*dgrid
	ye1=ynew*dgrid
	!
	! --- Set sigma**2 at final rise due to buoyancy enhancement
	bidsq=bidfnl(ii)
	!
	! --- Set distance to final rise, and final rise height (delta)
	xfrise=xfinal(ii)
	zfrise=zfinal(ii)-(ht0(ii)-stipdw(ii))
	!
	! --- Set distance from upwind edge of line sources, or distance to
	! --- end of cavity for point sources with PRIME downwash
	xshift=xshift0(ii)
	!
	! --- Wind speed variables at time of release of emissions
	speedi=ws0(ii)
	srat=srat0(ii)
	!
	! --- Duration (sec) of the original emissions release
	temis=temit0(ii)
	!
	! --- Treat all puffs as OLD
	iage=1
	! --- Gravitational settling (Initial implementation!)
	vsetl=vgrav(1,1)
	if(mtilt.EQ.1) then
		ze1=ze1-vsetl*tsamp
		ze1=AMAX1(ze1,0.0)
	endif
	! ------------------------------
	! --- Update /PUFF/ variables
	! ------------------------------
	!
	! --- Update total distance (m) and time (s) to end of current step
	xtotb(ii)=xtotb(ii)+distm
	tmtotb(ii)=tmtotb(ii)+tsamp
	!
	! --- Update position of puff at end of step
	xpb(ii)=xnew
	ypb(ii)=ynew
	zpb(ii)=ze1
	if(ldbhr)then
		write(io6,*) 'SETPUF --'
		write(io6,252)dmidkm,tmid,xtotb(ii),&
		tmtotb(ii)
		252      format(10x,' DMID(km)=',f10.3,' TMID=',f10.1,&
		' XTOTB(m)=',f10.1,' TMTOTB=',f10.1)
		write(io6,256)icode,istab,sigyb(ii),sigzb(ii),strain
		256      format(10x,'ICODE=',i2,' ISTAB=',i1,' SIGYB=',f10.1,&
		' SIGZB=',f10.1,' STRAIN=',f10.3)
		write(io6,*) 'Time to start of Heffter curves:'
		write(io6,*) '   THFTY,THFTZ =',thfty,thftz
		write(io6,*) 'Dispersion properties assessed at mid-pt cell'
		write(io6,*) '   ixmc,iymc,mdmc = ',ixmc,iymc,mdmc
		write(io6,*) '   yz0fac(ixmc,iymc,mdmc) = ',&
		yz0fac(ixmc,iymc,mdmc)
		write(io6,*) '   az0fac(ixmc,iymc,mdmc) = ',&
		az0fac(ixmc,iymc,mdmc)
		write(io6,*) '   bz0trm(ixmc,iymc,mdmc) = ',&
		bz0trm(ixmc,iymc,mdmc)
		write(io6,*) 'Virtuals stored in /CURRENT/'
		write(io6,*) '   VTZB1,VTZE1 = ',vtzb1,vtze1
		write(io6,*) '   VDZB1,VDZE1 = ',vdzb1,vdze1
		write(io6,*) '   VTYB1,VTYE1 = ',vtyb1,vtye1
		write(io6,*) '   VDYB1,VDYE1 = ',vdyb1,vdye1
		write(io6,*) 'SIG-Y0 Squared = ',sy0sq
		if(idw0(ipnum).EQ.3 .OR. idw0(ipnum).EQ.4) then
			write(io6,*) '   IDW0,IPRIME = ',idw0(ipnum),iprime
		endif
	endif
	return
end
!----------------------------------------------------------------------
subroutine setslg(ii,iold,tsamp,iru,istab,el,sigv,sigw,bvf,ws,&
	dpbl,dxmold,dymold,dxmnew,dymnew,ixmet,iymet,&
	mdmet,ldbhr,uavg,lcalm)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 141230            SETSLG
	!                J. Scire, D. Strimaitis
	!
	! --- PURPOSE:  Set the coordinates, sigmas, and plume height variables
	!               for the "current" slug in common block /CURRENT/
	!               for use by the slug sampling routine
	!
	! --- UPDATE
	! --- V6.268 - TNG-7.1.0  141230  (DGS)
	!                                : Allow roughness adjustment (MROUGH)
	!                                  of RURAL PG sigmas to be used with
	!                                  CALMET gridded fields
	! --- V5.75-V6.268  100108  (DGS): Remove approximation to BID-altered
	!                                  sigmas
	! --- V5.75-V6.2    070629  (DGS): move CALM re-assignment of IDW0()
	!                                  from SLGRECS so downwash effects are
	!                                  turned off as soon as a puff reaches
	!                                  a calm area.
	!                   070629  (DGS): add LCALM as output argument for
	!                                  subsequent use
	! --- V5.73-V5.75   050225  (DGS): Add DPBL arg to pass on to SETCSIG
	!                                  for TAULY
	! --- V5.3-V5.73    040611  (DGS): Gravitational settling adjustment
	!                                  to slug height at end of step;
	!                                  Add tb1/2,te1/2,vsetl to /CURRENT/
	! --- V5.2-V5.3     991222  (DGS): IPUFID already decoded in COMP
	! --- V5.0-V5.0     980918  (DGS): add area-source sigy in quadrature
	! --- V4.0-V5.0     971107  (DGS): add treatment for variable line
	!                                  sources
	!                   971107  (DGS): compute speed0 to be consistent with
	!                                  actual length of fresh slug
	! --- V4.0-V5.0     971107  (DGS): adjust puff ht for MCTADJ=2
	!                   971107  (DGS): replace ZFINAL array with ZPB,ZPE
	!                   971107  (DGS): add WALLTRAP call to constrain sigy
	!                                  in valleys
	!                   971107  (DGS): include tip DW in ZFRISE
	!
	! --- INPUTS:
	!          II - integer   - Slug number
	!        IOLD - integer   - Flag indicating if slug "II" is a new slug
	!                           currently being released (0=new slug,
	!                           1=old slug)
	!       TSAMP - real      - Sampling time step (sec)
	!         IRU - integer   - Rural cell indicator (rural=0 ; urban=1)
	!       ISTAB - integer   - Current PGT stability class at slug
	!          EL - real      - Current Monin-Obukhov length (m)
	!        SIGV - real      - Current sigma-v velocity (m/s)
	!        SIGW - real      - Current sigma-w velocity (m/s)
	!         BVF - real      - Current Brunt-Vaisala freq (1/s)
	!          WS - real      - Current wind speed (m/s)
	!        DPBL - real      - Current depth of PBL (m)
	!      DXMOLD - real      - Change in X coordinate (m) of the oldest
	!                           end of the slug
	!      DYMOLD - real      - Change in Y coordinate (m) of the oldest
	!                           end of the slug
	!      DXMNEW - real      - Change in X coordinate (m) of the youngest
	!                           end of the slug
	!      DYMNEW - real      - Change in Y coordinate (m) of the youngest
	!                           end of the slug
	!   IXMET,IYMET,MDMET
	!             - integers  - Grid cell location
	!       LDBHR - logical   - Debug write logical
	!
	!       Common block /COMPARM/ variables:
	!             SYMIN, SZMIN, WSCALM
	!       Common block /CURRENT/ variables:
	!             IXMC,IYMC,MDMC
	!             IDOPTY, IDOPTZ, IPNUM, ISTYPE, ISNUM
	!       Common block /DRYPART/ variables:
	!             VGRAV(mxint,mxpdep)
	!       Common block /FLAGS/ variables:
	!             MSLUG, MHFTSZ, MTILT
	!       Common block /GRID/ variables:
	!             DGRID, DGRIDI
	!       Common block /PUFF/ variables:
	!             XPB(mxpuff), YPB(mxpuff), SIGYB(mxpuff), SIGZB(mxpuff)
	!             ZPB(mxpuff), TMTOTB(mxpuff), XTOTB(mxpuff), HT0(mxpuff)
	!             BIDFNL(mxpuff), WS0(mxpuff), SRAT0(mxpuff), TEMIT0(mxpuff)
	!             XSHIFT0(mxpuff), SY0(mxpuff), SZ0(mxpuff),
	!             STIPDW(mxpuff)
	!       Common block /SLUG/ variables:
	!             XPE(mxpuff), YPE(mxpuff), SIGYE(mxpuff), SIGZE(mxpuff),
	!             ZPE(mxpuff), TMTOTE(mxpuff), XTOTE(mxpuff)
	!       Common block /CSIGMA/ variables:
	!             SYH
	!       Parameters:
	!             MXPUFF, MXSPEC, MXNZP1, IO6
	!
	! --- OUTPUT:
	!        UAVG - real      - Mean transport speed (m/s)
	!       LCALM - logical   - CALM logical
	!
	!       Common block /CURRENT/ variables:
	!             All variables
	!       Common block /CSIGMA/ variables:
	!             THFTY, THFTZ, SZH
	!       Common block /SLUG/ variables:
	!             SPEED0(mxpuff)
	!
	! --- SETSLG called by:  COMP
	! --- SETSLG calls:      SETCSIG, SIGTY, SIGTZ, HEFTRAN, CTADJ2
	!
	!----------------------------------------------------------------------
	!
	! --- Include parameters
	include 'params.puf'
	!
	! --- Include common blocks
	include 'comparm.puf'
	include 'current.puf'
	include 'drypart.puf'
	include 'flags.puf'
	include 'grid.puf'
	include 'puff.puf'
	include 'slug.puf'
	include 'csigma.puf'
	!
	logical ldbhr, lcalm
	data zero/0.0/, half/0.5/, one/1.0/, dm2km/0.001/
	!
	! --- Slug coordinates in /CURRENT/ are relative to the origin of the
	! --- meteorological grid (0.0, 0.0) = LL corner of cell (1,1) --
	! --- units: meters
	!
	if(ldbhr) then
		! ---    Write out SETSLG information
		write(io6,*)
		write(io6,*) 'SETSLG:  puff #= ',ii
		write(io6,*) 'SETSLG:  sigye,sigze= ',sigye(ii),sigze(ii)
		write(io6,*) 'SETSLG:  sigyb,sigzb= ',sigyb(ii),sigzb(ii)
		write(io6,*)
	endif
	! --- Assign puff number
	ipnum=ii
	! --- Pass cell location into /CURRENT/ variables for use now and later
	ixmc=ixmet
	iymc=iymet
	mdmc=mdmet
	! --- Set sigma-y squared for initial size of area source
	if(istype.EQ.3 .OR. istype.EQ.4) then
		sy0sq=sy0(ii)*sy0(ii)
	else
		sy0sq=zero
	endif
	! --- Add BID to this
	sysq=bidfnl(ii)+sy0sq
	! --- Note on nomenclature for variables ---
	!     _____b : specifications for the NEWER end of slug (stored arrays)
	!     _____e : specifications for the OLDER end of slug (stored arrays)
	!     _____b0: specifications for the CENTER of slug at START of step
	!     _____e0: specifications for the CENTER of slug at END of step
	!     _____b1: specifications for the OLDER end of slug at START of step
	!     _____e1: specifications for the OLDER end of slug at END of step
	!     _____b2: specifications for the NEWER end of slug at START of step
	!     _____e2: specifications for the NEWER end of slug at END of step
	! --- Initialize variables associated with terrain option MCTADJ=2
	strain=one
	fracz1=zero
	fracz2=zero
	! --- Determine slug travel distance and travel time (s) at BEGINNING
	! --- and END of step
	xttb1=xtote(ii)
	distm1=sqrt(dxmold**2+dymold**2)
	xtte1=xtote(ii)+distm1
	tb1=tmtote(ii)
	te1=tb1+tsamp
	tmtote1=te1
	if(iold.eq.1)then
		xttb2=xtotb(ii)
		distm2=sqrt(dxmnew**2+dymnew**2)
		xtte2=xtotb(ii)+distm2
		tb2=tmtotb(ii)
		te2=tb2+tsamp
		tmtote2=te2
	else
		xttb2=zero
		xtte2=zero
		tb2=zero
		te2=zero
		tmtote2=zero
	endif
	! --- Set the average transport speed (use old end at end of step)
	uavg=ws
	! --- Set dispersion option ("calms" use time-based sigmas)
	if(ws.LT.wscalm) then
		idopty=1
		idoptz=1
		! ---    Turn off any downwash and plume rise calculations if CALM
		idw0(ii)=0
		xfinal(ii)=zero
		lcalm=.TRUE.
	else
		lcalm=.FALSE.
	endif
	!
	! --- Set selected data in /CSIGMA/ for sigma calls
	call SETCSIG(idopty,idoptz,iru,uavg,istab,el,bvf,&
	sigv,sigw,symin,szmin,zpe(ii),dpbl,ixmc,iymc,mdmc)
	! --- Set gravitational settling velocity
	vsetl=vgrav(1,1)
	!
	! ---------------------------------------------------------
	! --- Set parameters for the OLDEST end of the slug (1)
	! ---------------------------------------------------------
	!
	! --- Values at the START of the sampling step (end of previous step)
	! --------------------------------------------
	if(iold.eq.1)then
		!
		! ---    OLD SLUG -- Oldest end starts away from source
		xb1=xpe(ii)*dgrid
		yb1=ype(ii)*dgrid
		zb1=zpe(ii)
		! ---    Identify "ambient" sigmas without BID
		syab1=sigye(ii)
		szab1=sigze(ii)
		! ---    Add quadrature contribution to sigmas in /CURRENT/
		syb1=syab1
		if(sysq.GT.zero) syb1=sqrt(syab1**2+sysq)
		szb1=szab1
		if(bidfnl(ii).GT.zero) szb1=sqrt(szab1**2+bidfnl(ii))
		!
	else
		!
		! ---    NEW SLUG -- Oldest end starts at source
		xb1=xpb(ii)*dgrid
		yb1=ypb(ii)*dgrid
		zb1=zpb(ii)
		! ---    Identify "ambient" sigmas without BID
		syab1=sigyb(ii)
		szab1=sigzb(ii)
		! ---    Do not add quadrature contribution to sigmas (BID is zero
		! ---    at source and  NEW SLUG from AREA SOURCE uses line source
		! ---    instead of effective sigma-y)
		syb1=syab1
		szb1=szab1
	endif
	!
	! --- Calculate the virtual times and sigmas at Heffter transition
	if(mhftsz.EQ.0)then
		szh=syh
		call sigty(syh,zero,zero,dum,thfty,dhfty)
	else
		call heftran(1,zb1,syab1,szab1,zero,zero,zero,zero)
	endif
	!
	! --- Set virtual time/distance for starting sigmas without BID
	call sigty(syab1,zero,zero,dumy,vtyb1,vdyb1)
	call sigtz(szab1,zero,zero,zb1,dumz,vtzb1,vdzb1)
	!
	! --- Values at the END of the sampling step
	! ------------------------------------------
	xe1=xb1+dxmold
	ye1=yb1+dymold
	ze1=zb1
	distkm=distm1*dm2km
	tye=vtyb1+tsamp
	dye=vdyb1+distkm
	tze=vtzb1+tsamp
	dze=vdzb1+distkm
	! --- Gravitational settling (Initial implementation!)
	if(mtilt.EQ.1) then
		ze1=ze1-vsetl*tsamp
		ze1=AMAX1(ze1,0.0)
	endif
	!
	! --- Compute new values of sigma y, sigma z (without BID),
	! --- and the corresponding virtual time/distance
	call sigty(zero,dye,tye,syae1,vtye1,vdye1)
	call sigtz(zero,dze,tze,ze1,szae1,vtze1,vdze1)
	!
	! --- Apply the terrain adjustments to sigma-z and sigma-y at end
	! --- of step if option is selected
	if(mctadj.EQ.2) then
		! ---    Old end of slug
		r=szab1/szae1
		call ctadj2(r,zb1,xb1*dgridi,yb1*dgridi,xe1*dgridi,ye1*dgridi,&
		szab1,bvf,ws,ldbhr,&
		strain,ze1,fracz1,lup1)
		szadj=szae1*strain
		call sigtz(szadj,zero,zero,ze1,szae1,vtze1,vdze1)
		!
		call walltrap(ldbhr,syab1,syae1,zb1,xb1*dgridi,yb1*dgridi,&
		xe1*dgridi,ye1*dgridi,bvf,ws,itrap)
		if(itrap.EQ.1) then
			syadj=syae1
			call sigty(syadj,zero,zero,syae1,vtye1,vdye1)
		endif
	endif
	!
	! --- Add quadrature contribution to new sigmas
	sye1=syae1
	sze1=szae1
	if(sysq.GT.zero .AND. iold.EQ.1) sye1=SQRT(syae1*syae1+sysq)
	if(bidfnl(ii).GT.zero) sze1=SQRT(szae1*szae1+bidfnl(ii))
	!      if(sysq.GT.zero .AND. iold.EQ.1) then
	!         ratio=sysq/(syae1*syae1)
	!         if(ratio.LE.0.2) then
	!c ---       Approximate square root
	!            sye1=syae1*(one+half*ratio)
	!         else
	!            sye1=syae1*sqrt(one+ratio)
	!         endif
	!      endif
	!      if(bidfnl(ii).GT.zero) then
	!         ratio=bidfnl(ii)/(szae1*szae1)
	!         if(ratio.LE.0.2) then
	!c ---       Approximate square root
	!            sze1=szae1*(one+half*ratio)
	!         else
	!            sze1=szae1*sqrt(one+ratio)
	!         endif
	!      endif
	! --- Set elongation speed for fresh slug, based on older end
	if(iold .EQ. 0) speed0(ii)=distm1/(srat0(ii)*temit0(ii))
	!
	! ---------------------------------------------------------
	! --- Set parameters for the YOUNGEST end of the slug (2)
	! ---------------------------------------------------------
	!
	if(iold.eq.0) then
		!
		! ---    NEW SLUG -- Youngest end attached to source at START & END
		! ---                Set parameters equal to Old end at START (b1)
		!
		! ---    START of sampling step
		! -----------------------------
		xb2=xb1
		yb2=yb1
		zb2=zb1
		syab2=syab1
		szab2=szab1
		syb2=syb1
		szb2=szb1
		vtyb2=vtyb1
		vdyb2=vdyb1
		vtzb2=vtzb1
		vdzb2=vdzb1
		! ---    END of sampling step
		! ---------------------------
		xe2=xb1
		ye2=yb1
		ze2=zb1
		syae2=syab1
		szae2=szab1
		sye2=syb1
		sze2=szb1
		vtye2=vtyb1
		vdye2=vdyb1
		vtze2=vtzb1
		vdze2=vdzb1
		!
	else
		!
		! ---    OLD SLUG -- Youngest end may be attached to source at START
		! ---             -- but is allowed to move away from source at END
		!
		! ---    START of the sampling step (end of previous step)
		! ---------------------------------
		xb2=xpb(ii)*dgrid
		yb2=ypb(ii)*dgrid
		zb2=zpb(ii)
		! ---    Identify "ambient" sigmas without BID
		syab2=sigyb(ii)
		szab2=sigzb(ii)
		! ---    Add quadrature contribution to new sigmas
		syb2=syab2
		szb2=szab2
		if(sysq.GT.zero) syb2=SQRT(syab2*syab2+sysq)
		if(bidfnl(ii).GT.zero) szb2=SQRT(szab2*szab2+bidfnl(ii))
		!         if(sysq.GT.zero) then
		!            ratio=sysq/(syab2*syab2)
		!            if(ratio.LE.0.2) then
		!c ---          Approximate square root
		!               syb2=syab2*(one+half*ratio)
		!            else
		!               syb2=syab2*sqrt(one+ratio)
		!            endif
		!         endif
		!         if(bidfnl(ii).GT.zero) then
		!            ratio=bidfnl(ii)/(szab2*szab2)
		!            if(ratio.LE.0.2) then
		!c ---          Approximate square root
		!               szb2=szab2*(one+half*ratio)
		!            else
		!               szb2=szab2*sqrt(one+ratio)
		!            endif
		!         endif
		!
		! ---    Calculate the virtual times and sigmas at Heffter transition
		if(mhftsz.EQ.0)then
			szh=syh
			call sigty(syh,zero,zero,dum,thfty,dhfty)
		else
			call heftran(1,zb2,syab2,szab2,zero,zero,zero,zero)
		endif
		!
		! ---    Set virtual time/distance for starting sigmas without BID
		call sigty(syab2,zero,zero,dumy,vtyb2,vdyb2)
		call sigtz(szab2,zero,zero,zb2,dumz,vtzb2,vdzb2)
		!
		!
		! ---    END of the sampling step
		! -------------------------------
		xe2=xb2+dxmnew
		ye2=yb2+dymnew
		ze2=zb2
		distkm=distm2*dm2km
		tye=vtyb2+tsamp
		dye=vdyb2+distkm
		tze=vtzb2+tsamp
		dze=vdzb2+distkm
		! ---    Gravitational settling (Initial implementation!)
		if(mtilt.EQ.1) then
			ze2=ze2-vsetl*tsamp
			ze2=AMAX1(ze2,0.0)
		endif
		!
		! ---    Compute new values of sigma y, sigma z (without BID),
		! ---    and the corresponding virtual time/distance
		call sigty(zero,dye,tye,syae2,vtye2,vdye2)
		call sigtz(zero,dze,tze,ze2,szae2,vtze2,vdze2)
		!
		! ---    Apply the terrain adjustment to sigma-z and sigma-y at end
		! ---    of step if option is selected
		if(mctadj.EQ.2) then
			! ---       Young end of slug
			r=szab2/szae2
			call ctadj2(r,zb2,xb2*dgridi,yb2*dgridi,xe2*dgridi,&
			ye2*dgridi,szab2,bvf,ws,ldbhr,&
			strain,ze2,fracz2,lup2)
			szadj=szae2*strain
			call sigtz(szadj,zero,zero,ze2,szae2,vtze2,vdze2)
			!
			call walltrap(ldbhr,syab2,syae2,ze2,xb2*dgridi,yb2*dgridi,&
			xe2*dgridi,ye2*dgridi,bvf,ws,itrap)
			if(itrap.EQ.1) then
				syadj=syae2
				call sigty(syadj,zero,zero,syae2,vtye2,vdye2)
			endif
		endif
		!
		! ---    Add contribution of buoyancy-enhancement
		sye2=syae2
		sze2=szae2
		if(sysq.GT.zero) sye2=SQRT(syae2*syae2+sysq)
		if(bidfnl(ii).GT.zero) sze2=SQRT(szae2*szae2+bidfnl(ii))
		!         if(sysq.GT.zero) then
		!            ratio=sysq/(syae2*syae2)
		!            if(ratio.LE.0.2) then
		!c ---          Approximate square root
		!               sye2=syae2*(one+half*ratio)
		!            else
		!               sye2=syae2*sqrt(one+ratio)
		!            endif
		!         endif
		!         if(bidfnl(ii).GT.zero) then
		!            ratio=bidfnl(ii)/(szae2*szae2)
		!            if(ratio.LE.0.2) then
		!c ---          Approximate square root
		!               sze2=szae2*(one+half*ratio)
		!            else
		!               sze2=szae2*sqrt(one+ratio)
		!            endif
		!         endif
	endif
	!
	! ------------------------------------------------------------
	! --- Transfer other puff and slug array data to current slug
	! ------------------------------------------------------------
	!
	! --- Set sigma**2 at final rise due to buoyancy enhancement
	bidsq=bidfnl(ii)
	!
	! --- Set distance to final rise, and final rise height (delta)
	xfrise=xfinal(ii)
	zfrise=zfinal(ii)-(ht0(ii)-stipdw(ii))
	!
	! --- Set distance from upwind edge of line sources
	xshift=xshift0(ii)
	!
	! --- Wind speed variables at time of release of emissions
	speedi=speed0(ii)
	srat=srat0(ii)
	!
	! --- Duration (sec) of the original emissions release
	temis=temit0(ii)
	!
	! --- Set the new/old slug flag (0=new slug, 1=old slug)
	iage=iold
	!
	! --- Reset stored slug data for the next step
	xpe(ii)=xe1*dgridi
	ype(ii)=ye1*dgridi
	zpe(ii)=ze1
	sigye(ii)=syae1
	sigze(ii)=szae1
	xtote(ii)=xtte1
	tmtote(ii)=tmtote1
	xpb(ii)=xe2*dgridi
	ypb(ii)=ye2*dgridi
	zpb(ii)=ze2
	sigyb(ii)=syae2
	sigzb(ii)=szae2
	xtotb(ii)=xtte2
	tmtotb(ii)=tmtote2
	!
	!
	! ---------------------------------------------------------------
	! --- Special treatment of NEW SLUG from LINE SOURCE (istype=5,6)
	! ---------------------------------------------------------------
	!
	! --- Alter /CURRENT/ sigma-y, sigma-z to include only downwash effects
	! --- for the line source.  During the first step, the line source is
	! --- modeled as a series of crosswind line sources, so the line-length
	! --- is explicitly modeled.  Later steps will be using the effective
	! --- sigmas, as these are now stored in the /PUFF/ and /SLUG/ commons
	! --- (sigyb(ii),sigye(ii)).
	! --- NOTE: BID adjustment is zero for line sources!
	if(iold.EQ.0 .AND. (istype.EQ.5 .OR. istype.EQ.6)) then
		! ---    Older end of slug
		! ---    Define virtuals for the "downwash sigmas" at the source
		call sigty(sy0(ii),zero,zero,syb1,vtyb1,vdyb1)
		call sigtz(sz0(ii),zero,zero,zb1,szb1,vtzb1,vdzb1)
		! ---    Sigmas and virtuals at end of step
		distkm=distm1*dm2km
		tye=vtyb1+tsamp
		dye=vdyb1+distkm
		tze=vtzb1+tsamp
		dze=vdzb1+distkm
		call sigty(zero,dye,tye,sye1,vtye1,vdye1)
		call sigtz(zero,dze,tze,ze1,sze1,vtze1,vdze1)
		! ---    Younger end of slug
		syb2=sy0(ii)
		szb2=sz0(ii)
		vtyb2=vtyb1
		vdyb2=vdyb1
		vtzb2=vtzb1
		vdzb2=vdzb1
		sye2=sy0(ii)
		sze2=sz0(ii)
		vtye2=vtyb2
		vdye2=vdyb2
		vtze2=vtzb2
		vdze2=vdzb2
	endif
	!*****
	if(ldbhr)then
		write(io6,*) 'SETSLG --'
		write(io6,260) xb2,yb2,zb2,syb2,szb2,&
		xe2,ye2,ze2,sye2,sze2
		260      format(10x,'Youngest end of slug at START of sampling step:'/&
		12x,'XB2:',f8.0,2x,'YB2:',f8.0,2x,'ZB2:',f6.1,2x,&
		'SYB2:',f10.1,2x,'SZB2:',f10.1/&
		10x,'Youngest end of slug at END of sampling step:'/&
		12x,'XE2:',f8.0,2x,'YE2:',f8.0,2x,'ZE2:',f6.1,2x,&
		'SYE2:',f10.1,2x,'SZE2:',f10.1)
		write(io6,262) xb1,yb1,zb1,syb1,szb1,&
		xe1,ye1,ze1,sye1,sze1
		262      format(10x,'Oldest end of slug at START of sampling step:'/&
		12x,'XB1:',f8.0,2x,'YB1:',f8.0,2x,'ZB1:',f6.1,2x,&
		'SYB1:',f10.1,2x,'SZB1:',f10.1/&
		10x,'Oldest end of slug at END of sampling step:'/&
		12x,'XE1:',f8.0,2x,'YE1:',f8.0,2x,'ZE1:',f6.1,2x,&
		'SYE1:',f10.1,2x,'SZE1:',f10.1)
		write(io6,264) xttb1,xtte1
		264      format(10x,'Distance travelled by OLD end of slug       :'/&
		12x,'XTTB1:',f8.0,2x,'XTTE1:',f8.0)
		write(io6,265) xttb2,xtte2
		265      format(10x,'Distance travelled by YOUNG end of slug     :'/&
		12x,'XTTB2:',f8.0,2x,'XTTE2:',f8.0)
		write(io6,*) 'Time/Distance to start of Heffter curves:'
		write(io6,*) '   THFTY,THFTZ =',thfty,thftz
		write(io6,*) 'Dispersion properties assessed at mid-pt cell'
		write(io6,*) '   ixmc,iymc,mdmc = ',ixmc,iymc,mdmc
		write(io6,*) '   yz0fac(ixmc,iymc,mdmc) = ',&
		yz0fac(ixmc,iymc,mdmc)
		write(io6,*) '   az0fac(ixmc,iymc,mdmc) = ',&
		az0fac(ixmc,iymc,mdmc)
		write(io6,*) '   bz0trm(ixmc,iymc,mdmc) = ',&
		bz0trm(ixmc,iymc,mdmc)
		write(io6,*) 'Virtuals stored in /CURRENT/'
		write(io6,*) '   VTZB1,VTZE1 = ',vtzb1,vtze1
		write(io6,*) '   VDZB1,VDZE1 = ',vdzb1,vdze1
		write(io6,*) '   VTYB1,VTYE1 = ',vtyb1,vtye1
		write(io6,*) '   VDYB1,VDYE1 = ',vdyb1,vdye1
		write(io6,*) '   VTZB2,VTZE2 = ',vtzb2,vtze2
		write(io6,*) '   VDZB2,VDZE2 = ',vdzb2,vdze2
		write(io6,*) '   VTYB2,VTYE2 = ',vtyb2,vtye2
		write(io6,*) '   VDYB2,VDYE2 = ',vdyb2,vdye2
		write(io6,*) 'SIG-Y0 squared = ',sy0sq
	endif
	!*****
	!
	return
end
!----------------------------------------------------------------------
subroutine slugi(ibsamp,iesamp,jbsamp,jesamp,meshdn,dgrid,delsam)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 921231             SLUGI
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Initialize the variables defining the sampling grid
	!               in common block /RECGRD/.  This common is used by the
	!               slug sampling function.
	!
	! --- INPUTS:
	!          IBSAMP - integer    - Element number of the met. grid
	!                                defining the beginning of the
	!                                sampling grid in the X direction
	!          JBSAMP - integer    - Element number of the met. grid
	!                                defining the beginning of the
	!                                sampling grid in the Y direction
	!          IESAMP - integer    - Element number of the met. grid
	!                                defining the end of the sampling
	!                                grid in the X direction
	!          JESAMP - integer    - Element number of the met. grid
	!                                defining the end of the sampling
	!                                grid in the Y direction
	!          MESHDN - integer    - Nesting factor of the sampling grid.
	!                                The sampling grid spacing (in meters)
	!                                is DGRID/MESHDN
	!           DGRID - real       - METOROLOGICAL grid spacing (m)
	!          DELSAM - real       - SAMPLING grid spacing (m)
	!
	! --- OUTPUT:
	!       Common block /RECGRD/ variables:
	!             All variables
	!
	! --- SLUGI called by:  SETUP
	! --- SLUGI calls:      none
	!
	!----------------------------------------------------------------------
	!
	! --- Include parameters
	include 'params.puf'
	!
	common/RECGRD/nx,dx,xleft,xrght,ny,dy,ybot,ytop
	!
	nx=meshdn*(iesamp-ibsamp)+1
	dx=delsam
	xleft=(FLOAT(ibsamp)-0.5)*dgrid
	xrght=xleft+(nx-1)*delsam
	!
	ny=meshdn*(jesamp-jbsamp)+1
	dy=delsam
	ybot=(FLOAT(jbsamp)-0.5)*dgrid
	ytop=ybot+(ny-1)*delsam
	!
	return
end
!----------------------------------------------------------------------
subroutine setup(itest)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 150918             SETUP
	!                J. Scire
	!
	! --- SETUP PHASE of model run -- perform all initialization and
	!     one-time setup operations
	!
	! --- UPDATE
	!
	! --- v7.2.1 - v7.3.0  150918
	!                                : Set road source indices for each
	!                                  variable emission file RDEMARB.DAT (Jelena)
	!                                : Add SPEMARB.DAT Spray sources (CDA)
	!                                  New calls to subroutines: RDHDEM8,RDTIEM8
	!                                  Updates to RUNSIZE,READCF,WRFILES,OPENOT,
	!                                             EMQA,WROUT1,QAPLOT1
	!
	! --- TNG-7.0.0 - TNG-7.1.0  141230  (DGS)
	!                                : Add RDEMARB.DAT Road sources
	!                                : Trap file-open error
	!
	! --- V6.302 - TNG-7.0.0  140913  (DGS)
	!                                : Add Flare source
	!                                : Add Road source
	! --- V6.302-6.42_x1.0  121203   : Pass file version to RDTIEM* subs
	!                                  as a full array, not as the current
	!                                  value
	!
	! --- V6.301-V6.302 100917  (DGS): Add auxiliary CALMET file for 3D
	!                                  cloud water
	! --- V6.3-V6.301   100827  (DGS): Allow nested CALMET files to have
	!                                  different time-periods
	! --- V6.26-V6.3    100212  (DGS): remove temp array and nx,ny from
	!                                  call to MET1 since MET1 internally
	!                                  controls inputs from nested CALMET
	!                                  (drop WRKSPC.PUF)
	!                   100212  (DGS): add SETNEST call
	! --- V6.01-V6.26   080430  (DGS): add MRISE filter to NUMPR1 call
	! --- V5.75-V6.01   050915  (DGS): add call to MET4PROF and full
	!                                  begin/end time structures
	!                           (DGS): convert PT2 coordinates to model
	!                                  Map projection and DATUM
	! --- V5.74-V5.75   050225  (DGS): add platform downwash to RDTIEM2
	! --- V5.72-V5.74   040715  (DGS): add METFM=5 (AERMET)
	! --- V5.71-V5.72   031017  (DGS): modify call to WROUT1 for source
	!                                  contribution option (MSOURCE)
	!                   031017  (DGS): process source location data from
	!                                  variable emissions files (RDEMSRC)
	!                   031017  (DGS): add IQAPLOT control option
	! --- V5.7-V5.71    030528  (DGS): MBCON=2 option for using CONC.DAT
	!                                  as BCON.DAT file
	! --- V5.4-V5.7     030402  (DGS): move METLATLON subroutine here
	!                   030402  (DGS): add structures to pass control file
	!                                  image and other inputs to header of
	!                                  output files
	!                   030402  (DGS): add PRIME (building downwash) inputs
	! --- V5.4-V5.4     000602_5(DGS): add QA plot file subroutine
	! --- V5.4-V5.4     000602_3(DGS): add aqueous phase chemistry
	! --- V5.3-V5.4     000602  (DGS): NVOLDAT replaces IGRDVL for 
	!                                  multiple VOLEMARB.DAT files
	!                   000602  (DGS): allow multiple PTEMARB.DAT and
	!                                  BAEMARB.DAT files
	!                   000602  (DGS): allow units choice in BAEMARB.DAT
	!                   000602  (DGS): set up FOG Model output info
	! --- V5.2-V5.3     991222  (DGS): add boundary condition setup
	! --- V5.0-V5.2     991104  (DGS): add DSRISE to NUMPR1 call to pass
	!                                  control file variable into /numparm/
	! --- V5.0-V5.0     990228d (DGS): add mass balance setup
	!                   990228d (DGS): add banner to mass flux and mass
	!                                  balance list files
	! --- V5.0-V5.0     990228c (DGS): add mass flux setup
	! --- V5.0-V5.0     980807  (DGS): add MFVL2 for VOLEM.DAT format
	! --- V5.0-V5.0     980515  (DGS): add setup for sub-grid TIBL module
	! --- V5.0-V5.0     980304  (DGS): add QA of RESTART file
	! --- V4.0-V5.0     971107  (DGS): write errors to screen if found
	!                                  before list file is opened.
	!                   971107  (DGS): accept start-time from met file
	!                                  for METRUN=1
	!                   971107  (DGS): allow for binary of ASCII variable
	!                                  emissions files
	!                   971107  (JSS): add METFM to NUMPR1 call to allow
	!                                  use of all met formats
	!                   971107  (DGS): add call to TRELIEF and VWIDTH to set
	!                                  up terrain relief and valley widths
	!                                  used for MCTADJ=2
	!                   971107  (DGS): add calls to process LNEMARB.DAT
	!
	! --- OUTPUT:
	!              ITEST - integer - Flag indicating if execution is to
	!                                proceed beyond SETUP phase
	!                                (ITEST = 1 to STOP program after
	!                                           SETUP phase,
	!                                 ITEST = 2 to CONTINUE execution)
	!
	!     Common block /OUTPT/
	!        IMESG, IOMESG, LDEBUG
	!     Common block /PT2/
	!        MFPT2(mxemdat), TIEM2(7,mxpt2), BHT2(36,mxpt2), BWD2(36,mxpt2)
	!        BLN2(36,mxpt2), XBADJ2(36,mxpt2), YBADJ2(36,mxpt2),
	!        ZPLATPT2(mxpt2)
	!     Common block /VOL2/
	!        MFVL2(mxemdat)
	!     Common block /FL2/
	!        TIEM6(8,mxflr2), MFFL2(mxemdat)
	!     Module |MROAD2| variables:
	!           nrddat,rddat(nrddat),nrdseg2b(nrddat),nrdseg2e(nrddat)
	!     Module |MSPRAY2| variables:
	!           nspdat,spdat(nspdat)
	!
	! --- Parameters used:
	!        MXMAIN, MXARR, MXSPEC, MXNZ, MXNZP1, MXPT2,
	!        MXEMDAT, IO6, IOX, IOPT2, IOAR2, IOVOL
	!        mxp6, mxp14, mxarea, mxvol, mxlines, mxlngrp
	!        mxp11, mxflr2
	!
	! --- SETUP called by: MAIN
	! --- SETUP calls:     DATETM,  COMLINE, READFN,  READCF,  TIMESET,
	!                      WRFILES, SETCOM,  OPENOT,  RDHDEM2, RDHDEM3,
	!                      RDHDEM4, RDHDEM5, RDTIEM2, RDTIEM3, RDTIEM5,
	!                      RDHDEM6, RDTIEM6, RDEMSRC, RDHDEM7, RDTIEM7,
	!                      RDHDEM8, RDTIEM8
	!                      CHEMI,   EMQA,
	!                      MET1,    MET2,    MET3,    MET4,   MET4PROF,
	!                      RDHDTVW, SIGSET,
	!                      SLUGI,   DRYI,    CTINIT,  WROUT1,  ELEVI,
	!                      NUMPR1,  TRELIEF, RESTARTQ,TIBLSET,
	!                      MFLXSET, MFLXHDR, MBALHDR, WRIVL,
	!                      RDHDBC,  RDHDBC2, SETFOG,  QAPLOT1, METLATLON,
	!                      TFERCF,  MET1DOC, SETNEST,
	!                      AUX1,    RUNSIZE, ROADSEG, OPEN_ERR
	!----------------------------------------------------------------------
	! --- Modules
	use mroad2
	use mspray2
	! --- Include parameters
	include 'params.puf'
	! --- Include common blocks
	include 'ar2.puf'
	include 'comparm.puf'
	include 'ctsgdat.puf'
	include 'dataset.puf'
	include 'drydep.puf'
	include 'filnam.puf'
	include 'flags.puf'
	include 'gen.puf'
	include 'grid.puf'
	include 'ln2.puf'
	include 'map.puf'
	include 'methd.puf'
	include 'outpt.puf'
	include 'pt2.puf'
	include 'fl2.puf'
	include 'qa.puf'
	include 'vol2.puf'
	!
	character*80 title(3)
	logical LPRT,ldb,lnpt2,lnar2,lnvl2,lnfl2,lnrd2,lnsp2
	! --- Time-variable emission arrays (declare full dimensions here)
	real em4dat(mxp6,mxvol)
	real em3dat(mxp14,mxarea)
	real em5dat(mxp6,mxlines),em5grp(7,mxlngrp)
	real em6dat(mxp11,mxfl2)
	! --- For coordinate transformations
	character*8 cmapi,cmapo
	character*12 caction
	character*4 c4hem
	real*8 vecti(9),vecto(9)
	! --- Get date and time from system
	call DATETM(rdate,rtime,rcpu)
	! --- Get the name of the control file from the command line
	call COMLINE(pufinp)
	! --- Open the control file
	open(io5,file=pufinp,status='old',iostat=ierr)
	if(ierr.NE.0) call OPEN_ERR(-1,'SETUP','Control File',&
		pufinp,io5)
	! --- Report progress
	write(iomesg,*)'SETUP PHASE'
	! --- Check that the version and level number in the parameter
	! --- file matches those in the code itself
	if(ver.ne.mver.or.level.ne.mlevel)then
		write(iomesg,10)mmodel,ver,level,mver,mlevel
		10       format(/1x,'ERROR in SUBR. SETUP -- The ',a12,' version ',&
		'and level numbers do not match those in the parameter file'/&
		5x,'    Model Code - Version: ',a12,' Level: ',a12/&
		5x,'Parameter File - Version: ',a12,' Level: ',a12)
		stop
	endif
	! --- Skim the control file for run configuration and allocate arrays
	call RUNSIZE(io5,iomesg,mxspec,mxqsf,mxqstep)
	! --- Read control file inputs
	call READCF(title,itest)
	! --- Pass image of CALPUFF control file to scratch file
	call TFERCF
	! --- Assign debug logical for local use
	ldb=ldebug
	! --- Write the files used in this run to the list file
	call WRFILES(npt2,nar2,nln2,nvl2,nfl2,idryflg,mhill)
	! --- Set misc. common block parameters (grid parameters, etc.)
	call SETCOM
	! --- Pass CALMET.DAT control file images to scratch file
	! --- (Open, process, and close CALMET files)
	if(metfm.EQ.1) call MET1DOC
	! --- Establish nested met grid information
	! --- (Open, process, and close CALMET files)
	call SETNEST
	! --- Open all other I/O files
	call OPENOT(nln2,idryflg,mhill,mfpt2,mfvl2,mffl2)
	! --- Read header records from meteorological data file(s)
	lprt=ldebug
	!      lprt=.true.
	if(metfm.EQ.1) then
		do kg=1,nmetdom
			call MET1(kg,lprt)
			if(mlwc.EQ.1) call aux1(kg,lprt)
		enddo
	elseif(metfm.EQ.2) then
		call MET2(lprt)
	elseif(metfm.EQ.3) then
		call MET3(lprt)
	elseif(metfm.EQ.4 .OR. metfm.EQ.5) then
		call MET4(metfm,lprt)
	endif
	! --- Transfer beginning/end time of met data to run-control variables
	! --- if entire met file is to be processed (METRUN=1)
	if(metrun.EQ.1) then
		ibyr=ibymet(1)
		ibmo=ibmmet(1)
		ibdy=ibdmet(1)
		ibhr=ibhmet(1)
		ibsec=ibsmet(1)
		ibdathr=ibymet(1)*100000+ibjdmet(1)*100+ibhmet(1)
		iesec=iesmet(1)
		iedathr=ieymet(1)*100000+iejdmet(1)*100+iehmet(1)
		! ---    Compute run length to end of met file
		! ---    No. hours
		call DELTT(ibymet(1),ibjdmet(1),ibhmet(1),&
		ieymet(1),iejdmet(1),iehmet(1),idelhr)
		! ---    Adjust for seconds
		idelsec=-ibsmet(1)+iesmet(1)
		! ---    Timesteps
		irlg=(idelhr*3600)/nsecdt+(idelsec/nsecdt)
		write(io6,*)
		write(io6,*)
		write(io6,*)'---------------------------------------------'
		write(io6,*)
		write(io6,*)'           REVISED CONTROL DATA  '
		write(io6,*)'         Running All Met Periods '
		write(io6,*)
		write(io6,*)'-------------  INPUT GROUP 1  ---------------'
		write(io6,*)
		write(io6,*)'metrun  = ',metrun
		write(io6,*)'ibyr    = ',ibyr
		write(io6,*)'ibmo    = ',ibmo
		write(io6,*)'ibdy    = ',ibdy
		write(io6,*)'ibhr    = ',ibhr
		write(io6,*)'ibsec   = ',ibsec
		write(io6,*)'nsecdt  = ',nsecdt
		write(io6,*)'irlg    = ',irlg
		write(io6,*)'ibdathr = ',ibdathr
		write(io6,*)'iedathr = ',iedathr
		write(io6,*)'iesec   = ',iesec
		write(io6,*)
		write(io6,*)'(End-times in other data files are NOT checked)'
		write(io6,*)
		write(io6,*)'---------------------------------------------'
	endif
	! --- Set starting time variables
	call TIMESET
	! --- Set PROFILE.DAT format if used for turbulence or inversion
	!     strength data ONLY
	if(metfm.NE.4 .AND. metfm.NE.5) then
		if(mtinv.EQ.1) then
			call MET4PROF(lprt)
		elseif((mdisp.EQ.1 .OR. mdisp.EQ.5) .AND. mturbvw.LT.4) then
			call MET4PROF(lprt)
		endif
	endif
	! --- Read header records from the emissions files:
	!        PTEMARB.DAT - iopt2- Arbitrarily-varying pt. source emissions
	!        FLEMARB.DAT - iofl2- Variable FLARE emissions
	!        BAEMARB.DAT - ioar2- Variable buoyant area source emissions
	!        VOLEMARB.DAT- iovol- Variable volume source emissions
	!        LNEMARB.DAT - io19 - Variable buoyant line source emissions
	!        RDEMARB.DAT - iord2- Variable ROAD emissions
	!        SPEMARB.DAT - iosp2- Variable SPRAY emissions
	! --- and Pass images to scratch file
	lprt=.true.
	lnpt2=.true.
	lnar2=.true.
	lnvl2=.true.
	lnfl2=.true.
	lnrd2=.true.
	lnsp2=.true.
	! --- Set CALPUFF map projection for any coordinate transformations
	! --- Scale factor for Tangential TM projection
	tmsone=1.00000
	! --- Output of coord transform is to CALPUFF (x,y) - /MAP/
	iutmo=iutmzn
	if(utmhem.EQ.'S   ' .AND. iutmo.LT.900) iutmo=-iutmo
	cmapo=pmap
	if(cmapo.EQ.'TTM     ') cmapo='TM      '
	! --- Points
	do iem=1,nptdat
		call RDHDEM2(iopt2,iem,lprt,lnpt2)
		! --- V6.42_x1.0, Level 121203
		call RDTIEM2(iopt2,npt2,nptdat,iem,mfpt2,ibsrc2,iesrc2,&
		verparb,mbdw,lprt,io6,iox,tiem2,zplatpt2,&
		bht2,bwd2,bln2,xbadj2,ybadj2,cid2,ncommout)
	enddo
	! -------------------------------------------- PTEMARB Source Locations
	! --- Convert the source coordinates to the CALPUFF map projection
	! --- and datum, and then convert from km to MET GRID
	! --- Input is from PTEMARB.DAT header - /PT2/
	! --- Loop over PTEMARB files
	do iem=1,nptdat
		iutmi=iutmznpt2(iem)
		if(utmhempt2(iem).EQ.'S   ' .AND. iutmi.LT.900) iutmi=-iutmi
		cmapi=pmappt2(iem)
		if(cmapi.EQ.'TTM     ') cmapi='TM      '
		! ---    Set conversion vectors
		call GLOBE1(cmapi,iutmi,tmsone,rnlat1pt2(iem),rnlat2pt2(iem),&
		rnlat0pt2(iem),relon0pt2(iem),&
		feastpt2(iem),fnorthpt2(iem),&
		cmapo,iutmo,tmsone,xlat1,xlat2,rnlat0,relon0,&
		feast,fnorth,&
		caction,vecti,vecto)
		do is=ibsrc2(iem),iesrc2(iem)
			xpt2=tiem2(1,is)
			ypt2=tiem2(2,is)
			call GLOBE(io6,caction,datumpt2(iem),vecti,datum,&
			vecto,xpt2,ypt2,tiem2(1,is),tiem2(2,is),&
			izone,c4hem)
			tiem2(1,is)=(1000.*tiem2(1,is)-xorig)*dgridi
			tiem2(2,is)=(1000.*tiem2(2,is)-yorig)*dgridi
		enddo
	enddo
	! -------------------------------------------- PTEMARB Source Locations
	! --- Areas
	do iem=1,nardat
		call RDHDEM3(ioar2,iem,lprt,lnar2)
		! --- V6.42_x1.0, Level 121203
		call RDTIEM3(ioar2,nar2,nardat,iem,ibsrc3,iesrc3,iuem3,&
		veraarb,lprt,io6,iox,cid3,baemunit,ncommout)
	enddo
	! --- Volumes
	do iem=1,nvoldat
		call RDHDEM4(iovol,iem,lprt,lnvl2)
		! --- V6.42_x1.0, Level 121203
		call RDTIEM4(iovol,nvl2,nvoldat,iem,ibsrc4,iesrc4,&
		vervarb,lprt,io6,iox,cid4,ncommout)
	enddo
	! --- Lines
	if(nln2.gt.0)then
		call RDHDEM5(io19,lprt)
		call RDTIEM5(io19,mxrise,nln2,lprt,io6,iox,cid5,ncommout,&
		mxnseg2,nlrise2)
	endif
	! --- Flares
	do iem=1,nfldat
		call RDHDEM6(iofl2,iem,lprt,lnfl2)
		call RDTIEM6(iofl2,nfl2,nfldat,iem,ibsrc6,iesrc6,&
		verflarb,lprt,io6,iox,tiem6,cid6,ncommout)
	enddo
	! --- Roads
	! --- Open scratch file for processing road segment coordinates
	if(nrd2.GT.0) OPEN(iordx,form='unformatted',status='scratch')
	nrdseg2=0
	! --- Road indices allocation and initialization, v7.2.2
	ALLOCATE(nrdseg2b(nrddat),nrdseg2e(nrddat))
	nrdseg2b=0
	nrdseg2e=0
	do iem=1,nrddat
		call RDHDEM7(iord2,iem,lprt,lnrd2)
		! ---    Set up coordinate transformation for this file
		iutmi=iutmznrd2(iem)
		if(utmhemrd2(iem).EQ.'S   ' .AND. iutmi.LT.900) iutmi=-iutmi
		cmapi=pmaprd2(iem)
		if(cmapi.EQ.'TTM     ') cmapi='TM      '
		! ---    Set conversion vectors
		call GLOBE1(cmapi,iutmi,tmsone,rnlat1rd2(iem),rnlat2rd2(iem),&
		rnlat0rd2(iem),relon0rd2(iem),&
		feastrd2(iem),fnorthrd2(iem),&
		cmapo,iutmo,tmsone,xlat1,xlat2,rnlat0,relon0,&
		feast,fnorth,caction,vecti,vecto)
		call RDTIEM7(iord2,nrd2,nrddat,iem,ibsrc7,iesrc7,&
		verrdarb,lprt,io6,iox,&
		caction,datumrd2(iem),vecti,datum,vecto,&
		nrdseg2,cid7,ncommout)
		! ---    Set road source index range for the current file, L150817
		if(iem.eq.1) then
			nrdseg2b(iem)=1
			nrdseg2e(iem)=nrdseg2
		else
			nrdseg2b(iem)=nrdseg2e(iem-1)+1
			nrdseg2e(iem)=nrdseg2
		endif
	enddo
	! --- Process scratch file (conditional)
	if(nrdseg2.GT.0) then
		call ROADSEG(2)
		if(lnrd2) then
			write(io6,*)
			write(io6,*)'Road segments for generating emissions'
			write(io6,*)'Road                x(grd)      y(grd)'//&
			'      z(mMSL) to      x(grd)      y(grd)'//&
			'      z(mMSL)   Length(m)'
			do k=1,nrdseg2
				write(io6,'(a16,2f12.5,f12.2,4x,2f12.5,2f12.2)')&
				cid7(iroad2(k)),xrd2grd(1,k),&
				yrd2grd(1,k),elrd2(1,k),xrd2grd(2,k),&
				yrd2grd(2,k),elrd2(2,k),rdlen2(k)
			enddo
		endif
	endif
	! --- Sprays
	nspseg2=0
	do iem=1,nspdat
		call RDHDEM8(iosp2,iem,lprt,lnsp2)
		! ---    Set up coordinate transformation for this file
		iutmi=iutmznsp2(iem)
		if(utmhemsp2(iem).EQ.'S   ' .AND. iutmi.LT.900) iutmi=-iutmi
		cmapi=pmapsp2(iem)
		if(cmapi.EQ.'TTM     ') cmapi='TM      '
		! ---    Set conversion vectors
		call GLOBE1(cmapi,iutmi,tmsone,rnlat1sp2(iem),rnlat2sp2(iem),&
		rnlat0sp2(iem),relon0sp2(iem),&
		feastsp2(iem),fnorthsp2(iem),&
		cmapo,iutmo,tmsone,xlat1,xlat2,rnlat0,relon0,&
		feast,fnorth,caction,vecti,vecto)
		call RDTIEM8(iosp2,nsp2,nspdat,iem,ibsrc8,iesrc8,&
		versparb,lprt,io6,iox,&
		nspseg2,cid8,ncommout)
	enddo
	! --- Make sure enough sources are read from the emissions files
	if(.NOT.lnpt2) then
		write(io6,11) npt2
		11       format(/1x,'ERROR in SUBR. SETUP -- number of sources in',&
		' PTEMARB.DAT files does not match expected value',i6)
		write(*,*)
		stop 'Halted in SETUP -- see list file.'
	endif
	if(.NOT.lnar2) then
		write(io6,12) nar2
		12       format(/1x,'ERROR in SUBR. SETUP -- number of sources in',&
		' BAEMARB.DAT files does not match expected value',i6)
		write(*,*)
		stop 'Halted in SETUP -- see list file.'
	endif
	if(.NOT.lnvl2) then
		write(io6,13) nvl2
		13       format(/1x,'ERROR in SUBR. SETUP -- number of sources in',&
		' VOLEMARB.DAT files does not match expected value',i6)
		write(*,*)
		stop 'Halted in SETUP -- see list file.'
	endif
	if(.NOT.lnfl2) then
		write(io6,14) nfl2
		14       format(/1x,'ERROR in SUBR. SETUP -- number of sources in',&
		' FLEMARB.DAT files does not match expected value',i6)
		write(*,*)
		stop 'Halted in SETUP -- see list file.'
	endif
	if(.NOT.lnrd2) then
		write(io6,15) nrd2
		15       format(/1x,'ERROR in SUBR. SETUP -- number of sources in',&
		' RDEMARB.DAT files does not match expected value',i6)
		write(*,*)
		stop 'Halted in SETUP -- see list file.'
	endif
	! --- Read header records from the boundary condition file:
	! --- BCON.DAT - io15 - boundary concentrations,
	! --- and perform setup operations
	if(mbcon.eq.1) call RDHDBC
	if(mbcon.eq.2) call RDHDBC2(io15)
	! --- Perform setup operations for chemical transformation module
	if(mchem.ge.1)call CHEMI(mchem,maqchem,ldb)
	! --- Perform QA checks on emissions header record data
	lprt=.TRUE.
	call EMQA(lprt)
	! --- Pull source location data from the first set of time-variable
	! --- records for area, volume, and line sources and reposition
	! --- file pointers (source locations are needed for output files)
	call RDEMSRC(nardat,nvoldat,nfldat,em3dat,em4dat,em5dat,em5grp,&
	em6dat)
	! --- Interpolate elevations from meteorological grid to gridded recs.
	if(lsamp) then
		call ELEVI
	endif
	! --- Perform setup operations for the dispersion coefficient routines
	call SIGSET
	! --- Setup the sampling grid for the SLUG function
	if(lsamp) then
		call SLUGI(ibsamp,iesamp,jbsamp,jesamp,meshdn,dgrid,delsam)
	else
		! ---    Gridded receptors are not used in run, so define slug domain
		! ---    as the computational domain
		call SLUGI(ibcomp,iecomp,jbcomp,jecomp,1,dgrid,dgrid)
	endif
	! --- Perform setup computations for the FOG Model output option
	if(mfog.GT.0) call SETFOG(title(1))
	! --- Perform setup operations for dry deposition
	!      zgrid1=0.5*(zface(1)+zface(2))
	!      if(mdry.eq.1)call DRYI(zgrid1,ldb)
	! --- Perform setup computations for CTADJ2 complex terrain
	! --- module (simplified CALPUFF treatment)
	if(mctadj.eq.2)then
		call TRELIEF(ldb)
		call VWIDTH(ldb)
	endif
	! --- Perform setup computations for sub-grid scale complex terrain
	! --- module (CTSG)
	if(mctsg.gt.0)then
		call CTINIT(dgrid,mhill,nhill,hilldat,nctrec,xrct,yrct)
	endif
	! --- Perform setup computations for the numerical plume rise algorithm
	! --- (either for buoyant areas, or points with numerical rise or PRIME)
	if(nar2.GT.0  .OR. mbdw.EQ.2 .OR.&
		mrise.EQ.2 .OR. mrise_fl.EQ.2) then
		call NUMPR1(nz,zgpt,nzp1,zface,metfm,dsrise,trajincl)
	endif
	! --- Check (QA) information in RESTART file for continuation run
	if(mrestart.EQ.1 .OR. mrestart.EQ.3) call RESTARTQ
	! --- Perform setup operations for the sub-grid TIBL module
	if(msgtibl.EQ.1) call TIBLSET(ldb)
	! --- Perform setup operations for the mass flux output option
	if(imflx.EQ.1) then
		call WRIVL(io36,mmodel,ver,level)
		call MFLXSET(ldb)
		call MFLXHDR(title)
	endif
	! --- Perform setup operations for the mass balance output option
	if(imbal.EQ.1) then
		call WRIVL(io37,mmodel,ver,level)
		call MBALHDR(title,mbcon)
	endif
	! --- Compute latitude/longitude of all MET gridpoints
	call METLATLON
	! --- Write header records to output disk files
	call WROUT1(title)
	! --- Write QA information to plot files
	if(iqaplot.EQ.1) call QAPLOT1
	return
end
!----------------------------------------------------------------------
subroutine openot(nln2,idryflg,mhill,mfpt2,mfvl2,mffl2)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 141230            OPENOT
	!                J. Scire, D. Strimaitis
	!
	! --- PURPOSE:  Open all input/output files other than input control
	! ---           file (IO5) and output list file (IO6)
	!
	! --- UPDATE
	! --- v7.2.1 - v7.3.0  150717  (CDA)
	!                                : add SPEMARB.DAT (sprays)
	! --- TNG-7.0.0 - TNG-7.1.0  141230  (DGS)
	!                                : add RDEMARB.DAT (roads)
	!                                : trap file-open error
	! --- V6.302 - TNG-7.0.0  140913  (DGS)
	!                                : add FLEMARB.DAT (flares)
	! --- V6.3-V6.302   100917  (DGS): add MCHEM=6,7
	!                           (DGS): add AUX file for MLWC=1
	! --- V6.267-V6.3   100212  (DGS): add nested CALMET grid
	! --- V6.261-V6.267 090710  (DGS): add PFTRAK.DAT file for puff-
	!                                  tracking output
	! --- V6.26-V6.261  080520  (DGS): add scratch file for direct access
	!                                  to store source tables (rise)
	! --- V6.1-V6.26    080430  (DGS): add RISE.DAT for NUMRISE output
	! --- V5.74-V6.1    050915  (DGS): add FILVERS for dataset versions
	!                   050915  (DGS): expand date-time in TRACK header
	! --- V5.71-V5.725  050128  (DGS): add TK2D.DAT for 2D temperature
	!                   050128  (DGS): add RHO2D.DAT for 2D density
	! --- V5.71-V5.74   040715  (DGS): add METFM=5 (AERMET)
	! --- V5.7-V5.71    030528  (DGS): MBCON=2 opens a binary CONC.DAT file
	! --- V5.4-V5.7     030402  (DGS): replace OPENAB with explicit ASCII
	!                                  file assumption
	! --- V5.4-V5.4     000602_3(DGS): add H2O2.DAT for aqueous chemistry
	! --- V5.3-V5.4     000602  (DGS): NVOLDAT replaces IGRDVL, and
	!                                  open multiple VOLEMARB.DAT files
	!                   000602  (DGS): open multiple PTEMARB.DAT and
	!                                  BAEMARB.DAT files
	!                   000602  (DGS): include FOG.DAT file
	! --- V5.2-V5.3     991222  (DGS): add BCON.DAT
	! --- V5.0-V5.2     991104  (DGS): YYYY for year in TRACK header
	! --- V5.0-V5.0     990228d (DGS): add MASSBAL.DAT
	! --- V5.0-V5.0     990228c (DGS): add FLUXBDY.DAT and MASSFLX.DAT
	! --- V5.0-V5.0     990228a (DGS): allow array of CALMET.DAT filenames
	! --- V5.0-V5.0     980918  (DGS): include MCHEM=4
	! --- V5.0-V5.0     980807  (DGS): allow either unformatted of formatted
	!                                  Variable emissions file for VOLUMES
	! --- V5.0-V5.0     980731  (DGS): augment output to DEBUG.DAT
	! --- V5.0-V5.0     980515  (DGS): add COASTLN.DAT file
	! --- V5.0-V5.0     980304  (DGS): add RESTART file
	! --- V4.0-V5.0     971107  (DGS): add LNEMARB.DAT file
	! --- V4.0-V4.1     971107  (DGS): allow either unformatted of formatted
	!                                  Variable emissions file for POINTS
	!                   971107  (DGS): open PROFILE.DAT when MTINV=1 for
	!                                  getting inversion strength
	!
	! --- INPUTS:
	!               NLN2 - integer - Number of buoyant line sources with
	!                                variable location and emissions
	!                                (LNEMARB.DAT - arbitrary emissions)
	!     IDRYFLG(mxspec) - integer - Array of dry deposition flags for
	!                       array     each pollutant
	!                                   0 = No deposition
	!                                   1 = Resistance model - gas
	!                                   2 = Resistance model - particle
	!                                   3 = User-specified dep. velocities
	!               MHILL - integer - Flag controlling use of CTDM-format
	!                                 hill & receptor information (for CTSG)
	!                                   0 = No file (CTSG option not used)
	!                                   1 = HILL.DAT and HILLRCT.DAT files
	!                                   2 = No file (hill data from OPTHILL
	!                                       are supplied in Subgroup 6b, and
	!                                       CTSG receptors are supplied in
	!                                       Subgroup 6c)
	!
	!     Common block /CHEMDAT/ variables:
	!           MOZ, MNH3
	!     Common block /FLAGS/ variables:
	!           MCHEM, MDRY, MDISP, MCTSG, MTURBVW, MTINV, MSGTIBL, MBCON,
	!           MAQCHEM, MLWC
	!     Common block /GEN/ variables:
	!           METFM, MRESTART
	!     Common block /GRIDNEST/ variables:
	!           NGRID
	!     Common block /OUTPT/ variables:
	!           ICON, IVIS, IT2D, IRHO, IDRY, IWET, IMFLX, IMBAL, IFOG,
	!           INRISE, IPFTRAK
	!           LDEBUG
	!     Common block /FILNAM/ variables:
	!           METDATL, ISCDAT, PLMDAT, CONDAT, DFDAT, WFDAT, VISDAT,
	!           T2DDAT, RHODAT, PTDAT(mxemdat), ARDAT(mxemdat),
	!           VOLDAT(mxemdat), LNDAT, VDDAT, OZDAT, CHEMDAT, HILDAT,
	!           RCTDAT, PRFDAT, SFCDAT, RSTARTB, CSTDAT, BDTDAT, FLXDAT,
	!           BALDAT, DEBUG, NPTDAT, NARDAT, NVOLDAT, FOGDAT, H2O2DAT,
	!           RISDAT, TRKDAT, AUXEXT, NFLDAT, FLDAT(mxemdat)
	!     Module |MROAD2| variables:
	!           nrddat,rddat(nrddat)
	!     Module |MSPRAY2| variables:
	!           nspdat,spdat(nspdat)
	!
	!     Parameters: IO3, IO6, IO7, IO8, IO9, IO10, IO11, IO12, IO13, IO14,
	!                 IO15, IO19 IO20, IO22, IO23, IO24, IO25, IO28, IO29,
	!                 IO30, IO31, IO32, IO35, IO36, IO37, IO38, IO40, MXSPEC,
	!                 IOPT2, IOAR2, IOVOL, IOFL2, IORD2,
	!                 MXEMDAT, MXMETDAT, MXMETDOM,
	!                 IOTAB, IOTRK
	!
	! --- OUTPUT:
	!  MFPT2(mxemdat) - integer    - Formatted PTEMARB.DAT file?
	!                                 (0:NO, 1:YES)
	!  MFVL2(mxemdat) - integer    - Formatted VOLEMARB.DAT file?
	!                                 (0:NO, 1:YES)
	!  MFFL2(mxemdat) - integer    - Formatted FLEMARB.DAT file?
	!                                 (0:NO, 1:YES)
	!                                 (Only formatted file supported)
	!  MFRD2(mxemdat) - integer    - Formatted RDEMARB.DAT file?
	!                                 (0:NO, 1:YES)
	!                                 (Only formatted file supported)
	!     Module |MROAD2| variables:
	!           mfrd2(nrddat),verrdarb(nrddat)
	!     Module |MSPRAY2| variables:
	!           mfsp2(nspdat),versparb(nspdat)
	!
	!     Common block /DATASET/ variables:
	!           vermet,verisc,verplm,verprf,versfc
	!           verparb(mxemdat),veraarb(mxemdat),verlarb
	!           vervarb(mxemdat),verflarb(mxemdat)
	!           veroz,verh2o2,vercoast,verflxb,verbcon,verrest
	!
	! --- OPENOT called by:  SETUP
	! --- OPENOT calls:      FILVERS, OPEN_ERR, (OPENAB)
	!----------------------------------------------------------------------
	! --- Modules
	use mroad2
	use mspray2
	! --- Include parameter statements
	include 'params.puf'
	! --- Include common blocks
	include 'chemdat.puf'
	include 'dataset.puf'
	include 'filnam.puf'
	include 'flags.puf'
	include 'gen.puf'
	include 'gridnest.puf'
	include 'outpt.puf'
	!
	integer idryflg(mxspec)
	integer mfpt2(mxemdat),mfvl2(mxemdat)
	integer mffl2(mxemdat)
	!
	character*7 cstat
	!
	logical lprofile
	!
	data cstat/'unknown'/, lprofile/.FALSE./
	! --- Open Restart file (may or may not exist)
	if(mrestart.EQ.1 .OR. mrestart.EQ.3) then
		call FILVERS(io3,'BIN',rstartb,'RESTART         ',verrest)
		open(io3,file=rstartb,form='unformatted',status='old',&
		iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','Restart File',&
			rstartb,io3)
	endif
	!
	! --- Open Primary met file
	if(metfm.EQ.1) then
		! ---    open CALMET meteorological data file (CALMET.DAT) for each
		! ---    CALMET grid
		do k=1,nmetdom
			iomet=io7+(k-1)
			call FILVERS(iomet,'BIN',metdatl(1,k),'CALMET          ',&
			vercmet(k))
			open(iomet,file=metdatl(1,k),status='old',&
			form='unformatted',iostat=ierr)
			if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','CALMET File',&
				metdatl(1,k),iomet)
			ioaux=io7+(k-1)+ngrid
				if(mlwc.EQ.1)&
			open(ioaux,file=TRIM(metdatl(1,k))//TRIM(auxext),&
			status='old',form='unformatted',iostat=ierr)
			if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','AUX File',&
				TRIM(metdatl(1,k))//TRIM(auxext),ioaux)
		enddo
	elseif(metfm.EQ.2) then
		! ---    open ISC meteorological data file (ISCMET.DAT)
		call FILVERS(io7,'ASC',iscdat,'ISCMET          ',verisc)
		open(io7,file=iscdat,status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','ISCMET File',&
			iscdat,io7)
	elseif(metfm.EQ.3) then
		! ---    open PLM meteorological data file (PLMMET.DAT)
		call FILVERS(io7,'ASC',plmdat,'PLMMET          ',verplm)
		open(io7,file=plmdat,status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','PLMMET File',&
			plmdat,io7)
	elseif(metfm.EQ.4 .OR. metfm.EQ.5) then
		! ---    open PROFILE meteorological data file (PROFILE.DAT)
		call FILVERS(io31,'ASC',prfdat,'PROFILE         ',verprf)
		open(io31,file=prfdat,status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','PROFILE File',&
			prfdat,io31)
		lprofile=.TRUE.
		! ---    open SURFACE meteorological data file (SURFACE.DAT)
		call FILVERS(io32,'ASC',sfcdat,'SURFACE         ',versfc)
		open(io32,file=sfcdat,status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','SURFACE File',&
			sfcdat,io32)
	endif
	!
	! --- Open PROFILE.DAT if inversion strength data are needed
	if(.not.lprofile .AND. mtinv.EQ.1) then
		call FILVERS(io31,'ASC',prfdat,'PROFILE         ',verprf)
		open(io31,file=prfdat,status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','PROFILE File',&
			prfdat,io31)
	endif
	!
	! --- open output concentration file (CONC.DAT)
	if(ICON.eq.1)open(io8,file=condat,status=cstat,&
		form='unformatted')
	!
	! --- open output dry flux file (DFLX.DAT)
	if(IDRY.eq.1)open(io9,file=dfdat,status=cstat,&
		form='unformatted')
	!
	! --- open output wet flux file (WFLX.DAT)
	if(IWET.eq.1)open(io10,file=wfdat,status=cstat,&
		form='unformatted')
	!
	! --- open visibility-related file (VISB.DAT)
	if(IVIS.eq.1)open(io11,file=visdat,status=cstat,&
		form='unformatted')
	!
	! --- open 2D temperature file (TK2D.DAT)
	if(IT2D.eq.1)open(io13,file=t2ddat,status=cstat,&
		form='unformatted')
	!
	! --- open 2D density file (RHO2D.DAT)
	if(IRHO.eq.1)open(io14,file=rhodat,status=cstat,&
		form='unformatted')
	!
	! --- open BOUNDARY CONCENTRATION file (BCON.DAT)
	if(mbcon.eq.1) then
		call FILVERS(io15,'ASC',bcndat,'BCON            ',verbcon)
		open(io15,file=bcndat,status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','BCON File',&
			bcndat,io15)
	elseif(mbcon.eq.2) then
		call FILVERS(io15,'BIN',bcndat,'BCON            ',verbcon)
		open(io15,file=bcndat,status='old',form='unformatted',&
		iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','BCON File',&
			bcndat,io15)
	endif
	!
	! --- open output fog-model file (FOG.DAT)
	if(IFOG.eq.1)open(io12,file=fogdat,status=cstat,&
		form='unformatted')
	!
	! --- open POINT SOURCE emissions file(s) (PTEMARB.DAT)
	! --- (stationary point sources with arbitrary variation in emissions)
	do i=1,nptdat
		io=iopt2+i-1
		! ---    Assume ASCII file (formatted)
		!        call OPENAB(io,ptdat(i),mfpt2(i))
		call FILVERS(io,'ASC',ptdat(i),'PTEMARB         ',verparb(i))
		mfpt2(i)=1
		open(io,file=ptdat(i),status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','PTEMARB File',&
			ptdat(i),io)
	enddo
	!
	! --- open BUOYANT AREA SOURCE file(s)  (BAEMARB.DAT)
	! --- (area sources with arbitrary variation in location & emissions)
	do i=1,nardat
		io=ioar2+i-1
		call FILVERS(io,'ASC',ardat(i),'BAEMARB         ',veraarb(i))
		open(io,file=ardat(i),status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','BAEMARB File',&
			ardat(i),io)
	enddo
	!
	! --- open buoyant LINE SOURCE file  (LNEMARB.DAT)
	! --- (line sources with arbitrary variation in location & emissions)
	if(nln2.gt.0) then
		call FILVERS(io19,'ASC',lndat,'LNEMARB         ',verlarb)
		open(io19,file=lndat,status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','LNEMARB File',&
			lndat,io19)
	endif
	!
	! --- open VOLUME SOURCE file(s) (VOLEMARB.DAT)
	! --- (volume sources with arbitrary variation in location & emissions)
	do i=1,nvoldat
		io=iovol+i-1
		! ---    Assume ASCII file (formatted)
		!        call OPENAB(io,voldat(i),mfvl2(i))
		call FILVERS(io,'ASC',voldat(i),'VOLEMARB        ',vervarb(i))
		mfvl2(i)=1
		open(io,file=voldat(i),status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','VOLEMARB File',&
			voldat(i),io)
	enddo
	!
	! --- Open FLARE SOURCE file(s) (FLEMARB.DAT)
	! --- (flare sources with arbitrary variation in location & emissions)
	do i=1,nfldat
		io=iofl2+i-1
		! ---    Assume ASCII file (formatted)
		call FILVERS(io,'ASC',fldat(i),'FLEMARB        ',&
		verflarb(i))
		mffl2(i)=1
		OPEN(io,file=fldat(i),status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','FLEMARB File',&
			fldat(i),io)
	enddo
	!
	! --- Open ROAD SOURCE file(s) (RDEMARB.DAT)
	! --- (road sources with arbitrary variation in location & emissions)
	do i=1,nrddat
		io=iord2+i-1
		! ---    Assume ASCII file (formatted)
		call FILVERS(io,'ASC',rddat(i),'RDEMARB        ',&
		verrdarb(i))
		mfrd2(i)=1
		OPEN(io,file=rddat(i),status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','RDEMARB File',&
			rddat(i),io)
	enddo
	!
	! --- Open SPRAY SOURCE file(s) (SPEMARB.DAT)
	! --- (spray sources with arbitrary variation in location & emissions)
	do i=1,nspdat
		io=iosp2+i-1
		! ---    Assume ASCII file (formatted)
		call FILVERS(io,'ASC',spdat(i),'SPEMARB        ',&
		versparb(i))
		mfsp2(i)=1
		OPEN(io,file=spdat(i),status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','SPEMARB File',&
			spdat(i),io)
	enddo
	!
	! --- open DEPOSITION VELOCITY file (VD.DAT) (if user-specified
	!     deposition velocities are used for any species AND computation
	!     of dry deposition is requested
	if(mdry.eq.1)then
		do 10 i=1,mxspec
			if(idryflg(i).eq.3)then
				open(io20,file=vddat,status='old',iostat=ierr)
				if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','VD.DAT File',&
					vddat,io20)
				exit ! add by @creaqi break the loop goto 12 in openot with len equal 1
			endif
			10       continue
			12       continue
		endif
		!
		! --- open OZONE data file (OZONE.DAT) (if chemical transformation
		!     is computed AND hourly ozone input is requested)
		if((mchem.EQ.1 .OR. mchem.EQ.3 .OR. mchem.EQ.4 .OR. mchem.EQ.6&
			.OR. mchem.EQ.7).AND. moz.EQ.1) then
			call FILVERS(io22,'ASC',ozdat,'OZONE           ',veroz)
			open(io22,file=ozdat,status='old',iostat=ierr)
			if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','OZONE File',&
				ozdat,io22)
		endif
		!
		! --- open H2O2 data file (H2O2.DAT) (if aqueous phase chemical
		!     transformation is computed AND hourly H2O2 input is requested)
		if(maqchem.EQ.1 .AND.mh2o2.EQ.1) then
			call FILVERS(io23,'ASC',h2o2dat,'H2O2            ',verh2o2)
			open(io23,file=h2o2dat,status='old',iostat=ierr)
			if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','H2O2 File',&
				h2o2dat,io23)
		endif
		!
		! --- open NH3 data file (NH3Z.DAT) (if MCHEM = 6 or 7)
		if((mchem.EQ.6 .OR. mchem.EQ.7) .AND.mnh3.GT.0) then
			open(io40,file=nh3zdat,status='old',iostat=ierr)
			if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','NH3Z File',&
				nh3zdat,io40)
		endif
		!
		! --- open CHEMICAL TRANSFORMATION file (CHEM.DAT) (if user-specified
		!     chemical transformation rates are used)
		if(mchem.eq.2)open(io24,file=chemdat,status='old',iostat=ierr)
		if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','CHEM.DAT File',&
			chemdat,io24)
		!
		! --- open TURBULENCE data file (PROFILE.DAT) if needed
		if(.not.lprofile .AND. mtinv.NE.1) then
			! ---    Need turbulence only for MDISP=1,5
			if(mdisp.EQ.1 .OR. mdisp.EQ.5) then
				! ---       Open file only when explicitly requested
				if(mturbvw.LT.4) then
					call FILVERS(io31,'ASC',prfdat,'PROFILE         ',verprf)
					open(io31,file=prfdat,status='old',iostat=ierr)
					if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','PROFILE File',&
						prfdat,io31)
				endif
			endif
		endif
		!
		! --- open CTSG hill information files (HILL.DAT, HILLRCT.DAT)
		!     (if CTDM processors are used to create them)
		if(mctsg.eq.1.and.mhill.eq.1) then
			open(io28,file=hildat,status='old',iostat=ierr)
			if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','HILL.DAT File',&
				hildat,io28)
			open(io29,file=rctdat,status='old',iostat=ierr)
			if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','HILLRCT.DAT File',&
				rctdat,io29)
		endif
		!
		! --- open Coast Line(s) file (COASTLN.DAT)
		!     (if sub-grid TIBL module is used)
		if(msgtibl.eq.1) then
			call FILVERS(io25,'ASC',cstdat,'COASTLN         ',vercoast)
			open(io25,file=cstdat,status='old',iostat=ierr)
			if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','COASTLN.DAT File',&
				cstdat,io25)
		endif
		!
		! --- open Mass Flux files (FLUXBDY.DAT, MASSFLX.DAT)
		!     (if mass flux output option is selected)
		if(imflx.eq.1) then
			call FILVERS(io35,'ASC',bdydat,'FLUXBDY         ',verflxb)
			open(io35,file=bdydat,status='old',iostat=ierr)
			if(ierr.NE.0) call OPEN_ERR(io6,'OPENOT','FLUXBDY.DAT File',&
				bdydat,io35)
			open(io36,file=flxdat,status=cstat)
		endif
		!
		! --- open Mass Balance file (MASSBAL.DAT)
		!     (if mass balance output option is selected)
		if(imbal.eq.1) then
			open(io37,file=baldat,status=cstat)
		endif
		!
		! --- open NUMRISE file (RISE.DAT) and temporary work file
		!     (if output option is selected)
		if(INRISE.eq.1) then
			open(io38,file=risdat,status=cstat)
		endif
		!
		! --- open PUFF-TRACKING file (PFTRAK.DAT)
		!     (if output option is selected)
		if(IPFTRAK.GT.0) then
			open(iotrk,file=trkdat,status=cstat,&
			form='unformatted')
		endif
		!
		! --- open Puff/Slug DEBUG file (DEBUG.LST) (if in DEBUG mode)
		if(LDEBUG) then
			open(io30,file=debug,status=cstat)
			! ---    Write column headers for Puffs/Slugs
			write(io30,301)
			write(io30,302)
		endif
		! --- Open Direct Access scratch file for source tabulations (rise)
		! --- Record length in bytes holds 6 integers and 11 real arrays
		! --- dimensioned MXRISE
		nbytes=4*(6+11*mxrise)
		OPEN(iotab,access='direct',form='unformatted',recl=nbytes,&
		status='scratch')
		301   format('                              ---- PUFF/ Old SLUG end --',&
		'-------')
		302   format('YYYYJJJHH  SEC   ipnum cd   zfnl   x(metG)   y(metG)',&
		'    sigyB    sigzB          QM          QU   zimax',&
		'  rflctn    dpbl jdstab   Length')
		!
		return
end
	!----------------------------------------------------------------------
subroutine openab(iunit,fname,mformat)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 980807            OPENAB
	!                D. Strimaitis, SRC
	!
	! --- PURPOSE:  Open existing input file after determining if file
	!               is ASCII or Binary format
	!
	! --- UPDATE
	! --- V6.11  980807-060309  (DGS): filenames c*70 to c*132
	!
	! --- INPUTS:
	!              IUNIT - integer   - File unit number
	!              FNAME - character - File name
	!
	! --- OUTPUT:
	!            MFORMAT - integer   - Formatted file?
	!                                  (0:NO, 1:YES)
	!
	! --- OPENAB called by:  OPENOT
	! --- OPENAB calls:      INQUIRE(system-dependent?)
	!----------------------------------------------------------------------
	!
	character*132 fname
	character aform*7
	logical linquire
	! ---------------------------------------------------------------
	! --- Set logical to T if system supports this form of INQUIRE
	data linquire/.FALSE./
	! ---------------------------------------------------------------
	if(LINQUIRE)then
		INQUIRE(file=fname,formatted=aform)
		if(aform(1:3).EQ.'YES') then
			mformat=1
			open(iunit,file=fname,status='old')
		else
			mformat=0
			open(iunit,file=fname,status='old',form='unformatted')
		endif
		return
	endif
	! ---------------------------------------------------------------
	! --- Otherwise, use ERR to recover from inappropriate format
	! ---------------------------------------------------------------
	! --- Assume file is unformatted (binary)
	! ---------------------------------------
	mformat=0
	open(iunit,file=fname,status='old',form='unformatted',&
	err=200)
	! --- Test read
	read(iunit,err=200) aform
	! --- Close and ReOpen file to place pointer at start
	close(iunit)
	open(iunit,file=fname,status='old',form='unformatted')
	return
	200   continue
	close(iunit)
	! --- Assume file is formatted (ASCII)
	! ------------------------------------
	mformat=1
	open(iunit,file=fname,status='old',err=300)
	! --- Test read
	read(iunit,*,err=300) aform
	! --- Close and ReOpen file to place pointer at start
	close(iunit)
	open(iunit,file=fname,status='old')
	return
	300   continue
	close(iunit)
	! --- FATAL ERROR - file could not be opened and read
	! --- Simply open file again, and let system report the error
	open(iunit,file=fname,status='old')
	return      
end
!----------------------------------------------------------------------
subroutine setcom
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 000602            SETCOM
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Set miscellaneous common block variables in the
	!               /GRID/ common block
	!
	! --- UPDATE
	! --- V5.3-V5.4     000602  (DGS): add message to "stop"
	! --- V5.2-V5.3     991222  (DGS): compute SAM2GRID here
	!
	! --- INPUTS:
	!     Common block /GEN/ variables:
	!          NSPEC, ISPLST(4,mxspec)
	!     Common block /GRID/ variables:
	!          NX, NY, NZ, ZFACE(mxnzp1), DGRID, IBCOMP, JBCOMP,
	!          IECOMP, JECOMP, LSAMP, IBSAMP, JBSAMP, IESAMP, JESAMP,
	!          MESHDN
	!     Parameters:  MXNX, MXNY, MXNXG, MXNYG, MXNZ, IO6, MXNZP1
	!
	! --- OUTPUT:
	!     Common block /GEN/ variables:
	!          NSE, NSDD
	!     Common block /GRID/ variables:
	!          NXCMP, NYCMP, NXSAM, NYSAM, DELSAM, SAM2GRID,
	!          NXM1, NXM2, NYM1, NYM2, NZP1, ZGPT(mxnz)
	!
	! --- SETCOM called by: SETUP
	! --- SETCOM calls:     none
	!----------------------------------------------------------------------
	!
	! --- Include parameter statements
	include 'params.puf'
	!
	include 'gen.puf'
	include 'grid.puf'
	logical lerror
	!
	! --- Initialize error detector
	lerror=.false.
	!
	! ---------------------------------
	! --- Computational grid dimensions
	! ---------------------------------
	nxcmp=iecomp-ibcomp+1
	nycmp=jecomp-jbcomp+1
	!
	if(ibcomp.le.0.or.iecomp.gt.mxnx.or.&
		jbcomp.le.0.or.jecomp.gt.mxny)then
		write(io6,2)ibcomp,iecomp,jbcomp,jecomp,mxnx,mxny
		2          format(/1x,'ERROR in subr. SETCOM -- Invalid range for the ',&
		'COMPUTATIONAL grid'/1x,'IBCOMP = ',i6,2x,'IECOMP = ',i6/1x,&
		'JBCOMP = ',i6,2x,'JECOMP = ',i6/1x,'MXNX   = ',i6,2x,&
		'MXNY   = ',i6)
		lerror=.true.
	endif
	if(nxcmp.gt.mxnx.or.nycmp.gt.mxny)then
		write(io6,4)nxcmp,nycmp,mxnx,mxny
		4        format(/1x,'ERROR in subr. SETCOM -- Computational grid is ',&
		'too large for current array dimensions'/1x,'NXCMP  = ',i6,&
		2x,'NYCMP  = ',i6/1x,'MXNX   = ',i6,2x,'MXNY   = ',i6)
		lerror=.true.
	endif
	!
	! --- Computational grid must be a subset of meteorological grid
	! --- and at least 2 x 2 in size
	if(ibcomp.gt.nx.or.iecomp.gt.nx)then
		write(io6,34)ibcomp,iecomp,nx
		34       format(/1x,'ERROR in subr. SETCOM -- Computational grid is ',&
		'outside meteorological grid'/1x,'IBCOMP = ',i6,&
		2x,'IECOMP = ',i6/1x,'NX   = ',i6)
		lerror=.true.
	endif
	if(jbcomp.gt.ny.or.jecomp.gt.ny)then
		write(io6,36)jbcomp,jecomp,ny
		36       format(/1x,'ERROR in subr. SETCOM -- Computational grid is ',&
		'outside meteorological grid'/1x,'JBCOMP = ',i6,&
		2x,'JECOMP = ',i6/1x,'NY   = ',i6)
		lerror=.true.
	endif
	!
	! --- Index of end of computational grid must exceed beginning
	if(iecomp.le.ibcomp.or.jecomp.le.jbcomp)then
		write(io6,38)ibcomp,iecomp,jbcomp,jecomp
		38       format(/1x,'ERROR in subr. SETCOM -- Computational grid is ',&
		'ill-defined'/1x,'IBCOMP = ',i6,2x,'IECOMP = ',i6/&
		1x,'JBCOMP = ',i6,2x,'JECOMP = ',i6)
		lerror=.true.
	endif
	!
	! -------------------------------------------
	! --- Sampling grid dimensions & grid spacing
	! -------------------------------------------
	!
	if(lsamp)then
		if(ibsamp.le.0.or.iesamp.gt.mxnx.or.&
			jbsamp.le.0.or.jesamp.gt.mxny)then
			write(io6,42)ibsamp,iesamp,jbsamp,jesamp,mxnx,mxny
			42         format(/1x,'ERROR in subr. SETCOM -- Invalid range for the ',&
			'SAMPLING grid'/1x,'IBSAMP = ',i6,2x,'IESAMP = ',i6/1x,&
			'JBSAMP = ',i6,2x,'JESAMP = ',i6/1x,'MXNX   = ',i6,2x,&
			'MXNY   = ',i6)
			lerror=.true.
		endif
		!
		! --- Sampling grid must be a subset of computational grid
		! --- and at least 2 x 2 in size
		if(ibsamp.gt.iecomp.or.iesamp.gt.iecomp)then
			write(io6,44)ibsamp,iesamp,iecomp
			44          format(/1x,'ERROR in subr. SETCOM -- Sampling grid is ',&
			'outside computational grid'/1x,'IBSAMP = ',i6,&
			2x,'IESAMP = ',i6/1x,'IECOMP = ',i6)
			lerror=.true.
		endif
		if(jbsamp.gt.jecomp.or.jesamp.gt.jecomp)then
			write(io6,46)jbsamp,jesamp,jecomp
			46          format(/1x,'ERROR in subr. SETCOM -- Sampling grid is ',&
			'outside computational grid'/1x,'JBSAMP = ',i6,&
			2x,'JESAMP = ',i6/1x,'JECOMP = ',i6)
			lerror=.true.
		endif
		!
		! --- Sampling grid must part of computational grid
		if(ibsamp.lt.ibcomp.or.iesamp.lt.ibcomp.or.&
			jbsamp.lt.jbcomp.or.jesamp.lt.jbcomp)then
			write(io6,48)ibsamp,iesamp,ibcomp,&
			jbsamp,jesamp,jbcomp
			48          format(/1x,'ERROR in subr. SETCOM -- Computational grid ',&
			'is ill-defined'/1x,'IBSAMP = ',i6,2x,'IESAMP = ',i6/&
			1x,'IECOMP = ',i6/1x,'JBSAMP = ',i6,2x,'JESAMP = ',i6/&
			1x,'JECOMP = ',i6//1x,'IBSAMP, IESAMP must be >= IBCOMP ',&
			'and JBSAMP, JESAMP must be >= IBCOMP')
			lerror=.true.
		endif
		!
		! --- Index of end of sampling grid must exceed beginning
		if(iesamp.le.ibsamp.or.jesamp.le.jbsamp)then
			write(io6,49)ibsamp,iesamp,jbsamp,jesamp
			49          format(/1x,'ERROR in subr. SETCOM -- Computational grid ',&
			'is ill-defined'/1x,'IBSAMP = ',i6,2x,'IESAMP = ',i6/&
			1x,'JBSAMP = ',i6,2x,'JESAMP = ',i6//1x,'IESAMP must be ',&
			'> IBSAMP and JESAMP must be > JBSAMP')
			lerror=.true.
		endif
	endif
	!
	if(lsamp)then
		nxsam=meshdn*(iesamp-ibsamp)+1
		nysam=meshdn*(jesamp-jbsamp)+1
		sam2grid=1./float(meshdn)
		delsam=dgrid*sam2grid
	else
		nxsam=0
		nysam=0
		sam2grid=0.0
		delsam=0.0
	endif
	!
	if(nxsam.gt.mxnxg.or.nysam.gt.mxnyg)then
		write(io6,6)nxsam,nysam,mxnxg,mxnyg
		6        format(/1x,'ERROR in subr. SETCOM -- Sampling grid requested ',&
		'is too large for current array dimensions'/1x,'NXSAM = ',i6,&
		2x,'NYSAM = ',i6/1x,'MXNXG = ',i6,2x,'MXNYG = ',i6)
		lerror=.true.
	endif
	!
	! --- Derived met. grid parameters
	nxm1=nx-1
	nxm2=nx-2
	nym1=ny-1
	nym2=ny-2
	nzp1=nz+1
	!
	! --- Grid point heights
	do i=1,nz! add by @creaqi do label 10
		zgpt(i)=0.5*(zface(i)+zface(i+1))
	enddo !add by @creaqi 10
	10    continue
	!
	! --- Count the number of deposited species
	LL=0
	nse=0
	nsdd=0
	do i=1,nspec! add by @creaqi do label 15
		!
		! --- Count number of species modeled
		if(isplst(1,i).eq.1)then
			LL=LL+1
		endif
		!
		! --- Count number of species emitted
		if(isplst(2,i).eq.1)then
			nse=nse+1
		endif
		!
		! --- Count number of species dry deposited
		if(isplst(3,i).ge.1)then
			nsdd=nsdd+1
		endif
	enddo !add by @creaqi 15
	15    continue
	!
	if(LL.ne.nspec)then
		write(io6,25)LL,nspec,(isplst(1,n),n=1,mxspec)
		25       format(/1x,'ERROR in subr. SETCOM -- Number of species in ',&
		'species list does not match NSPEC -- LL = ',i5,3x,&
		'NSPEC = ',i5/1x,'ISPLST(1,-) = ',100i2)
		lerror=.true.
	endif
	!
	if(lerror)then
		write(io6,*)'Execution terminating in subr. SETCOM'
		write(*,*)
		stop 'Halted in SETCOM -- see list file.'
	endif
	!
	return
end
!----------------------------------------------------------------------
subroutine elevi
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 950630             ELEVI
	!                J. Scire, D. Strimaitis,  SRC
	!
	! --- PURPOSE:  Interpolate CALMET gridded terrain to array of
	!               gridded receptors
	!
	! --- INPUTS:
	!
	!     Common block /GRID/ variables:
	!           NXSAM, NYSAM, DGRID, IBSAMP, JBSAMP, DELSAM
	!     Parameters:
	!           MXNXG, MXNYG
	!
	! --- OUTPUT:
	!
	!     Common block /GRID/ variables:
	!           ELEVG(mxnxg,mxmyg)
	!
	! --- ELEVI called by:  SETUP
	! --- ELEVI calls:      GETELEV
	!----------------------------------------------------------------------
	include 'params.puf'
	include 'grid.puf'
	! --- Loop over grid receptors
	do i=1,nxsam
		do j=1,nysam
			! ---       Locate the receptor (met. grid units)
			xrec = (float(ibsamp)-0.5)+float(i-1)*delsam/dgrid
			yrec = (float(jbsamp)-0.5)+float(j-1)*delsam/dgrid
			!
			! ---       Get elevation from the CALMET elevation array
			call getelev(xrec,yrec,zrterr)
			elevg(i,j)=zrterr
		enddo
	enddo
	return
end
!----------------------------------------------------------------------
subroutine sigty(sigy0,dxkm,dt,sigy,virty,virdy)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 141230             SIGTY
	!                R. Yamartino, SRC
	!                Modifications for V2.0 made by J. Scire
	!                Modifications for V3.0 made by D. Strimaitis
	!
	! --- PURPOSE:  For a given dispersion option, IDOPT, and a given initial
	!               lateral plume standard deviation, SIGY0, compute the plume
	!               standard deviation for a given downwind distance or time
	!               increment, consistent with the dispersion regime, and
	!               return this value, SIGY (meters), for use as the puff's
	!               or slug's lateral sigma.
	!
	! --- UPDATE
	! --- V6.268 - TNG-7.1.0  141230  (DGS)
	!                                : apply roughness adjustment to rural
	!                                  PG sigmas using gridded z0 (CALMET)
	!                                : remove roughness adjustment that was
	!                                  applied to URBAN PG sigmas
	! --- V5.721-V6.268 100108  (DGS): add threshold to WARN argument list
	! --- V5.7-V5.721   040503  (DGS): add WARN to collect information on
	!                                  the largest (most) negative
	!                                  virtual time/distance;
	!                                  reset very small negative virtual
	!                                  time/distance to zero
	! --- V5.4-V5.7     030402  (DGS): use local small variable to test for
	!                                  non-zero sigy0
	! --- V5.3-V5.4     000602  (DGS): add message to "stop"
	!
	! --- INPUTS:
	!
	!             SIGY0 - real    - Initial dispersion coefficient (m).
	!              DXKM - real    - Incremental travel distance (km).
	!                DT - real    - Incremental travel time (s).
	!
	!    Common block /CSIGMA/ variables:
	!         TYIR, AYT, THFTY, SYH, AYPGT, BYPGT, AYPGTI, BYPGTI,
	!         AYURB, XIYURB, AVEFAC, YZ0FAC, IXCELL, IYCELL, MDCELL,
	!         IDOPTYS, IRUS, UAVGS, TSIGVS, KSTABS
	!
	! --- OUTPUTS:
	!
	!             SIGY  - real    - Lateral dispersion coefficient (m).
	!             VIRTY - real    - Virtual travel time (total)  (s)
	!             VIRDY - real    - Virtual travel distance (total) (km)
	!
	! ---   SIGTY called by:  AREAS1, AREAS2, LINES1, POINTS1, POINTS2,
	!                         SETPUF, SETSLG, DWSIGS, SIGMA, HEFTRANS,
	!                         PUFRECS, SLGRECS, PLGRECS, RECSPEC0, WARN
	! ---   SIGTY calls:      XVY
	!----------------------------------------------------------------------
	!
	! --- SUPPLEMENTAL NOTES:
	!
	! *** For IDOPTYS = 1,2
	!     Dispersion determined by local turbulence (tsigvs,tsigws) and the
	!     Irwin(1983) recommended forms of (fy,fz) of Draxler(1976):
	!     fy = 1.0 / (1.0 + 0.9 * sqrt(tyidr * t) )
	!                TYIDR  - real const - Reciprocal time scale (1/s) for
	!                                      Draxler form of fy.
	!
	! *** For IDOPTYS = 3
	!
	!     Pasquill-Gifford-Turner(PGT) coeffs. for RURAL conditions
	!     computed using the following ISC6-8 approximation formula:
	!           th = 0.017453293*(c(istab)-d(istab)*alog(xkm))
	!           sigy1 = 465.11628*xkm*tan(th)
	!     local array C(6)  - real array - PGT Y coeffs. for each PGT class.
	!     local array D(6)  - real array - PGT Y expons. for each PGT class.
	!
	!     Briggs urban coeffs. for each PGT class for URBAN conditions.
	!     Computed exactly as in ISC6-8.
	!             AYURB(6)  - real array - Urban Y coeffs. by PGT class.
	!            XIYURB(6)  - real array - Reciprocal length scale (1/m)
	!                                      for SIGY for each PGT class.
	!
	!     Heffter time dependent growth coeffs. are used for long ranges.
	!                  AYT  - real const - Time dependent Y growth rate for
	!                                      x > xtmdep for all PGT classes.
	!                  SYH  - real const - Horizontal sigma(m) beyond which
	!                                      time dependent growth assumed.
	!                THFTY  - real       - Virtual travel time (s) associated
	!                                      with SYH for current met; transition
	!                                      to Heffter growth law.
	!
	! *** For IDOPTYS = 4.  Same as IDOPTYS=3, but for
	!     Pasquill-Gifford-Turner(PGT) coeffs. for RURAL conditions, use
	!     values of coeffs. and exponents from MESOPUFF II approximations.
	!             AYPGT(6)  - real array - PGT Y coeffs. for each PGT class.
	!             BYPGT(6)  - real array - PGT Y expons. for each PGT class.
	!            AYPGTI(6)  - real array - Reciprocals of AYPGT(6).
	!            BYPGTI(6)  - real array - Reciprocals of BYPGT(6).
	!
	! *** For IDOPTYS = 5
	!     Dispersion determined by local turbulence (tsigvs) and the
	!     CTDM form of (fy) for neutral/stable (IDOPTYS(1) otherwise):
	!     fy = 1.0 / SQRT( 1.0 + u*t/20000.)
	!
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	include 'csigma.puf'
	!
	real c(6),d(6)
	!
	! --- Declare variables to test negative virtuals
	! --- (threshold for warning, threshold for HALT)
	real zerosec(2), zerokm(2)
	!
	! --- Include the ISC parameters for PGT (i.e.,rural) for SIGY.
	data c/24.1667,18.333,12.5,8.333,6.25,4.1667/
	data d/2.5334,1.8096,1.0857,0.72382,0.54287,0.36191/
	!
	data crit/1.0e-4/,zero/0.0/
	!
	! --- Set values to test for negative virtuals that are treated as zero
	! --- (threshold for warning, threshold for HALT)
	data zerosec/-.01,-1.0/, zerokm/-0.00001,-.001/
	! --- Set a 'small' sigma (m) to test for sigy0=0.0
	data zerosig/0.001/
	!
	! --- Make sure travel time is positive.
	if(dt.lt.zero) then
		if(dt.ge.zerosec(1)) then
			dt=zero
		elseif(dt.ge.zerosec(2)) then
			call WARN('SIGTY-t     ',dt,zerosec(1))
			dt=zero
		else
			write(io6,*)'ERROR in SUBR. SIGTY -- Incremental travel ',&
			'time < 0 -- DT = ',dt
			write(*,*)
			stop 'Halted in SIGTY -- see list file.'
		endif
	endif
	!
	! --- Make sure downwind distance is positive.
	if(dxkm.lt.zero) then
		if(dxkm.ge.zerokm(1)) then
			dxkm=zero
		elseif(dxkm.ge.zerokm(2)) then
			call WARN('SIGTY-x     ',dxkm,zerokm(1))
			dxkm=zero
		else
			write(io6,*)'ERROR in SUBR. SIGTY -- Incremental travel ',&
			'distance < 0 -- DXKM = ',dxkm,' IDOPTY = ',idoptys,&
			' IRU = ',irus
			write(*,*)
			stop 'Halted in SIGTY -- see list file.'
		endif
	endif
	!
	! --- Make sure stability class is valid.
	istab=kstabs
	if(istab.lt.1 .or. istab.gt.6) then
		write(io6,*)'ERROR in SUBR. SIGTY -- Invalid value of ',&
		'stability class -- ISTAB = ',istab,' IDOPTY = ',idoptys,&
		' IRU = ',irus
		write(*,*)
		stop 'Halted in SIGTY -- see list file.'
	endif
	!
	! --- For all values of IDOPTYS, use Heffter (1965) time-dependent
	! --- equations at large virtual time (large SIGMA Y).  Include a
	! --- time shift (t') to match curves at transition point.
	if(dt.GT.thfty .AND. sigy0.LT.zerosig) then
		tprime = syh/ayt - thfty
		sigy = ayt * (dt + tprime)
		! ---    Assign virtuals
		virty = dt
		virdy = uavgs * virty * 0.001
		return
	elseif(sigy0.GT.syh) then
		tprime = syh/ayt - thfty
		t0 = sigy0/ayt - tprime
		t = t0 + dt
		sigy = ayt * (t + tprime)
		! ---    Assign virtuals
		virty = t
		virdy = uavgs * virty * 0.001
		return
	endif
	!
	! --- Special treatment for IDOPTYS=5:  IDOPTYS=1 for Stabilities 1-3
	idopt5=5
	if(idoptys.eq.5 .and. istab.lt.4) idopt5=1
	!
	if(idoptys.le.2 .or. idopt5.eq.1) then
		!
		! -------------------
		! --- For IDOPTYS = 1,2
		! -------------------
		!        Compute dispersion coefficients using given TSIGVS.
		! ---    Dispersion determined by local turbulence tsigvs and the
		!        Irwin(1983) recommended form of fy of Draxler(1976):
		!        fy = 1.0 / (1.0 + 0.9 * sqrt(tyidr * t) )  ,
		!        where  TYIDR = Reciprocal time scale (1/s) for Draxler form of fy.
		!
		! ---    For all distances, compute the following.
		! ---    Compute initial travel time, t0, based on SIGY0
		t0 = zero
		if(sigy0.gt.zerosig) then
			gama = sigy0 / tsigvs
			!           beta = 0.5 * gama * ( 2.0   +   (0.9)**2 * gama * tyidr )
			beta = gama * (1.0 + 0.405*gama*tyidr)
			t0 = beta + sqrt(beta*beta - gama*gama)
		endif
		!
		! ---    Compute effective travel time, t
		t = t0 + dt
		! ---    Assign virtuals
		virty = t
		virdy = uavgs * t * 0.001
		!
		! ---    Set sigma-y
		if(dt.GT.zero) then
			! ---       Compute fy function using Draxler (1976)
			fy = 1.0 / ( 1.0  +  0.9 * sqrt(t * tyidr) )
			sigy = tsigvs * t * fy
		else
			sigy = sigy0
		endif
		!
	elseif(idoptys.le.4) then
		!
		! ------------------
		! --- IDOPTYS = 3 or 4
		! ------------------
		!
		! ---    Select roughness adjustment factor
		! ---    (May be different from 1.0 when option is selected)
		yrfac=yz0fac(ixcell,iycell,mdcell)
		yrfaci=1./yrfac
		!
		! --------------------------
		! --- For IDOPTYS = 3 -- RURAL
		! --------------------------
		! --- Compute rural PGT sigma y (in m) using the ISC formulation, with
		! --- averaging time adjustment factor (AVEFAC) and
		! --- roughness factor (yrfac)
		if(idoptys.eq.3 .and. irus.eq.0) then
			!
			xpskm = zero
			! ---       If initial sigma, sigy0, solve for pseudo distance, xpskm
			if(sigy0.gt.zerosig) then
				!
				! ---          Set initial guesses for travel distance x and convert to km.
				!              x2=sigy0 / 0.01 assumes lateral turbulence intensity of 0.01
				!              xkm2 = 0.001 * sigy0 * 100.
				xkm1 = 0.001 * sigy0
				!
				! ---          Improve xkm2 guess with the realistic MESOPUFF II estimate.
				xkm2 = 0.001*&
				(sigy0*avefaci*yrfaci*aypgti(istab))**bypgti(istab)
				!
				!
				! ---          compute the sigmas for the first guesses.
				th = 0.017453293*(c(istab)-d(istab)*alog(xkm1))
				sigy1 = avefac*yrfac*465.11628*xkm1*tan(th)
				!
				th = 0.017453293*(c(istab)-d(istab)*alog(xkm2))
				sigy2 = avefac*yrfac*465.11628*xkm2*tan(th)
				!
				niter = 0
				! ---          iterate until the sigma is found to be within fractional
				!              tolerance of crit.
				! ---          new guess
				do ! insert do to replace label 10, add by @creaqi 2020-02-18 11:30:05.550152
					10          denx = xkm2 - xkm1
					if(denx .eq. 0.) then
						xkm3 = xkm2
					else
						fint = (sigy0-sigy1) / (sigy2-sigy1)
						xkm3 = xkm1 + fint*denx
						! ---          restrict rate of change of position estimate
						step = xkm3 - xkm2
						xstep = 0.85 * abs(denx)
						if(abs(step) .gt. xstep) xkm3 = xkm2+sign(xstep,step)
					endif
					!
					niter=niter+1
					if(niter.gt.50) then
						write(*,*)
						stop 'SIGTY: XPSKM failed to converge'
					endif
					! ---          compute quantities now for the new guess.
					th = 0.017453293*(c(istab)-d(istab)*alog(xkm3))
					sigy3=avefac*yrfac*465.11628*xkm3*tan(th)
					!
					! ---          compute the error in the solution.
					errsig = (sigy3-sigy0) / sigy0
					!
					! ---          quit if error criteria satisfied.
					if(abs(errsig) .lt. crit) exit ! add by @creaqi break the loop goto 50 in sigty with len equal 1
					!
					! ---          Always replace 1 with 2 and load new info(i.e.,3) into 2.
					xkm1 = xkm2
					sigy1 = sigy2
					xkm2 = xkm3
					sigy2 = sigy3
				enddo ! insert enddo to replace goto [10, add by @creaqi 2020-02-18 11:30:05.550152
				! goto 10 add by @creaqi 2020-02-18 11:30:05.550152
				!
				50          xpskm = xkm3
				!
			endif
			!
			! ---       Combine the actual and pseudo distances and compute sigy.
			xkm3 = xpskm + dxkm
			if(dxkm.GT.zero) then
				if(xkm3.gt.0.) then
					th=0.017453293*(c(istab)-d(istab)*alog(xkm3))
					sigy=avefac*yrfac*465.11628*xkm3*tan(th)
				else
					th=0.
					sigy=0.
				endif
			else
				th=0.
				sigy=sigy0
			endif
			! ---       Assign virtuals
			virdy = xkm3
			virty = xkm3 * 1000. / uavgs
			!
		endif
		!
		! --------------------------
		! --- For IDOPTYS = 4 -- RURAL
		! --------------------------
		! --- Compute rural PGT sigma y (in m) using MESOPUFF II formulation.
		if(idoptys.eq.4 .and. irus.eq.0) then
			!
			xpsm = zero
			! ---       If initial sigma, sigy0, solve for pseudo distance, xpsm
			if(sigy0.gt.zerosig) then
				xpsm=(sigy0*avefaci*yrfaci*aypgti(istab))**bypgti(istab)
			endif
			!
			! ---       Combine the actual and pseudo distances and compute sigy.
			xm = xpsm + 1000.0 * dxkm
			if(dxkm.GT.zero) then
				sigy = avefac*yrfac * aypgt(istab) * xm**bypgt(istab)
			else
				sigy = sigy0
			endif
			!
			! ---       Assign virtuals
			virdy = xm * 0.001
			virty = xm / uavgs
			!
		endif
		!
		! -------------------------------
		! --- For IDOPTYS = 3 or 4 -- URBAN
		! -------------------------------
		! --- Compute urban Briggs sigma y (in m) as in ISC.
		if(idoptys.ge.3 .and. irus.eq.1) then
			!
			xpskm = zero
			! ---       If initial sigma, sigy0, solve for pseudo distance, xpskm
			if(sigy0.gt.zerosig) xpskm = xvy(sigy0*avefaci,istab)
			!
			! ---       Combine the actual and pseudo distances and compute sigy.
			xm = 1000.0 * (xpskm + dxkm)
			if(dxkm.GT.zero) then
				sigy = avefac*ayurb(istab)*xm/sqrt( 1.0 +&
				xiyurb(istab)*xm )
			else
				sigy = sigy0
			endif
			!
			! ---       Assign virtuals
			virdy = xm * 0.001
			virty = xm / uavgs
			!
		endif
		!
	elseif(idoptys.le.5) then
		!
		! ------------------------------------
		! --- For IDOPTYS = 5 --- Stable/Neutral
		! ------------------------------------
		!        Dispersion determined by local turbulence (tsigvs) and the
		!        CTDM form of (fy) for neutral/stable (IDOPTYS(1) otherwise):
		!        fy = 1.0 / SQRT( 1.0 + u*t/20000.)
		!
		! ---    Compute reciprocal timescale using mean transport speed over
		! ---    life of puff
		tyict = uavgs/20000.
		! ---    Compute initial travel time, t0, based on SIGY0
		t0 = zero
		if(sigy0.gt.zerosig) then
			gamasq = (sigy0/tsigvs)**2
			beta = gamasq*tyict*0.5
			t0 = beta+SQRT(beta*beta+gamasq)
		endif
		! ---    Compute effective travel time, t
		t = t0 + dt
		! ---    Compute sigma
		if(dt.GT.zero) then
			! ---       Compute fy function using CTDM form
			fy = 1.0 / SQRT( 1.0  +  t * tyict )
			sigy = tsigvs * t * fy
		else
			sigy = sigy0
		endif
		! ---    Assign virtuals
		virty = t
		virdy = uavgs * t * 0.001
		!
	endif
	!
	return
end
! ---------------------------------------------------------------------
function xvy (sy0,kst)                                           ! isc36640
	! ---------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 900216               XVY
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  XVY calculates the virtual distance necessary to        isc36660
	!               account for the initial crosswind dispersion.           isc36670
	!               Based on Briggs urban dispersion coeffcients.           isc36680
	!               Adapted directly from ISC6-8.
	!
	!
	! --- INPUTS:
	!
	!               SY0 - real    - Initial dispersion coefficient (m).
	!               KST - integer - PGT stability index (1-6)
	!
	!     COMMON /CSIGMA/ variables:
	!     Briggs urban coeffs. for each PGT class for URBAN conditions.
	!             AYURB(6)  - real array - Urban Y coeffs. by PGT class.
	!            XIYURB(6)  - real array - Reciprocal length scale (1/m)
	!                                      for SIGY for each PGT class.
	!
	! --- OUTPUTS:
	!
	!              XVY  - real    - Pseudo-distance (km).
	!
	!
	! ---   XVY called by:  SIGTY
	! ---   XVY calls:       none
	! ---------------------------------------------------------------------
	!                                                                       isc36650
	include 'params.puf'
	include 'csigma.puf'
	!
	a2 = ayurb(kst)**2
	bsy0 = 0.5 * xiyurb(kst) * sy0
	!
	! *** Direct solution for urban dispersion                              isc36720
	xvy = sy0 * (bsy0 + sqrt(bsy0**2 + a2)) / a2                     ! isc36730
	!
	! --- Convert to kilometers                                             isc36750
	xvy = xvy * 0.001                                                ! isc36760
	return                                                           ! isc36770
	end                                                              ! isc36780
	!----------------------------------------------------------------------
subroutine sigtz(sigz0,dxkm,dt,zht,sigz,virtz,virdz)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 141230             SIGTZ
	!                R. Yamartino
	!                Modifications for V3.0 made by D. Strimaitis
	!
	! --- PURPOSE:  For a given dispersion option, IDOPT, and a given initial
	!               vertical plume standard deviation, SIGZ0, compute the plume
	!               standard deviation for a given downwind distance or time
	!               increment, consistent with the dispersion regime, and
	!               return this value, SIGZ (meters), for use as the puff's
	!               or slug's vertical sigma.
	!
	! --- UPDATE
	! --- V6.268 - TNG-7.1.0  141230  (DGS)
	!                                : apply roughness adjustment to PG
	!                                  sigmas using gridded z0 (CALMET)
	! --- V5.721-V6.268 100108  (DGS): add WARN to collect information on
	!                                  the largest (most) negative
	!                                  virtual time/distance
	! --- V5.7-V5.721   040503  (DGS): reset very small negative virtual
	!                                  time/distance to zero
	! --- V5.4-V5.7     030402  (DGS): use local small variable to test for
	!                                  non-zero sigz0
	! --- V5.3-V5.4     000602  (DGS): add message to "stop"
	!
	!
	! --- INPUTS:
	!
	!             SIGZ0 - real    - Initial dispersion coefficient (m).
	!              DXKM - real    - Incremental travel distance (km).
	!                DT - real    - Incremental travel time (s).
	!
	!         For IDOPT = 5   also need:
	!               ZHT - real    - Height of center of puff/slug
	!
	!    Common block /CSIGMA/ variables:
	!     TZIDR,TZISDR,AZT,THFTZ,SZH,AZRUR,BZRUR,AZPGT,BZPGT,AZURB,XIZURB,
	!     IDOPTZS, IRUS, UAVGS, KSTABS, ELS, BVFS, MHFTSZS, TSIGWS
	!     AZ0FAC, BZ0TRM, IXCELL, IYCELL, MDCELL
	!
	! --- OUTPUTS:
	!
	!             SIGZ  - real    - Vertical dispersion coefficient (m).
	!             VIRTZ - real    - Virtual travel time (total)  (s)
	!             VIRDZ - real    - Virtual travel distance (total)  (km)
	!
	!
	! ---   SIGTZ called by:  AREAS1, AREAS2, LINES1, POINTS1, POINTS2,
	!                         VOLS, SETPUF, SETSLG, DWSIGS,
	!                         PUFRECS, SLGRECS, RECSPEC0
	! ---   SIGTZ calls:      XVZ, WARN
	!----------------------------------------------------------------------
	!
	! --- SUPPLEMENTAL NOTES:
	!
	! *** For IDOPTZS = 1,2
	!     Dispersion determined by local turbulence (tsigvs,tsigws) and the
	!     Irwin(1983) recommended forms of (fy,fz) of Draxler(1976):
	!     fz = 1.0 / (1.0 + 0.9 * sqrt(tzidr * t) )         for L < 0
	!     fz = 1.0 / (1.0 + 0.945 * (tzisdr * t)**0.806 )   for L > 0
	!                TZIDR  - real const - Reciprocal time scale (1/s) for
	!                                      Draxler form of fz for L < 0
	!               TZISDR  - real const - Reciprocal time scale (1/s) for
	!                                      Draxler stable form of fz (L > 0)
	!
	! *** For IDOPTZS = 3
	!
	!     Pasquill-Gifford-Turner(PGT) coeffs. for RURAL conditions
	!     computed using the ISC6-8 multi-segment approximation formulae.
	!
	!     A,B parameters are adjusted for roughness length
	!
	!
	! *** For IDOPTZS = 3 and 4
	!     Briggs urban coeffs. for each PGT class for URBAN conditions.
	!     Computed exactly as in ISC6-8.
	!             AZURB(6)  - real array - PGT Z coeffs. for each PGT class.
	!            XIZURB(6)  - real array - Reciprocal length scale (1/m)
	!                                      for SIGZ for each PGT class.
	!
	!     Heffter(1965) time dependent growth coeffs. for long range.
	!               AZT(6)  - real array - Time dependent Z growth rates for
	!                                      x > xtmdep for each PGT class.
	!                                      each grid point
	!                  SZH  - real const - Vertical sigma (m) beyond which
	!                                      time dependent growth assumed.
	!
	! *** For IDOPTZS = 4.  Same as IDOPTZS=3, but for
	!     Pasquill-Gifford-Turner(PGT) coeffs. for RURAL conditions, use
	!     values of coeffs. and exponents from MESOPUFF II approximations.
	!             AZPGT(6)  - real array - PGT Z coeffs. for each PGT class.
	!             BZPGT(6)  - real array - PGT Z expons. for each PGT class.
	!
	!     A,B parameters are adjusted for roughness length
	!
	! *** For IDOPTZS = 5
	!     Dispersion determined by local turbulence (tsigws) and the
	!     CTDM form of (fz) for neutral/stable (IDOPTZS(1) otherwise):
	!     fz = 1.0 / SQRT( 1.0 + TSIGWS*t*[1./(.72*ZHT)+BVFS/(.54*TSIGWS)])
	!
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	include 'csigma.puf'
	! --- Local arrays for PG A,B coefficients and segment sigmas
	real aprm(10),bprm(10),szbrur(10)
	!
	! --- Declare variables to test negative virtuals
	! --- (threshold for warning, threshold for HALT)
	real zerosec(2), zerokm(2)
	!
	data crit/1.0e-4/,zero/0.0/
	!
	! --- Set values to test for negative virtuals that are treated as zero
	! --- (threshold for warning, threshold for HALT)
	data zerosec/-.01,-1.0/, zerokm/-0.00001,-.001/
	! --- Set a 'small' sigma (m) to test for sigz0=0.0
	data zerosig/0.001/
	!
	!     SIGMAX=5000. conforms with EPA use of the Urban curves.
	!     See IDOPTZS > 3 and IRUS = 1 section of this routine.
	data sigmax/5000./
	!
	! --- Make sure travel time is positive.
	if(dt.lt.zero) then
		if(dt.ge.zerosec(1)) then
			dt=zero
		elseif(dt.ge.zerosec(2)) then
			call WARN('SIGTZ-t     ',dt,zerosec(1))
			dt=zero
		else
			write(io6,*)'ERROR in SUBR. SIGTZ -- Incremental travel ',&
			'time < 0 -- DT = ',dt
			write(*,*)
			stop 'Halted in SIGTZ -- see list file.'
		endif
	endif
	!
	! --- Make sure downwind distance is positive.
	if(dxkm.lt.zero) then
		if(dxkm.ge.zerokm(1)) then
			dxkm=zero
		elseif(dxkm.ge.zerokm(2)) then
			call WARN('SIGTZ-x     ',dxkm,zerokm(1))
			dxkm=zero
		else
			write(io6,*)'ERROR in SUBR. SIGTZ -- Incremental travel ',&
			'distance < 0 -- DXKM = ',dxkm
			write(*,*)
			stop 'Halted in SIGTZ -- see list file.'
		endif
	endif
	!
	! --- Make sure stability class is valid.
	istab=kstabs
	if(istab.lt.1 .or. istab.gt.6) then
		write(io6,*)'SR. SIGTZ: FATAL ERROR: istab = ',istab
		write(*,*)
		stop 'Halted in SIGTZ -- see list file.'
	endif
	!
	! --- Special treatment for IDOPTZS=5:  IDOPTZS=1 for Stabilities 1-3
	idopt5=5
	if(idoptzs.eq.5 .and. istab.lt.4) idopt5=1
	!
	! --- For all values of IDOPTZS, use Heffter (1965) time-dependent
	! --- equations at large virtual time (large SIGMA Z).  Include a
	! --- time shift (t') to match curves at transition point.
	if(mhftszs.EQ.1) then
		if(dt.GT.thftz .AND. sigz0.LT.zerosig) then
			tprime = (szh/azt(istab))**2 - thftz
			! ---       Compute sigma
			sigz = azt(istab) * SQRT(dt + tprime)
			! ---       Assign virtuals
			virtz = dt
			virdz = uavgs * dt * 0.001
			return
		elseif(sigz0.GT.szh) then
			tprime = (szh/azt(istab))**2 - thftz
			t0 = (sigz0/azt(istab))**2 - tprime
			t = t0 + dt
			! ---       Compute sigma
			sigz = azt(istab) * SQRT(t + tprime)
			! ---       Assign virtuals
			virtz = t
			virdz = uavgs * t * 0.001
			return
		endif
	endif
	if(idoptzs.le.2 .or. idopt5.eq.1) then
		!
		! --------------------
		! --- For IDOPTZS = 1,2
		! --------------------
		!        Dispersion determined by local turbulence (tsigvs,tsigws) and
		!        Irwin(1983) recommended forms of (fy,fz) of Draxler(1976):
		!        fz = 1.0 / (1.0 + 0.9 * sqrt(tzidr * t) )         for L < 0
		!        fz = 1.0 / (1.0 + 0.945 * (tzisdr * t)**0.806 )   for L > 0
		!                TZIDR  - real const - Reciprocal time scale (1/s) for
		!                                      Draxler form of fz for L < 0
		!               TZISDR  - real const - Reciprocal time scale (1/s) for
		!                                      Draxler stable form of fz (L > 0)
		!
		! ---    For all distances, compute the following.
		! ---    Compute initial travel time, t0, based on SIGZ0
		t0 = zero
		if(sigz0.gt.zerosig) then
			gama = sigz0 / tsigws
			if(els.le.zero) then
				!              fz = 1.0 / (1.0 + 0.9 * sqrt(tzidr * t) )
				!              beta = 0.5 * gama * ( 2.0   +   (0.9)**2 * gama * tzidr )
				beta = gama * (1.0 + 0.405*gama*tzidr)
				t0 = beta + sqrt(beta*beta - gama*gama)
			else
				!              els > 0 and
				!              fz = 1.0 / (1.0 + 0.945 * (tzisdr * t)**0.806 ) for L > 0
				!
				! ---          Use the log of gama for faster convergence.
				gamal = alog(gama)
				!
				! ---          Compute first guess using small t approx.
				t1 =  gama
				gm1 = t1 / (1.0 + 0.945 * (tzisdr * t1)**0.806 )
				gm1 = alog(gm1)
				t1l = alog(t1)
				!
				! ---          Compute second guess using large t approx. if
				! ---          gamma is large enough to produce t2 > 2*t1
				! ---          note: (.945/2**.194)**(-1/.806) = 1.2675
				tcrit = 1.2675/tzisdr
				if(t1.LE.tcrit) then
					t2=2.0*t1
				else
					t2 = (gama * 0.945 * tzisdr**0.806)**5.154639
				endif
				!              t2 = amax1(t2,(100.0*t1))
				gm2 = t2 / (1.0 + 0.945 * (tzisdr * t2)**0.806 )
				gm2 = alog(gm2)
				t2l = alog(t2)
				!
				niter = 0
				! ---          Iterate until the sigma is found to be within fractional
				! ---          tolerance of crit.
				do ! insert do to replace label 5, add by @creaqi 2020-02-18 11:30:05.570184
					5          niter=niter+1
					!
					! ---          Report failure if niter exceeds 50
					if(niter.gt.50) then
						write(io6,*) 'SUBR. SIGTZ: T0 does not converge'
						write(io6,*) 'sigz0, tsigws, els: ',sigz0,tsigws,els
						write(io6,*) 'tzidr, tzisdr  : ',tzidr, tzisdr
						write(io6,*) 't1,t2          : ',t1,t2
						write(*,*)
						stop 'Halted in SIGTZ -- see list file.'
					endif
					!
					! ---          New guess
					denx = t2l - t1l
					if(denx .eq. 0.) then
						t3l = t2l
					else
						fint = (gamal-gm1) / (gm2-gm1)
						t3l = t1l + fint*denx
						! ---             Restrict rate of change of position estimate
						step = t3l - t2l
						xstep = 0.85 * abs(denx)
						if(abs(step) .gt. xstep) t3l = t2l + sign(xstep,step)
					endif
					!
					! ---          Compute quantities now for the new guess.
					t3 = exp(t3l)
					gm3 = t3 / (1.0 + 0.945 * (tzisdr * t3)**0.806 )
					!
					! ---          Compute the error and quit if error criteria satisfied.
					errsig = (gm3 - gama) / gama
					if(abs(errsig) .lt. crit) exit ! add by @creaqi break the loop goto 10 in sigtz with len equal 1
					!
					! ---          Always replace 1 with 2 and load new info(i.e.,3) into 2.
					t1  = t2
					t1l = t2l
					gm1 = gm2
					t2  = t3
					t2l = t3l
					gm2 = alog(gm3)
				enddo ! insert enddo to replace goto [5, add by @creaqi 2020-02-18 11:30:05.570184
				! goto 5 add by @creaqi 2020-02-18 11:30:05.570184
				!
				10          t0 = t3
			endif
		endif
		!
		! ---    Compute effective travel time, t
		t = t0 + dt
		!
		! ---    Compute sigma
		if(dt.GT.zero) then
			! ---       Compute fz function using Draxler (1976)
			if(els.le.zero) fz = 1.0 / (1.0 + 0.9 * sqrt(tzidr * t) )
			if(els.gt.zero) fz = 1.0 / (1.0 + 0.945 * (tzisdr*t)**0.806)
			sigz = tsigws * t * fz
		else
			sigz = sigz0
		endif
		!
		! ---    Assign virtuals
		virtz = t
		virdz = t * uavgs * 0.001
		!
	elseif(idoptzs.le.4) then
		!
		! -----------------------
		! --- For IDOPTZS = 3 or 4
		! -----------------------
		!
		! ---    Adjust A,B for roughness if rural (active for MROUGH=1)
		!
		! ---------------------------
		! --- For IDOPTZS = 3 --- RURAL
		! ---------------------------
		! ---    Compute rural PGT sigma z (in m) using the ISC formulation.
		if(idoptzs.eq.3 .and. irus.eq.0) then
			!
			! ---       Define the number of sigma Z curve segments, NSEG.
			nseg = nzbrur(istab)
			! ---       Set the roughness-adjusted A', B' values for this location
			! ---       and stability class
			aprm(:)=azrur(:,istab)
			bprm(:)=bzrur(:,istab)
			if(bz0trm(ixcell,iycell,mdcell).GT.-999.0) then
				! ---          Apply roughness adjustment (B=-1000. if MROUGH=0)
				do k=1,nseg
					aprm(k)=azrur(k,istab)*az0fac(ixcell,iycell,mdcell)
					bprm(k)=bzrur(k,istab)-bz0trm(ixcell,iycell,mdcell)
				enddo
			endif
			! ---       Set sigma-z values at segment boundaries
			do k=1,nseg-1
				szbrur(k)=aprm(k)*(xzbrur(k,istab))**bprm(k)
			enddo
			szbrur(nseg)=1.0e20
			!
			xpskm = zero
			! ---       If initial sigma, sigz0, solve for pseudo distance, xpskm
			if(sigz0.gt.zerosig) then
				!
				! ---          Find the correct region, denoted by the segment JSEG.
				jseg = 1
				do ! insert do to replace label 15, add by @creaqi 2020-02-18 11:30:05.570275
					15          if(sigz0.le.szbrur(jseg)) exit ! add by @creaqi break the loop goto 20 in sigtz with len equal 1
					jseg = jseg + 1
				enddo ! insert enddo to replace goto [15, add by @creaqi 2020-02-18 11:30:05.570275
				! goto 15 add by @creaqi 2020-02-18 11:30:05.570275
				!
				20          xpskm = ( sigz0/aprm(jseg) )**(1./bprm(jseg))
			endif
			!
			! ---       Combine the actual and pseudo distances and compute sigz.
			xkm = xpskm + dxkm
			!
			! ---       Compute sigma
			if(dxkm.GT.zero) then
				! ---          Find the correct region, denoted by the segment JSEG.
				jseg = 1
				do ! insert do to replace label 25, add by @creaqi 2020-02-18 11:30:05.570349
					25          if(xkm.le.xzbrur(jseg,istab)) exit ! add by @creaqi break the loop goto 30 in sigtz with len equal 1
					jseg = jseg + 1
				enddo ! insert enddo to replace goto [25, add by @creaqi 2020-02-18 11:30:05.570349
				! goto 25 add by @creaqi 2020-02-18 11:30:05.570349
				!
				30          sigz = aprm(jseg) * xkm**bprm(jseg)
			else
				sigz = sigz0
			endif
			!
			! ---       Assign virtuals
			virdz = xkm
			virtz = xkm *1000. / uavgs
			!
		endif
		!
		! ---------------------------
		! --- For IDOPTZS = 4 --- RURAL
		! ---------------------------
		! ---    Compute rural PGT sigma z (in m) using MESOPUFF II formulation.
		if(idoptzs.eq.4 .and. irus.eq.0) then
			!
			! ---       Set the roughness-adjusted A', B' for this location
			! ---       and stability class
			azprm=azpgt(istab)
			bzprm=bzpgt(istab)
			if(bz0trm(ixcell,iycell,mdcell).GT.-999.0) then
				! ---          Apply roughness adjustment (B=-1000. if MROUGH=0)
				azprm=azpgt(istab)*az0fac(ixcell,iycell,mdcell)
				bzprm=bzpgt(istab)-bz0trm(ixcell,iycell,mdcell)
			endif
			xpsm = zero
			! ---       If initial sigma, sigz0, solve for pseudo distance, xpsm
			if(sigz0.gt.zerosig) then
				xpsm = (sigz0/azprm)**(1./bzprm)
			endif
			!
			! ---       Combine the actual and pseudo distances and compute sigz.
			xm = xpsm + 1000.0 * dxkm
			if(dxkm.GT.zero) then
				sigz = azprm * xm**bzprm
			else
				sigz = sigz0
			endif
			!
			! ---       Assign virtuals
			virdz = xm * 0.001
			virtz = xm / uavgs
			!
		endif
		!
		! --------------------------------
		! --- For IDOPTZS = 3 or 4 --- URBAN
		! --------------------------------
		! ---    Compute urban Briggs sigma z (in m) as in ISC.
		if(idoptzs.ge.3 .and. irus.eq.1) then
			!
			! ---       If initial sigma exceeds sigmax set sigz=sigz0 and return.
			if(sigz0.ge.sigmax) then
				sigz = sigz0
				return
			endif
			!
			xpskm = zero
			! ---       If initial sigma, sigz0, solve for pseudo distance, xpskm
			if(sigz0.gt.zerosig) xpskm = xvz(sigz0,istab)
			!
			! ---       Combine the actual and pseudo distances and compute sigz.
			xm = 1000.0 * (xpskm + dxkm)
			if(dxkm.GT.zero) then
				sigz = azurb(istab) * xm
				if(istab.le.2) sigz = sigz * sqrt(1.0+xizurb(istab)*xm)
				if(istab.ge.4) sigz = sigz / sqrt(1.0+xizurb(istab)*xm)
				if(sigz .gt. sigmax) sigz = sigmax
			else
				sigz = sigz0
			endif
			!
			! ---       Assign virtuals
			virdz = xm * 0.001
			virtz = xm / uavgs
			!
		endif
		!
	elseif(idoptzs.le.5) then
		!
		! ------------------------------------
		! --- For IDOPTZS = 5 --- Stable/Neutral
		! ------------------------------------
		!        Dispersion determined by local turbulence (tsigws) and the
		!        CTDM form of (fz) for neutral/stable (IDOPTZS(1) otherwise):
		!        fz = 1.0/SQRT(1.0 +t*TSIGWS*[1./(.72*ZHT)+BVFS/(.54*TSIGWS)])
		!        fz = 1.0 / SQRT( 1.0 + t*TZICT)
		!
		! ---    Do not allow neutral scale to blow up! (set min zht=1 m)
		zz=AMAX1(zht,1.0)
		! ---    Compute reciprocal timescale (1/s)
		tzict = tsigws/(.72*zz) + bvfs*1.851852
		! ---    Compute initial travel time, t0, based on SIGZ0
		t0 = zero
		if(sigz0.gt.zerosig) then
			gamasq = (sigz0/tsigws)**2
			beta = gamasq*tzict*0.5
			t0 = beta+SQRT(beta*beta+gamasq)
		endif
		! ---    Compute effective travel time, t
		t = dt + t0
		! ---    Compute sigma
		if(dt.GT.zero) then
			! ---       Compute fz function using CTDM form
			fz = 1.0 / SQRT( 1.0  +  t * tzict )
			sigz = tsigws * t * fz
		else
			sigz = sigz0
		endif
		!
		! ---    Assign Virtuals
		virtz = t
		virdz = t * uavgs * 0.001
		!
	endif
	!
	return
end
! ---------------------------------------------------------------------
function xvz (sz0,kst)                                           ! isc36640
	! ---------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 900216               XVZ
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  XVZ calculates the virtual distance necessary to        isc36660
	!               account for the initial vertical dispersion.            isc36670
	!               Based on Briggs urban dispersion coeffcients.           isc36680
	!               Adapted directly from ISC6-8.
	!
	!
	! --- INPUTS:
	!
	!               SZ0 - real    - Initial dispersion coefficient (m).
	!               KST - integer - PGT stability index (1-6).
	!
	!     COMMON /CSIGMA/ variables:
	!     Briggs urban coeffs. for each PGT class for URBAN conditions.
	!             AZURB(6)  - real array - Urban Y coeffs. by PGT class.
	!            XIZURB(6)  - real array - Reciprocal length scale (1/m)
	!                                      for SIGZ for each PGT class.
	!
	! --- OUTPUTS:
	!
	!              XVZ  - real    - Pseudo-distance (km).
	!
	!
	! ---   XVZ called by:  SIGTZ
	! ---   XVZ calls:       none
	! ---------------------------------------------------------------------
	!                                                                       isc36650
	include 'params.puf'
	include 'csigma.puf'
	!
	!     dimension c(6),d(6)                                               isc36850
	!     data c /1.e06,1.e06,.20,.14,.08,.08/                              isc36860
	!     data d /1.e09,1.e09,0.,.0003,.0015,.0015/                         isc36870
	data third/0.3333333/
	!
	! --- Use above info to override values in /CSIGMA/
	c = azurb(kst)
	d = xizurb(kst)
	if(kst.le.2) then
		c = 1.0e06
		d = 1.0e09
	endif
	if(kst.eq.3) d = 0.0
	!
	!
	!     Direct solution for urban dispersion                              isc36880
	select case(kst)
	case(1)
	a=-c * third
	b=(2./27.-(sz0/240.)**2) * d
	s=0.25*b**2 + a**3/27.
	if(.not. (s .lt. 0.)) then ! add by @creaqi goto 185 in fun modify_goto_pure
		s=sqrt(s)                                                        ! isc36970
		!     e=1./3.                                                           isc36980
		if (.not. ((-b/2.+s) .lt. 0.0)) then ! add by @creaqi 185 state_same == True
			!     ba=(-b/2.+s)**e                                                   isc36990
			ba=(-b/2.+s)**third
			!     bb=(-b/2.-s)**e                                                   isc37000
			bb=(-b/2.-s)**third
			y=ba+bb                                                          ! isc37010
			xvz=y-1000. * third
			xvz=xvz * 0.001
			return                                                           ! isc37040
			!     cs=(sz0/240.)**2*27./2.-1.                                        isc37050
		endif ! add by @creaqi 185 state_same == True
	endif !add by @creaqi label 185 modify_goto_pure
	cs=13.5 * (sz0/240.)**2. - 1.
	th=acos(cs) * third
	y=2.* third * cos(th) * 1000.
	xvz=y-1000. * third
	xvz=xvz * 0.001
	return                                                           ! isc37040
	!      go to 182                                                        ! isc37080comment by @creaqi
	!     stability c(190)                                                  isc37090
	case(2)
	a=-c * third
	b=(2./27.-(sz0/240.)**2) * d
	s=0.25*b**2 + a**3/27.
	if(.not. (s .lt. 0.)) then ! add by @creaqi goto 185 in fun modify_goto_pure
		s=sqrt(s)                                                        ! isc36970
		!     e=1./3.                                                           isc36980
		if (.not. ((-b/2.+s) .lt. 0.0)) then ! add by @creaqi 185 state_same == True
			!     ba=(-b/2.+s)**e                                                   isc36990
			ba=(-b/2.+s)**third
			!     bb=(-b/2.-s)**e                                                   isc37000
			bb=(-b/2.-s)**third
			y=ba+bb                                                          ! isc37010
			xvz=y-1000. * third
			xvz=xvz * 0.001
			return                                                           ! isc37040
			!     cs=(sz0/240.)**2*27./2.-1.                                        isc37050
		endif ! add by @creaqi 185 state_same == True
	endif !add by @creaqi label 185 modify_goto_pure
	cs=13.5 * (sz0/240.)**2. - 1.
	th=acos(cs) * third
	y=2.* third * cos(th) * 1000.
	xvz=y-1000. * third
	xvz=xvz * 0.001
	return                                                           ! isc37040
	!      go to 182                                                        ! isc37080comment by @creaqi
	!     stability c(190)                                                  isc37090
	case(3)
	xvz=sz0/c                                                        ! isc37100
	xvz=xvz * 0.001
	return                                                           ! isc37120
	!     stability d,e,&f (200)                                            isc37130
	case(4)
	xvz=sz0 * (d*sz0 + sqrt( (d*sz0)**2 + 4.*c**2 ) ) / (2.*c**2)
	xvz=xvz * 0.001
	case(5)
	xvz=sz0 * (d*sz0 + sqrt( (d*sz0)**2 + 4.*c**2 ) ) / (2.*c**2)
	xvz=xvz * 0.001
	case(6)
	xvz=sz0 * (d*sz0 + sqrt( (d*sz0)**2 + 4.*c**2 ) ) / (2.*c**2)
	xvz=xvz * 0.001
	end select
	!170   go to (180,180,190,200,200,200), kst                             ! isc36890
	!!     Solution to the cubic equation                                    isc36900
	!!     from CRC mathematical tables                                      isc36910
	!!     stability a&b (180)                                               isc36920
	!!
	!  180 a=-c * third
	!      b=(2./27.-(sz0/240.)**2) * d
	!      s=0.25*b**2 + a**3/27.
	!      if(.not. (s .lt. 0.)) then ! add by @creaqi goto 185 in fun modify_goto_pure
	!      s=sqrt(s)                                                        ! isc36970
	!!     e=1./3.                                                           isc36980
	! if (.not. ((-b/2.+s) .lt. 0.0)) then ! add by @creaqi 185 state_same == True
	!!     ba=(-b/2.+s)**e                                                   isc36990
	!      ba=(-b/2.+s)**third
	!!     bb=(-b/2.-s)**e                                                   isc37000
	!      bb=(-b/2.-s)**third
	!      y=ba+bb                                                          ! isc37010
	!182   xvz=y-1000. * third
	!      xvz=xvz * 0.001
	!      return                                                           ! isc37040
	!! 185 cs=(sz0/240.)**2*27./2.-1.                                        isc37050
	!       endif ! add by @creaqi 185 state_same == True
	!      endif !add by @creaqi label 185 modify_goto_pure
	!  185 cs=13.5 * (sz0/240.)**2. - 1.
	!      th=acos(cs) * third
	!      y=2.* third * cos(th) * 1000.
	!182   xvz=y-1000. * third
	!      xvz=xvz * 0.001
	!      return                                                           ! isc37040
	!!      go to 182                                                        ! isc37080comment by @creaqi
	!!     stability c(190)                                                  isc37090
	!190   xvz=sz0/c                                                        ! isc37100
	!      xvz=xvz * 0.001
	!      return                                                           ! isc37120
	!!     stability d,e,&f (200)                                            isc37130
	!200   xvz=sz0 * (d*sz0 + sqrt( (d*sz0)**2 + 4.*c**2 ) ) / (2.*c**2)
	!      xvz=xvz * 0.001
	!      return                                                           ! isc37170
	end                                                              ! isc37180
	! ---------------------------------------------------------------------
function xvz2(sz0,kst)                                           ! isc36640
	! ---------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 040503               XVZ
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  XVZ calculates the virtual distance necessary to        isc36660
	!               account for the initial vertical dispersion.            isc36670
	!               Based on Briggs urban dispersion coeffcients.           isc36680
	!               Adapted directly from ISC6-8.
	!
	! --- UPDATE
	! --- V5.72-V5.721  040503  (DGS): recast algebraic solution to improve
	!                                  single precision accuracy
	!
	! --- INPUTS:
	!
	!               SZ0 - real    - Initial dispersion coefficient (m).
	!               KST - integer - PGT stability index (1-6).
	!
	!     COMMON /CSIGMA/ variables:
	!     Briggs urban coeffs. for each PGT class for URBAN conditions.
	!             AZURB(6)  - real array - Urban Y coeffs. by PGT class.
	!            XIZURB(6)  - real array - Reciprocal length scale (1/m)
	!                                      for SIGZ for each PGT class.
	!
	! --- OUTPUTS:
	!
	!              XVZ  - real    - Pseudo-distance (km).
	!
	!
	! ---   XVZ called by:  SIGTZ
	! ---   XVZ calls:       none
	! ---------------------------------------------------------------------
	!                                                                       isc36650
	!    include 'csigma.puf'
	!
	dimension azurb(6),xizurb(6)                                     !          isc36850
	data azurb/1.e06,1.e06,.20,.14,.08,.08/                          !    isc36860
	data xizurb/1.e09,1.e09,0.,.0003,.0015,.0015/                    !     isc36870
	data third/0.3333333/
	!
	! --- Use above info to override values in /CSIGMA/
	c = azurb(kst)
	d = xizurb(kst)
	if(kst.le.2) then
		c = 1.0e06
		d = 1.0e09
	endif
	if(kst.eq.3) d = 0.0
	!
	!
	!     Direct solution for urban dispersion                              isc36880
	select case(kst)
	case(1)
	a=-1.0
	bby2=-sz0/(2.0*240.)
	! --- s2=bby2**2 + a**3/27.
	s2=bby2**2 - 1.0/27.
	s=sqrt(ABS(s2))
	a3=-bby2+s
	b3=-bby2-s
	if(.not. (s2 .lt. 0.)) then ! add by @creaqi goto 185 in fun modify_goto_pure
		if (.not. (a3 .lt. 0.0)) then ! add by @creaqi 185 state_same == True
			ba=a3**third
			bb=b3**third
			y=ba+bb                                                          ! isc37010
			xvz2=y*y-1.0
			return                                                           ! isc37040
			! --- cs=-(b/2)/SQRT(-a^3/27)
		endif ! add by @creaqi 185 state_same == True
	endif !add by @creaqi label 185 modify_goto_pure
	cs=-bby2*SQRT(27.)
	th=acos(cs) * third
	! --- y=2.* SQRT(-a/3) * cos(th)
	y=2.* SQRT(third) * cos(th)
	xvz2=y*y-1.0
	return                                                           ! isc37040
	!      go to 182                                                        ! isc37080comment by @creaqi
	!     stability c(190)                                                  isc37090
	case(2)
	a=-1.0
	bby2=-sz0/(2.0*240.)
	! --- s2=bby2**2 + a**3/27.
	s2=bby2**2 - 1.0/27.
	s=sqrt(ABS(s2))
	a3=-bby2+s
	b3=-bby2-s
	if(.not. (s2 .lt. 0.)) then ! add by @creaqi goto 185 in fun modify_goto_pure
		if (.not. (a3 .lt. 0.0)) then ! add by @creaqi 185 state_same == True
			ba=a3**third
			bb=b3**third
			y=ba+bb                                                          ! isc37010
			xvz2=y*y-1.0
			return                                                           ! isc37040
			! --- cs=-(b/2)/SQRT(-a^3/27)
		endif ! add by @creaqi 185 state_same == True
	endif !add by @creaqi label 185 modify_goto_pure
	cs=-bby2*SQRT(27.)
	th=acos(cs) * third
	! --- y=2.* SQRT(-a/3) * cos(th)
	y=2.* SQRT(third) * cos(th)
	xvz2=y*y-1.0
	return                                                           ! isc37040
	!      go to 182                                                        ! isc37080comment by @creaqi
	!     stability c(190)                                                  isc37090
	case(3)
	xvz=sz0/c                                                        ! isc37100
	xvz2=xvz * 0.001
	return                                                           ! isc37120
	!     stability d,e,&f (200)                                            isc37130
	case(4)
	xvz=sz0 * (d*sz0 + sqrt( (d*sz0)**2 + 4.*c**2 ) ) / (2.*c**2)
	xvz2=xvz * 0.001
	case(5)
	xvz=sz0 * (d*sz0 + sqrt( (d*sz0)**2 + 4.*c**2 ) ) / (2.*c**2)
	xvz2=xvz * 0.001
	case(6)
	xvz=sz0 * (d*sz0 + sqrt( (d*sz0)**2 + 4.*c**2 ) ) / (2.*c**2)
	xvz2=xvz * 0.001
	end select
	!170   go to (180,180,190,200,200,200), kst                             ! isc36890
	!!     Solution to the cubic equation                                    isc36900
	!!     from CRC mathematical tables                                      isc36910
	!!     stability a&b (180)                                               isc36920
	!!
	!! --- Solve for xvz in KM rather than M, using y**2=x+1
	!! --- substitution  (DGS)
	!  180 a=-1.0
	!      bby2=-sz0/(2.0*240.)
	!! --- s2=bby2**2 + a**3/27.
	!      s2=bby2**2 - 1.0/27.
	!      s=sqrt(ABS(s2))
	!      a3=-bby2+s
	!      b3=-bby2-s
	!      if(.not. (s2 .lt. 0.)) then ! add by @creaqi goto 185 in fun modify_goto_pure
	! if (.not. (a3 .lt. 0.0)) then ! add by @creaqi 185 state_same == True
	!      ba=a3**third
	!      bb=b3**third
	!      y=ba+bb                                                          ! isc37010
	!182   xvz2=y*y-1.0
	!      return                                                           ! isc37040
	!! --- cs=-(b/2)/SQRT(-a^3/27)
	!       endif ! add by @creaqi 185 state_same == True
	!      endif !add by @creaqi label 185 modify_goto_pure
	!185   cs=-bby2*SQRT(27.)
	!      th=acos(cs) * third
	!! --- y=2.* SQRT(-a/3) * cos(th)
	!      y=2.* SQRT(third) * cos(th)
	!182   xvz2=y*y-1.0
	!      return                                                           ! isc37040
	!!      go to 182                                                        ! isc37080comment by @creaqi
	!!     stability c(190)                                                  isc37090
	!190   xvz=sz0/c                                                        ! isc37100
	!      xvz2=xvz * 0.001
	!      return                                                           ! isc37120
	!!     stability d,e,&f (200)                                            isc37130
	!200   xvz=sz0 * (d*sz0 + sqrt( (d*sz0)**2 + 4.*c**2 ) ) / (2.*c**2)
	!      xvz2=xvz * 0.001
	!      return                                                           ! isc37170
	end                                                              ! isc37180
	!-----------------------------------------------------------------------
subroutine turbset(ldbhr,ustr,el,wstr,jdstab,dpbl,z0m,htmet,uatzi,&
	ws,wd,ix0,iy0,md0,ilw,&
	tsigv,tsigw,idopty,idoptz)
	!-----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 140521           TURBSET
	!                D. Strimaitis
	!
	! --- PURPOSE:  Determines turbulence velocities and associated
	!               dispersion options (backup if needed)
	!
	! --- UPDATE
	! --- V6.402-V6.42_v1.1 140521   : Apply averaging time factor to 
	!                                  lateral turbulence
	! --- V6.301-V6.402 101111  (DGS): Update GETCELL arguments
	! --- V6.3-V6.301   100827  (DGS): Allow nested grid domains that do not
	!                                  align with cells in parent domains
	! --- V6.11-V6.3    100212  (DGS): Add nested CALMET grids
	! --- V5.751-V6.11  060309  (DGS): Add overwater SVMIN,SWMIN (ILW arg.)
	! --- V5.75-V5.751  050805  (DGS): Add option to modify turbulence due
	!                                  to advection-decay mechanism
	!                                  (test implementation)
	! --- V5.3-V5.75    050225  (DGS): Add AERMOD turbulence profiles, new
	!                                  argument UATZI, and Zi(mech) estimate
	! --- V5.0-V5.3     991222a (DGS): Do not extract sigw data from profile
	!                                  when MTURBVW=1 or 4
	! --- V5.0-V5.0     980722  (DGS): SVMIN & SWMIN by stability class
	! --- V4.0-V5.0     971107  (DGS): skip call to XTPRF for nzprf=0
	!
	! --- INPUTS:
	!         LDBHR - logical - Debug output (T,F)
	!          USTR - real    - Friction velocity (m/s)
	!            EL - real    - Monin-Obukhov length (m)
	!          WSTR - real    - Convective velocity scale (m/s)
	!        JDSTAB - integer - Stability class for puff
	!          DPBL - real    - Depth of planetary boundary layer (current
	!                           mixing height, m)
	!           Z0M - real    - Roughness length (m)
	!         HTMET - real    - Height for evaluating turbulence (m)
	!         UATZI - real    - Wind speed at mixing layer top (m/s)
	!            WS - real    - Wind speed at puff center (m/s)
	!            WD - real    - Wind direction at puff center (deg)
	!           IX0 - integer - Cell x-index for current turbulence
	!           IY0 - integer - Cell y-index for current turbulence
	!           MD0 - integer - MET grid domain index for IX0,IY0
	!           ILW - integer - Current cell is land (1) or water (2)
	!
	! --- Parameters: IO6, MXPRFZ
	!     Common block /COMPARM/ variables:
	!            XMINZI, XMAXZI,SVMIN(6,2), SWMIN(6,2)
	! --- 6.42_x1.1 140521
	!     Common block /DISPDAT/ variables:
	!            AVGTIMFAC
	!     Common block /FLAGS/ variables:
	!            MDISP, MDISP2, MTURBVW, MCTURB, MTAUADV
	!     Common block /GRID/ variables:
	!            NX, NY, DGRIDI
	!     Common block /GRIDNEST/ variables:
	!            NESTFACMX, NESTFAC(mxmetdom)
	!            RNXNEST0(mxmetdom),RNYNEST0(mxmetdom)
	!     Common block /METHR/ variables:
	!            NZPRF, SVPRF(mxprfz), SWPRF(mxprfz), ZPRF(mxprfz), PTG(2)
	!            HTMIX(mxnx,mxny,mxmetdom), XMONIN(mxnx,mxny,mxmetdom),
	!            USTAR(mxnx,mxny,mxmetdom),WSTAR(mxnx,mxny,mxmetdom)
	!     Common block /METHD/ variables:
	!            Z0(mxnx,mxny,mxmetdom)
	!
	! --- OUTPUT:
	!         TSIGV - real    - Sigma-v (m/s)
	!         TSIGW - real    - Sigma-w (m/s)
	!        IDOPTY - integer - Current dispersion option for sigma-y
	!        IDOPTZ - integer - Current dispersion option for sigma-z
	!
	! --- TURBSET called by: COMP, RLSMET
	! --- TURBSET calls:     SIGWV, XTPRF, AERSWV, GETCELL
	!----------------------------------------------------------------------
	!
	! --- Include parameters
	include 'params.puf'
	!
	! --- Include common blocks
	include 'comparm.puf'
	! --- 6.42_x1.1 140521
	include 'dispdat.puf'
	include 'flags.puf'
	include 'grid.puf'
	include 'gridnest.puf'
	include 'methd.puf'
	include 'methr.puf'
	!
	logical ldbhr,lproblm
	data deg2rad/0.0174533/
	data zero/0.0/
	! --- Fix generic inverse coriolis parameter 1/f = 10000 for estimating
	! --- mechanical BL height
	data finv/10000./
	!
	! --- Initialize
	tsigw=-999.
	tsigv=-999.
	idopty=mdisp
	idoptz=mdisp
	!
	! --- Reset turbulence velocities to calculated values for MDISP=2
	if(mdisp.EQ.2 .OR. mdisp2.EQ.2) then
		if(mcturb.EQ.1) then
			call SIGWV(io6,ldbhr,ustr,el,wstr,dpbl,htmet,tsigw,tsigv)
		elseif(mcturb.EQ.2) then
			ziconv=dpbl
			zimech=0.3*ustr*finv
			zimech=AMIN1(zimech,dpbl)
			call AERSWV(io6,ldbhr,ustr,el,wstr,dpbl,ziconv,zimech,&
			uatzi,htmet,tsigw,tsigv)
		else
			stop 'TURBSET:  invalid MCTURB (must be 1,2)'
		endif
		! ---    Modify turbulence velocities for advection
		if(mtauadv.GT.10) then
			vvar=tsigv**2
			wvar=tsigw**2
			vmod=0.0
			wmod=0.0
			! ---       Center of current cell in Grid Domain 1 (outermost) units
			x0=(rnxnest0(md0)+ix0-0.5)/FLOAT(nestfac(md0))
			y0=(rnynest0(md0)+iy0-0.5)/FLOAT(nestfac(md0))
			wdrad=deg2rad*wd
			! ---       Set upwind range from center of current cell
			tmax=FLOAT(mtauadv)/0.7
			dmax=tmax*ws*dgridi
			! ---       Resolve to 10 steps per cell with the highest resolution
			n=10*NINT(dmax)*nestfacmx+1
			dr=dmax/n
			dx=dr*sin(wdrad)
			dy=dr*cos(wdrad)
			ix=ix0
			iy=iy0
			kg=md0
			do k=1,n
				x=x0+k*dx
				y=y0+k*dy
				call GETCELL('TURBSET     ','GRIDIJ',1,x,y,zero,zero,&
				i,j,md)
				if(i.NE.ix .OR. j.NE.iy .OR. md.NE.kg) then
					! ---             Crossed into new cell or new domain
					ix=i
					iy=j
					kg=md
					! ---             Decay factor
					factor=AMAX1(0.,1.-FLOAT(k)/FLOAT(n))
					! ---             Cell properties
					dpbla=AMAX1(htmix(ix,iy,kg),xminzi)
					dpbla=AMIN1(dpbla,xmaxzi)
					ela=xmonin(ix,iy,kg)
					ustra=ustar(ix,iy,kg)
					wstra=wstar(ix,iy,kg)
					z0ma=z0(ix,iy,kg)
					! ---             Turbulence velocities
					if(mcturb.EQ.1) then
						call SIGWV(io6,ldbhr,ustra,ela,wstra,dpbla,&
						htmet,tsigwa,tsigva)
					else
						ziconv=dpbla
						zimech=0.3*ustra*finv
						zimech=AMIN1(zimech,dpbla)
						call AERSWV(io6,ldbhr,ustra,ela,wstra,dpbla,&
						ziconv,zimech,uatzi,htmet,&
						tsigwa,tsigva)
					endif
					! ---             Keep largest increase in turbulence
					if(tsigva.GT.tsigv) then
						test=factor*(tsigva**2-vvar)
						vmod=AMAX1(vmod,test)
					endif
					if(tsigwa.GT.tsigw) then
						test=factor*(tsigwa**2-wvar)
						wmod=AMAX1(wmod,test)
					endif
				endif
			enddo
			! ---       Compute modified turbulence
			if(vmod.GT.0.) tsigv=SQRT(vvar+vmod)
			if(wmod.GT.0.) tsigw=SQRT(wvar+wmod)
			if(ldbhr) then
				tv0=SQRT(vvar)
				tw0=SQRT(wvar)
				write(io6,*) 
				write(io6,*) 'TURBSET:  IX,IY,MD = ',ix0,iy0,md0
				write(io6,*) '  original sigv,sigw = ',tv0,tw0
				write(io6,*) '  modified sigv,sigw = ',tsigv,tsigw
			endif
		endif
	endif
	! --- Set turbulence velocities from measured profiles for MDISP=1,5
	if(mdisp.EQ.1 .OR. mdisp.EQ.5) then
		sigw=-999.
		sigv=-999.
		if(nzprf.GT.0) then
			if(mturbvw.EQ.2 .OR. mturbvw.EQ.3) then
				! ---          Use measured sigma-w data
				lproblm=.FALSE.
				call XTPRF(nzprf,swprf,zprf,htmet,'sig',z0m,el,dpbl,&
				jdstab,ptg,sigw,lproblm)
				if(lproblm) sigw=-999.
			endif
			if(mturbvw.NE.2) then
				! ---          Use measured sigma-v data
				lproblm=.FALSE.
				call XTPRF(nzprf,svprf,zprf,htmet,'sig',z0m,el,dpbl,&
				jdstab,ptg,sigv,lproblm)
				if(lproblm) sigv=-999.
			endif
		endif
		! ---    Use measured turbulence if good, or use backup dispersion
		if(sigw.GE.0.0) then
			tsigw=sigw
		else
			idoptz=mdisp2
		endif
		if(sigv.GE.0.0) then
			tsigv=sigv
		else
			idopty=mdisp2
		endif
	endif
	! --- 6.42_x1.1 140521
	! --- Apply averaging-time factor
	if(tsigv.GT.0.0) then
		tv0=tsigv
		tsigv=tsigv*avgtimfac
		if(ldbhr) then
			write(io6,*) 
			write(io6,*) 'TURBSET: Averaging Time Factor = ',avgtimfac
			write(io6,*) '  Before applying minimum, sigma-v = ',tv0
			write(io6,*) '  is changed to            sigma-v = ',tsigv
			write(io6,*) 
		endif
	endif
	! --- Impose lower limit on turbulence (-999. does not survive!)
	tsigv=AMAX1(tsigv,svmin(jdstab,ilw))
	tsigw=AMAX1(tsigw,swmin(jdstab,ilw))
	!
	return
end
!----------------------------------------------------------------------
subroutine sigwv(io6,ldbhr,ustar,xl,wstar,zi,z,sigw,sigv)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 950630             SIGWV
	!
	!     ADAPTED from:
	! --- URBDEP   Version 1.2   Level 870113  SIGWV by J. Scire, SRC
	!
	! --- PURPOSE:  For dispersion option IDOPT=2 compute the
	!               standard deviation of vertical velocity (sigw) and the
	!               horizontal crosswind velocity (sigv) based on
	!               micrometeorological parameters.
	!
	! --- INPUTS:
	!
	!               IO6 - integer - Unit number for list file
	!             LDBHR - logical - Debug output (T,F)
	!             USTAR - real    - Friction velocity (m/s)
	!             WSTAR - real    - Convective velocity scale (m/s)
	!                XL - real    - Monin-Obukhov length (m)
	!                ZI - real    - Mixing height (m)
	!                 Z - real    - Plume height (m)
	!
	! --- OUTPUTS:
	!
	!              SIGW - real    - Standard deviation of vertical
	!                               velocity (m/s)
	!              SIGV - real    - Standard deviation of horizontal
	!                               crosswind velocity (m/s)
	!
	! --- SIGWV called by:  TURBSET
	! --- SIGWV calls:      none
	!
	!----------------------------------------------------------------------
	!
	logical ldbhr
	data xlmn/1.e-2/
	!
	sigv=0.0
	sigw=0.0
	!
	zdh=z/zi
	if(zdh.le.1.2) then
		! ---    Within MIXED/ENTRAINMENT layer, non-zero turbulence
		!
		if(xl.gt.0.0) then
			! ---       STABLE --> NEUTRAL section
			! ---       (minimum Monin-Obukhov length to prevent numerical problems)
			zdl=z/amax1(xl,xlmn)
			zdh2=amin1(zdh,1.0)
			cs=(1.-zdh2)**0.75
			an=exp(-0.9*zdh)
			! ---       Stable-neutral sigma w,v in surface layer
			t4i=1.0/(1.+zdl)
			sigw=1.3*ustar*(cs*zdl+an)*t4i
			sigv=ustar*(1.6*cs*zdl+1.8*an)*t4i
			!
		else
			! ---       UNSTABLE --> NEUTRAL section
			an2=(exp(-0.9*zdh))**2
			!
			if(zdh.ge.0.8) then
				! ---          Unstable-neutral sigma w,v in entrainment layer
				!                                   (0.8 zi <= z <= 1.2 zi)
				if(zdh.lt.1.0)then
					! ---             0.8 to 1.0 zi
					ac12=(0.5+(zi-z)/(0.4*zi))**2
				else
					! ---             1.0 to 1.2 zi
					ac12=(0.3333333+(1.2*zi-z)/(1.2*zi))**2
				endif
				t1=ustar*ustar*an2
				t2=wstar*wstar
				sigw=sqrt(1.15*t1+0.35*t2*ac12)
				sigv=sqrt(4.0*t1+0.35*t2)
				!
			elseif(zdh.ge.0.1) then
				! ---          Unstable-neutral sigma w,v in mixed layer
				!                                   (0.1 zi <= z <= 0.8 zi)
				t1=ustar*ustar*an2
				t3=0.35*wstar*wstar
				sigw=sqrt(1.15*t1+t3)
				sigv=sqrt(4.0*t1+t3)
				!
			else
				! ---          Unstable-neutral sigma w,v in surface layer
				!                                   (0.0 zi <= z <= 0.1 zi)
				! ---          (minimum M-O length to prevent numerical problems)
				! ---          (Monin-Obukhov length is negative -- ensure z/xl < 0.0)
				zdl=-z/amax1(abs(xl),xlmn)
				sigw=ustar*sqrt(1.6*an2+2.9*(-zdl)**0.6666667)
				sigv=sqrt(4.0*ustar*ustar*an2+0.35*wstar*wstar)
				!
			endif
		endif
		!
	endif
	! --- (above 1.2*zi, return zero turbulence)
	!
	if(LDBHR) then
		write(io6,*)'SIGWV:  (MDISP = 2)'
		write(io6,*)'      el,ustar,wstar= ',xl,ustar,wstar
		write(io6,*)'      z,zi          = ',z,zi
		write(io6,*)'      sigv,sigw     = ',sigv,sigw
		write(io6,*)'      (before minimum values imposed)'
	endif
	!
	return
end
!----------------------------------------------------------------------
subroutine sigset
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 141230            SIGSET
	!                R. Yamartino, J. Scire
	!
	! --- PURPOSE:  Computes several /CSIGMA/ common block variables during
	!               the setup phase.
	!               Pass turbulence averaging time factor to /DISPDAT/
	!
	! --- UPDATES
	! --- V6.42_x1.1 - TNG-7.1.0  141230  (DGS)
	!                                : create gridded roughness factors
	! --- V6.3-V6.42_x1.1  140521    : pass AVGTIMFAC to /DISPDAT/
	! --- V5.4-V6.3     100212  (DGS): add nested CALMET grids
	! --- V5.2-V5.4     000602  (DGS): add message to "stop"
	! --- V5.1-V5.2     991104  (JSS): Error messages written to list
	!                                  file as well as to screen
	! --- V5.0-V5.1     990625b (DGS): PGTIME is from control file
	!                   990625b (DGS): Move tests on AVET and PGTIME to
	!                                  QAINP
	!
	! --- INPUTS:
	!
	!     Common Block /CSIGMA/ variables used:
	!             AYPGT - real arr- PGT Y coeffs. for each PGT class.
	!             BYPGT - real arr- PGT Y expons. for each PGT class.
	!             AZPGT - real arr- PGT Z coeffs. for each PGT class.
	!             BZPGT - real arr- PGT Z expons. for each PGT class.
	!            NZBRUR - int. arr- Number segments per PGT class.
	!            XZBRUR - real arr- Junction x's (km) per PGT class.
	!             AZRUR - real arr- PGT Z coeffs. for each PGT class.
	!             BZRUR - real arr- PGT Z expons. for each PGT class.
	!     Common block /COMPARM/ variables:
	!          SVMIN
	!     Common block /DISPDAT/ variables:
	!          SYTDEP, AVET, PGTIME
	!     Common block /GRIDNEST/ variables:
	!          NGRID
	!     Common block /METHD/ variables:
	!          NXM(mxmetdom), NYM(mxmetdom), Z0(mxnx,mxny,mxmetdom) 
	!     Common block /PARAMS/ parameters:
	!          IO6, MMODEL, MXNX, MXNY, MXMETDOM
	!
	! --- OUTPUTS:
	!     Common block /DISPDAT/ variables:
	!          AVGTIMFAC
	!     Common block /CSIGMA/ variables:
	!          AYPGTI, BYPGTI
	!          SYH, XZBRUR(10,6), AVEFAC, AVEFACI
	!          YZ0FAC(mxnx,mxny,mxmetdom),
	!          AZ0FAC(mxnx,mxny,mxmetdom),
	!          BZ0TRM(mxnx,mxny,mxmetdom),
	!
	!
	! ---   SIGSET called by:  SETUP
	! ---   SIGSET calls:       none
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	include 'comparm.puf'
	include 'csigma.puf'
	include 'dispdat.puf'
	include 'flags.puf'
	include 'gridnest.puf'
	include 'methd.puf'
	data plaw/0.2/
	! --- Store averaging time factor into /DISPDAT/
	if(avet.NE.pgtime) then
		avgtimfac=(avet/pgtime)**plaw
		write(io6,*)
		write(io6,*)
		write(io6,*)
		write(io6,*)'****************** NOTE ***********************'
		write(io6,*)'Sigma-y values are adjusted for averaging time'
		write(io6,*)'by multiplying the ambient sigma-v by ',&
		avgtimfac
		write(io6,*)'Averaging time (AVET) (minutes)        : ',avet
		write(io6,*)'Base averaging time (PGTIME) (minutes) : ',pgtime
		write(io6,*)
		write(io6,*)'Adjusted sigma-v is subject to the minimum '
		write(io6,*)'sigma-v (SVMIN) (m/s) set in the control file:'
		write(io6,*)'Stability   A      B      C      D      E      F'
		write(io6,'(a8,6f7.3)')' Land : ',(svmin(k,1),k=1,6)
		write(io6,'(a8,6f7.3)')' Water: ',(svmin(k,2),k=1,6)
		write(io6,*)'***********************************************'
		write(io6,*)
		write(*,*)
		write(*,*)'Note --  Sigma-y values are adjusted for '//&
		'averaging time'
		write(*,*)'See list file (search for * NOTE *)'
		write(*,*)
	else
		avgtimfac=1.0
	endif
	! --- Initialize roughness arrays to NO adjustment
	az0fac=1.0
	bz0trm=-1000.0
	yz0fac=1.0
	! --- Assign averaging time adjustment factor array for PG sigma-y,
	! --- based on the averaging time "avet" (minutes)
	avefac=avgtimfac
	avefaci=1.0/avgtimfac
	! --- Initialize test for maximum roughness factor for sigma-y
	ymaxfac=0.0
	! --- Compute reciprocals that are not adjusted
	do i=1,6! add by @creaqi do label 10
		aypgti(i) = 1.0 / aypgt(i)
		bypgti(i) = 1.0 / bypgt(i)
	enddo !add by @creaqi 10
	10 continue
	! --- Compute sigma-z X-transition values (no roughness dependence)
	do 20 i=1,6
		jmax = nzbrur(i) - 1
		if(jmax.eq.0) cycle ! add by @creaqi replace goto do label with cycle goto 20 in sigset
		!
		do j=1,jmax! add by @creaqi do label 15
			jp1 = j+1
			arat = azrur(j,i) / azrur(jp1,i)
			bdiff = bzrur(jp1,i) - bzrur(j,i)
			xcut = 1.0e20
			if(bdiff.gt.0.0) xcut = arat**(1.0/bdiff)
			if(bdiff.lt.0.0) xcut = (1.0 / arat)**(-1.0/bdiff)
			!     write(6,*) 'i,j,arat,bdiff,xcut = ',i,j,arat,bdiff,xcut
			xzbrur(j,i) = xcut
		enddo !add by @creaqi 15
		15 continue
		!
		20 continue
		! --- PG and MESOPUFF roughness adjustment (if option selected)
		! --- (Adapted in part from AUSPLUME subroutine Z0FIX)
		if(mrough.EQ.1) then
			write(*,*)'Looping over cells for PG roughness adjustments'
			! ---    Loop over all cells, all met domains, for roughness lengths
			do md=1,ngrid
				do j=1,nym(md)
					do i=1,nxm(md)
						if(z0(i,j,md).LE.0.0) then
							write(io6,*)'Invalid roughness (z0) found in SIGSET',&
							z0(i,j,md)
							write(*,*)
							stop 'Halted in SIGSET -- see list file.'
						endif
						! ---       Additive term to adjust B-exponent of sigma-z
						bz0trm(i,j,md)=0.0777+.0215*alog(z0(i,j,md))
						! ---       Factor to adjust A-coefficient of sigma-z
						az0fac(i,j,md)=(1000.**bz0trm(i,j,md))*&
						1.585*z0(i,j,md)**0.1301
						! ---       PG and MESOPUFF sigma-y factor
						yz0fac(i,j,md)=(z0(i,j,md)/.03)**0.2
						ymaxfac=MAX(ymaxfac,yz0fac(i,j,md))
					enddo
				enddo
			enddo
			write(*,*)'END Loop over all cells for roughness adjustments'
		else
			ymaxfac=1.0
		endif
		!
		! --- Sigma-y at which Heffter growth begins can cause numerical
		! --- problems if set too large: reset large values here
		! --- Set largest value of SYTDEP(m) that is consistent with the final
		! --- adjustments just computed, and with the largest value of sigma-y
		! --- that can be computed for PG(rural) stability class F using
		! --- the standard curves.  (syhmax=avefac*100000. to nearest 1000 m)
		imax=NINT(avefac*ymaxfac*100.)
		syhmax=imax*1000.
		if(sytdep.GT.syhmax) then
			write(io6,*)
			write(io6,*)
			write(io6,*)
			write(io6,*)'------------------------------------------'
			write(io6,*)'           QA ALERT  !! '
			write(io6,*)'------------------------------------------'
			write(io6,*)'--  Sigma-y at Heffter transition is    --'
			write(io6,*)'--  too large.  SYTDEP has been RESET!  --'
			write(io6,*)'    Old Value (m): ',sytdep
			write(io6,*)'    New Value (m): ',syhmax
			write(io6,*)'    Time-Avg Factor : ',avefac
			write(io6,*)'    Roughness Factor: ',ymaxfac
			write(io6,*)'------------------------------------------'
			write(io6,*)
			sytdep=syhmax
		endif
		! --- Transfer SYTDEP from /DISPDAT/ to /CSIGMA/ as SYH
		syh = sytdep
		return
end
	!----------------------------------------------------------------------
subroutine setcsig(idopty,idoptz,iru,uavg,kst,el,bvf,&
	tsigv,tsigw,symin,szmin,&
	zht,zmix,ix,iy,md)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 141230           SETCSIG
	!                D. Strimaitis, SRC
	!
	! --- PURPOSE:  Set selected parameters for calls to sigma routines,
	!               and place in /CSIGMA/
	!
	! --- UPDATES
	! --- V5.75 - TNG-7.1.0  141230  (DGS)
	!                                : Add grid-cell for roughness adj.
	! --- V5.7-V5.75    050225  (DGS): Add TAULY call for computing the
	!                                  Lagrangian time scale for lateral
	!                                  growth for Draxler Fy;
	!                                  Pull MHFTSZ, MTAULY from /FLAGS/
	! --- V5.4-V5.7     030402  (DGS): add virtual time for Heffter
	!                                  transition for SIGMA-Y
	!
	! --- INPUTS:
	!     IDOPTY - integer   - Dispersion method option for SIGY **
	!     IDOPTZ - integer   - Dispersion method option for SIGZ **
	!                          **  computed from:
	!                               1,2 = SIGMA V,W
	!                                 3 = PG curves (rural), MP (urban)
	!                                 4 = MESOPUFF II curves (rural),
	!                                     MP (urban)
	!                                 5 = CTDM (neutral/stable),
	!                                     IDOPT(1) (other)
	!                               (All IDOPT - use Heffter eqns. for
	!                                long travel times)
	!        IRU - integer   - Rural cell indicator (rural=0 ; urban=1)
	!       UAVG - real      - Mean transport speed (m/s)
	!        KST - integer   - PGT stability class at puff
	!         EL - real      - Current Monin-Obukhov length (m)
	!        BVF - real      - Current Brunt-Vaisala freq (1/s)
	!      TSIGV - real      - Current sigma-v velocity (m/s)
	!      TSIGW - real      - Current sigma-w velocity (m/s)
	!      SYMIN - real      - Minimum value of sigma-y (m)
	!      SZMIN - real      - Minimum value of sigma-z (m)
	!        ZHT - real      - Height above ground for TAULY (m)
	!       ZMIX - real      - Mixing height for TAULY (m)
	!   IX,IY,MD - real      - Current cell (and met domain)
	!
	!       Common block /CSIGMA/ variables:
	!             syh
	!       Common block /FLAGS/ variables:
	!             MHFTSZ - integer   - Flag indicating use of Heffter growth
	!                                  for z
	!                              (0: NO Heffter   1: Heffter)
	!             MTAULY - integer   - Flag for calling TAULY subroutine
	!                              (1: call TAULY   not 1: no call)
	!
	!       Parameters:  MXNX,MXNY,MXMETDOM
	!
	! --- OUTPUT:
	!       Common block /CSIGMA/ variables:
	!             idoptys,idoptzs,irus,uavgs,kstabs,els,bvfs,mhftszs,
	!             tsigys,tsigzs,symins,szmins,thfty,
	!             tyidr,ixcell,iycell,mdcell
	!
	! --- SETCSIG called by:  CALCSL, SLGRECS, PLGRECS, PUFRECS,
	!                         POINTS1, LINES1, POINTS2, LINES2,
	!                         SETPUF, SETSLG, WAKE_XSIG, FLARES
	! --- SETCSIG calls:      SIGTY, TAULY
	!
	!----------------------------------------------------------------------
	!
	! --- Include common blocks
	include 'params.puf'
	include 'csigma.puf'
	include 'flags.puf'
	logical ldb
	data zero/0.0/
	! --- Set debug for TAULY
	ldb=.FALSE.
	!      ldb=.TRUE.
	! --- Assign input variables to names in /CSIGMA/
	idoptys=idopty
	idoptzs=idoptz
	irus   =iru
	uavgs  =uavg
	kstabs =kst
	els    =el
	bvfs   =bvf
	mhftszs=mhftsz
	tsigvs =tsigv
	tsigws =tsigw
	symins =symin
	szmins =szmin
	ixcell =ix
	iycell =iy
	mdcell =md
	! --- Condition stability class to 1-6
	if(kstabs.GT.6) kstabs=6
	! --- Compute lateral time scale
	if(mtauly.EQ.1) then
		call TAULY(ldb,tsigv,tsigw,el,zmix,zht,tau)
		tyidr=1./(1.62*tau)
	endif
	! --- Calculate virtuals at Heffter transition
	! --- Process sigma-y; if vertical distribution is Gaussian and Heffter
	! --- is used, sigma-z must be treated in call to HEFTRAN
	szh=syh
	call SIGTY(syh,zero,zero,dum,thfty,dhfty)
	return
end
!----------------------------------------------------------------------
subroutine heftran(meth,zht,sy,sz,tvz1,tvz2,tvy1,tvy2)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 000602           HEFTRAN
	!                D. Strimaitis,  SRC
	!
	! --- PURPOSE:  Define sigmas and corresponding virtual times at which
	!               transition to the Heffter time-based sigma functions
	!               is made
	!
	! --- UPDATES
	! --- V5.3-V5.4     000602  (DGS): add message to "stop"
	!
	! --- INPUTS:
	!          METH - integer - Method used to obtain SZH:
	!                            1 = Time to SYH
	!                            2 = Scale distance along slug
	!           ZHT - real    - Current puff/slug height above ground (m)
	!         SY,SZ - real    - Current puff/slug sigmas (m)
	!     TVZ1,TVZ2 - real    - Virtual time for sigma-z at older (1) and
	!                           younger (2) ends of slug (s)
	!     TVY1,TVY2 - real    - Virtual time for sigma-y at older (1) and
	!                           younger (2) ends of slug (s)
	!
	!     Common block /CSIGMA/ variables:
	!           SYH, SZH, AZT(6),
	!           UAVGS, MHFTSZS, SZMINS, KSTABS
	!     Parameters:
	!           MXPUFF, IO6
	!
	! --- OUTPUT:
	!     Common block /CSIGMA/ variables:
	!           SZH, THFTY, THFTZ
	!
	! --- HEFTRAN called by:  SETPUF, SETSLG, SLGRECS
	! --- HEFTRAN calls:      SIGTY, SIGTZ
	!----------------------------------------------------------------------
	! --- Notes:
	!
	!       The transition to the Heffter curves is made when sigma-y
	!       grows to SYH, a value provided in the input file.  At this
	!       point, BOTH sigma-y and sigma-z begin to follow the time-based
	!       Heffter growth rate.  Because dispersion regimes may change
	!       one or more times before SYH is attained, the virtual times
	!       at the transition point for sigma-y and sigma-z can be very
	!       different, and a wide range of "SZH" values may be associated
	!       with the single SYH.
	!
	!       Two methods are needed for identifying SZH.  The FIRST adresses
	!       the case in which the current sigma-y and sigma-z are known,
	!       and we use the current sigma-y dispersion curve to identify the
	!       time to reach SYH.  This time is then used to find SZH at the
	!       same point.  The coresponding virtual times THFTY and THFTZ are
	!       computed.
	!
	!       The SECOND addresses the case in which sigmas and the
	!       corresponding virtuals (time/distance) are known at both
	!       ends of a slug, and we must define the virtual time/distance
	!       to the transition point along the axis of the slug.  The
	!       length of the slug provides the metric for translating THFTY
	!       to a position along the axis.  This position is then used to
	!       obtain the corresponding THFTZ.  SZH is computed from THFTZ.
	! ---------------------------------------------------------------------
	!
	include 'params.puf'
	include 'csigma.puf'
	data zero/0.0/
	! --- Store Heffter switch for sigma-z
	mhftsz=mhftszs
	if(meth.EQ.1) then
		! ---    Compute THFTY, SZH, and THFTZ for /CSIGMA/
		! ---    Assume SYH already valid in /CSIGMA/
		!
		! ---    Set the sigma-y virtuals at which Heffter puff growth begins
		call sigty(syh,zero,zero,dum,thfty,dhfty)
		! ---    Find sigma-y virtuals for current "puff"
		call sigty(sy,zero,zero,dum,ty,dy)
		! ---    Time to reach transition from this point
		thftr=thfty-ty
		!
		if(mhftszs.EQ.0) then
			! ---       Heffter NOT used: fill sigma-z values from sigma-y
			szh=syh
			thftz=thfty
		else
			! ---       Find sigma-z and virtual time at Heffter transition
			! ---       Calls to SIGTZ are for times before transition, so
			! ---       temporarily set switch to OFF
			mhftszs=0
			if(thftr.GT.0.0) then
				! ---          Puff is not within Heffter regime
				! ---          Find sigma-z virtuals for current "puff"
				call sigtz(sz,zero,zero,zht,dumy,tz,dz)
				! ---          Add time/distance(km) to reach transition from this point
				time=tz+thftr
				dkm=dz+thftr*uavgs*0.001
				! ---          Compute sigma-z at transition (SZH)
				call sigtz(zero,dkm,time,zht,szh,dumz,dumz)
				! ---          Set time to transition (THFTZ)
				thftz=time
			elseif(thftr.EQ.0.0) then
				! ---          Puff is at the transition to the Heffter regime
				szh=sz
				! ---          Find sigma-z virtuals for current "puff"
				call sigtz(sz,zero,zero,zht,dumy,thftz,dheftz)
			else
				! ---          Puff is within Heffter regime (THFTR not positive)
				szhsq=sz*sz+thftr*(azt(kstabs))**2
				if(szhsq.GT.szmins**2) then
					szh=sqrt(szhsq)
					call sigtz(szh,zero,zero,zht,dumy,thftz,dhftz)
				else
					! ---             Travel from current "time" back to transition is too
					! ---             large for current rate of growth, so virtuals are zero
					szh=szmins
					thftz=zero
				endif
			endif
			! ---       Restore switch setting
			mhftszs=mhftsz
		endif
	elseif(meth.EQ.2) then
		! ---    Compute new THFTZ for /CSIGMA/ (will not need SZH)
		! ---    Assume THFTY already valid in /CSIGMA/
		if(tvy1.NE.tvy2) then
			thftz=tvz2+(thfty-tvy2)*(tvz1-tvz2)/(tvy1-tvy2)
		else
			! ---       Restore value consistent with SZH
			! ---       Temporarily set switch to OFF
			mhftszs=0
			call sigtz(szh,zero,zero,zht,dumy,thftz,dhftz)
			! ---       Restore switch setting
			mhftszs=mhftsz
		endif
	else
		write(io6,*) 'FATAL ERROR --- HEFTRAN'
		write(io6,*) 'Invalid Method: METH = ',meth
		write(*,*)
		stop 'Halted in HEFTRAN -- see list file.'
	endif
	return
end
!----------------------------------------------------------------------
subroutine rombtin(lwflux,tlower,tupper,x,y,z,zpr,tfacc,&
	syb,sye,szrb,szre,hlid,hlidmax,eps,&
	ss1,ss2,ss3,ss4)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 090710           ROMBTIN
	!                R. Yamartino
	!
	! --- PURPOSE:  Performs ROMBerg Time INtegration of the sequence of
	!               slug snapshots over the interval (TLOWER,TUPPER) via
	!               calls to the slug quardrture routine TRAPSL.
	!
	!               Adapted to integration of multiple quantities from
	!               QROMB Module of ISC2 Short Term Model - ISCST2
	!
	!               which performs Romberg Integration of Function Using
	!               Polynomial Extrapolation for h=0 With h1(i)=h1(i-1)/4
	!               Modifed To Use Variable Order Extrapolation
	!
	!               (as programmed on July 7, 1993 by:
	!                    Jeff Wang, Roger Brode  and
	!                    Adapted From Codes By Richard Strelitz, CSC)
	!
	! --- UPDATE
	! --- V5.0-V6.267   090710  (DGS): add HLIDMAX for use in VCOUP
	! --- V4.0-V5.0     971107  (DGS): add eps variable to calling args
	!
	!
	! --- INPUTS:
	!
	!            LWFLUX - logical - Receptor specific wet deposition flag
	!                                .true. if calculation is to be made
	!                                .false. if not.
	!            TLOWER - real    - Lower limit (m) for the integration.
	!            TUPPER - real    - Upper limit (m) for the integration.
	!                 X - real    - X coord. of receptor.
	!                 Y - real    - Y coord. of receptor.
	!                 Z - real    - Z coord. of receptor.
	!               ZPR - real    - Height (m) of the slug above receptor
	!                               terrain height (allowing for terr adj)
	!           SYB,SYE - real    - Sigma Y at receptor at beginning and end
	!                               of time period.
	!         SZRB,SZRE - real    - Sigma Z at receptor at beginning and end
	!                               of time period.
	!              HLID - real    - Relevant mixing depth (m) at receptor.
	!           HLIDMAX - real    - Maximum lid height (m) for mass
	!                               above HLID
	!               EPS - real    - Tolerance limit for integral convergence
	!
	!
	! --- OUTPUTS:
	!
	!            SS1    - real    - Coupling coeff. (s/m**3) #1 = CCQB
	!              CCQB - real    - Coupling coefficient (s/m**3) for
	!                               the source rate ,QB, at the
	!                               beginning of the time step.
	!            SS2    - real    - Coupling coeff. (s/m**3) #2 = CCDQ
	!              CCDQ - real    - Coupling coefficient (s/m**3) for
	!                               the change in source rate, DQ, between
	!                               the beginning and end of the time step.
	!            SS3    - real    - Coupling coeff. (s/m**2) #3 = CCIZQB
	!            CCIZQB - real    - Z-integrated coefficient (s/m**2) for
	!                               the source rate ,QB, at the
	!                               beginning of the time step.
	!            SS4    - real    - Coupling coeff. (s/m**2) #4 = CCIZDQ
	!            CCIZDQ - real    - Z-integrated coefficient (s/m**2) for
	!                               the change in source rate, DQ, between
	!                               the beginning and end of the time step.
	!
	!
	! --- ROMBTIN called by:  SLUGAVE
	! --- ROMBTIN calls:      TRAPSL, POLINT
	!----------------------------------------------------------------------
	! --- Variable declarations
	PARAMETER (K1 = 5, JMAX1 =10, ITMAX =100, EPS2 = 1.0E-20)
	! *** PARAMETER (K1 = 5, JMAX1 =10, ITMAX =100, EPS = 1.0E-4,
	! ***&           EPS2 = 1.0E-20)
	!**   K1    = Order of Extrapolating Polynomial
	!**   JMAX1 = Maximum Number of Iterations in Halving Interval
	!**   ITMAX = Maximum Number of Integral Iterations
	!**   EPS   = Tolerance Limit for Convergence of the Integral
	!**   EPS2  = Lower Threshold Limit for the Value of the Integral
	REAL S1(21), H1(21)
	real s2(21), s3(21), s4(21)
	logical lwflux
	data zero/0.0/
	h1(1) = 1
	s1(1) = zero
	s2(1) = zero
	s3(1) = zero
	s4(1) = zero
	call trapsl(lwflux,tlower,tupper,x,y,z,&
	zpr,tfacc,syb,sye,szrb,szre,hlid,hlidmax,&
	s1(1),s2(1),s3(1),s4(1),1)
	ss1 = s1(1)
	ss2 = s2(1)
	ss3 = s3(1)
	ss4 = s4(1)
	do j = 2,jmax1
		h1(j) = 0.25*h1(j-1)
		! ---    Obtain samples for all four integrals at once.
		call trapsl(lwflux,tlower,tupper,x,y,z,&
		zpr,tfacc,syb,sye,szrb,szre,hlid,hlidmax,&
		s1(j),s2(j),s3(j),s4(j),j)
		kp = MIN0(j,k1)-1
		! ---    Estimate all four integrals at once.
		call polint(h1(j-kp),s1(j-kp),kp+1,ss1,dss1)
		call polint(h1(j-kp),s2(j-kp),kp+1,ss2,dss2)
		call polint(h1(j-kp),s3(j-kp),kp+1,ss3,dss3)
		call polint(h1(j-kp),s4(j-kp),kp+1,ss4,dss4)
		!***********************************************************************
		!        Check The Convergence Criteria:
		!        EPS is tolerance level for convergence of the integral,
		!          initially set = 1.0E-4 in a PARAMETER statement in MAIN1.INC;
		!        EPS2 is lower threshold for the integral, initially set = 1.0E-10
		!          in a PARAMETER statement in MAIN1.INC;
		!        J is number of halving intervals and must be at least 3 for
		!          convergence criteria to be met.  Maximum number of intervals
		!          is set by JMAX1 (=10).
		!***********************************************************************
		! ---    Consider the convergence of integrals 1 & 3
		if((abs(dss3).LE.eps*abs(ss3) .OR. abs(ss3*dss3).LE.eps2)&
			.AND. j.GE.3) then
			if((abs(dss1).LE.eps*abs(ss1) .OR. abs(ss1*dss1).LE.eps2)&
				.AND. j.GE.3) exit ! add by @creaqi break the loop goto 999 in rombtin with len equal 1
		endif
	enddo
	999  return
end
!----------------------------------------------------------------------
subroutine trapsl(lwflux,tlower,tupper,x,y,z,&
	zpr,tfacc,syb,sye,szrb,szre,hlid,hlidmax,&
	val1,val2,val3,val4,n)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 090710            TRAPSL
	!                R. Yamartino
	!
	! --- PURPOSE:  Performs Trapezoidal integration of polygon segment
	!               in the upwind distance domain (TLOWER,TUPPER) via
	!               calls to the slug snapshot routine SLUGSNP.
	!
	!               Adapted to integration of multiple quantities from
	!               TRAPZD Module of ISC2 Short Term Model - ISCST2
	!               which performs standard trapezoidal integration for 2-d
	!               integrals.
	!
	!               (as programmed on July 7, 1993 by:
	!                    Jeff Wang, Roger Brode  and
	!                    Adapted From Codes By Richard Strelitz, CSC)
	!
	! --- UPDATES:
	! --- V5.0-V6.267   090710  (DGS): add HLIDMAX for use in VCOUP
	!
	! --- INPUTS:
	!
	!            LWFLUX - logical - Receptor specific wet deposition flag
	!                                .true. if calculation is to be made
	!                                .false. if not.
	!            TLOWER - real    - Lower limit (m) for the integration.
	!            TUPPER - real    - Upper limit (m) for the integration.
	!                 X - real    - X coord. of receptor.
	!                 Y - real    - Y coord. of receptor.
	!                 Z - real    - Z coord. of receptor.
	!               ZPR - real    - Height (m) of the slug above receptor
	!                               terrain height (allowing for terr adj)
	!           SYB,SYE - real    - Sigma Y at receptor at beginning and end
	!                               of time period.
	!         SZRB,SZRE - real    - Sigma Z at receptor at beginning and end
	!                               of time period.
	!              HLID - real    - Relevant mixing depth (m) at receptor.
	!                 N - integer - Order of the integration.
	!
	! --- OUTPUTS:
	!
	!           VAL1    - real    - Coupling coeff. (s/m**3) #1
	!           VAL2    - real    - Coupling coeff. (s/m**3) #2
	!           VAL3    - real    - Coupling coeff. (s/m**2) #3
	!           VAL4    - real    - Coupling coeff. (s/m**2) #4
	!
	! --- TRAPSL called by:  ROMBTIN
	! --- TRAPSL calls:      SLUGSNP
	!----------------------------------------------------------------------
	! --- Variable declarations
	real del, sum1, sum2, sum3, sum4, sval1, sval2, sval3, sval4
	save neval2, sval1, sval2, sval3, sval4
	logical lwflux
	data zero/0.0/
	if(n.EQ.1) then
		call slugsnp(lwflux,tupper,x,y,z,&
		zpr,tfacc,sye,szre,hlid,hlidmax,dccizqb,vert)
		temp = dccizqb * vert
		! ---    ccqb = ccqb + temp
		sum1 = temp
		! ---    ccdq = ccdq + temp * tupper
		sum2 = temp * tupper
		! ---    ccizqb = ccizqb + dccizqb
		sum3 = dccizqb
		! ---    ccizdq = ccizdq + dccizqb * tupper
		sum4 = dccizqb * tupper
		!
		call slugsnp(lwflux,tlower,x,y,z,&
		zpr,tfacc,syb,szrb,hlid,hlidmax,dccizqb,vert)
		temp = dccizqb * vert
		sum1 = sum1 + temp
		sum2 = sum2 + temp * tlower
		sum3 = sum3 + dccizqb
		sum4 = sum4 + dccizqb * tlower
		!
		del = tupper-tlower
		sval1 = zero
		sval2 = zero
		sval3 = zero
		sval4 = zero
		neval2 = 1
	else
		del = (tupper-tlower)/neval2
		t1 = tlower+del*0.5
		!        sum = 0.0
		sum1 = zero
		sum2 = zero
		sum3 = zero
		sum4 = zero
		!
		do i = 1,neval2
			! ---       Interpolate the receptor specific sigmas.  8/31/94
			sy = syb   +  t1 * (sye  - syb )
			sz = szrb  +  t1 * (szre - szrb)
			!
			call slugsnp(lwflux,t1,x,y,z,&
			zpr,tfacc,sy,sz,hlid,hlidmax,dccizqb,vert)
			!
			temp = dccizqb * vert
			sum1 = sum1 + temp
			sum2 = sum2 + temp * tlower
			sum3 = sum3 + dccizqb
			sum4 = sum4 + dccizqb * tlower
			!
			t1 = t1+del
		enddo
		neval2 = neval2*2
	endif
	!
	val1 = 0.5 * (sval1 + del*sum1)
	sval1 = val1
	!
	val2 = 0.5 * (sval2 + del*sum2)
	sval2 = val2
	!
	val3 = 0.5 * (sval3 + del*sum3)
	sval3 = val3
	!
	val4 = 0.5 * (sval4 + del*sum4)
	sval4 = val4
	!
	return
end
!----------------------------------------------------------------------
subroutine slugave(ldbhr,lwflux,dt,x,y,z,zpr,&
	tfacc,syrb,szrb,syre,szre,hlid,hlidmax,&
	ccqb,ccdq,ccizqb,ccizdq)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 090710           SLUGAVE
	!                R. Yamartino
	!                Modified 7/90 -- J. Scire
	!                Polygon areas -- RJY 12/28/93
	!                Modified for receptor specific sigmas -- RJY 8/31/94
	!                                                         DGS 8/31/94
	!                Adaptive time integration -- RJY 3/21/95
	!                Remove area-source treatment -- DGS 6/30/95
	!
	! --- PURPOSE:  Computes the time-average coupling coefficients (s/m**3)
	!               at the point (x,y,z) over the time period T to T+DT
	!                (where DT is a time step and TDDT
	!                is the fraction of DT that is elapsed).
	!
	! --- UPDATE
	! --- V5.0-V6.267   090710  (DGS): add HLIDMAX for use in VCOUP and
	!                                  allow receptor to sample mass above
	!                                  lid
	! --- V4.0-V5.0     971107  (DGS): add debug switch to SLUGINT call
	!                   971107  (DGS): add /COMPARM/ with "eps" variable for
	!                                  use in ROMBTIN
	!                   971107  (DGS): pass ICODE to VCOUP from /CURRENT/
	! --- V4.0-V4.07    971107  (DGS): add PDF logic for VCOUP calls
	! --- V4.0-V5.0     971107  (DGS): package slug geometry in SLGFRAC
	!
	!
	! --- INPUTS:
	!
	!             LDBHR - logical - Debug output? (T/F)
	!            LWFLUX - logical - Receptor specific wet deposition flag
	!                                .true. if calculation is to be made
	!                                .false. if not.
	!                DT - real    - Time elapsed in moving slug ends from
	!                               (XB1,YB1,ZB1) to (XE1,YE1,ZE1) and
	!                               (XB2,YB2,ZB2) to (XE2,YE2,ZE2) plus
	!                               companion changes in end point sigmas.
	!         (X, Y, Z) - real    - Coordinates of the receptor (m)
	!               ZPR - real    - Height (m) of the slug above receptor
	!                               terrain height (allowing for terr adj)
	!             TFACC - real    - Cross-slug T factor to account for shear
	!       (SYRB,SZRB) - real    - Sigmas at the receptor before T factor.
	!                               At beginning of time step.
	!       (SYRE,SZRE) - real    - Sigmas at the receptor before T factor.
	!                               At end of time step.
	!              HLID - real    - Relevant mixing depth (m) at receptor.
	!           HLIDMAX - real    - Maximum lid height (m) for mass
	!                               above HLID
	!
	!     Common Block /COMPARM/ variables:
	!        EPSSLUG
	!     Common Block /CURRENT/ variables:
	!        XB1, YB1, ZB1, SYB1, SZB1,
	!        XE1, YE1, ZE1, SYE1, SZE1,
	!        XB2, YB2, ZB2, SYB2, SZB2,
	!        XE2, YE2, ZE2, SYE2, SZE2,
	!        IAGE, SPEEDI, SRAT, TEMIS, ICODE
	!     Common Block /PDF/ variables:
	!        LPDF, SWUPF, SWDNF, SZUPB, SZDNB, WTUP, WTDN, ZUP, ZDN, RFACSQ
	!     Parameters:
	!        none
	!
	! --- OUTPUTS:
	!
	!              CCQB - real    - Coupling coefficient (s/m**3) for
	!                               the source rate ,QB, at the
	!                               beginning of the time step.
	!              CCDQ - real    - Coupling coefficient (s/m**3) for
	!                               the change in source rate, DQ, between
	!                               the beginning and end of the time step.
	!            CCIZQB - real    - Z-integrated coefficient (s/m**2) for
	!                               the source rate ,QB, at the
	!                               beginning of the time step.
	!            CCIZDQ - real    - Z-integrated coefficient (s/m**2) for
	!                               the change in source rate, DQ, between
	!                               the beginning and end of the time step.
	!
	! --- SLUGAVE called by:  CALCSL
	! --- SLUGAVE calls:      VCOUP, SLUGINT, SLGTLIM, SLUGSNP, XERFDIF, ERF
	!                         ROMBTIN, SLGFRAC
	!----------------------------------------------------------------------
	!
	! --- Include common blocks
	include 'params.puf'
	include 'comparm.puf'
	include 'current.puf'
	include 'pdf.puf'
	! --- Some variables passed to LSSLINT via SLGLIN common block
	COMMON /SLGLIN/ DXY12,RDXY12
	!
	logical ldbhr,lwflux
	!
	data srthaf/0.7071/,half/0.5/,small/1.0e-10/
	data srt2pi/2.506628/,fourth/0.25/
	data erfcut/3.0/,sigcut/3.0/
	!
	ccqb = 0.0
	ccdq = 0.0
	ccizqb = 0.0
	ccizdq = 0.0
	!
	! ----------------------------------------------------------------
	! --- Consider the special case of the current period's emissions.
	! ----------------------------------------------------------------
	IF(IAGE.EQ.0) THEN
		!
		! ---    Determine receptor position relative to final slug end-point
		! ---    Note that there is no change in (X2,Y2) over time.
		call SLGFRAC(x,y,xe1,ye1,xb2,yb2,rhoa,rhoc,fracs,dxy12,rdxy12)
		! ---    Constrain FRACS to be 0.0 to 1.0 for the actual slug.
		FRACS = AMAX1(0.0,FRACS)
		FRACS = AMIN1(1.0,FRACS)
		! ---    Note that in x-y space the coordinates of the point (XP,YP,ZP)
		!        corresponding to rhoc = 0 (i.e., rhoa along the slug axis) are
		!        XP = X2  +  RHOA * SINOM  Not used
		!        YP = Y2  +  RHOA * COSOM  Not used
		! OR     XP = X2  +  FRACS * (X1 - X2)  IF WITHIN SLUG BOUNDS.
		! OR     YP = Y2  +  FRACS * (Y1 - Y2)  IF WITHIN SLUG BOUNDS.
		!
		! ---    Note that for Z coordinates, we take receptor-specific value
		zp = zpr
		!
		! ***    Replace with actual values @ receptor. T factor=1.0 for IAGE=0
		! ---    12/3/89 Protect against divide by sigma of zero.
		sy = amax1(syre,small)
		sz = amax1(szre,small)
		! ***
		!
		! ***    All coordinates ready ! now compute coupling coefficient for
		!        a unit source (Q = 1 ug/sec)
		!
		! ---    Check if any vertical coupling.
		!        vcoup(icode,z,zp,sz,hlid) includes the 1/sqrt(2*pi) and
		!        the sum over all reflection terms.
		if(LPDF .AND. z.LE.hlid) then
			szsq=sz**2
			szdn=SQRT(szsq*swdnf+szdnb*rfacsq)
			szup=SQRT(szsq*swupf+szupb*rfacsq)
			vert = wtdn*VCOUP(icode,z,zdn,szdn,hlid,hlidmax) +&
			wtup*VCOUP(icode,z,zup,szup,hlid,hlidmax)
		else
			vert = vcoup(icode,z,zp,sz,hlid,hlidmax)
		endif
		!*****
		if(ldbhr) then
			write(io6,*)'SLUGAVE -- New slug --'
			write(io6,*)'        sz,z,zp = ',sz,z,zp
			write(io6,*)'      hlid,vert = ',hlid,vert
		endif
		!*****
		!
		! ---    No impact if no vertical coupling AND if wet flux not modeled
		if(.NOT.lwflux .and. vert.LT.small) return
		!
		! ---    Check if any cross-slug distance coupling.
		SRAT2 = SRAT * SRAT
		PHIC = RHOC / SY
		EXPY = 0.0
		IF(ABS(PHIC) .LE. SIGCUT) EXPY = EXP(-HALF*SRAT2*PHIC*PHIC)
		!*****
		if(ldbhr) then
			write(io6,*)'        sy,rhoc = ',sy,rhoc
			write(io6,*)'      srat,expy = ',srat,expy
		endif
		!*****
		!
		! ---    No impact if no significant cross-slug coupling
		IF(EXPY .LT. SMALL) RETURN
		!
		! ---    Check if any along-slug distance coupling.
		! ---    12/3/89 Protect against divide by sigma of zero.
		PHIA2 = SRTHAF * RHOA / amax1(SYB2,small)
		ETA2 = SRTHAF * (RHOA - DXY12) / SY
		! ---    For ETA1, I need the slug length at the beginning of the
		!        time step.  This is usually zero but may not be if we allow
		!        for prolonged steady-state releases somehow. however, this
		!        would then require a mechanism for telling this routine
		!        that IAGE > 0 but all conditions (e.g., met., source strength)
		!        are unchanged. since this may never be implemented, assume zero
		!        slug length and set ETA1 = PHIA2
		! ***    Modify eta1 to use sigma y at receptor rather than at source
		eta1 = srthaf * rhoa / sy
		! ***
		! ---    Eliminate cases beyond the causal frontier.
		! i.e.   IF(ETA1.GT.ERFCUT .AND. ETA2.GT.ERFCUT) RETURN for downwind
		!        receptors, but in general (including upwind receptors)
		!        consider the following approach.
		PROD = ETA2 * ETA1
		!*****
		! *** write(6,*)'average subr. -- eta1 = ',eta1,'  eta2 = ',eta2,
		! ***1 '  prod = ',prod
		!*****
		! ***    SIGCUT in following is reset to ERFCUT 3/8/89
		IF(PROD.GT.0.0 .AND. ABS(ETA1).GT.ERFCUT .AND.&
			ABS(ETA2).GT.ERFCUT) RETURN
		!
		ERFA2 = 1.0
		IF(PHIA2.LT.0.0) ERFA2 = -1.0
		IF(ABS(PHIA2) .LE. ERFCUT) ERFA2 = ERF(PHIA2)
		!
		!
		! ---    Compute the causality factors FCAUS0 AND FCAUS1.
		! ***    vmws = speedi * srat
		! ***    coeff = sy / ( vmws * dt )
		coeff = SRTHAF/(eta1-eta2)
		coeff2 = coeff * coeff
		call xerfdif(eta2,eta1,xint0,xint1)
		fcaus0 = half * erfa2  +  srthaf * coeff * xint0
		fcaus1 = fourth * erfa2  -  coeff2 * xint1  +&
		srthaf * coeff2 * ( rhoa / sy ) * xint0
		!
		!*****
		! *** write(6,*)'average subr. -- fcaus = ',fcaus,'  erfa2 = ',erfa2,
		! ***1 '  coeff = ',coeff,'  eta2 = ',eta2,'  erfet2 = ',erfet2,
		! ***2 '  eta1 = ',eta1,'  erfet1 = ',erfet1,'  expet2 = ',expet2,
		! ***3 '  expet1 = ',expet1,'  srtpi = ',srtpi
		!*****
		!
		! ---    Compute the time-average conc. terms for unit emission rate !
		ccoup = expy  / (srt2pi * speedi * sy)
		if(ccoup .lt. small) return
		!
		if(fcaus0 .gt. small) then
			ccizqb = ccoup * fcaus0
			ccqb = vert * ccizqb
		endif
		!
		if(fcaus1 .gt. small) then
			ccizdq = ccoup * fcaus1
			ccdq = vert * ccizdq
		endif
		!
		!*****
		! *** write(6,*)'SLUGAVE -- New slug -- vert = ',vert,
		! ***1 '  expy = ',expy,'  speedi = ',speedi,'  sy = ',sy,
		! ***2 '  sz = ',sz,'  iage = ',iage
		!*****
		!
		return
	endif
	!
	! ---------------------------------------------------------------
	!
	! --- The following section provides several methods to time average
	!     the more general case of IAGE > 0.
	!
	! --- SLUGINT computes the average concentration CONC at the point
	!     (X,Y,Z) over the time T to T+DT (where DT is a time step)
	!
	! --- Include the effect of the crosswind T factor on sy. 4/5/89
	!     Note that the following statement allows sy to experience growth
	!     but not shrinkage.  To allow shrinkage as well, one need only have
	!     sy = syr * tfacc
	! --- Note that tfacc is cumulative and therefore represents the effect
	!     of all growths and shrinkages.
	! --- NOTE same limitation used in SUBR. SLGXLIM
	!
	syb = syrb * amax1(1.0,tfacc)
	sye = syre * amax1(1.0,tfacc)
	call slugint(ldbhr,iint,lwflux,x,y,z,&
	zpr,tfacc,syb,szrb,sye,szre,hlid,hlidmax,&
	ccqb,ccdq,ccizqb,ccizdq)
	!
	! --- IINT gives the status of the integration;
	!          = -1 implies that numerical integration must be done.
	!          =  0 implies that concs vanish and no further work be done.
	!          = +1 implies that concs are correct as is.
	!
	!*****
	if(ldbhr) then
		write(io6,*)'SLUGINT result (-1:Do numerical; 0:No concs; 1:Done)'
			write(io6,*)'        IINT = ',iint
		endif
		!*****
		if(iint.GE.0) return
		!
		! ---------------------------------------------------------------
		! ---------------------------------------------------------------
		!
		! --- Do a numerical integration.
		!
		! --- SLGTLIM computes the integration start and stop times
		!     over the time T to T+DT (where DT is a time step)
		!     Use average sigma-y at receptor over the step
		!
		symax=amax1(syb,sye)
		call slgtlim(x,y,symax,itf,tstart,tend)
		!
		! --- ITF gives the status of the time limits;
		!          =  0 implies that CONC = 0 and do not integrate.
		!          = +1 implies that the integration should be done
		!               with limits    T + TSTART * DT
		!               and            T + TEND * DT.
		!*****
		if(ldbhr) then
			write(io6,*)'SLGTLIM result (1:Do numerical; 0:No concs)'
				write(io6,*)'     X,Y,SYMAX = ',x,y,symax
				write(io6,*)'           ITF = ',itf
				write(io6,*)'TSTART,TEND,DT = ',tstart,tend,dt
			endif
			!*****
			if(itf.EQ.0) return
			!
			! --- Use the trapezoidal rule to integrate.
			ccqb = 0.0
			ccdq = 0.0
			ccizqb = 0.0
			ccizdq = 0.0
			!
			! --- Evaluate time integral for this interval. RJY            3/21/95
			call rombtin(lwflux,tstart,tend,x,y,z,zpr,tfacc,&
			syb,sye,szrb,szre,hlid,hlidmax,epsslug,&
			ccqb,ccdq,ccizqb,ccizdq)
			!
			!*****
			! *** write(6,*)'SUBR. SLUGAVE -- CCQB = ',ccqb,' CCDQ = ',ccdq,
			! ***1 ' CCIZQB = ',ccizqb,' CCIZDQ = ',ccizdq,' DSAMP = ',dsamp
			! ****
			!
			RETURN
END
		!----------------------------------------------------------------------
subroutine slugsnp(lwflux,tddt,x,y,z,zpr,tfacc,syr,szr,&
	hlid,hlidmax,dccizqb,vert)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 090710           SLUGSNP
	!                R. Yamartino
	!
	! --- PURPOSE:  Computes the instantaneous coupling coefficient
	!               (s/m**3) at the point (x,y,z) at the time T + TDDT*DT
	!               (where DT is a time step and TDDT
	!               is the fraction of DT that is elapsed).
	!
	! --- UPDATE
	! --- V5.0-V6.267   090710  (DGS): add HLIDMAX for use in VCOUP and
	!                                  allow receptor to sample mass above
	!                                  lid
	! --- V5.0-V5.0     980918 (DGS) : add area source distribution factor
	! --- V5.0-V5.0     980430 (DGS) : 'Use' TFACC (compiler warning)
	! --- V4.0-V5.0     971107  (DGS): switch from slug-end sig-y to
	!                                  receptor-specific sig-y in causality
	!                   971107  (DGS): pass ICODE to VCOUP from /CURRENT/
	! --- V4.0-V4.07    971107  (DGS): add PDF logic for VCOUP calls
	! --- V4.0-V5.0     971107  (DGS): package slug geometry in SLGFRAC
	!
	! --- INPUTS:
	!
	!            LWFLUX - logical - Receptor specific wet deposition flag
	!                                .true. if calculation is to be made
	!                                .false. if not.
	!              TDDT - real    - Fraction (0 to 1) of time step elapsed
	!         (X, Y, Z) - real    - Coordinates of the receptor (m)
	!               ZPR - real    - Height (m) of the slug above receptor
	!                               terrain height.
	!             TFACC - real    - Cross-slug T factor to account for shear
	!         (SYR,SZR) - real    - Sigmas at the receptor (m).
	!              HLID - real    - Relevant mixing depth (m) at receptor.
	!           HLIDMAX - real    - Maximum lid height (m) for mass
	!
	!     Common Block /CURRENT/ variables:
	!        XB1, YB1, ZB1, SYB1, SZB1,
	!        XE1, YE1, ZE1, SYE1, SZE1,
	!        XB2, YB2, ZB2, SYB2, SZB2,
	!        XE2, YE2, ZE2, SYE2, SZE2,
	!        SPEEDI, SRAT, ICODE,
	!     Common Block /PDF/ variables:
	!        LPDF, SWUPF, SWDNF, SZUPB, SZDNB, WTUP, WTDN, ZUP, ZDN, RFACSQ
	!     Parameters:
	!        IO6
	!
	! --- OUTPUTS:
	!
	!           DCCIZQB - real    - Z-integrated coefficient (s/m**2)
	!                               for an arbitrary source rate.
	!              VERT - real    - The Z coupling coefficient (1/m).
	!
	! --- SLUGSNP called by:  SLUGAVE
	! --- SLUGSNP calls:      VCOUP, ERF, SLGFRAC, ASDF
	!----------------------------------------------------------------------
	!
	! --- Include common blocks
	include 'params.puf'
	include 'current.puf'
	include 'pdf.puf'
	!
	logical lwflux,ldb
	!
	data srthaf/0.7071/,half/0.5/,small/1.0e-10/
	data erfcut/3.0/,sigcut/3.0/,srt2pi/2.506628/
	! --- Local control of debug output
	data ldb/.FALSE./
	! --- Determine the current coordinates of the slug end-points.
	X1 = XB1 + TDDT * (XE1 - XB1)
	Y1 = YB1 + TDDT * (YE1 - YB1)
	!
	X2 = XB2 + TDDT * (XE2 - XB2)
	Y2 = YB2 + TDDT * (YE2 - YB2)
	! --- Determine the length and orientation of the slug, and
	! --- define the position of the receptor relative to slug end 2
	! --- Convert receptor position to along-slug and cross-slug units
	call SLGFRAC(x,y,x1,y1,x2,y2,rhoa,rhoc,fracs,dxy12,rdxy12)
	! --- Constrain FRACS to be 0.0 to 1.0 for the actual slug.
	FRACS = AMAX1(0.0,FRACS)
	FRACS = AMIN1(1.0,FRACS)
	! --- note that in x-y space the coordinates of the point (XP,YP,ZP)
	!     corresponding to RHOC = 0 (i.e., RHOA along the slug axis) are
	!     XP = X2  +  RHOA * SINOM  Not used
	!     YP = Y2  +  RHOA * COSOM  Not used
	! OR  XP = X2  +  FRACS * (X1 - X2)  if within slug bounds.
	! OR  YP = Y2  +  FRACS * (Y1 - Y2)  if within slug bounds.
	!
	! --- Note that we use the receptor-specific slug height
	zp = zpr
	!
	! --- Use sigma values at receptor as interpolation done in SLUGAVE
	! --- before call.
	sy=syr
	sz=szr
	!
	! --- Include the effect of the crosswind T factor on syint.
	!     Note that the following statement allows sy to experience growth
	!     but not shrinkage.  To allow shrinkage as well, one need only have
	!     sy = syint * tfacc
	!     sy = syint * amax1(1.0,tfacc)
	! --- 'Use' TFACC without applying it to sy
	tfacc=tfacc
	! *** All coordinates ready ! Now compute coupling coefficient for
	!     a unit source (Q = 1 ug/sec)
	dccizqb = 0.0
	!
	! --- Check if any vertical coupling.
	!     vert = vcoup(icode,z,zp,sz,hlid) includes the 1/sqrt(2*pi) and
	!     the sum over all reflection terms.
	if(LPDF .AND. z.LE.hlid) then
		szsq=sz**2
		szdn=SQRT(szsq*swdnf+szdnb*rfacsq)
		szup=SQRT(szsq*swupf+szupb*rfacsq)
		vert = wtdn*VCOUP(icode,z,zdn,szdn,hlid,hlidmax) +&
		wtup*VCOUP(icode,z,zup,szup,hlid,hlidmax)
	else
		vert = vcoup(icode,z,zp,sz,hlid,hlidmax)
	endif
	!
	! *** No longer always skip if no vertical coupling !!  11/12/88
	IF(.not.lwflux .and. vert .LT. SMALL) return ! add by @creaqi goto return clb is_return
	!
	!
	SRAT2 = SRAT * SRAT
	! --- Check if any cross-slug distance coupling.
	PHIC = RHOC / SY
	EXPY = 0.0
	IF(ABS(PHIC) .LE. SIGCUT) EXPY = EXP(-HALF * SRAT2 * PHIC * PHIC)
	IF(EXPY .LT. SMALL) return ! add by @creaqi goto return clb is_return
	!
	! --- Check if any along-slug distance coupling.
	PHIA2 = SRTHAF * RHOA / SY
	PHIA1 = SRTHAF * (RHOA - DXY12) / SY
	PROD = PHIA2 * PHIA1
	IF(PROD.GT.0.0 .AND. ABS(PHIA1).GT.ERFCUT .AND.&
		ABS(PHIA2).GT.ERFCUT) return ! add by @creaqi goto return clb is_return
	!
	ERFA1 = 1.0
	IF(PHIA1.LT.0.0) ERFA1 = -1.0
	!     Note that for ERFCUT = 3, ERF(ERFCUT) = 0.999978
	IF(ABS(PHIA1) .LE. ERFCUT) ERFA1 = ERF(PHIA1)
	!
	ERFA2 = 1.0
	IF(PHIA2.LT.0.0) ERFA2 = -1.0
	IF(ABS(PHIA2) .LE. ERFCUT) ERFA2 = ERF(PHIA2)
	! --- Compute the causality factor FCAUS.
	FCAUS = HALF * (ERFA2 - ERFA1)
	IF(FCAUS .LT. SMALL) return ! add by @creaqi goto return clb is_return
	!
	! --- Compute the snapshot concentration for unit emission rate,
	! --- including correction factor for cross-slug distribution from
	! --- area sources
	dccizqb = ASDF(ldb,rhoc,sy)*EXPY*FCAUS/(srt2pi*SPEEDI*SY)
	! --- Debug output
	if(LDB) then
		write(io6,*)'Snapshot -- slugsnp = ',dccizqb,'  vert = ',vert,&
		'  expy = ',expy,'  fcaus = ',fcaus,&
		'  speedi = ',speedi,'  sy = ',sy,'  sz = ',sz
	endif
	!
	!
	100 RETURN
END
!----------------------------------------------------------------------
SUBROUTINE SLGXLIM(ILOW,IHGH,JLOW,JHGH,tfacc)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 140913           SLGXLIM
	!                R. Yamartino, SRC
	!                Modified by J. Scire, SRC 7/12/90
	!                Generalized to polygon area sources 12/29/93
	!
	! --- PURPOSE:  For a given slug's initial and final positions, compute
	!               the window of receptor space which should be examined.
	!
	!               This version computes such a window for both grided
	!               and non-gridded receptors.
	!
	! --- UPDATE
	! --- V5.0-TNG-7.0.0   140913    : Fix typo is screening on the young
	!                                  end at the end of the step (location
	!                                  of the young end at the beginning
	!                                  was used with sigma-y at end)
	! --- V4.0-V5.0     971107  (DGS): adjust istype range to recognize
	!                                  variable line sources (6)
	! --- V4.0-V5.0     971107  (DGS): revise limits on range of impact
	!
	! --- INPUTS:
	!
	!             TFACC - real    - Dimensionless lateral distortion factor
	!
	!     Common Block /CURRENT/ variables:
	!        XB1, YB1, XE1, YE1, XB2, YB2, XE2, YE2
	!        SYB1, SYE1, SYB2, SYE2
	!        IAGE, SPEEDI, SRAT, TEMIS
	!     Common Block /recgrd/ variables:
	!        NX,DX,XLEFT,XRGHT,NY,DY,YBOT,YTOP
	!
	! --- OUTPUTS:
	!
	!             ILOW  - integer - Minimum x receptor no. to be considered.
	!             IHGH  - integer - Maximum x receptor no. to be considered.
	!             JLOW  - integer - Minimum y receptor no. to be considered.
	!             JHGH  - integer - Maximum y receptor no. to be considered.
	!
	!     Common Block /SLGBND/ variables:
	!        XLOW,XHGH,YLOW,YHGH
	!
	! --- SLGXLIM called by:  CALCSL
	! --- SLGXLIM calls:       none
	!----------------------------------------------------------------------
	!
	! --- SUPPLEMENTAL NOTES
	!
	! --- THIS SUBROUTINE RETURNS THE I,J RECEPTOR LOCATION BOUNDS THAT
	!     NEED TO BE SEARCHED FOR SIGNIFICANT IMPACT.
	!
	!     These bounds are computed by looking at the slug endpoints and
	!     adding an additional piece, SIGTRY, defined as
	!              SIGTRY = SIGCUT * SY * tfaccl        ,
	!     to ensure that receptors receiving only partial impacts are
	!     included.  Note that SIGCUT is in a DATA statement and SY
	!     represents the several relevant values.  The quantity, TFACCL,
	!     accounts for any enhanced horizontal spreading due to shear by
	!     considering the T factor, TFACC, and applying the condition,
	!              tfaccl = amax1(1.0,tfacc)            .
	!
	! --- CORRESPONDING TO THE OUTPUT BOUNDS (ILOW,IHGH,JLOW,JHGH)
	!     WILL ALSO BE THE COORDINATE BOUNDS (XLOW,XHGH,YLOW,YHGH)
	!     FOR NON-GRIDDED RECEPTORS.  THIS INFO WILL BE IN /SLGBND/
	include 'params.puf'
	include 'current.puf'
	COMMON/SLGBND/ XLOW,XHGH,YLOW,YHGH
	!
	! --- ALSO ASSUME RECEPTOR GRID INFO IN /RECGRD/
	COMMON/RECGRD/ NX,DX,XLEFT,XRGHT,NY,DY,YBOT,YTOP
	!
	! --- SET SIGCUT AS THE MAX. NO. OF STD. DEVS.
	DATA SIGCUT/3.0/
	! --- Set BIG number to use for initial high/low marker (m)
	data xbig/1.0e20/
	!
	! --- Limit local value of tfacc to .ge. one.  See SUBR. SLUGAVE
	tfaccl = amax1(1.0,tfacc)
	!
	! --- INITIALIZE BOUNDS AT UNPHYSICAL VALUES OFF GRID.
	XLOW =  xbig
	XHGH = -xbig
	YLOW =  xbig
	YHGH = -xbig
	!
	! --- WORK FIRST WITH THE OLDEST END (1) OF THE SLUG AT THE END
	!     (NEW or E) OF THE TIME STEP, AS THIS END HAS THE LARGEST
	!     SIGMAS AND HENCE IS LIKELY TO ESTABLISH THE LARGEST WINDOW
	!     MOST QUICKLY AND THUS CUT DOWN ON THE AMOUNT OF COMPUTATION.
	! --- N.B. J. SCIRE CONVENTION: 1 = OLD END OF PUFF } SAME AS RJY
	!                               2 = NEW END OF PUFF }
	!
	SIGTRY = SIGCUT * SYE1 * tfaccl
	XTRY = XE1 + SIGTRY
	IF(XTRY .GT. XHGH) XHGH = XTRY
	XTRY = XE1 - SIGTRY
	IF(XTRY .LT. XLOW) XLOW = XTRY
	!
	YTRY = YE1 + SIGTRY
	IF(YTRY .GT. YHGH) YHGH = YTRY
	YTRY = YE1 - SIGTRY
	IF(YTRY .LT. YLOW) YLOW = YTRY
	!
	! --- NOW CONSIDER THE OLDEST END (1) OF THE SLUG AT THE BEGINNING
	!     OF THE TIME STEP.  NOW SKIP EXPLICIT DEFINITION OF XTRY,YTRY
	!     TO SAVE INSTRUCTIONS.
	SIGTRY = SIGCUT * SYB1 * tfaccl
	IF((XB1+SIGTRY) .GT. XHGH) XHGH = XB1+SIGTRY
	IF((XB1-SIGTRY) .LT. XLOW) XLOW = XB1-SIGTRY
	IF((YB1+SIGTRY) .GT. YHGH) YHGH = YB1+SIGTRY
	IF((YB1-SIGTRY) .LT. YLOW) YLOW = YB1-SIGTRY
	!
	! --- NOW CONSIDER THE NEWEST END (2) OF THE SLUG AT THE END
	!     OF THE TIME STEP.  NOW SKIP EXPLICIT DEFINITION OF XTRY,YTRY
	!     TO SAVE INSTRUCTIONS.
	SIGTRY = SIGCUT * SYE2 * tfaccl
	IF((XE2+SIGTRY) .GT. XHGH) XHGH = XE2+SIGTRY
	IF((XE2-SIGTRY) .LT. XLOW) XLOW = XE2-SIGTRY
	IF((YE2+SIGTRY) .GT. YHGH) YHGH = YE2+SIGTRY
	IF((YE2-SIGTRY) .LT. YLOW) YLOW = YE2-SIGTRY
	!
	! --- NOW CONSIDER THE NEWEST END (2) OF THE SLUG AT THE BEGINNING
	!     OF THE TIME STEP ONLY IF THIS END NOT TIED TO SOURCE.
	!     TO SAVE INSTRUCTIONS.
	IF(IAGE.GT.0) THEN
		SIGTRY = SIGCUT * SYB2 * tfaccl
		IF((XB2+SIGTRY) .GT. XHGH) XHGH = XB2+SIGTRY
		IF((XB2-SIGTRY) .LT. XLOW) XLOW = XB2-SIGTRY
		IF((YB2+SIGTRY) .GT. YHGH) YHGH = YB2+SIGTRY
		IF((YB2-SIGTRY) .LT. YLOW) YLOW = YB2-SIGTRY
	ENDIF
	!
	! --- Now consider the effect of a polygon area source.    12/29/93
	!     This is a very rough approx,  likely to overestimate the domain.
	if(iage.EQ.0 .AND. (istype.GE.3 .AND. istype.LE.6)) then
		xshft = vecmax(nside,xvert) - vecmin(nside,xvert)
		yshft = vecmax(nside,yvert) - vecmin(nside,yvert)
		xhgh = xhgh + xshft
		xlow = xlow - xshft
		yhgh = yhgh + yshft
		ylow = ylow - yshft
	endif
	! --- Compute corresponding gridded receptor indicies
	ilow = NINT((xlow - xleft)/dx)+1
	ihgh = NINT((xhgh - xleft)/dx)+1
	jlow = NINT((ylow - ybot)/dy)+1
	jhgh = NINT((yhgh - ybot)/dy)+1
	if(ihgh.LT.1 .OR. jhgh.LT.1 .OR.&
		ilow.GT.nx .OR. jlow.GT.ny) then
		! ---    No overlap; set "high" index <0 and "low" index = 1
		! ---    to deactivate do-loops in subsequent routines
		ihgh=-1
		jhgh=-1
		ilow= 1
		jlow= 1
	else
		! ---    Impact zone overlaps gridded receptors, so trim to actual range
		if(ilow.LT.1)  ilow = 1
		if(ihgh.GT.nx) ihgh = nx
		if(jlow.LT.1)  jlow = 1
		if(jhgh.GT.ny) jhgh = ny
	endif
	!*****
	! *** write(6,*)
	! *** write(6,*)'ILOW = ',ilow,' IHGH = ',ihgh
	! *** write(6,*)'JLOW = ',jlow,' JHGH = ',jhgh
	!*****
	!
	RETURN
END
!----------------------------------------------------------------------
FUNCTION VCOUP(ICODE,ZR,ZS,SZ,HLID0,HLIDMAX0)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 090710             VCOUP
	!                R. Yamartino
	!
	! --- PURPOSE:  Computes the vertical coupling coefficient for a source
	!               at height ZS, to a receptor at height ZR given a plume
	!               with sigma z of SZ and including reflections from the
	!               ground and lid at height HLID0.
	!
	! --- UPDATE
	! --- V5.7-V6.267   090710  (DGS): Treat cases of an elevated receptor
	!                                  above HLID0 assuming a uniform
	!                                  distribution between HLID0 and the
	!                                  top of the puff HLIDMAX0 (vcoup is
	!                                  still zero above HLID0 for puff code
	!                                  4 because HLIDMAX0 is not defined)
	!
	! --- V5.0-V5.7     030402  (DGS): Screen output for 'zero' sigma_z
	!
	! --- V4.0-V5.0     971107  (DGS): Invoke unlimited mixing when adjusted
	!                                  puff ht exceeds the current relecting
	!                                  lid ht (receptors below stack base
	!                                  can result in effective ZS>HLID)
	!                   971107  (DGS): Include puff code and associated
	!                                  logic to treat well-mixed puffs
	!
	!
	! --- INPUTS:
	!
	!             ICODE - integer - Puff code
	!                                1,11: within mixed layer & Gaussian
	!                                2,12: within mixed layer & uniform
	!                                3,13: above mixed layer & Gaussian
	!                                4,14: above mixed layer & uniform
	!                                5,15: currently above mixed layer
	!                                    (but previously below) & Gaussian
	!                                6,16: currently above mixed layer
	!                                    (but previously below) & uniform
	!                                99:   off computational grid
	!                ZR - real    - Z-coordinate of receptor (m)
	!                ZS - real    - Z-coordinate of source (m)
	!                SZ - real    - Z-sigma at receptor (m)
	!             HLID0 - real    - Mixing depth at receptor (m)
	!          HLIDMAX0 - real    - Top of any mass distribution (m)
	!                               usually for well-mixed cases
	!                               (HLIDMAX0 >= HLID0)
	!              Note that these input values must have same units.
	!
	!
	! --- OUTPUTS:
	!
	!             VCOUP - real    - Vertical coupling coefficient (1/m)
	!
	! --- VCOUP called by:  VCBAR, SLUGAVE, SLUGSNP, SLUGINT
	!                       LSSLINT, CALCPF, CAV_SAMP, PLMFOG
	! --- VCOUP calls:      none
	!----------------------------------------------------------------------
	!
	! --- All heights have same units.      1/26/89
	!
	data small/1.0e-10/,srttpi/2.5066283/,pi/3.1415926/
	data zunlim/9.9e10/
	! --- Set minimum sigma-z (m)
	data szcut/1.0e-2/
	
	! --- Treat well-mixed puff/slug codes first
	! ------------------------------------------
	iicode=icode
	if(iicode.GT.10) iicode=iicode-10
	if(iicode.EQ.2 .OR. iicode.EQ.6) then
		! ---    Within past/present mixed layer and uniform
		vcoup=0.0
		if(zr.LE.hlid0) then
			! ---       Distribution in surface layer
			vcoup=1./hlid0
		elseif(zr.LE.hlidmax0) then
			! ---       Distribution above surface layer
			vcoup=1./(hlidmax0-hlid0)
		endif
		return
	elseif(iicode .EQ. 4) then
		! ---    Above mixed layer and uniform (no defined concs)
		vcoup=0.0
		return
	endif
	! --- Gaussian Distribution
	! -------------------------
	vcoup = 0.0
	!
	! --- Remove mixing lid if source ht > mixing ht
	! --- (Unlimited mixing for GP above current lid)
	if(zs.GT.hlid0) then
		hlid=zunlim
		hlidmax=zunlim
	else
		hlid=hlid0
		hlidmax=AMAX1(hlid0,hlidmax0)
	endif
	! --- Case of receptor above current lid sampling mass aloft
	! --- (Either zero or mixed aloft)
	if(zr.GT.hlid) then
		if(zr.LE.hlidmax) then
			vcoup=1./(hlidmax-hlid)
		else
			vcoup=0.0
		endif
		return
	endif
	! --- Screen for sigma_z that is 'zero'
	if(sz.LT.szcut) return
	!
	! --- Check for significant trapping
	if(.not. ((sz/hlid).gt.0.63)) then ! add by @creaqi goto 15 in fun modify_goto_pure
		!
		! --- Sum the reflection terms
		sz1 = sz + small
		sz2 = sz*sz + small
		x = -0.5*(zr-zs)**2/sz2
		if(x.lt.-20.0) return ! add by @creaqi goto return clb is_return
		expz = exp(x)
		x = -0.5*(zr+zs)**2/sz2
		if(x.gt.-20.0) expz = expz + exp(x)
		!
		do j = -1 , +1 , 2! add by @creaqi do label 10
			zrefl = 2.0*float(j)*hlid
			x = -0.5*(zr+zs+zrefl)**2/sz2
			if(x.gt.-20.0) expz = expz + exp(x)
			x = -0.5*(zr-zs+zrefl)**2/sz2
			if(x.gt.-20.0) expz = expz + exp(x)
		enddo !add by @creaqi 10
		10       continue
		!
		vcoup = expz/(srttpi*sz1)
		return ! add by @creaqi goto return clb is_return
		!
		! --- Near uniform mixing using approximation of R. Yamartino
		!     (JAPCA 27, 5, MAY 1977)
	endif !add by @creaqi label 15 modify_goto_pure
	15   szsb = -0.5*(pi*sz/hlid)**2
	if(szsb.gt.-20.0) then
		beta = exp(szsb)
		beta2 = beta*beta
		expz = (1.0-beta2)*(1.0+beta2+2.0*beta*cos(pi*zs/hlid)*&
		cos(pi*zr/hlid))
	else
		expz = 1.0
	endif
	vcoup = expz/hlid
	!
	20   return
end
!----------------------------------------------------------------------
function vcbar(ip,mfact0,hlid,ppcoef,lcalm)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 090710             VCBAR
	!                R. Yamartino, D. Strimaitis
	!
	! --- PURPOSE:  Computes the space- and time-averaged vertical coupling
	!               coefficient, via calls to VCOUP.  A slug is sampled at
	!               each end, both at the start and end of the step.  A puff
	!               is sampled at the start, mid-pt, and end of the step.
	!               If terrain is modeled with MCTADJ=1/3, then the sampling
	!               uses the terrain elevation array at these points.
	!
	! --- UPDATE
	! --- V6.22-V6.267  090710  (DGS): Add max lid height to VCOUP args
	!                                  (Equals HLID here since ZGR=0)
	! --- V5.73-V6.22   070921  (DGS): Require LCALM=FALSE to disable the
	!                                  'frozen puff' treatment, since this
	!                                  feature was not fully implemented
	!                                  due to unassigned value of LCALM
	! --- V5.4-V5.73    040611  (DGS): add gravitational settling for one
	!                                  particle size (plume tilt)
	! --- V5.0-V5.4     000602  (DGS): add 'frozen puff' treatment for calms
	! --- V4.0-V5.0     971107  (DGS): pass ICODE to VCOUP from /CURRENT/
	!
	!
	! --- INPUTS:
	!                IP - integer - Puff index
	!            MFACT0 - integer - Puff/slug flag (0: puff)
	!              HLID - real    - Mixing depth  (m AGL)
	!            PPCOEF - real    - Plume Path Coefficient for partial
	!                               height correction (MCTADJ = 3)
	!             LCALM - logical - Denotes calm period when true
	!
	!     Common Block /FLAGS/ variables:
	!        MCTADJ, MTILT
	!     Common Block /GRID/ variables:
	!        DGRIDI
	!     Common Block /PUFF/ variables:
	!        HT0(mxpuff), ELBASE(mxpuff)
	!     Common Block /CURRENT/ variables:
	!        XB1, YB1, ZB1, SZB1,
	!                       SZM1,
	!        XE1, YE1, ZE1, SZE1,
	!        XB2, YB2, ZB2, SZB2,
	!        XE2, YE2, ZE2, SZE2, ICODE,
	!        TB1, TB2, TE1, TE2, VSETL
	!
	!
	! --- OUTPUTS:
	!
	!             VCBAR - real    - Vertical coupling coefficient (1/m)
	!
	! --- VCBAR called by:  DRY
	! --- VCBAR calls:      VCOUP, GETELEV, CTADJ
	!----------------------------------------------------------------------
	!
	! --- SUPPLEMENTAL NOTES
	! --- zgr = receptor height in terrain following coordinates
	!     is assumed to be zero.
	! --------------------------------------------------------------------
	!
	include 'params.puf'
	include 'flags.puf'
	include 'grid.puf'
	include 'puff.puf'
	include 'current.puf'
	logical lcalm, ldbg
	data half/0.50/,third/0.3333333/,fourth/0.250/,zgr/0.0/
	! --- Set list file output switch for testing
	ldbg=.FALSE.
	! --- Require lcalm = false
	if(LCALM) then
		write(io6,*)'VCBAR: LCALM must be FALSE, LCALM = ',lcalm
		stop 'Halted in VCBAR -- See list file.'
	endif
	vcbar=0.0
	! --- Set maximum lid ht = HLID for VCOUP call
	hlidmax=hlid
	if(mfact0.EQ.0) then
		! ---    PUFF !
		if(mtilt.EQ.1) then
			! ---       Set heights with gravitational settling
			! ---       Use full settling over puff age here (ht may be negative)
			zpb1=zfinal(ip)-tb1*vsetl
			zpe1=zfinal(ip)-te1*vsetl
		else
			zpb1=zb1
			zpe1=ze1
		endif
		! ---    Middle of step
		zpm1=half*(zpb1+zpe1)
		zp=zpm1
		if(mctadj.EQ.1 .OR. mctadj.EQ.3) then
			xm1mg=half*(xb1+xe1)*dgridi
			ym1mg=half*(yb1+ye1)*dgridi
			call getelev(xm1mg,ym1mg,zrterr)
			call ctadj(zrterr,zpm1,ht0(ip),elbase(ip),ppcoef,zp)
		endif
		! ---    Condition effective puff ht to be non-negative
		zp=AMAX1(zp,0.0)
		vcbar=vcbar+third*vcoup(icode,zgr,zp,szm1,hlid,hlidmax)
		! ---    Special treatment for calm periods:
		! ---    Use puff properties at the middle of the sampling period
		if(LCALM) then
			vcbar=3.*vcbar
		else
			! ---       Beginning of step
			zp=zpb1
			if(mctadj.EQ.1 .OR. mctadj.EQ.3) then
				xb1mg=xb1*dgridi
				yb1mg=yb1*dgridi
				call getelev(xb1mg,yb1mg,zrterr)
				call ctadj(zrterr,zpb1,ht0(ip),elbase(ip),ppcoef,zp)
			endif
			! ---       Condition effective puff ht to be non-negative
			zp=AMAX1(zp,0.0)
			vcbar=vcbar+third*vcoup(icode,zgr,zp,szb1,hlid,hlidmax)
			! ---       End of step
			zp=zpe1
			if(mctadj.EQ.1 .OR. mctadj.EQ.3) then
				xe1mg=xe1*dgridi
				ye1mg=ye1*dgridi
				call getelev(xe1mg,ye1mg,zrterr)
				call ctadj(zrterr,zpe1,ht0(ip),elbase(ip),ppcoef,zp)
			endif
			! ---       Condition effective puff ht to be non-negative
			zp=AMAX1(zp,0.0)
			vcbar=vcbar+third*vcoup(icode,zgr,zp,sze1,hlid,hlidmax)
		endif
		if(LDBG) then
			write(io6,*)'VCBAR --  plume tilt'
			write(io6,*)'zpb1,zpm1,zpe1   = ',zpb1,zpm1,zpe1
			write(io6,*)'tb1,te1          = ',tb1,te1
		endif
	else
		! ---    SLUG !
		if(mtilt.EQ.1) then
			! ---       Set heights with gravitational settling
			! ---       Use full settling over puff age here (ht may be negative)
			zpb1=zfinal(ip)-tb1*vsetl
			zpe1=zfinal(ip)-te1*vsetl
			zpb2=zfinal(ip)-tb2*vsetl
			zpe2=zfinal(ip)-te2*vsetl
		else
			zpb1=zb1
			zpe1=ze1
			zpb2=zb2
			zpe2=ze2
		endif
		zp=zpb1
		if(mctadj.EQ.1 .OR. mctadj.EQ.3) then
			xb1mg=xb1*dgridi
			yb1mg=yb1*dgridi
			call getelev(xb1mg,yb1mg,zrterr)
			call ctadj(zrterr,zpb1,ht0(ip),elbase(ip),ppcoef,zp)
		endif
		! ---    Condition effective puff ht to be non-negative
		zp=AMAX1(zp,0.0)
		vcbar=vcbar+fourth*vcoup(icode,zgr,zp,szb1,hlid,hlidmax)
		zp=zpb2
		if(mctadj.EQ.1 .OR. mctadj.EQ.3) then
			xb2mg=xb2*dgridi
			yb2mg=yb2*dgridi
			call getelev(xb2mg,yb2mg,zrterr)
			call ctadj(zrterr,zpb2,ht0(ip),elbase(ip),ppcoef,zp)
		endif
		! ---    Condition effective puff ht to be non-negative
		zp=AMAX1(zp,0.0)
		vcbar=vcbar+fourth*vcoup(icode,zgr,zp,szb2,hlid,hlidmax)
		zp=zpe1
		if(mctadj.EQ.1 .OR. mctadj.EQ.3) then
			xe1mg=xe1*dgridi
			ye1mg=ye1*dgridi
			call getelev(xe1mg,ye1mg,zrterr)
			call ctadj(zrterr,zpe1,ht0(ip),elbase(ip),ppcoef,zp)
		endif
		! ---    Condition effective puff ht to be non-negative
		zp=AMAX1(zp,0.0)
		vcbar=vcbar+fourth*vcoup(icode,zgr,zp,sze1,hlid,hlidmax)
		zp=zpe2
		if(mctadj.EQ.1 .OR. mctadj.EQ.3) then
			xe2mg=xe2*dgridi
			ye2mg=ye2*dgridi
			call getelev(xe2mg,ye2mg,zrterr)
			call ctadj(zrterr,zpe2,ht0(ip),elbase(ip),ppcoef,zp)
		endif
		! ---    Condition effective puff ht to be non-negative
		zp=AMAX1(zp,0.0)
		vcbar=vcbar+fourth*vcoup(icode,zgr,zp,sze2,hlid,hlidmax)
		if(LDBG) then
			write(io6,*)'VCBAR --  plume tilt'
			write(io6,*)'zpb1,zpm1,zpe1   = ',zpb1,zpm1,zpe1
			write(io6,*)'tb1,te1          = ',tb1,te1
			write(io6,*)'zpb2,zpe2        = ',zpb2,zpe2
			write(io6,*)'tb2,te2          = ',tb2,te2
		endif
	endif
	return
end
!----------------------------------------------------------------------
SUBROUTINE SLGTLIM(X,Y,SYR,ITF,TSTART,TEND)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 900228           SLGTLIM
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  For a given slug and receptor combination for which a
	!               numerical dose integration is about to be done,
	!               determine the dimensionless time integration limits,
	!               0.0 < TSTART,TEND < 1.0  , which will capture the
	!               major part of the integrand's contribution.
	!
	!
	! --- INPUTS:
	!
	!                 X - real    - X-coordinate of receptor (m)
	!                 Y - real    - Y-coordinate of receptor (m)
	!               SYR - real    - Time-independent Y-sigma at receptor (m)
	!               ITF - integer - Return code
	!                                   =  0 implies that conc = 0 and
	!                                        do not integrate.
	!                                   = +1 implies that the integration
	!                                        should be done
	!
	!     Common Block /CURRENT/ variables:
	!        XB1, YB1, XE2, YE2, XB2, YB2, XE2, YE2
	!     Parameters:
	!        IO6
	!
	! --- OUTPUTS:
	!
	!            TSTART - real    - Start of integrand sampling
	!                               at time:  t + tstart * dt
	!              TEND - real    - End of integrand sampling
	!                               at time:  t + tend * dt
	!
	! --- SLGTLIM called by:  SLUGAVE
	! --- SLGTLIM calls:      FINLINI, ROOT3, ROOT2
	!----------------------------------------------------------------------
	!
	! --- SUPPLEMENTAL NOTES
	!
	! --- COMPUTES THE INTEGRATION START AND STOP TIMES
	!     OVER THE TIME T TO T+DT (WHERE DT IS A TIME STEP)
	!
	! --- ITF GIVES THE STATUS OF THE TIME LIMITS;
	!          =  0 IMPLIES THAT CONC = 0 AND DO NOT INTEGRATE.
	!          = +1 IMPLIES THAT THE INTEGRATION SHOULD BE DONE
	!               WITH LIMITS    T + TSTART * DT
	!               AND            T + TEND * DT.
	!
	! --- (X,Y,Z) ARE RECEPTOR COORDINATES.
	!
	! --- (XB1,YB1,ZB1) ARE THE COORDS OF THE OLDEST END OF THE SLUG AT
	!                   THE BEGINNING (B) OF THE TIME STEP (I.E., TIME = T).
	! --- (XE1,YE1,ZE1) ARE THE COORDS OF THE OLDEST END OF THE SLUG AT
	!                   THE END (E) OF THE TIME STEP (I.E., TIME = T+DT).
	! --- (XB2,YB2,ZB2) ARE THE COORDS OF THE YOUNGEST END OF THE SLUG AT
	!                   THE BEGINNING (B) OF THE TIME STEP (I.E., TIME = T).
	! --- (XE2,YE2,ZE2) ARE THE COORDS OF THE YOUNGEST END OF THE SLUG AT
	!                   THE END (E) OF THE TIME STEP (I.E., TIME = T+DT).
	!
	!     SYR IS THE TIME-INDEPENDENT Y SIGMA AT THE RECEPTOR.
	!
	!     ALL SPATIAL DIFFERENCES ARE COMPUTED TO 1 METER RESOLUTION
	!
	!
	! --- SOME OF THE INPUT VARIABLES NEEDED ARE NOW IN /CURRENT/
	!     common/current/xb1,yb1,zb1,syb1,szb1,
	!    1              xe1,ye1,ze1,sye1,sze1,
	!    2              xb2,yb2,zb2,syb2,szb2,
	!    3              xe2,ye2,ze2,sye2,sze2,
	!    4              iage,speedi,srat,temis
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	include 'current.puf'
	!
	!
	DIMENSION RT(3)
	DATA TWO/2.0/,SIGCUT/3.0/,SMALL/1.0E-10/
	!
	! --- SET THE INFLUENCE RADIUS, R, AROUND THE RECEPTOR
	R = SIGCUT * SYR
	R2 = R * R
	!
	! --- INITIALIZE TMIN AND TMAX AT UNPHYSICAL VALUES.
	TMIN =  1.0E10
	TSTART =  -9.0
	TMAX = -1.0E10
	TEND =  -9.0
	!
	!
	! $$$ DO SPECIAL CASES OF TIME LIMITS AT THEIR FROZEN EXTREMA FIRST.
	!
	! ### END POINT PENETRATIONS AT T=0 AND T=1
	!     AX = XB2 - X
	!     AY = YB2 - Y
	! --- Condition differences to nearest 1.0m
	AX = FLOAT( NINT(XB2 - X) )
	AY = FLOAT( NINT(YB2 - Y) )
	! --- Condition differences to nearest 0.1m
	!     AX = 0.1*FLOAT( NINT(10.*(XB2 - X)) )
	!     AY = 0.1*FLOAT( NINT(10.*(YB2 - Y)) )
	!
	R2TEST = AX * AX  +  AY * AY
	IF(R2TEST.LT.R2) TMIN = -0.1
	R2TEST = (XB1 - X)**2  +  (YB1 - Y)**2
	IF(R2TEST.LT.R2) TMIN = -0.1
	R2TEST = (XE1 - X)**2  +  (YE1 - Y)**2
	IF(R2TEST.LT.R2) TMAX = 1.1
	R2TEST = (XE2 - X)**2  +  (YE2 - Y)**2
	IF(R2TEST.LT.R2) TMAX = 1.1
	!
	!     DO CONDITIONAL TEST ON TMIN,TMAX. IF THIS TEST PASSES, THEN THERE
	!     IS END POINT PENETRATION AT BOTH THE BEGINNING AND END OF THE
	!     TIME STEP (AND THUS FOR ALL INTERMEDIATE TIMES).
	if(.not. (TMIN.LT.0.0 .AND. TMAX.GT.1.0)) then ! add by @creaqi goto 100 in fun modify_goto_pure
		!
		! --- DEFINE NEEDED COORDINATE DIFFERENCES
		!     BX = XB1 - XB2
		!     BY = YB1 - YB2
		!     CX = XE2 - XB2
		!     CY = YE2 - YB2
		!     EX = (XE1 - XE2) - (XB1 - XB2)
		!     EY = (YE1 - YE2) - (YB1 - YB2)
		! --- Condition differences to nearest 1.0m
		BX = FLOAT( NINT(XB1 - XB2) )
		BY = FLOAT( NINT(YB1 - YB2) )
		CX = FLOAT( NINT(XE2 - XB2) )
		CY = FLOAT( NINT(YE2 - YB2) )
		EX = FLOAT( NINT(XE1 - XE2 - XB1 + XB2) )
		EY = FLOAT( NINT(YE1 - YE2 - YB1 + YB2) )
		! --- Condition differences to nearest 0.1m
		!     BX = 0.1*FLOAT( NINT(10.*(XB1 - XB2)) )
		!     BY = 0.1*FLOAT( NINT(10.*(YB1 - YB2)) )
		!     CX = 0.1*FLOAT( NINT(10.*(XE2 - XB2)) )
		!     CY = 0.1*FLOAT( NINT(10.*(YE2 - YB2)) )
		!     EX = 0.1*FLOAT( NINT(10.*(XE1 - XE2 - XB1 + XB2)) )
		!     EY = 0.1*FLOAT( NINT(10.*(YE1 - YE2 - YB1 + YB2)) )
		!
		! ### CHECK FOR TANGENTIAL (I.E., 0 < S < 1 ) PENETRATION AT T=0,1.
		!
		! --- CONSIDER FIRST THE T=0 SITUATION.
		A2 = BX * BX  +  BY * BY
		A1 = AX * BX  +  AY * BY
		S = -9.
		IF(ABS(A2).GT.SMALL) S = -A1 / A2
		if(.not. (S .LT. 0.0)) then ! add by @creaqi goto 10 in fun modify_goto_pure
			if (.not. (S .GT. 1.0)) then ! add by @creaqi 10 state_same == True
				AXS = AX + BX * S
				AYS = AY + BY * S
				R2TEST = AXS * AXS  + AYS * AYS
				!     WRITE(*,1002) S,R2TEST
				!     WRITE(io6,1002) S,R2TEST
				!1002 FORMAT(' SLGTLIM:T0/TAN S,R2TEST=',2G12.4)
				IF(R2TEST.LT.R2) TMIN = -0.1
				!
				!
				! --- CONSIDER NEXT THE T=1 SITUATION.
			endif ! add by @creaqi 10 state_same == True
		endif !add by @creaqi label 10 modify_goto_pure
		10 AXT = AX + CX
		AYT = AY + CY
		BXT = BX + EX
		BYT = BY + EY
		A2 = BXT * BXT  +  BYT * BYT
		A1 = AXT * BXT  +  AYT * BYT
		S = -9.
		IF(ABS(A2).GT.SMALL) S = -A1 / A2
		if(.not. (S .LT. 0.0)) then ! add by @creaqi goto 20 in fun modify_goto_pure
			if (.not. (S .GT. 1.0)) then ! add by @creaqi 20 state_same == True
				AXS = AXT + BXT * S
				AYS = AYT + BYT * S
				R2TEST = AXS * AXS  + AYS * AYS
				!     WRITE(*,1004) S,R2TEST
				!     WRITE(io6,1004) S,R2TEST
				!1004 FORMAT(' SLGTLIM:T1/TAN S,R2TEST=',2G12.4)
				IF(R2TEST.LT.R2) TMAX = 1.1
				!
				!     DO CONDITIONAL TEST ON TMIN,TMAX. IF THIS TEST PASSES,
				!     THEN THERE IS PENETRATION OF THE CIRCLE BY THE FINITE SLUG
				!     AT THE BEGINNING AND/OR END OF THE TIME STEP.
			endif ! add by @creaqi 20 state_same == True
		endif !add by @creaqi label 20 modify_goto_pure
		if (.not. (TMIN.LT.0.0 .AND. TMAX.GT.1.0)) then ! add by @creaqi 100 state_same == True
			!
			!----------------------------------------------------------------
			! $$$ IF WE'VE REACHED THIS POINT IN THE CODE THEN ANY INTRUSION
			!     OF THE FINITE SLUG INTO THE CIRCLE OF RADIUS R ABOUT THE
			!     RECEPTOR MUST BE A DYNAMIC FUNCTION OF TIME.
			!
			! ### END POINT PENETRATIONS AT  0 < T < 1.
			!
			! --- CONSIDER FIRST THE #2 (OR S=0) END OF THE SLUG.
			!
			A2 = CX * CX  +  CY * CY
			A1 = TWO * (AX * CX  +  AY * CY)
			A0 = AX * AX  +  AY * AY  -  R2
			CALL ROOT2(A2,A1,A0,NR,RT)
			!     WRITE(*,1001) NR,(RT(II),II=1,NR)
			!     WRITE(io6,1001) NR,(RT(II),II=1,NR)
			!1001 FORMAT(' SLGTLIM:S0/END NR,T(I) =',I3,3G12.4)
			IF(NR.GT.0) THEN
				DO I=1,NR! add by @creaqi do label 30
					T = RT(I)
					IF(T .LT. TMIN) TMIN = T
					IF(T .GT. TMAX) TMAX = T
				enddo !add by @creaqi 30
				30             CONTINUE
			ENDIF
			!
			!
			! --- CONSIDER NEXT THE #1 (OR S=1) END OF THE SLUG.
			!
			AXS = AX + BX
			AYS = AY + BY
			CXS = CX + EX
			CYS = CY + EY
			A2 = CXS * CXS  +  CYS * CYS
			A1 = TWO * (AXS * CXS  +  AYS * CYS)
			A0 = AXS * AXS  +  AYS * AYS  -  R2
			CALL ROOT2(A2,A1,A0,NR,RT)
			!     WRITE(*,1003) NR,(RT(II),II=1,NR)
			!     WRITE(io6,1003) NR,(RT(II),II=1,NR)
			!1003 FORMAT(' SLGTLIM:S1/END NR,T(I) =',I3,3G12.4)
			IF(NR.GT.0) THEN
				DO I=1,NR! add by @creaqi do label 40
					T = RT(I)
					IF(T .LT. TMIN) TMIN = T
					IF(T .GT. TMAX) TMAX = T
				enddo !add by @creaqi 40
				40             CONTINUE
			ENDIF
			!
			! --- WE COULD DO CONDITIONAL TEST ON TMIN,TMAX. IF THIS TEST PASSES,
			! I.E., IF(TMAX.GT.TMIN .AND. TMIN.LT.1.0 .AND. TMAX.GT.0.0) GO TO 100,
			!     THEN THERE IS END POINT PENETRATION AT SOMETIME WITHIN THE
			!     TIME STEP; HOWEVER, THERE IS NO GUARANTEE THAT THE END POINT
			!     MADE THE FIRST PENETRATION.  THAT IS, THE FINITE PORTION OF THE
			!     SLUG MAY HAVE MADE AN EARLIER PENETRATION OF THE CIRCLE.  THIS
			!     PROBLEM THE CALLS FOR THE SOLUTION OF THE QUARTIC EQUATION
			!     INVOLVING BOTH VARIABLE T AND S.
			! ### TANGENTIAL PENETRATIONS AT  0 < T < 1.
			!
			! --- FORM NEEDED PRODUCTS.
			AB = AX * BY - AY * BX
			AD = AX * EY - AY * EX
			BD = BX * EX + BY * EY
			BB = BX * BX + BY * BY
			CB = CX * BY - CY * BX
			CD = CX * EY - CY * EX
			ADCB = AD + CB
			DD = EX * EX + EY * EY
			! --- CONSIDER PENETRATION OF THE SQUARE WITH DIMENSION R INSTEAD,
			!     AS THIS REDUCES THE PROBLEM TO A CUBIC.
			!
			! --- DETERMINE CUBIC COEFFS. FOR X INTERCEPTION AT +R.
			A3 = EY * CD
			PROD2 = EY * ADCB  +  BY * CD
			A2 = PROD2  +  R * DD
			PROD1 = BY * ADCB  +  EY * AB
			A1 = PROD1  +  TWO * R * BD
			A0 = BY * AB  +  R * BB
			CALL ROOT3(A3,A2,A1,A0,NR,RT)
			!     CALL DROOT3(A3,A2,A1,A0,NR,RT)
			! --- CHECK REAL ROOTS FOR FINITE LINE INTERCEPT.
			IF(NR.GT.0) THEN
				DO I=1,NR! add by @creaqi do label 50
					T = RT(I)
					CALL FINLINI(AX,BX,CX,EX,AY,BY,CY,EY,T,R2,TMIN,TMAX)
				enddo !add by @creaqi 50
				50             CONTINUE
			ENDIF
			!
			!
			! --- DETERMINE CUBIC COEFFS. FOR X INTERCEPTION AT -R.
			A3 = EY * CD
			A2 = PROD2  -  R * DD
			A1 = PROD1  -  TWO * R * BD
			A0 = BY * AB  -  R * BB
			CALL ROOT3(A3,A2,A1,A0,NR,RT)
			!     CALL DROOT3(A3,A2,A1,A0,NR,RT)
			! --- CHECK REAL ROOTS FOR FINITE LINE INTERCEPT.
			IF(NR.GT.0) THEN
				DO I=1,NR! add by @creaqi do label 60
					T = RT(I)
					CALL FINLINI(AX,BX,CX,EX,AY,BY,CY,EY,T,R2,TMIN,TMAX)
				enddo !add by @creaqi 60
				60             CONTINUE
			ENDIF
			!
			!
			! --- DETERMINE CUBIC COEFFS. FOR Y INTERCEPTION AT +R.
			A3 = EX * CD
			PROD2 = EX * ADCB  +  BX * CD
			A2 = PROD2  +  R * DD
			PROD1 = BX * ADCB  +  EX * AB
			A1 = PROD1  +  TWO * R * BD
			A0 = BX * AB  +  R * BB
			CALL ROOT3(A3,A2,A1,A0,NR,RT)
			!     CALL DROOT3(A3,A2,A1,A0,NR,RT)
			! --- CHECK REAL ROOTS FOR FINITE LINE INTERCEPT.
			IF(NR.GT.0) THEN
				DO I=1,NR! add by @creaqi do label 70
					T = RT(I)
					CALL FINLINI(AX,BX,CX,EX,AY,BY,CY,EY,T,R2,TMIN,TMAX)
				enddo !add by @creaqi 70
				70             CONTINUE
			ENDIF
			!
			!
			! --- DETERMINE CUBIC COEFFS. FOR Y INTERCEPTION AT -R.
			A3 = EX * CD
			A2 = PROD2  -  R * DD
			A1 = PROD1  -  TWO * R * BD
			A0 = BX * AB  -  R * BB
			CALL ROOT3(A3,A2,A1,A0,NR,RT)
			!     CALL DROOT3(A3,A2,A1,A0,NR,RT)
			! --- CHECK REAL ROOTS FOR FINITE LINE INTERCEPT.
			IF(NR.GT.0) THEN
				DO I=1,NR! add by @creaqi do label 80
					T = RT(I)
					CALL FINLINI(AX,BX,CX,EX,AY,BY,CY,EY,T,R2,TMIN,TMAX)
				enddo !add by @creaqi 80
				80             CONTINUE
			ENDIF
			!
			!     DO CONDITIONAL TEST ON TMIN,TMAX. IF THIS TEST PASSES,
			!     THEN THERE IS PENETRATION AT SOMETIME WITHIN THE
			!     TIME STEP.
			if (.not. (TMAX.GT.TMIN .AND. TMIN.LT.1.0 .AND. TMAX.GT.0.0)) then ! add by @creaqi 100 state_same == True
				!
				!-------------------------------------------------------------------
				! $$$ IF WE'VE REACHED THIS POINT AND NOT SATISFIED ANY OF THE ABOVE
				!     CONDITIONS THEN FORGET ABOUT A NUMERICAL INTEGRATION AS THERE
				!     IS NO SLUG PENETRATION OF THE REGION IN THE PERIOD OF INTEREST.
				ITF = 0
				RETURN
				!*****
				! *** write(io6,debug5)
				! *** write(io6,debug6)
				!*****
				!
			endif ! add by @creaqi 100 state_same == True
		endif ! add by @creaqi 100 state_same == True
	endif !add by @creaqi label 100 modify_goto_pure
	100 ITF = 1
	TSTART = AMAX1(0.0,TMIN)
	TEND   = AMIN1(1.0,TMAX)
	RETURN
END
!----------------------------------------------------------------------
SUBROUTINE FINLINI(AX,BX,CX,EX,AY,BY,CY,EY,T,R2,TMIN,TMAX)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 900228           FINLINI
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  Check real roots for FInite LINe Intercept.
	!               For a specific time T, this routine computes the point
	!               on the line segment where the intersection with the
	!               receptor perimeter(circle or square) occurs.
	!               If that intercept yields an S such that 0 < S < 1, then
	!               the intersection falls on the finite portion of the line
	!               and is of interest. If S is outside this domain, we
	!               RETURN without altering TMIN,TMAX integration limits.
	!               12/18/87.
	!
	!
	! --- INPUTS:
	!
	!                AX - real    - X-distance (m) from slug end 2 to
	!                               receptor at time T=0. (m)
	!                               AX = XB2 - X
	!                BX - real    - X-distance (m) from slug end 2 to slug
	!                               end 1 at time T=0. (m)
	!                               BX = XB1 - XB2
	!                CX - real    - X-distance (m) from slug end 2 at T=1 to
	!                               slug end 2 at time T=0. (m)
	!                               CX = XE2 - XB2
	!                EX - real    - X-slug shear (m) defined as
	!                               EX = XE1 - XE2 - (XB1 - XB2)
	!                AY - real    - Y-distance (m) from slug end 2 to
	!                               receptor at time T=0. (m)
	!                               AY = YB2 - Y
	!                BY - real    - Y-distance (m) from slug end 2 to slug
	!                               end 1 at time T=0. (m)
	!                               BY = YB1 - YB2
	!                CY - real    - Y-distance (m) from slug end 2 at T=1 to
	!                               slug end 2 at time T=0. (m)
	!                               CY = YE2 - YB2
	!                EY - real    - Y-slug shear (m), defined as
	!                               EY = YE1 - YE2 - (YB1 - YB2)
	!                 T - real    - Dimensionless current time: 0 < T < 1
	!                R2 - real    - Radius squared of intercept circle
	!                               about receptor (m**2)
	!
	!     Parameters:
	!        IO6
	!
	! --- OUTPUTS:
	!
	!              TMIN - real    - Updated start of integrand sampling
	!                               at time:  t + tstart * dt
	!              TMAX - real    - Updated end of integrand sampling
	!                               at time:  t + tend * dt
	!
	! --- FINLINI called by:  SLGTLIM
	! --- FINLINI calls:      none
	!----------------------------------------------------------------------
	!
	DATA FR2/2.5/
	!
	ALFX = AX  +  CX * T
	ALFY = AY  +  CY * T
	BETX = BX  +  EX * T
	BETY = BY  +  EY * T
	BET2 = BETX * BETX  +  BETY * BETY
	IF(BET2 .LT. 1.0E-30) BET2 = 1.0E-30
	SI = -(ALFX * BETX  +  ALFY * BETY) / BET2
	IF(SI .GT. 1.0) return ! add by @creaqi goto return clb is_return
	IF(SI .LT. 0.0) return ! add by @creaqi goto return clb is_return
	! --- At this point 0 < SI < 1 and we have actual tangential
	!     penetration of zone via the finite line segment.
	! --- Check total length of perpendicular line from receptor.
	! --- XL2 = QI**2  *  BET2
	XL2 = (ALFX * BETY  -  ALFY * BETX)**2 / BET2
	IF(XL2 .GT. (FR2*R2)) return ! add by @creaqi goto return clb is_return
	! --- N.B.: The factor FR2 accounts for distance to a corner. Diagonal
	! --- of (1.414*R)**2=2.0*R**2.  We use 2.5>2.0 to avoid roundoff.
	! +++ Store adjusted TMIN,TMAX
	IF(T .LT. TMIN) TMIN = T
	IF(T .GT. TMAX) TMAX = T
	!     WRITE(*,1001) SI,T
	!     WRITE(io6,1001) SI,T
	!1001 FORMAT(' FINLINI: SI,T =',2G12.4)
	50 RETURN
END
!----------------------------------------------------------------------
subroutine slugint(ldb,iint,lwflux,x,y,z,&
	zpr,tfacc,syrb,szrb,syre,szre,hlid,hlidmax,&
	ccqb,ccdq,ccizqb,ccizdq)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 100108           SLUGINT
	!                R. Yamartino
	!
	! --- PURPOSE:  Computes the time-average coupling coefficients (s/m**3)
	!               at the point (x,y,z) over the time period T to T+DT
	!                (where DT is a time step and TDDT
	!                is the fraction of DT that is elapsed).
	!
	! --- UPDATE
	! --- V6.267-V6.268 100108  (DGS): fix logical name in call to ASDF
	! --- V5.4-V6.267   090710  (DGS): add HLIDMAX for use in VCOUP and
	!                                  allow receptor to sample mass above
	!                                  lid
	! --- V5.0-V5.4     000602  (DGS): recalculate exponentials after the
	!                                  SIGCUT test for receptors that this
	!                                  slug impacts
	! --- V5.0-V5.0     980918  (DGS): add area source distribution factor
	! --- V5.0-V5.0     980430  (DGS): 'Use' TFACC (compiler warning)
	! --- V4.0-V5.0     971107  (DGS): add debug option
	!                   971107  (DGS): add check for slug passing receptor
	!                                  along slug-axis
	!                   971107  (DGS): pass ICODE to VCOUP from /CURRENT/
	! --- V4.0-V4.07    971107  (DGS): add PDF logic for VCOUP calls
	! --- V4.0-V5.0     971107  (DGS): package slug geometry in SLGFRAC
	!
	! --- INPUTS:
	!
	!               LDB - logical - Report debug info when TRUE
	!              IINT - logical - Gives the status of the integration;
	!                               -1 : numerical integration must be done.
	!                                0 : conc=0 and no further work be done.
	!                               +1 : conc is correct as is.
	!            LWFLUX - logical - Receptor specific wet deposition flag
	!                                .true. if calculation is to be made
	!                                .false. if not.
	!         (X, Y, Z) - real    - Coordinates of the receptor (m)
	!               ZPR - real    - Height (m) of the plume above receptor
	!                               terrain height.
	!             TFACC - real    - Cross-slug T factor to account for shear
	!       (SYRB,SZRB) - real    - Sigmas at the receptor before T factor.
	!                               At beginning of time step.
	!       (SYRE,SZRE) - real    - Sigmas at the receptor before T factor.
	!                               At end of time step.
	!              HLID - real    - Relevant mixing depth (m) at receptor.
	!           HLIDMAX - real    - Maximum lid height (m) for mass
	!                               above HLID
	!
	!     Common Block /CURRENT/ variables:
	!        XB1, YB1, ZB1, SYB1, SZB1,
	!        XE1, YE1, ZE1, SYE1, SZE1,
	!        XB2, YB2, ZB2, SYB2, SZB2,
	!        XE2, YE2, ZE2, SYE2, SZE2,
	!        SPEEDI, SRAT, ICODE
	!     Common Block /PDF/ variables:
	!        LPDF, SWUPF, SWDNF, SZUPB, SZDNB, WTUP, WTDN, ZUP, ZDN, RFACSQ
	!     Parameters:
	!        none
	!
	! --- OUTPUTS:
	!
	!              CCQB - real    - Coupling coefficient (s/m**3) for
	!                               the source rate ,QB, at the
	!                               beginning of the time step.
	!              CCDQ - real    - Coupling coefficient (s/m**3) for
	!                               the change in source rate, DQ, between
	!                               the beginning and end of the time step.
	!            CCIZQB - real    - Z-integrated coefficient (s/m**2) for
	!                               the source rate ,QB, at the
	!                               beginning of the time step.
	!            CCIZDQ - real    - Z-integrated coefficient (s/m**2) for
	!                               the change in source rate, DQ, between
	!                               the beginning and end of the time step.
	!
	! --- SLUGINT called by:  SLUGAVE
	! --- SLUGINT calls:      VCOUP, ERF, SLGFRAC, ASDF
	!----------------------------------------------------------------------
	!
	! --- Include common blocks
	include 'params.puf'
	include 'current.puf'
	include 'pdf.puf'
	!
	logical lwflux,ldb
	!
	DATA SRTHAF/0.7071/,HALF/0.5/,SMALL/1.0E-10/,VSMALL/1.0E-20/
	DATA SRT2PI/2.506628/,SRTPI/1.772454/
	DATA ERFCUT/3.0/,SIGCUT/3.0/,TOL/0.01/
	! --- NOTE THAT TOL ALLOWS THE ALONGWIND CAUSALITY FACTOR
	!     TO VARY AS MUCH AS THE FRACTION 2 * TOL.
	!
	! --- Set limit for argument of EXP (expmx=69.)
	data rtexpmx/8.3/
	!
	! --- DETERMINE THE CURRENT COORDINATES OF THE SLUG END-POINTS.
	!     X1 = XB1 + TDDT * (XE1 - XB1)
	!     Y1 = YB1 + TDDT * (YE1 - YB1)
	!     Z1 = ZB1 + TDDT * (ZE1 - ZB1)
	!
	!     X2 = XB2 + TDDT * (XE2 - XB2)
	!     Y2 = YB2 + TDDT * (YE2 - YB2)
	!     Z2 = ZB2 + TDDT * (ZE2 - ZB2)
	! --- DETERMINE THE LENGTH AND ORIENTATION OF THE SLUG
	! --- DEFINE THE POSITION OF THE RECEPTOR RELATIVE TO SLUG END 2
	! --- CONVERT RECEPTOR POSITION TO ALONG-SLUG AND CROSS-SLUG AXIS
	!     AT TIMES T=0 (ENDS WITH B) AND T=1 (ENDS WITH E).
	call SLGFRAC(x,y,xb1,yb1,xb2,yb2,&
	rhoab,rhocb,fracsb,dxy12b,rdxy12b)
	call SLGFRAC(x,y,xe1,ye1,xe2,ye2,&
	rhoae,rhoce,fracse,dxy12e,rdxy12e)
	! --- CONSTRAIN FRACS TO BE 0.0 TO 1.0 FOR THE ACTUAL SLUG.
	FRACSB = AMAX1(0.0,FRACSB)
	FRACSB = AMIN1(1.0,FRACSB)
	FRACSE = AMAX1(0.0,FRACSE)
	FRACSE = AMIN1(1.0,FRACSE)
	! --- NOTE THAT IN X-Y SPACE THE COORDINATES OF THE POINT (XP,YP,ZP)
	!     CORRESPONDING TO RHOC = 0 (I.E., RHOA ALONG THE SLUG AXIS) ARE
	!     XP = X2  +  RHOA * SINOM
	!     YP = Y2  +  RHOA * COSOM
	! OR  XP = X2  +  FRACS * (X1 - X2)  IF WITHIN SLUG BOUNDS.
	! OR  YP = Y2  +  FRACS * (Y1 - Y2)  IF WITHIN SLUG BOUNDS.
	!
	RHOCPRO = RHOCB * RHOCE
	FRACT = 0.0
	IF(RHOCPRO.LT.0.0) FRACT=ABS(RHOCB/(ABS(RHOCB)+ABS(RHOCE)+SMALL))
	IF(RHOCPRO.GE.0.0 .AND. ABS(RHOCB).GT.ABS(RHOCE) ) FRACT = 1.0
	FRACT = AMAX1(0.0,FRACT)
	FRACT = AMIN1(1.0,FRACT)
	OMFRACT = 1.0 - FRACT
	IF(FRACT.EQ.1.0) OMFRACT = 0.0
	!
	sy =  fract * syre  +  omfract * syrb
	sz =  fract * szre  +  omfract * szrb
	dy =  0.0
	if(rhocpro.GT.0.) dy = fract*rhoce + omfract*rhocb
	!
	! --- Include the effect of the crosswind T factor on syint.
	!     Note that the following statement allows sy to experience growth
	!     but not shrinkage.  To allow shrinkage as well, one need only have
	!     sy = syint * tfacc
	!     sy = syint * amax1(1.0,tfacc)
	! --- 'Use' TFACC without applying it to sy
	tfacc=tfacc
	!*****
	if(LDB) then
		write(io6,*)'------------------ slugint ---------------'
		write(io6,*)'        rhoab,rhocb = ',rhoab,rhocb
		write(io6,*)'        rhoae,rhoce = ',rhoae,rhoce
		write(io6,*)'      fracsb,fracse = ',fracsb,fracse
		write(io6,*)'          syrb,syre = ',syrb,syre
		write(io6,*)'           fract,dy = ',fract,dy
		write(io6,*)'              sy,sz = ',sy,sz
	endif
	!*****
	!
	!
	! *** ALL COORDINATES READY ! NOW COMPUTE COUPLING COEFFICIENT FOR
	!     A UNIT SOURCE (Q = 1 UG/SEC)
	ccqb = 0.0
	ccdq = 0.0
	ccizqb = 0.0
	ccizdq = 0.0
	IINT = 0
	! --- Screen out cases where slug moves across receptor along its axis
	! --- This is a severe test for this case; more care could be taken!
	if(fracsb.EQ.1.0 .AND. fracse.EQ.0.0) then
		! ---    Numerical treatment needed
		iint=-1
		return ! add by @creaqi goto return clb is_return
	endif
	!
	! --- CHECK IF ANY VERTICAL COUPLING.
	! --- Note that we use receptor-specific slug height (m) now!
	!     vertb = vcoup(icode,z,zpb,sz,hlid)
	!     verte = vcoup(icode,z,zpe,sz,hlid)
	if(LPDF .AND. z.LE.hlid) then
		szsq=sz**2
		szdn=SQRT(szsq*swdnf+szdnb*rfacsq)
		szup=SQRT(szsq*swupf+szupb*rfacsq)
		vertb = wtdn*VCOUP(icode,z,zdn,szdn,hlid,hlidmax) +&
		wtup*VCOUP(icode,z,zup,szup,hlid,hlidmax)
	else
		vertb = vcoup(icode,z,zpr,sz,hlid,hlidmax)
	endif
	verte = vertb
	! --- Sum the contributions and test.  Average later.
	vert =  vertb + verte
	if(.not.lwflux  .and.  vert .lt. small) return ! add by @creaqi goto return clb is_return
	!
	! --- CHECK IF ANY CROSS-SLUG DISTANCE COUPLING.
	PHICB = RHOCB / SY
	PHICE = RHOCE / SY
	!
	SRAT2 = SRAT * SRAT
	EXPYB = 0.0
	IF(ABS(PHICB) .LE. SIGCUT) EXPYB = EXP(-HALF * SRAT2 *PHICB*PHICB)
	EXPYE = 0.0
	IF(ABS(PHICE) .LE. SIGCUT) EXPYE = EXP(-HALF * SRAT2 *PHICE*PHICE)
	! --- IF A CROSSOVER EXISTS THEN CONTRIBUTION TO INTEGRAL COULD COME
	!     BETWEEN T=0 AND T=1 AND NOT PEAK AT ENDPOINT; HENCE,
	if(.not. ((PHICB * PHICE) .LE. 0.0)) then ! add by @creaqi goto 20 in fun modify_goto_pure
		IF(EXPYB .LT. SMALL  .AND.  EXPYE .LT. SMALL) return ! add by @creaqi goto return clb is_return
		!
		! --- CHECK IF ANY ALONG-SLUG DISTANCE COUPLING.
	endif !add by @creaqi label 20 modify_goto_pure
	20 PHIA2B = SRTHAF * RHOAB / SYB2
	PHIA1B = SRTHAF * (RHOAB - DXY12B) / SYB1
	PRODB = PHIA2B * PHIA1B
	PHIA2E = SRTHAF * RHOAE / SYE2
	PHIA1E = SRTHAF * (RHOAE - DXY12E) / SYE1
	PRODE = PHIA2E * PHIA1E
	! --- IF A CROSSOVER EXISTS THEN CONTRIBUTION TO INTEGRAL COULD COME
	!     BETWEEN T=0 AND T=1 AND NOT PEAK AT ENDPOINT; HENCE,
	if(.not. ((PHIA1B * PHIA1E) .LE. 0.0)) then ! add by @creaqi goto 30 in fun modify_goto_pure
		if (.not. ((PHIA2B * PHIA2E) .LE. 0.0)) then ! add by @creaqi 30 state_same == True
			! --- IF SLUG ALWAYS WELL AWAY FROM RECEPTOR (AND NO CROSSOVER
			!     OCCURRED), THE CONTRIBUTION IS ZERO.
			IF(PRODB.GT.0.0           .AND.  PRODE.GT.0.0           .AND.&
				ABS(PHIA1B).GT.ERFCUT  .AND.  ABS(PHIA2B).GT.ERFCUT  .AND.&
				ABS(PHIA1E).GT.ERFCUT  .AND.  ABS(PHIA2E).GT.ERFCUT) return ! add by @creaqi goto return clb is_return
			!
			! --- ALL OBVIOUS CONC = 0 CASES HAVE NOW BEEN ELIMINATED SO CHECK
			!     IF VERTICAL COUPLING AND ALONG-SLUG CAUSALITY FACTORS ARE TIME-
			!     INDEPENDENT ENOUGH TO JUSTIFY ANALYTIC INTEGRATION (IINT = +1)
			!     OR A NUMERICAL INTEGRATION (IINT = -1) MUST BE DONE.
		endif ! add by @creaqi 30 state_same == True
	endif !add by @creaqi label 30 modify_goto_pure
	30   IINT = -1
	! --- CHECK VERTICAL COUPLING.
	IF((ABS(VERTB-VERTE)/(VERT+SMALL)).GT.TOL) return ! add by @creaqi goto return clb is_return
	vert = half * vert
	!
	! --- CHECK ALONG-SLUG VARIABILITY.
	ERFA1B = 1.0
	IF(PHIA1B.LT.0.0) ERFA1B = -1.0
	!     NOTE THAT FOR ERFCUT = 3, ERF(ERFCUT) = 0.999978
	IF(ABS(PHIA1B) .LE. ERFCUT) ERFA1B = ERF(PHIA1B)
	ERFA1E = 1.0
	IF(PHIA1E.LT.0.0) ERFA1E = -1.0
	IF(ABS(PHIA1E) .LE. ERFCUT) ERFA1E = ERF(PHIA1E)
	ERFSUM = ABS(ERFA1B) + ABS(ERFA1E) + SMALL
	IF((ABS(ERFA1B-ERFA1E)/ERFSUM).GT.TOL) return ! add by @creaqi goto return clb is_return
	!
	ERFA2B = 1.0
	IF(PHIA2B.LT.0.0) ERFA2B = -1.0
	IF(ABS(PHIA2B) .LE. ERFCUT) ERFA2B = ERF(PHIA2B)
	ERFA2E = 1.0
	IF(PHIA2E.LT.0.0) ERFA2E = -1.0
	IF(ABS(PHIA2E) .LE. ERFCUT) ERFA2E = ERF(PHIA2E)
	ERFSUM = ABS(ERFA2B) + ABS(ERFA2E) + SMALL
	IF((ABS(ERFA2B-ERFA2E)/ERFSUM).GT.TOL) return ! add by @creaqi goto return clb is_return
	!
	! --- COMPUTE THE CAUSALITY FACTOR FCAUS.
	FCAUSB = HALF * (ERFA2B - ERFA1B)
	if(erfa2b .eq. erfa1b) fcausb = 0.0
	!
	FCAUSE = HALF * (ERFA2E - ERFA1E)
	if(erfa2e .eq. erfa1e) fcause = 0.0
	!
	ERFSUM = ABS(FCAUSB) + ABS(FCAUSE) + SMALL
	IF((ABS(ERFA1B-ERFA1E)/ERFSUM).GT.TOL) return ! add by @creaqi goto return clb is_return
	!
	! --- Add symmetric test to end #2  10/12/89
	if((abs(erfa2b-erfa2e)/erfsum).gt.tol) return ! add by @creaqi goto return clb is_return
	!
	FCAUS = HALF * (FCAUSB + FCAUSE)
	IF(FCAUS .LT. SMALL) return ! add by @creaqi goto return clb is_return
	!
	!
	! --- AT THIS POINT THE CAUSALITY FACTOR IS LARGE AND UNIFORM ENOUGH TO
	!     REMOVE IT FROM THE INTEGRAL AND INTEGRATE JUST THE EXPONENTIAL
	!     TERMS.
	IINT = +1
	! ***************** INSERT THE INTEGRALS *****************************
	!
	! --- Insert the factor srat/sqrt(2) into PHI limits.
	PHICB = SRTHAF * SRAT * PHICB
	PHICE = SRTHAF * SRAT * PHICE
	phicd = phice - phicb
	!
	! --- Recompute the exponentials (expyb,expye) without SIGCUT limit
	if(ABS(phicb).LT.rtexpmx) expyb = EXP(-phicb*phicb)
	if(ABS(phice).LT.rtexpmx) expye = EXP(-phice*phice)
	!
	!
	expy0 = half * (expyb + expye)
	!     The extra half in expy1 comes from half*t**2 from integral
	expy1 = half * expy0
	!
	! --- TOL=0.01 at large phi limits could cause up to a 4%
	!     difference between expyb and expye and hence a 2% error in
	!     the average.  Use of tol/2 should cut this back to 1% as desired;
	!     however, erfdif accuracy drops off if phicd too small.  Hence,
	!     stick with TOL.  2*TOL also tried but TOL appears good compromise.
	!     if(abs(phicd).gt.(half*tol)) then
	!     if(abs(phicd).gt.(2.0*tol))  then
	if(abs(phicd) .gt. tol)      then
		expy0 = half * srtpi * erfdif(phice,phicb) / phicd
		phicd2 = phicd * phicd
		expy11 = half * (expyb - expye) / phicd2
		expy1 = expy11  -  phicb * expy0 / phicd
	endif
	!
	! --- Compute the time-average conc. terms for unit emission rate,
	! --- including correction factor for cross-slug distribution from
	! --- area sources
	ccoup = ASDF(ldb,dy,sy) * fcaus  /  (srt2pi * speedi * sy)
	! --- ccoup can legitimately get quite small (e.g., (e-4.5)/sy**2 or
	!     of order small for sy=10 km), hence introduce vsmall.
	if(ccoup .lt. vsmall) return ! add by @creaqi goto return clb is_return
	!
	if(expy0 .gt. small) then
		ccizqb = ccoup * expy0
		ccqb = vert * ccizqb
	endif
	if(abs(expy1) .gt. small) then
		ccizdq = ccoup * expy1
		ccdq = vert * ccizdq
	endif
	! *** write(6,*)'Integrated -- slugint = ',slugint,'  vert = ',vert,
	! ***1 '  expy = ',expy,'  fcaus = ',fcaus,'  twopi = ',twopi,
	! ***2 '  speedi = ',speedi,'  sy = ',sy,'  sz = ',sz
	!*****
	!
	!
	100 RETURN
END
!----------------------------------------------------------------------
SUBROUTINE XERFDIF(XU,XL,X0ERF,X1ERF)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 040913           XERFDIF
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  Computes the integral of X**N * ERF(X) from XL to XU
	!               for N = 0 the quantity X0ERF is computed and
	!               for N = 1 the quantity X1ERF is computed.
	!
	! --- UPDATE
	! --- 900228 - V5.741 040913 (DGS):  Fix bug that produces an error in
	!                                    integral F1. This affects the 
	!                                    analytic SLUG sampling when the
	!                                    mass in a slug changes during a
	!                                    sampling step. Analytic sampling
	!                                    is done for emitting slugs
	!                                    (youngest end is at the source).
	!
	! --- INPUTS:
	!
	!                XU - real    - Upper limit of integral (no units)
	!                XL - real    - Lower limit of integral (no units)
	!
	! --- OUTPUTS:
	!
	!             X0ERF - real    - Integral of erf(X) over XL < X < XU
	!             X1ERF - real    - Integral of X * erf(X) over XL < X < XU
	!
	! --- XERFDIF called by:  SLUGINT, SLUGAVE
	! --- XERFDIF calls:      ERF
	!----------------------------------------------------------------------
	!
	data one/1.0/,half/0.5/,fourth/0.25/,zero/0.0/
	data erfcut/3.0/,srtpi/1.772454/
	!
	expxu = zero
	erfxu = one
	if(xu.lt.0.0) erfxu = -one
	!     Note that for erfcut = 3, erf(erfcut) = 0.999978
	if(abs(xu) .le. erfcut) then
		erfxu = erf(xu)
		expxu = exp(-xu * xu)
	endif
	!
	expxl = zero
	erfxl = one
	if(xl.lt.0.0) erfxl = -one
	!     Note that for erfcut = 3, erf(erfcut) = 0.999978
	if(abs(xl) .le. erfcut) then
		erfxl = erf(xl)
		expxl = exp(-xl * xl)
	endif
	!
	x0erf = (xu*erfxu + expxu/srtpi) - (xl*erfxl + expxl/srtpi)
	x1erf = (half*xu*(xu*erfxu + expxu/srtpi) - fourth*erfxu) -&
	(half*xl*(xl*erfxl + expxl/srtpi) - fourth*erfxl)
	!
	return
end
!----------------------------------------------------------------------
FUNCTION ERFDIF(X1,X2)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 900228            ERFDIF
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  Computes the difference: erfdif = erf(x1) - erf(x2).
	!               Various methods are used to avoid roundoff errors
	!               depending on the values of the two arguments.
	!
	! --- INPUTS:
	!
	!                X1 - real    - Argument 1 (no units)
	!                X2 - real    - Argument 1 (no units)
	!
	! --- OUTPUTS:
	!
	!            ERFDIF - real    - erf(x1) - erf(x2)
	!
	! --- ERFDIF called by:  SLUGINT,SLUGAVE,SLUGSNP
	! --- ERFDIF calls:      ERF,ERFC
	!----------------------------------------------------------------------
	! *** V3.21
	!
	ERFDIF=0.0
	IF(X1.EQ.X2) return ! add by @creaqi goto return clb is_return
	if(.not. ((X1*X2).LE.0.0)) then ! add by @creaqi goto 50 in fun modify_goto_pure
		XTEST=ABS(X2)
		IF(ABS(X1).LT.XTEST) XTEST=ABS(X1)
		! --- Some compilers cannot handle reals .LT. 1.18e-38, so reset cut
		!     IF(XTEST.GE.13.306) GO TO 40
		if(xtest .GE. 9.15) return ! add by @creaqi goto return clb is_return
		if (.not. (XTEST.LT.0.47)) then ! add by @creaqi 50 state_same == True
			!     CAN ONLY REACH HERE WHEN X1 AND X2 HAVE SAME SIGN.
			ISIGN=1
			XX1=X1
			XX2=X2
			if(.not. (X1.GT.0.0)) then ! add by @creaqi goto 30 in fun modify_goto_pure
				ISIGN=-1
				XX1=-XX1
				XX2=-XX2
				!  30 ERFDIF=ISIGN*(ERFC(XX2)-ERFC(XX1))
			endif !add by @creaqi label 30 modify_goto_pure
			30 ERFCX1=0.0
			ERFCX2=0.0
			! --- Some compilers cannot handle reals .LT. 1.18e-38, so reset cut
			!     IF(XX1.LT.13.306) ERFCX1=ERFC(XX1)
			!     IF(XX2.LT.13.306) ERFCX2=ERFC(XX2)
			if(xx1 .LT. 9.15) erfcx1=ERFC(xx1)
			if(xx2 .LT. 9.15) erfcx2=ERFC(xx2)
			ERFDIF=ISIGN*(ERFCX2-ERFCX1)
			! --- Protect against flakey LAHEY compiler 4/9/89
			if(erfcx2.eq.erfcx1) erfdif=0.0
			40 RETURN
		endif ! add by @creaqi 50 state_same == True
	endif !add by @creaqi label 50 modify_goto_pure
	50 ERFDIF=ERF(X1)-ERF(X2)
	RETURN
END
!-----------------------------------------------------------------------
FUNCTION ERF(XX)
	!-----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 941228                ERF
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  Computes the error function, erf(x).
	! ---           This is the Quick medium accuracy ERROR FUNCTION from
	! ---           NBS 55.  Using an approximation due to Hastings;
	! ---           absolute error about 3e-7
	!
	!
	! --- INPUTS:
	!
	!                XX - real    - Argument  (no units)
	!
	! --- OUTPUTS:
	!
	!               ERF - real    - error function of x
	!
	! --- ERF called by:  SLUGINT, SLUGAVE, SLUGSNP, ERFDIF, XERFDIF
	! --- ERF calls:   no routines
	!----------------------------------------------------------------------
	!
	real x, xx ,t, t16, a(6)
	data a/0.0000430638, 0.0002765672, 0.0001520143,&
	0.0092705272, 0.0422820123, 0.0705230784/
	data xcut/ 3.919206/
	!
	x = abs(xx)
	if(x .gt. xcut) then
		t16 = 0.0
	else
		!
		t = ((((((((( a(1)*x + a(2) ) * x ) + a(3) ) * x ) + a(4) ) *&
		x ) + a(5) ) * x ) + a(6) ) * x
		!
		t = 1.0 / (t + 1.0)
		!
		t16 = t * t * t * t
		t16 = t16 * t16 * t16 * t16
	endif
	!
	if(xx .gt. 0.0) then
		erf =  1.0 - t16
	else
		erf =  t16 - 1.0
	endif
	!
	return
end
!-----------------------------------------------------------------------
FUNCTION ERFC(XX)
	!-----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 941228               ERFC
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  Computes the complementary error function, 1-erf(x).
	! ---           This is the Quick medium accuracy COMP. ERROR FUNCTION
	! ---           from NBS 55.  Using an approximation due to Hastings;
	! ---           absolute error about 3e-7.  Asymptotic expression added
	! ---           for large xx to reduce percent error.
	!
	!
	! --- INPUTS:
	!
	!                XX - real    - Argument  (no units)
	!
	! --- OUTPUTS:
	!
	!              ERFC - real    - complementary error function of x
	!
	! --- ERFC called by:  ERFDIF
	! --- ERFC calls:   no routines
	!-----------------------------------------------------------------------
	!
	real x, xx ,t, t16, a(6)
	data a/0.0000430638, 0.0002765672, 0.0001520143,&
	0.0092705272, 0.0422820123, 0.0705230784/
	data xcutl/-3.919206/
	data xcuth/13.306   /
	data rtpii/0.5641896/
	!
	if(xx .gt. xcuth) then
		erfc = 0.0
		!
	elseif(xx .lt. xcutl) then
		erfc = 2.0
		!
	elseif(xx .gt. 2.79) then
		x = abs(xx)
		z = 1.0 / x
		erfc = rtpii * z * exp(-x*x) * ( 1.0 - 0.5*z*z*(1.0-1.5*z*z) )
		!
	else
		x = abs(xx)
		t = ((((((((( a(1)*x + a(2) ) * x ) + a(3) ) * x ) + a(4) ) *&
		x ) + a(5) ) * x ) + a(6) ) * x
		!
		t = 1.0 / (t + 1.0)
		!
		!        erfc = t**16   for x > 0
		t16 = t * t * t * t
		t16 = t16 * t16 * t16 * t16
		!
		if(xx .gt. 0.0) then
			erfc =  t16
		else
			erfc =  2.0 - t16
		endif
		!
	endif
	!
	return
end
!----------------------------------------------------------------------
SUBROUTINE ROOT3(AA3,AA2,AA1,AA0,NR,RT)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 900228             ROOT3
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  Solves for NR multiple roots RT(I) of the cubic equation
	!               AA3 * Z**3  +  AA2 * Z**2  +  AA1 * Z  +  AA0  =  0
	!
	!               Uses J. Scire's cubic routine modified to give all
	!               all three roots.  This methodology is also described
	!               in standard math handbooks such as,
	!                      CRC STANDARD MATHEMATICAL TABLES
	!                      W. H. Beyer, CRC Press, Boca Raton, Florida
	!
	!
	! --- INPUTS:
	!
	!               AA3 - real    - Coefficient of Z**3 term (no units)
	!               AA2 - real    - Coefficient of Z**2 term (no units)
	!               AA1 - real    - Coefficient of Z**1 term (no units)
	!               AA0 - real    - Coefficient of Z**0 term (no units)
	!
	! --- OUTPUTS:
	!
	!                NR - integer - Number of real roots found.
	!                RT - real    - Array of real roots found.
	!
	! --- ROOT3 called by:  SLGTLIM
	! --- ROOT3 calls:      none
	!----------------------------------------------------------------------
	DIMENSION RT(3)
	DATA SMALL/1.0E-10/,TPID3/2.094395/,FPID3/4.188790/
	DATA ONE/1.0/,THIRD/0.3333333/
	!
	if(.not. (ABS(AA3).GT.SMALL)) then ! add by @creaqi goto 5 in fun modify_goto_pure
		CALL ROOT2(AA2,AA1,AA0,NR,RT)
		RETURN
		!
		! --- CONVERT TO CUBIC'S NOTATION.
	endif !add by @creaqi label 5 modify_goto_pure
	5 AA3I = 1.0 / AA3
	A = AA2 * AA3I
	B = AA1 * AA3I
	C = AA0 * AA3I
	!
	!     SUBROUTINE CUBIC(A,B,C,Z)
	!     SUBROUTINE CUBIC (VERSION 82102), PART OF BLP.
	!      BLP   VERSION 4.1  LEVEL 800212     CUBIC
	!     SOLVES FOR ONE ROOT OF THE CUBIC EQUATION:
	!     Z**3 + A*Z**2 + B*Z + C = 0
	!
	A3=A*THIRD
	AP=B-A*A3
	BP=A3*(2.*A3*A3 - B) + C
	AP3=AP*THIRD
	BP2=BP*0.5
	TROOT=BP2*BP2+AP3*AP3*AP3
	if(.not. (TROOT.LE.0.0)) then ! add by @creaqi goto 50 in fun modify_goto_pure
		TR=SQRT(TROOT)
		! *** RJY PROTECTION AGAINST NEGATIVE RAISED TO REAL POWER.
		APV=(-BP2+TR)
		!     IF(APV.LT.SMALL) WRITE(6,1001) APV,TR,BP2
		!     IF(APV.LT.SMALL) WRITE(*,1001) APV,TR,BP2
		!     APV = AMAX1(SMALL,APV)
		1001 format(' ***ROOT3: APV IS LT 0: APV,TR,BP2=',/,20X,3G12.5)
		! *** APV=APV**THIRD  replaced with the following on 1/11/88
		SGN=SIGN(ONE,APV)
		APP=SGN*(ABS(APV))**THIRD
		!********************************************************************
		BSV=-BP2-TR
		SGN=SIGN(ONE,BSV)
		BPP=SGN*(ABS(BSV))**THIRD
		Z=APP+BPP-A3
		RT(1) = Z
		NR = 1
		RETURN
		!
	endif !add by @creaqi label 50 modify_goto_pure
	50    CM=2.*SQRT(-AP3)
	! *** pc modification
	!     ALPHA=ARCOS(BP/(AP3*CM))*THIRD
	ALPHA=ACOS(BP/(AP3*CM))*THIRD
	! ***
	Z = CM * COS(ALPHA) - A3
	RT(1) = Z
	Z = CM * COS(ALPHA + TPID3) - A3
	RT(2) = Z
	Z = CM * COS(ALPHA + FPID3) - A3
	RT(3) = Z
	NR = 3
	RETURN
END
!----------------------------------------------------------------------
SUBROUTINE DROOT3(AA3,AA2,AA1,AA0,NR,RT)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 940831            DROOT3
	!                R. Yamartino, SRC
	!                (Modified for double precision)
	!
	! --- PURPOSE:  Solves for NR multiple roots RT(I) of the cubic equation
	!               AA3 * Z**3  +  AA2 * Z**2  +  AA1 * Z  +  AA0  =  0
	!
	!               Uses J. Scire's cubic routine modified to give all
	!               all three roots.  This methodology is also described
	!               in standard math handbooks such as,
	!                      CRC STANDARD MATHEMATICAL TABLES
	!                      W. H. Beyer, CRC Press, Boca Raton, Florida
	!
	!
	! --- INPUTS:
	!
	!               AA3 - real    - Coefficient of Z**3 term (no units)
	!               AA2 - real    - Coefficient of Z**2 term (no units)
	!               AA1 - real    - Coefficient of Z**1 term (no units)
	!               AA0 - real    - Coefficient of Z**0 term (no units)
	!
	! --- OUTPUTS:
	!
	!                NR - integer - Number of real roots found.
	!                RT - real    - Array of real roots found.
	!
	! --- DROOT3 called by:  SLGTLIM
	! --- DROOT3 calls:      none
	!----------------------------------------------------------------------
	IMPLICIT DOUBLE PRECISION (A-Z)
	real*4 RT(3),aa3,aa2,aa1,aa0,aa3i,small
	integer nr
	DATA SMALL/1.0E-10/
	!     DATA DSMALL/1.0D-10/
	DATA ONE/1.D0/,two/2.D0/,THREE/3.D0/,pi/3.1415927D0/
	third=one/three
	tpid3=two*pi*third
	fpid3=two*tpid3
	!
	if(.not. (ABS(AA3).GT.SMALL)) then ! add by @creaqi goto 5 in fun modify_goto_pure
		CALL ROOT2(AA2,AA1,AA0,NR,RT)
		RETURN
		!
		! --- CONVERT TO CUBIC'S NOTATION.
	endif !add by @creaqi label 5 modify_goto_pure
	5 AA3I = 1.0 / AA3
	A = DPROD(AA2 , AA3I)
	B = DPROD(AA1 , AA3I)
	C = DPROD(AA0 , AA3I)
	!
	!     SUBROUTINE CUBIC(A,B,C,Z)
	!     SUBROUTINE CUBIC (VERSION 82102), PART OF BLP.
	!      BLP   VERSION 4.1  LEVEL 800212     CUBIC
	!     SOLVES FOR ONE ROOT OF THE CUBIC EQUATION:
	!     Z**3 + A*Z**2 + B*Z + C = 0
	!
	A3=A*THIRD
	AP=B-A*A3
	BP=A3*(2.D0*A3*A3 - B) + C
	AP3=AP*THIRD
	BP2=BP*0.5D0
	TROOT=BP2*BP2+AP3*AP3*AP3
	if(.not. (TROOT.LE.0.D0)) then ! add by @creaqi goto 50 in fun modify_goto_pure
		TR=DSQRT(TROOT)
		! *** RJY PROTECTION AGAINST NEGATIVE RAISED TO REAL POWER.
		APV=(-BP2+TR)
		!     IF(APV.LT.DSMALL) WRITE(6,1001) APV,TR,BP2
		!     IF(APV.LT.DSMALL) WRITE(*,1001) APV,TR,BP2
		!     APV = DAMAX1(DSMALL,APV)
		1001 format(' ***ROOT3: APV IS LT 0: APV,TR,BP2=',/,20X,3G12.5)
		! *** APV=APV**THIRD  replaced with the following on 1/11/88
		SGN=DSIGN(ONE,APV)
		APP=SGN*(DABS(APV))**THIRD
		!********************************************************************
		BSV=-BP2-TR
		SGN=DSIGN(ONE,BSV)
		BPP=SGN*(DABS(BSV))**THIRD
		Z=APP+BPP-A3
		RT(1) = Z
		NR = 1
		RETURN
		!
	endif !add by @creaqi label 50 modify_goto_pure
	50    CM=2.D0*DSQRT(-AP3)
	! *** pc modification
	ALPHA=DACOS(BP/(AP3*CM))*THIRD
	! ***
	Z = CM * DCOS(ALPHA) - A3
	RT(1) = Z
	Z = CM * DCOS(ALPHA + TPID3) - A3
	RT(2) = Z
	Z = CM * DCOS(ALPHA + FPID3) - A3
	RT(3) = Z
	NR = 3
	RETURN
END
!----------------------------------------------------------------------
SUBROUTINE ROOT2(AA2,AA1,AA0,NR,RT)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 900228             ROOT2
	!                R. Yamartino, SRC
	!
	! --- PURPOSE:  Solves for NR multiple roots RT(I) of the quadradic
	!               AA2 * Z**2  +  AA1 * Z  +  AA0  =  0
	!
	!               Uses standard methodology as is described in
	!               standard math handbooks such as,
	!                      CRC STANDARD MATHEMATICAL TABLES
	!                      W. H. Beyer, CRC Press, Boca Raton, Florida
	!
	!
	! --- INPUTS:
	!
	!               AA2 - real    - Coefficient of Z**2 term (no units)
	!               AA1 - real    - Coefficient of Z**1 term (no units)
	!               AA0 - real    - Coefficient of Z**0 term (no units)
	!
	! --- OUTPUTS:
	!
	!                NR - integer - Number of real roots found.
	!                RT - real    - Array of real roots found.
	!
	! --- ROOT2 called by:  SLGTLIM
	! --- ROOT2 calls:      none
	!----------------------------------------------------------------------
	!
	!
	DIMENSION RT(2)
	DATA SMALL/1.0E-10/,HALF/0.50/,FOUR/4.0/
	!
	NR = 0
	if(.not. (ABS(AA2).GT.SMALL)) then ! add by @creaqi goto 5 in fun modify_goto_pure
		!
		IF(ABS(AA1).GT.SMALL) THEN
			RT(1) = -AA0 / AA1
			NR = 1
		ENDIF
		!
		RETURN
		!
		! ********************************************
		! --- CONSIDER TWO ROOTS.
	endif !add by @creaqi label 5 modify_goto_pure
	5 TROOT = AA1 * AA1  -  FOUR * AA2 * AA0
	! --- QUIT IF IMAGINARY ROOTS.
	IF(TROOT.LE.0.0)return ! add by @creaqi goto return clb is_return
	TR=SQRT(TROOT)
	ALF = HALF / AA2
	TERM1 = -ALF * AA1
	TERM2 =  ALF * TR
	RT(1) = TERM1 + TERM2
	RT(2) = TERM1 - TERM2
	NR = 2
	!
	50 RETURN
END
!----------------------------------------------------------------------
subroutine recspec0(rhoa,zrterr,zstak,zbase,ppcoef,&
	syr,szr,zpr)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 080520          RECSPEC0
	!                R. Yamartino, D. Strimaitis
	!
	! --- PURPOSE:  Computes the receptor-specific sigmas y and z,
	!               and receptor-specific slug height (gradual rise)
	!               with terrain adjustments, for the special case of the
	!               unaged slug (IAGE = 0) from either an area source or
	!               a line source
	!
	! --- UPDATE
	! --- V6.261-V6.265 090612  (DGS): Add check for zero final rise from a
	!                                  buoyant line source (zfrises) when
	!                                  computing GRBID
	! --- V6.11-V6.261  080520  (DGS): Replace individual LN1,LN2 rise
	!                                  tables with /SRCTAB/ arrays from
	!                                  Direct Access file
	! --- V6.1-V6.11    060309  (DGS): turn OFF the debug output (LDBG)
	! --- V5.73-V6.1    050915  (DGS): add emission step index to rise 
	!                                  tables for variable sources
	! --- V5.0-V5.73    040611  (DGS): add gravitational settling for one
	!                                  particle size (plume tilt)
	! --- V4.0-V5.0     971107  (DGS): add treatment for variable line
	!                                  sources (istype=6)
	!
	! --- INPUTS:
	!
	!              RHOA - real    - Upwind receptor-source distance (m)
	!            ZRTERR - real    - Terrain elevation (m MSL) at receptor
	!             ZSTAK - real    - Stack height of source of puff (m)
	!             ZBASE - real    - Stack base elevation MSL (m)
	!            PPCOEF - real    - Plume path coefficient
	!
	!     Common Block /COMPARM/ variables:
	!        SYMIN, SZMIN
	!     Common Block /CURRENT/ variables:
	!        XE1, YE1, ZE1, SYE1, SZE1,
	!        XE2, YE2, ZE2, SYE2, SZE2,
	!        te1,vsetl,
	!        vtye1,vtze1,vtye2,vtze2,
	!        vdye1,vdze1,vdye2,vdze2,
	!        bidsq, IPNUM, ISNUM, ISTYPE, IQSTEP,
	!        xupedge, xfrise, xshift, zfrise
	!     Common Block /FLAGS/ variables:
	!        MTRANS, MTILT
	!     Common Block /PUFF/ variables:
	!        IDW0
	!     Common Block /SLGLIN/ variables:
	!        DXY12,RDXY12
	!     Common block /SRCTAB/ variables:
	!           NTR, XTR(mxrise), ZTR(mxrise)
	!
	!     Parameters:
	!
	! --- OUTPUTS:
	!
	!               SYR - real    - Receptor sigma Y (m)
	!               SZR - real    - Receptor sigma Z (m)
	!               ZPR - real    - Receptor-specific slug height after any
	!                               terrain adjustments (m)
	!
	!     Common Block /CURRENT/ variables revised "locally":
	!        xfrise, xshift, zfrise
	!
	! --- RECSPEC0 called by:  CPTRAP
	! --- RECSPEC0 calls:      HEFTRAN, SIGTY, SIGTZ, GRISE, CTADJ
	!----------------------------------------------------------------------
	!
	! --- Include parameter block
	include 'params.puf'
	! --- Include common blocks
	include 'comparm.puf'
	include 'pt1.puf'
	include 'flags.puf'
	include 'puff.puf'
	include 'current.puf'
	include 'srctab.puf'
	COMMON /SLGLIN/ DXY12,RDXY12
	logical ldbg
	data zero/0.0/
	! --- Set list file output switch for testing
	ldbg=.FALSE.
	! --- ldbg=.TRUE.
	! -------------------------------------------------------------------
	! --- Determine the spatial interpolation factor for along-slug
	! --- positions
	! -------------------------------------------------------------------
	!
	! --- Fully extended slug position  (FRACSF)
	!
	! --- DXY12 is projection of the slug length on the x-y plane.
	! --- FRACSF is the fraction RHOA/DXY12 for the fully-extended slug
	! --- FRACSF = 0 corresponds to the source location
	! --- FRACSF = 1 corresponds to the slug end furthest downwind
	FRACSF = RHOA * RDXY12
	! -------------------------------------------------------------------
	! --- Find the receptor-specific sigmas for the fully-extended slug
	! -------------------------------------------------------------------
	! --- Do linear spatial interpolation of the virtual t,x but limit the
	! --- virtual quantities to be positive via use of AMAX1.
	vty = vtye2 + fracsf * (vtye1 - vtye2)
	vty = amax1(vty,zero)
	vtz = vtze2 + fracsf * (vtze1 - vtze2)
	vtz = amax1(vtz,zero)
	vdy = vdye2 + fracsf * (vdye1 - vdye2)
	vdy = amax1(vdy,zero)
	vdz = vdze2 + fracsf * (vdze1 - vdze2)
	vdz = amax1(vdz,zero)
	!
	! -------------------------------------------------------------------
	! --- Redefine line-source data for current source location ---
	! --- (integrator dynamically breaks line sources into segments, so
	! ---  that rise from these will differ from that in puff arrays)
	! -------------------------------------------------------------------
	! --- Save "real" /CURRENT/ values before altering them for rise
	xshifts=xshift
	xfrises=xfrise
	zfrises=zfrise
	if(istype.EQ.5 .OR. istype.EQ.6) then
		! ---    Constant or variable line sources
		! ---    Must have something in the rise arrays
		if(ntr.LE.0) then
			write(io6,*)
			write(io6,*) 'FATAL ERROR in RECSPEC0: invalid array'
			write(io6,*) 'Expected NTR greater than 0'
			write(io6,*) 'Found NTR = ',ntr
			write(io6,*)
			write(*,*)
			stop 'Halted in RECSPEC0 -- See list file'
		endif
		xshift=amax1(zero,xupedge-rhoa)
		if(xshift.GT.xtr(1)) xshift=xtr(1)
		if(xtr(1).GE.xtr(ntr)) then
			! ---       Case 1:  Final rise reached before "full buoyancy" distance
			size=1.0-xshift/xtr(1)
			xfrise=xtr(ntr)*size
			zfrise=ztr(1)*size
		else
			! ---       Case 2:  Final rise reached beyond "full buoyancy" distance
			xfrise=xtr(ntr)-xshift
			zfrise=ztr(ntr)-xshift*ztr(1)/xtr(1)
		endif
	endif
	! --- Set final rise height for this source location
	hfinal=zstak+zfrise
	!
	! --- Reset Heffter transition for sigma-z for current slug
	call heftran(2,hfinal,zero,zero,vtze1,vtze2,vtye1,vtye2)
	!
	! --- Now compute the receptor-specific sigmas (syar,szar) via 'forward'
	! --- calls to the sigma routines using the VIRTUALS as space/time step.
	call sigty(zero,vdy,vty,syar,dum1,dum2)
	call sigtz(zero,vdz,vtz,hfinal,szar,dum1,dum2)
	!
	! -------------------------------------------------------------------
	! --- Compute the gradual rise for BID
	! -------------------------------------------------------------------
	! --- Initialize gradual rise plume height to final rise value
	hgr=hfinal
	if(zfrise.LE.zero .OR. zfrises.LE.zero) then
		grbid=zero
	else
		grbid=bidsq*(zfrise/zfrises)**2
	endif
	!
	! --- Check for gradual rise
	if(rhoa.LT.xfrise .AND. rhoa.GT.zero) then
		! ---    Compute gradual rise
		if(idw0(ipnum).GT.0) then
			! ---       Building downwash (line source -- no BID)
			call grise(rhoa,hgr,risefac)
			grbid=zero
		else
			! ---       No building downwash (also, no stack-tip DW for these
			! ---       sources either)
			call grise(rhoa,hgr,risefac)
			grbid=grbid*risefac**2
		endif
	elseif(rhoa.LE.zero) then
		! ---    Receptor upwind
		hgr=zstak
		grbid=zero
	endif
	! --- Adjust sigmas:  add contribution due to buoyancy enhancement
	syr=syar
	szr=szar
	if(grbid.GT.zero) then
		syr=sqrt(syar**2+grbid)
		szr=sqrt(szar**2+grbid)
	endif
	! --- Set a "floor" to the sigma values equal to SYMIN, SZMIN
	if(syr .LE. symin) syr=symin
	if(szr .LE. szmin) szr=szmin
	! -------------------------------------------------------------------
	! --- Set slug height at receptor
	! -------------------------------------------------------------------
	! --- Use gradual rise height if requested, or if downwash is active
	if(mtrans.EQ.1 .OR. idw0(ipnum).GT.0) then
		zpr = hgr
	else
		zpr = hfinal
	endif
	! --- Estimate slug height at receptor with gravitational settling
	! --- (Initial Implementation!)
	if(mtilt.EQ.1) then
		! ---    Travel time to end of slug (FRACSF=1) is te1 in /CURRENT/
		dzprg=-fracsf*te1*vsetl
		dzprg=AMIN1(0.0,dzprg)
	else
		dzprg=0.0
	endif
	! --- Account for settling (may be negative)
	zpr=zpr+dzprg
	if(mctadj.eq.1 .OR. mctadj.eq.3) then
		call ctadj(zrterr,zpr,zstak,zbase,ppcoef,zpra)
		zpr=zpra
	endif
	! --- Condition final effective puff ht to be non-negative
	zpr=AMAX1(zpr,0.0)
	!***
	if(LDBG) then
		write(io6,*)'RECSPEC0: rhoa, xfrise = ',rhoa,xfrise
		write(io6,*)'              syr, szr = ',syr,szr
		write(io6,*)'        xshift, zfrise = ',xshift,zfrise
		write(io6,*)'      xshifts, zfrises = ',xshifts,zfrises
		write(io6,*)'      hfinal, hgr, zpr = ',hfinal,hgr,zpr
		write(io6,*)'    fracsf, te1, dzprg = ',fracsf,te1,dzprg
	endif
	!***
	! --- Restore "real" rise values to common /CURRENT/
	xshift=xshifts
	xfrise=xfrises
	zfrise=zfrises
	return
end
!----------------------------------------------------------------------
subroutine out(rarray,iarray,ityp,nsigd,ldate,messag,nx,ny)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 050915               OUT
	! ---            J. Scire, SRC
	!
	! --- PURPOSE:  Write a gridded field of real or integer numbers
	!
	! --- UPDATE
	! --- V5.4-V6.1     050915  (DGS): resolve times to the second and
	! --- V5.2-V5.4     000602  (DGS): add message to "stop"
	!                   000602  (DGS): increase format for cell IDs
	! --- V5.0-V5.2     991104  (DGS): YYYY format for year
	!
	! --- INPUTS:
	!     RARRAY(MXNX,MXNY) - Real array  - Array of real numbers to print
	!                                       (used only if ITYP = 1)
	!     IARRAY(MXNX,MXNY) - Int. array  - Array of integer numbers to
	!                                       print (used only if ITYP = 2)
	!                  ITYP - Integer     - Array type (1=real, 2=integer)
	!                 NSIGD - Integer     - No. digits to print (valid range
	!                                       for NSIGD is 1 to 5)
	!                 LDATE - Logical     - Control variable for printing
	!                                       of date (.true. = print date in
	!                                       common /GEN/, .false. = do not
	!                                       print date)
	!                MESSAG - Char.*70    - Label of table
	!                    NX - Integer     - No. X grid cells being used in
	!                                       array
	!                    NY - Integer     - No. Y grid cells being used in
	!                                       array
	!       Common block /DATEHR/ variables: - (Used only if LDATE=.true.)
	!          NYRB, NMOB, NDAYB, NJULB, NHRB, NSECB,
	!          NYRE, NMOE, NDAYE, NJULE, NHRE, NSECE
	!       Parameters: MXNX, MXNY, IO6
	!
	! --- OUTPUT:  none
	!
	! --- OUT    called by:  MET1, CHEMI
	! --- OUT    calls:      WRT, WRT2
	!----------------------------------------------------------------------
	!
	! --- Include parameters
	include 'params.puf'
	!
	! --- Include date/hour common block
	include 'datehr.puf'
	!
	real rarray(mxnx,mxny)
	!
	integer iarray(mxnx,mxny),icol(5)
	integer iout(mxnx)
	!
	logical ldate
	!
	character*70 messag
	character*1 sign(mxnx),plus,minus
	character*24 form1(5)
	character*21 form2(5)
	character*18 form3(5)
	!
	data icol /40,40,30,25,20/
	data plus,minus /'+','-'/
	data form1 /'(1x,i3,1x,1hI,40(i3,1x))',&
	'(1x,i3,1x,1hI,40(i3,1x))',&
	'(1x,i3,1x,1hI,40(i3,1x))',&
	'(1x,i3,1x,1hI,40(i4,1x))',&
	'(1x,i3,1x,1hI,40(i5,1x))'/
	data form2 /'(5x,1hI,40(2x,a1,1x))',&
	'(5x,1hI,40(2x,a1,1x))',&
	'(5x,1hI,40(2x,a1,1x))',&
	'(5x,1hI,40(3x,a1,1x))',&
	'(5x,1hI,40(4x,a1,1x))'/
	data form3 /'(6x,40(i3,1x))',&
	'(6x,40(i3,1x))',&
	'(6x,40(i3,1x))',&
	'(6x,40(i4,1x))',&
	'(6x,40(i5,1x))'/
	!
	! --- check that valid values of array type (ityp) and print digits
	! --- (nsigd) have been passed to routine
	if(ityp.ne.1.and.ityp.ne.2)then
		write(io6,*)'ERROR in SUBR. OUT -- invalid value of ITYP -- ',&
		'ITYP = ',ityp
		write(*,*)
		stop 'Halted in OUT -- see list file.'
	endif
	if(nsigd.lt.1.or.nsigd.gt.5)then
		write(io6,*)'ERROR in SUBR. OUT -- invalid value of NSIGD -- ',&
		'NSIGD = ',nsigd
		write(*,*)
		stop 'Halted in OUT -- see list file.'
	endif
	!
	icr=2
	if(nsigd.eq.1)icr=1
	if(mod(nx,icol(nsigd)).eq.0)then
		npass=nx/icol(nsigd)
	else
		npass=nx/icol(nsigd)+1
	endif
	!
	! --- real array -- find min. & max. values
	if(.not. (ityp.ne.1)) then ! add by @creaqi goto 50 in fun modify_goto_pure
		xmax=-1.e-25
		xmin=1.e25
		do i=1,nx! add by @creaqi do label 10
			do j=1,ny! add by @creaqi do label 10
				if(rarray(i,j).gt.xmax)xmax=rarray(i,j)
				if(rarray(i,j).lt.xmin)xmin=rarray(i,j)
			enddo !add by @creaqi 10
		enddo !add by @creaqi 10
		10    continue
		if(.not. (xmin.ne.0.0.or.xmax.ne.0.0)) then ! add by @creaqi goto 12 in fun modify_goto_pure
				if(ldate)write(io6,94)messag,nyrb,nmob,ndayb,njulb,nhrb,nsecb,&
			nyre,nmoe,ndaye,njule,nhre,nsece
			if(.not.ldate)write(io6,95)messag
			write(io6,11)
			11    format(1x,'GRID NOT PRINTED -- all values zero')
			return
			!
		endif !add by @creaqi label 12 modify_goto_pure
		12    continue
		xexp=xmax
		if(abs(xmin).gt.xmax)xexp=abs(xmin)
		iexp=alog10(xexp)
		if(xexp.lt.1.0)iexp=iexp-1
		nexp=iexp-(nsigd-icr)
		xscale=10.**(-nexp)
		!
		ic2=0
		do ipass=1,npass! add by @creaqi do label 30
			!
				if(ldate)write(io6,94)messag,nyrb,nmob,ndayb,njulb,nhrb,nsecb,&
			nyre,nmoe,ndaye,njule,nhre,nsece
			if(.not.ldate)write(io6,95)messag
			write(io6,109)nexp
			!
			ic1=ic2+1
			ic2=ic2+icol(nsigd)
			if(ic2.gt.nx)ic2=nx
			!
			do jj=ny,1,-1! add by @creaqi do label 20
				icnt=0
				!
				do i=ic1,ic2! add by @creaqi do label 18
					icnt=icnt+1
					if(rarray(i,jj).lt.0)then
						iout(icnt)=-(rarray(i,jj)*xscale-0.5)
						sign(icnt)=minus
					else
						iout(icnt)=rarray(i,jj)*xscale+0.5
						sign(icnt)=plus
					endif
				enddo !add by @creaqi 18
				18          continue
				call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,io6)
			enddo !add by @creaqi 20
			20       continue
			nund=(nsigd+1)*icnt-1
			if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
			write(io6,101)(minus,n=1,nund)
			101   format(5x,128a1)
			call wrt2(form3(nsigd),ic1,ic2,io6)
		enddo !add by @creaqi 30
		30    continue
		return
		!
		! --- integer array -- find min. & max. values
	endif !add by @creaqi label 50 modify_goto_pure
	50    continue
	kmax=-9999999
	kmin=9999999
	do i=1,nx! add by @creaqi do label 110
		do j=1,ny! add by @creaqi do label 110
			if(iarray(i,j).gt.kmax)kmax=iarray(i,j)
			if(iarray(i,j).lt.kmin)kmin=iarray(i,j)
		enddo !add by @creaqi 110
	enddo !add by @creaqi 110
	110   continue
	if(.not. (kmin.ne.0.or.kmax.ne.0)) then ! add by @creaqi goto 102 in fun modify_goto_pure
			if(ldate)write(io6,94)messag,nyrb,nmob,ndayb,njulb,nhrb,nsecb,&
		nyre,nmoe,ndaye,njule,nhre,nsece
		if(.not.ldate)write(io6,95)messag
		write(io6,11)
		return
		!
	endif !add by @creaqi label 102 modify_goto_pure
	102   continue
	xexp=kmax
	if(iabs(kmin).gt.kmax)xexp=iabs(kmin)
	iexp=alog10(xexp)
	if(xexp.lt.1.0)iexp=iexp-1
	nexp=iexp-(nsigd-icr)
	xscale=10.**(-nexp)
	!
	ic2=0
	do ipass=1,npass! add by @creaqi do label 130
		!
			if(ldate)write(io6,94)messag,nyrb,nmob,ndayb,njulb,nhrb,nsecb,&
		nyre,nmoe,ndaye,njule,nhre,nsece
		if(.not.ldate)write(io6,95)messag
		write(io6,109)nexp
		!
		ic1=ic2+1
		ic2=ic2+icol(nsigd)
		if(ic2.gt.nx)ic2=nx
		!
		do jj=ny,1,-1! add by @creaqi do label 120
			icnt=0
			!
			do i=ic1,ic2! add by @creaqi do label 118
				icnt=icnt+1
				if(iarray(i,jj).lt.0)then
					iout(icnt)=-(iarray(i,jj)*xscale-0.5)
					sign(icnt)=minus
				else
					iout(icnt)=iarray(i,jj)*xscale+0.5
					sign(icnt)=plus
				endif
			enddo !add by @creaqi 118
			118         continue
			call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,io6)
		enddo !add by @creaqi 120
		120      continue
		nund=(nsigd+1)*icnt-1
		if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
		write(io6,101)(minus,n=1,nund)
		call wrt2(form3(nsigd),ic1,ic2,io6)
	enddo !add by @creaqi 130
	130   continue
	!
	return
	94    format(/1x,a70,2x,'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,&
	'Julian day: ',i3,2x,'hr: ',i2,2x,'sec: ',i4/,t70,&
	'to year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,&
	'Julian day: ',i3,2x,'hr: ',i2,2x,'sec: ',i4/)
	95    format(/1x,a70/)
	109   format(1x,'Multiply all values by 10 ** ',i3/)
end
!----------------------------------------------------------------------
subroutine wrt(form1,form2,jj,iout,sign,n,io6)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 940430               WRT
	! ---            J. Scire, SRC
	!
	! --- PURPOSE:  Write one Y row of gridded data
	!
	! --- INPUTS:
	!              FORM1 - Char.*24    - Format field for Y label and data
	!                                    to be printed
	!              FORM2 - Char.*21    - Format field for sign of data
	!                 JJ - Integer     - Y grid cell number
	!            IOUT(N) - Int. array  - Array of data to be printed
	!                                    (one Y row)
	!            SIGN(N) - Char.*1     - Array containing sign of data
	!                                    ('+' or '-')
	!                  N - Integer     - Number of cells in this row
	!                IO6 - Integer     - Fortran unit no. of output
	!
	! --- OUTPUT:  none
	!
	! --- WRT called by:  OUT, OUTSAM
	! --- WRT calls:      none
	!----------------------------------------------------------------------
	integer iout(n)
	!
	character*1 sign(n)
	character*24 form1
	character*21 form2
	!
	write(io6,form1)jj,iout
	write(io6,form2)sign
	!
	return
end
!----------------------------------------------------------------------
subroutine wrt2(form,n1,n2,io6)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 940430              WRT2
	! ---            J. Scire, SRC
	!
	! --- PURPOSE:  Write a line labeling grid cell numbers
	!
	! --- INPUTS:
	!               FORM - Char.*18    - Format field of data to be printed
	!                 N1 - Integer     - Starting grid cell number
	!                 N2 - Integer     - Ending grid cell number
	!                IO6 - Integer     - Fortran unit no. of output
	!
	! --- OUTPUT:  none
	!
	! --- WRT2 called by:  OUT, OUTSAM
	! --- WRT2 calls:      none
	!----------------------------------------------------------------------
	character*18 form
	!
	write(io6,form)(i,i=n1,n2)
	return
end
!----------------------------------------------------------------------
subroutine outsam(rarray,iarray,ityp,nsigd,ldate,messag,nx,ny)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 050915            OUTSAM
	! ---            J. Scire, SRC
	!
	! --- PURPOSE:  Write a gridded field of real or integer numbers
	! ---           Array dimensions sized for SAMPLING grid
	!
	! --- UPDATE
	! --- V5.4-V6.1     050915  (DGS): resolve times to the second
	! --- V5.2-V5.4     000602  (DGS): add message to "stop"
	! --- V5.0-V5.2     991104  (DGS): YYYY format for year
	!
	! --- INPUTS:
	!   RARRAY(MXNXG,MXNYG) - Real array  - Array of real numbers to print
	!                                       (used only if ITYP = 1)
	!   IARRAY(MXNXG,MXNYG) - Int. array  - Array of integer numbers to
	!                                       print (used only if ITYP = 2)
	!                  ITYP - Integer     - Array type (1=real, 2=integer)
	!                 NSIGD - Integer     - No. digits to print (valid range
	!                                       for NSIGD is 1 to 5)
	!                 LDATE - Logical     - Control variable for printing
	!                                       of date (.true. = print date in
	!                                       common /GEN/, .false. = do not
	!                                       print date)
	!                MESSAG - Char.*70    - Label of table
	!                    NX - Integer     - No. X grid cells being used in
	!                                       array
	!                    NY - Integer     - No. Y grid cells being used in
	!                                       array
	!       Common block /DATEHR/ variables: - (Used only if LDATE=.true.)
	!          NYRAB, NMOAB, NDAYAB, NJULAB, NHRAB, NSECAB,
	!          NYRE, NMOE, NDAYE, NJULE, NHRE, NSECE
	!       Parameters: MXNXG, MXNYG, IO6
	!
	! --- OUTPUT:  none
	!
	! --- OUTSAM called by:  OUTPUT
	! --- OUTSAM calls:      WRT, WRT2
	!----------------------------------------------------------------------
	!
	! --- Include parameters
	include 'params.puf'
	!
	! --- Include date/hour common block
	include 'datehr.puf'
	!
	real rarray(mxnxg,mxnyg)
	!
	integer iarray(mxnxg,mxnyg),icol(5)
	integer iout(mxnxg)
	!
	logical ldate
	!
	character*70 messag
	character*1 sign(mxnxg),plus,minus
	character*24 form1(5)
	character*21 form2(5)
	character*18 form3(5)
	!
	data icol /40,40,30,25,20/
	data plus,minus /'+','-'/
	data form1 /'(1x,i2,1x,1hI,40(i2,1x))',&
	'(1x,i2,1x,1hI,40(i2,1x))',&
	'(1x,i2,1x,1hI,40(i3,1x))',&
	'(1x,i2,1x,1hI,40(i4,1x))',&
	'(1x,i2,1x,1hI,40(i5,1x))'/
	data form2 /'(4x,1hI,40(1x,a1,1x))',&
	'(4x,1hI,40(1x,a1,1x))',&
	'(4x,1hI,40(2x,a1,1x))',&
	'(4x,1hI,40(3x,a1,1x))',&
	'(4x,1hI,40(4x,a1,1x))'/
	data form3 /'(5x,40(i2,1x))',&
	'(5x,40(i2,1x))',&
	'(5x,40(i3,1x))',&
	'(5x,40(i4,1x))',&
	'(5x,40(i5,1x))'/
	!
	! --- check that valid values of array type (ityp) and print digits
	! --- (nsigd) have been passed to routine
	if(ityp.ne.1.and.ityp.ne.2)then
		write(io6,*)'ERROR in SUBR. OUTSAM -- invalid value of ITYP',&
		' -- ITYP = ',ityp
		write(*,*)
		stop 'Halted in OUTSAM -- see list file.'
	endif
	if(nsigd.lt.1.or.nsigd.gt.5)then
		write(io6,*)'ERROR in SUBR. OUTSAM -- invalid value of NSIGD',&
		' -- NSIGD = ',nsigd
		write(*,*)
		stop 'Halted in OUTSAM -- see list file.'
	endif
	!
	icr=2
	if(nsigd.eq.1)icr=1
	if(mod(nx,icol(nsigd)).eq.0)then
		npass=nx/icol(nsigd)
	else
		npass=nx/icol(nsigd)+1
	endif
	!
	! --- real array -- find min. & max. values
	if(.not. (ityp.ne.1)) then ! add by @creaqi goto 50 in fun modify_goto_pure
		xmax=-1.e-25
		xmin=1.e25
		do i=1,nx! add by @creaqi do label 10
			do j=1,ny! add by @creaqi do label 10
				if(rarray(i,j).gt.xmax)xmax=rarray(i,j)
				if(rarray(i,j).lt.xmin)xmin=rarray(i,j)
			enddo !add by @creaqi 10
		enddo !add by @creaqi 10
		10    continue
		if(.not. (xmin.ne.0.0.or.xmax.ne.0.0)) then ! add by @creaqi goto 12 in fun modify_goto_pure
				if(ldate)write(io6,94)messag,nyrab,nmoab,ndayab,njulab,nhrab,&
			nsecab,nyre,nmoe,ndaye,njule,nhre,nsece
			if(.not.ldate)write(io6,95)messag
			write(io6,11)
			11    format(1x,'GRID NOT PRINTED -- all values zero')
			return
			!
		endif !add by @creaqi label 12 modify_goto_pure
		12    continue
		xexp=xmax
		if(abs(xmin).gt.xmax)xexp=abs(xmin)
		iexp=alog10(xexp)
		if(xexp.lt.1.0)iexp=iexp-1
		nexp=iexp-(nsigd-icr)
		xscale=10.**(-nexp)
		!
		ic2=0
		do ipass=1,npass! add by @creaqi do label 30
			!
				if(ldate)write(io6,94)messag,nyrab,nmoab,ndayab,njulab,nhrab,&
			nsecab,nyre,nmoe,ndaye,njule,nhre,nsece
			if(.not.ldate)write(io6,95)messag
			write(io6,109)nexp
			!
			ic1=ic2+1
			ic2=ic2+icol(nsigd)
			if(ic2.gt.nx)ic2=nx
			!
			do jj=ny,1,-1! add by @creaqi do label 20
				icnt=0
				!
				do i=ic1,ic2! add by @creaqi do label 18
					icnt=icnt+1
					if(rarray(i,jj).lt.0)then
						iout(icnt)=-(rarray(i,jj)*xscale-0.5)
						sign(icnt)=minus
					else
						iout(icnt)=rarray(i,jj)*xscale+0.5
						sign(icnt)=plus
					endif
				enddo !add by @creaqi 18
				18          continue
				call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,io6)
			enddo !add by @creaqi 20
			20       continue
			nund=(nsigd+1)*icnt-1
			if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
			write(io6,101)(minus,n=1,nund)
			101   format(5x,128a1)
			call wrt2(form3(nsigd),ic1,ic2,io6)
		enddo !add by @creaqi 30
		30    continue
		return
		!
		! --- integer array -- find min. & max. values
	endif !add by @creaqi label 50 modify_goto_pure
	50    continue
	kmax=-9999999
	kmin=9999999
	do i=1,nx! add by @creaqi do label 110
		do j=1,ny! add by @creaqi do label 110
			if(iarray(i,j).gt.kmax)kmax=iarray(i,j)
			if(iarray(i,j).lt.kmin)kmin=iarray(i,j)
		enddo !add by @creaqi 110
	enddo !add by @creaqi 110
	110   continue
	if(.not. (kmin.ne.0.or.kmax.ne.0)) then ! add by @creaqi goto 102 in fun modify_goto_pure
			if(ldate)write(io6,94)messag,nyrab,nmoab,ndayab,njulab,nhrab,&
		nsecab,nyre,nmoe,ndaye,njule,nhre,nsece
		if(.not.ldate)write(io6,95)messag
		write(io6,11)
		return
		!
	endif !add by @creaqi label 102 modify_goto_pure
	102   continue
	xexp=kmax
	if(iabs(kmin).gt.kmax)xexp=iabs(kmin)
	iexp=alog10(xexp)
	if(xexp.lt.1.0)iexp=iexp-1
	nexp=iexp-(nsigd-icr)
	xscale=10.**(-nexp)
	!
	ic2=0
	do ipass=1,npass! add by @creaqi do label 130
		!
			if(ldate)write(io6,94)messag,nyrab,nmoab,ndayab,njulab,nhrab,&
		nsecab,nyre,nmoe,ndaye,njule,nhre,nsece
		if(.not.ldate)write(io6,95)messag
		write(io6,109)nexp
		!
		ic1=ic2+1
		ic2=ic2+icol(nsigd)
		if(ic2.gt.nx)ic2=nx
		!
		do jj=ny,1,-1! add by @creaqi do label 120
			icnt=0
			!
			do i=ic1,ic2! add by @creaqi do label 118
				icnt=icnt+1
				if(iarray(i,jj).lt.0)then
					iout(icnt)=-(iarray(i,jj)*xscale-0.5)
					sign(icnt)=minus
				else
					iout(icnt)=iarray(i,jj)*xscale+0.5
					sign(icnt)=plus
				endif
			enddo !add by @creaqi 118
			118         continue
			call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,io6)
		enddo !add by @creaqi 120
		120      continue
		nund=(nsigd+1)*icnt-1
		if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
		write(io6,101)(minus,n=1,nund)
		call wrt2(form3(nsigd),ic1,ic2,io6)
	enddo !add by @creaqi 130
	130   continue
	!
	return
	94    format(/1x,a70,2x,'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,&
	'Julian day: ',i3,2x,'hr: ',i2,2x,'sec: ',i4/,t71,&
	'to year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,&
	'Julian day: ',i3,2x,'hr: ',i2,2x,'sec: ',i4/)
	95    format(/1x,a70/)
	109   format(1x,'Multiply all values by 10 ** ',i3/)
end
!----------------------------------------------------------------------
subroutine rdi1d(iomet,mtver,idat,nwords,clabel,ndathrb,&
	nsecb,ndathre,nsece)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 050915             RDI1D
	!                J. Scire, Earth Tech
	!
	! --- PURPOSE:  Read "NWORDS" of a one-dimensional integer array
	!
	! --- UPDATE
	! --- V5.0-V6.1     050915  (DGS): resolve times to the second, and
	!                                  include begin/end times
	!                                  (remains compatible with
	!                                  older end-time version)
	!
	! --- INPUTS:
	!         IOMET - integer       - Fortran unit number of input file
	!         MTVER - integer       - Time-mark flag
	!                                 0: end-time (no seconds)
	!                                 1: begin-time / end-time
	!
	! --- OUTPUT:
	!  IDAT(nwords) - integer array - Array read from file
	!        NWORDS - integer       - Number of words to read
	!        CLABEL - character*8   - Variable name
	!       NDATHRB - integer       - Beginning date and time (YYYYJJJHH)
	!         NSECB - integer       - Beginning seconds (SSSS)
	!       NDATHRE - integer       - Ending date and time (YYYYJJJHH)
	!         NSECE - integer       - Ending seconds (SSSS)
	!
	! --- RDI1D called by:  RDMET
	! --- RDI1D calls:      none
	!----------------------------------------------------------------------
	!
	integer idat(nwords)
	character*8 clabel
	!
	if(mtver.EQ.1) then
		read(iomet)clabel,ndathrb,nsecb,ndathre,nsece,idat
	elseif(mtver.EQ.0) then
		read(iomet)clabel,ndathre,idat
		nsece=0
		ndathrb=0
		nsecb=0
	endif
	return
end
!----------------------------------------------------------------------
subroutine rdi2d(iomet,mtver,idat,ibuf,mxnx,mxny,nx,ny,clabel,&
	ndathrb,nsecb,ndathre,nsece)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 050915             RDI2D
	!                J. Scire, Earth Tech
	!
	! --- PURPOSE:  Read NX * NY words of a 2-D integer array
	!
	! --- UPDATE
	! --- V5.0-V6.1     050915  (DGS): resolve times to the second, and
	!                                  include begin/end times for CALMET
	!                                  Version 6 (remains compatible with
	!                                  older version of CALMET)
	!
	! --- INPUTS:
	!               IOMET - integer       - Fortran unit number of input
	!                                       file
	!               MTVER - integer       - Time-mark flag
	!                                       0: end-time (no seconds)
	!                                       1: begin-time / end-time
	!         IBUF(nx,ny) - integer array - Buffer to hold input data
	!           MXNX,MXNY - integers      - Dimensions of data array
	!               NX,NY - integers      - Actual size of grid to read
	!
	! --- OUTPUT:
	!     IDAT(mxnx,mxny) - integer array - Input data array (padded if
	!                                       necessary)
	!              CLABEL - character*8   - Variable name
	!             NDATHRB - integer       - Beginning date and time (YYYYJJJHH)
	!               NSECB - integer       - Beginning seconds (SSSS)
	!             NDATHRE - integer       - Ending date and time (YYYYJJJHH)
	!               NSECE - integer       - Ending seconds (SSSS)
	!
	! --- RDI2D called by:  MET1, RDMET
	! --- RDI2D calls:      none
	!----------------------------------------------------------------------
	integer idat(mxnx,mxny),ibuf(nx,ny)
	character*8 clabel
	!
	if(nx.eq.mxnx.and.ny.eq.mxny)then
		!
		! ---    entire array is being used -- read full grid
		if(mtver.EQ.1) then
			read(iomet)clabel,ndathrb,nsecb,ndathre,nsece,idat
		elseif(mtver.EQ.0) then
			read(iomet)clabel,ndathre,idat
			nsece=0
			ndathrb=0
			nsecb=0
		endif
	else
		!
		! ---    only a portion of grid being used -- read and
		! ---    transfer from buffer
		!
		if(mtver.EQ.1) then
			read(iomet)clabel,ndathrb,nsecb,ndathre,nsece,ibuf
		elseif(mtver.EQ.0) then
			read(iomet)clabel,ndathre,ibuf
			nsece=0
			ndathrb=0
			nsecb=0
		endif
		!
		do i=1,nx! add by @creaqi do label 10
			do j=1,ny! add by @creaqi do label 10
				idat(i,j)=ibuf(i,j)
			enddo !add by @creaqi 10
		enddo !add by @creaqi 10
		10       continue
	endif
	!
	return
end
!----------------------------------------------------------------------
subroutine rdr1d(iomet,mtver,x,nwords,clabel,&
	ndathrb,nsecb,ndathre,nsece)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1         Level: 050915            RDR1D
	!                J. Scire, Earth Tech
	!
	! --- PURPOSE:  Read "NWORDS" of a one-dimensional real array
	!
	! --- UPDATE
	! --- V5.0-V6.1     050915  (DGS): resolve times to the second, and
	!                                  include begin/end times for CALMET
	!                                  Version 6 (remains compatible with
	!                                  older version of CALMET)
	!
	! --- INPUTS:
	!         IOMET - integer     - Fortran unit number of input file
	!         MTVER - integer     - Time-mark flag
	!                               0: end-time (no seconds)
	!                               1: begin-time / end-time
	!
	! --- OUTPUT:
	!     X(nwords) - real array  - Array read from file
	!        NWORDS - integer     - Number of words to read
	!        CLABEL - character*8 - Variable name
	!       NDATHRB - integer     - Beginning date and time (YYYYJJJHH)
	!         NSECB - integer     - Beginning seconds (SSSS)
	!       NDATHRE - integer     - Ending date and time (YYYYJJJHH)
	!         NSECE - integer     - Ending seconds (SSSS)
	!
	! --- RDR1D called by:  MET1, RDMET
	! --- RDR1D calls:      none
	!----------------------------------------------------------------------
	real x(nwords)
	character*8 clabel
	!
	if(mtver.EQ.1) then
		read(iomet)clabel,ndathrb,nsecb,ndathre,nsece,x
	elseif(mtver.EQ.0) then
		read(iomet)clabel,ndathre,x
		nsece=0
		ndathrb=0
		nsecb=0
	endif
	return
end
!----------------------------------------------------------------------
subroutine rdr2d(iomet,mtver,x,xbuf,mxnx,mxny,nx,ny,clabel,&
	ndathrb,nsecb,ndathre,nsece,ieof)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 050915             RDR2D
	!                J. Scire, Earth Tech
	!
	! --- PURPOSE:  Read NX * NY words of a 2-D real array
	!
	! --- UPDATE 
	! --- V5.0-V6.1     050915  (DGS): resolve times to the second, and
	!                                  include begin/end times for CALMET
	!                                  Version 6 (remains compatible with
	!                                  older version of CALMET)
	! --- V5.0-V5.0     990228a (DGS): add IEOF to recover from end-of-file
	!
	! --- INPUTS:
	!            IOMET - integer     - Fortran unit number of input file
	!            MTVER - integer     - Time-mark flag
	!                                  0: end-time (no seconds)
	!                                  1: begin-time / end-time
	!      XBUF(nx,ny) - real array  - Buffer to hold input data
	!      MXNX,MXNY   - integers    - Dimensions of data array
	!            NX,NY - integers    - Actual size of grid to read
	!
	! --- OUTPUT:
	!     X(mxnx,mxny) - real array  - Input data array (padded if nec.)
	!           CLABEL - character*8 - Variable name
	!          NDATHRB - integer     - Beginning date and time (YYYYJJJHH)
	!            NSECB - integer     - Beginning seconds (SSSS)
	!          NDATHRE - integer     - Ending date and time (YYYYJJJHH)
	!            NSECE - integer     - Ending seconds (SSSS)
	!             IEOF - integer     - End-of-File status
	!                                  0 = pointer within file
	!                                  1 = EOF reached on read
	!
	! --- RDR2D called by:  MET1, RDMET
	! --- RDR2D calls:      none
	!----------------------------------------------------------------------
	real x(mxnx,mxny),xbuf(nx,ny)
	character*8 clabel
	! --- Set EOF
	ieof=0
	!
	if(nx.eq.mxnx.and.ny.eq.mxny)then
		!
		! ---    entire array is being used -- read full grid
		if(mtver.EQ.1) then
			read(iomet,end=999)clabel,ndathrb,nsecb,ndathre,nsece,x
		elseif(mtver.EQ.0) then
			read(iomet,end=999)clabel,ndathre,x
			nsece=0
			ndathrb=0
			nsecb=0
		endif
	else
		!
		! ---    only a portion of grid being used -- read and
		! ---    transfer from buffer
		if(mtver.EQ.1) then
			read(iomet,end=999)clabel,ndathrb,nsecb,ndathre,nsece,xbuf
		elseif(mtver.EQ.0) then
			read(iomet,end=999)clabel,ndathre,xbuf
			nsece=0
			ndathrb=0
			nsecb=0
		endif
		!
		do i=1,nx! add by @creaqi do label 10
			do j=1,ny! add by @creaqi do label 10
				x(i,j)=xbuf(i,j)
			enddo !add by @creaqi 10
		enddo !add by @creaqi 10
		10       continue
	endif
	!
	return
	999   ieof=1
	return
end
!----------------------------------------------------------------------
subroutine initar(xvalue,nwords,xarr)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 950610            INITAR
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Initialize an array with a constant value
	!
	! --- INPUTS:
	!
	!          XVALUE - real       - Value to used for initialization
	!          NWORDS - integer    - Lengths of the data array (words)
	!
	! --- OUTPUT:
	!
	!    XARR(nwords) - real array - Array initialized
	!
	! --- INITAR called by:  RDISC, RDPLM
	! --- INITAR calls:      none
	!
	!----------------------------------------------------------------------
	!
	real xarr(nwords)
	!
	x=xvalue
	do i=1,nwords! add by @creaqi do label 10
		xarr(i)=x
	enddo !add by @creaqi 10
	10    continue
	!
	return
end
!----------------------------------------------------------------------
subroutine initai(ivalue,nwords,iarr)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 950610            INITAI
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Initialize an integer array with a constant value
	!
	! --- INPUTS:
	!
	!          IVALUE - integer    - Value to used for initialization
	!          NWORDS - integer    - Lengths of the data array (words)
	!
	! --- OUTPUT:
	!
	!    IARR(nwords) - int. array - Array initialized
	!
	! --- INITAI called by:  RDISC, RDPLM
	! --- INITAI calls:      none
	!
	!----------------------------------------------------------------------
	!
	integer iarr(nwords)
	!
	ival=ivalue
	do i=1,nwords! add by @creaqi do label 10
		iarr(i)=ival
	enddo !add by @creaqi 10
	10    continue
	!
	return
end
!----------------------------------------------------------------------
subroutine initr2d(xvalue,mx1,mx2,n1,n2,xarr)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 950610           INITR2D
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Initialize all or a portion of a REAL 2-D array
	!               with a constant value
	!
	! --- INPUTS:
	!
	!          XVALUE - real       - Value to used for initialization
	!          MX1    - integer    - Maximum first dimension of array
	!          MX2    - integer    - Maximum second dimension of array
	!          N1     - integer    - Number of 1st dim. elements to
	!                                initialize
	!          N2     - integer    - Number of 2nd dim. elements to
	!                                initialize
	!
	! --- OUTPUT:
	!
	!   XARR(mx1,mx2) - real array - Array initialized with XVALUE -
	!                                Elements (1-N1, 1-N2) are initialized
	!
	! --- INITR2D called by:  COMP, MET2, MET3, RDISC, RDPLM
	! --- INITR2D calls:      none
	!
	!----------------------------------------------------------------------
	!
	real xarr(mx1,mx2)
	!
	x=xvalue
	do i=1,n1! add by @creaqi do label 10
		do j=1,n2! add by @creaqi do label 10
			xarr(i,j)=x
		enddo !add by @creaqi 10
	enddo !add by @creaqi 10
	10    continue
	!
	return
end
!----------------------------------------------------------------------
subroutine initi2d(ivalue,mx1,mx2,n1,n2,iarr)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 950610           INITI2D
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Initialize an integer array with a constant value
	!
	! --- INPUTS:
	!
	!          IVALUE - integer    - Value to used for initialization
	!          MX1    - integer    - Maximum first dimension of array
	!          MX2    - integer    - Maximum second dimension of array
	!          N1     - integer    - Number of 1st dim. elements to
	!                                initialize
	!          N2     - integer    - Number of 2nd dim. elements to
	!                                initialize
	!
	! --- OUTPUT:
	!
	!   IARR(mx1,mx2) - int. array - Array initialized with IVALUE -
	!                                Elements (1-N1, 1-N2) are initialized
	!
	! --- INITI2D called by:  COMP, MET2, MET3, RDISC, RDPLM
	! --- INITI2D calls:      none
	!
	!----------------------------------------------------------------------
	!
	integer iarr(mx1,mx2)
	!
	ival=ivalue
	do i=1,n1! add by @creaqi do label 10
		do j=1,n2! add by @creaqi do label 10
			iarr(i,j)=ival
		enddo !add by @creaqi 10
	enddo !add by @creaqi 10
	10    continue
	!
	return
end
!----------------------------------------------------------------------
subroutine track(ip,idatm,isec,hlid0,jdstab)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 050915             TRACK
	!                D. Strimaitis, SRC
	!
	! --- PURPOSE:  Write selected puff/slug data to a file to track
	!               evolution in time
	!
	! --- UPDATE
	! --- V5.3-V6.1     050915  (DGS): report end-time including seconds
	! --- V5.2-V5.3     991222  (DGS): move write format to format statements
	! --- V5.0-V5.2     991104  (DGS): report date-time with YYYY format
	! --- V5.0-V5.0     980731  (DGS): add jdstab to output
	! --- V4.0-V5.0     971107  (DGS): replace ZFINAL array with ZPB,ZPE
	!
	! --- INPUTS:
	!              IP - integer    - Puff/slug index
	!           IDATM - integer    - Ending date/time (yyyyjjjhh)
	!            ISEC - integer    - Ending seconds (ssss)
	!           HLID0 - real       - Mixing height (m) before change to
	!                                reflecting height, zi
	!          JDSTAB - integer    - Stability class used for puff
	!
	! --- OUTPUT:     (directed to file)
	!
	!
	! --- TRACK called by:  COMP
	! --- TRACK calls:      none
	!
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	include 'grid.puf'
	include 'puff.puf'
	include 'slug.puf'
	!
	if(ipufcd(ip).EQ.99) return
	! ---   (Puff/slug off computational grid)
	if(ipufcd(ip).GT.10) then
		! ---    Slug
		slen=dgrid*SQRT((xpe(ip)-xpb(ip))**2+(ype(ip)-ypb(ip))**2)
		write(io30,101)&
		idatm,isec,ip,ipufcd(ip),zpe(ip),xpe(ip),ype(ip),&
		sigye(ip),sigze(ip),qm(1,ip),qu(1,ip),&
		zimax(ip),ziold(ip),hlid0,jdstab,slen
	else
		! ---    Puff
		write(io30,102)&
		idatm,isec,ip,ipufcd(ip),zpb(ip),xpb(ip),ypb(ip),&
		sigyb(ip),sigzb(ip),qm(1,ip),qu(1,ip),&
		zimax(ip),ziold(ip),hlid0,jdstab
	endif
	!
	return
	101   format(i9,i5,i8,i3,f7.1,2f10.4,2f9.1,1p2e12.4,0p3f8.1,i4,3x,f8.1)
	102   format(i9,i5,i8,i3,f7.1,2f10.4,2f9.1,1p2e12.4,0p3f8.1,i4)
end
!----------------------------------------------------------------------
subroutine wet(ixs,iys,md,nspec,tsamp,jp,tempk,ldbhr,&
	xlam,fracwet)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 140521               WET
	!                J. Scire
	!
	! --- PURPOSE:  Compute the scavenging ratios as a function of species
	!               for wet removal
	! --- UPDATE:
	!
	! --- V6.302-6.42_x1.1 140521    : Replace calculated scavenging rate
	!                                  if MCHEM=6,7 and MAQCHEM=1, if zero
	!                                  with default because local 3D cloud
	!                                  water may be zero during precip
	! --- V6.3-V6.302   100917  (DGS): Use calculated scavenging coeff. if
	!                                  MCHEM=6,7 and MAQCHEM=1, based on
	!                                  AER work for API (add logic for
	!                                  species not in AQRADM)
	! --- V6.22-V6.3    100212  (DGS): Add nested CALMET grids
	! --- V6.101-V6.22  070921  (DGS): Initialize variables for debug
	!                                  output that may not be computed
	! --- V5.7-V6.101   051020  (DGS): Multiply XLAM by the precipitation
	!                                  rate to produce the scavenging ratio
	! --- V5.5-V5.7     030402  (FRR): Add 2D met arrays (i2dmet)
	!
	! --- INPUTS:
	!              IXS - integer - X index of the closest met. grid point
	!                              to the puff/slug center
	!              IYS - integer - Y index of the closest met. grid point
	!                              to the puff/slug center
	!               MD - integer - MET Grid Domain index
	!            NSPEC - integer - Number of modeled species
	!            TSAMP - real    - Sampling step (s)
	!               JP - integer - Puff number of current puff
	!            TEMPK - real    - Air temperature (deg. K)
	!            LDBHR - logical - Flag activating debug write statements
	!                              (T = write, F = do not write)
	!
	!     Common Block /METHD/ variables:
	!        I2DMET, NEARS(mxnx,mxny,mxmetdom)
	!     Common Block /METHR/ variables:
	!        RMM(mxnx,mxny,mxmetdom), IPCODE(mxss,mxmetdom),
	!        IPCODE2D(mxnx,mxny,mxmetdom)
	!     Common Block /PUFF/ variables:
	!        QM(mxspec,mxpuff), QU(mxspec,mxpuff)
	!     Common Block /WETDAT/ variables:
	!        WA(2,mxspec)
	!     Parameters:
	!        MXNX, MXNY, MXNZ, MXSS, MXUS, MXPS, MXNZP1, MXPUFF,
	!        MXSPEC, MXMETDOM, IO6
	!
	! --- OUTPUT:
	!      XLAM(nspec) - real    - Scavenging ratio (1/s) for each species
	!   FRACWET(nspec) - real    - Fraction of puff mass remaining after
	!                              consideration of wet removal effects
	!     Common Block /PUFF/ variables:
	!        QM(mxspec,mxpuff), QU(mxspec,mxpuff)
	!
	! --- WET called by:  COMP
	! --- WET calls:      none
	!----------------------------------------------------------------------
	!
	! --- Include parameter statements
	include 'params.puf'
	!
	! --- Include common blocks
	include 'aqueous.puf'
	include 'flags.puf'
	include 'methd.puf'
	include 'methr.puf'
	include 'puff.puf'
	include 'wetdat.puf'
	!
	real fracwet(nspec),xlam(nspec)
	logical ldbhr, l_scav6
	logical :: lgoto_101_0 = .false. ! add by @creaqi wet modify_goto_in_if
	!
	! --- Missing value indicators for integer, real variables
	data imiss/9999/
	! ----------------------
	! --- Set local controls
	! ----------------------
	! --- Enable aqueous chem change to the code to be reverted to
	! --- original API formulation (for testing).
	! ---     Selecting l_scav6=.TRUE. restricts the use of the SCAV
	! ---     arrays to just the RIVAD species 1-6 since the aqueous
	! ---     phase module does not provide scavenging coefficients
	! ---     for other modeled species.
	! ---     Selecting l_scav6=.FALSE. retains the original code and
	! ---     unassigned scavenging coefficients may result.
	data l_scav6/.TRUE./
	! --- Initialize variables written as debug output that may not
	! --- be assigned
	ista=0
	ilq=0
	iprecip=0
	! --- If no precipitation, no wet removal
	if(rmm(ixs,iys,md).eq.0.)then
		do i=1,nspec
			xlam(i)=0.0
			fracwet(i)=1.0
		enddo
		lgoto_101_0 = .true. !         go to 101
	endif
	if(.not.lgoto_101_0) then ! start second start of goto 101 modify_goto_related_if
		!
		! --- Determine if precipitation is liquid (ILQ=1) or frozen (ILQ=2)
		! frr (09/01)- Use 2D arrays if available in CALMET
		if(i2dmet.EQ.1) then
			iprecip=ipcode2d(ixs,iys,md)
		elseif(i2dmet.EQ.0) then
			ista=nears(ixs,iys,md)
			iprecip=ipcode(ista,md)
		else
			write(*,*)'Subr. WET:  Invalid I2DMET = ',i2dmet
			stop
		endif
		if(iprecip.eq.imiss.or.iprecip.eq.0)then
			!
			! ---    Precip. code is unavailable due to missing data or no precip.
			! ---    at time of obs. at surface station, therefore, determine
			! ---    precip. type based on the air temperature
			!
			! ---    Assume liquid precip. if temp. > freezing, otherwise,
			! ---    assume frozen precip.
			if(tempk.gt.273.15)then
				ilq=1
			else
				ilq=2
			endif
		else if(iprecip.le.18)then
			!
			! ---    Liquid precipitation type
			ilq=1
		else
			!
			! ---    Frozen precipitation type
			ilq=2
		endif
		!
		! --- Determine the amount of pollutant mass remaining after wet
		! --- removal
		do i=1,nspec
			xlam(i)=wa(ilq,i)*rmm(ixs,iys,md)
			if(ilq.EQ.1) then
				if((mchem.EQ.6 .OR. mchem.EQ.7) .AND. maqchem.EQ.1) then
					if(l_scav6) then
						! ---             Restrict aqueous-phase scavenging to species 1-6 
						if(i.LE.6) then
							! --- 6.42_x1.1 140521
							! ---             Scavenging may be zero due to either no precip or no
							! ---             cloud water overlap with puff mass, so replace zero
							! ---             with default XLAM(i) which is non-zero when precip>0
							if (qu(i,jp)+qm(i,jp) .GT. 0.) then
								tmpu=scavu(i)
								if(tmpu.EQ.0.0) tmpu=xlam(i)
								tmpm=scavm(i)
								if(tmpm.EQ.0.0) tmpm=xlam(i)
								xlam(i)=(qu(i,jp)*tmpu+qm(i,jp)*tmpm)/&
								(qu(i,jp)+qm(i,jp))
							endif
						endif
					else
						if (qu(i,jp)+qm(i,jp) .GT. 0.) then
							xlam(i)=(qu(i,jp)*scavu(i)+qm(i,jp)*scavm(i))/&
							(qu(i,jp)+qm(i,jp))
						endif
					endif
				endif
			endif
			fracwet(i)=exp(-xlam(i)*tsamp)
			qu(i,jp)=fracwet(i)*qu(i,jp)
			qm(i,jp)=fracwet(i)*qm(i,jp)
		enddo
		!
	endif ! add by @creaqi max lgoto_101_0 end of goto 101 modify_goto_related_if
	101   continue
	!*****
	if(ldbhr)then
		write(io6,111)ixs,iys,md,ista,iprecip,tempk,ilq,&
		rmm(ixs,iys,md),(xlam(n),n=1,nspec)
		write(io6,112)(fracwet(n),n=1,nspec)
		111      format(15x,'WET REMOVAL -- IXS = ',i3,2x,'IYS = ',i3,2x,&
		'MG = ',i3,2x,'ISTA = ',i4/17x,'Precip. code = ',i4,3x,&
		'TEMPK = ',f6.2,2x,'ILQ = ',i2,2x,'RMM (mm/hr) = ',&
		f7.2/17x,'Scav. Ratio   = ',2x,5(1pe11.3,1x))
		112      format(17x,'Fraction left = ',2x,5(f11.3,1x))
	endif
	!*****
	return
end
!----------------------------------------------------------------------
subroutine wrout1(title)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 150918            WROUT1
	!                J. Scire
	!
	! --- PURPOSE:  Write the header records to the concentration,
	!               dry flux, wet flux, and visibility output files
	!                Concentration file: CONC.DAT (IO8)
	!                Dry flux file:      DFLX.DAT (IO9)
	!                Wet flux file:      WFLX.DAT (IO10)
	!                Visibility files:   VISB.DAT (IO11)
	!                                    TK2D.DAT (IO13)
	!                                    RHO2D.DAT (IO14)
	!               and to other output files
	!                FOG data file:      FOG.DAT  (IO12)
	!                NUMRISE data file:  RISE.DAT (IO38)
	!                PUFF-TRACKING file: PFTRAK.DAT (IOTRK)
	!
	! --- UPDATE
	! --- v7.2.1 - v7.3.0        150918  (CDA):
	!                                : Add variable spary source (SPEMARB.DAT)
	! --- TNG-7.1.0 - V7.2.0     141230  (DGS):
	!                                : Datasets TNG-3.0 renamed to 7.0
	! --- TNG-7.0.0 - TNG-7.1.0  141230  (DGS):
	!                                : Add variable road source option
	!                                  (RDEMARB.DAT)
	! --- V6.42_x1.1 - TNG-7.0.0  140913  (DGS)
	!                                : CALPUFF Dataset TNG-3.0
	!                                : Include new source type structure
	!                                : IOVERS option removed
	!                                : Add flares and roads
	!                                : Add discrete receptor pole ht
	!                                : Add discrete receptor group names
	!                                : RISE.DAT Dataset TNG-3.0 with
	!                                    structure for additional
	!                                    source types
	!                                : PFTRAK.DAT Dataset TNG-3.0 with
	!                                    additional time and space info
	!                                    and the structure for additional
	!                                    source types
	! --- V6.41-V6.42_x1.1  140521   : PFTRAK.DAT Dataset v1.1, for
	!                                  snapshot times on substeps;
	!                                  Expect up to 12 characters in ver
	!                                  and level when constructing COMMENT0
	! --- V6.302-V6.41  110301  (DGS): Dataset v2.2, with IOVERS toggle
	! --- V6.3-V6.302   100917  (DGS): Character time zone ABTZ is in common
	! --- V6.267-V6.3   100212  (DGS): Add nested CALMET grids
	!                                  Initial implementation writes just
	!                                  surface station information for MET
	!                                  grid domain 1 (outermost)
	! --- V6.26-V6.267  090710  (DGS): Add PUFF-TRACKING output file
	!                                  (PFTRAK.DAT)
	! --- V6.1-V6.26    080430  (DGS): Add numerical rise output file
	!                                  (RISE.DAT)
	!                                  Create comment0 instead of comment1
	!                                  for first comment record
	!                   080430  (DGS): Do not close scratch file, just
	!                                  rewind for later use
	! --- V5.725-V6.1   050915  (DGS): IAVG processed in READCF, so do not
	!                                  set here
	! --- V5.72-V5.725  050128  (DGS): Add TK2D.DAT and RHO2D.DAT files
	!                   050128  (DGS): Add call to COORDSVER and write info
	!                                  to output file headers
	! --- V5.7-V5.72    031017  (DGS): Revise format for source contribution
	!                                  output option; use full begin/end
	!                                  times as hour and second
	!                                  (Dataset Version 2.1)
	! --- V5.5-V5.7     030402  (DGS): Add list file unit to JULDAY call
	!                   030402  (DGS): Revised header format with control
	!                                  file images, and /MAP/ variables
	!                   010901  (FRR): Add i2dmet for VISB.DAT header
	! --- V5.0-V5.4     000602_2(DGS): Add LCOMPRS for FOG.DAT header
	! --- V5.0-V5.4     000602  (DGS): IGRDVL changed to NVL2
	!                   000602  (DGS): add FOG-model file (FOG.DAT)
	! --- V4.0-V5.0     971107  (DGS): uses species-groups
	!                   971107  (DGS): add number of lines from LNEMARB.DAT
	!
	! --- INPUTS:
	!             TITLE(3) - Char.*80    - Run title (3 lines of
	!                                      80 char. each)
	!       Common block /AR1/ variables:
	!             NAR1,CNAMAR1(mxarea)
	!       Common block /AR2/ variables:
	!             NAR2,CID3(mxarea)
	!       Common block /CTSGDAT/ variables:
	!             NHILL, NCTREC, XRCT(mxrect), YRCT(mxrect),
	!             ELRECT(mxrect), IHILL(mxrect)
	!       Common block /DATEHR/ variables:
	!             XBTZ, ABTZ
	!       Common block /FL2/ variables:
	!             NFL2,CID6(mxfl2)
	!       Common block /FOG/ variables:
	!             (all)
	!       Common block /GEN/ variables:
	!             IBYR, IBMO, IBDY, IBHR, IRLG, IBCOMP, JBCOMP, IECOMP,
	!             JECOMP, IBSAMP, JBSAMP, IESAMP, JESAMP, MESHDN,
	!             NGRUP, CGRUP(mxgrup)
	!       Common block /GRID/ variables:
	!             NX, NY, NZ, DGRID, XORIG, YORIG
	!       Common block /LN1/ variables:
	!             NLINES,CNAMLN1(mxlines)
	!       Common block /LN2/ variables:
	!             NLN2,CID5(mxlines)
	!       Common block /MAP/ variables:
	!             iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
	!             pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
	!       Common block /METHD/ variables:
	!             NSSTA, NEARS(mxnx,mxny,mxmetdom), XSSTA(mxss,mxmetdom),
	!             YSSTA(mxss,mxmetdom), I2DMET
	!       Common block /NONGRD/ variables:
	!             NREC,XNG(mxrec),YNG(mxrec),ZNG(mxrec),ELEVNG(mxrec),
	!             NRGRP,IRGRP(mxrec),RGRPNAM(mxrgrp)
	!       Common block /OUTPT/ variables:
	!             IOUTU, IOVERS, ICON, IDRY, IWET, IVIS, IT2D, IRHO,
	!             IOUTOP(7,mxspec), LCOMPRS, IPRU, IFOG, INRISE,
	!             IPFTRAK, MSOURCE
	!       Common block /PT1/ variables:
	!             NPT1,CNAMPT1(mxpt1)
	!       Common block /PT2/ variables:
	!             NPT2,CID2(mxpt2)
	!       Common block /QA/ variables:
	!             VER, LEVEL, NCOMMOUT
	!       Common block /VOL1/ variables:
	!             NVL1,CNAMVL1(mxvol)
	!       Common block /VOL2/ variables:
	!             NVL2,CID4(mxvol)
	!       Common block /WRKSPC/ variables:
	!             TMP3(mxrec), TMP4(mxrec), TMP5(mxrect), TMP6(mxrect)
	!
	!     |MROAD1| variables:
	!             NRD1, SRCNAMRD1(nrd1)
	!     |MROAD2| variables:
	!             NRD2, CID7(nrd2)
	!     |MSPRAY2| variables:
	!             NSP2, CID8(nsp2)
	!
	!       Parameters:
	!             MXNZ, MXNZP1, MXGRUP, MXNZMP1,  MXPT1, MXPT2, MXREC,
	!             MXNX, MXNY, MXSS, MXHILL, MXRECT, MXMETDOM,
	!             MXPT1,MXPT2,MXAREA,MXLINES,MXVOL,
	!             IO6, IO8, IO9, IO10, IO11, IO12, IO13, IO14, IO38
	!             MMODEL, IOX, IOTRK
	!
	! --- OUTPUT:  none
	!
	! --- WROUT1 called by:  SETUP
	! --- WROUT1 calls:      JULDAY, INCR, COORDSVER,
	!                        XTRACT, XTRACTI, WRDAT, WRINT
	!
	!----------------------------------------------------------------------
	! --- Modules
	use mroad1
	use mroad2
	use mspray2
	!
	! --- Include parameters
	include 'params.puf'
	!
	! --- Include common blocks
	include 'ar1.puf'
	include 'ar2.puf'
	include 'ctsgdat.puf'
	include 'datehr.puf'
	include 'fl2.puf'
	include 'fog.puf'
	include 'gen.puf'
	include 'grid.puf'
	include 'ln1.puf'
	include 'ln2.puf'
	include 'map.puf'
	include 'methd.puf'
	include 'nongrd.puf'
	include 'outpt.puf'
	include 'pt1.puf'
	include 'pt2.puf'
	include 'qa.puf'
	include 'vol1.puf'
	include 'vol2.puf'
	include 'wrkspc.puf'
	!
	real xkmsta(mxss),ykmsta(mxss)
	integer itmp(mxnx,mxny)
	character*80 title(3)
	character*15 csout(mxgrup),cdfout(mxgrup),cwfout(mxgrup)
	character*15 cvsout(1),ct2out,crhoout
	character*15 cname
	character*16 aspunitc(3), aspunitf(3)
	character*16 avisunit, at2dunit, arhounit
	character*16 acunit(mxgrup),afunit(mxgrup)
	!
	character*4 xyunit
	character*16 conset,dryset,wetset,visset,dataver
	character*16 t2dset,rhoset
	character*16 risset,risever,trkset,trkver
	character*33 blank33
	character*64 datamod,risemod,trkmod
	character*132 comment0,comment1,blank
	!
	character*50 verdoc
	! --- Local variables for managing source-names for each type
	integer                        :: maxnsrc, nsrctype
	integer,           allocatable :: nsrcbytype(:)                   !nsrctype
	character(len=16), allocatable :: cnamsrc(:,:)                    !maxnsrc,nsrctype
	data ione/1/
	data xyunit/'KM  '/
	! --- Assign concentration units
	data aspunitc/'g/m3            ',&
	'odour_units     ',&
	'Bq/m3           '/
	! --- Assign deposition units
	data aspunitf/'g/m2/s          ',&
	'void            ',&
	'Bq/m2/s         '/
	! --- Assign visibility-related units
	data avisunit/'percent         '/
	data at2dunit/'kelvin          '/
	data arhounit/'kg/m3           '/
	data blank33/'                                 '/
	! --- Configure output documentation
	data conset/'CONC.DAT        '/
	data dataver/'7.0            '/
	data dryset/'DFLX.DAT        '/, wetset/'WFLX.DAT        '/
	data visset/'VISB.DAT        '/, t2dset/'TK2D.DAT        '/
	data rhoset/'RHO2D.DAT       '/
	data datamod/'File structure with embedded control file'/
	data risset/'RISE.DAT        '/, risever/'7.0             '/
	data trkset/'PFTRAK.DAT      '/, trkver/'7.0             '/
	data risemod/'File structure with embedded control file'/
	data trkmod/'File structure with embedded control file'/
	data comment0/'Produced by CALPUFF Version: '/
	! --- Set blank (132 characters)
	blank(1:33)=blank33
	blank(34:66)=blank33
	blank(67:99)=blank33
	blank(100:132)=blank33
	! --- Construct the version-level comment string
	comment0=blank
	j=29
	comment0(1:j)='Produced by CALPUFF Version: '
	j=j+1
	do i=1,12
		if(ver(i:i).NE.' ') then
			comment0(j:j)=ver(i:i)
			j=j+1
		endif
	enddo
	j=j+1
	comment0(j:j+7)=' Level: '
	j=j+8
	do i=1,12
		if(level(i:i).NE.' ') then
			comment0(j:j)=level(i:i)
			j=j+1
		endif
	enddo
	! --- Obtain COORDS version information
	call COORDSVER(io6,verdoc)
	! --- Set internal variables for output
	call JULDAY(io6,ibyr,ibmo,ibdy,ibjul)
	xorigkm=0.001*xorig
	yorigkm=0.001*yorig
	dgridkm=0.001*dgrid
	dxkm=dgridkm
	dykm=dgridkm
	! --- NSPOUT, NDFOUT, and NWFOUT are the number of species stored in the
	! --- concentration, dry flux, and wet flux output files, respectively
	! --- NVSOUT is the number of visibility-related parameters in the
	! --- visibility output file
	! --- NT2OUT is number of parameters (1) in 2D Temperature output file
	! --- NRHOOUT is number of parameters (1) in 2D Density output file
	nspout=0
	ndfout=0
	nwfout=0
	nvsout=1
	nt2out=1
	nrhoout=1
	do i=1,ngrup! add by @creaqi do label 10
		if(ioutop(2,i).eq.1)nspout=nspout+1
		if(ioutop(4,i).eq.1)ndfout=ndfout+1
		if(ioutop(6,i).eq.1)nwfout=nwfout+1
	enddo !add by @creaqi 10
	10    continue
	! --- Skip out if no output files are generated
	isum=icon+idry+iwet
	if(.not. (isum.eq.0)) then ! add by @creaqi goto 100 in fun modify_goto_pure
		! --- Set up source names for output
		! ----------------------------------
		! --- Set number of source-types (currently 13)
		nsrctype=15
		! --- Drop number of sources into local array
		ALLOCATE(nsrcbytype(nsrctype))
		nsrcbytype(1)=npt1
		nsrcbytype(2)=npt2
		nsrcbytype(3)=nar1
		nsrcbytype(4)=nar2
		nsrcbytype(5)=nlines
		nsrcbytype(6)=nln2
		nsrcbytype(7)=nvl1
		nsrcbytype(8)=nvl2
		nsrcbytype(9)=0
		nsrcbytype(10)=0
		nsrcbytype(11)=nfl2
		nsrcbytype(12)=nrd1
		nsrcbytype(13)=nrd2
		nsrcbytype(14)=0
		nsrcbytype(15)=nsp2
		! --- Max number of sources of any type
		maxnsrc=MAXVAL(nsrcbytype)
		! --- Source names into local array
		ALLOCATE(cnamsrc(maxnsrc,nsrctype))
		cnamsrc='                '
		do n=1,npt1
			cnamsrc(n,1)=cnampt1(n)
		enddo
		do n=1,npt2
			cnamsrc(n,2)=cid2(n)
		enddo
		do n=1,nar1
			cnamsrc(n,3)=cnamar1(n)
		enddo
		do n=1,nar2
			cnamsrc(n,4)=cid3(n)
		enddo
		do n=1,nlines
			cnamsrc(n,5)=cnamln1(n)
		enddo
		do n=1,nln2
			cnamsrc(n,6)=cid5(n)
		enddo
		do n=1,nvl1
			cnamsrc(n,7)=cnamvl1(n)
		enddo
		do n=1,nvl2
			cnamsrc(n,8)=cid4(n)
		enddo
		! --- Skip boundary sources (9)
		! --- No control-file flares (10)
		do n=1,nfl2
			cnamsrc(n,11)=cid6(n)
		enddo
		do n=1,nrd1
			cnamsrc(n,12)=srcnamrd1(n)
		enddo
		do n=1,nrd2
			cnamsrc(n,13)=cid7(n)
		enddo
		do n=1,nsp2
			cnamsrc(n,15)=cid8(n)
		enddo
		! -----------------------------------
		! --- Add dataset and comment records
		! -----------------------------------
		!
		! --- Record #1 - File Declaration -- 24 words
		if(ICON.eq.1)write(io8) conset,dataver,datamod
		if(IDRY.eq.1)write(io9) dryset,dataver,datamod
		if(IWET.eq.1)write(io10) wetset,dataver,datamod
		if(IVIS.eq.1)write(io11) visset,dataver,datamod
		if(IT2D.eq.1)write(io13) t2dset,dataver,datamod
		if(IRHO.eq.1)write(io14) rhoset,dataver,datamod
		!
		! --- Record #2 - Number of comment lines -- 1 word
		ncom=ncommout+2
		if(ICON.eq.1)write(io8) ncom
		if(IDRY.eq.1)write(io9) ncom
		if(IWET.eq.1)write(io10) ncom
		if(IVIS.eq.1)write(io11) ncom
		if(IT2D.eq.1)write(io13) ncom
		if(IRHO.eq.1)write(io14) ncom
		!
		! --- Record #3 to NCOM+2 (Comment record section) -- 33 words each
		if(ICON.eq.1)write(io8) comment0
		if(IDRY.eq.1)write(io9) comment0
		if(IWET.eq.1)write(io10) comment0
		if(IVIS.eq.1)write(io11) comment0
		if(IT2D.eq.1)write(io13) comment0
		if(IRHO.eq.1)write(io14) comment0
		! --- Report COORDS version
		comment1=blank
		comment1(1:36)='Internal Coordinate Transformations '
		comment1(37:86)=verdoc
		if(ICON.eq.1)write(io8) comment1
		if(IDRY.eq.1)write(io9) comment1
		if(IWET.eq.1)write(io10) comment1
		if(IVIS.eq.1)write(io11) comment1
		if(IT2D.eq.1)write(io13) comment1
		if(IRHO.eq.1)write(io14) comment1
		! --- Go to beginning of the scratch file with the control file image
		REWIND(iox)
		! --- Loop over records
		do i=1,ncommout
			comment1=blank
			read(iox,'(a132)') comment1
			if(ICON.eq.1)write(io8) comment1
			if(IDRY.eq.1)write(io9) comment1
			if(IWET.eq.1)write(io10) comment1
			if(IVIS.eq.1)write(io11) comment1
			if(IT2D.eq.1)write(io13) comment1
			if(IRHO.eq.1)write(io14) comment1
		enddo
		!
		! --- HEADER RECORD #NCOM+3,3a -- General run parameters
			if(ICON.eq.1)write(io8)mmodel,ver,level,ibyr,ibjul,ibhr,ibsec,&
		abtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,&
		nssta(1),ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,&
		jesamp,meshdn,nsrctype,msource,nrec,nrgrp,&
		nctrec,lsamp,nspout,lcomprs,i2dmet,&
		iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,&
		pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
		if(ICON.eq.1)write(io8) (nsrcbytype(n),n=1,nsrctype)
			if(IDRY.eq.1)write(io9)mmodel,ver,level,ibyr,ibjul,ibhr,ibsec,&
		abtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,&
		nssta(1),ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,&
		jesamp,meshdn,nsrctype,msource,nrec,nrgrp,&
		nctrec,lsamp,ndfout,lcomprs,i2dmet,&
		iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,&
		pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
		if(IDRY.eq.1)write(io9) (nsrcbytype(n),n=1,nsrctype)
			if(IWET.eq.1)write(io10)mmodel,ver,level,ibyr,ibjul,ibhr,ibsec,&
		abtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,&
		nssta(1),ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,&
		jesamp,meshdn,nsrctype,msource,nrec,nrgrp,&
		nctrec,lsamp,nwfout,lcomprs,i2dmet,&
		iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,&
		pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
		if(IWET.eq.1)write(io10) (nsrcbytype(n),n=1,nsrctype)
			if(IVIS.eq.1)write(io11)mmodel,ver,level,ibyr,ibjul,ibhr,ibsec,&
		abtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,&
		nssta(1),ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,&
		jesamp,meshdn,nsrctype,msource,nrec,nrgrp,&
		nctrec,lsamp,nvsout,lcomprs,i2dmet,&
		iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,&
		pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
		if(IVIS.eq.1)write(io11) (nsrcbytype(n),n=1,nsrctype)
			if(IT2D.eq.1)write(io13)mmodel,ver,level,ibyr,ibjul,ibhr,ibsec,&
		abtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,&
		nssta(1),ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,&
		jesamp,meshdn,nsrctype,msource,nrec,nrgrp,&
		nctrec,lsamp,nt2out,lcomprs,i2dmet,&
		iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,&
		pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
		if(IT2D.eq.1)write(io13) (nsrcbytype(n),n=1,nsrctype)
			if(IRHO.eq.1)write(io14)mmodel,ver,level,ibyr,ibjul,ibhr,ibsec,&
		abtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,&
		nssta(1),ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,&
		jesamp,meshdn,nsrctype,msource,nrec,nrgrp,&
		nctrec,lsamp,nrhoout,lcomprs,i2dmet,&
		iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,&
		pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
		if(IRHO.eq.1)write(io14) (nsrcbytype(n),n=1,nsrctype)
		!
		! --- HEADER RECORD #NCOM+4 -- Run title
		if(ICON.eq.1)write(io8)title
		if(IDRY.eq.1)write(io9)title
		if(IWET.eq.1)write(io10)title
		if(IVIS.eq.1)write(io11)title
		if(IT2D.eq.1)write(io13)title
		if(IRHO.eq.1)write(io14)title
		!
		! --- HEADER RECORD #NCOM+5 -- List of species-groups output
		nc=0
		nd=0
		nw=0
		do k=1,ngrup! add by @creaqi do label 20
			!
			! --- Set output concentration/layer labels
			if(ioutop(2,k).eq.1)then
				nc=nc+1
				write(csout(nc)(1:12),'(a12)')cgrup(k)
				csout(nc)(13:15)='  1'
			endif
			!
			! --- Set output dry flux labels
			if(ioutop(4,k).eq.1)then
				nd=nd+1
				write(cdfout(nd)(1:12),'(a12)')cgrup(k)
				cdfout(nd)(13:15)=' DF'
			endif
			!
			! --- Set output wet flux labels
			if(ioutop(6,k).eq.1)then
				nw=nw+1
				write(cwfout(nw)(1:12),'(a12)')cgrup(k)
				cwfout(nw)(13:15)=' WF'
			endif
		enddo !add by @creaqi 20
		20    continue
		!
		! --- Set output visibility labels
		nv=1
		write(cvsout(nv)(1:12),'(a12)') ' REL HUM (%)   '
		write(ct2out(1:12),'(a12)') ' TEMP 2D (K)   '
		write(crhoout(1:12),'(a12)') ' RHO 2D (K)    '
		!
		if(ICON.eq.1)write(io8)(csout(n),n=1,nspout)
		if(IDRY.eq.1)write(io9)(cdfout(n),n=1,ndfout)
		if(IWET.eq.1)write(io10)(cwfout(n),n=1,nwfout)
		if(IVIS.eq.1)write(io11)(cvsout(n),n=1,nvsout)
		if(IT2D.eq.1)write(io13)(ct2out)
		if(IRHO.eq.1)write(io14)(crhoout)
		! --- HEADER RECORD #NCOM+5a -- List of species-group units
		do k=1,mxgrup
			! ---    All output species have the same units
			acunit(k)=aspunitc(ioutu)
			afunit(k)=aspunitf(ioutu)
		enddo
		if(ICON.eq.1)write(io8)(acunit(n),n=1,nspout)
		if(IDRY.eq.1)write(io9)(afunit(n),n=1,ndfout)
		if(IWET.eq.1)write(io10)(afunit(n),n=1,nwfout)
		if(IVIS.eq.1)write(io11) avisunit
		if(IT2D.eq.1)write(io13) at2dunit
		if(IRHO.eq.1)write(io14) arhounit
		!
		! --- HEADER RECORD #NCOM+6 -- Discrete (non-gridded) receptor data
		!
		! --- Convert receptor coordinates from grid units back to map(km)
		if(nrec.gt.0)then
			do i=1,nrec! add by @creaqi do label 30
				tmp3(i)=xng(i)*dgridkm+xorigkm
				tmp4(i)=yng(i)*dgridkm+yorigkm
			enddo !add by @creaqi 30
			30          continue
			! --- Write receptor x(km),y(km),ground elevation(mMSL),z(mAGL),
			! --- and receptor-group index
			!
				if(ICON.eq.1)write(io8)(tmp3(n),n=1,nrec),&
			(tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec),&
			(zng(n4),n4=1,nrec), (irgrp(n5),n5=1,nrec)
				if(IDRY.eq.1)write(io9)(tmp3(n),n=1,nrec),&
			(tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec),&
			(zng(n4),n4=1,nrec), (irgrp(n5),n5=1,nrec)
				if(IWET.eq.1)write(io10)(tmp3(n),n=1,nrec),&
			(tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec),&
			(zng(n4),n4=1,nrec), (irgrp(n5),n5=1,nrec)
				if(IVIS.eq.1)write(io11)(tmp3(n),n=1,nrec),&
			(tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec),&
			(zng(n4),n4=1,nrec), (irgrp(n5),n5=1,nrec)
				if(IT2D.eq.1)write(io13)(tmp3(n),n=1,nrec),&
			(tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec),&
			(zng(n4),n4=1,nrec), (irgrp(n5),n5=1,nrec)
				if(IRHO.eq.1)write(io14)(tmp3(n),n=1,nrec),&
			(tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec),&
			(zng(n4),n4=1,nrec), (irgrp(n5),n5=1,nrec)
		endif
		! --- HEADER RECORD #NCOM+6a -- List of receptor-group names
		!
		if(nrec.gt.0)then
			if(ICON.eq.1)write(io8) (rgrpnam(n),n=1,nrgrp)
			if(IDRY.eq.1)write(io9) (rgrpnam(n),n=1,nrgrp)
			if(IWET.eq.1)write(io10)(rgrpnam(n),n=1,nrgrp)
			if(IVIS.eq.1)write(io11)(rgrpnam(n),n=1,nrgrp)
			if(IT2D.eq.1)write(io13)(rgrpnam(n),n=1,nrgrp)
			if(IRHO.eq.1)write(io14)(rgrpnam(n),n=1,nrgrp)
		endif
		!
		!
		! --- HEADER RECORD #NCOM+7 -- Complex terrain receptor data
		!
		! --- Convert CTSG receptor coordinates from grid units back to UTM (km)
		if(nctrec.gt.0)then
			do i=1,nctrec! add by @creaqi do label 40
				tmp5(i)=xrct(i)*dgridkm+xorigkm
				tmp6(i)=yrct(i)*dgridkm+yorigkm
			enddo !add by @creaqi 40
			40          continue
			!
				if(ICON.eq.1)write(io8)(tmp5(n),n=1,nctrec),&
			(tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),&
			(ihill(n4),n4=1,nctrec)
				if(IDRY.eq.1)write(io9)(tmp5(n),n=1,nctrec),&
			(tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),&
			(ihill(n4),n4=1,nctrec)
				if(IWET.eq.1)write(io10)(tmp5(n),n=1,nctrec),&
			(tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),&
			(ihill(n4),n4=1,nctrec)
				if(IVIS.eq.1)write(io11)(tmp5(n),n=1,nctrec),&
			(tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),&
			(ihill(n4),n4=1,nctrec)
				if(IT2D.eq.1)write(io13)(tmp5(n),n=1,nctrec),&
			(tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),&
			(ihill(n4),n4=1,nctrec)
				if(IRHO.eq.1)write(io14)(tmp5(n),n=1,nctrec),&
			(tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),&
			(ihill(n4),n4=1,nctrec)
		endif
		!
		!
		! --- HEADER RECORD #NCOM+8 (+ variable) -- Source names
		!     (not written to VISIBILITY files)
		do itype=1,nsrctype
			if(nsrcbytype(itype).GT.0) then
				if(ICON.eq.1)write(io8) itype,(cnamsrc(n,itype),&
					n=1,nsrcbytype(itype))
				if(IDRY.eq.1)write(io9) itype,(cnamsrc(n,itype),&
					n=1,nsrcbytype(itype))
				if(IWET.eq.1)write(io10) itype,(cnamsrc(n,itype),&
					n=1,nsrcbytype(itype))
			endif
		enddo
		!
		!
		! --- HEADER RECORDS
		! ---     #NCOM+8 -- Nearest Surface Station for VISIBILITY ONLY
		! ---     #NCOM+9 -- X coord (UTM) of stations for VISIBILITY ONLY
		! ---    #NCOM+10 -- Y coord (UTM) of stations for VISIBILITY ONLY
		!
		! frr (4/02) if 2D RH file, no need for nearest station info (nssta can
		!            be =0)
		!
		!            
		! ---
		!      if(IVIS.eq.1)then
		if((IVIS.eq.1 .OR. IT2D.eq.1 .OR. IRHO.eq.1)&
			.and. i2dmet.eq.0 )then
			!
			! ---    Check if active portion of the MET grid is less than the full
			! ---    dimension (active portion is extracted for output purposes)
			ifullm=0
			if(nx.eq.mxnx.and.ny.eq.mxny)then
				ifullm=1
			endif
			! ---    Nearest Station Array (rec #6)
			cname=' NEARS         '
			if(ifullm.eq.1)then
				if(IVIS.eq.1)call wrint(io11,cname,nears(1,1,1),nx,ny)
				if(IT2D.eq.1)call wrint(io13,cname,nears(1,1,1),nx,ny)
				if(IRHO.eq.1)call wrint(io14,cname,nears(1,1,1),nx,ny)
			else
				call xtracti(nears(1,1,1),mxnx,mxny,nx,ny,itmp)
				if(IVIS.eq.1)call wrint(io11,cname,itmp,nx,ny)
				if(IT2D.eq.1)call wrint(io13,cname,itmp,nx,ny)
				if(IRHO.eq.1)call wrint(io14,cname,itmp,nx,ny)
			endif
			! ---    Convert station coordinates to UTM from meters relative to
			! ---    met grid origin
			do is=1,nssta(1)
				xkmsta(is)=xorigkm+0.001*xssta(is,1)
				ykmsta(is)=yorigkm+0.001*yssta(is,1)
			enddo
			! ---    X-coord (km) of each surface station (rec #7)
			cname=' XSSTA-UTM     '
			if(IVIS.eq.1)call wrdat(io11,cname,xkmsta,nssta(1),1)
			if(IT2D.eq.1)call wrdat(io13,cname,xkmsta,nssta(1),1)
			if(IRHO.eq.1)call wrdat(io14,cname,xkmsta,nssta(1),1)
			! ---    Y-coord (km) of each surface station (rec #8)
			cname=' YSSTA-UTM     '
			if(IVIS.eq.1)call wrdat(io11,cname,ykmsta,nssta(1),1)
			if(IT2D.eq.1)call wrdat(io13,cname,ykmsta,nssta(1),1)
			if(IRHO.eq.1)call wrdat(io14,cname,ykmsta,nssta(1),1)
		endif
		!
	endif !add by @creaqi label 100 modify_goto_pure
	100   continue
	! --- Section for FOG.DAT (binary)
	! -------------------------------
	if(IFOG.GT.0) then
		if(LPMODE) then
			! ---       Plume mode: write second header record with
			! ---       downwind distances in meters from source
			write(io12) ftitle,nfrec,nfpts,ipcp,ifyr,ifdays,lpmode
			write(io12) lcomprs,xrfog
		else
			write(io12) ftitle,nfrec,nfpts,ipcp,ifyr,ifdays,lpmode
			write(io12) lcomprs
		endif
	endif
	! --- Section for RISE.DAT (ASCII)
	! --------------------------------
	if(inrise.GT.0) then
		! ---    Initial header records
		! ---    Record #1 - File Declaration
		write(io38,'(2a16,a64)') risset,risever,risemod
		! ---    Record #2 - Number of comment lines
		ncom=ncommout+2
		write(io38,*) ncom
		! ---    Record #3 to NCOM+2 (Comment record section)
		write(io38,'(a132)') comment0
		! ---    Report COORDS version
		comment1=blank
		comment1(1:36)='Internal Coordinate Transformations '
		comment1(37:86)=verdoc
		write(io38,'(a132)') comment1
		! ---    Control file image
		! ---    Go to beginning of the scratch file with the control file image
		REWIND(iox)
		! ---    Loop over control file records
		do i=1,ncommout
			comment1=blank
			read(iox,'(a132)') comment1
			write(io38,'(a132)') comment1
		enddo
		! ---    Map
		write(io38,'(a8)')pmap
		if(LUTM) then
			write(io38,'(i4,a4)')  iutmzn,utmhem
		elseif(LLCC) then
			write(io38,'(4a16)')  clat0,clon0,clat1,clat2
		elseif(LPS) then
			write(io38,'(3a16)')  clat0,clon0,clat1
		elseif(LEM.or.LLAZA.or.LTTM) then
			write(io38,'(2a16)')  clat0,clon0
		endif
		! ---    Map false Easting/Northing
		if(LLCC.or.LLAZA.or.LTTM) then
			write(io38,*) feast,fnorth
		endif
		! ---    Map DATUM and date
		write(io38,'(a8,a12)')  datum,daten
		! ---    Units
		write(io38,'(a4)')  xyunit
		! ---    Time zone
		write(io38,'(a8)')abtz
		! ---    Source information
		nqa=0
		write(io38,*) nsrctype
		do itype=1,nsrctype
			! ---       Number by type
			nqa=nqa+nsrcbytype(itype)
			write(io38,*) itype,nsrcbytype(itype)
			! ---       Names
			if(nsrcbytype(itype).GT.0) then
				write(io38,*) (cnamsrc(n,itype),n=1,nsrcbytype(itype))
			endif
		enddo
		! ---    Restrict number of sources to ONE for this output to limit
		! ---    the size of the output file
		if(nqa.NE.1) then
			write(io6,*)
			write(io6,*)
			write(io6,*)
			write(io6,*)'----------------------------------------------'
			write(io6,*)'          FATAL FLAW  !! '
			write(io6,*)'----------------------------------------------'
			write(io6,*)'Selection of RISE.DAT file output is currently'
			write(io6,*)'restricted to one source.  Number of sources'
			write(io6,*)'found = ',nqa
			write(io6,*)'----------------------------------------------'
			stop 'Halted in WROUT1 --- see list file'
		endif
	endif
	! --- Section for PFTRAK.DAT (binary)
	! -----------------------------------
	if(ipftrak.GT.0) then
		! ---    Initial header records
		! ---    Record #1 - File Declaration
		write(iotrk) trkset,trkver,trkmod
		! ---    Record #2 - Number of comment lines
		ncom=ncommout+2
		write(iotrk) ncom
		! ---    Record #3 to NCOM+2 (Comment record section)
		write(iotrk) comment0
		! ---    Report COORDS version
		comment1=blank
		comment1(1:36)='Internal Coordinate Transformations '
		comment1(37:86)=verdoc
		write(iotrk) comment1
		! ---    Control file image
		! ---    Go to beginning of the scratch file with the control file image
		REWIND(iox)
		! ---    Loop over control file records
		do i=1,ncommout
			comment1=blank
			read(iox,'(a132)') comment1
			write(iotrk) comment1
		enddo
		! ---    General run parameters (abbreviated list)
		write(iotrk)mmodel,ver,level,ibyr,ibjul,ibhr,ibsec,&
		abtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,&
		xorigkm,yorigkm,ibcomp,iecomp,jbcomp,jecomp,&
		ibsamp,jbsamp,iesamp,jesamp,meshdn,nsrctype,&
		iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,&
		pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
		! ---    Source information
		! ---    Number by type
		write(iotrk) (nsrcbytype(n),n=1,nsrctype)
		! ---    Names
		do itype=1,nsrctype
			if(nsrcbytype(itype).GT.0) then
				write(iotrk) itype,(cnamsrc(n,itype),&
				n=1,nsrcbytype(itype))
			endif
		enddo
	endif
	! --- Finished with scratch file image of documentation records
	REWIND(iox)
	return
end
!----------------------------------------------------------------------
subroutine output(istep,isrcmode,ktype,ksource)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 110301            OUTPUT
	!                J. Scire
	!
	! --- PURPOSE:  Output concentration, dry & wet deposition flux
	!               fields
	!
	!               Print concentrations every "ICFRQ" hours (if ICPRT=1)
	!               Print dry fluxes every "IDFRQ" hours (if IDPRT=1)
	!               Print wet fluxes every "IWFRQ" hours (if IWPRT=1)
	!
	!               Write specified concentrations and fluxes to disk
	!               hourly -- concentration file (IO8), dry flux file (IO9),
	!               wet flux file (IO10)
	!
	! --- UPDATE
	! --- V6.1-V6.41    110301  (DGS): Add radiation units
	! --- V5.725-V6.1   050915  (DGS): Update /DATEHR/ variables
	!                           (DGS): Add emission step to AR2,VOL2,LN2
	!                                  arrays
	! --- V5.72-V5.725  050128  (DGS): Add TK2D.DAT & RHO2D.DAT output files
	! --- V5.7-V5.72    031017  (DGS): Add ISRCMODE switch to distinguish
	!                                  between source contribution output
	!                                  and total output;
	!                                  Augment time record with both begin
	!                                  and end times;
	!                                  Add new record with source name and
	!                                  location
	! --- V5.2-V5.7     030402  (FRR): Add 2D met arrays (i2dmet)
	! --- V5.0-V5.2     991104  (DGS): YYYY format for year
	! --- V5.0-V5.0     990228d (DGS): add mass balance output
	! --- V5.0-V5.0     990228c (DGS): add mass flux output
	! --- V5.0-V5.0     981025  (DGS): full list-file output for fluxes
	! --- V4.0-V5.0     971107  (DGS): add user-units in LIST output
	!                   971107  (DGS): change from species list to
	!                                  species-groups for output
	!
	! --- INPUTS:
	!              ISTEP   - integer    - Time step number
	!           ISRCMODE   - integer    - Source contribution mode
	!                                     0: output for all sources
	!                                     1: source contribution output
	!              KTYPE   - integer    - Source type
	!            KSOURCE   - integer    - Source number
	!
	!     Common block /AR1/ variables:
	!          NAR1,CNAMAR1(mxarea),NVERT1(mxarea),
	!          XAR1GRD(mxvertp1,mxarea),YAR1GRD(mxvertp1,mxarea)
	!     Common block /AR2/ variables:
	!          NAR2,CID3(mxarea),NVERT2(mxarea),
	!          XAR2GRD(mxvertp1,mxqstep,mxarea),
	!          YAR2GRD(mxvertp1,mxqstep,mxarea)
	!     Common block /CHIFLX/ variables
	!          CHISAM(mxnxg,mxnyg,mxspec), DFSAM(mxnxg,mxnyg,mxspec),
	!          WFSAM(mxnxg,mxnyg,mxspec), CHIREC(mxrec,mxspec),
	!          DFREC(mxrec,mxspec), WFREC(mxrec,mxspec),
	!          CHICT(mxrect,mxspec)
	!     Common block /CTSGDAT/ variables
	!          NCTREC, XRCT(mxrect), YRCT(mxrect), ELRECT(mxrect),
	!          IHILL(mxrect)
	!       Common block /DATEHR/ variables
	!          NYRAB, NMOAB, NDAYAB, NJULAB, NHRAB, NSECAB,
	!          NYRE, NMOE, NDAYE, NJULE, NHRE, NSECE
	!     Common block /GEN/ variables
	!          NSPEC, CSPEC(mxgrup),
	!          NGRUP, CGRUP(mxgrup), ISPGRP(mxspec)
	!     Common block /GRID/ variables
	!          DGRID, XORIG, YORIG,
	!          NXSAM, NYSAM, LSAMP
	!     Common block /LN1/ variables:
	!          NLINES,CNAMLN1(mxlines),XL1BEGGRD(mxlines),YL1BEGGRD(mxlines)
	!          XL1ENDGRD(mxlines),YL1ENDGRD(mxlines)
	!     Common block /LN2/ variables:
	!          NLN2,CID5(mxlines),
	!          XL2BEGGRD(mxqstep,mxlines),YL2BEGGRD(mxqstep,mxlines),
	!          XL2ENDGRD(mxqstep,mxlines),YL2ENDGRD(mxqstep,mxlines)
	!     Common block /METHD/ variables
	!          NSSTA, I2DMET , NXM, NYM
	!     Common block /METHR/ variables
	!          IRHSS(mxss), IRH2D(mxnx,mxny),
	!          TEMPSS(mxss), TEMP2D(mxnx,mxny)
	!     Common block /NONGRD/ variables
	!          NREC
	!     Common block /OUTPT/ variables
	!          ICON, IDRY, IWET, ICPRT, IDPRT, IWPRT, ICFRQ, IDFRQ, IWFRQ,
	!          IVIS, IT2D, IRHO,
	!          IOUTOP(7,mxspec), LCOMPRS, IPRTU, IMFLX, IMBAL
	!     Common block /PT1/ variables:
	!          NPT1,CNAMPT1(mxpt1),,XPT1GRD(mxpt1),YPT1GRD(pt1)
	!     Common block /PT2/ variables:
	!          NPT2,CID2(mxpt2),TIEM2(8,mxpt2)
	!     Common block /VOL1/ variables:
	!          NVL1,CNAMVL1(mxvol),XVL1GRD(mxvol),YVL1GRD(mxvol)
	!     Common block /VOL2/ variables:
	!          NVL2,CID4(mxvol),
	!          XVL2GRD(mxqstep,mxvol),YVL2GRD(mxqstep,mxvol)
	!     Common block /WRKSPC/ variables
	!          TMP1(mxnx,mxny),
	!          TMP3(mxrec),TMP5(mxrect),TMP7(mxnxg,mxnyg),TMP8(mxnxg,mxnyg)
	!
	!     Parameters: MXNXG, MXNYG, MXSPEC, MXREC, MXRECT, MXNZP1, MXSS,
	!                 MXPT1,MXPT2,MXAREA,MXLINES,MXVOL,  MXNX,MXNY
	!                 IO6, IO8, IO9, IO10, IO11, IO13, IO14
	!
	! --- OUTPUT:  none
	!
	! --- OUTPUT called by: COMP
	! --- OUTPUT calls:     OUTSAM, WRDAT, COMPRS, MFLXGRP,
	!                       MFLXOUT, MBALOUT
	!----------------------------------------------------------------------
	!
	! --- Include parameter statements
	include 'params.puf'
	!
	real xtmp(mxspec),rprtu(8)
	integer istore(mxgrup)
	character*70 messag
	character*16 csrcnam
	character*15 cname
	character*15 cnameRh, cnameTK, cnameDN
	character*13 cunits,funits
	character*12 cspecsv(mxspec)
	character*1  cprtu(8)
	logical ldate
	!
	! frr(09/01)
	integer itmp(mxnx,mxny)
	include 'chiflx.puf'
	include 'ctsgdat.puf'
	include 'datehr.puf'
	include 'methd.puf'
	include 'methr.puf'
	include 'gen.puf'
	include 'grid.puf'
	include 'nongrd.puf'
	include 'outpt.puf'
	include 'wrkspc.puf'
	! --- Include source commons for source names
	include 'ar1.puf'
	include 'ar2.puf'
	include 'ln1.puf'
	include 'ln2.puf'
	include 'pt1.puf'
	include 'pt2.puf'
	include 'vol1.puf'
	include 'vol2.puf'
	!
	data ldate/.true./
	!
	! --- Set character for scale of output units (m:milli,u:micro,n:nano)
	data cprtu/' ','m','u','n',' ','T','G',' '/
	! --- Set units conversion factor
	data rprtu/1.0,1.0e3,1.0e6,1.0e9,1.0,1.0e-12,1.0e-09,1.0 /
	! --- Construct units name for concentrations (list file)
	if(iprtu.LT.5) then
		cunits='( g/m**3)    '
		cunits(2:2)=cprtu(iprtu)
	elseif(iprtu.EQ.5) then
		cunits='(Odour Units)'
	else
		cunits='( Bq/m**3)   '
		cunits(2:2)=cprtu(iprtu)
	endif
	! --- Set grid coordinates in km
	xorigkm=0.001*xorig
	yorigkm=0.001*yorig
	dgridkm=0.001*dgrid
	! --- Identify source
	! --- (Use position of current emission step 1 for moving sources)
	csrcnam='                '
	if(isrcmode.EQ.1) then
		! ---    Output for a specific source
		if(ktype.EQ.1) then
			csrcnam=cnampt1(ksource)
			xmapkm=xpt1grd(ksource)*dgridkm+xorigkm
			ymapkm=ypt1grd(ksource)*dgridkm+yorigkm
		elseif(ktype.EQ.2) then
			csrcnam=cid2(ksource)
			xmapkm=tiem2(1,ksource)*dgridkm+xorigkm
			ymapkm=tiem2(2,ksource)*dgridkm+yorigkm
		elseif(ktype.EQ.3) then
			csrcnam=cnamar1(ksource)
			xmapkm=xar1grd(1+nvert1(ksource),ksource)*dgridkm+xorigkm
			ymapkm=yar1grd(1+nvert1(ksource),ksource)*dgridkm+yorigkm
		elseif(ktype.EQ.4) then
			csrcnam=cid3(ksource)
			xmapkm=xar2grd(1+nvert2(ksource),1,ksource)*dgridkm+xorigkm
			ymapkm=yar2grd(1+nvert2(ksource),1,ksource)*dgridkm+yorigkm
		elseif(ktype.EQ.5) then
			csrcnam=cnamln1(ksource)
			xgrd=0.5*(xlbeggrd(ksource)+xlendgrd(ksource))
			ygrd=0.5*(ylbeggrd(ksource)+ylendgrd(ksource))
			xmapkm=xgrd*dgridkm+xorigkm
			ymapkm=ygrd*dgridkm+yorigkm
		elseif(ktype.EQ.6) then
			csrcnam=cid5(ksource)
			xgrd=0.5*(xl2beggrd(1,ksource)+xl2endgrd(1,ksource))
			ygrd=0.5*(yl2beggrd(1,ksource)+yl2endgrd(1,ksource))
			xmapkm=xgrd*dgridkm+xorigkm
			ymapkm=ygrd*dgridkm+yorigkm
		elseif(ktype.EQ.7) then
			csrcnam=cnamvl1(ksource)
			xmapkm=xvl1grd(ksource)*dgridkm+xorigkm
			ymapkm=yvl1grd(ksource)*dgridkm+yorigkm
		elseif(ktype.EQ.8) then
			csrcnam=cid4(ksource)
			xmapkm=xvl2grd(1,ksource)*dgridkm+xorigkm
			ymapkm=yvl2grd(1,ksource)*dgridkm+yorigkm
		else
			write(io6,*) 'FATAL error in OUTPUT - invalid source'
			write(io6,*) 'Source Type and number = ',ktype,ksource
			stop 'Subr. OUTPUT:  Invalid Source'
		endif
	else
		! ---    Output for all sources (total)
		if(ktype.EQ.0 .AND. ksource.EQ.1) then
			csrcnam='TOTAL           '
			xmapkm=xorigkm
			ymapkm=yorigkm
		else
			write(io6,*) 'FATAL error in OUTPUT - invalid source'
			write(io6,*) 'Source Type and number = ',ktype,ksource
			stop 'Subr. OUTPUT:  Invalid Source'
		endif
	endif
	! --- Construct units name for fluxes  (list file)
	funits='(-----------)'
	if(iprtu.LT.5) then
		funits='( g/m**2/s)  '
		funits(2:2)=cprtu(iprtu)
	endif
	if(iprtu.LT.5) then
		funits='( g/m**2/s)  '
		funits(2:2)=cprtu(iprtu)
	elseif(iprtu.EQ.5) then
		funits='(-----------)'
	else
		funits='( Bq/m**2/s) '
		funits(2:2)=cprtu(iprtu)
	endif
	!
	! --- Check if active portion of the sampling arrays is less than
	! --- the full dimension (if so, active portion is extracted for
	! --- output purposes
	ifull=0
	if(lsamp) then
		if(nxsam.eq.mxnxg.and.nysam.eq.mxnyg)then
			ifull=1
		endif
	endif
	! ---------------------------------------------------------
	! --- PROCESS OUTPUT SPECIES-GROUPS
	! ---------------------------------------------------------
	! --- Conc/flux data for each species that are in a group are summed
	! --- prior to output, and given the name of the species-group.  The
	! --- summation is done at the array location of the first species
	! --- in the group.
	!
	! --- Definitions ---
	! --- ISPGRP(i)       :group index for species i
	! --- CGRUP(j)        :output species name for group j
	! --- ISTORE(j)       :index of conc/flux arrays used to store
	!                      summed results for group j
	! ---------------------------------------------------------
	! --- Clear istore array
	do is=1,nspec
		istore(is)=0
	enddo
	! --- Group species and store
	do is=1,nspec
		if(istore(ispgrp(is)).EQ.0) then
			! ---       First species in this group; just store array location
			istore(ispgrp(is))=is
		else
			! ---       Add results for this species to the existing sum
			! ---       Gridded receptors
			if(LSAMP) then
				do iy=1,nysam
					do ix=1,nxsam
						chisam(ix,iy,istore(ispgrp(is)))=&
						chisam(ix,iy,istore(ispgrp(is)))+chisam(ix,iy,is)
						dfsam(ix,iy,istore(ispgrp(is)))=&
						dfsam(ix,iy,istore(ispgrp(is)))+dfsam(ix,iy,is)
						wfsam(ix,iy,istore(ispgrp(is)))=&
						wfsam(ix,iy,istore(ispgrp(is)))+wfsam(ix,iy,is)
					enddo
				enddo
			endif
			! ---       Discrete receptors
			do ir=1,nrec
				chirec(ir,istore(ispgrp(is)))=&
				chirec(ir,istore(ispgrp(is)))+chirec(ir,is)
				dfrec(ir,istore(ispgrp(is)))=&
				dfrec(ir,istore(ispgrp(is)))+dfrec(ir,is)
				wfrec(ir,istore(ispgrp(is)))=&
				wfrec(ir,istore(ispgrp(is)))+wfrec(ir,is)
			enddo
			! ---       CTSG receptors
			do ir=1,nctrec
				chict(ir,istore(ispgrp(is)))=&
				chict(ir,istore(ispgrp(is)))+chict(ir,is)
			enddo
			! ---       Mass flux arrays
			if(imflx.EQ.1 .AND. isrcmode.NE.1) call MFLXGRP(istore&
				(ispgrp(is)),is)
		endif
	enddo
	! ---------------------------------------------------------------
	! --- WRITE CONCENTRATIONS TO DISK   (g/m**3,odour_units,Bq/m**3)
	! ---------------------------------------------------------------
	!
	! --- Output date/hour times use 0-23 convention so that hour 24
	! --- of day 12 starts at 23 0000 on day 12 and ends at 00 0000
	! --- on day 13.
	if(.not. (icon.ne.1)) then ! add by @creaqi goto 492 in fun modify_goto_pure
		!
		! --- Write date/time and source data records
		write(io8)nyrab,njulab,nhrab,nsecab,nyre,njule,nhre,nsece
		write(io8)ktype,ksource,csrcnam,xmapkm,ymapkm
		!
		do ig=1,ngrup! add by @creaqi do label 400
			! --- Identify array storage location for this group
			i=istore(ig)
			!
			! --- Only species-groups specified are stored on disk
			if(ioutop(2,ig).eq.1)then
				cname=cgrup(ig)
				cname(13:15)='  1'
				!
				! ---    Gridded receptor concentrations
				if(lsamp) then
					if(ifull.eq.1)then
						if(lcomprs)then
							! ---             Write compressed data records
							call comprs(chisam(1,1,i),mxnxyg,tmp8,mxnxyg,&
							cname,io8)
						else
							! ---             Write uncompressed data record
							call wrdat(io8,cname,chisam(1,1,i),nxsam,nysam)
						endif
					else
						call xtract(chisam(1,1,i),mxnxg,mxnyg,nxsam,nysam,tmp7)
						if(lcomprs)then
							! ---             Write compressed data records
							nwords=nxsam*nysam
							call comprs(tmp7,nwords,tmp8,mxnxyg,cname,io8)
						else
							! ---             Write uncompressed data record
							call wrdat(io8,cname,tmp7,nxsam,nysam)
						endif
					endif
				endif
				!
				! ---    Discrete receptor concentrations
				if(nrec .GT. 0) then
					if(lcomprs)then
						! ---          Write compressed data records
						call comprs(chirec(1,i),nrec,tmp3,mxrec,cname,io8)
					else
						! ---          Write uncompressed data record
						call wrdat(io8,cname,chirec(1,i),nrec,1)
					endif
				endif
				!
				! ---    Discrete CTSG receptor concentrations
				if(nctrec .GT. 0) then
					if(lcomprs)then
						! ---          Write compressed data records
						call comprs(chict(1,i),nctrec,tmp5,mxrect,cname,io8)
					else
						! ---          Write uncompressed data record
						call wrdat(io8,cname,chict(1,i),nctrec,1)
					endif
				endif
			endif
		enddo !add by @creaqi 400
		400   continue
	endif !add by @creaqi label 492 modify_goto_pure
	492   continue
	! ---------------------------------------------------------
	! --- WRITE DRY FLUXES TO DISK  (g/m**2/s,Bq/m**2/s)
	! ---------------------------------------------------------
	!
	! --- Output date/hour times use 0-23 convention so that hour 24
	! --- of day 12 starts at 23 0000 on day 12 and ends at 00 0000
	! --- on day 13.
	if(.not. (idry.ne.1)) then ! add by @creaqi goto 592 in fun modify_goto_pure
		!
		! --- Write date/time and source data records
		write(io9)nyrab,njulab,nhrab,nsecab,nyre,njule,nhre,nsece
		write(io9)ktype,ksource,csrcnam,xmapkm,ymapkm
		!
		do ig=1,ngrup! add by @creaqi do label 500
			! --- Identify array storage location for this group
			i=istore(ig)
			!
			! --- Only species specified are stored on disk
			if(ioutop(4,ig).eq.1)then
				cname=cgrup(ig)
				cname(13:15)=' DF'
				!
				! ---    Gridded receptor dry fluxes
				if(lsamp) then
					if(ifull.eq.1)then
						if(lcomprs)then
							! ---             Write compressed data records
							call comprs(dfsam(1,1,i),mxnxyg,tmp8,mxnxyg,&
							cname,io9)
						else
							! ---             Write uncompressed data record
							call wrdat(io9,cname,dfsam(1,1,i),nxsam,nysam)
						endif
					else
						call xtract(dfsam(1,1,i),mxnxg,mxnyg,nxsam,nysam,tmp7)
						if(lcomprs)then
							! ---             Write compressed data records
							nwords=nxsam*nysam
							call comprs(tmp7,nwords,tmp8,mxnxyg,cname,io9)
						else
							! ---             Write uncompressed data record
							call wrdat(io9,cname,tmp7,nxsam,nysam)
						endif
					endif
				endif
				!
				! ---    Discrete receptor dry fluxes
				if(nrec .GT. 0) then
					if(lcomprs)then
						! ---          Write compressed data records
						call comprs(dfrec(1,i),nrec,tmp3,mxrec,cname,io9)
					else
						! ---          Write uncompressed data record
						call wrdat(io9,cname,dfrec(1,i),nrec,1)
					endif
				endif
			endif
		enddo !add by @creaqi 500
		500   continue
	endif !add by @creaqi label 592 modify_goto_pure
	592   continue
	! ---------------------------------------------------------
	! --- WRITE WET FLUXES TO DISK  (g/m**2/s,Bq/m**2/s)
	! ---------------------------------------------------------
	!
	! --- Output date/hour times use 0-23 convention so that hour 24
	! --- of day 12 starts at 23 0000 on day 12 and ends at 00 0000
	! --- on day 13.
	if(.not. (iwet.ne.1)) then ! add by @creaqi goto 692 in fun modify_goto_pure
		!
		! --- Write date/time and source data records
		write(io10)nyrab,njulab,nhrab,nsecab,nyre,njule,nhre,nsece
		write(io10)ktype,ksource,csrcnam,xmapkm,ymapkm
		!
		do ig=1,ngrup! add by @creaqi do label 600
			! --- Identify array storage location for this group
			i=istore(ig)
			!
			! --- Only species specified are stored on disk
			if(ioutop(6,ig).eq.1)then
				cname=cgrup(ig)
				cname(13:15)=' WF'
				!
				! ---    Gridded receptor wet fluxes
				if(lsamp) then
					if(ifull.eq.1)then
						if(lcomprs)then
							! ---             Write compressed data records
							call comprs(wfsam(1,1,i),mxnxyg,tmp8,mxnxyg,&
							cname,io10)
						else
							! ---             Write uncompressed data record
							call wrdat(io10,cname,wfsam(1,1,i),nxsam,nysam)
						endif
					else
						call xtract(wfsam(1,1,i),mxnxg,mxnyg,nxsam,nysam,tmp7)
						if(lcomprs)then
							! ---             Write compressed data records
							nwords=nxsam*nysam
							call comprs(tmp7,nwords,tmp8,mxnxyg,cname,io10)
						else
							! ---             Write uncompressed data record
							call wrdat(io10,cname,tmp7,nxsam,nysam)
						endif
					endif
				endif
				!
				! ---    Discrete receptor wet fluxes
				if(nrec .GT. 0) then
					if(lcomprs)then
						! ---          Write compressed data records
						call comprs(wfrec(1,i),nrec,tmp3,mxrec,cname,io10)
					else
						! ---          Write uncompressed data record
						call wrdat(io10,cname,wfrec(1,i),nrec,1)
					endif
				endif
			endif
		enddo !add by @creaqi 600
		600   continue
	endif !add by @creaqi label 692 modify_goto_pure
	692   continue
	!
	! ---------------------------------------------------------
	! --- WRITE VISIBILITY-RELATED DATA TO DISK (RH, T2D)
	! ---------------------------------------------------------
	!
	! --- Output date/hour times use 0-23 convention so that hour 24
	! --- of day 12 starts at 23 0000 on day 12 and ends at 00 0000
	! --- on day 13.
	if(.not.((IVIS.ne.1 .AND. IT2D.ne.1 .AND. IRHO.ne.1)&
		.OR. isrcmode.NE.0))then ! add by @creaqi continuation
		!
		! --- Write date/time data record
			if(IVIS.eq.1)write(io11)nyrab,njulab,nhrab,nsecab,&
		nyre,njule,nhre,nsece
			if(IT2D.eq.1)write(io13)nyrab,njulab,nhrab,nsecab,&
		nyre,njule,nhre,nsece
			if(IRHO.eq.1)write(io14)nyrab,njulab,nhrab,nsecab,&
		nyre,njule,nhre,nsece
		!
		! --- Set variable names
		cnameRH=' REL HUM (%)   '
		cnameTK=' TEMP 2D (K)   '
		cnameDN=' RHO 2D (kg/m3)'
		!
		! frr (09/01) new calmet format (2D RH)
		if(i2dmet.eq.1) then
			if(IVIS.eq.1) then
				! ---       Reported RH at CALMET gridpoints
				call xtracti(irh2d,mxnx,mxny,nxm,nym,itmp)
				call wrint(io11,cnameRH,itmp,nxm,nym)
			endif
			if(IT2D.eq.1) then
				! ---       Reported TK at CALMET gridpoints
				call xtract(temp2d,mxnx,mxny,nxm,nym,tmp1)
				call wrdat(io13,cnameTK,tmp1,nxm,nym)
			endif
			if(IRHO.eq.1) then
				! ---       Reported Rho at CALMET gridpoints
				call xtract(rho2d,mxnx,mxny,nxm,nym,tmp1)
				call wrdat(io14,cnameDN,tmp1,nxm,nym)
			endif
		elseif(i2dmet.eq.0) then
			if(IVIS.eq.1) then
				! ---       Reported RH at surface stations
				call wrint(io11,cnameRH,irhss,nssta,1)
			endif
			if(IT2D.eq.1) then
				! ---       Reported TK at surface stations
				call wrdat(io13,cnameTK,tempss,nssta,1)
			endif
			if(IRHO.eq.1) then
				! ---       Reported Rho at surface stations
				call wrdat(io14,cnameDN,rhoss,nssta,1)
			endif
		else
			write(*,*)'Subr. OUTPUT:  Invalid I2DMET = ',i2dmet
			stop
		endif
		!      call wrint(io11,cname,irhss,nssta,1)
		!
	endif ! add by @creaqi continuation
	792   continue
	! --- Set format for List-File heading text
	1050  format(//,'------ Contribution from Source: ',a16,//)
	1051  format(//,'------ Contribution from ALL Sources',//)
	! ---------------------------------------------------------
	! --- PRINT CONCENTRATIONS TO LIST FILE  (user units)
	! ---------------------------------------------------------
	if(.not. (icprt.ne.1)) then ! add by @creaqi goto 192 in fun modify_goto_pure
		if (.not. (mod(istep,icfrq).ne.0)) then ! add by @creaqi 192 state_same == True
			if(.not. (.not.LSAMP)) then ! add by @creaqi goto 101 in fun modify_goto_pure
				!
				! --- Gridded receptor concentrations
				! -----------------------------------
				messag='CONCENTRATIONS '
				messag(16:28)=cunits
				messag(31:38)='SPECIES:'
				!
				do ig=1,ngrup! add by @creaqi do label 100
					! --- Identify array storage location for this group
					i=istore(ig)
					!
					if(ioutop(1,ig).eq.1)then
						if(isrcmode.EQ.1) then
							write(io6,1050) csrcnam
						else
							write(io6,1051)
						endif
						write(messag(40:51),'(a12)')cgrup(ig)
						! ---    Scale units for IPRTU
						do iy=1,nysam
							do ix=1,nxsam
								chisam(ix,iy,i)=chisam(ix,iy,i)*rprtu(iprtu)
							enddo
						enddo
						call outsam(chisam(1,1,i),idum,1,5,ldate,messag,nxsam,nysam)
					endif
				enddo !add by @creaqi 100
				100   continue
			endif !add by @creaqi label 101 modify_goto_pure
			101   continue
			if(.not. (nrec.eq.0)) then ! add by @creaqi goto 111 in fun modify_goto_pure
				!
				! --- Discrete receptor concentrations
				! ------------------------------------
				! --- Create label of species to be printed
				ns=0
				do ig=1,ngrup
					if(ioutop(1,ig).eq.1)then
						ns=ns+1
						cspecsv(ns)=cgrup(ig)
					endif
				enddo
				if (.not. (ns.EQ.0)) then ! add by @creaqi 111 state_same == True
					if(isrcmode.EQ.1) then
						write(io6,1050) csrcnam
					else
						write(io6,1051)
					endif
					write(io6,106)cunits,nyrab,nmoab,ndayab,njulab,nhrab,&
					nsecab,nyre,nmoe,ndaye,njule,nhre,nsece,&
					(cspecsv(n),n=1,ns)
					106   format(//1x,'DISCRETE RECEPTOR CONCENTRATIONS ',a13,t71,&
					'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,' Julian day: ',i3,&
					2x,'hour: ',i2,2x,'sec: ',i4/,t68,'to ',&
					'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,' Julian day: ',i3,&
					2x,'hour: ',i2,2x,'sec: ',i4/,1x,'Receptor No.',5x,10a12)
					!
					do ir=1,nrec! add by @creaqi do label 110
						! ---    Scale & store only those groups to be printed in a work array
						ns=0
						do ig=1,ngrup
							if(ioutop(1,ig).eq.1)then
								ns=ns+1
								xtmp(ns)=chirec(ir,istore(ig))*rprtu(iprtu)
							endif
						enddo
						!
						! ---    Write the discrete receptor concentrations
						write(io6,108)ir,(xtmp(n),n=1,ns)
						108      format(1x,i6,8x,10(1pe11.4,1x))
					enddo !add by @creaqi 110
					110   continue
				endif ! add by @creaqi 111 state_same == True
			endif !add by @creaqi label 111 modify_goto_pure
			111   continue
			if (.not. (nctrec.eq.0)) then ! add by @creaqi 192 state_same == True
				!
				! --- Complex terrain (CTSG) receptor concentrations
				! --------------------------------------------------
				! --- Create label of species to be printed
				ns=0
				do ig=1,ngrup
					if(ioutop(1,ig).eq.1)then
						ns=ns+1
						cspecsv(ns)=cgrup(ig)
					endif
				enddo
				if (.not. (ns.EQ.0)) then ! add by @creaqi 192 state_same == True
					if(isrcmode.EQ.1) then
						write(io6,1050) csrcnam
					else
						write(io6,1051)
					endif
					write(io6,116)cunits,nyrab,nmoab,ndayab,njulab,nhrab,&
					nsecab,nyre,nmoe,ndaye,njule,nhre,nsece,&
					(cspecsv(n),n=1,ns)
					116   format(//1x,'CTSG RECEPTOR CONCENTRATIONS ',a13,t71,&
					'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,' Julian day: ',i3,&
					2x,'hour: ',i2,2x,'sec: ',i4/,t68,'to ',&
					'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,' Julian day: ',i3,&
					2x,'hour: ',i2,2x,'sec: ',i4/,4x,'No.',4x,'X met',5x,'Y met',4x,&
					'ELEV.',4x,'Hill No.',3x,10a12)
					do i=1,nctrec! add by @creaqi do label 120
						! ---    Scale & store only those groups to be printed in a work array
						ns=0
						do ig=1,ngrup
							if(ioutop(1,ig).eq.1)then
								ns=ns+1
								xtmp(ns)=chict(i,istore(ig))*rprtu(iprtu)
							endif
						enddo
						write(io6,118)i,xrct(i),yrct(i),elrect(i),ihill(i),&
						(xtmp(n),n=1,ns)
						118      format(1x,i5,3x,f7.3,3x,f7.3,3x,f6.1,3x,i5,6x,10(1pe11.4,1x))
					enddo !add by @creaqi 120
					120   continue
				endif ! add by @creaqi 192 state_same == True
			endif ! add by @creaqi 192 state_same == True
		endif ! add by @creaqi 192 state_same == True
	endif !add by @creaqi label 192 modify_goto_pure
	192   continue
	! ---------------------------------------------------------
	! --- PRINT DRY FLUXES TO LIST FILE
	! ---------------------------------------------------------
	if(.not. (idprt.ne.1)) then ! add by @creaqi goto 292 in fun modify_goto_pure
		if (.not. (mod(istep,idfrq).ne.0)) then ! add by @creaqi 292 state_same == True
			!
			if(LSAMP) then
				! ---    Gridded receptors
				! ------------------------
				messag='DRY FLUXES '
				messag(12:24)=funits
				messag(31:38)='SPECIES:'
				!
				do ig=1,ngrup
					! ---       Identify array storage location for this group
					i=istore(ig)
					if(ioutop(3,ig).eq.1)then
						if(isrcmode.EQ.1) then
							write(io6,1050) csrcnam
						else
							write(io6,1051)
						endif
						write(messag(40:51),'(a12)')cgrup(ig)
						! ---          Scale units for IPRTU
						do iy=1,nysam
							do ix=1,nxsam
								dfsam(ix,iy,i)=dfsam(ix,iy,i)*rprtu(iprtu)
							enddo
						enddo
						call outsam(dfsam(1,1,i),idum,1,5,ldate,messag,nxsam,&
						nysam)
					endif
				enddo
			endif
			!
			if(nrec.GT.0) then
				! ---    Discrete receptors
				! -------------------------
				! ---    Create label of species to be printed
				ns=0
				do ig=1,ngrup
					if(ioutop(3,ig).eq.1)then
						ns=ns+1
						cspecsv(ns)=cgrup(ig)
					endif
				enddo
				if(ns.GT.0) then
					if(isrcmode.EQ.1) then
						write(io6,1050) csrcnam
					else
						write(io6,1051)
					endif
					write(io6,206)funits,nyrab,nmoab,ndayab,njulab,nhrab,&
					nsecab,nyre,nmoe,ndaye,njule,nhre,nsece,&
					(cspecsv(n),n=1,ns)
					206         format(//1x,'DISCRETE RECEPTOR DRY FLUXES ',a13,t71,&
					'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,&
					' Julian day: ',i3,2x,'hour: ',i2,2x,'sec: ',i4/,t68,'to ',&
					'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,&
					' Julian day: ',i3,2x,'hour: ',i2,2x,'sec: ',i4/&
					,1x,'Receptor No.',5x,10a12)
					!
					do ir=1,nrec
						! ---          Scale & store only those groups to be printed
						is=0
						do ig=1,ngrup
							if(ioutop(3,ig).eq.1)then
								is=is+1
								xtmp(is)=dfrec(ir,istore(ig))*rprtu(iprtu)
							endif
						enddo
						! ---          Write the discrete receptor dry fluxes
						write(io6,208)ir,(xtmp(n),n=1,ns)
						208            format(1x,i6,8x,10(1pe11.4,1x))
					enddo
				endif
			endif
		endif ! add by @creaqi 292 state_same == True
	endif !add by @creaqi label 292 modify_goto_pure
	292   continue
	! ---------------------------------------------------------
	! --- PRINT WET FLUXES TO LIST FILE
	! ---------------------------------------------------------
	if(.not. (iwprt.ne.1)) then ! add by @creaqi goto 392 in fun modify_goto_pure
		if (.not. (mod(istep,iwfrq).ne.0)) then ! add by @creaqi 392 state_same == True
			!
			if(LSAMP) then
				! ---    Gridded receptors
				! ------------------------
				messag='WET FLUXES '
				messag(12:24)=funits
				messag(31:38)='SPECIES:'
				!
				do ig=1,ngrup
					! ---       Identify array storage location for this group
					i=istore(ig)
					if(ioutop(5,ig).eq.1)then
						if(isrcmode.EQ.1) then
							write(io6,1050) csrcnam
						else
							write(io6,1051)
						endif
						write(messag(40:51),'(a12)')cgrup(ig)
						! ---          Scale units for IPRTU
						do iy=1,nysam
							do ix=1,nxsam
								wfsam(ix,iy,i)=wfsam(ix,iy,i)*rprtu(iprtu)
							enddo
						enddo
						call outsam(wfsam(1,1,i),idum,1,5,ldate,messag,nxsam,&
						nysam)
					endif
				enddo
			endif
			!
			if(nrec.GT.0) then
				! ---    Discrete receptors
				! -------------------------
				! ---    Create label of species to be printed
				ns=0
				do ig=1,ngrup
					if(ioutop(5,ig).eq.1)then
						ns=ns+1
						cspecsv(ns)=cgrup(ig)
					endif
				enddo
				if(ns.GT.0) then
					if(isrcmode.EQ.1) then
						write(io6,1050) csrcnam
					else
						write(io6,1051)
					endif
					write(io6,306)funits,nyrab,nmoab,ndayab,njulab,nhrab,&
					nsecab,nyre,nmoe,ndaye,njule,nhre,nsece,&
					(cspecsv(n),n=1,ns)
					306         format(//1x,'DISCRETE RECEPTOR WET FLUXES ',a13,t71,&
					'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,&
					' Julian day: ',i3,2x,'hour: ',i2,2x,'sec: ',i4/,t68,'to ',&
					'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,&
					' Julian day: ',i3,2x,'hour: ',i2,2x,'sec: ',i4/&
					,1x,'Receptor No.',5x,10a12)
					!
					do ir=1,nrec
						! ---          Scale & store only those groups to be printed
						is=0
						do ig=1,ngrup
							if(ioutop(5,ig).eq.1)then
								is=is+1
								xtmp(is)=wfrec(ir,istore(ig))*rprtu(iprtu)
							endif
						enddo
						! ---          Write the discrete receptor wet fluxes
						write(io6,208)ir,(xtmp(n),n=1,ns)
						308            format(1x,i6,8x,10(1pe11.4,1x))
					enddo
				endif
			endif
		endif ! add by @creaqi 392 state_same == True
	endif !add by @creaqi label 392 modify_goto_pure
	392   continue
	! ------------------------------------------------------------
	! --- Write MASS FLUXES for species-groups to MASSFLX.DAT file
	! ------------------------------------------------------------
	if(imflx.EQ.1 .AND. isrcmode.EQ.0) call MFLXOUT(istore)
	! ------------------------------------------------------------
	! --- Write MASS BALANCE for species to MASSBAL.DAT file
	! ------------------------------------------------------------
	if(imbal.EQ.1 .AND. isrcmode.EQ.0) call MBALOUT
	return
end
!----------------------------------------------------------------------
subroutine wrdat(iounit,cname,outarr,nx,ny)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 940430             WRDAT
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Write a gridded concentration or dry/wet
	!               flux data record
	!               (one 15-character identifier and a 2-D data array)
	!
	! --- INPUTS:
	!          IOUNIT - integer      - Fortran unit no. of output file
	!           CNAME - character*15 - Species identifier
	!   OUTARR(nx,ny) - real array   - Array of concentration data (g/m**3)
	!                                  or dry/wet flux data (g/m**2/s)
	!              NX - integer      - Number of sampling grid points in the
	!                                  X direction
	!              NY - integer      - Number of sampling grid points in the
	!                                  Y direction
	!
	! --- OUTPUT:  none
	!
	! --- WRDAT called by:  OUTPUT
	! --- WRDAT calls:      none
	!
	!----------------------------------------------------------------------
	!
	real outarr(nx,ny)
	character*15 cname
	!
	write(iounit)cname,outarr
	!
	return
end
!----------------------------------------------------------------------
subroutine wrint(iounit,cname,ioutarr,nx,ny)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 940430             WRINT
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Write a gridded field of integers to binary file
	!               (one 15-character identifier and a 2-D data array)
	!
	! --- INPUTS:
	!          IOUNIT - integer      - Fortran unit no. of output file
	!           CNAME - character*15 - Data identifier/name
	!  IOUTARI(nx,ny) - int. array   - Data array
	!              NX - integer      - Number of grid points in the
	!                                  X direction
	!              NY - integer      - Number of grid points in the
	!                                  Y direction
	!
	! --- OUTPUT:  none
	!
	! --- WRINT called by:
	! --- WRINT calls:      none
	!
	!----------------------------------------------------------------------
	!
	integer ioutarr(nx,ny)
	character*15 cname
	!
	write(iounit)cname,ioutarr
	!
	return
end
!----------------------------------------------------------------------
subroutine xtract(datarr,nxmax,nymax,nxact,nyact,outarr)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 000602            XTRACT
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Extract the active portion of a 2-D array
	!
	! --- UPDATE
	! --- V5.0-V5.4     000602  (DGS): add message to "stop"
	! --- V4.0-V5.0     971107  (DGS): NYACT compared to NYMAX (not NXMAX)
	!
	! --- INPUTS:
	!   DATARR(nxmax,nymax) - real    - Full data array
	!                 NXMAX - integer - First dimension of data array
	!                 NYMAX - integer - Second dimension of data array
	!                 NXACT - integer - Number of active elements of the
	!                                   array (first dimension)
	!                 NYACT - integer - Number of active elements of the
	!                                   array (second dimension)
	!        Parameters:
	!           IO6
	!
	! --- OUTPUT:
	!   OUTARR(nxact,nyact) - real    - Output array consisting only
	!                                   of the active elements of the
	!                                   full input array
	!
	! --- XTRACT called by:  OUTPUT
	! --- XTRACT calls:      none
	!
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	!
	real datarr(nxmax,nymax),outarr(nxact,nyact)
	!
	! --- Check that values of array dimensions are reasonable
	if(nxact.le.0.or.nxact.gt.nxmax.or.&
		nyact.le.0.or.nyact.gt.nymax)then
		write(io6,*)'ERROR in subr. XTRACT -- Invalid values ',&
		'of array dimensions input -- NXACT = ',nxact,' NYACT = ',&
		nyact,' NXMAX = ',nxmax,' NYMAX = ',nymax
		write(*,*)
		stop 'Halted in XTRACT -- see list file.'
	endif
	!
	! --- Extract the active portion of the input data array
	do i=1,nxact! add by @creaqi do label 100
		do j=1,nyact! add by @creaqi do label 100
			outarr(i,j)=datarr(i,j)
		enddo !add by @creaqi 100
	enddo !add by @creaqi 100
	100   continue
	!
	return
end
!----------------------------------------------------------------------
subroutine xtracti(idatarr,nxmax,nymax,nxact,nyact,ioutarr)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 000602           XTRACTI
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Extract the active portion of a 2-D integer array
	!
	! --- UPDATE
	! --- V5.0-V5.4     000602  (DGS): add message to "stop"
	! --- V4.0-V5.0     971107  (DGS): NYACT compared to NYMAX (not NXMAX)
	!
	! --- INPUTS:
	!  IDATARR(nxmax,nymax) - integer - Full data array
	!                 NXMAX - integer - First dimension of data array
	!                 NYMAX - integer - Second dimension of data array
	!                 NXACT - integer - Number of active elements of the
	!                                   array (first dimension)
	!                 NYACT - integer - Number of active elements of the
	!                                   array (second dimension)
	!        Parameters:
	!           IO6
	!
	! --- OUTPUT:
	!  IOUTARR(nxact,nyact) - integer - Output array consisting only
	!                                   of the active elements of the
	!                                   full input array
	!
	! --- XTRACTI called by:
	! --- XTRACTI calls:      none
	!
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	!
	integer idatarr(nxmax,nymax),ioutarr(nxact,nyact)
	!
	! --- Check that values of array dimensions are reasonable
	if(nxact.le.0.or.nxact.gt.nxmax.or.&
		nyact.le.0.or.nyact.gt.nymax)then
		write(io6,*)'ERROR in subr. XTRACTI -- Invalid values ',&
		'of array dimensions input -- NXACT = ',nxact,' NYACT = ',&
		nyact,' NXMAX = ',nxmax,' NYMAX = ',nymax
		write(*,*)
		stop 'Halted in XTRACTI -- see list file.'
	endif
	!
	! --- Extract the active portion of the input data array
	do i=1,nxact! add by @creaqi do label 100
		do j=1,nyact! add by @creaqi do label 100
			ioutarr(i,j)=idatarr(i,j)
		enddo !add by @creaqi 100
	enddo !add by @creaqi 100
	100   continue
	!
	return
end
!----------------------------------------------------------------------
subroutine comprs(xdat,nwords,xwork,nwork,clabel,io)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 000602            COMPRS
	!                J. Scire, EARTH TECH
	!
	! --- PURPOSE:  Compress an array of concentrations, dry fluxes,
	!               or wet fluxes by replacing strings of zero values
	!               with a negative code indicating the number of zero
	!               values
	!
	! --- UPDATE
	! --- V5.3-V5.4     000602  (DGS): add message to "stop"
	!
	! --- INPUTS:
	!        XDAT(nwords) - real array - Array of uncompressed data to be
	!                                    output
	!              NWORDS - integer    - Number of values in data array
	!        XWORK(nwork) - real array - Work array to temporarily store
	!                                    compressed array
	!               NWORK - integer    - Dimension of work array - NWORK
	!                                    must be >= NWORDS
	!              CLABEL - char*15    - Character record header
	!                  IO - integer    - Unit number of output file
	!
	!     Parameters: IO6
	!
	! --- OUTPUT:  none
	!
	! --- COMPRS called by: OUTPUT
	! --- COMPRS calls:     WRDAT
	!----------------------------------------------------------------------
	!
	! --- Include parameter statements
	include 'params.puf'
	!
	real xdat(nwords),xwork(nwork)
	character*15 clabel
	!
	! --- Check that work array is sized large enough
	if(nwork.lt.nwords)then
		write(io6,*)'ERROR in Subr. COMPRS -- Work array ',&
		'dimension is too small -- NWORK = ',nwork,' NWORDS = ',&
		nwords
		write(*,*)
		stop 'Halted in COMPRS -- see list file.'
	endif
	!
	! --- Replace all zeroes with negative coded integer
	nzero=0
	ii=0
	do 100 i=1,nwords
		!
		if(xdat(i).eq.0.0)then
			nzero=nzero+1
			cycle ! add by @creaqi replace goto do label with cycle goto 100 in comprs
		else if(xdat(i).lt.0.0)then
			write(io6,*)'ERROR in Subr. COMPRS -- Negative value ',&
			'encountered with COMPRESS option on -- I = ',i,&
			' XDAT(i) = ',xdat(i)
			write(io6,*)'COMPRESS option cannot be used when data ',&
			'values are negative'
			write(*,*)
			stop 'Halted in COMPRS -- see list file.'
		endif
		!
		if(nzero.eq.0)then
			ii=ii+1
			xwork(ii)=xdat(i)
		else
			ii=ii+1
			xwork(ii)=-(float(nzero)+0.0001)
			nzero=0
			ii=ii+1
			xwork(ii)=xdat(i)
		endif
		100   continue
		!
		if(nzero.gt.0)then
			ii=ii+1
			xwork(ii)=-(float(nzero)+0.0001)
		endif
		!
		! --- Write the data records (header, compressed data record)
		write(io)ii
		call wrdat(io,clabel,xwork,ii,1)
		!
		return
end
	!-----------------------------------------------------------------------
	! --- Section for SLATEC routines used in puff model
	!-----------------------------------------------------------------------
	!DECK SNSQE
	!-----------------------------------------------------------------------
SUBROUTINE SNSQE (FCN, JAC, IOPT, N, X, FVEC, TOL, NPRINT, INFO,&
	WA, LWA)
	!-----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 951204              SNSQE
	!***BEGIN PROLOGUE  SNSQE
	!***PURPOSE  An easy-to-use code to find a zero of a system of N
	!            nonlinear functions in N variables by a modification of
	!            the Powell hybrid method.
	!***LIBRARY   SLATEC
	!***CATEGORY  F2A
	!***TYPE      SINGLE PRECISION (SNSQE-S, DNSQE-D)
	!***KEYWORDS  EASY-TO-USE, NONLINEAR SQUARE SYSTEM,
	!             POWELL HYBRID METHOD, ZEROS
	!***AUTHOR  Hiebert, K. L., (SNLA)
	!***DESCRIPTION
	!
	! 1. Purpose.
	!
	!
	!       The purpose of SNSQE is to find a zero of a system of N non-
	!       linear functions in N variables by a modification of the Powell
	!       hybrid method.  This is done by using the more general nonlinear
	!       equation solver SNSQ.  The user must provide a subroutine which
	!       calculates the functions.  The user has the option of either to
	!       provide a subroutine which calculates the Jacobian or to let the
	!       code calculate it by a forward-difference approximation.  This
	!       code is the combination of the MINPACK codes (Argonne) HYBRD1
	!       and HYBRJ1.
	!
	!
	! 2. Subroutine and Type Statements.
	!
	!       SUBROUTINE SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,
	!      *                  WA,LWA)
	!       INTEGER IOPT,N,NPRINT,INFO,LWA
	!       REAL TOL
	!       REAL X(N),FVEC(N),WA(LWA)
	!       EXTERNAL FCN,JAC
	!
	!
	! 3. Parameters.
	!
	!       Parameters designated as input parameters must be specified on
	!       entry to SNSQE and are not changed on exit, while parameters
	!       designated as output parameters need not be specified on entry
	!       and are set to appropriate values on exit from SNSQE.
	!
	!       FCN is the name of the user-supplied subroutine which calculates
	!         the functions.  FCN must be declared in an EXTERNAL statement
	!         in the user calling program, and should be written as follows.
	!
	!         SUBROUTINE FCN(N,X,FVEC,IFLAG)
	!         INTEGER N,IFLAG
	!         REAL X(N),FVEC(N)
	!         ----------
	!         Calculate the functions at X and
	!         return this vector in FVEC.
	!         ----------
	!         RETURN
	!         END
	!
	!         The value of IFLAG should not be changed by FCN unless the
	!         user wants to terminate execution of SNSQE.  In this case, set
	!         IFLAG to a negative integer.
	!
	!       JAC is the name of the user-supplied subroutine which calculates
	!         the Jacobian.  If IOPT=1, then JAC must be declared in an
	!         EXTERNAL statement in the user calling program, and should be
	!         written as follows.
	!
	!         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
	!         INTEGER N,LDFJAC,IFLAG
	!         REAL X(N),FVEC(N),FJAC(LDFJAC,N)
	!         ----------
	!         Calculate the Jacobian at X and return this
	!         matrix in FJAC.  FVEC contains the function
	!         values at X and should not be altered.
	!         ----------
	!         RETURN
	!         END
	!
	!         The value of IFLAG should not be changed by JAC unless the
	!         user wants to terminate execution of SNSQE.  In this case, set
	!         IFLAG to a negative integer.
	!
	!         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
	!
	!       IOPT is an input variable which specifies how the Jacobian will
	!         be calculated.  If IOPT=1, then the user must supply the
	!         Jacobian through the subroutine JAC.  If IOPT=2, then the
	!         code will approximate the Jacobian by forward-differencing.
	!
	!       N is a positive integer input variable set to the number of
	!         functions and variables.
	!
	!       X is an array of length N.  On input, X must contain an initial
	!         estimate of the solution vector.  On output, X contains the
	!         final estimate of the solution vector.
	!
	!       FVEC is an output array of length N which contains the functions
	!         evaluated at the output X.
	!
	!       TOL is a non-negative input variable.  Termination occurs when
	!         the algorithm estimates that the relative error between X and
	!         the solution is at most TOL.  Section 4 contains more details
	!         about TOL.
	!
	!       NPRINT is an integer input variable that enables controlled
	!         printing of iterates if it is positive.  In this case, FCN is
	!         called with IFLAG = 0 at the beginning of the first iteration
	!         and every NPRINT iteration thereafter and immediately prior
	!         to return, with X and FVEC available for printing. Appropriate
	!         print statements must be added to FCN (see example). If NPRINT
	!         is not positive, no special calls of FCN with IFLAG = 0 are
	!         made.
	!
	!       INFO is an integer output variable.  If the user has terminated
	!         execution, INFO is set to the (negative) value of IFLAG.  See
	!         description of FCN and JAC. Otherwise, INFO is set as follows.
	!
	!         INFO = 0  improper input parameters.
	!
	!         INFO = 1  algorithm estimates that the relative error between
	!                   X and the solution is at most TOL.
	!
	!         INFO = 2  number of calls to FCN has reached or exceeded
	!                   100*(N+1) for IOPT=1 or 200*(N+1) for IOPT=2.
	!
	!         INFO = 3  TOL is too small.  No further improvement in the
	!                   approximate solution X is possible.
	!
	!         INFO = 4  iteration is not making good progress.
	!
	!         Sections 4 and 5 contain more details about INFO.
	!
	!       WA is a work array of length LWA.
	!
	!       LWA is a positive integer input variable not less than
	!         (3*N**2+13*N))/2.
	!
	!
	! 4. Successful Completion.
	!
	!       The accuracy of SNSQE is controlled by the convergence parame-
	!       ter TOL.  This parameter is used in a test which makes a compar-
	!       ison between the approximation X and a solution XSOL.  SNSQE
	!       terminates when the test is satisfied.  If TOL is less than the
	!       machine precision (as defined by the function R1MACH(4)), then
	!       SNSQE attempts only to satisfy the test defined by the machine
	!       precision.  Further progress is not usually possible.  Unless
	!       high precision solutions are required, the recommended value
	!       for TOL is the square root of the machine precision.
	!
	!       The test assumes that the functions are reasonably well behaved,
	!       and, if the Jacobian is supplied by the user, that the functions
	!       and the Jacobian  coded consistently.  If these conditions
	!       are not satisfied, SNSQE may incorrectly indicate convergence.
	!       The coding of the Jacobian can be checked by the subroutine
	!       CHKDER.  If the Jacobian is coded correctly or IOPT=2, then
	!       the validity of the answer can be checked, for example, by
	!       rerunning SNSQE with a tighter tolerance.
	!
	!       Convergence Test.  If ENORM(Z) denotes the Euclidean norm of a
	!         vector Z, then this test attempts to guarantee that
	!
	!               ENORM(X-XSOL) .LE.  TOL*ENORM(XSOL).
	!
	!         If this condition is satisfied with TOL = 10**(-K), then the
	!         larger components of X have K significant decimal digits and
	!         INFO is set to 1.  There is a danger that the smaller compo-
	!         nents of X may have large relative errors, but the fast rate
	!         of convergence of SNSQE usually avoids this possibility.
	!
	!
	! 5. Unsuccessful Completion.
	!
	!       Unsuccessful termination of SNSQE can be due to improper input
	!       parameters, arithmetic interrupts, an excessive number of func-
	!       tion evaluations, errors in the functions, or lack of good prog-
	!       ress.
	!
	!       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1, or
	!         IOPT .GT. 2, or N .LE. 0, or TOL .LT. 0.E0, or
	!         LWA .LT. (3*N**2+13*N)/2.
	!
	!       Arithmetic Interrupts.  If these interrupts occur in the FCN
	!         subroutine during an early stage of the computation, they may
	!         be caused by an unacceptable choice of X by SNSQE.  In this
	!         case, it may be possible to remedy the situation by not evalu-
	!         ating the functions here, but instead setting the components
	!         of FVEC to numbers that exceed those in the initial FVEC.
	!
	!       Excessive Number of Function Evaluations.  If the number of
	!         calls to FCN reaches 100*(N+1) for IOPT=1 or 200*(N+1) for
	!         IOPT=2, then this indicates that the routine is converging
	!         very slowly as measured by the progress of FVEC, and INFO is
	!         set to 2.  This situation should be unusual because, as
	!         indicated below, lack of good progress is usually diagnosed
	!         earlier by SNSQE, causing termination with INFO = 4.
	!
	!       Errors in the Functions.  When IOPT=2, the choice of step length
	!         in the forward-difference approximation to the Jacobian
	!         assumes that the relative errors in the functions are of the
	!         order of the machine precision.  If this is not the case,
	!         SNSQE may fail (usually with INFO = 4).  The user should
	!         then either use SNSQ and set the step length or use IOPT=1
	!         and supply the Jacobian.
	!
	!       Lack of Good Progress.  SNSQE searches for a zero of the system
	!         by minimizing the sum of the squares of the functions.  In so
	!         doing, it can become trapped in a region where the minimum
	!         does not correspond to a zero of the system and, in this situ-
	!         ation, the iteration eventually fails to make good progress.
	!         In particular, this will happen if the system does not have a
	!         zero.  If the system has a zero, rerunning SNSQE from a dif-
	!         ferent starting point may be helpful.
	!
	!
	! 6. Characteristics of the Algorithm.
	!
	!       SNSQE is a modification of the Powell hybrid method.  Two of
	!       its main characteristics involve the choice of the correction as
	!       a convex combination of the Newton and scaled gradient direc-
	!       tions, and the updating of the Jacobian by the rank-1 method of
	!       Broyden.  The choice of the correction guarantees (under reason-
	!       able conditions) global convergence for starting points far from
	!       the solution and a fast rate of convergence.  The Jacobian is
	!       calculated at the starting point by either the user-supplied
	!       subroutine or a forward-difference approximation, but it is not
	!       recalculated until the rank-1 method fails to produce satis-
	!       factory progress.
	!
	!       Timing.  The time required by SNSQE to solve a given problem
	!         depends on N, the behavior of the functions, the accuracy
	!         requested, and the starting point.  The number of arithmetic
	!         operations needed by SNSQE is about 11.5*(N**2) to process
	!         each evaluation of the functions (call to FCN) and 1.3*(N**3)
	!         to process each evaluation of the Jacobian (call to JAC,
	!         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
	!         the timing of SNSQE will be strongly influenced by the time
	!         spent in FCN and JAC.
	!
	!       Storage.  SNSQE requires (3*N**2 + 17*N)/2 single precision
	!         storage locations, in addition to the storage required by the
	!         program.  There are no internally declared storage arrays.
	!
	!
	! 7. Example.
	!
	!       The problem is to determine the values of X(1), X(2), ..., X(9),
	!       which solve the system of tridiagonal equations
	!
	!       (3-2*X(1))*X(1)           -2*X(2)                   = -1
	!               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
	!                                   -X(8) + (3-2*X(9))*X(9) = -1
	!
	!       **********
	!
	!       PROGRAM TEST
	! C
	! C     Driver for SNSQE example.
	! C
	!       INTEGER J,N,IOPT,NPRINT,INFO,LWA,NWRITE
	!       REAL TOL,FNORM
	!       REAL X(9),FVEC(9),WA(180)
	!       REAL ENORM,R1MACH
	!       EXTERNAL FCN
	!       DATA NWRITE /6/
	! C
	!       IOPT = 2
	!       N = 9
	! C
	! C     The following starting values provide a rough solution.
	! C
	!       DO 10 J = 1, 9
	!          X(J) = -1.E0
	!    10    CONTINUE
	!
	!       LWA = 180
	!       NPRINT = 0
	! C
	! C     Set TOL to the square root of the machine precision.
	! C     Unless high precision solutions are required,
	! C     this is the recommended setting.
	! C
	!       TOL = SQRT(R1MACH(4))
	! C
	!       CALL SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA)
	!       FNORM = ENORM(N,FVEC)
	!       WRITE (NWRITE,1000) FNORM,INFO,(X(J),J=1,N)
	!       STOP
	!  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
	!      *        5X,' EXIT PARAMETER',16X,I10 //
	!      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
	!       END
	!       SUBROUTINE FCN(N,X,FVEC,IFLAG)
	!       INTEGER N,IFLAG
	!       REAL X(N),FVEC(N)
	!       INTEGER K
	!       REAL ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
	!       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
	! C
	!       DO 10 K = 1, N
	!          TEMP = (THREE - TWO*X(K))*X(K)
	!          TEMP1 = ZERO
	!          IF (K .NE. 1) TEMP1 = X(K-1)
	!          TEMP2 = ZERO
	!          IF (K .NE. N) TEMP2 = X(K+1)
	!          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
	!    10    CONTINUE
	!       RETURN
	!       END
	!
	!       Results obtained with different compilers or machines
	!       may be slightly different.
	!
	!       FINAL L2 NORM OF THE RESIDUALS  0.1192636E-07
	!
	!       EXIT PARAMETER                         1
	!
	!       FINAL APPROXIMATE SOLUTION
	!
	!       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
	!       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
	!       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
	!
	!***REFERENCES  M. J. D. Powell, A hybrid method for nonlinear equa-
	!                 tions. In Numerical Methods for Nonlinear Algebraic
	!                 Equations, P. Rabinowitz, Editor.  Gordon and Breach,
	!                 1988.
	!***ROUTINES CALLED  SNSQ, XERMSG
	!***REVISION HISTORY  (YYMMDD)
	!   800301  DATE WRITTEN
	!   890831  Modified array declarations.  (WRB)
	!   890831  REVISION DATE from Version 3.2
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
	!   920501  Reformatted the REFERENCES section.  (WRB)
	!***END PROLOGUE  SNSQE
	INTEGER IOPT,N,NPRINT,INFO,LWA
	REAL TOL
	REAL X(*),FVEC(*),WA(LWA)
	EXTERNAL FCN, JAC
	INTEGER INDEX,J,LR,MAXFEV,ML,MODE,MU,NFEV,NJEV
	REAL EPSFCN,FACTOR,ONE,XTOL,ZERO
	SAVE FACTOR, ONE, ZERO
	DATA FACTOR,ONE,ZERO /1.0E2,1.0E0,0.0E0/
	!***FIRST EXECUTABLE STATEMENT  SNSQE
	INFO = 0
	!
	!     CHECK THE INPUT PARAMETERS FOR ERRORS.
	!
	if(.not.(IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0&
		.OR. TOL .LT. ZERO .OR. LWA .LT. (3*N**2 +13*N)/2)&
		)then ! add by @creaqi continuation
		!
		!     CALL SNSQ.
		!
		MAXFEV = 100*(N + 1)
		IF (IOPT .EQ. 2) MAXFEV = 2*MAXFEV
		XTOL = TOL
		ML = N - 1
		MU = N - 1
		EPSFCN = ZERO
		MODE = 2
		DO J = 1, N! add by @creaqi do label 10
			WA(J) = ONE
		enddo !add by @creaqi 10
		10    CONTINUE
		LR = (N*(N + 1))/2
		INDEX=6*N+LR
		CALL SNSQ(FCN,JAC,IOPT,N,X,FVEC,WA(INDEX+1),N,XTOL,MAXFEV,ML,MU,&
		EPSFCN,WA(1),MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,&
		WA(6*N+1),LR,WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),&
		WA(5*N+1))
		IF (INFO .EQ. 5) INFO = 4
	endif ! add by @creaqi continuation
	20 CONTINUE
	IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'SNSQE',&
		'INVALID INPUT PARAMETER.', 2, 1)
	RETURN
	!
	!     LAST CARD OF SUBROUTINE SNSQE.
	!
END
!DECK SNSQ
SUBROUTINE SNSQ (FCN, JAC, IOPT, N, X, FVEC, FJAC, LDFJAC, XTOL,&
	MAXFEV, ML, MU, EPSFCN, DIAG, MODE, FACTOR, NPRINT, INFO, NFEV,&
	NJEV, R, LR, QTF, WA1, WA2, WA3, WA4)
	!***BEGIN PROLOGUE  SNSQ
	!***PURPOSE  Find a zero of a system of a N nonlinear functions in N
	!            variables by a modification of the Powell hybrid method.
	!***LIBRARY   SLATEC
	!***CATEGORY  F2A
	!***TYPE      SINGLE PRECISION (SNSQ-S, DNSQ-D)
	!***KEYWORDS  NONLINEAR SQUARE SYSTEM, POWELL HYBRID METHOD, ZEROS
	!***AUTHOR  Hiebert, K. L., (SNLA)
	!***DESCRIPTION
	!
	! 1. Purpose.
	!
	!       The purpose of SNSQ is to find a zero of a system of N non-
	!       linear functions in N variables by a modification of the Powell
	!       hybrid method.  The user must provide a subroutine which calcu-
	!       lates the functions.  The user has the option of either to
	!       provide a subroutine which calculates the Jacobian or to let the
	!       code calculate it by a forward-difference approximation.
	!       This code is the combination of the MINPACK codes (Argonne)
	!       HYBRD and HYBRDJ.
	!
	!
	! 2. Subroutine and Type Statements.
	!
	!       SUBROUTINE SNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,
	!      *                 ML,MU,EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,
	!      *                 NJEV,R,LR,QTF,WA1,WA2,WA3,WA4)
	!       INTEGER IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,NJEV,LR
	!       REAL XTOL,EPSFCN,FACTOR
	!       REAL X(N),FVEC(N),DIAG(N),FJAC(LDFJAC,N),R(LR),QTF(N),
	!      *     WA1(N),WA2(N),WA3(N),WA4(N)
	!       EXTERNAL FCN,JAC
	!
	!
	! 3. Parameters.
	!
	!       Parameters designated as input parameters must be specified on
	!       entry to SNSQ and are not changed on exit, while parameters
	!       designated as output parameters need not be specified on entry
	!       and are set to appropriate values on exit from SNSQ.
	!
	!       FCN is the name of the user-supplied subroutine which calculates
	!         the functions.  FCN must be declared in an EXTERNAL statement
	!         in the user calling program, and should be written as follows.
	!
	!         SUBROUTINE FCN(N,X,FVEC,IFLAG)
	!         INTEGER N,IFLAG
	!         REAL X(N),FVEC(N)
	!         ----------
	!         Calculate the functions at X and
	!         return this vector in FVEC.
	!         ----------
	!         RETURN
	!         END
	!
	!         The value of IFLAG should not be changed by FCN unless the
	!         user wants to terminate execution of SNSQ.  In this case, set
	!         IFLAG to a negative integer.
	!
	!       JAC is the name of the user-supplied subroutine which calculates
	!         the Jacobian.  If IOPT=1, then JAC must be declared in an
	!         EXTERNAL statement in the user calling program, and should be
	!         written as follows.
	!
	!         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
	!         INTEGER N,LDFJAC,IFLAG
	!         REAL X(N),FVEC(N),FJAC(LDFJAC,N)
	!         ----------
	!         Calculate the Jacobian at X and return this
	!         matrix in FJAC.  FVEC contains the function
	!         values at X and should not be altered.
	!         ----------
	!         RETURN
	!         END
	!
	!         The value of IFLAG should not be changed by JAC unless the
	!         user wants to terminate execution of SNSQ.  In this case, set
	!         IFLAG to a negative integer.
	!
	!         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
	!
	!       IOPT is an input variable which specifies how the Jacobian will
	!         be calculated.  If IOPT=1, then the user must supply the
	!         Jacobian through the subroutine JAC.  If IOPT=2, then the
	!         code will approximate the Jacobian by forward-differencing.
	!
	!       N is a positive integer input variable set to the number of
	!         functions and variables.
	!
	!       X is an array of length N.  On input, X must contain an initial
	!         estimate of the solution vector.  On output, X contains the
	!         final estimate of the solution vector.
	!
	!       FVEC is an output array of length N which contains the functions
	!         evaluated at the output X.
	!
	!       FJAC is an output N by N array which contains the orthogonal
	!         matrix Q produced by the QR factorization of the final approx-
	!         imate Jacobian.
	!
	!       LDFJAC is a positive integer input variable not less than N
	!         which specifies the leading dimension of the array FJAC.
	!
	!       XTOL is a non-negative input variable.  Termination occurs when
	!         the relative error between two consecutive iterates is at most
	!         XTOL.  Therefore, XTOL measures the relative error desired in
	!         the approximate solution.  Section 4 contains more details
	!         about XTOL.
	!
	!       MAXFEV is a positive integer input variable.  Termination occurs
	!         when the number of calls to FCN is at least MAXFEV by the end
	!         of an iteration.
	!
	!       ML is a non-negative integer input variable which specifies the
	!         number of subdiagonals within the band of the Jacobian matrix.
	!         If the Jacobian is not banded or IOPT=1, set ML to at
	!         least N - 1.
	!
	!       MU is a non-negative integer input variable which specifies the
	!         number of superdiagonals within the band of the Jacobian
	!         matrix.  If the Jacobian is not banded or IOPT=1, set MU to at
	!         least N - 1.
	!
	!       EPSFCN is an input variable used in determining a suitable step
	!         for the forward-difference approximation.  This approximation
	!         assumes that the relative errors in the functions are of the
	!         order of EPSFCN.  If EPSFCN is less than the machine preci-
	!         sion, it is assumed that the relative errors in the functions
	!         are of the order of the machine precision.  If IOPT=1, then
	!         EPSFCN can be ignored (treat it as a dummy argument).
	!
	!       DIAG is an array of length N.  If MODE = 1 (see below), DIAG is
	!         internally set.  If MODE = 2, DIAG must contain positive
	!         entries that serve as implicit (multiplicative) scale factors
	!         for the variables.
	!
	!       MODE is an integer input variable.  If MODE = 1, the variables
	!         will be scaled internally.  If MODE = 2, the scaling is speci-
	!         fied by the input DIAG.  Other values of MODE are equivalent
	!         to MODE = 1.
	!
	!       FACTOR is a positive input variable used in determining the ini-
	!         tial step bound.  This bound is set to the product of FACTOR
	!         and the Euclidean norm of DIAG*X if nonzero, or else to FACTOR
	!         itself.  In most cases FACTOR should lie in the interval
	!         (.1,100.).  100. is a generally recommended value.
	!
	!       NPRINT is an integer input variable that enables controlled
	!         printing of iterates if it is positive.  In this case, FCN is
	!         called with IFLAG = 0 at the beginning of the first iteration
	!         and every NPRINT iteration thereafter and immediately prior
	!         to return, with X and FVEC available for printing. Appropriate
	!         print statements must be added to FCN(see example).  If NPRINT
	!         is not positive, no special calls of FCN with IFLAG = 0 are
	!         made.
	!
	!       INFO is an integer output variable.  If the user has terminated
	!         execution, INFO is set to the (negative) value of IFLAG.  See
	!         description of FCN and JAC. Otherwise, INFO is set as follows.
	!
	!         INFO = 0  improper input parameters.
	!
	!         INFO = 1  relative error between two consecutive iterates is
	!                   at most XTOL.
	!
	!         INFO = 2  number of calls to FCN has reached or exceeded
	!                   MAXFEV.
	!
	!         INFO = 3  XTOL is too small.  No further improvement in the
	!                   approximate solution X is possible.
	!
	!         INFO = 4  iteration is not making good progress, as measured
	!                   by the improvement from the last five Jacobian eval-
	!                   uations.
	!
	!         INFO = 5  iteration is not making good progress, as measured
	!                   by the improvement from the last ten iterations.
	!
	!         Sections 4 and 5 contain more details about INFO.
	!
	!       NFEV is an integer output variable set to the number of calls to
	!         FCN.
	!
	!       NJEV is an integer output variable set to the number of calls to
	!         JAC. (If IOPT=2, then NJEV is set to zero.)
	!
	!       R is an output array of length LR which contains the upper
	!         triangular matrix produced by the QR factorization of the
	!         final approximate Jacobian, stored rowwise.
	!
	!       LR is a positive integer input variable not less than
	!         (N*(N+1))/2.
	!
	!       QTF is an output array of length N which contains the vector
	!         (Q TRANSPOSE)*FVEC.
	!
	!       WA1, WA2, WA3, and WA4 are work arrays of length N.
	!
	!
	! 4. Successful Completion.
	!
	!       The accuracy of SNSQ is controlled by the convergence parameter
	!       XTOL.  This parameter is used in a test which makes a comparison
	!       between the approximation X and a solution XSOL.  SNSQ termi-
	!       nates when the test is satisfied.  If the convergence parameter
	!       is less than the machine precision (as defined by the function
	!       R1MACH(4)), then SNSQ only attempts to satisfy the test
	!       defined by the machine precision.  Further progress is not
	!       usually possible.
	!
	!       The test assumes that the functions are reasonably well behaved,
	!       and, if the Jacobian is supplied by the user, that the functions
	!       and the Jacobian are coded consistently.  If these conditions
	!       are not satisfied, then SNSQ may incorrectly indicate conver-
	!       gence.  The coding of the Jacobian can be checked by the
	!       subroutine CHKDER. If the Jacobian is coded correctly or IOPT=2,
	!       then the validity of the answer can be checked, for example, by
	!       rerunning SNSQ with a tighter tolerance.
	!
	!       Convergence Test.  If ENORM(Z) denotes the Euclidean norm of a
	!         vector Z and D is the diagonal matrix whose entries are
	!         defined by the array DIAG, then this test attempts to guaran-
	!         tee that
	!
	!               ENORM(D*(X-XSOL)) .LE. XTOL*ENORM(D*XSOL).
	!
	!         If this condition is satisfied with XTOL = 10**(-K), then the
	!         larger components of D*X have K significant decimal digits and
	!         INFO is set to 1.  There is a danger that the smaller compo-
	!         nents of D*X may have large relative errors, but the fast rate
	!         of convergence of SNSQ usually avoids this possibility.
	!         Unless high precision solutions are required, the recommended
	!         value for XTOL is the square root of the machine precision.
	!
	!
	! 5. Unsuccessful Completion.
	!
	!       Unsuccessful termination of SNSQ can be due to improper input
	!       parameters, arithmetic interrupts, an excessive number of func-
	!       tion evaluations, or lack of good progress.
	!
	!       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1,
	!         or IOPT .GT. 2, or N .LE. 0, or LDFJAC .LT. N, or
	!         XTOL .LT. 0.E0, or MAXFEV .LE. 0, or ML .LT. 0, or MU .LT. 0,
	!         or FACTOR .LE. 0.E0, or LR .LT. (N*(N+1))/2.
	!
	!       Arithmetic Interrupts.  If these interrupts occur in the FCN
	!         subroutine during an early stage of the computation, they may
	!         be caused by an unacceptable choice of X by SNSQ.  In this
	!         case, it may be possible to remedy the situation by rerunning
	!         SNSQ with a smaller value of FACTOR.
	!
	!       Excessive Number of Function Evaluations.  A reasonable value
	!         for MAXFEV is 100*(N+1) for IOPT=1 and 200*(N+1) for IOPT=2.
	!         If the number of calls to FCN reaches MAXFEV, then this
	!         indicates that the routine is converging very slowly as
	!         measured by the progress of FVEC, and INFO is set to 2.  This
	!         situation should be unusual because, as indicated below, lack
	!         of good progress is usually diagnosed earlier by SNSQ,
	!         causing termination with INFO = 4 or INFO = 5.
	!
	!       Lack of Good Progress.  SNSQ searches for a zero of the system
	!         by minimizing the sum of the squares of the functions.  In so
	!         doing, it can become trapped in a region where the minimum
	!         does not correspond to a zero of the system and, in this situ-
	!         ation, the iteration eventually fails to make good progress.
	!         In particular, this will happen if the system does not have a
	!         zero.  If the system has a zero, rerunning SNSQ from a dif-
	!         ferent starting point may be helpful.
	!
	!
	! 6. Characteristics of the Algorithm.
	!
	!       SNSQ is a modification of the Powell hybrid method.  Two of its
	!       main characteristics involve the choice of the correction as a
	!       convex combination of the Newton and scaled gradient directions,
	!       and the updating of the Jacobian by the rank-1 method of Broy-
	!       den.  The choice of the correction guarantees (under reasonable
	!       conditions) global convergence for starting points far from the
	!       solution and a fast rate of convergence.  The Jacobian is
	!       calculated at the starting point by either the user-supplied
	!       subroutine or a forward-difference approximation, but it is not
	!       recalculated until the rank-1 method fails to produce satis-
	!       factory progress.
	!
	!       Timing.  The time required by SNSQ to solve a given problem
	!         depends on N, the behavior of the functions, the accuracy
	!         requested, and the starting point.  The number of arithmetic
	!         operations needed by SNSQ is about 11.5*(N**2) to process
	!         each evaluation of the functions (call to FCN) and 1.3*(N**3)
	!         to process each evaluation of the Jacobian (call to JAC,
	!         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
	!         the timing of SNSQ will be strongly influenced by the time
	!         spent in FCN and JAC.
	!
	!       Storage.  SNSQ requires (3*N**2 + 17*N)/2 single precision
	!         storage locations, in addition to the storage required by the
	!         program.  There are no internally declared storage arrays.
	!
	!
	! 7. Example.
	!
	!       The problem is to determine the values of X(1), X(2), ..., X(9),
	!       which solve the system of tridiagonal equations
	!
	!       (3-2*X(1))*X(1)           -2*X(2)                   = -1
	!               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
	!                                   -X(8) + (3-2*X(9))*X(9) = -1
	! C     **********
	!
	!       PROGRAM TEST
	! C
	! C     Driver for SNSQ example.
	! C
	!       INTEGER J,IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,LR,
	!      *        NWRITE
	!       REAL XTOL,EPSFCN,FACTOR,FNORM
	!       REAL X(9),FVEC(9),DIAG(9),FJAC(9,9),R(45),QTF(9),
	!      *     WA1(9),WA2(9),WA3(9),WA4(9)
	!       REAL ENORM,R1MACH
	!       EXTERNAL FCN
	!       DATA NWRITE /6/
	! C
	!       IOPT = 2
	!       N = 9
	! C
	! C     The following starting values provide a rough solution.
	! C
	!       DO 10 J = 1, 9
	!          X(J) = -1.E0
	!    10    CONTINUE
	! C
	!       LDFJAC = 9
	!       LR = 45
	! C
	! C     Set XTOL to the square root of the machine precision.
	! C     Unless high precision solutions are required,
	! C     this is the recommended setting.
	! C
	!       XTOL = SQRT(R1MACH(4))
	! C
	!       MAXFEV = 2000
	!       ML = 1
	!       MU = 1
	!       EPSFCN = 0.E0
	!       MODE = 2
	!       DO 20 J = 1, 9
	!          DIAG(J) = 1.E0
	!    20    CONTINUE
	!       FACTOR = 1.E2
	!       NPRINT = 0
	! C
	!       CALL SNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,ML,MU,
	!      *           EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
	!      *           R,LR,QTF,WA1,WA2,WA3,WA4)
	!       FNORM = ENORM(N,FVEC)
	!       WRITE (NWRITE,1000) FNORM,NFEV,INFO,(X(J),J=1,N)
	!       STOP
	!  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
	!      *        5X,' NUMBER OF FUNCTION EVALUATIONS',I10 //
	!      *        5X,' EXIT PARAMETER',16X,I10 //
	!      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
	!       END
	!       SUBROUTINE FCN(N,X,FVEC,IFLAG)
	!       INTEGER N,IFLAG
	!       REAL X(N),FVEC(N)
	!       INTEGER K
	!       REAL ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
	!       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
	! C
	!       IF (IFLAG .NE. 0) GO TO 5
	! C
	! C     Insert print statements here when NPRINT is positive.
	! C
	!       RETURN
	!     5 CONTINUE
	!       DO 10 K = 1, N
	!          TEMP = (THREE - TWO*X(K))*X(K)
	!          TEMP1 = ZERO
	!          IF (K .NE. 1) TEMP1 = X(K-1)
	!          TEMP2 = ZERO
	!          IF (K .NE. N) TEMP2 = X(K+1)
	!          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
	!    10    CONTINUE
	!       RETURN
	!       END
	!
	!       Results obtained with different compilers or machines
	!       may be slightly different.
	!
	!       FINAL L2 NORM OF THE RESIDUALS  0.1192636E-07
	!
	!       NUMBER OF FUNCTION EVALUATIONS        14
	!
	!       EXIT PARAMETER                         1
	!
	!       FINAL APPROXIMATE SOLUTION
	!
	!       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
	!       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
	!       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
	!
	!***REFERENCES  M. J. D. Powell, A hybrid method for nonlinear equa-
	!                 tions. In Numerical Methods for Nonlinear Algebraic
	!                 Equations, P. Rabinowitz, Editor.  Gordon and Breach,
	!                 1988.
	!***ROUTINES CALLED  DOGLEG, ENORM, FDJAC1, QFORM, QRFAC, R1MACH,
	!                    R1MPYQ, R1UPDT, XERMSG
	!***REVISION HISTORY  (YYMMDD)
	!   800301  DATE WRITTEN
	!   890531  Changed all specific intrinsics to generic.  (WRB)
	!   890831  Modified array declarations.  (WRB)
	!   890831  REVISION DATE from Version 3.2
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
	!   920501  Reformatted the REFERENCES section.  (WRB)
	!***END PROLOGUE  SNSQ
	INTEGER IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,LR,NJEV
	REAL XTOL,EPSFCN,FACTOR
	REAL X(*),FVEC(*),DIAG(*),FJAC(LDFJAC,*),R(LR),QTF(*),WA1(*),&
	WA2(*),WA3(*),WA4(*)
	EXTERNAL FCN
	INTEGER I,IFLAG,ITER,J,JM1,L,NCFAIL,NCSUC,NSLOW1,NSLOW2
	INTEGER IWA(1)
	LOGICAL JEVAL,SING
	logical :: lgoto_240_16 = .false. ! add by @creaqi SNSQ modify_goto_in_if
	logical :: lgoto_32_5 = .false. ! add by @creaqi SNSQ modify_goto_in_if
	REAL ACTRED,DELTA,EPSMCH,FNORM,FNORM1,ONE,PNORM,PRERED,P1,P5,&
	P001,P0001,RATIO,SUM,TEMP,XNORM,ZERO
	REAL R1MACH,ENORM
	SAVE ONE, P1, P5, P001, P0001, ZERO
	DATA ONE,P1,P5,P001,P0001,ZERO&
	/1.0E0,1.0E-1,5.0E-1,1.0E-3,1.0E-4,0.0E0/
	!
	!***FIRST EXECUTABLE STATEMENT  SNSQ
	EPSMCH = R1MACH(4)
	!
	INFO = 0
	IFLAG = 0
	NFEV = 0
	NJEV = 0
	!
	!     CHECK THE INPUT PARAMETERS FOR ERRORS.
	!
	if(.not.(IOPT .LT. 1 .OR. IOPT .GT. 2 .OR.&
		N .LE. 0 .OR. XTOL .LT. ZERO .OR. MAXFEV .LE. 0&
		.OR. ML .LT. 0 .OR. MU .LT. 0 .OR. FACTOR .LE. ZERO&
		.OR. LDFJAC .LT. N .OR. LR .LT. (N*(N + 1))/2))then ! add by @creaqi continuation
		if(.not. (MODE .NE. 2)) then ! add by @creaqi goto 20 in fun modify_goto_pure
			label10:      do  J = 1, N
				if(.not.(DIAG(J) .LE. ZERO)) then ! add by @creaqi flag 3
					lgoto300_0=.true. ! add by @creaqi flag 3
					exit ! add by @creaqi flag 3
				endif ! add by @creaqi flag 3
			enddo label10! 10 continue flag 2   label10
		endif !add by @creaqi label 20 modify_goto_pure
		20 CONTINUE
		!
		!     EVALUATE THE FUNCTION AT THE STARTING POINT
		!     AND CALCULATE ITS NORM.
		!
		IFLAG = 1
		CALL FCN(N,X,FVEC,IFLAG)
		if(.not.lgoto300_0) then !flag 4 i_el, in_el
		endif ! add by @creaqi flag 4 i_el, in_el
		NFEV = 1
		if(.not. (IFLAG .LT. 0)) then ! add by @creaqi goto 300 in fun modify_goto_pure
			FNORM = ENORM(N,FVEC)
			!
			!     INITIALIZE ITERATION COUNTER AND MONITORS.
			!
			ITER = 1
			NCSUC = 0
			NCFAIL = 0
			NSLOW1 = 0
			NSLOW2 = 0
			!
			!     BEGINNING OF THE OUTER LOOP.
			!
			label44:      do ! insert do to replace label 30, add by @creaqi 2020-02-18 11:30:05.979775
				30 CONTINUE
				JEVAL = .TRUE.
				!
				!        CALCULATE THE JACOBIAN MATRIX.
				!
				if(.not. (IOPT .EQ. 2)) then ! add by @creaqi goto 31 in fun modify_goto_pure
					!
					!        USER SUPPLIES JACOBIAN
					!
					CALL JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
					NJEV = NJEV+1
					lgoto_32_5 = .true. !            GO TO 32
					!
					!        CODE APPROXIMATES THE JACOBIAN
					!
				endif !add by @creaqi label 31 modify_goto_pure
				if(.not.lgoto_32_5) then ! start second start of goto 32 modify_goto_related_if
					31       IFLAG = 2
					CALL FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,WA1,&
					WA2)
					NFEV = NFEV + MIN0(ML+MU+1,N)
					!
				endif ! add by @creaqi max lgoto_32_5 end of goto 32 modify_goto_related_if
				32    IF (IFLAG .LT. 0) exit ! add by @creaqi break the loop goto 300 in SNSQ with len equal 1
				!
				!        COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.
				!
				CALL QRFAC(N,N,FJAC,LDFJAC,.FALSE.,IWA,1,WA1,WA2,WA3)
				!
				!        ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING
				!        TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.
				!
				if(.not. (ITER .NE. 1)) then ! add by @creaqi goto 70 in fun modify_goto_pure
					if(.not. (MODE .EQ. 2)) then ! add by @creaqi goto 50 in fun modify_goto_pure
						DO J = 1, N! add by @creaqi do label 40
							DIAG(J) = WA2(J)
							IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE
						enddo !add by @creaqi 40
						40       CONTINUE
					endif !add by @creaqi label 50 modify_goto_pure
					50    CONTINUE
					!
					!        ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED X
					!        AND INITIALIZE THE STEP BOUND DELTA.
					!
					DO J = 1, N! add by @creaqi do label 60
						WA3(J) = DIAG(J)*X(J)
					enddo !add by @creaqi 60
					60       CONTINUE
					XNORM = ENORM(N,WA3)
					DELTA = FACTOR*XNORM
					IF (DELTA .EQ. ZERO) DELTA = FACTOR
				endif !add by @creaqi label 70 modify_goto_pure
				70    CONTINUE
				!
				!        FORM (Q TRANSPOSE)*FVEC AND STORE IN QTF.
				!
				DO I = 1, N! add by @creaqi do label 80
					QTF(I) = FVEC(I)
				enddo !add by @creaqi 80
				80       CONTINUE
				DO 120 J = 1, N
					if(.not. (FJAC(J,J) .EQ. ZERO)) then ! add by @creaqi goto 110 in fun modify_goto_pure
						SUM = ZERO
						DO I = J, N! add by @creaqi do label 90
							SUM = SUM + FJAC(I,J)*QTF(I)
						enddo !add by @creaqi 90
						90          CONTINUE
						TEMP = -SUM/FJAC(J,J)
						DO I = J, N! add by @creaqi do label 100
							QTF(I) = QTF(I) + FJAC(I,J)*TEMP
						enddo !add by @creaqi 100
						100          CONTINUE
					endif !add by @creaqi label 110 modify_goto_pure
					110       CONTINUE
					120       CONTINUE
					!
					!        COPY THE TRIANGULAR FACTOR OF THE QR FACTORIZATION INTO R.
					!
					SING = .FALSE.
					DO 150 J = 1, N
						L = J
						JM1 = J - 1
						if(.not. (JM1 .LT. 1)) then ! add by @creaqi goto 140 in fun modify_goto_pure
							DO I = 1, JM1! add by @creaqi do label 130
								R(L) = FJAC(I,J)
								L = L + N - I
							enddo !add by @creaqi 130
							130          CONTINUE
						endif !add by @creaqi label 140 modify_goto_pure
						140       CONTINUE
						R(L) = WA1(J)
						IF (WA1(J) .EQ. ZERO) SING = .TRUE.
						150       CONTINUE
						!
						!        ACCUMULATE THE ORTHOGONAL FACTOR IN FJAC.
						!
						CALL QFORM(N,N,FJAC,LDFJAC,WA1)
						!
						!        RESCALE IF NECESSARY.
						!
						if(.not. (MODE .EQ. 2)) then ! add by @creaqi goto 170 in fun modify_goto_pure
							DO J = 1, N! add by @creaqi do label 160
								DIAG(J) = MAX(DIAG(J),WA2(J))
							enddo !add by @creaqi 160
							160       CONTINUE
						endif !add by @creaqi label 170 modify_goto_pure
						170    CONTINUE
						!
						!        BEGINNING OF THE INNER LOOP.
						!
						do ! insert do to replace label 180, add by @creaqi 2020-02-18 11:30:05.978721
							180    CONTINUE
							!
							!           IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.
							!
							if(.not. (NPRINT .LE. 0)) then ! add by @creaqi goto 190 in fun modify_goto_pure
								IFLAG = 0
								IF (MOD(ITER-1,NPRINT) .EQ. 0) CALL FCN(N,X,FVEC,IFLAG)
								if(.not.(IFLAG .LT. 0)) then ! add by @creaqi flag 3
									lgoto300_1=.true. ! add by @creaqi flag 3
									exit ! add by @creaqi flag 3
								endif ! add by @creaqi flag 3
							endif !add by @creaqi label 190 modify_goto_pure
							190       CONTINUE
							!
							!           DETERMINE THE DIRECTION P.
							!
							CALL DOGLEG(N,R,LR,DIAG,QTF,DELTA,WA1,WA2,WA3)
							!
							!           STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.
							!
							DO J = 1, N! add by @creaqi do label 200
								WA1(J) = -WA1(J)
								WA2(J) = X(J) + WA1(J)
								WA3(J) = DIAG(J)*WA1(J)
							enddo !add by @creaqi 200
							200          CONTINUE
							PNORM = ENORM(N,WA3)
							!
							!           ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.
							!
							IF (ITER .EQ. 1) DELTA = MIN(DELTA,PNORM)
							!
							!           EVALUATE THE FUNCTION AT X + P AND CALCULATE ITS NORM.
							!
							IFLAG = 1
							CALL FCN(N,WA2,WA4,IFLAG)
							NFEV = NFEV + 1
							if(.not.(IFLAG .LT. 0)) then ! add by @creaqi flag 3
								lgoto300_2=.true. ! add by @creaqi flag 3
								exit ! add by @creaqi flag 3
							endif ! add by @creaqi flag 3
							FNORM1 = ENORM(N,WA4)
							!
							!           COMPUTE THE SCALED ACTUAL REDUCTION.
							!
							ACTRED = -ONE
							IF (FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
							!
							!           COMPUTE THE SCALED PREDICTED REDUCTION.
							!
							L = 1
							DO I = 1, N! add by @creaqi do label 220
								SUM = ZERO
								DO J = I, N! add by @creaqi do label 210
									SUM = SUM + R(L)*WA1(J)
									L = L + 1
								enddo !add by @creaqi 210
								210             CONTINUE
								WA3(I) = QTF(I) + SUM
							enddo !add by @creaqi 220
							220          CONTINUE
							TEMP = ENORM(N,WA3)
							PRERED = ZERO
							IF (TEMP .LT. FNORM) PRERED = ONE - (TEMP/FNORM)**2
							!
							!           COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED
							!           REDUCTION.
							!
							RATIO = ZERO
							IF (PRERED .GT. ZERO) RATIO = ACTRED/PRERED
							!
							!           UPDATE THE STEP BOUND.
							!
							if(.not. (RATIO .GE. P1)) then ! add by @creaqi goto 230 in fun modify_goto_pure
								NCSUC = 0
								NCFAIL = NCFAIL + 1
								DELTA = P5*DELTA
								lgoto_240_16 = .true. !               GO TO 240
							endif !add by @creaqi label 230 modify_goto_pure
							if(.not.lgoto_240_16) then ! start second start of goto 240 modify_goto_related_if
								230       CONTINUE
								NCFAIL = 0
								NCSUC = NCSUC + 1
									IF (RATIO .GE. P5 .OR. NCSUC .GT. 1)&
								DELTA = MAX(DELTA,PNORM/P5)
								IF (ABS(RATIO-ONE) .LE. P1) DELTA = PNORM/P5
							endif ! add by @creaqi max lgoto_240_16 end of goto 240 modify_goto_related_if
							240       CONTINUE
							!
							!           TEST FOR SUCCESSFUL ITERATION.
							!
							if(.not. (RATIO .LT. P0001)) then ! add by @creaqi goto 260 in fun modify_goto_pure
								!
								!           SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.
								!
								DO J = 1, N! add by @creaqi do label 250
									X(J) = WA2(J)
									WA2(J) = DIAG(J)*X(J)
									FVEC(J) = WA4(J)
								enddo !add by @creaqi 250
								250          CONTINUE
								XNORM = ENORM(N,WA2)
								FNORM = FNORM1
								ITER = ITER + 1
							endif !add by @creaqi label 260 modify_goto_pure
							260       CONTINUE
							!
							!           DETERMINE THE PROGRESS OF THE ITERATION.
							!
							NSLOW1 = NSLOW1 + 1
							IF (ACTRED .GE. P001) NSLOW1 = 0
							IF (JEVAL) NSLOW2 = NSLOW2 + 1
							IF (ACTRED .GE. P1) NSLOW2 = 0
							!
							!           TEST FOR CONVERGENCE.
							!
							IF (DELTA .LE. XTOL*XNORM .OR. FNORM .EQ. ZERO) INFO = 1
							if(.not.(INFO .NE. 0)) then ! add by @creaqi flag 3
								lgoto300_3=.true. ! add by @creaqi flag 3
								exit ! add by @creaqi flag 3
							endif ! add by @creaqi flag 3
							!
							!           TESTS FOR TERMINATION AND STRINGENT TOLERANCES.
							!
							IF (NFEV .GE. MAXFEV) INFO = 2
							IF (P1*MAX(P1*DELTA,PNORM) .LE. EPSMCH*XNORM) INFO = 3
							IF (NSLOW2 .EQ. 5) INFO = 4
							IF (NSLOW1 .EQ. 10) INFO = 5
							if(.not.(INFO .NE. 0)) then ! add by @creaqi flag 3
								lgoto300_4=.true. ! add by @creaqi flag 3
								exit ! add by @creaqi flag 3
							endif ! add by @creaqi flag 3
							!
							!           CRITERION FOR RECALCULATING JACOBIAN
							!
							IF (NCFAIL .EQ. 2) exit ! add by @creaqi break the loop goto 290 in SNSQ with len equal 1
							!
							!           CALCULATE THE RANK ONE MODIFICATION TO THE JACOBIAN
							!           AND UPDATE QTF IF NECESSARY.
							!
							DO J = 1, N! add by @creaqi do label 280
								SUM = ZERO
								DO I = 1, N! add by @creaqi do label 270
									SUM = SUM + FJAC(I,J)*WA4(I)
								enddo !add by @creaqi 270
								270             CONTINUE
								WA2(J) = (SUM - WA3(J))/PNORM
								WA1(J) = DIAG(J)*((DIAG(J)*WA1(J))/PNORM)
								IF (RATIO .GE. P0001) QTF(J) = SUM
							enddo !add by @creaqi 280
							280          CONTINUE
							!
							!           COMPUTE THE QR FACTORIZATION OF THE UPDATED JACOBIAN.
							!
							CALL R1UPDT(N,N,R,LR,WA1,WA2,WA3,SING)
							CALL R1MPYQ(N,N,FJAC,LDFJAC,WA2,WA3)
							CALL R1MPYQ(1,N,QTF,1,WA2,WA3)
							!
							!           END OF THE INNER LOOP.
							!
							JEVAL = .FALSE.
						enddo ! insert enddo to replace goto [180, add by @creaqi 2020-02-18 11:30:05.978721
						! goto 180 add by @creaqi 2020-02-18 11:30:05.978721
						290    CONTINUE
						!
						!        END OF THE OUTER LOOP.
						!
					enddo label44! 44 continue flag 2   label44
					! goto 30 add by @creaqi 2020-02-18 11:30:05.979775
				endif ! add by @creaqi continuation
			endif !add by @creaqi label 300 modify_goto_pure
			300 CONTINUE
			!
			!     TERMINATION, EITHER NORMAL OR USER IMPOSED.
			!
			IF (IFLAG .LT. 0) INFO = IFLAG
			IFLAG = 0
			IF (NPRINT .GT. 0) CALL FCN(N,X,FVEC,IFLAG)
			IF (INFO .LT. 0) CALL XERMSG ('SLATEC', 'SNSQ',&
				'EXECUTION TERMINATED BECAUSE USER SET IFLAG NEGATIVE.', 1, 1)
			IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'SNSQ',&
				'INVALID INPUT PARAMETER.', 2, 1)
			IF (INFO .EQ. 2) CALL XERMSG ('SLATEC', 'SNSQ',&
				'TOO MANY FUNCTION EVALUATIONS.', 9, 1)
			IF (INFO .EQ. 3) CALL XERMSG ('SLATEC', 'SNSQ',&
				'XTOL TOO SMALL. NO FURTHER IMPROVEMENT POSSIBLE.', 3, 1)
			IF (INFO .GT. 4) CALL XERMSG ('SLATEC', 'SNSQ',&
				'ITERATION NOT MAKING GOOD PROGRESS.', 1, 1)
			RETURN
			!
			!     LAST CARD OF SUBROUTINE SNSQ.
			!
END
		!DECK DOGLEG
SUBROUTINE DOGLEG (N, R, LR, DIAG, QTB, DELTA, X, WA1, WA2)
	!***BEGIN PROLOGUE  DOGLEG
	!***SUBSIDIARY
	!***PURPOSE  Subsidiary to SNSQ and SNSQE
	!***LIBRARY   SLATEC
	!***TYPE      SINGLE PRECISION (DOGLEG-S, DDOGLG-D)
	!***AUTHOR  (UNKNOWN)
	!***DESCRIPTION
	!
	!     Given an M by N matrix A, an N by N nonsingular DIAGONAL
	!     matrix D, an M-vector B, and a positive number DELTA, the
	!     problem is to determine the convex combination X of the
	!     Gauss-Newton and scaled gradient directions that minimizes
	!     (A*X - B) in the least squares sense, subject to the
	!     restriction that the Euclidean norm of D*X be at most DELTA.
	!
	!     This subroutine completes the solution of the problem
	!     if it is provided with the necessary information from the
	!     QR factorization of A. That is, if A = Q*R, where Q has
	!     orthogonal columns and R is an upper triangular matrix,
	!     then DOGLEG expects the full upper triangle of R and
	!     the first N components of (Q TRANSPOSE)*B.
	!
	!     The subroutine statement is
	!
	!       SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)
	!
	!     where
	!
	!       N is a positive integer input variable set to the order of R.
	!
	!       R is an input array of length LR which must contain the upper
	!         triangular matrix R stored by rows.
	!
	!       LR is a positive integer input variable not less than
	!         (N*(N+1))/2.
	!
	!       DIAG is an input array of length N which must contain the
	!         diagonal elements of the matrix D.
	!
	!       QTB is an input array of length N which must contain the first
	!         N elements of the vector (Q TRANSPOSE)*B.
	!
	!       DELTA is a positive input variable which specifies an upper
	!         bound on the Euclidean norm of D*X.
	!
	!       X is an output array of length N which contains the desired
	!         convex combination of the Gauss-Newton direction and the
	!         scaled gradient direction.
	!
	!       WA1 and WA2 are work arrays of length N.
	!
	!***SEE ALSO  SNSQ, SNSQE
	!***ROUTINES CALLED  ENORM, R1MACH
	!***REVISION HISTORY  (YYMMDD)
	!   800301  DATE WRITTEN
	!   890531  Changed all specific intrinsics to generic.  (WRB)
	!   890831  Modified array declarations.  (WRB)
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900326  Removed duplicate information from DESCRIPTION section.
	!           (WRB)
	!   900328  Added TYPE section.  (WRB)
	!***END PROLOGUE  DOGLEG
	INTEGER N,LR
	REAL DELTA
	REAL R(LR),DIAG(*),QTB(*),X(*),WA1(*),WA2(*)
	INTEGER I,J,JJ,JP1,K,L
	REAL ALPHA,BNORM,EPSMCH,GNORM,ONE,QNORM,SGNORM,SUM,TEMP,ZERO
	REAL R1MACH,ENORM
	SAVE ONE, ZERO
	DATA ONE,ZERO /1.0E0,0.0E0/
	!***FIRST EXECUTABLE STATEMENT  DOGLEG
	EPSMCH = R1MACH(4)
	!
	!     FIRST, CALCULATE THE GAUSS-NEWTON DIRECTION.
	!
	JJ = (N*(N + 1))/2 + 1
	DO 50 K = 1, N
		J = N - K + 1
		JP1 = J + 1
		JJ = JJ - K
		L = JJ + 1
		SUM = ZERO
		if(.not. (N .LT. JP1)) then ! add by @creaqi goto 20 in fun modify_goto_pure
			DO I = JP1, N! add by @creaqi do label 10
				SUM = SUM + R(L)*X(I)
				L = L + 1
			enddo !add by @creaqi 10
			10       CONTINUE
		endif !add by @creaqi label 20 modify_goto_pure
		20    CONTINUE
		TEMP = R(JJ)
		if(.not. (TEMP .NE. ZERO)) then ! add by @creaqi goto 40 in fun modify_goto_pure
			L = J
			DO I = 1, J! add by @creaqi do label 30
				TEMP = MAX(TEMP,ABS(R(L)))
				L = L + N - I
			enddo !add by @creaqi 30
			30       CONTINUE
			TEMP = EPSMCH*TEMP
			IF (TEMP .EQ. ZERO) TEMP = EPSMCH
		endif !add by @creaqi label 40 modify_goto_pure
		40    CONTINUE
		X(J) = (QTB(J) - SUM)/TEMP
		50    CONTINUE
		!
		!     TEST WHETHER THE GAUSS-NEWTON DIRECTION IS ACCEPTABLE.
		!
		DO J = 1, N! add by @creaqi do label 60
			WA1(J) = ZERO
			WA2(J) = DIAG(J)*X(J)
		enddo !add by @creaqi 60
		60    CONTINUE
		QNORM = ENORM(N,WA2)
		if(.not. (QNORM .LE. DELTA)) then ! add by @creaqi goto 140 in fun modify_goto_pure
			!
			!     THE GAUSS-NEWTON DIRECTION IS NOT ACCEPTABLE.
			!     NEXT, CALCULATE THE SCALED GRADIENT DIRECTION.
			!
			L = 1
			DO J = 1, N! add by @creaqi do label 80
				TEMP = QTB(J)
				DO I = J, N! add by @creaqi do label 70
					WA1(I) = WA1(I) + R(L)*TEMP
					L = L + 1
				enddo !add by @creaqi 70
				70       CONTINUE
				WA1(J) = WA1(J)/DIAG(J)
			enddo !add by @creaqi 80
			80    CONTINUE
			!
			!     CALCULATE THE NORM OF THE SCALED GRADIENT DIRECTION,
			!     NORMALIZE, AND RESCALE THE GRADIENT.
			!
			GNORM = ENORM(N,WA1)
			SGNORM = ZERO
			ALPHA = DELTA/QNORM
			if(.not. (GNORM .EQ. ZERO)) then ! add by @creaqi goto 120 in fun modify_goto_pure
				DO J = 1, N! add by @creaqi do label 90
					WA1(J) = (WA1(J)/GNORM)/DIAG(J)
				enddo !add by @creaqi 90
				90    CONTINUE
				!
				!     CALCULATE THE POINT ALONG THE SCALED GRADIENT
				!     AT WHICH THE QUADRATIC IS MINIMIZED.
				!
				L = 1
				DO J = 1, N! add by @creaqi do label 110
					SUM = ZERO
					DO I = J, N! add by @creaqi do label 100
						SUM = SUM + R(L)*WA1(I)
						L = L + 1
					enddo !add by @creaqi 100
					100       CONTINUE
					WA2(J) = SUM
				enddo !add by @creaqi 110
				110    CONTINUE
				TEMP = ENORM(N,WA2)
				SGNORM = (GNORM/TEMP)/TEMP
				!
				!     TEST WHETHER THE SCALED GRADIENT DIRECTION IS ACCEPTABLE.
				!
				ALPHA = ZERO
				if (.not. (SGNORM .GE. DELTA)) then ! add by @creaqi 120 state_same == True
					!
					!     THE SCALED GRADIENT DIRECTION IS NOT ACCEPTABLE.
					!     FINALLY, CALCULATE THE POINT ALONG THE DOGLEG
					!     AT WHICH THE QUADRATIC IS MINIMIZED.
					!
					BNORM = ENORM(N,QTB)
					TEMP = (BNORM/GNORM)*(BNORM/QNORM)*(SGNORM/DELTA)
					TEMP = TEMP - (DELTA/QNORM)*(SGNORM/DELTA)**2&
					+ SQRT((TEMP-(DELTA/QNORM))**2&
					+(ONE-(DELTA/QNORM)**2)*(ONE-(SGNORM/DELTA)**2))
					ALPHA = ((DELTA/QNORM)*(ONE - (SGNORM/DELTA)**2))/TEMP
				endif ! add by @creaqi 120 state_same == True
			endif !add by @creaqi label 120 modify_goto_pure
			120 CONTINUE
			!
			!     FORM APPROPRIATE CONVEX COMBINATION OF THE GAUSS-NEWTON
			!     DIRECTION AND THE SCALED GRADIENT DIRECTION.
			!
			TEMP = (ONE - ALPHA)*MIN(SGNORM,DELTA)
			DO J = 1, N! add by @creaqi do label 130
				X(J) = TEMP*WA1(J) + ALPHA*X(J)
			enddo !add by @creaqi 130
			130    CONTINUE
		endif !add by @creaqi label 140 modify_goto_pure
		140 CONTINUE
		RETURN
		!
		!     LAST CARD OF SUBROUTINE DOGLEG.
		!
END
	!DECK ENORM
	REAL FUNCTION ENORM (N, X)
	!***BEGIN PROLOGUE  ENORM
	!***SUBSIDIARY
	!***PURPOSE  Subsidiary to SNLS1, SNLS1E, SNSQ and SNSQE
	!***LIBRARY   SLATEC
	!***TYPE      SINGLE PRECISION (ENORM-S, DENORM-D)
	logical :: lgoto_120_11 = .false. ! add by @creaqi ENORM modify_goto_in_if
	logical :: lgoto_130_9 = .false. ! add by @creaqi ENORM modify_goto_in_if
	logical :: lgoto_80_7 = .false. ! add by @creaqi ENORM modify_goto_in_if
	logical :: lgoto_50_6 = .false. ! add by @creaqi ENORM modify_goto_in_if
	logical :: lgoto_60_4 = .false. ! add by @creaqi ENORM modify_goto_in_if
	logical :: lgoto_20_3 = .false. ! add by @creaqi ENORM modify_goto_in_if
	!***AUTHOR  (UNKNOWN)
	!***DESCRIPTION
	!
	!     Given an N-vector X, this function calculates the
	!     Euclidean norm of X.
	!
	!     The Euclidean norm is computed by accumulating the sum of
	!     squares in three different sums. The sums of squares for the
	!     small and large components are scaled so that no overflows
	!     occur. Non-destructive underflows are permitted. Underflows
	!     and overflows do not occur in the computation of the unscaled
	!     sum of squares for the intermediate components.
	!     The definitions of small, intermediate and large components
	!     depend on two constants, RDWARF and RGIANT. The main
	!     restrictions on these constants are that RDWARF**2 not
	!     underflow and RGIANT**2 not overflow. The constants
	!     given here are suitable for every known computer.
	!
	!     The function statement is
	!
	!       REAL FUNCTION ENORM(N,X)
	!
	!     where
	!
	!       N is a positive integer input variable.
	!
	!       X is an input array of length N.
	!
	!***SEE ALSO  SNLS1, SNLS1E, SNSQ, SNSQE
	!***ROUTINES CALLED  (NONE)
	!***REVISION HISTORY  (YYMMDD)
	!   800301  DATE WRITTEN
	!   890831  Modified array declarations.  (WRB)
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900326  Removed duplicate information from DESCRIPTION section.
	!           (WRB)
	!   900328  Added TYPE section.  (WRB)
	!***END PROLOGUE  ENORM
	INTEGER N
	REAL X(*)
	INTEGER I
	REAL AGIANT,FLOATN,ONE,RDWARF,RGIANT,S1,S2,S3,XABS,X1MAX,X3MAX,&
	ZERO
	SAVE ONE, ZERO, RDWARF, RGIANT
	DATA ONE,ZERO,RDWARF,RGIANT /1.0E0,0.0E0,3.834E-20,1.304E19/
	!***FIRST EXECUTABLE STATEMENT  ENORM
	S1 = ZERO
	S2 = ZERO
	S3 = ZERO
	X1MAX = ZERO
	X3MAX = ZERO
	FLOATN = N
	AGIANT = RGIANT/FLOATN
	DO 90 I = 1, N
		XABS = ABS(X(I))
		if(.not. (XABS .GT. RDWARF .AND. XABS .LT. AGIANT)) then ! add by @creaqi goto 70 in fun modify_goto_pure
			if(.not. (XABS .LE. RDWARF)) then ! add by @creaqi goto 30 in fun modify_goto_pure
				!
				!              SUM FOR LARGE COMPONENTS.
				!
				if(.not. (XABS .LE. X1MAX)) then ! add by @creaqi goto 10 in fun modify_goto_pure
					S1 = ONE + S1*(X1MAX/XABS)**2
					X1MAX = XABS
					lgoto_20_3 = .true. !                  GO TO 20
				endif !add by @creaqi label 10 modify_goto_pure
				if(.not.lgoto_20_3) then ! start second start of goto 20 modify_goto_related_if
					10          CONTINUE
					S1 = S1 + (XABS/X1MAX)**2
				endif ! add by @creaqi max lgoto_20_3 end of goto 20 modify_goto_related_if
				20          CONTINUE
				lgoto_60_4 = .true. !               GO TO 60
			endif !add by @creaqi label 30 modify_goto_pure
			if(.not.lgoto_60_4) then ! start second start of goto 60 modify_goto_related_if
				30       CONTINUE
				!
				!              SUM FOR SMALL COMPONENTS.
				!
				if(.not. (XABS .LE. X3MAX)) then ! add by @creaqi goto 40 in fun modify_goto_pure
					S3 = ONE + S3*(X3MAX/XABS)**2
					X3MAX = XABS
					lgoto_50_6 = .true. !                  GO TO 50
				endif !add by @creaqi label 40 modify_goto_pure
				if(.not.lgoto_50_6) then ! start second start of goto 50 modify_goto_related_if
					40          CONTINUE
					IF (XABS .NE. ZERO) S3 = S3 + (XABS/X3MAX)**2
				endif ! add by @creaqi max lgoto_50_6 end of goto 50 modify_goto_related_if
				50          CONTINUE
			endif ! add by @creaqi max lgoto_60_4 end of goto 60 modify_goto_related_if
			60       CONTINUE
			lgoto_80_7 = .true. !            GO TO 80
		endif !add by @creaqi label 70 modify_goto_pure
		if(.not.lgoto_80_7) then ! start second start of goto 80 modify_goto_related_if
			70    CONTINUE
			!
			!           SUM FOR INTERMEDIATE COMPONENTS.
			!
			S2 = S2 + XABS**2
		endif ! add by @creaqi max lgoto_80_7 end of goto 80 modify_goto_related_if
		80    CONTINUE
		90    CONTINUE
		!
		!     CALCULATION OF NORM.
		!
		if(.not. (S1 .EQ. ZERO)) then ! add by @creaqi goto 100 in fun modify_goto_pure
			ENORM = X1MAX*SQRT(S1+(S2/X1MAX)/X1MAX)
			lgoto_130_9 = .true. !         GO TO 130
		endif !add by @creaqi label 100 modify_goto_pure
		if(.not.lgoto_130_9) then ! start second start of goto 130 modify_goto_related_if
			100 CONTINUE
			if(.not. (S2 .EQ. ZERO)) then ! add by @creaqi goto 110 in fun modify_goto_pure
					IF (S2 .GE. X3MAX)&
				ENORM = SQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))
					IF (S2 .LT. X3MAX)&
				ENORM = SQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))
				lgoto_120_11 = .true. !            GO TO 120
			endif !add by @creaqi label 110 modify_goto_pure
			if(.not.lgoto_120_11) then ! start second start of goto 120 modify_goto_related_if
				110    CONTINUE
				ENORM = X3MAX*SQRT(S3)
			endif ! add by @creaqi max lgoto_120_11 end of goto 120 modify_goto_related_if
			120    CONTINUE
		endif ! add by @creaqi max lgoto_130_9 end of goto 130 modify_goto_related_if
		130 CONTINUE
		RETURN
		!
		!     LAST CARD OF FUNCTION ENORM.
		!
END
	!DECK FDJAC1
SUBROUTINE FDJAC1 (FCN, N, X, FVEC, FJAC, LDFJAC, IFLAG, ML, MU,&
	EPSFCN, WA1, WA2)
	!***BEGIN PROLOGUE  FDJAC1
	!***SUBSIDIARY
	!***PURPOSE  Subsidiary to SNSQ and SNSQE
	!***LIBRARY   SLATEC
	!***TYPE      SINGLE PRECISION (FDJAC1-S, DFDJC1-D)
	!***AUTHOR  (UNKNOWN)
	!***DESCRIPTION
	!
	!     This subroutine computes a forward-difference approximation
	!     to the N by N Jacobian matrix associated with a specified
	!     problem of N functions in N VARIABLES. If the Jacobian has
	!     a banded form, then function evaluations are saved by only
	!     approximating the nonzero terms.
	!
	!     The subroutine statement is
	!
	!       SUBROUTINE FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,
	!                         WA1,WA2)
	!
	!     where
	!
	!       FCN is the name of the user-supplied subroutine which
	!         calculates the functions. FCN must be declared
	!         in an external statement in the user calling
	!         program, and should be written as follows.
	!
	!         SUBROUTINE FCN(N,X,FVEC,IFLAG)
	!         INTEGER N,IFLAG
	!         REAL X(N),FVEC(N)
	!         ----------
	!         Calculate the functions at X and
	!         return this vector in FVEC.
	!         ----------
	!         RETURN
	!         END
	!
	!         The value of IFLAG should not be changed by FCN unless
	!         the user wants to terminate execution of FDJAC1.
	!         In this case set IFLAG to a negative integer.
	!
	!       N Is a positive integer input variable set to the number
	!         of functions and variables.
	!
	!       X is an input array of length N.
	!
	!       FVEC is an input array of length N which must contain the
	!         functions evaluated at X.
	!
	!       FJAC is an output N by N array which contains the
	!         approximation to the Jacobian matrix evaluated at X.
	!
	!       LDFJAC is a positive integer input variable not less than N
	!         which specifies the leading dimension of the array FJAC.
	!
	!       IFLAG is an integer variable which can be used to terminate
	!         the execution of FDJAC1. See description of FCN.
	!
	!       ML is a nonnegative integer input variable which specifies
	!         the number of subdiagonals within the band of the
	!         Jacobian matrix. If the Jacobian is not banded, set
	!         ML to at least N - 1.
	!
	!       EPSFCN is an input variable used in determining a suitable
	!         step length for the forward-difference approximation. This
	!         approximation assumes that the relative errors in the
	!         functions are of the order of EPSFCN. If EPSFCN is less
	!         than the machine precision, it is assumed that the relative
	!         errors in the functions are of the order of the machine
	!         precision.
	!
	!       MU is a nonnegative integer input variable which specifies
	!         the number of superdiagonals within the band of the
	!         Jacobian matrix. If the Jacobian is not banded, set
	!         MU to at least N - 1.
	!
	!       WA1 and WA2 are work arrays of length N. If ML + MU + 1 is at
	!         least N, then the Jacobian is considered dense, and WA2 is
	!         not referenced.
	!
	!***SEE ALSO  SNSQ, SNSQE
	!***ROUTINES CALLED  R1MACH
	!***REVISION HISTORY  (YYMMDD)
	!   800301  DATE WRITTEN
	!   890531  Changed all specific intrinsics to generic.  (WRB)
	!   890831  Modified array declarations.  (WRB)
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900326  Removed duplicate information from DESCRIPTION section.
	!           (WRB)
	!   900328  Added TYPE section.  (WRB)
	!***END PROLOGUE  FDJAC1
	INTEGER N,LDFJAC,IFLAG,ML,MU
	REAL EPSFCN
	REAL X(*),FVEC(*),FJAC(LDFJAC,*),WA1(*),WA2(*)
	INTEGER I,J,K,MSUM
	REAL EPS,EPSMCH,H,TEMP,ZERO
	REAL R1MACH
	logical :: lgoto_110_2 = .false. ! add by @creaqi FDJAC1 modify_goto_in_if
	SAVE ZERO
	DATA ZERO /0.0E0/
	!***FIRST EXECUTABLE STATEMENT  FDJAC1
	EPSMCH = R1MACH(4)
	!
	EPS = SQRT(MAX(EPSFCN,EPSMCH))
	MSUM = ML + MU + 1
	if(.not. (MSUM .LT. N)) then ! add by @creaqi goto 40 in fun modify_goto_pure
		!
		!        COMPUTATION OF DENSE APPROXIMATE JACOBIAN.
		!
		DO 20 J = 1, N
			TEMP = X(J)
			H = EPS*ABS(TEMP)
			IF (H .EQ. ZERO) H = EPS
			X(J) = TEMP + H
			CALL FCN(N,X,WA1,IFLAG)
			IF (IFLAG .LT. 0) exit ! add by @creaqi break the loop goto 30 in FDJAC1 with len equal 1
			X(J) = TEMP
			DO I = 1, N! add by @creaqi do label 10
				FJAC(I,J) = (WA1(I) - FVEC(I))/H
			enddo !add by @creaqi 10
			10          CONTINUE
			20       CONTINUE
			30    CONTINUE
			lgoto_110_2 = .true. !         GO TO 110
		endif !add by @creaqi label 40 modify_goto_pure
		if(.not.lgoto_110_2) then ! start second start of goto 110 modify_goto_related_if
			40 CONTINUE
			!
			!        COMPUTATION OF BANDED APPROXIMATE JACOBIAN.
			!
			DO 90 K = 1, MSUM
				DO J = K, N, MSUM! add by @creaqi do label 60
					WA2(J) = X(J)
					H = EPS*ABS(WA2(J))
					IF (H .EQ. ZERO) H = EPS
					X(J) = WA2(J) + H
				enddo !add by @creaqi 60
				60          CONTINUE
				CALL FCN(N,X,WA1,IFLAG)
				IF (IFLAG .LT. 0) exit ! add by @creaqi break the loop goto 100 in FDJAC1 with len equal 1
				DO J = K, N, MSUM! add by @creaqi do label 80
					X(J) = WA2(J)
					H = EPS*ABS(WA2(J))
					IF (H .EQ. ZERO) H = EPS
					DO I = 1, N! add by @creaqi do label 70
						FJAC(I,J) = ZERO
							IF (I .GE. J - MU .AND. I .LE. J + ML)&
						FJAC(I,J) = (WA1(I) - FVEC(I))/H
					enddo !add by @creaqi 70
					70             CONTINUE
				enddo !add by @creaqi 80
				80          CONTINUE
				90       CONTINUE
				100    CONTINUE
			endif ! add by @creaqi max lgoto_110_2 end of goto 110 modify_goto_related_if
			110 CONTINUE
			RETURN
			!
			!     LAST CARD OF SUBROUTINE FDJAC1.
			!
END
		!DECK FDUMP
SUBROUTINE FDUMP
	!***BEGIN PROLOGUE  FDUMP
	!***PURPOSE  Symbolic dump (should be locally written).
	!***LIBRARY   SLATEC (XERROR)
	!***CATEGORY  R3
	!***TYPE      ALL (FDUMP-A)
	!***KEYWORDS  ERROR, XERMSG
	!***AUTHOR  Jones, R. E., (SNLA)
	!***DESCRIPTION
	!
	!        ***Note*** Machine Dependent Routine
	!        FDUMP is intended to be replaced by a locally written
	!        version which produces a symbolic dump.  Failing this,
	!        it should be replaced by a version which prints the
	!        subprogram nesting list.  Note that this dump must be
	!        printed on each of up to five files, as indicated by the
	!        XGETUA routine.  See XSETUA and XGETUA for details.
	!
	!     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
	!
	!***REFERENCES  (NONE)
	!***ROUTINES CALLED  (NONE)
	!***REVISION HISTORY  (YYMMDD)
	!   790801  DATE WRITTEN
	!   861211  REVISION DATE from Version 3.2
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!***END PROLOGUE  FDUMP
	!***FIRST EXECUTABLE STATEMENT  FDUMP
	RETURN
END
!DECK I1MACH
INTEGER FUNCTION I1MACH (I)
!***BEGIN PROLOGUE  I1MACH
!***PURPOSE  Return integer machine dependent constants.
!***LIBRARY   SLATEC
!***CATEGORY  R1
!***TYPE      INTEGER (I1MACH-I)
!***KEYWORDS  MACHINE CONSTANTS
!***AUTHOR  Fox, P. A., (Bell Labs)
!           Hall, A. D., (Bell Labs)
!           Schryer, N. L., (Bell Labs)
!***DESCRIPTION
!
!   I1MACH can be used to obtain machine-dependent parameters for the
!   local machine environment.  It is a function subprogram with one
!   (input) argument and can be referenced as follows:
!
!        K = I1MACH(I)
!
!   where I=1,...,16.  The (output) value of K above is determined by
!   the (input) value of I.  The results for various values of I are
!   discussed below.
!
!   I/O unit numbers:
!     I1MACH( 1) = the standard input unit.
!     I1MACH( 2) = the standard output unit.
!     I1MACH( 3) = the standard punch unit.
!     I1MACH( 4) = the standard error message unit.
!
!   Words:
!     I1MACH( 5) = the number of bits per integer storage unit.
!     I1MACH( 6) = the number of characters per integer storage unit.
!
!   Integers:
!     assume integers are represented in the S-digit, base-A form
!
!                sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
!
!                where 0 .LE. X(I) .LT. A for I=0,...,S-1.
!     I1MACH( 7) = A, the base.
!     I1MACH( 8) = S, the number of base-A digits.
!     I1MACH( 9) = A**S - 1, the largest magnitude.
!
!   Floating-Point Numbers:
!     Assume floating-point numbers are represented in the T-digit,
!     base-B form
!                sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
!
!                where 0 .LE. X(I) .LT. B for I=1,...,T,
!                0 .LT. X(1), and EMIN .LE. E .LE. EMAX.
!     I1MACH(10) = B, the base.
!
!   Single-Precision:
!     I1MACH(11) = T, the number of base-B digits.
!     I1MACH(12) = EMIN, the smallest exponent E.
!     I1MACH(13) = EMAX, the largest exponent E.
!
!   Double-Precision:
!     I1MACH(14) = T, the number of base-B digits.
!     I1MACH(15) = EMIN, the smallest exponent E.
!     I1MACH(16) = EMAX, the largest exponent E.
!
!   To alter this function for a particular environment, the desired
!   set of DATA statements should be activated by removing the C from
!   column 1.  Also, the values of I1MACH(1) - I1MACH(4) should be
!   checked for consistency with the local operating system.
!
!***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
!                 a portable library, ACM Transactions on Mathematical
!                 Software 4, 2 (June 1978), pp. 177-188.
!***ROUTINES CALLED  (NONE)
!***REVISION HISTORY  (YYMMDD)
!   750101  DATE WRITTEN
!   891012  Added VAX G-floating constants.  (WRB)
!   891012  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900618  Added DEC RISC constants.  (WRB)
!   900723  Added IBM RS 6000 constants.  (WRB)
!   901009  Correct I1MACH(7) for IBM Mainframes. Should be 2 not 16.
!           (RWC)
!   910710  Added HP 730 constants.  (SMR)
!   911114  Added Convex IEEE constants.  (WRB)
!   920121  Added SUN -r8 compiler option constants.  (WRB)
!   920229  Added Touchstone Delta i860 constants.  (WRB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!   920625  Added Convex -p8 and -pd8 compiler option constants.
!           (BKS, WRB)
!   930201  Added DEC Alpha and SGI constants.  (RWC and WRB)
!   930618  Corrected I1MACH(5) for Convex -p8 and -pd8 compiler
!           options.  (DWL, RWC and WRB).
!***END PROLOGUE  I1MACH
!
INTEGER IMACH(16),OUTPUT
SAVE IMACH
EQUIVALENCE (IMACH(4),OUTPUT)
!
!     MACHINE CONSTANTS FOR THE AMIGA
!     ABSOFT COMPILER
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          5 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -126 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1022 /
!     DATA IMACH(16) /       1023 /
!
!     MACHINE CONSTANTS FOR THE APOLLO
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -125 /
!     DATA IMACH(13) /        129 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1021 /
!     DATA IMACH(16) /       1025 /
!
!     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM
!
!     DATA IMACH( 1) /          7 /
!     DATA IMACH( 2) /          2 /
!     DATA IMACH( 3) /          2 /
!     DATA IMACH( 4) /          2 /
!     DATA IMACH( 5) /         36 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         33 /
!     DATA IMACH( 9) / Z1FFFFFFFF /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -256 /
!     DATA IMACH(13) /        255 /
!     DATA IMACH(14) /         60 /
!     DATA IMACH(15) /       -256 /
!     DATA IMACH(16) /        255 /
!
!     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          7 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         48 /
!     DATA IMACH( 6) /          6 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         39 /
!     DATA IMACH( 9) / O0007777777777777 /
!     DATA IMACH(10) /          8 /
!     DATA IMACH(11) /         13 /
!     DATA IMACH(12) /        -50 /
!     DATA IMACH(13) /         76 /
!     DATA IMACH(14) /         26 /
!     DATA IMACH(15) /        -50 /
!     DATA IMACH(16) /         76 /
!
!     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          7 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         48 /
!     DATA IMACH( 6) /          6 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         39 /
!     DATA IMACH( 9) / O0007777777777777 /
!     DATA IMACH(10) /          8 /
!     DATA IMACH(11) /         13 /
!     DATA IMACH(12) /        -50 /
!     DATA IMACH(13) /         76 /
!     DATA IMACH(14) /         26 /
!     DATA IMACH(15) /     -32754 /
!     DATA IMACH(16) /      32780 /
!
!     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          7 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         64 /
!     DATA IMACH( 6) /          8 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         63 /
!     DATA IMACH( 9) / 9223372036854775807 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         47 /
!     DATA IMACH(12) /      -4095 /
!     DATA IMACH(13) /       4094 /
!     DATA IMACH(14) /         94 /
!     DATA IMACH(15) /      -4095 /
!     DATA IMACH(16) /       4094 /
!
!     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          7 /
!     DATA IMACH( 4) /    6LOUTPUT/
!     DATA IMACH( 5) /         60 /
!     DATA IMACH( 6) /         10 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         48 /
!     DATA IMACH( 9) / 00007777777777777777B /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         47 /
!     DATA IMACH(12) /       -929 /
!     DATA IMACH(13) /       1070 /
!     DATA IMACH(14) /         94 /
!     DATA IMACH(15) /       -929 /
!     DATA IMACH(16) /       1069 /
!
!     MACHINE CONSTANTS FOR THE CELERITY C1260
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          0 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / Z'7FFFFFFF' /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -126 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1022 /
!     DATA IMACH(16) /       1023 /
!
!     MACHINE CONSTANTS FOR THE CONVEX
!     USING THE -fn COMPILER OPTION
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          7 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -127 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1023 /
!     DATA IMACH(16) /       1023 /
!
!     MACHINE CONSTANTS FOR THE CONVEX
!     USING THE -fi COMPILER OPTION
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          7 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -125 /
!     DATA IMACH(13) /        128 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1021 /
!     DATA IMACH(16) /       1024 /
!
!     MACHINE CONSTANTS FOR THE CONVEX
!     USING THE -p8 COMPILER OPTION
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          7 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         64 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         63 /
!     DATA IMACH( 9) / 9223372036854775807 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         53 /
!     DATA IMACH(12) /      -1023 /
!     DATA IMACH(13) /       1023 /
!     DATA IMACH(14) /        113 /
!     DATA IMACH(15) /     -16383 /
!     DATA IMACH(16) /      16383 /
!
!     MACHINE CONSTANTS FOR THE CONVEX
!     USING THE -pd8 COMPILER OPTION
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          7 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         64 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         63 /
!     DATA IMACH( 9) / 9223372036854775807 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         53 /
!     DATA IMACH(12) /      -1023 /
!     DATA IMACH(13) /       1023 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1023 /
!     DATA IMACH(16) /       1023 /
!
!     MACHINE CONSTANTS FOR THE CRAY
!     USING THE 46 BIT INTEGER COMPILER OPTION
!
!     DATA IMACH( 1) /        100 /
!     DATA IMACH( 2) /        101 /
!     DATA IMACH( 3) /        102 /
!     DATA IMACH( 4) /        101 /
!     DATA IMACH( 5) /         64 /
!     DATA IMACH( 6) /          8 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         46 /
!     DATA IMACH( 9) / 1777777777777777B /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         47 /
!     DATA IMACH(12) /      -8189 /
!     DATA IMACH(13) /       8190 /
!     DATA IMACH(14) /         94 /
!     DATA IMACH(15) /      -8099 /
!     DATA IMACH(16) /       8190 /
!
!     MACHINE CONSTANTS FOR THE CRAY
!     USING THE 64 BIT INTEGER COMPILER OPTION
!
!     DATA IMACH( 1) /        100 /
!     DATA IMACH( 2) /        101 /
!     DATA IMACH( 3) /        102 /
!     DATA IMACH( 4) /        101 /
!     DATA IMACH( 5) /         64 /
!     DATA IMACH( 6) /          8 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         63 /
!     DATA IMACH( 9) / 777777777777777777777B /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         47 /
!     DATA IMACH(12) /      -8189 /
!     DATA IMACH(13) /       8190 /
!     DATA IMACH(14) /         94 /
!     DATA IMACH(15) /      -8099 /
!     DATA IMACH(16) /       8190 /
!
!     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
!
!     DATA IMACH( 1) /         11 /
!     DATA IMACH( 2) /         12 /
!     DATA IMACH( 3) /          8 /
!     DATA IMACH( 4) /         10 /
!     DATA IMACH( 5) /         16 /
!     DATA IMACH( 6) /          2 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         15 /
!     DATA IMACH( 9) /      32767 /
!     DATA IMACH(10) /         16 /
!     DATA IMACH(11) /          6 /
!     DATA IMACH(12) /        -64 /
!     DATA IMACH(13) /         63 /
!     DATA IMACH(14) /         14 /
!     DATA IMACH(15) /        -64 /
!     DATA IMACH(16) /         63 /
!
!     MACHINE CONSTANTS FOR THE DEC ALPHA
!     USING G_FLOAT
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          5 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -127 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1023 /
!     DATA IMACH(16) /       1023 /
!
!     MACHINE CONSTANTS FOR THE DEC ALPHA
!     USING IEEE_FLOAT
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -125 /
!     DATA IMACH(13) /        128 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1021 /
!     DATA IMACH(16) /       1024 /
!
!     MACHINE CONSTANTS FOR THE DEC RISC
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -125 /
!     DATA IMACH(13) /        128 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1021 /
!     DATA IMACH(16) /       1024 /
!
!     MACHINE CONSTANTS FOR THE DEC VAX
!     USING D_FLOATING
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          5 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -127 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         56 /
!     DATA IMACH(15) /       -127 /
!     DATA IMACH(16) /        127 /
!
!     MACHINE CONSTANTS FOR THE DEC VAX
!     USING G_FLOATING
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          5 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -127 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1023 /
!     DATA IMACH(16) /       1023 /
!
!     MACHINE CONSTANTS FOR THE ELXSI 6400
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         32 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -126 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1022 /
!     DATA IMACH(16) /       1023 /
!
!     MACHINE CONSTANTS FOR THE HARRIS 220
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          0 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         24 /
!     DATA IMACH( 6) /          3 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         23 /
!     DATA IMACH( 9) /    8388607 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         23 /
!     DATA IMACH(12) /       -127 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         38 /
!     DATA IMACH(15) /       -127 /
!     DATA IMACH(16) /        127 /
!
!     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /         43 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         36 /
!     DATA IMACH( 6) /          6 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         35 /
!     DATA IMACH( 9) / O377777777777 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         27 /
!     DATA IMACH(12) /       -127 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         63 /
!     DATA IMACH(15) /       -127 /
!     DATA IMACH(16) /        127 /
!
!     MACHINE CONSTANTS FOR THE HP 730
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -125 /
!     DATA IMACH(13) /        128 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1021 /
!     DATA IMACH(16) /       1024 /
!
!     MACHINE CONSTANTS FOR THE HP 2100
!     3 WORD DOUBLE PRECISION OPTION WITH FTN4
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          4 /
!     DATA IMACH( 4) /          1 /
!     DATA IMACH( 5) /         16 /
!     DATA IMACH( 6) /          2 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         15 /
!     DATA IMACH( 9) /      32767 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         23 /
!     DATA IMACH(12) /       -128 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         39 /
!     DATA IMACH(15) /       -128 /
!     DATA IMACH(16) /        127 /
!
!     MACHINE CONSTANTS FOR THE HP 2100
!     4 WORD DOUBLE PRECISION OPTION WITH FTN4
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          4 /
!     DATA IMACH( 4) /          1 /
!     DATA IMACH( 5) /         16 /
!     DATA IMACH( 6) /          2 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         15 /
!     DATA IMACH( 9) /      32767 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         23 /
!     DATA IMACH(12) /       -128 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         55 /
!     DATA IMACH(15) /       -128 /
!     DATA IMACH(16) /        127 /
!
!     MACHINE CONSTANTS FOR THE HP 9000
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          7 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         32 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -126 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1015 /
!     DATA IMACH(16) /       1017 /
!
!     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
!     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
!     THE PERKIN ELMER (INTERDATA) 7/32.
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          7 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) /  Z7FFFFFFF /
!     DATA IMACH(10) /         16 /
!     DATA IMACH(11) /          6 /
!     DATA IMACH(12) /        -64 /
!     DATA IMACH(13) /         63 /
!     DATA IMACH(14) /         14 /
!     DATA IMACH(15) /        -64 /
!     DATA IMACH(16) /         63 /
!
!     MACHINE CONSTANTS FOR THE IBM PC
!
DATA IMACH( 1) /          5 /
DATA IMACH( 2) /          6 /
DATA IMACH( 3) /          0 /
DATA IMACH( 4) /          0 /
DATA IMACH( 5) /         32 /
DATA IMACH( 6) /          4 /
DATA IMACH( 7) /          2 /
DATA IMACH( 8) /         31 /
DATA IMACH( 9) / 2147483647 /
DATA IMACH(10) /          2 /
DATA IMACH(11) /         24 /
DATA IMACH(12) /       -125 /
DATA IMACH(13) /        127 /
DATA IMACH(14) /         53 /
DATA IMACH(15) /      -1021 /
DATA IMACH(16) /       1023 /
!
!     MACHINE CONSTANTS FOR THE IBM RS 6000
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          0 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -125 /
!     DATA IMACH(13) /        128 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1021 /
!     DATA IMACH(16) /       1024 /
!
!     MACHINE CONSTANTS FOR THE INTEL i860
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -125 /
!     DATA IMACH(13) /        128 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1021 /
!     DATA IMACH(16) /       1024 /
!
!     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR)
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          5 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         36 /
!     DATA IMACH( 6) /          5 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         35 /
!     DATA IMACH( 9) / "377777777777 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         27 /
!     DATA IMACH(12) /       -128 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         54 /
!     DATA IMACH(15) /       -101 /
!     DATA IMACH(16) /        127 /
!
!     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR)
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          5 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         36 /
!     DATA IMACH( 6) /          5 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         35 /
!     DATA IMACH( 9) / "377777777777 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         27 /
!     DATA IMACH(12) /       -128 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         62 /
!     DATA IMACH(15) /       -128 /
!     DATA IMACH(16) /        127 /
!
!     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
!     32-BIT INTEGER ARITHMETIC.
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          5 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -127 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         56 /
!     DATA IMACH(15) /       -127 /
!     DATA IMACH(16) /        127 /
!
!     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
!     16-BIT INTEGER ARITHMETIC.
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          5 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         16 /
!     DATA IMACH( 6) /          2 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         15 /
!     DATA IMACH( 9) /      32767 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -127 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         56 /
!     DATA IMACH(15) /       -127 /
!     DATA IMACH(16) /        127 /
!
!     MACHINE CONSTANTS FOR THE SILICON GRAPHICS
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -125 /
!     DATA IMACH(13) /        128 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1021 /
!     DATA IMACH(16) /       1024 /
!
!     MACHINE CONSTANTS FOR THE SUN
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -125 /
!     DATA IMACH(13) /        128 /
!     DATA IMACH(14) /         53 /
!     DATA IMACH(15) /      -1021 /
!     DATA IMACH(16) /       1024 /
!
!     MACHINE CONSTANTS FOR THE SUN
!     USING THE -r8 COMPILER OPTION
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          6 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         32 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         31 /
!     DATA IMACH( 9) / 2147483647 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         53 /
!     DATA IMACH(12) /      -1021 /
!     DATA IMACH(13) /       1024 /
!     DATA IMACH(14) /        113 /
!     DATA IMACH(15) /     -16381 /
!     DATA IMACH(16) /      16384 /
!
!     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES FTN COMPILER
!
!     DATA IMACH( 1) /          5 /
!     DATA IMACH( 2) /          6 /
!     DATA IMACH( 3) /          1 /
!     DATA IMACH( 4) /          6 /
!     DATA IMACH( 5) /         36 /
!     DATA IMACH( 6) /          4 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         35 /
!     DATA IMACH( 9) / O377777777777 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         27 /
!     DATA IMACH(12) /       -128 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         60 /
!     DATA IMACH(15) /      -1024 /
!     DATA IMACH(16) /       1023 /
!
!     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR
!
!     DATA IMACH( 1) /          1 /
!     DATA IMACH( 2) /          1 /
!     DATA IMACH( 3) /          0 /
!     DATA IMACH( 4) /          1 /
!     DATA IMACH( 5) /         16 /
!     DATA IMACH( 6) /          2 /
!     DATA IMACH( 7) /          2 /
!     DATA IMACH( 8) /         15 /
!     DATA IMACH( 9) /      32767 /
!     DATA IMACH(10) /          2 /
!     DATA IMACH(11) /         24 /
!     DATA IMACH(12) /       -127 /
!     DATA IMACH(13) /        127 /
!     DATA IMACH(14) /         56 /
!     DATA IMACH(15) /       -127 /
!     DATA IMACH(16) /        127 /
!
!***FIRST EXECUTABLE STATEMENT  I1MACH
if(.not. (I .LT. 1  .OR.  I .GT. 16)) then ! add by @creaqi goto 10 in fun modify_goto_pure
	!
	I1MACH = IMACH(I)
	RETURN
	!
endif !add by @creaqi label 10 modify_goto_pure
10 CONTINUE
WRITE (UNIT = OUTPUT, FMT = 9000)
9000 FORMAT ('1ERROR    1 IN I1MACH - I OUT OF BOUNDS')
!
!     CALL FDUMP
!
STOP
END
!DECK J4SAVE
FUNCTION J4SAVE (IWHICH, IVALUE, ISET)
	!***BEGIN PROLOGUE  J4SAVE
	!***SUBSIDIARY
	!***PURPOSE  Save or recall global variables needed by error
	!            handling routines.
	!***LIBRARY   SLATEC (XERROR)
	!***TYPE      INTEGER (J4SAVE-I)
	!***KEYWORDS  ERROR MESSAGES, ERROR NUMBER, RECALL, SAVE, XERROR
	!***AUTHOR  Jones, R. E., (SNLA)
	!***DESCRIPTION
	!
	!     Abstract
	!        J4SAVE saves and recalls several global variables needed
	!        by the library error handling routines.
	!
	!     Description of Parameters
	!      --Input--
	!        IWHICH - Index of item desired.
	!                = 1 Refers to current error number.
	!                = 2 Refers to current error control flag.
	!                = 3 Refers to current unit number to which error
	!                    messages are to be sent.  (0 means use standard.)
	!                = 4 Refers to the maximum number of times any
	!                     message is to be printed (as set by XERMAX).
	!                = 5 Refers to the total number of units to which
	!                     each error message is to be written.
	!                = 6 Refers to the 2nd unit for error messages
	!                = 7 Refers to the 3rd unit for error messages
	!                = 8 Refers to the 4th unit for error messages
	!                = 9 Refers to the 5th unit for error messages
	!        IVALUE - The value to be set for the IWHICH-th parameter,
	!                 if ISET is .TRUE. .
	!        ISET   - If ISET=.TRUE., the IWHICH-th parameter will BE
	!                 given the value, IVALUE.  If ISET=.FALSE., the
	!                 IWHICH-th parameter will be unchanged, and IVALUE
	!                 is a dummy parameter.
	!      --Output--
	!        The (old) value of the IWHICH-th parameter will be returned
	!        in the function value, J4SAVE.
	!
	!***SEE ALSO  XERMSG
	!***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
	!                 Error-handling Package, SAND82-0800, Sandia
	!                 Laboratories, 1982.
	!***ROUTINES CALLED  (NONE)
	!***REVISION HISTORY  (YYMMDD)
	!   790801  DATE WRITTEN
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900205  Minor modifications to prologue.  (WRB)
	!   900402  Added TYPE section.  (WRB)
	!   910411  Added KEYWORDS section.  (WRB)
	!   920501  Reformatted the REFERENCES section.  (WRB)
	!***END PROLOGUE  J4SAVE
	LOGICAL ISET
	INTEGER IPARAM(9)
	SAVE IPARAM
	DATA IPARAM(1),IPARAM(2),IPARAM(3),IPARAM(4)/0,2,0,10/
	DATA IPARAM(5)/1/
	DATA IPARAM(6),IPARAM(7),IPARAM(8),IPARAM(9)/0,0,0,0/
	!***FIRST EXECUTABLE STATEMENT  J4SAVE
	J4SAVE = IPARAM(IWHICH)
	IF (ISET) IPARAM(IWHICH) = IVALUE
	RETURN
END
!DECK QFORM
SUBROUTINE QFORM (M, N, Q, LDQ, WA)
	!***BEGIN PROLOGUE  QFORM
	!***SUBSIDIARY
	!***PURPOSE  Subsidiary to SNSQ and SNSQE
	!***LIBRARY   SLATEC
	!***TYPE      SINGLE PRECISION (QFORM-S, DQFORM-D)
	!***AUTHOR  (UNKNOWN)
	!***DESCRIPTION
	!
	!     This subroutine proceeds from the computed QR factorization of
	!     an M by N matrix A to accumulate the M by M orthogonal matrix
	!     Q from its factored form.
	!
	!     The subroutine statement is
	!
	!       SUBROUTINE QFORM(M,N,Q,LDQ,WA)
	!
	!     where
	!
	!       M is a positive integer input variable set to the number
	!         of rows of A and the order of Q.
	!
	!       N is a positive integer input variable set to the number
	!         of columns of A.
	!
	!       Q is an M by M array. On input the full lower trapezoid in
	!         the first min(M,N) columns of Q contains the factored form.
	!         On output Q has been accumulated into a square matrix.
	!
	!       LDQ is a positive integer input variable not less than M
	!         which specifies the leading dimension of the array Q.
	!
	!       WA is a work array of length M.
	!
	!***SEE ALSO  SNSQ, SNSQE
	!***ROUTINES CALLED  (NONE)
	!***REVISION HISTORY  (YYMMDD)
	!   800301  DATE WRITTEN
	!   890531  Changed all specific intrinsics to generic.  (WRB)
	!   890831  Modified array declarations.  (WRB)
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900326  Removed duplicate information from DESCRIPTION section.
	!           (WRB)
	!   900328  Added TYPE section.  (WRB)
	!***END PROLOGUE  QFORM
	INTEGER M,N,LDQ
	REAL Q(LDQ,*),WA(*)
	INTEGER I,J,JM1,K,L,MINMN,NP1
	REAL ONE,SUM,TEMP,ZERO
	SAVE ONE, ZERO
	DATA ONE,ZERO /1.0E0,0.0E0/
	!***FIRST EXECUTABLE STATEMENT  QFORM
	MINMN = MIN(M,N)
	if(.not. (MINMN .LT. 2)) then ! add by @creaqi goto 30 in fun modify_goto_pure
		DO J = 2, MINMN! add by @creaqi do label 20
			JM1 = J - 1
			DO I = 1, JM1! add by @creaqi do label 10
				Q(I,J) = ZERO
			enddo !add by @creaqi 10
			10       CONTINUE
		enddo !add by @creaqi 20
		20    CONTINUE
	endif !add by @creaqi label 30 modify_goto_pure
	30 CONTINUE
	!
	!     INITIALIZE REMAINING COLUMNS TO THOSE OF THE IDENTITY MATRIX.
	!
	NP1 = N + 1
	if(.not. (M .LT. NP1)) then ! add by @creaqi goto 60 in fun modify_goto_pure
		DO J = NP1, M! add by @creaqi do label 50
			DO I = 1, M! add by @creaqi do label 40
				Q(I,J) = ZERO
			enddo !add by @creaqi 40
			40       CONTINUE
			Q(J,J) = ONE
		enddo !add by @creaqi 50
		50    CONTINUE
	endif !add by @creaqi label 60 modify_goto_pure
	60 CONTINUE
	!
	!     ACCUMULATE Q FROM ITS FACTORED FORM.
	!
	DO 120 L = 1, MINMN
		K = MINMN - L + 1
		DO I = K, M! add by @creaqi do label 70
			WA(I) = Q(I,K)
			Q(I,K) = ZERO
		enddo !add by @creaqi 70
		70       CONTINUE
		Q(K,K) = ONE
		if(.not. (WA(K) .EQ. ZERO)) then ! add by @creaqi goto 110 in fun modify_goto_pure
			DO J = K, M! add by @creaqi do label 100
				SUM = ZERO
				DO I = K, M! add by @creaqi do label 80
					SUM = SUM + Q(I,J)*WA(I)
				enddo !add by @creaqi 80
				80          CONTINUE
				TEMP = SUM/WA(K)
				DO I = K, M! add by @creaqi do label 90
					Q(I,J) = Q(I,J) - TEMP*WA(I)
				enddo !add by @creaqi 90
				90          CONTINUE
			enddo !add by @creaqi 100
			100       CONTINUE
		endif !add by @creaqi label 110 modify_goto_pure
		110    CONTINUE
		120    CONTINUE
		RETURN
		!
		!     LAST CARD OF SUBROUTINE QFORM.
		!
END
	!DECK QRFAC
SUBROUTINE QRFAC (M, N, A, LDA, PIVOT, IPVT, LIPVT, SIGMA, ACNORM,&
	WA)
	!***BEGIN PROLOGUE  QRFAC
	!***SUBSIDIARY
	!***PURPOSE  Subsidiary to SNLS1, SNLS1E, SNSQ and SNSQE
	!***LIBRARY   SLATEC
	!***TYPE      SINGLE PRECISION (QRFAC-S, DQRFAC-D)
	!***AUTHOR  (UNKNOWN)
	!***DESCRIPTION
	!
	!     This subroutine uses Householder transformations with column
	!     pivoting (optional) to compute a QR factorization of the
	!     M by N matrix A. That is, QRFAC determines an orthogonal
	!     matrix Q, a permutation matrix P, and an upper trapezoidal
	!     matrix R with diagonal elements of nonincreasing magnitude,
	!     such that A*P = Q*R. The Householder transformation for
	!     column K, K = 1,2,...,MIN(M,N), is of the form
	!
	!                           T
	!           I - (1/U(K))*U*U
	!
	!     where U has zeros in the first K-1 positions. The form of
	!     this transformation and the method of pivoting first
	!     appeared in the corresponding LINPACK subroutine.
	!
	!     The subroutine statement is
	!
	!       SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,SIGMA,ACNORM,WA)
	!
	!     where
	!
	!       M is a positive integer input variable set to the number
	!         of rows of A.
	!
	!       N is a positive integer input variable set to the number
	!         of columns of A.
	!
	!       A is an M by N array. On input A contains the matrix for
	!         which the QR factorization is to be computed. On output
	!         the strict upper trapezoidal part of A contains the strict
	!         upper trapezoidal part of R, and the lower trapezoidal
	!         part of A contains a factored form of Q (the non-trivial
	!         elements of the U vectors described above).
	!
	!       LDA is a positive integer input variable not less than M
	!         which specifies the leading dimension of the array A.
	!
	!       PIVOT is a logical input variable. If pivot is set .TRUE.,
	!         then column pivoting is enforced. If pivot is set .FALSE.,
	!         then no column pivoting is done.
	!
	!       IPVT is an integer output array of length LIPVT. IPVT
	!         defines the permutation matrix P such that A*P = Q*R.
	!         Column J of P is column IPVT(J) of the identity matrix.
	!         If pivot is .FALSE., IPVT is not referenced.
	!
	!       LIPVT is a positive integer input variable. If PIVOT is
	!             .FALSE., then LIPVT may be as small as 1. If PIVOT is
	!             .TRUE., then LIPVT must be at least N.
	!
	!       SIGMA is an output array of length N which contains the
	!         diagonal elements of R.
	!
	!       ACNORM is an output array of length N which contains the
	!         norms of the corresponding columns of the input matrix A.
	!         If this information is not needed, then ACNORM can coincide
	!         with SIGMA.
	!
	!       WA is a work array of length N. If pivot is .FALSE., then WA
	!         can coincide with SIGMA.
	!
	!***SEE ALSO  SNLS1, SNLS1E, SNSQ, SNSQE
	!***ROUTINES CALLED  ENORM, R1MACH
	!***REVISION HISTORY  (YYMMDD)
	!   800301  DATE WRITTEN
	!   890531  Changed all specific intrinsics to generic.  (WRB)
	!   890831  Modified array declarations.  (WRB)
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900326  Removed duplicate information from DESCRIPTION section.
	!           (WRB)
	!   900328  Added TYPE section.  (WRB)
	!***END PROLOGUE  QRFAC
	INTEGER M,N,LDA,LIPVT
	INTEGER IPVT(*)
	LOGICAL PIVOT
	REAL A(LDA,*),SIGMA(*),ACNORM(*),WA(*)
	INTEGER I,J,JP1,K,KMAX,MINMN
	REAL AJNORM,EPSMCH,ONE,P05,SUM,TEMP,ZERO
	REAL R1MACH,ENORM
	SAVE ONE, P05, ZERO
	DATA ONE,P05,ZERO /1.0E0,5.0E-2,0.0E0/
	!***FIRST EXECUTABLE STATEMENT  QRFAC
	EPSMCH = R1MACH(4)
	!
	!     COMPUTE THE INITIAL COLUMN NORMS AND INITIALIZE SEVERAL ARRAYS.
	!
	DO J = 1, N! add by @creaqi do label 10
		ACNORM(J) = ENORM(M,A(1,J))
		SIGMA(J) = ACNORM(J)
		WA(J) = SIGMA(J)
		IF (PIVOT) IPVT(J) = J
	enddo !add by @creaqi 10
	10    CONTINUE
	!
	!     REDUCE A TO R WITH HOUSEHOLDER TRANSFORMATIONS.
	!
	MINMN = MIN(M,N)
	DO 110 J = 1, MINMN
		if(.not. (.NOT.PIVOT)) then ! add by @creaqi goto 40 in fun modify_goto_pure
			!
			!        BRING THE COLUMN OF LARGEST NORM INTO THE PIVOT POSITION.
			!
			KMAX = J
			DO K = J, N! add by @creaqi do label 20
				IF (SIGMA(K) .GT. SIGMA(KMAX)) KMAX = K
			enddo !add by @creaqi 20
			20       CONTINUE
			if (.not. (KMAX .EQ. J)) then ! add by @creaqi 40 state_same == True
				DO I = 1, M! add by @creaqi do label 30
					TEMP = A(I,J)
					A(I,J) = A(I,KMAX)
					A(I,KMAX) = TEMP
				enddo !add by @creaqi 30
				30       CONTINUE
				SIGMA(KMAX) = SIGMA(J)
				WA(KMAX) = WA(J)
				K = IPVT(J)
				IPVT(J) = IPVT(KMAX)
				IPVT(KMAX) = K
			endif ! add by @creaqi 40 state_same == True
		endif !add by @creaqi label 40 modify_goto_pure
		40    CONTINUE
		!
		!        COMPUTE THE HOUSEHOLDER TRANSFORMATION TO REDUCE THE
		!        J-TH COLUMN OF A TO A MULTIPLE OF THE J-TH UNIT VECTOR.
		!
		AJNORM = ENORM(M-J+1,A(J,J))
		if(.not. (AJNORM .EQ. ZERO)) then ! add by @creaqi goto 100 in fun modify_goto_pure
			IF (A(J,J) .LT. ZERO) AJNORM = -AJNORM
			DO I = J, M! add by @creaqi do label 50
				A(I,J) = A(I,J)/AJNORM
			enddo !add by @creaqi 50
			50       CONTINUE
			A(J,J) = A(J,J) + ONE
			!
			!        APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS
			!        AND UPDATE THE NORMS.
			!
			JP1 = J + 1
			if (.not. (N .LT. JP1)) then ! add by @creaqi 100 state_same == True
				DO 90 K = JP1, N
					SUM = ZERO
					DO I = J, M! add by @creaqi do label 60
						SUM = SUM + A(I,J)*A(I,K)
					enddo !add by @creaqi 60
					60          CONTINUE
					TEMP = SUM/A(J,J)
					DO I = J, M! add by @creaqi do label 70
						A(I,K) = A(I,K) - TEMP*A(I,J)
					enddo !add by @creaqi 70
					70          CONTINUE
					if(.not. (.NOT.PIVOT .OR. SIGMA(K) .EQ. ZERO)) then ! add by @creaqi goto 80 in fun modify_goto_pure
						TEMP = A(J,K)/SIGMA(K)
						SIGMA(K) = SIGMA(K)*SQRT(MAX(ZERO,ONE-TEMP**2))
						if (.not. (P05*(SIGMA(K)/WA(K))**2 .GT. EPSMCH)) then ! add by @creaqi 80 state_same == True
							SIGMA(K) = ENORM(M-J,A(JP1,K))
							WA(K) = SIGMA(K)
						endif ! add by @creaqi 80 state_same == True
					endif !add by @creaqi label 80 modify_goto_pure
					80       CONTINUE
					90       CONTINUE
				endif ! add by @creaqi 100 state_same == True
			endif !add by @creaqi label 100 modify_goto_pure
			100    CONTINUE
			SIGMA(J) = -AJNORM
			110    CONTINUE
			RETURN
			!
			!     LAST CARD OF SUBROUTINE QRFAC.
			!
END
		!DECK R1MACH
		REAL FUNCTION R1MACH (I)
		!***BEGIN PROLOGUE  R1MACH
		!***PURPOSE  Return floating point machine dependent constants.
		!***LIBRARY   SLATEC
		!***CATEGORY  R1
		!***TYPE      SINGLE PRECISION (R1MACH-S, D1MACH-D)
		!***KEYWORDS  MACHINE CONSTANTS
		!***AUTHOR  Fox, P. A., (Bell Labs)
		!           Hall, A. D., (Bell Labs)
		!           Schryer, N. L., (Bell Labs)
		!***DESCRIPTION
		!
		!   R1MACH can be used to obtain machine-dependent parameters for the
		!   local machine environment.  It is a function subprogram with one
		!   (input) argument, and can be referenced as follows:
		!
		!        A = R1MACH(I)
		!
		!   where I=1,...,5.  The (output) value of A above is determined by
		!   the (input) value of I.  The results for various values of I are
		!   discussed below.
		!
		!   R1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
		!   R1MACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
		!   R1MACH(3) = B**(-T), the smallest relative spacing.
		!   R1MACH(4) = B**(1-T), the largest relative spacing.
		!   R1MACH(5) = LOG10(B)
		!
		!   Assume single precision numbers are represented in the T-digit,
		!   base-B form
		!
		!              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
		!
		!   where 0 .LE. X(I) .LT. B for I=1,...,T, 0 .LT. X(1), and
		!   EMIN .LE. E .LE. EMAX.
		!
		!   The values of B, T, EMIN and EMAX are provided in I1MACH as
		!   follows:
		!   I1MACH(10) = B, the base.
		!   I1MACH(11) = T, the number of base-B digits.
		!   I1MACH(12) = EMIN, the smallest exponent E.
		!   I1MACH(13) = EMAX, the largest exponent E.
		!
		!   To alter this function for a particular environment, the desired
		!   set of DATA statements should be activated by removing the C from
		!   column 1.  Also, the values of R1MACH(1) - R1MACH(4) should be
		!   checked for consistency with the local operating system.
		!
		!***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
		!                 a portable library, ACM Transactions on Mathematical
		!                 Software 4, 2 (June 1978), pp. 177-188.
		!***ROUTINES CALLED  XERMSG
		!***REVISION HISTORY  (YYMMDD)
		!   790101  DATE WRITTEN
		!   890213  REVISION DATE from Version 3.2
		!   891214  Prologue converted to Version 4.0 format.  (BAB)
		!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
		!   900618  Added DEC RISC constants.  (WRB)
		!   900723  Added IBM RS 6000 constants.  (WRB)
		!   910710  Added HP 730 constants.  (SMR)
		!   911114  Added Convex IEEE constants.  (WRB)
		!   920121  Added SUN -r8 compiler option constants.  (WRB)
		!   920229  Added Touchstone Delta i860 constants.  (WRB)
		!   920501  Reformatted the REFERENCES section.  (WRB)
		!   920625  Added CONVEX -p8 and -pd8 compiler option constants.
		!           (BKS, WRB)
		!   930201  Added DEC Alpha and SGI constants.  (RWC and WRB)
		!***END PROLOGUE  R1MACH
		!
		! --- Set up for IBM PC: declare as reals   ..........(DGS)
		!     INTEGER SMALL(2)
		!     INTEGER LARGE(2)
		!     INTEGER RIGHT(2)
		!     INTEGER DIVER(2)
		!     INTEGER LOG10(2)
		real SMALL(2)
		real LARGE(2)
		real RIGHT(2)
		real DIVER(2)
		real LOG10(2)
		! --- Set up for IBM PC: declare as reals   ..........(DGS)
		!
		REAL RMACH(5)
		SAVE RMACH
		!
		EQUIVALENCE (RMACH(1),SMALL(1))
		EQUIVALENCE (RMACH(2),LARGE(1))
		EQUIVALENCE (RMACH(3),RIGHT(1))
		EQUIVALENCE (RMACH(4),DIVER(1))
		EQUIVALENCE (RMACH(5),LOG10(1))
		!
		!     MACHINE CONSTANTS FOR THE AMIGA
		!     ABSOFT FORTRAN COMPILER USING THE 68020/68881 COMPILER OPTION
		!
		!     DATA SMALL(1) / Z'00800000' /
		!     DATA LARGE(1) / Z'7F7FFFFF' /
		!     DATA RIGHT(1) / Z'33800000' /
		!     DATA DIVER(1) / Z'34000000' /
		!     DATA LOG10(1) / Z'3E9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE AMIGA
		!     ABSOFT FORTRAN COMPILER USING SOFTWARE FLOATING POINT
		!
		!     DATA SMALL(1) / Z'00800000' /
		!     DATA LARGE(1) / Z'7EFFFFFF' /
		!     DATA RIGHT(1) / Z'33800000' /
		!     DATA DIVER(1) / Z'34000000' /
		!     DATA LOG10(1) / Z'3E9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE APOLLO
		!
		!     DATA SMALL(1) / 16#00800000 /
		!     DATA LARGE(1) / 16#7FFFFFFF /
		!     DATA RIGHT(1) / 16#33800000 /
		!     DATA DIVER(1) / 16#34000000 /
		!     DATA LOG10(1) / 16#3E9A209B /
		!
		!     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM
		!
		!     DATA RMACH(1) / Z400800000 /
		!     DATA RMACH(2) / Z5FFFFFFFF /
		!     DATA RMACH(3) / Z4E9800000 /
		!     DATA RMACH(4) / Z4EA800000 /
		!     DATA RMACH(5) / Z500E730E8 /
		!
		!     MACHINE CONSTANTS FOR THE BURROUGHS 5700/6700/7700 SYSTEMS
		!
		!     DATA RMACH(1) / O1771000000000000 /
		!     DATA RMACH(2) / O0777777777777777 /
		!     DATA RMACH(3) / O1311000000000000 /
		!     DATA RMACH(4) / O1301000000000000 /
		!     DATA RMACH(5) / O1157163034761675 /
		!
		!     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
		!
		!     DATA RMACH(1) / Z"3001800000000000" /
		!     DATA RMACH(2) / Z"4FFEFFFFFFFFFFFE" /
		!     DATA RMACH(3) / Z"3FD2800000000000" /
		!     DATA RMACH(4) / Z"3FD3800000000000" /
		!     DATA RMACH(5) / Z"3FFF9A209A84FBCF" /
		!
		!     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
		!
		!     DATA RMACH(1) / 00564000000000000000B /
		!     DATA RMACH(2) / 37767777777777777776B /
		!     DATA RMACH(3) / 16414000000000000000B /
		!     DATA RMACH(4) / 16424000000000000000B /
		!     DATA RMACH(5) / 17164642023241175720B /
		!
		!     MACHINE CONSTANTS FOR THE CELERITY C1260
		!
		!     DATA SMALL(1) / Z'00800000' /
		!     DATA LARGE(1) / Z'7F7FFFFF' /
		!     DATA RIGHT(1) / Z'33800000' /
		!     DATA DIVER(1) / Z'34000000' /
		!     DATA LOG10(1) / Z'3E9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE CONVEX
		!     USING THE -fn COMPILER OPTION
		!
		!     DATA RMACH(1) / Z'00800000' /
		!     DATA RMACH(2) / Z'7FFFFFFF' /
		!     DATA RMACH(3) / Z'34800000' /
		!     DATA RMACH(4) / Z'35000000' /
		!     DATA RMACH(5) / Z'3F9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE CONVEX
		!     USING THE -fi COMPILER OPTION
		!
		!     DATA RMACH(1) / Z'00800000' /
		!     DATA RMACH(2) / Z'7F7FFFFF' /
		!     DATA RMACH(3) / Z'33800000' /
		!     DATA RMACH(4) / Z'34000000' /
		!     DATA RMACH(5) / Z'3E9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE CONVEX
		!     USING THE -p8 OR -pd8 COMPILER OPTION
		!
		!     DATA RMACH(1) / Z'0010000000000000' /
		!     DATA RMACH(2) / Z'7FFFFFFFFFFFFFFF' /
		!     DATA RMACH(3) / Z'3CC0000000000000' /
		!     DATA RMACH(4) / Z'3CD0000000000000' /
		!     DATA RMACH(5) / Z'3FF34413509F79FF' /
		!
		!     MACHINE CONSTANTS FOR THE CRAY
		!
		!     DATA RMACH(1) / 200034000000000000000B /
		!     DATA RMACH(2) / 577767777777777777776B /
		!     DATA RMACH(3) / 377224000000000000000B /
		!     DATA RMACH(4) / 377234000000000000000B /
		!     DATA RMACH(5) / 377774642023241175720B /
		!
		!     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
		!     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
		!     STATIC RMACH(5)
		!
		!     DATA SMALL /    20K,       0 /
		!     DATA LARGE / 77777K, 177777K /
		!     DATA RIGHT / 35420K,       0 /
		!     DATA DIVER / 36020K,       0 /
		!     DATA LOG10 / 40423K,  42023K /
		!
		!     MACHINE CONSTANTS FOR THE DEC ALPHA
		!     USING G_FLOAT
		!
		!     DATA RMACH(1) / '00000080'X /
		!     DATA RMACH(2) / 'FFFF7FFF'X /
		!     DATA RMACH(3) / '00003480'X /
		!     DATA RMACH(4) / '00003500'X /
		!     DATA RMACH(5) / '209B3F9A'X /
		!
		!     MACHINE CONSTANTS FOR THE DEC ALPHA
		!     USING IEEE_FLOAT
		!
		!     DATA RMACH(1) / '00800000'X /
		!     DATA RMACH(2) / '7F7FFFFF'X /
		!     DATA RMACH(3) / '33800000'X /
		!     DATA RMACH(4) / '34000000'X /
		!     DATA RMACH(5) / '3E9A209B'X /
		!
		!     MACHINE CONSTANTS FOR THE DEC RISC
		!
		!     DATA RMACH(1) / Z'00800000' /
		!     DATA RMACH(2) / Z'7F7FFFFF' /
		!     DATA RMACH(3) / Z'33800000' /
		!     DATA RMACH(4) / Z'34000000' /
		!     DATA RMACH(5) / Z'3E9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE DEC VAX
		!     (EXPRESSED IN INTEGER AND HEXADECIMAL)
		!     THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSTEMS
		!     THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS
		!
		!     DATA SMALL(1) /       128 /
		!     DATA LARGE(1) /    -32769 /
		!     DATA RIGHT(1) /     13440 /
		!     DATA DIVER(1) /     13568 /
		!     DATA LOG10(1) / 547045274 /
		!
		!     DATA SMALL(1) / Z00000080 /
		!     DATA LARGE(1) / ZFFFF7FFF /
		!     DATA RIGHT(1) / Z00003480 /
		!     DATA DIVER(1) / Z00003500 /
		!     DATA LOG10(1) / Z209B3F9A /
		!
		!     MACHINE CONSTANTS FOR THE ELXSI 6400
		!     (ASSUMING REAL*4 IS THE DEFAULT REAL)
		!
		!     DATA SMALL(1) / '00800000'X /
		!     DATA LARGE(1) / '7F7FFFFF'X /
		!     DATA RIGHT(1) / '33800000'X /
		!     DATA DIVER(1) / '34000000'X /
		!     DATA LOG10(1) / '3E9A209B'X /
		!
		!     MACHINE CONSTANTS FOR THE HARRIS 220
		!
		!     DATA SMALL(1), SMALL(2) / '20000000, '00000201 /
		!     DATA LARGE(1), LARGE(2) / '37777777, '00000177 /
		!     DATA RIGHT(1), RIGHT(2) / '20000000, '00000352 /
		!     DATA DIVER(1), DIVER(2) / '20000000, '00000353 /
		!     DATA LOG10(1), LOG10(2) / '23210115, '00000377 /
		!
		!     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES
		!
		!     DATA RMACH(1) / O402400000000 /
		!     DATA RMACH(2) / O376777777777 /
		!     DATA RMACH(3) / O714400000000 /
		!     DATA RMACH(4) / O716400000000 /
		!     DATA RMACH(5) / O776464202324 /
		!
		!     MACHINE CONSTANTS FOR THE HP 730
		!
		!     DATA RMACH(1) / Z'00800000' /
		!     DATA RMACH(2) / Z'7F7FFFFF' /
		!     DATA RMACH(3) / Z'33800000' /
		!     DATA RMACH(4) / Z'34000000' /
		!     DATA RMACH(5) / Z'3E9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE HP 2100
		!     3 WORD DOUBLE PRECISION WITH FTN4
		!
		!     DATA SMALL(1), SMALL(2) / 40000B,       1 /
		!     DATA LARGE(1), LARGE(2) / 77777B, 177776B /
		!     DATA RIGHT(1), RIGHT(2) / 40000B,    325B /
		!     DATA DIVER(1), DIVER(2) / 40000B,    327B /
		!     DATA LOG10(1), LOG10(2) / 46420B,  46777B /
		!
		!     MACHINE CONSTANTS FOR THE HP 2100
		!     4 WORD DOUBLE PRECISION WITH FTN4
		!
		!     DATA SMALL(1), SMALL(2) / 40000B,       1 /
		!     DATA LARGE(1), LARGE(2) / 77777B, 177776B /
		!     DATA RIGHT(1), RIGHT(2) / 40000B,    325B /
		!     DATA DIVER(1), DIVER(2) / 40000B,    327B /
		!     DATA LOG10(1), LOG10(2) / 46420B,  46777B /
		!
		!     MACHINE CONSTANTS FOR THE HP 9000
		!
		!     DATA SMALL(1) / 00004000000B /
		!     DATA LARGE(1) / 17677777777B /
		!     DATA RIGHT(1) / 06340000000B /
		!     DATA DIVER(1) / 06400000000B /
		!     DATA LOG10(1) / 07646420233B /
		!
		!     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
		!     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86  AND
		!     THE PERKIN ELMER (INTERDATA) 7/32.
		!
		!     DATA RMACH(1) / Z00100000 /
		!     DATA RMACH(2) / Z7FFFFFFF /
		!     DATA RMACH(3) / Z3B100000 /
		!     DATA RMACH(4) / Z3C100000 /
		!     DATA RMACH(5) / Z41134413 /
		!
		!     MACHINE CONSTANTS FOR THE IBM PC
		!
		DATA SMALL(1) / 1.18E-38      /
		DATA LARGE(1) / 3.40E+38      /
		DATA RIGHT(1) / 0.595E-07     /
		DATA DIVER(1) / 1.19E-07      /
		DATA LOG10(1) / 0.30102999566 /
		!
		!     MACHINE CONSTANTS FOR THE IBM RS 6000
		!
		!     DATA RMACH(1) / Z'00800000' /
		!     DATA RMACH(2) / Z'7F7FFFFF' /
		!     DATA RMACH(3) / Z'33800000' /
		!     DATA RMACH(4) / Z'34000000' /
		!     DATA RMACH(5) / Z'3E9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE INTEL i860
		!
		!     DATA RMACH(1) / Z'00800000' /
		!     DATA RMACH(2) / Z'7F7FFFFF' /
		!     DATA RMACH(3) / Z'33800000' /
		!     DATA RMACH(4) / Z'34000000' /
		!     DATA RMACH(5) / Z'3E9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE PDP-10 (KA OR KI PROCESSOR)
		!
		!     DATA RMACH(1) / "000400000000 /
		!     DATA RMACH(2) / "377777777777 /
		!     DATA RMACH(3) / "146400000000 /
		!     DATA RMACH(4) / "147400000000 /
		!     DATA RMACH(5) / "177464202324 /
		!
		!     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
		!     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
		!
		!     DATA SMALL(1) /    8388608 /
		!     DATA LARGE(1) / 2147483647 /
		!     DATA RIGHT(1) /  880803840 /
		!     DATA DIVER(1) /  889192448 /
		!     DATA LOG10(1) / 1067065499 /
		!
		!     DATA RMACH(1) / O00040000000 /
		!     DATA RMACH(2) / O17777777777 /
		!     DATA RMACH(3) / O06440000000 /
		!     DATA RMACH(4) / O06500000000 /
		!     DATA RMACH(5) / O07746420233 /
		!
		!     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
		!     16-BIT INTEGERS  (EXPRESSED IN INTEGER AND OCTAL).
		!
		!     DATA SMALL(1), SMALL(2) /   128,     0 /
		!     DATA LARGE(1), LARGE(2) / 32767,    -1 /
		!     DATA RIGHT(1), RIGHT(2) / 13440,     0 /
		!     DATA DIVER(1), DIVER(2) / 13568,     0 /
		!     DATA LOG10(1), LOG10(2) / 16282,  8347 /
		!
		!     DATA SMALL(1), SMALL(2) / O000200, O000000 /
		!     DATA LARGE(1), LARGE(2) / O077777, O177777 /
		!     DATA RIGHT(1), RIGHT(2) / O032200, O000000 /
		!     DATA DIVER(1), DIVER(2) / O032400, O000000 /
		!     DATA LOG10(1), LOG10(2) / O037632, O020233 /
		!
		!     MACHINE CONSTANTS FOR THE SILICON GRAPHICS
		!
		!     DATA RMACH(1) / Z'00800000' /
		!     DATA RMACH(2) / Z'7F7FFFFF' /
		!     DATA RMACH(3) / Z'33800000' /
		!     DATA RMACH(4) / Z'34000000' /
		!     DATA RMACH(5) / Z'3E9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE SUN
		!
		!     DATA RMACH(1) / Z'00800000' /
		!     DATA RMACH(2) / Z'7F7FFFFF' /
		!     DATA RMACH(3) / Z'33800000' /
		!     DATA RMACH(4) / Z'34000000' /
		!     DATA RMACH(5) / Z'3E9A209B' /
		!
		!     MACHINE CONSTANTS FOR THE SUN
		!     USING THE -r8 COMPILER OPTION
		!
		!     DATA RMACH(1) / Z'0010000000000000' /
		!     DATA RMACH(2) / Z'7FEFFFFFFFFFFFFF' /
		!     DATA RMACH(3) / Z'3CA0000000000000' /
		!     DATA RMACH(4) / Z'3CB0000000000000' /
		!     DATA RMACH(5) / Z'3FD34413509F79FF' /
		!
		!     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES
		!
		!     DATA RMACH(1) / O000400000000 /
		!     DATA RMACH(2) / O377777777777 /
		!     DATA RMACH(3) / O146400000000 /
		!     DATA RMACH(4) / O147400000000 /
		!     DATA RMACH(5) / O177464202324 /
		!
		!     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR
		!
		!     DATA SMALL(1), SMALL(2) /     0,    256/
		!     DATA LARGE(1), LARGE(2) /    -1,   -129/
		!     DATA RIGHT(1), RIGHT(2) /     0,  26880/
		!     DATA DIVER(1), DIVER(2) /     0,  27136/
		!     DATA LOG10(1), LOG10(2) /  8347,  32538/
		!
		!***FIRST EXECUTABLE STATEMENT  R1MACH
		IF (I .LT. 1 .OR. I .GT. 5) CALL XERMSG ('SLATEC', 'R1MACH',&
			'I OUT OF BOUNDS', 1, 2)
		!
		R1MACH = RMACH(I)
		RETURN
		!
END
	!DECK R1MPYQ
SUBROUTINE R1MPYQ (M, N, A, LDA, V, W)
	!***BEGIN PROLOGUE  R1MPYQ
	!***SUBSIDIARY
	!***PURPOSE  Subsidiary to SNSQ and SNSQE
	!***LIBRARY   SLATEC
	!***TYPE      SINGLE PRECISION (R1MPYQ-S, D1MPYQ-D)
	!***AUTHOR  (UNKNOWN)
	!***DESCRIPTION
	!
	!     Given an M by N matrix A, this subroutine computes A*Q where
	!     Q is the product of 2*(N - 1) transformations
	!
	!           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)
	!
	!     and GV(I), GW(I) are Givens rotations in the (I,N) plane which
	!     eliminate elements in the I-th and N-th planes, respectively.
	!     Q itself is not given, rather the information to recover the
	!     GV, GW rotations is supplied.
	!
	!     The subroutine statement is
	!
	!       SUBROUTINE R1MPYQ(M,N,A,LDA,V,W)
	!
	!     where
	!
	!       M is a positive integer input variable set to the number
	!         of rows of A.
	!
	!       N is a positive integer input variable set to the number
	!         of columns of A.
	!
	!       A is an M by N ARRAY. On input A must contain the matrix
	!         to be postmultiplied by the orthogonal matrix Q
	!         described above. On output A*Q has replaced A.
	!
	!       LDA is a positive integer input variable not less than M
	!         which specifies the leading dimension of the array A.
	!
	!       V is an input array of length N. V(I) must contain the
	!         information necessary to recover the Givens rotation GV(I)
	!         described above.
	!
	!       W is an input array of length N. W(I) must contain the
	!         information necessary to recover the Givens rotation GW(I)
	!         described above.
	!
	!***SEE ALSO  SNSQ, SNSQE
	!***ROUTINES CALLED  (NONE)
	!***REVISION HISTORY  (YYMMDD)
	!   800301  DATE WRITTEN
	!   890831  Modified array declarations.  (WRB)
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900326  Removed duplicate information from DESCRIPTION section.
	!           (WRB)
	!   900328  Added TYPE section.  (WRB)
	!***END PROLOGUE  R1MPYQ
	INTEGER M,N,LDA
	REAL A(LDA,*),V(*),W(*)
	INTEGER I,J,NMJ,NM1
	REAL COS,ONE,SIN,TEMP
	SAVE ONE
	DATA ONE /1.0E0/
	!***FIRST EXECUTABLE STATEMENT  R1MPYQ
	NM1 = N - 1
	if(.not. (NM1 .LT. 1)) then ! add by @creaqi goto 50 in fun modify_goto_pure
		DO NMJ = 1, NM1! add by @creaqi do label 20
			J = N - NMJ
			IF (ABS(V(J)) .GT. ONE) COS = ONE/V(J)
			IF (ABS(V(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)
			IF (ABS(V(J)) .LE. ONE) SIN = V(J)
			IF (ABS(V(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)
			DO I = 1, M! add by @creaqi do label 10
				TEMP = COS*A(I,J) - SIN*A(I,N)
				A(I,N) = SIN*A(I,J) + COS*A(I,N)
				A(I,J) = TEMP
			enddo !add by @creaqi 10
			10       CONTINUE
		enddo !add by @creaqi 20
		20    CONTINUE
		!
		!     APPLY THE SECOND SET OF GIVENS ROTATIONS TO A.
		!
		DO J = 1, NM1! add by @creaqi do label 40
			IF (ABS(W(J)) .GT. ONE) COS = ONE/W(J)
			IF (ABS(W(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)
			IF (ABS(W(J)) .LE. ONE) SIN = W(J)
			IF (ABS(W(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)
			DO I = 1, M! add by @creaqi do label 30
				TEMP = COS*A(I,J) + SIN*A(I,N)
				A(I,N) = -SIN*A(I,J) + COS*A(I,N)
				A(I,J) = TEMP
			enddo !add by @creaqi 30
			30       CONTINUE
		enddo !add by @creaqi 40
		40    CONTINUE
	endif !add by @creaqi label 50 modify_goto_pure
	50 CONTINUE
	RETURN
	!
	!     LAST CARD OF SUBROUTINE R1MPYQ.
	!
END
!DECK R1UPDT
SUBROUTINE R1UPDT (M, N, S, LS, U, V, W, SING)
	!***BEGIN PROLOGUE  R1UPDT
	!***SUBSIDIARY
	!***PURPOSE  Subsidiary to SNSQ and SNSQE
	!***LIBRARY   SLATEC
	!***TYPE      SINGLE PRECISION (R1UPDT-S, D1UPDT-D)
	!***AUTHOR  (UNKNOWN)
	!***DESCRIPTION
	!
	!     Given an M by N lower trapezoidal matrix S, an M-vector U,
	!     and an N-vector V, the problem is to determine an
	!     orthogonal matrix Q such that
	!
	!                   T
	!           (S + U*V )*Q
	!
	!     is again lower trapezoidal.
	!
	!     This subroutine determines Q as the product of 2*(N - 1)
	!     transformations
	!
	!           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)
	!
	!     where GV(I), GW(I) are Givens rotations in the (I,N) plane
	!     which eliminate elements in the I-th and N-th planes,
	!     respectively. Q Itself is not accumulated, rather the
	!     information to recover the GV, GW rotations is returned.
	!
	!     The subroutine statement is
	!
	!       SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)
	!
	!     where
	!
	!       M is a positive integer input variable set to the number
	!         of rows of S.
	!
	!       N is a positive integer input variable set to the number
	!         of columns of S. N must not exceed M.
	!
	!       S is an array of length LS. On input S must contain the lower
	!         trapezoidal matrix S stored by columns. On output S contains
	!         the lower trapezoidal matrix produced as described above.
	!
	!       LS is a positive integer input variable not less than
	!         (N*(2*M-N+1))/2.
	!
	!       U is an input array of length M which must contain the
	!         vector U.
	!
	!       V is an array of length N. On input V must contain the vector
	!         V. On output V(I) contains the information necessary to
	!         recover the Givens rotation GV(I) described above.
	!
	!       W is an output array of length M. W(I) contains information
	!         necessary to recover the Givens rotation GW(I) described
	!         above.
	!
	!       SING is a logical output variable. SING is set .TRUE. if any
	!         of the diagonal elements of the output S are zero. Otherwise
	!         SING is set .FALSE.
	!
	!***SEE ALSO  SNSQ, SNSQE
	!***ROUTINES CALLED  R1MACH
	!***REVISION HISTORY  (YYMMDD)
	!   800301  DATE WRITTEN
	!   890831  Modified array declarations.  (WRB)
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900326  Removed duplicate information from DESCRIPTION section.
	!           (WRB)
	!   900328  Added TYPE section.  (WRB)
	!***END PROLOGUE  R1UPDT
	INTEGER M,N,LS
	LOGICAL SING
	REAL S(*),U(*),V(*),W(*)
	INTEGER I,J,JJ,L,NMJ,NM1
	REAL COS,COTAN,GIANT,ONE,P5,P25,SIN,TAN,TAU,TEMP,ZERO
	REAL R1MACH
	logical :: lgoto_100_7 = .false. ! add by @creaqi R1UPDT modify_goto_in_if
	logical :: lgoto_30_3 = .false. ! add by @creaqi R1UPDT modify_goto_in_if
	SAVE ONE, P5, P25, ZERO
	DATA ONE,P5,P25,ZERO /1.0E0,5.0E-1,2.5E-1,0.0E0/
	!***FIRST EXECUTABLE STATEMENT  R1UPDT
	GIANT = R1MACH(2)
	!
	!     INITIALIZE THE DIAGONAL ELEMENT POINTER.
	!
	JJ = (N*(2*M - N + 1))/2 - (M - N)
	!
	!     MOVE THE NONTRIVIAL PART OF THE LAST COLUMN OF S INTO W.
	!
	L = JJ
	DO I = N, M! add by @creaqi do label 10
		W(I) = S(L)
		L = L + 1
	enddo !add by @creaqi 10
	10    CONTINUE
	!
	!     ROTATE THE VECTOR V INTO A MULTIPLE OF THE N-TH UNIT VECTOR
	!     IN SUCH A WAY THAT A SPIKE IS INTRODUCED INTO W.
	!
	NM1 = N - 1
	if(.not. (NM1 .LT. 1)) then ! add by @creaqi goto 70 in fun modify_goto_pure
		DO 60 NMJ = 1, NM1
			J = N - NMJ
			JJ = JJ - (M - J + 1)
			W(J) = ZERO
			if(.not. (V(J) .EQ. ZERO)) then ! add by @creaqi goto 50 in fun modify_goto_pure
				!
				!        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
				!        J-TH ELEMENT OF V.
				!
				if(.not. (ABS(V(N)) .GE. ABS(V(J)))) then ! add by @creaqi goto 20 in fun modify_goto_pure
					COTAN = V(N)/V(J)
					SIN = P5/SQRT(P25+P25*COTAN**2)
					COS = SIN*COTAN
					TAU = ONE
					IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS
					lgoto_30_3 = .true. !            GO TO 30
				endif !add by @creaqi label 20 modify_goto_pure
				if(.not.lgoto_30_3) then ! start second start of goto 30 modify_goto_related_if
					20    CONTINUE
					TAN = V(J)/V(N)
					COS = P5/SQRT(P25+P25*TAN**2)
					SIN = COS*TAN
					TAU = SIN
				endif ! add by @creaqi max lgoto_30_3 end of goto 30 modify_goto_related_if
				30    CONTINUE
				!
				!        APPLY THE TRANSFORMATION TO V AND STORE THE INFORMATION
				!        NECESSARY TO RECOVER THE GIVENS ROTATION.
				!
				V(N) = SIN*V(J) + COS*V(N)
				V(J) = TAU
				!
				!        APPLY THE TRANSFORMATION TO S AND EXTEND THE SPIKE IN W.
				!
				L = JJ
				DO I = J, M! add by @creaqi do label 40
					TEMP = COS*S(L) - SIN*W(I)
					W(I) = SIN*S(L) + COS*W(I)
					S(L) = TEMP
					L = L + 1
				enddo !add by @creaqi 40
				40       CONTINUE
			endif !add by @creaqi label 50 modify_goto_pure
			50    CONTINUE
			60    CONTINUE
		endif !add by @creaqi label 70 modify_goto_pure
		70 CONTINUE
		!
		!     ADD THE SPIKE FROM THE RANK 1 UPDATE TO W.
		!
		DO I = 1, M! add by @creaqi do label 80
			W(I) = W(I) + V(N)*U(I)
		enddo !add by @creaqi 80
		80    CONTINUE
		!
		!     ELIMINATE THE SPIKE.
		!
		SING = .FALSE.
		if(.not. (NM1 .LT. 1)) then ! add by @creaqi goto 140 in fun modify_goto_pure
			DO 130 J = 1, NM1
				if(.not. (W(J) .EQ. ZERO)) then ! add by @creaqi goto 120 in fun modify_goto_pure
					!
					!        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
					!        J-TH ELEMENT OF THE SPIKE.
					!
					if(.not. (ABS(S(JJ)) .GE. ABS(W(J)))) then ! add by @creaqi goto 90 in fun modify_goto_pure
						COTAN = S(JJ)/W(J)
						SIN = P5/SQRT(P25+P25*COTAN**2)
						COS = SIN*COTAN
						TAU = ONE
						IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS
						lgoto_100_7 = .true. !            GO TO 100
					endif !add by @creaqi label 90 modify_goto_pure
					if(.not.lgoto_100_7) then ! start second start of goto 100 modify_goto_related_if
						90    CONTINUE
						TAN = W(J)/S(JJ)
						COS = P5/SQRT(P25+P25*TAN**2)
						SIN = COS*TAN
						TAU = SIN
					endif ! add by @creaqi max lgoto_100_7 end of goto 100 modify_goto_related_if
					100    CONTINUE
					!
					!        APPLY THE TRANSFORMATION TO S AND REDUCE THE SPIKE IN W.
					!
					L = JJ
					DO I = J, M! add by @creaqi do label 110
						TEMP = COS*S(L) + SIN*W(I)
						W(I) = -SIN*S(L) + COS*W(I)
						S(L) = TEMP
						L = L + 1
					enddo !add by @creaqi 110
					110       CONTINUE
					!
					!        STORE THE INFORMATION NECESSARY TO RECOVER THE
					!        GIVENS ROTATION.
					!
					W(J) = TAU
				endif !add by @creaqi label 120 modify_goto_pure
				120    CONTINUE
				!
				!        TEST FOR ZERO DIAGONAL ELEMENTS IN THE OUTPUT S.
				!
				IF (S(JJ) .EQ. ZERO) SING = .TRUE.
				JJ = JJ + (M - J + 1)
				130    CONTINUE
			endif !add by @creaqi label 140 modify_goto_pure
			140 CONTINUE
			!
			!     MOVE W BACK INTO THE LAST COLUMN OF THE OUTPUT S.
			!
			L = JJ
			DO I = N, M! add by @creaqi do label 150
				S(L) = W(I)
				L = L + 1
			enddo !add by @creaqi 150
			150    CONTINUE
			IF (S(JJ) .EQ. ZERO) SING = .TRUE.
			RETURN
			!
			!     LAST CARD OF SUBROUTINE R1UPDT.
			!
END
		!DECK XERCNT
SUBROUTINE XERCNT (LIBRAR, SUBROU, MESSG, NERR, LEVEL, KONTRL)
	!***BEGIN PROLOGUE  XERCNT
	!***SUBSIDIARY
	!***PURPOSE  Allow user control over handling of errors.
	!***LIBRARY   SLATEC (XERROR)
	!***CATEGORY  R3C
	!***TYPE      ALL (XERCNT-A)
	!***KEYWORDS  ERROR, XERROR
	!***AUTHOR  Jones, R. E., (SNLA)
	!***DESCRIPTION
	!
	!     Abstract
	!        Allows user control over handling of individual errors.
	!        Just after each message is recorded, but before it is
	!        processed any further (i.e., before it is printed or
	!        a decision to abort is made), a call is made to XERCNT.
	!        If the user has provided his own version of XERCNT, he
	!        can then override the value of KONTROL used in processing
	!        this message by redefining its value.
	!        KONTRL may be set to any value from -2 to 2.
	!        The meanings for KONTRL are the same as in XSETF, except
	!        that the value of KONTRL changes only for this message.
	!        If KONTRL is set to a value outside the range from -2 to 2,
	!        it will be moved back into that range.
	!
	!     Description of Parameters
	!
	!      --Input--
	!        LIBRAR - the library that the routine is in.
	!        SUBROU - the subroutine that XERMSG is being called from
	!        MESSG  - the first 20 characters of the error message.
	!        NERR   - same as in the call to XERMSG.
	!        LEVEL  - same as in the call to XERMSG.
	!        KONTRL - the current value of the control flag as set
	!                 by a call to XSETF.
	!
	!      --Output--
	!        KONTRL - the new value of KONTRL.  If KONTRL is not
	!                 defined, it will remain at its original value.
	!                 This changed value of control affects only
	!                 the current occurrence of the current message.
	!
	!***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
	!                 Error-handling Package, SAND82-0800, Sandia
	!                 Laboratories, 1982.
	!***ROUTINES CALLED  (NONE)
	!***REVISION HISTORY  (YYMMDD)
	!   790801  DATE WRITTEN
	!   861211  REVISION DATE from Version 3.2
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900206  Routine changed from user-callable to subsidiary.  (WRB)
	!   900510  Changed calling sequence to include LIBRARY and SUBROUTINE
	!           names, changed routine name from XERCTL to XERCNT.  (RWC)
	!   920501  Reformatted the REFERENCES section.  (WRB)
	!***END PROLOGUE  XERCNT
	CHARACTER*(*) LIBRAR, SUBROU, MESSG
	!***FIRST EXECUTABLE STATEMENT  XERCNT
	RETURN
END
!DECK XERHLT
SUBROUTINE XERHLT (MESSG)
	!***BEGIN PROLOGUE  XERHLT
	!***SUBSIDIARY
	!***PURPOSE  Abort program execution and print error message.
	!***LIBRARY   SLATEC (XERROR)
	!***CATEGORY  R3C
	!***TYPE      ALL (XERHLT-A)
	!***KEYWORDS  ABORT PROGRAM EXECUTION, ERROR, XERROR
	!***AUTHOR  Jones, R. E., (SNLA)
	!***DESCRIPTION
	!
	!     Abstract
	!        ***Note*** machine dependent routine
	!        XERHLT aborts the execution of the program.
	!        The error message causing the abort is given in the calling
	!        sequence, in case one needs it for printing on a dayfile,
	!        for example.
	!
	!     Description of Parameters
	!        MESSG is as in XERMSG.
	!
	!***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
	!                 Error-handling Package, SAND82-0800, Sandia
	!                 Laboratories, 1982.
	!***ROUTINES CALLED  (NONE)
	!***REVISION HISTORY  (YYMMDD)
	!   790801  DATE WRITTEN
	!   861211  REVISION DATE from Version 3.2
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900206  Routine changed from user-callable to subsidiary.  (WRB)
	!   900510  Changed calling sequence to delete length of character
	!           and changed routine name from XERABT to XERHLT.  (RWC)
	!   920501  Reformatted the REFERENCES section.  (WRB)
	!***END PROLOGUE  XERHLT
	CHARACTER*(*) MESSG
	!***FIRST EXECUTABLE STATEMENT  XERHLT
	STOP
END
!DECK XERMSG
SUBROUTINE XERMSG (LIBRAR, SUBROU, MESSG, NERR, LEVEL)
	!***BEGIN PROLOGUE  XERMSG
	!***PURPOSE  Process error messages for SLATEC and other libraries.
	!***LIBRARY   SLATEC (XERROR)
	!***CATEGORY  R3C
	!***TYPE      ALL (XERMSG-A)
	!***KEYWORDS  ERROR MESSAGE, XERROR
	!***AUTHOR  Fong, Kirby, (NMFECC at LLNL)
	!***DESCRIPTION
	!
	!   XERMSG processes a diagnostic message in a manner determined by the
	!   value of LEVEL and the current value of the library error control
	!   flag, KONTRL.  See subroutine XSETF for details.
	!
	!    LIBRAR   A character constant (or character variable) with the name
	!             of the library.  This will be 'SLATEC' for the SLATEC
	!             Common Math Library.  The error handling package is
	!             general enough to be used by many libraries
	!             simultaneously, so it is desirable for the routine that
	!             detects and reports an error to identify the library name
	!             as well as the routine name.
	!
	!    SUBROU   A character constant (or character variable) with the name
	!             of the routine that detected the error.  Usually it is the
	!             name of the routine that is calling XERMSG.  There are
	!             some instances where a user callable library routine calls
	!             lower level subsidiary routines where the error is
	!             detected.  In such cases it may be more informative to
	!             supply the name of the routine the user called rather than
	!             the name of the subsidiary routine that detected the
	!             error.
	!
	!    MESSG    A character constant (or character variable) with the text
	!             of the error or warning message.  In the example below,
	!             the message is a character constant that contains a
	!             generic message.
	!
	!                   CALL XERMSG ('SLATEC', 'MMPY',
	!                  *'THE ORDER OF THE MATRIX EXCEEDS THE ROW DIMENSION',
	!                  *3, 1)
	!
	!             It is possible (and is sometimes desirable) to generate a
	!             specific message--e.g., one that contains actual numeric
	!             values.  Specific numeric values can be converted into
	!             character strings using formatted WRITE statements into
	!             character variables.  This is called standard Fortran
	!             internal file I/O and is exemplified in the first three
	!             lines of the following example.  You can also catenate
	!             substrings of characters to construct the error message.
	!             Here is an example showing the use of both writing to
	!             an internal file and catenating character strings.
	!
	!                   CHARACTER*5 CHARN, CHARL
	!                   WRITE (CHARN,10) N
	!                   WRITE (CHARL,10) LDA
	!                10 FORMAT(I5)
	!                   CALL XERMSG ('SLATEC', 'MMPY', 'THE ORDER'//CHARN//
	!                  *   ' OF THE MATRIX EXCEEDS ITS ROW DIMENSION OF'//
	!                  *   CHARL, 3, 1)
	!
	!             There are two subtleties worth mentioning.  One is that
	!             the // for character catenation is used to construct the
	!             error message so that no single character constant is
	!             continued to the next line.  This avoids confusion as to
	!             whether there are trailing blanks at the end of the line.
	!             The second is that by catenating the parts of the message
	!             as an actual argument rather than encoding the entire
	!             message into one large character variable, we avoid
	!             having to know how long the message will be in order to
	!             declare an adequate length for that large character
	!             variable.  XERMSG calls XERPRN to print the message using
	!             multiple lines if necessary.  If the message is very long,
	!             XERPRN will break it into pieces of 72 characters (as
	!             requested by XERMSG) for printing on multiple lines.
	!             Also, XERMSG asks XERPRN to prefix each line with ' *  '
	!             so that the total line length could be 76 characters.
	!             Note also that XERPRN scans the error message backwards
	!             to ignore trailing blanks.  Another feature is that
	!             the substring '$$' is treated as a new line sentinel
	!             by XERPRN.  If you want to construct a multiline
	!             message without having to count out multiples of 72
	!             characters, just use '$$' as a separator.  '$$'
	!             obviously must occur within 72 characters of the
	!             start of each line to have its intended effect since
	!             XERPRN is asked to wrap around at 72 characters in
	!             addition to looking for '$$'.
	!
	!    NERR     An integer value that is chosen by the library routine's
	!             author.  It must be in the range -99 to 999 (three
	!             printable digits).  Each distinct error should have its
	!             own error number.  These error numbers should be described
	!             in the machine readable documentation for the routine.
	!             The error numbers need be unique only within each routine,
	!             so it is reasonable for each routine to start enumerating
	!             errors from 1 and proceeding to the next integer.
	!
	!    LEVEL    An integer value in the range 0 to 2 that indicates the
	!             level (severity) of the error.  Their meanings are
	!
	!            -1  A warning message.  This is used if it is not clear
	!                that there really is an error, but the user's attention
	!                may be needed.  An attempt is made to only print this
	!                message once.
	!
	!             0  A warning message.  This is used if it is not clear
	!                that there really is an error, but the user's attention
	!                may be needed.
	!
	!             1  A recoverable error.  This is used even if the error is
	!                so serious that the routine cannot return any useful
	!                answer.  If the user has told the error package to
	!                return after recoverable errors, then XERMSG will
	!                return to the Library routine which can then return to
	!                the user's routine.  The user may also permit the error
	!                package to terminate the program upon encountering a
	!                recoverable error.
	!
	!             2  A fatal error.  XERMSG will not return to its caller
	!                after it receives a fatal error.  This level should
	!                hardly ever be used; it is much better to allow the
	!                user a chance to recover.  An example of one of the few
	!                cases in which it is permissible to declare a level 2
	!                error is a reverse communication Library routine that
	!                is likely to be called repeatedly until it integrates
	!                across some interval.  If there is a serious error in
	!                the input such that another step cannot be taken and
	!                the Library routine is called again without the input
	!                error having been corrected by the caller, the Library
	!                routine will probably be called forever with improper
	!                input.  In this case, it is reasonable to declare the
	!                error to be fatal.
	!
	!    Each of the arguments to XERMSG is input; none will be modified by
	!    XERMSG.  A routine may make multiple calls to XERMSG with warning
	!    level messages; however, after a call to XERMSG with a recoverable
	!    error, the routine should return to the user.  Do not try to call
	!    XERMSG with a second recoverable error after the first recoverable
	!    error because the error package saves the error number.  The user
	!    can retrieve this error number by calling another entry point in
	!    the error handling package and then clear the error number when
	!    recovering from the error.  Calling XERMSG in succession causes the
	!    old error number to be overwritten by the latest error number.
	!    This is considered harmless for error numbers associated with
	!    warning messages but must not be done for error numbers of serious
	!    errors.  After a call to XERMSG with a recoverable error, the user
	!    must be given a chance to call NUMXER or XERCLR to retrieve or
	!    clear the error number.
	!***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
	!                 Error-handling Package, SAND82-0800, Sandia
	!                 Laboratories, 1982.
	!***ROUTINES CALLED  FDUMP, J4SAVE, XERCNT, XERHLT, XERPRN, XERSVE
	!***REVISION HISTORY  (YYMMDD)
	!   880101  DATE WRITTEN
	!   880621  REVISED AS DIRECTED AT SLATEC CML MEETING OF FEBRUARY 1988.
	!           THERE ARE TWO BASIC CHANGES.
	!           1.  A NEW ROUTINE, XERPRN, IS USED INSTEAD OF XERPRT TO
	!               PRINT MESSAGES.  THIS ROUTINE WILL BREAK LONG MESSAGES
	!               INTO PIECES FOR PRINTING ON MULTIPLE LINES.  '$$' IS
	!               ACCEPTED AS A NEW LINE SENTINEL.  A PREFIX CAN BE
	!               ADDED TO EACH LINE TO BE PRINTED.  XERMSG USES EITHER
	!               ' ***' OR ' *  ' AND LONG MESSAGES ARE BROKEN EVERY
	!               72 CHARACTERS (AT MOST) SO THAT THE MAXIMUM LINE
	!               LENGTH OUTPUT CAN NOW BE AS GREAT AS 76.
	!           2.  THE TEXT OF ALL MESSAGES IS NOW IN UPPER CASE SINCE THE
	!               FORTRAN STANDARD DOCUMENT DOES NOT ADMIT THE EXISTENCE
	!               OF LOWER CASE.
	!   880708  REVISED AFTER THE SLATEC CML MEETING OF JUNE 29 AND 30.
	!           THE PRINCIPAL CHANGES ARE
	!           1.  CLARIFY COMMENTS IN THE PROLOGUES
	!           2.  RENAME XRPRNT TO XERPRN
	!           3.  REWORK HANDLING OF '$$' IN XERPRN TO HANDLE BLANK LINES
	!               SIMILAR TO THE WAY FORMAT STATEMENTS HANDLE THE /
	!               CHARACTER FOR NEW RECORDS.
	!   890706  REVISED WITH THE HELP OF FRED FRITSCH AND REG CLEMENS TO
	!           CLEAN UP THE CODING.
	!   890721  REVISED TO USE NEW FEATURE IN XERPRN TO COUNT CHARACTERS IN
	!           PREFIX.
	!   891013  REVISED TO CORRECT COMMENTS.
	!   891214  Prologue converted to Version 4.0 format.  (WRB)
	!   900510  Changed test on NERR to be -9999999 < NERR < 99999999, but
	!           NERR .ne. 0, and on LEVEL to be -2 < LEVEL < 3.  Added
	!           LEVEL=-1 logic, changed calls to XERSAV to XERSVE, and
	!           XERCTL to XERCNT.  (RWC)
	!   920501  Reformatted the REFERENCES section.  (WRB)
	!***END PROLOGUE  XERMSG
	CHARACTER*(*) LIBRAR, SUBROU, MESSG
	CHARACTER*8 XLIBR, XSUBR
	CHARACTER*72  TEMP
	CHARACTER*20  LFIRST
	!***FIRST EXECUTABLE STATEMENT  XERMSG
	LKNTRL = J4SAVE (2, 0, .FALSE.)
	MAXMES = J4SAVE (4, 0, .FALSE.)
	!
	!       LKNTRL IS A LOCAL COPY OF THE CONTROL FLAG KONTRL.
	!       MAXMES IS THE MAXIMUM NUMBER OF TIMES ANY PARTICULAR MESSAGE
	!          SHOULD BE PRINTED.
	!
	!       WE PRINT A FATAL ERROR MESSAGE AND TERMINATE FOR AN ERROR IN
	!          CALLING XERMSG.  THE ERROR NUMBER SHOULD BE POSITIVE,
	!          AND THE LEVEL SHOULD BE BETWEEN 0 AND 2.
	!
	IF (NERR.LT.-9999999 .OR. NERR.GT.99999999 .OR. NERR.EQ.0 .OR.&
		LEVEL.LT.-1 .OR. LEVEL.GT.2) THEN
		CALL XERPRN (' ***', -1, 'FATAL ERROR IN...$$ ' //&
		'XERMSG -- INVALID ERROR NUMBER OR LEVEL$$ '//&
		'JOB ABORT DUE TO FATAL ERROR.', 72)
		CALL XERSVE (' ', ' ', ' ', 0, 0, 0, KDUMMY)
		CALL XERHLT (' ***XERMSG -- INVALID INPUT')
		RETURN
	ENDIF
	!
	!       RECORD THE MESSAGE.
	!
	I = J4SAVE (1, NERR, .TRUE.)
	CALL XERSVE (LIBRAR, SUBROU, MESSG, 1, NERR, LEVEL, KOUNT)
	!
	!       HANDLE PRINT-ONCE WARNING MESSAGES.
	!
	IF (LEVEL.EQ.-1 .AND. KOUNT.GT.1) RETURN
	!
	!       ALLOW TEMPORARY USER OVERRIDE OF THE CONTROL FLAG.
	!
	XLIBR  = LIBRAR
	XSUBR  = SUBROU
	LFIRST = MESSG
	LERR   = NERR
	LLEVEL = LEVEL
	CALL XERCNT (XLIBR, XSUBR, LFIRST, LERR, LLEVEL, LKNTRL)
	!
	LKNTRL = MAX(-2, MIN(2,LKNTRL))
	MKNTRL = ABS(LKNTRL)
	!
	!       SKIP PRINTING IF THE CONTROL FLAG VALUE AS RESET IN XERCNT IS
	!       ZERO AND THE ERROR IS NOT FATAL.
	!
	IF (LEVEL.LT.2 .AND. LKNTRL.EQ.0) return ! add by @creaqi goto return clb is_return
	IF (LEVEL.EQ.0 .AND. KOUNT.GT.MAXMES) return ! add by @creaqi goto return clb is_return
	IF (LEVEL.EQ.1 .AND. KOUNT.GT.MAXMES .AND. MKNTRL.EQ.1) return ! add by @creaqi goto return clb is_return
	IF (LEVEL.EQ.2 .AND. KOUNT.GT.MAX(1,MAXMES)) return ! add by @creaqi goto return clb is_return
	!
	!       ANNOUNCE THE NAMES OF THE LIBRARY AND SUBROUTINE BY BUILDING A
	!       MESSAGE IN CHARACTER VARIABLE TEMP (NOT EXCEEDING 66 CHARACTERS)
	!       AND SENDING IT OUT VIA XERPRN.  PRINT ONLY IF CONTROL FLAG
	!       IS NOT ZERO.
	!
	IF (LKNTRL .NE. 0) THEN
		TEMP(1:21) = 'MESSAGE FROM ROUTINE '
		I = MIN(LEN(SUBROU), 16)
		TEMP(22:21+I) = SUBROU(1:I)
		TEMP(22+I:33+I) = ' IN LIBRARY '
		LTEMP = 33 + I
		I = MIN(LEN(LIBRAR), 16)
		TEMP(LTEMP+1:LTEMP+I) = LIBRAR (1:I)
		TEMP(LTEMP+I+1:LTEMP+I+1) = '.'
		LTEMP = LTEMP + I + 1
		CALL XERPRN (' ***', -1, TEMP(1:LTEMP), 72)
	ENDIF
	!
	!       IF LKNTRL IS POSITIVE, PRINT AN INTRODUCTORY LINE BEFORE
	!       PRINTING THE MESSAGE.  THE INTRODUCTORY LINE TELLS THE CHOICE
	!       FROM EACH OF THE FOLLOWING THREE OPTIONS.
	!       1.  LEVEL OF THE MESSAGE
	!              'INFORMATIVE MESSAGE'
	!              'POTENTIALLY RECOVERABLE ERROR'
	!              'FATAL ERROR'
	!       2.  WHETHER CONTROL FLAG WILL ALLOW PROGRAM TO CONTINUE
	!              'PROG CONTINUES'
	!              'PROG ABORTED'
	!       3.  WHETHER OR NOT A TRACEBACK WAS REQUESTED.  (THE TRACEBACK
	!           MAY NOT BE IMPLEMENTED AT SOME SITES, SO THIS ONLY TELLS
	!           WHAT WAS REQUESTED, NOT WHAT WAS DELIVERED.)
	!              'TRACEBACK REQUESTED'
	!              'TRACEBACK NOT REQUESTED'
	!       NOTICE THAT THE LINE INCLUDING FOUR PREFIX CHARACTERS WILL NOT
	!       EXCEED 74 CHARACTERS.
	!       WE SKIP THE NEXT BLOCK IF THE INTRODUCTORY LINE IS NOT NEEDED.
	!
	IF (LKNTRL .GT. 0) THEN
		!
		!       THE FIRST PART OF THE MESSAGE TELLS ABOUT THE LEVEL.
		!
		IF (LEVEL .LE. 0) THEN
			TEMP(1:20) = 'INFORMATIVE MESSAGE,'
			LTEMP = 20
		ELSEIF (LEVEL .EQ. 1) THEN
			TEMP(1:30) = 'POTENTIALLY RECOVERABLE ERROR,'
			LTEMP = 30
		ELSE
			TEMP(1:12) = 'FATAL ERROR,'
			LTEMP = 12
		ENDIF
		!
		!       THEN WHETHER THE PROGRAM WILL CONTINUE.
		!
		IF ((MKNTRL.EQ.2 .AND. LEVEL.GE.1) .OR.&
			(MKNTRL.EQ.1 .AND. LEVEL.EQ.2)) THEN
			TEMP(LTEMP+1:LTEMP+14) = ' PROG ABORTED,'
			LTEMP = LTEMP + 14
		ELSE
			TEMP(LTEMP+1:LTEMP+16) = ' PROG CONTINUES,'
			LTEMP = LTEMP + 16
		ENDIF
		!
		!       FINALLY TELL WHETHER THERE SHOULD BE A TRACEBACK.
		!
		IF (LKNTRL .GT. 0) THEN
			TEMP(LTEMP+1:LTEMP+20) = ' TRACEBACK REQUESTED'
			LTEMP = LTEMP + 20
		ELSE
			TEMP(LTEMP+1:LTEMP+24) = ' TRACEBACK NOT REQUESTED'
			LTEMP = LTEMP + 24
		ENDIF
		CALL XERPRN (' ***', -1, TEMP(1:LTEMP), 72)
	ENDIF
	!
	!       NOW SEND OUT THE MESSAGE.
	!
	CALL XERPRN (' *  ', -1, MESSG, 72)
	!
	!       IF LKNTRL IS POSITIVE, WRITE THE ERROR NUMBER AND REQUEST A
	!          TRACEBACK.
	!
	IF (LKNTRL .GT. 0) THEN
		WRITE (TEMP, '(''ERROR NUMBER = '', I8)') NERR
		DO 10 I=16,22
			IF (TEMP(I:I) .NE. ' ') exit ! add by @creaqi break the loop goto 20 in XERMSG with len equal 1
			10    CONTINUE
			!
			20    CALL XERPRN (' *  ', -1, TEMP(1:15) // TEMP(I:23), 72)
			CALL FDUMP
		ENDIF
		!
		!       IF LKNTRL IS NOT ZERO, PRINT A BLANK LINE AND AN END OF MESSAGE.
		!
		IF (LKNTRL .NE. 0) THEN
			CALL XERPRN (' *  ', -1, ' ', 72)
			CALL XERPRN (' ***', -1, 'END OF MESSAGE', 72)
			CALL XERPRN ('    ',  0, ' ', 72)
		ENDIF
		!
		!       IF THE ERROR IS NOT FATAL OR THE ERROR IS RECOVERABLE AND THE
		!       CONTROL FLAG IS SET FOR RECOVERY, THEN RETURN.
		!
		30 IF (LEVEL.LE.0 .OR. (LEVEL.EQ.1 .AND. MKNTRL.LE.1)) RETURN
		!
		!       THE PROGRAM WILL BE STOPPED DUE TO AN UNRECOVERED ERROR OR A
		!       FATAL ERROR.  PRINT THE REASON FOR THE ABORT AND THE ERROR
		!       SUMMARY IF THE CONTROL FLAG AND THE MAXIMUM ERROR COUNT PERMIT.
		!
		IF (LKNTRL.GT.0 .AND. KOUNT.LT.MAX(1,MAXMES)) THEN
			IF (LEVEL .EQ. 1) THEN
				CALL XERPRN&
				(' ***', -1, 'JOB ABORT DUE TO UNRECOVERED ERROR.', 72)
			ELSE
				CALL XERPRN(' ***', -1, 'JOB ABORT DUE TO FATAL ERROR.', 72)
			ENDIF
			CALL XERSVE (' ', ' ', ' ', -1, 0, 0, KDUMMY)
			CALL XERHLT (' ')
		ELSE
			CALL XERHLT (MESSG)
		ENDIF
		RETURN
END
	!DECK XERPRN
SUBROUTINE XERPRN (PREFIX, NPREF, MESSG, NWRAP)
	!***BEGIN PROLOGUE  XERPRN
	!***SUBSIDIARY
	!***PURPOSE  Print error messages processed by XERMSG.
	!***LIBRARY   SLATEC (XERROR)
	!***CATEGORY  R3C
	!***TYPE      ALL (XERPRN-A)
	!***KEYWORDS  ERROR MESSAGES, PRINTING, XERROR
	!***AUTHOR  Fong, Kirby, (NMFECC at LLNL)
	!***DESCRIPTION
	!
	! This routine sends one or more lines to each of the (up to five)
	! logical units to which error messages are to be sent.  This routine
	! is called several times by XERMSG, sometimes with a single line to
	! print and sometimes with a (potentially very long) message that may
	! wrap around into multiple lines.
	!
	! PREFIX  Input argument of type CHARACTER.  This argument contains
	!         characters to be put at the beginning of each line before
	!         the body of the message.  No more than 16 characters of
	!         PREFIX will be used.
	!
	! NPREF   Input argument of type INTEGER.  This argument is the number
	!         of characters to use from PREFIX.  If it is negative, the
	!         intrinsic function LEN is used to determine its length.  If
	!         it is zero, PREFIX is not used.  If it exceeds 16 or if
	!         LEN(PREFIX) exceeds 16, only the first 16 characters will be
	!         used.  If NPREF is positive and the length of PREFIX is less
	!         than NPREF, a copy of PREFIX extended with blanks to length
	!         NPREF will be used.
	!
	! MESSG   Input argument of type CHARACTER.  This is the text of a
	!         message to be printed.  If it is a long message, it will be
	!         broken into pieces for printing on multiple lines.  Each line
	!         will start with the appropriate prefix and be followed by a
	!         piece of the message.  NWRAP is the number of characters per
	!         piece; that is, after each NWRAP characters, we break and
	!         start a new line.  In addition the characters '$$' embedded
	!         in MESSG are a sentinel for a new line.  The counting of
	!         characters up to NWRAP starts over for each new line.  The
	!         value of NWRAP typically used by XERMSG is 72 since many
	!         older error messages in the SLATEC Library are laid out to
	!         rely on wrap-around every 72 characters.
	!
	! NWRAP   Input argument of type INTEGER.  This gives the maximum size
	!         piece into which to break MESSG for printing on multiple
	!         lines.  An embedded '$$' ends a line, and the count restarts
	!         at the following character.  If a line break does not occur
	!         on a blank (it would split a word) that word is moved to the
	!         next line.  Values of NWRAP less than 16 will be treated as
	!         16.  Values of NWRAP greater than 132 will be treated as 132.
	!         The actual line length will be NPREF + NWRAP after NPREF has
	!         been adjusted to fall between 0 and 16 and NWRAP has been
	!         adjusted to fall between 16 and 132.
	!
	!***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
	!                 Error-handling Package, SAND82-0800, Sandia
	!                 Laboratories, 1982.
	!***ROUTINES CALLED  I1MACH, XGETUA
	!***REVISION HISTORY  (YYMMDD)
	!   880621  DATE WRITTEN
	!   880708  REVISED AFTER THE SLATEC CML SUBCOMMITTEE MEETING OF
	!           JUNE 29 AND 30 TO CHANGE THE NAME TO XERPRN AND TO REWORK
	!           THE HANDLING OF THE NEW LINE SENTINEL TO BEHAVE LIKE THE
	!           SLASH CHARACTER IN FORMAT STATEMENTS.
	!   890706  REVISED WITH THE HELP OF FRED FRITSCH AND REG CLEMENS TO
	!           STREAMLINE THE CODING AND FIX A BUG THAT CAUSED EXTRA BLANK
	!           LINES TO BE PRINTED.
	!   890721  REVISED TO ADD A NEW FEATURE.  A NEGATIVE VALUE OF NPREF
	!           CAUSES LEN(PREFIX) TO BE USED AS THE LENGTH.
	!   891013  REVISED TO CORRECT ERROR IN CALCULATING PREFIX LENGTH.
	!   891214  Prologue converted to Version 4.0 format.  (WRB)
	!   900510  Added code to break messages between words.  (RWC)
	!   920501  Reformatted the REFERENCES section.  (WRB)
	!***END PROLOGUE  XERPRN
	CHARACTER*(*) PREFIX, MESSG
	INTEGER NPREF, NWRAP
	CHARACTER*148 CBUFF
	INTEGER IU(5), NUNIT
	CHARACTER*2 NEWLIN
	PARAMETER (NEWLIN = '$$')
	!***FIRST EXECUTABLE STATEMENT  XERPRN
	CALL XGETUA(IU,NUNIT)
	!
	!       A ZERO VALUE FOR A LOGICAL UNIT NUMBER MEANS TO USE THE STANDARD
	!       ERROR MESSAGE UNIT INSTEAD.  I1MACH(4) RETRIEVES THE STANDARD
	!       ERROR MESSAGE UNIT.
	!
	N = I1MACH(4)
	DO I=1,NUNIT! add by @creaqi do label 10
		IF (IU(I) .EQ. 0) IU(I) = N
	enddo !add by @creaqi 10
	10 CONTINUE
	!
	!       LPREF IS THE LENGTH OF THE PREFIX.  THE PREFIX IS PLACED AT THE
	!       BEGINNING OF CBUFF, THE CHARACTER BUFFER, AND KEPT THERE DURING
	!       THE REST OF THIS ROUTINE.
	!
	IF ( NPREF .LT. 0 ) THEN
		LPREF = LEN(PREFIX)
	ELSE
		LPREF = NPREF
	ENDIF
	LPREF = MIN(16, LPREF)
	IF (LPREF .NE. 0) CBUFF(1:LPREF) = PREFIX
	!
	!       LWRAP IS THE MAXIMUM NUMBER OF CHARACTERS WE WANT TO TAKE AT ONE
	!       TIME FROM MESSG TO PRINT ON ONE LINE.
	!
	LWRAP = MAX(16, MIN(132, NWRAP))
	!
	!       SET LENMSG TO THE LENGTH OF MESSG, IGNORE ANY TRAILING BLANKS.
	!
	LENMSG = LEN(MESSG)
	N = LENMSG
	DO 20 I=1,N
		IF (MESSG(LENMSG:LENMSG) .NE. ' ') exit ! add by @creaqi break the loop goto 30 in XERPRN with len equal 1
		LENMSG = LENMSG - 1
		20 CONTINUE
		30 CONTINUE
		!
		!       IF THE MESSAGE IS ALL BLANKS, THEN PRINT ONE BLANK LINE.
		!
		IF (LENMSG .EQ. 0) THEN
			CBUFF(LPREF+1:LPREF+1) = ' '
			DO I=1,NUNIT! add by @creaqi do label 40
				WRITE(IU(I), '(A)') CBUFF(1:LPREF+1)
			enddo !add by @creaqi 40
			40    CONTINUE
			RETURN
		ENDIF
		!
		!       SET NEXTC TO THE POSITION IN MESSG WHERE THE NEXT SUBSTRING
		!       STARTS.  FROM THIS POSITION WE SCAN FOR THE NEW LINE SENTINEL.
		!       WHEN NEXTC EXCEEDS LENMSG, THERE IS NO MORE TO PRINT.
		!       WE LOOP BACK TO LABEL 50 UNTIL ALL PIECES HAVE BEEN PRINTED.
		!
		!       WE LOOK FOR THE NEXT OCCURRENCE OF THE NEW LINE SENTINEL.  THE
		!       INDEX INTRINSIC FUNCTION RETURNS ZERO IF THERE IS NO OCCURRENCE
		!       OR IF THE LENGTH OF THE FIRST ARGUMENT IS LESS THAN THE LENGTH
		!       OF THE SECOND ARGUMENT.
		!
		!       THERE ARE SEVERAL CASES WHICH SHOULD BE CHECKED FOR IN THE
		!       FOLLOWING ORDER.  WE ARE ATTEMPTING TO SET LPIECE TO THE NUMBER
		!       OF CHARACTERS THAT SHOULD BE TAKEN FROM MESSG STARTING AT
		!       POSITION NEXTC.
		!
		!       LPIECE .EQ. 0   THE NEW LINE SENTINEL DOES NOT OCCUR IN THE
		!                       REMAINDER OF THE CHARACTER STRING.  LPIECE
		!                       SHOULD BE SET TO LWRAP OR LENMSG+1-NEXTC,
		!                       WHICHEVER IS LESS.
		!
		!       LPIECE .EQ. 1   THE NEW LINE SENTINEL STARTS AT MESSG(NEXTC:
		!                       NEXTC).  LPIECE IS EFFECTIVELY ZERO, AND WE
		!                       PRINT NOTHING TO AVOID PRODUCING UNNECESSARY
		!                       BLANK LINES.  THIS TAKES CARE OF THE SITUATION
		!                       WHERE THE LIBRARY ROUTINE HAS A MESSAGE OF
		!                       EXACTLY 72 CHARACTERS FOLLOWED BY A NEW LINE
		!                       SENTINEL FOLLOWED BY MORE CHARACTERS.  NEXTC
		!                       SHOULD BE INCREMENTED BY 2.
		!
		!       LPIECE .GT. LWRAP+1  REDUCE LPIECE TO LWRAP.
		!
		!       ELSE            THIS LAST CASE MEANS 2 .LE. LPIECE .LE. LWRAP+1
		!                       RESET LPIECE = LPIECE-1.  NOTE THAT THIS
		!                       PROPERLY HANDLES THE END CASE WHERE LPIECE .EQ.
		!                       LWRAP+1.  THAT IS, THE SENTINEL FALLS EXACTLY
		!                       AT THE END OF A LINE.
		!
		NEXTC = 1
		do ! insert do to replace label 50, add by @creaqi 2020-02-18 11:30:06.236823
			do ! insert do to replace label 50, add by @creaqi 2020-02-18 11:30:06.237140
				50 LPIECE = INDEX(MESSG(NEXTC:LENMSG), NEWLIN)
				IF (LPIECE .EQ. 0) THEN
					!
					!       THERE WAS NO NEW LINE SENTINEL FOUND.
					!
					IDELTA = 0
					LPIECE = MIN(LWRAP, LENMSG+1-NEXTC)
					IF (LPIECE .LT. LENMSG+1-NEXTC) THEN
						label52:            do  I=LPIECE+1,2,-1
							IF (MESSG(NEXTC+I-1:NEXTC+I-1) .EQ. ' ') THEN
								LPIECE = I-1
								IDELTA = 1
								!                  GOTO 54
								lgoto54_0=.true. !add by @creaqi flag 2
								exit label52! add by @creaqi flag 2
							ENDIF
						enddo label52! 52 continue flag 2   label52
					ENDIF
					54    CBUFF(LPREF+1:LPREF+LPIECE) = MESSG(NEXTC:NEXTC+LPIECE-1)
					NEXTC = NEXTC + LPIECE + IDELTA
				ELSEIF (LPIECE .EQ. 1) THEN
					!
					!       WE HAVE A NEW LINE SENTINEL AT MESSG(NEXTC:NEXTC+1).
					!       DON'T PRINT A BLANK LINE.
					!
					NEXTC = NEXTC + 2
					! goto 50 add by @creaqi 2020-02-18 11:30:06.236823
				ELSEIF (LPIECE .GT. LWRAP+1) THEN
					!
					!       LPIECE SHOULD BE SET DOWN TO LWRAP.
					!
					IDELTA = 0
					LPIECE = LWRAP
					DO 56 I=LPIECE+1,2,-1
						IF (MESSG(NEXTC+I-1:NEXTC+I-1) .EQ. ' ') THEN
							LPIECE = I-1
							IDELTA = 1
							exit ! add by @creaqi break the loop goto 58 in XERPRN with len equal 1
						ENDIF
						56    CONTINUE
						58    CBUFF(LPREF+1:LPREF+LPIECE) = MESSG(NEXTC:NEXTC+LPIECE-1)
						NEXTC = NEXTC + LPIECE + IDELTA
					ELSE
						!
						!       IF WE ARRIVE HERE, IT MEANS 2 .LE. LPIECE .LE. LWRAP+1.
						!       WE SHOULD DECREMENT LPIECE BY ONE.
						!
						LPIECE = LPIECE - 1
						CBUFF(LPREF+1:LPREF+LPIECE) = MESSG(NEXTC:NEXTC+LPIECE-1)
						NEXTC  = NEXTC + LPIECE + 2
					ENDIF
					!
					!       PRINT
					!
				enddo ! insert enddo to replace goto [50 (shift down 28), add by @creaqi 2020-02-18 11:30:06.236823
				DO I=1,NUNIT! add by @creaqi do label 60
					WRITE(IU(I), '(A)') CBUFF(1:LPREF+LPIECE)
				enddo !add by @creaqi 60
				60 CONTINUE
				!
			enddo ! insert enddo to replace goto [50, add by @creaqi 2020-02-18 11:30:06.237140
			! goto 50 add by @creaqi 2020-02-18 11:30:06.237140
			RETURN
END
		!DECK XERSVE
SUBROUTINE XERSVE (LIBRAR, SUBROU, MESSG, KFLAG, NERR, LEVEL,&
	ICOUNT)
	!***BEGIN PROLOGUE  XERSVE
	!***SUBSIDIARY
	!***PURPOSE  Record that an error has occurred.
	!***LIBRARY   SLATEC (XERROR)
	!***CATEGORY  R3
	!***TYPE      ALL (XERSVE-A)
	!***KEYWORDS  ERROR, XERROR
	!***AUTHOR  Jones, R. E., (SNLA)
	!***DESCRIPTION
	!
	! *Usage:
	!
	!        INTEGER  KFLAG, NERR, LEVEL, ICOUNT
	!        CHARACTER * (len) LIBRAR, SUBROU, MESSG
	!
	!        CALL XERSVE (LIBRAR, SUBROU, MESSG, KFLAG, NERR, LEVEL, ICOUNT)
	!
	! *Arguments:
	!
	!        LIBRAR :IN    is the library that the message is from.
	!        SUBROU :IN    is the subroutine that the message is from.
	!        MESSG  :IN    is the message to be saved.
	!        KFLAG  :IN    indicates the action to be performed.
	!                      when KFLAG > 0, the message in MESSG is saved.
	!                      when KFLAG=0 the tables will be dumped and
	!                      cleared.
	!                      when KFLAG < 0, the tables will be dumped and
	!                      not cleared.
	!        NERR   :IN    is the error number.
	!        LEVEL  :IN    is the error severity.
	!        ICOUNT :OUT   the number of times this message has been seen,
	!                      or zero if the table has overflowed and does not
	!                      contain this message specifically.  When KFLAG=0,
	!                      ICOUNT will not be altered.
	!
	! *Description:
	!
	!   Record that this error occurred and possibly dump and clear the
	!   tables.
	!
	!***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
	!                 Error-handling Package, SAND82-0800, Sandia
	!                 Laboratories, 1982.
	!***ROUTINES CALLED  I1MACH, XGETUA
	!***REVISION HISTORY  (YYMMDD)
	!   800319  DATE WRITTEN
	!   861211  REVISION DATE from Version 3.2
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900413  Routine modified to remove reference to KFLAG.  (WRB)
	!   900510  Changed to add LIBRARY NAME and SUBROUTINE to calling
	!           sequence, use IF-THEN-ELSE, make number of saved entries
	!           easily changeable, changed routine name from XERSAV to
	!           XERSVE.  (RWC)
	!   910626  Added LIBTAB and SUBTAB to SAVE statement.  (BKS)
	!   920501  Reformatted the REFERENCES section.  (WRB)
	!***END PROLOGUE  XERSVE
	PARAMETER (LENTAB=10)
	INTEGER LUN(5)
	CHARACTER*(*) LIBRAR, SUBROU, MESSG
	CHARACTER*8  LIBTAB(LENTAB), SUBTAB(LENTAB), LIB, SUB
	CHARACTER*20 MESTAB(LENTAB), MES
	DIMENSION NERTAB(LENTAB), LEVTAB(LENTAB), KOUNT(LENTAB)
	SAVE LIBTAB, SUBTAB, MESTAB, NERTAB, LEVTAB, KOUNT, KOUNTX, NMSG
	DATA KOUNTX/0/, NMSG/0/
	!***FIRST EXECUTABLE STATEMENT  XERSVE
	!
	IF (KFLAG.LE.0) THEN
		!
		!        Dump the table.
		!
		IF (NMSG.EQ.0) RETURN
		!
		!        Print to each unit.
		!
		CALL XGETUA (LUN, NUNIT)
		DO KUNIT = 1,NUNIT! add by @creaqi do label 20
			IUNIT = LUN(KUNIT)
			IF (IUNIT.EQ.0) IUNIT = I1MACH(4)
			!
			!           Print the table header.
			!
			WRITE (IUNIT,9000)
			!
			!           Print body of table.
			!
			DO I = 1,NMSG! add by @creaqi do label 10
				WRITE (IUNIT,9010) LIBTAB(I), SUBTAB(I), MESTAB(I),&
				NERTAB(I),LEVTAB(I),KOUNT(I)
			enddo !add by @creaqi 10
			10       CONTINUE
			!
			!           Print number of other errors.
			!
			IF (KOUNTX.NE.0) WRITE (IUNIT,9020) KOUNTX
			WRITE (IUNIT,9030)
		enddo !add by @creaqi 20
		20    CONTINUE
		!
		!        Clear the error tables.
		!
		IF (KFLAG.EQ.0) THEN
			NMSG = 0
			KOUNTX = 0
		ENDIF
	ELSE
		!
		!        PROCESS A MESSAGE...
		!        SEARCH FOR THIS MESSG, OR ELSE AN EMPTY SLOT FOR THIS MESSG,
		!        OR ELSE DETERMINE THAT THE ERROR TABLE IS FULL.
		!
		LIB = LIBRAR
		SUB = SUBROU
		MES = MESSG
		DO I = 1,NMSG! add by @creaqi do label 30
			IF (LIB.EQ.LIBTAB(I) .AND. SUB.EQ.SUBTAB(I) .AND.&
				MES.EQ.MESTAB(I) .AND. NERR.EQ.NERTAB(I) .AND.&
				LEVEL.EQ.LEVTAB(I)) THEN
				KOUNT(I) = KOUNT(I) + 1
				ICOUNT = KOUNT(I)
				RETURN
			ENDIF
		enddo !add by @creaqi 30
		30    CONTINUE
		!
		IF (NMSG.LT.LENTAB) THEN
			!
			!           Empty slot found for new message.
			!
			NMSG = NMSG + 1
			LIBTAB(I) = LIB
			SUBTAB(I) = SUB
			MESTAB(I) = MES
			NERTAB(I) = NERR
			LEVTAB(I) = LEVEL
			KOUNT (I) = 1
			ICOUNT    = 1
		ELSE
			!
			!           Table is full.
			!
			KOUNTX = KOUNTX+1
			ICOUNT = 0
		ENDIF
	ENDIF
	RETURN
	!
	!     Formats.
	!
	9000 FORMAT ('0          ERROR MESSAGE SUMMARY' /&
	' LIBRARY    SUBROUTINE MESSAGE START             NERR',&
	'     LEVEL     COUNT')
	9010 FORMAT (1X,A,3X,A,3X,A,3I10)
	9020 FORMAT ('0OTHER ERRORS NOT INDIVIDUALLY TABULATED = ', I10)
	9030 FORMAT (1X)
END
!DECK XGETUA
SUBROUTINE XGETUA (IUNITA, N)
	!***BEGIN PROLOGUE  XGETUA
	!***PURPOSE  Return unit number(s) to which error messages are being
	!            sent.
	!***LIBRARY   SLATEC (XERROR)
	!***CATEGORY  R3C
	!***TYPE      ALL (XGETUA-A)
	!***KEYWORDS  ERROR, XERROR
	!***AUTHOR  Jones, R. E., (SNLA)
	!***DESCRIPTION
	!
	!     Abstract
	!        XGETUA may be called to determine the unit number or numbers
	!        to which error messages are being sent.
	!        These unit numbers may have been set by a call to XSETUN,
	!        or a call to XSETUA, or may be a default value.
	!
	!     Description of Parameters
	!      --Output--
	!        IUNIT - an array of one to five unit numbers, depending
	!                on the value of N.  A value of zero refers to the
	!                default unit, as defined by the I1MACH machine
	!                constant routine.  Only IUNIT(1),...,IUNIT(N) are
	!                defined by XGETUA.  The values of IUNIT(N+1),...,
	!                IUNIT(5) are not defined (for N .LT. 5) or altered
	!                in any way by XGETUA.
	!        N     - the number of units to which copies of the
	!                error messages are being sent.  N will be in the
	!                range from 1 to 5.
	!
	!***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
	!                 Error-handling Package, SAND82-0800, Sandia
	!                 Laboratories, 1982.
	!***ROUTINES CALLED  J4SAVE
	!***REVISION HISTORY  (YYMMDD)
	!   790801  DATE WRITTEN
	!   861211  REVISION DATE from Version 3.2
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   920501  Reformatted the REFERENCES section.  (WRB)
	!***END PROLOGUE  XGETUA
	DIMENSION IUNITA(5)
	!***FIRST EXECUTABLE STATEMENT  XGETUA
	N = J4SAVE(5,0,.FALSE.)
	DO I=1,N! add by @creaqi do label 30
		INDEX = I+4
		IF (I.EQ.1) INDEX = 3
		IUNITA(I) = J4SAVE(INDEX,0,.FALSE.)
	enddo !add by @creaqi 30
	30 CONTINUE
	RETURN
END
!DECK XSETF
SUBROUTINE XSETF (KONTRL)
	!***BEGIN PROLOGUE  XSETF
	!***PURPOSE  Set the error control flag.
	!***LIBRARY   SLATEC (XERROR)
	!***CATEGORY  R3A
	!***TYPE      ALL (XSETF-A)
	!***KEYWORDS  ERROR, XERROR
	!***AUTHOR  Jones, R. E., (SNLA)
	!***DESCRIPTION
	!
	!     Abstract
	!        XSETF sets the error control flag value to KONTRL.
	!        (KONTRL is an input parameter only.)
	!        The following table shows how each message is treated,
	!        depending on the values of KONTRL and LEVEL.  (See XERMSG
	!        for description of LEVEL.)
	!
	!        If KONTRL is zero or negative, no information other than the
	!        message itself (including numeric values, if any) will be
	!        printed.  If KONTRL is positive, introductory messages,
	!        trace-backs, etc., will be printed in addition to the message.
	!
	!              ABS(KONTRL)
	!        LEVEL        0              1              2
	!        value
	!          2        fatal          fatal          fatal
	!
	!          1     not printed      printed         fatal
	!
	!          0     not printed      printed        printed
	!
	!         -1     not printed      printed        printed
	!                                  only           only
	!                                  once           once
	!
	!***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
	!                 Error-handling Package, SAND82-0800, Sandia
	!                 Laboratories, 1982.
	!***ROUTINES CALLED  J4SAVE, XERMSG
	!***REVISION HISTORY  (YYMMDD)
	!   790801  DATE WRITTEN
	!   890531  Changed all specific intrinsics to generic.  (WRB)
	!   890531  REVISION DATE from Version 3.2
	!   891214  Prologue converted to Version 4.0 format.  (BAB)
	!   900510  Change call to XERRWV to XERMSG.  (RWC)
	!   920501  Reformatted the REFERENCES section.  (WRB)
	!***END PROLOGUE  XSETF
	CHARACTER *8 XERN1
	!***FIRST EXECUTABLE STATEMENT  XSETF
	IF (ABS(KONTRL) .GT. 2) THEN
		WRITE (XERN1, '(I8)') KONTRL
		CALL XERMSG ('SLATEC', 'XSETF',&
		'INVALID ARGUMENT = ' // XERN1, 1, 2)
		RETURN
	ENDIF
	!
	JUNK = J4SAVE(2,KONTRL,.TRUE.)
	RETURN
END
!----------------------------------------------------------------------
subroutine PDFVARS(ldb,io,mdisp,icode,sigw,wstar,ustar,&
	bidsq,fb,zi,hs,u)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 971107           PDFVARS
	!                D. Strimaitis,   Earth Tech
	!
	! --- PURPOSE:  Computes variables associated with the PDF formulation
	!               for the convective boundary layer (CBL).  Currently
	!               based on Weil (JAM, 1996 -- draft)
	!
	! --- INPUTS:
	!
	!               LDB - logical - Debug output generated when .TRUE.
	!                IO - integer - Unit number for output
	!             MDISP - integer - Dispersion code (see /PUFF/)
	!             ICODE - integer - Puff code  (see /PUFF/)
	!              SIGW - real    - Sigma-w (m/s)
	!             WSTAR - real    - Convective velocity scale w* (m/s)
	!             USTAR - real    - Shear velocity scale u* (m/s)
	!             BIDSQ - real    - Sigma**2 due to BID (m2)
	!                FB - real    - Plume buoyancy flux (m4/s3)
	!                ZI - real    - Mixed layer ht (m)
	!                HS - real    - Source ht (m)
	!                 U - real    - Effective advection speed (m/s)
	!
	!
	! --- OUTPUTS:
	!
	!     Common block /PDF/ variables:
	!              LPDF - logical - PDF computation active when .TRUE.
	!             SWUPF - real    - Updraft (sigma-w / full sigma-w)**2
	!             SWDNF - real    - Downdraft (sigma-w / full sigma-w)**2
	!             SZUPB - real    - Updraft BID**2 term (m2)
	!             SZDNB - real    - Downdraft BID**2 term (m2)
	!               WUP - real    - Updraft w (indirect path)(m/s)
	!               WDN - real    - Downdraft w (direct path)(m/s)
	!              WTUP - real    - Updraft path weight (indirect path)
	!              WTDN - real    - Downdraft path weight (direct path)
	!              RISQ - real    - Initial plume radius (squared) at CBL
	!                               top (m^2)
	!               RYZ - real    - Growth rate of elliptical plume
	!                               crossection (m**2/s**2)
	!             DHFAC - real    - Effective rise factor, where effective
	!                               rise given by t*dhfac/SQRT(risq+ryz*t^2)
	!
	! --- PDFVARS called by:  COMP
	! --- PDFVARS calls:      none
	!----------------------------------------------------------------------
	include 'pdf.puf'
	logical ldb
	data zero/0.0/,half/0.5/,one/1.0/,two/2.0/,three/3.0/,four/4.0/
	! --- Set sigmaw/w ratio to 2.0
	data r/2.0/
	! --- Set parameters for lofting rise [rconst=(0.1*(2.3)^1.5)/4]
	data alpha/1.4/, rconst/0.087203/, beta2/0.4/
	! --- Initialize /PDF/ variables only when turb option is used, and
	! --- w* is positive, and puff is Gaussian and in Mixed Layer
	lpdf=.FALSE.
	if(mdisp.EQ.1 .OR. mdisp.EQ.2 .OR. mdisp.EQ.5) then
		if(wstar.GT.zero .AND. MOD(icode,10).EQ.1) lpdf=.TRUE.
	endif
	if(.not.LPDF) return
	! --- Skewness designed to go to zero as w* goes to zero --- Use bulk
	! --- turbulence properties of the CBL rather than profiled or measured
	! --- sigma-w:  sigma-w = SQRT( 1.2 ustar^2 + 0.31 wstar^2)
	! --- This assures skewness lies between 0.0 and 0.6
	swskew=SQRT(1.2*ustar**2 + 0.31*wstar**2)
	skew=0.105*(wstar/swskew)**3
	! --- Compute constants
	gam2=one+r**2
	gam1=gam2/(one+three*(gam2-one))
	g1skew=gam1*skew
	rdcl=SQRT(g1skew**2+four/gam2)
	! --- Compute updraft and downdraft velocity (scale with local sigma-w)
	wup=half*sigw*(g1skew+rdcl)
	wdn=half*sigw*(g1skew-rdcl)
	! --- Compute updraft and downdraft sigma-w scaling factors (squared)
	swupf=(r*wup/sigw)**2
	swdnf=(-r*wdn/sigw)**2
	! --- Compute updraft and downdraft BID**2 adjustment to sigma-z
	szupb=bidsq*(one-swupf)
	szdnb=bidsq*(one-swdnf)
	! --- Compute updraft and downdraft path weight
	wtup=wdn/(wdn-wup)
	wtdn=one-wtup
	! --- Establish parameters for computing the apparent rise ht that
	! --- "delays" the reflection of the indirect path from the lid to
	! --- simulate tendency of buoyant plumes to "stick" at top of layer
	! --- Initial plume radius (squared) at CBL top
	risq=(beta2*(zi-hs))**2
	! --- Growth rate of elliptical plume crossection (multiply by t**2)
	ryz=rconst*wstar**2
	! --- Effective rise factor
	dhfac=SQRT(two*fb*zi/(alpha*u))
	if(LDB) then
		write(io,*) 'PDFVARS ------------------'
		write(io,*) 'Inputs:'
		write(io,*) ' sw,w*,BIDsq = ',sigw,wstar,bidsq
		write(io,*) ' Fb,Hs,Zi,U  = ',fb,hs,zi,u
		write(io,*) 'Outputs:'
		write(io,*) ' risq,ryz,dhfac= ',risq,ryz,dhfac
		write(io,*) ' skew,w+,w-    = ',skew,wup,wdn
		write(io,*) ' swf+,swf-     = ',swupf,swdnf
		write(io,*) ' szb+,szb-     = ',szupb,szdnb
		write(io,*) ' wt+,wt-       = ',wtup,wtdn
	endif
	return
end
!----------------------------------------------------------------------
subroutine PDFPATH(zp,zi,t)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 050128           PDFPATH
	!                D. Strimaitis,   Earth Tech
	!
	! --- PURPOSE:  Computes the vertical distribution factor for the
	!               combined direct and indirect path of the PDF formulation
	!
	! --- UPDATES
	! --- V5.0-V5.725   050128  (DGS): Add test for a zero lofting rise
	!                                  adjustment to avoid a potential
	!                                  attempt to divide by zero.
	!
	! --- INPUTS:
	!
	!                ZP - real    - Receptor-specific puff ht (m)
	!                ZI - real    - Mixed layer ht (m)
	!                 T - real    - Time since release (s)
	!
	!     Common block /PDF/ variables:
	!               WUP - real    - Updraft w (indirect path)(m/s)
	!               WDN - real    - Downdraft w (direct path)(m/s)
	!              RISQ - real    - Initial plume radius (squared) at CBL
	!                               top (m^2)
	!               RYZ - real    - Growth rate of elliptical plume
	!                               crossection (m**2/s**2)
	!             DHFAC - real    - Effective rise factor, where effective
	!                               rise given by t*dhfac/SQRT(risq+ryz*t^2)
	!
	! --- OUTPUTS:
	!
	!     Common block /PDF/ variables:
	!               ZUP - real    - Updraft puff ht (m)
	!               ZDN - real    - Downdraft puff ht (m)
	!
	! --- PDFPATH called by:  CALCPF, CALCSL, AREAINT
	! --- PDFPATH calls:      none
	!----------------------------------------------------------------------
	include 'pdf.puf'
	logical losci
	data losci/.FALSE./
	data two/2.0/
	data zero/0.0/, half/0.5/, one/1.0/
	data pi/3.1415927/, piby2/1.5707963/
	twozi=two*zi
	ziby2=half*zi
	! --- Downdraft path
	! -------------------------------------------
	! --- Set ht ignoring reflection from surface (wdn<0)
	z1=AMIN1(zi,(zp+t*wdn))
	! --- Bouncing-ball mapping
	zdn=AMOD(ABS(z1),twozi)
	if(zdn.GT.zi) zdn=twozi-zdn
	if(losci) then
		! ---    Oscillator mapping (Future Developments)
		if(z1.LT.zero) then
			zdn=ziby2*(one+SIN(-piby2-pi*z1/zi))
		else
			zdn=z1
		endif
	endif
	! --- Updraft path
	! -------------------------------------------
	! --- Set ht ignoring possible lofting (wup>0)
	z1=zp+t*wup
	! --- Set lofting rise adjustment here
	! --- delh=t*dhfac/SQRT(risq+ryz*t**2)
	xdum1=t*dhfac
	xdum2=risq+ryz*t**2
	if(xdum1.LE.0.0) then
		delh=0.0
	elseif(xdum2.LE.0.0) then
		write(*,*)'PDFPATH ERROR:  Illegal Calculation'
		write(*,*)'risq,ryz,t = ',risq,ryz,t
		write(*,*)'risq+ryz*t**2 = ',xdum2
		stop
	else
		delh=xdum1/SQRT(xdum2)
	endif
	! --- Add lofting adjustment only if z1 exceeds zi
	if(z1.GT.zi) z1=AMAX1(zi,(z1-delh))
	! --- Bouncing-ball mapping
	zup=AMOD(z1,twozi)
	if(zup.GT.zi) zup=twozi-zup
	if(losci) then
		! ---    Oscillator mapping (Future Developments)
		if(z1.GT.zi) then
			zup=ziby2*(one+COS(pi*(z1-zi)/zi))
		else
			zup=z1
		endif
	endif
	return
end
!----------------------------------------------------------------------
subroutine restartq
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 080520          RESTARTQ
	! ---            D. Strimaitis
	!
	! --- PURPOSE:  Read and check (QA) header records of the RESTART file
	!
	! --- UPDATES
	! --- V6.22-V6.261  080520  (DGS): Replace source-ordered tabulations
	!                                  with puff-ordered tabulations used
	!                                  for the /SRCTAB/ arrays
	!                                  (Dataset 3.1 format)
	!                                  (HALT if older Dataset is read)
	! --- V6.1-V6.22    070921  (DGS): Introduce dataset name and version
	!                                  record
	!                                  Add source tabulations stored for
	!                                  previous met periods, resolved to
	!                                  seconds
	!                                  (Dataset 2.1 format)
	! --- V6.1-V6.21    070801  (DGS): Relax mismatch in code version/level
	!                                  to a warning (add WARNING pathway)
	! --- V5.7-V6.1     050915  (DGS): resolve times to the second and
	!                                  use explicit begin-time/end-time
	! --- V5.5-V5.7     030402  (DGS): Add list file unit to JULDAY, INCR
	! --- V5.3-V5.4     000602  (DGS): NVL2 replaces NX4,NY4
	! ---               000602  (DGS): add message to "stop"
	!
	! --- INPUTS:
	!
	!    Common block /DATASET/ variables:
	!         VERREST
	!    Common Block /GEN/ variables:
	!         IBDATHR, IBSEC, NSPEC
	!
	!    --- For QA
	!    Common Block /QA/ variables:
	!         VER, LEVEL
	!    Common block /AR1/ variables:
	!         NAR1
	!    Common block /AR2/ variables:
	!         NAR2
	!    Common block /LN1/ variables:
	!         NLINES
	!    Common block /LN2/ variables:
	!         NLN2
	!    Common block /PT1/ variables:
	!         NPT1
	!    Common block /PT2/ variables:
	!         NPT2
	!    Common block /VOL1/ variables:
	!         NVL1
	!    Common block /VOL2/ variables:
	!         NVL2
	!
	!    Parameters:
	!         MXPUFF, IO3, IO6
	!
	! --- OUTPUT:
	!
	!    Common block /PUFF/ variables:
	!         npuffs
	!    Common block /RESTART/ variables:
	!         rstrtnam,rstrtver,rstrtmod,
	!
	! --- RESTARTQ called by:  COMP
	! --- RESTARTQ calls:      none
	!----------------------------------------------------------------------
	include 'params.puf'
	include 'dataset.puf'
	include 'gen.puf'
	include 'puff.puf'
	include 'restarthd.puf'
	include 'ar1.puf'
	include 'ar2.puf'
	include 'ln1.puf'
	include 'ln2.puf'
	include 'pt1.puf'
	include 'pt2.puf'
	include 'qa.puf'
	include 'vol1.puf'
	include 'vol2.puf'
	character*12 ver0,level0
	logical problem, warning
	problem=.FALSE.
	warning=.FALSE.
	if(verrest.EQ.'3.1             ') then
		! ---    Dataset 3.1 with source tables for each puff
		! ---    Header 1 (dataset record)
		read(io3) rstrtnam,rstrtver,rstrtmod
		! ---    Header 2 (run info record)
		read(io3) ver0,level0,npuffs0,nspec0,ndathr0,nsec0,&
		nar10,nar20,nln10,nln20,npt10,npt20,nvl10,nvl20
	elseif(verrest.EQ.'2.1             ') then
		! ---    Dataset 2.1 with source tables from previous met periods
		! ---    Header 1 (dataset record)
		read(io3) rstrtnam,rstrtver,rstrtmod
		! ---    Header 2 (run info record)
		read(io3) ver0,level0,npuffs0,nspec0,ndathr0,nsec0,&
		nar10,nar20,nln10,nln20,npt10,npt20,nvl10,nvl20
		! ---    Header 3 (source table info record)
		read(io3) ntabpt1,nmetpt1,nsrcpt1,&
		ntabpt2,nmetpt2,nqempt2,nsrcpt2,&
		ntabar1,nmetar1,nsrcar1,&
		ntabar2,nmetar2,nqemar2,nsrcar2,&
		ntabln1,nmetln1,nsrcln1,&
		ntabln2,nmetln2,nqemln2,nsrcln2,&
		ntabvl1,nmetvl1,nsrcvl1,&
		ntabvl2,nmetvl2,nqemvl2,nsrcvl2
		problem=.TRUE.
	elseif(verrest.EQ.'1.0             ' .OR.&
		verrest.EQ.'1.6             ') then
		! ---    Header 1 (run info record)
		read(io3) ver0,level0,npuffs0,nspec0,ndathr0,nsec0,&
		nar10,nar20,nln10,nln20,npt10,npt20,nvl10,nvl20
		!         if(nln10.GT.0 .OR. nln20.GT.0) then
		!            write(io6,*)
		!            write(io6,*)'FATAL ERROR in RESTARTQ -- obsolete version'
		!            write(io6,*)'Source tables for previous met periods are'
		!            write(io6,*)'needed for buoyant line sources'
		!            write(io6,*)'Expected VERREST = 2.0'
		!            write(io6,*)'Found    VERREST = ',verrest
		!            write(*,*)
		!            stop 'Halted in RESTARTQ -- see list file.'
		!         endif
		problem=.TRUE.
	else
		write(io6,*)
		write(io6,*)'FATAL ERROR in RESTARTQ -- bad version marker'
		write(io6,*)'Expected VERREST = 1.0, 1.6, 2.1 or 3.1'
		write(io6,*)'Found    VERREST = ',verrest
		write(*,*)
		stop 'Halted in RESTARTQ -- see list file.'
	endif
	! --- Stop now if the dataset is not current
	if(problem) then
		write(io6,*)
		write(io6,*)'FATAL ERROR in RESTARTQ -- obsolete version'
		write(io6,*)'Source tables are needed for each puff'
		write(io6,*)'Expected VERREST = 3.1'
		write(io6,*)'Found    VERREST = ',verrest
		write(*,*)
		stop 'Halted in RESTARTQ -- see list file.'
	endif
	! --- Check against variables for current run
	if(ver.NE.ver0 .OR. level.NE.level0) warning=.TRUE.
	if(mxpuff.LE.npuffs0 .OR. nspec.NE.nspec0) problem=.TRUE.
	if(nar1.NE.nar10 .OR. nar2.NE.nar20) problem=.TRUE.
	if(nlines.NE.nln10 .OR. nln2.NE.nln20) problem=.TRUE.
	if(npt1.NE.npt10 .OR. npt2.NE.npt20) problem=.TRUE.
	if(nvl1.NE.nvl10 .OR. nvl2.NE.nvl20) problem=.TRUE.
	! --- Compare ending date-time stamp with beginning date-time of run
	if(ibdathr.NE.ndathr0) problem=.TRUE.
	if(ibsec.NE.nsec0) problem=.TRUE.
	if(warning) then
		! ---    Report data and continue
		write(io6,*)
		write(io6,*)'WARNING: Header difference in Restart File!'
		write(io6,*)' ---    Run Data   /  Restart Data'
		write(io6,*)'Version: ',ver,ver0
		write(io6,*)'Level  : ',level,level0
		write(io6,*)
		write(*,*)
		write(*,*) 'WARNING reported in RESTARTQ -- see list file.'
	endif
	if(problem) then
		! ---    Report data and quit
		write(io6,*)
		write(io6,*)'FATAL Problem in Restart File!'
		write(io6,*)' ---              Run Data    /    Restart Data'
		write(io6,*)'Version: ',ver,ver0
		write(io6,*)'Level  : ',level,level0
		write(io6,*)'Species: ',nspec,nspec0
		write(io6,*)'Npuff  : ',mxpuff,npuffs0
		write(io6,*)'DateHr : ',ibdathr,ndathr0
		write(io6,*)'Seconds: ',ibsec,nsec0
		write(io6,*)'Areas1 : ',nar1,nar10
		write(io6,*)'Areas2 : ',nar2,nar20
		write(io6,*)'Lines1 : ',nlines,nln10
		write(io6,*)'Lines2 : ',nln2,nln20
		write(io6,*)'Points1: ',npt1,npt10
		write(io6,*)'Points2: ',npt2,npt20
		write(io6,*)'Volume1: ',nvl1,nvl10
		write(io6,*)'Volume2: ',nvl2,nvl20
		write(io6,*)
		write(*,*)
		stop 'Halted in RESTARTQ -- see list file.'
	else
		npuffs=npuffs0
	endif
	return
end
!----------------------------------------------------------------------
subroutine restarto
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 080520          RESTARTO
	! ---            D. Strimaitis
	!
	! --- PURPOSE:  Output all data needed to restart CALPUFF with
	!               current puffs in the domain
	!
	! --- UPDATES
	! --- V6.22-V6.261  080520  (DGS): Replace source-ordered tabulations
	!                                  with puff-ordered tabulations used
	!                                  for the /SRCTAB/ arrays
	!                                  (Dataset 3.1 format)
	! --- V6.1-V6.22    070921  (DGS): Introduce dataset name and version
	!                                  record
	!                                  Add source tabulations stored for
	!                                  previous met periods
	!                                  (Dataset 2.1 format)
	! --- V5.722-V6.1   050915  (DGS): Resolve times to the second and use
	!                                  explicit begin-time/end-time format
	!                           (DGS): Add IEMSTEP array to /PUFF/
	! --- V5.72-V5.722  040610  (DGS): Fix editing error introduced in 
	!                                  V5.72 (IPUFID truncated to PUFID).
	!                                  Use the /puff/ and /slug/ include
	!                                  files.
	! --- V5.71-V5.72   031017  (DGS): IRLSNUM,ISRCNUM,ISRCTYP replace
	!                                  IPUFID
	! --- V5.5-V5.71    030528  (DGS): Add RESTART notice to list file each
	!                                  time a restart file is rewritten
	! --- V5.4-V5.5     010730  (DGS): SYSRC0, SZSRC0 arrays added
	! --- V5.2-V5.4     000602  (DGS): NVL2 replaces NX4,NY4
	! ---               000602  (DGS): add message to "stop"
	! --- V5.1-V5.2     991104  (JSS): Error messages written to list
	!                                  file as well as to screen
	! --- V5.0-V5.0     980807  (DGS): no data records written if NPUFFS=0
	!                   980615  (DGS): IPUFID, IPUFCD written as integers
	!                                  (order changes also)
	!                   980515  (DGS): ZITIBL array added
	!
	! --- INPUTS:
	!
	!    Common Block /DATEHR/ variables:
	!         nsecb,ndathrb,nsece,ndathre
	!    Common Block /FILNAME/ variables:
	!         RSTARTE
	!    Common Block /GEN/ variables:
	!         NSPEC
	!    Common block /PUFF/ variables:
	!         all variables
	!    Common block /SLUG/ variables:
	!         all variables
	!
	!    Common Block /QA/ variables:
	!         VER, LEVEL
	!    Common block /AR1/ variables:
	!         NAR1
	!    Common block /AR2/ variables:
	!         NAR2
	!    Common block /LN1/ variables:
	!         NLINES
	!    Common block /LN2/ variables:
	!         NLN2
	!    Common block /PT1/ variables:
	!         NPT1
	!    Common block /PT2/ variables:
	!         NPT2
	!    Common block /VOL1/ variables:
	!         NVL1
	!    Common block /VOL2/ variables:
	!         NVL2
	!     Common block /SRCTAB/ variables:
	!           NTR, NWK, NCV,
	!           XTR(mxrise),ZTR(mxrise),RTR(mxrise),HTR(mxrise),
	!           XWK(mxrise),SYWK(mxrise),SZWK(mxrise),DRWK(mxrise),
	!           XCV(mxrise),SYCV(mxrise),SZCV(mxrise)
	!
	!    Parameters:
	!         MXPUFF, MXSPEC, MXRISE, IO4
	!
	! --- OUTPUT:
	!              none
	!
	! --- RESTARTO called by:  COMP
	! --- RESTARTO calls:      ROLLDN, WRTI1D, WRTR1D, WRTR2D, WRTR3D,
	!                          SRCTABIN
	!----------------------------------------------------------------------
	include 'params.puf'
	include 'datehr.puf'
	include 'filnam.puf'
	include 'gen.puf'
	! --- Use include file for /puff/ rather than explicit list here
	! *** Make sure this sub is updated WHENEVER /puff/ is altered ***
	include 'puff.puf'
	! --- Use include file for /slug/ rather than explicit list here
	! *** Make sure this sub is updated WHENEVER /slug/ is altered ***
	include 'slug.puf'
	include 'qa.puf'
	include 'ar1.puf'
	include 'ar2.puf'
	include 'ln1.puf'
	include 'ln2.puf'
	include 'pt1.puf'
	include 'pt2.puf'
	include 'vol1.puf'
	include 'vol2.puf'
	include 'srctab.puf'
	real xbuf(mxspec,mxpuff), xbuf3(3,2,mxpuff)
	character*16 rstrtnam,rstrtver
	character*64 rstrtmod
	character*8 clabel,blank8
	data rstrtnam/'RESTART.DAT     '/
	data rstrtver/'3.1             '/
	data blank8/'        '/
	rstrtmod(1:40)='Puff arrays and Source tables for each  '
	rstrtmod(41:64)='puff                    '
	! --- Save on disk space by removing puffs that are off the
	! --- grid and "roll down" arrays: np0 is total number of puffs
	! --- (new + npuffs = npuffs);  npuffs is updated in ROLLDN
	! --- (this also applies to the DA file of tabulated arrays)
	np0=npuffs
	call ROLLDN(np0)
	! --- Open restart output file (latest information overwrites)
	open(io4,file=rstarte,status='unknown',form='unformatted')
	! --- Identify dataset and version (new for version 6.22)
	write(io4) rstrtnam,rstrtver,rstrtmod
	! --- Write run specification record
	write(io4) ver,level,npuffs,nspec,ndathre,nsece,nar1,nar2,&
	nlines,nln2,npt1,npt2,nvl1,nvl2
	! --- Close file and return if puff arrays are empty
	if(npuffs.EQ.0) then
		close(io4)
		return
	elseif(npuffs.LT.0) then
		! ---    Fatal error
		write(io6,*)'RESTARTO:  FATAL ERROR FOUND'
		write(io6,*)'   Number of puffs after roll-down = ',npuffs
		write(*,*)
		stop 'Halted in RESTARTO -- see list file.'
	endif
	! --- Write /PUFF/ data, one variable for all puffs on one record
	! ---------------------------------------------------------------
	! --- 1D integer arrays
	! ---------------------
	clabel=blank8
	clabel='irlsnum'
	call wrti1d(io4,irlsnum,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='isrcnum'
	call wrti1d(io4,isrcnum,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='isrctyp'
	call wrti1d(io4,isrctyp,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='ipufcd'
	call wrti1d(io4,ipufcd,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='isplit'
	call wrti1d(io4,isplit,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='iemstep'
	call wrti1d(io4,iemstep,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='idw0'
	call wrti1d(io4,idw0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='istab0'
	call wrti1d(io4,istab0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='iru0'
	call wrti1d(io4,iru0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	! --- 3D real arrays
	! ------------------
	clabel=blank8
	clabel='tcon'
	call wrtr3d(io4,tcon,xbuf3,3,2,mxpuff,3,2,npuffs,&
	clabel,ndathrb,nsecb,ndathre,nsece)
	! --- 2D real arrays
	! ------------------
	clabel=blank8
	clabel='qu'
	call wrtr2d(io4,qu,xbuf,mxspec,mxpuff,nspec,npuffs,&
	clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='qm'
	call wrtr2d(io4,qm,xbuf,mxspec,mxpuff,nspec,npuffs,&
	clabel,ndathrb,nsecb,ndathre,nsece)
	! --- 1D real arrays
	! ------------------
	clabel=blank8
	clabel='xpb'
	call wrtr1d(io4,xpb,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='ypb'
	call wrtr1d(io4,ypb,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='zpb'
	call wrtr1d(io4,zpb,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='zimax'
	call wrtr1d(io4,zimax,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='ziold'
	call wrtr1d(io4,ziold,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='zitibl'
	call wrtr1d(io4,zitibl,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='sigyb'
	call wrtr1d(io4,sigyb,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='sigzb'
	call wrtr1d(io4,sigzb,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='xtotb'
	call wrtr1d(io4,xtotb,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='tmtotb'
	call wrtr1d(io4,tmtotb,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='zfinal'
	call wrtr1d(io4,zfinal,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='xfinal'
	call wrtr1d(io4,xfinal,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='bidfnl'
	call wrtr1d(io4,bidfnl,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='fb'
	call wrtr1d(io4,fb,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='fm'
	call wrtr1d(io4,fm,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='xbfin'
	call wrtr1d(io4,xbfin,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='xmfin'
	call wrtr1d(io4,xmfin,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='zbfin'
	call wrtr1d(io4,zbfin,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='zmfin'
	call wrtr1d(io4,zmfin,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='stipdw'
	call wrtr1d(io4,stipdw,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='elbase'
	call wrtr1d(io4,elbase,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='ht0'
	call wrtr1d(io4,ht0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='exitw0'
	call wrtr1d(io4,exitw0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='diam0'
	call wrtr1d(io4,diam0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='ws0'
	call wrtr1d(io4,ws0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='sqrts0'
	call wrtr1d(io4,sqrts0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='srat0'
	call wrtr1d(io4,srat0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='temit0'
	call wrtr1d(io4,temit0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='hb0'
	call wrtr1d(io4,hb0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='hw0'
	call wrtr1d(io4,hw0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='heff20'
	call wrtr1d(io4,heff20,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='sigv0'
	call wrtr1d(io4,sigv0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='sigw0'
	call wrtr1d(io4,sigw0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='el0'
	call wrtr1d(io4,el0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='plexp0'
	call wrtr1d(io4,plexp0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='zly0'
	call wrtr1d(io4,zly0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='r0'
	call wrtr1d(io4,r0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='sysrc0'
	call wrtr1d(io4,sysrc0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='szsrc0'
	call wrtr1d(io4,szsrc0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='xshift0'
	call wrtr1d(io4,xshift0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='sy0'
	call wrtr1d(io4,sy0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='sz0'
	call wrtr1d(io4,sz0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	! --- Write /SLUG/ data
	! --- 1D real arrays
	! ------------------
	clabel=blank8
	clabel='xpe'
	call wrtr1d(io4,xpe,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='ype'
	call wrtr1d(io4,ype,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='zpe'
	call wrtr1d(io4,zpe,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='sigye'
	call wrtr1d(io4,sigye,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='sigze'
	call wrtr1d(io4,sigze,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='xtote'
	call wrtr1d(io4,xtote,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='tmtote'
	call wrtr1d(io4,tmtote,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	clabel=blank8
	clabel='speed0'
	call wrtr1d(io4,speed0,npuffs,clabel,ndathrb,nsecb,ndathre,nsece)
	! --- Write records for puffs from sources with rise tables
	! --- One data header record is written for each puff, but arrays are
	! --- written only if they are not empty
	! -------------------------------------------------------------------
	nullt=0
	nullw=0
	nullc=0
	! --- Loop on all puffs regardless of whether tables exist, and
	! --- use null record for source types without tables
	do ii=1,npuffs
		if(isrctyp(ii).EQ.3 .OR. isrctyp(ii).GT.6) then
			! ---       No tables for control file Areas, all Volumes, and BCs
			write(io4) isrctyp(ii),isrcnum(ii),irlsnum(ii),&
			nullt,nullw,nullc
		else
			! ---       Points, Buoyant Areas, Buoyant Lines
			! ---       Place source tabulations for this puff into /SRCTAB/
			call SRCTABIN(ii,isrctyp(ii),isrcnum(ii),irlsnum(ii))
			! ---       Write valid data to restart file
			write(io4) isrctyp(ii),isrcnum(ii),irlsnum(ii),&
			ntr,nwk,ncv
			if(ntr.GT.0) then
				clabel=blank8
				clabel='xtr'
				call wrtr1d(io4,xtr,ntr,clabel,&
				ndathrb,nsecb,ndathre,nsece)
				clabel=blank8
				clabel='ztr'
				call wrtr1d(io4,ztr,ntr,clabel,&
				ndathrb,nsecb,ndathre,nsece)
				if(isrctyp(ii).LE.4) then
					! ---             Points and Areas
					clabel=blank8
					clabel='rtr'
					call wrtr1d(io4,rtr,ntr,clabel,&
					ndathrb,nsecb,ndathre,nsece)
				endif
				if(isrctyp(ii).LE.2) then
					! ---             Points
					clabel=blank8
					clabel='htr'
					call wrtr1d(io4,htr,ntr,clabel,&
					ndathrb,nsecb,ndathre,nsece)
				endif
			endif
			if(nwk.GT.0) then
				clabel=blank8
				clabel='xwk'
				call wrtr1d(io4,xwk,nwk,clabel,&
				ndathrb,nsecb,ndathre,nsece)
				clabel=blank8
				clabel='sywk'
				call wrtr1d(io4,sywk,nwk,clabel,&
				ndathrb,nsecb,ndathre,nsece)
				clabel=blank8
				clabel='szwk'
				call wrtr1d(io4,szwk,nwk,clabel,&
				ndathrb,nsecb,ndathre,nsece)
				clabel=blank8
				clabel='drwk'
				call wrtr1d(io4,drwk,nwk,clabel,&
				ndathrb,nsecb,ndathre,nsece)
			endif
			if(ncv.GT.0) then
				clabel=blank8
				clabel='xcv'
				call wrtr1d(io4,xcv,ncv,clabel,&
				ndathrb,nsecb,ndathre,nsece)
				clabel=blank8
				clabel='sycv'
				call wrtr1d(io4,sycv,ncv,clabel,&
				ndathrb,nsecb,ndathre,nsece)
				clabel=blank8
				clabel='szcv'
				call wrtr1d(io4,szcv,ncv,clabel,&
				ndathrb,nsecb,ndathre,nsece)
			endif
		endif
	enddo
	! --- Close file
	close(io4)
	! --- Write message to list file
	write(io6,*)'--- Restart file written from: ',ndathrb,nsecb
	write(io6,*)'---                        to: ',ndathre,nsece
	return
end
!----------------------------------------------------------------------
subroutine restarti
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 080520          RESTARTI
	! ---            D. Strimaitis
	!
	! --- PURPOSE:  Input all data needed to restart CALPUFF with
	!               current puffs in the domain
	!
	! --- UPDATE
	! --- V6.22-V6.261  080520  (DGS): Replace source-ordered tabulations
	!                                  with puff-ordered tabulations used
	!                                  for the /SRCTAB/ arrays
	!                                  (Dataset 3.1 format)
	! --- V6.114-V6.22  070921  (DGS): Introduce dataset name and version
	!                                  record
	!                                  Add source tabulations stored for
	!                                  previous met periods
	!                                  (Dataset 2.1 format)
	! --- V6.1-V6.114   060725  (DGS): Fix editing typo that dropped first
	!                                  character of array names iru0,stipdw
	!                                  and ws0
	!                                  Switch mtver from 0 to 1 (new format)
	! --- V5.722-V6.1   050915  (DGS): Resolve times to the second and use
	!                                  explicit begin-time/end-time format
	!                           (DGS): Add IEMSTEP array to /PUFF/
	! --- V5.72-V5.722  040610  (DGS): Fix editing error introduced in 
	!                                  V5.72 (IPUFID truncated to PUFID).
	!                                  Use the /puff/ and /slug/ include
	!                                  files.
	! --- V5.7-V5.72    031017  (DGS): IRLSNUM,ISRCNUM,ISRCTYP replace
	!                                  IPUFID
	! --- V5.5-V5.7     030402  (DGS): Add list file unit to INCR
	! --- V5.4-V5.5     010730  (DGS): SYSRC0, SZSRC0 arrays added
	! --- V5.0-V5.0     990228a (DGS): add IEOF to RDR2D arguments
	! --- V5.0-V5.0     981025  (DGS): Pad sy0,sz0 arguments to c*8
	! --- V5.0-V5.0     980615  (DGS): IPUFID, IPUFCD read as integers
	!                                  (order changes also)
	! --- V5.0-V5.0     980515  (DGS): ZITIBL array added
	!
	! --- INPUTS:
	!
	!    Common Block /DATEHR/ variables:
	!         NDATHRB, NSECB
	!    Common Block /GEN/ variables:
	!         NSPEC
	!
	!    Parameters:
	!         MXPUFF, MXSPEC, MXRISE, IO3, IO6
	!
	! --- OUTPUT:
	!
	!    Common block /PUFF/ variables:
	!         all variables
	!    Common block /SLUG/ variables:
	!         all variables
	!     Common block /SRCTAB/ variables:
	!           NTR, NWK, NCV,
	!           XTR(mxrise),ZTR(mxrise),RTR(mxrise),HTR(mxrise),
	!           XWK(mxrise),SYWK(mxrise),SZWK(mxrise),DRWK(mxrise),
	!           XCV(mxrise),SYCV(mxrise),SZCV(mxrise)
	!
	! --- RESTARTI called by:  COMP
	! --- RESTARTI calls:      RDI1D, RDR1D, RDR2D, RDR3D, RDQA,
	!                          ZEROTAB, SRCTABOUT
	!----------------------------------------------------------------------
	include 'params.puf'
	include 'datehr.puf'
	include 'gen.puf'
	include 'srctab.puf'
	! --- Use include file for /puff/ rather than explicit list here
	! *** Make sure this sub is updated WHENEVER /puff/ is altered ***
	include 'puff.puf'
	! --- Use include file for /slug/ rather than explicit list here
	! *** Make sure this sub is updated WHENEVER /slug/ is altered ***
	include 'slug.puf'
	real xbuf(mxspec,mxpuff), xbuf3(3,2,mxpuff)
	character*8 clabel
	! --- Define time format flag for use in argument list
	data mtver/1/
	! --- Read /PUFF/ data, one variable for all puffs on one record
	! ---------------------------------------------------------------
	! --- 1D integer arrays
	! ---------------------
	call rdi1d(io3,mtver,irlsnum,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'irlsnum ',ndathrb,nsecb,clabel,idathre,isece)
	call rdi1d(io3,mtver,isrcnum,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'isrcnum ',ndathrb,nsecb,clabel,idathre,isece)
	call rdi1d(io3,mtver,isrctyp,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'isrctyp ',ndathrb,nsecb,clabel,idathre,isece)
	call rdi1d(io3,mtver,ipufcd,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'ipufcd  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdi1d(io3,mtver,isplit,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'isplit  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdi1d(io3,mtver,iemstep,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'iemstep ',ndathrb,nsecb,clabel,idathre,isece)
	call rdi1d(io3,mtver,idw0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'idw0    ',ndathrb,nsecb,clabel,idathre,isece)
	call rdi1d(io3,mtver,istab0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'istab0   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdi1d(io3,mtver,iru0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'iru0     ',ndathrb,nsecb,clabel,idathre,isece)
	! --- 3D real arrays
	! ------------------
	call rdr3d(io3,tcon,xbuf3,3,2,mxpuff,3,2,npuffs,&
	clabel,idathrb,isecb,idathre,isece)
	call rdqa(io6,'tcon    ',ndathrb,nsecb,clabel,idathre,isece)
	! --- 2D real arrays
	! ------------------
	call rdr2d(io3,mtver,qu,xbuf,mxspec,mxpuff,nspec,npuffs,&
	clabel,idathrb,isecb,idathre,isece,ieof)
	if(ieof.EQ.1) then
		write(*,*)
		stop 'RESTARTI: Unexpected EOF in restart file'
	endif
	call rdqa(io6,'qu      ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr2d(io3,mtver,qm,xbuf,mxspec,mxpuff,nspec,npuffs,&
	clabel,idathrb,isecb,idathre,isece,ieof)
	if(ieof.EQ.1) then
		write(*,*)
		stop 'RESTARTI: Unexpected EOF in restart file'
	endif
	call rdqa(io6,'qm      ',ndathrb,nsecb,clabel,idathre,isece)
	! --- 1D real arrays
	! ------------------
	call rdr1d(io3,mtver,xpb,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'xpb     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,ypb,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'ypb     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,zpb,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'zpb     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,zimax,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'zimax   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,ziold,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'ziold   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,zitibl,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'zitibl  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,sigyb,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'sigyb   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,sigzb,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'sigzb   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,xtotb,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'xtotb   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,tmtotb,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'tmtotb  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,zfinal,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'zfinal  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,xfinal,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'xfinal  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,bidfnl,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'bidfnl  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,fb,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'fb      ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,fm,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'fm      ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,xbfin,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'xbfin   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,xmfin,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'xmfin   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,zbfin,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'zbfin   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,zmfin,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'zmfin   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,stipdw,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'stipdw  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,elbase,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'elbase  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,ht0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'ht0     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,exitw0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'exitw0  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,diam0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'diam0   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,ws0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'ws0     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,sqrts0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'sqrts0  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,srat0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'srat0   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,temit0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'temit0  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,hb0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'hb0     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,hw0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'hw0     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,heff20,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'heff20  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,sigv0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'sigv0   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,sigw0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'sigw0   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,el0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'el0     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,plexp0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'plexp0  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,zly0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'zly0    ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,r0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'r0      ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,sysrc0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'sysrc0  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,szsrc0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'szsrc0  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,xshift0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'xshift0 ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,sy0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'sy0     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,sz0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'sz0     ',ndathrb,nsecb,clabel,idathre,isece)
	! --- Read /SLUG/ data
	! --- 1D real arrays
	! ------------------
	call rdr1d(io3,mtver,xpe,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'xpe     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,ype,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'ype     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,zpe,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'zpe     ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,sigye,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'sigye   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,sigze,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'sigze   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,xtote,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'xtote   ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,tmtote,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'tmtote  ',ndathrb,nsecb,clabel,idathre,isece)
	call rdr1d(io3,mtver,speed0,npuffs,clabel,&
	idathrb,isecb,idathre,isece)
	call rdqa(io6,'speed0  ',ndathrb,nsecb,clabel,idathre,isece)
	! --- Read source table data for each puff
	! ----------------------------------------
	! --- Loop on all puffs regardless of whether tables exist
	do ii=1,npuffs
		call ZEROTAB
		read(io3) ityp,isrc,irls,ntr,nwk,ncv
		if(ntr.GT.0) then
			call rdr1d(io3,mtver,xtr,ntr,clabel,&
			idathrb,isecb,idathre,isece)
			call rdqa(io6,'xtr     ',ndathrb,nsecb,clabel,&
			idathre,isece)
			call rdr1d(io3,mtver,ztr,ntr,clabel,&
			idathrb,isecb,idathre,isece)
			call rdqa(io6,'ztr     ',ndathrb,nsecb,clabel,&
			idathre,isece)
			if(ityp.LE.4) then
				! ---          Points and Areas
				call rdr1d(io3,mtver,rtr,ntr,clabel,&
				idathrb,isecb,idathre,isece)
				call rdqa(io6,'rtr     ',ndathrb,nsecb,clabel,&
				idathre,isece)
			endif
			if(isrctyp(ii).LE.2) then
				! ---          Points
				call rdr1d(io3,mtver,htr,ntr,clabel,&
				idathrb,isecb,idathre,isece)
				call rdqa(io6,'htr     ',ndathrb,nsecb,clabel,&
				idathre,isece)
			endif
		endif
		if(nwk.GT.0) then
			call rdr1d(io3,mtver,xwk,nwk,clabel,&
			idathrb,isecb,idathre,isece)
			call rdqa(io6,'xwk     ',ndathrb,nsecb,clabel,&
			idathre,isece)
			call rdr1d(io3,mtver,sywk,nwk,clabel,&
			idathrb,isecb,idathre,isece)
			call rdqa(io6,'sywk    ',ndathrb,nsecb,clabel,&
			idathre,isece)
			call rdr1d(io3,mtver,szwk,nwk,clabel,&
			idathrb,isecb,idathre,isece)
			call rdqa(io6,'szwk    ',ndathrb,nsecb,clabel,&
			idathre,isece)
			call rdr1d(io3,mtver,drwk,nwk,clabel,&
			idathrb,isecb,idathre,isece)
			call rdqa(io6,'drwk    ',ndathrb,nsecb,clabel,&
			idathre,isece)
		endif
		if(ncv.GT.0) then
			call rdr1d(io3,mtver,xcv,ncv,clabel,&
			idathrb,isecb,idathre,isece)
			call rdqa(io6,'xcv     ',ndathrb,nsecb,clabel,&
			idathre,isece)
			call rdr1d(io3,mtver,sycv,ncv,clabel,&
			idathrb,isecb,idathre,isece)
			call rdqa(io6,'sycv    ',ndathrb,nsecb,clabel,&
			idathre,isece)
			call rdr1d(io3,mtver,szcv,ncv,clabel,&
			idathrb,isecb,idathre,isece)
			call rdqa(io6,'szcv    ',ndathrb,nsecb,clabel,&
			idathre,isece)
		endif
		call SRCTABOUT(ii,ityp,isrc,irls)
	enddo
	! --- Close file
	close(io3)
	return
end
!----------------------------------------------------------------------
subroutine rdqa(io,alabel,ndathr,nsec,clabel,idathr,isec)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 050915              RDQA
	! ---            D. Strimaitis  SRC
	!
	! --- PURPOSE:  Checks label and date-time of a RESTART record
	!
	! --- UPDATE
	! --- V5.4-V6.1     050915  (DGS): resolve times to the second
	! --- V5.3-V5.4     000602  (DGS): add message to "stop"
	!
	! --- INPUTS:
	!            IO - integer       - Fortran unit number of output file
	!        ALABEL - character     - Expected record label
	!        NDATHR - integer       - Expected record date-time (ending)
	!          NSEC - integer       - Expected record seconds (ending)
	!        CLABEL - character     - Record label
	!        IDATHR - integer       - Record date-time (ending)
	!          ISEC - integer       - Record seconds (ending)
	!
	!
	! --- OUTPUT:
	!              none
	!
	! --- RDQA called by:  RESTARTI
	! --- RDQA calls:      none
	!----------------------------------------------------------------------
	character*8 alabel,clabel
	logical problem
	problem=.FALSE.
	if(alabel.NE.clabel) problem=.TRUE.
	if(ndathr.NE.idathr) problem=.TRUE.
	if(nsec.NE.isec) problem=.TRUE.
	if(problem) then
		write(io,*)
		write(io,*)
		write(io,*)
		write(io,*)'FATAL Problem with RESTART File'
		write(io,*)' --- record out of order or missing'
		write(io,*)'     Expected ',alabel,ndathr,nsec
		write(io,*)'     Found    ',clabel,idathr,isec
		write(*,*)
		stop 'Halted in RDQA -- see list file.'
	endif
	return
end
!----------------------------------------------------------------------
subroutine wrti1d(iomet,idat,nwords,clabel,ndathrb,nsecb,&
	ndathre,nsece)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF  Version: 7.3.1        Level: 050915              WRTI1D
	! ---          J. Scire, SRC
	!
	! --- PURPOSE:  Write "NWORDS" of a one-dimensional integer array
	!
	! --- UPDATE
	! --- V5.0-V6.1     050915  (DGS): resolve times to the second and use
	!                                  explicit begin-time/end-time format
	!
	! --- INPUTS:
	!         IOMET - integer       - Fortran unit number of output file
	!  IDAT(nwords) - integer array - Array to output
	!        NWORDS - integer       - Number of words to write
	!        CLABEL - character*8   - Variable name
	!   NDATHR[B,E] - integer       - Date and time of data (YYYYJJJHH)
	!     NSEC[B,E] - integer       - Seconds for data (SSSS)
	!                 [B: beginning of period]
	!                 [E: end of period]
	!
	! --- OUTPUT:  none
	!
	! --- WRTI1D called by:  RESTARTO
	! --- WRTI1D calls:      none
	!----------------------------------------------------------------------
	integer idat(nwords)
	character*8 clabel
	!
	write(iomet)clabel,ndathrb,nsecb,ndathre,nsece,idat
	return
end
!----------------------------------------------------------------------
subroutine wrtr1d(iomet,x,nwords,clabel,ndathrb,nsecb,&
	ndathre,nsece)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF  Version: 7.3.1        Level: 050915              WRTR1D
	! ---          J. Scire, SRC
	!
	! --- PURPOSE:  Write "NWORDS" of a one-dimensional real array
	!
	! --- UPDATE
	! --- V5.0-V6.1     050915  (DGS): resolve times to the second and use
	!                                  explicit begin-time/end-time format
	!
	! --- INPUTS:
	!         IOMET - integer     - Fortran unit number of output file
	!     X(nwords) - real array  - Array to output
	!        NWORDS - integer     - Number of words to write
	!        CLABEL - character*8 - Variable name
	!   NDATHR[B,E] - integer     - Date and time of data (YYYYJJJHH)
	!     NSEC[B,E] - integer     - Seconds for data (SSSS)
	!                 [B: beginning of period]
	!                 [E: end of period]
	!
	! --- OUTPUT:  none
	!
	! --- WRTR1D called by:  RSTARTO
	! --- WRTR1D calls:      none
	!----------------------------------------------------------------------
	real x(nwords)
	character*8 clabel
	!
	write(iomet)clabel,ndathrb,nsecb,ndathre,nsece,x
	return
end
!----------------------------------------------------------------------
subroutine wrtr2d(iomet,x,xbuf,mxnx,mxny,nx,ny,clabel,&
	ndathrb,nsecb,ndathre,nsece)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF  Version: 7.3.1        Level: 050915              WRTR2D
	! ---          J. Scire, SRC
	!
	! --- PURPOSE:  Write NX * NY words of a 2-D real array
	!
	! --- UPDATE
	! --- V5.0-V6.1     050915  (DGS): resolve times to the second and use
	!                                  explicit begin-time/end-time format
	!
	! --- INPUTS:
	!            IOMET - integer     - Fortran unit number of output file
	!     X(mxnx,mxny) - real array  - Array to output
	!      XBUF(nx,ny) - real array  - Buffer to hold data for output
	!        MXNX,MXNY - integers    - Dimensions of data array
	!            NX,NY - integers    - Actual size of grid to output
	!           CLABEL - character*8 - Variable name
	!      NDATHR[B,E] - integer     - Date and time of data (YYYYJJJHH)
	!        NSEC[B,E] - integer     - Seconds for data (SSSS)
	!                    [B: beginning of period]
	!                    [E: end of period]
	!
	! --- OUTPUT:  none
	!
	! --- WRTR2D called by:  RESTARTO
	! --- WRTR2D calls:      none
	!----------------------------------------------------------------------
	real x(mxnx,mxny),xbuf(nx,ny)
	character*8 clabel
	!
	if(nx.eq.mxnx.and.ny.eq.mxny)then
		!
		! ---    entire array is being used -- write full grid
		write(iomet)clabel,ndathrb,nsecb,ndathre,nsece,x
	else
		!
		! ---    only a portion of grid being used -- transfer to buffer
		! ---    and write
		do i=1,nx! add by @creaqi do label 10
			do j=1,ny! add by @creaqi do label 10
				xbuf(i,j)=x(i,j)
			enddo !add by @creaqi 10
		enddo !add by @creaqi 10
		10       continue
		!
		write(iomet)clabel,ndathrb,nsecb,ndathre,nsece,xbuf
	endif
	!
	return
end
!----------------------------------------------------------------------
subroutine wrtr3d(iomet,x,xbuf,mxnx,mxny,mxnz,nx,ny,nz,&
	clabel,ndathrb,nsecb,ndathre,nsece)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF  Version: 7.3.1        Level: 050915              WRTR3D
	! ---          J. Scire, SRC
	!
	! --- PURPOSE:  Write NX * NY * NZ words of a 3-D real array
	!
	! --- UPDATE
	! --- V5.0-V6.1     050915  (DGS): resolve times to the second and use
	!                                  explicit begin-time/end-time format
	!
	! --- INPUTS:
	!            IOMET - integer     - Fortran unit number of output file
	!X(mxnx,mxny,mxnz) - real array  - Array to output
	!   XBUF(nx,ny,nz) - real array  - Buffer to hold data for output
	!   MXNX,MXNY,MXNZ - integers    - Dimensions of data array
	!         NX,NY,NZ - integers    - Actual size of grid to output
	!           CLABEL - character*8 - Variable name
	!      NDATHR[B,E] - integer     - Date and time of data (YYYYJJJHH)
	!        NSEC[B,E] - integer     - Seconds for data (SSSS)
	!                    [B: beginning of period]
	!                    [E: end of period]
	!
	! --- OUTPUT:  none
	!
	! --- WRTR3D called by:  RESTARTO
	! --- WRTR3D calls:      none
	!----------------------------------------------------------------------
	real x(mxnx,mxny,mxnz),xbuf(nx,ny,nz)
	character*8 clabel
	!
	if(nx.eq.mxnx.and.ny.eq.mxny.and.nz.eq.mxnz)then
		!
		! ---    entire array is being used -- write full grid
		write(iomet)clabel,ndathrb,nsecb,ndathre,nsece,x
	else
		!
		! ---    only a portion of grid being used -- transfer to buffer
		! ---    and write
		do i=1,nx! add by @creaqi do label 10
			do j=1,ny! add by @creaqi do label 10
				do k=1,nz! add by @creaqi do label 10
					xbuf(i,j,k)=x(i,j,k)
				enddo !add by @creaqi 10
			enddo !add by @creaqi 10
		enddo !add by @creaqi 10
		10       continue
		!
		write(iomet)clabel,ndathrb,nsecb,ndathre,nsece,xbuf
	endif
	!
	return
end
!----------------------------------------------------------------------
subroutine rdr3d(iomet,x,xbuf,mxnx,mxny,mxnz,nx,ny,nz,&
	clabel,ndathrb,nsecb,ndathre,nsece)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 050915             RDR3D
	!                J. Scire, SRC
	!
	! --- PURPOSE:  Read NX * NY * NZ words of a 3-D real array
	!
	! --- UPDATE
	! --- V5.0-V6.1     050915  (DGS): resolve times to the second and use
	!                                  explicit begin-time/end-time format
	!
	! --- INPUTS:
	!            IOMET - integer     - Fortran unit number of input file
	!   XBUF(nx,ny,nz) - real array  - Buffer to hold input data
	!   MXNX,MXNY,MXNZ - integers    - Dimensions of data array
	!         NX,NY,NZ - integers    - Actual size of grid to read
	!           CLABEL - character*8 - Variable name
	!      NDATHR[B,E] - integer     - Date and time of data (YYYYJJJHH)
	!        NSEC[B,E] - integer     - Seconds for data (SSSS)
	!                    [B: beginning of period]
	!                    [E: end of period]
	!
	! --- OUTPUT:
	!X(mxnx,mxny,mxnz) - real array  - Input data array (padded if nec.)
	!
	! --- RDR3D called by:  RESTARTI
	! --- RDR3D calls:      none
	!----------------------------------------------------------------------
	real x(mxnx,mxny,mxnz),xbuf(nx,ny,nz)
	character*8 clabel
	!
	if(nx.eq.mxnx.and.ny.eq.mxny.and.nz.eq.mxnz)then
		!
		! ---    entire array is being used -- read full grid
		read(iomet)clabel,ndathrb,nsecb,ndathre,nsece,x
	else
		!
		! ---    only a portion of grid being used -- read and
		! ---    transfer from buffer
		read(iomet)clabel,ndathrb,nsecb,ndathre,nsece,xbuf
		!
		do i=1,nx! add by @creaqi do label 10
			do j=1,ny! add by @creaqi do label 10
				do k=1,nz! add by @creaqi do label 10
					x(i,j,k)=xbuf(i,j,k)
				enddo !add by @creaqi 10
			enddo !add by @creaqi 10
		enddo !add by @creaqi 10
		10       continue
	endif
	!
	return
end
!----------------------------------------------------------------------
subroutine tiblset(ldb)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 101111           TIBLSET
	!                D. Strimaitis
	!
	! --- PURPOSE:  Setup functions for the sub-grid TIBL module
	!                 -- Transfer distance parameters from control file
	!                 -- Read user-supplied coast line(s) (COASTLN.DAT)
	!                 -- Convert points to MET GRID Units
	!                 -- Identify MET GRID cells with coast segments
	!                 -- Identify nearest land and water MET GRID cells
	!                    for each point stored for each coast line
	!                 -- Compute slope/intercept for coastline segments
	!
	! --- UPDATES
	! --- V6.301-V6.402 101111 (DGS): Update GETCELL arguments
	! --- V6.3-V6.301  100827  (DGS): Allow nested grid domains that do not
	!                                 align with cells in parent domains
	! --- V6.25-V6.3   100212  (DGS): Add nested CALMET grids - geometry
	!                                 uses Grid 1 (outermost) but cell
	!                                 references are tied to a specific
	!                                 MET grid domain
	! --- V5.7-V6.25   080227  (DGS): Replace NLOOK with NLUTIBL from the
	!                                 control file
	! --- V5.4-V5.7    030402  (DGS): add /MAP/
	! --- V5.2-V5.4    000602  (DGS): add message to "stop"
	! --- V5.1-V5.2    991104  (JSS): Error messages written to list
	!                                 file as well as to screen
	! --- V5.0-V5.0    990228e (DGS): process distance parameters TIBLDIST
	!     (from 980515)990228e (DGS): expand search radius for nearest land
	!                                 and water cells from 1 to NLOOK
	!
	! --- INPUTS:
	!              LDB - logical  - Flag controlling printing of internal
	!                               data (F=suppress, T=print)
	!
	!     Common block /COMPARM/ variables
	!        TIBLDIST(3), NLUTIBL
	!     Common block /GRID/ variables
	!        DGRIDI, XORIG, YORIG
	!     Common block /GRIDNEST/ variables
	!        NGRID,NESTFAC(mxmetdom)
	!        RNXNEST0(mxmetdom),RNYNEST0(mxmetdom)
	!     Common block /MAP/ variables
	!        IUTMZN
	!     Common Block /METHD/ variables:
	!        NXM(mxmetdom), NYM(mxmetdom), ILANDU(mxnx,mxny,mxmetdom),
	!        IWAT1, IWAT2
	!
	!     Parameters:
	!         MXCOAST, IO25, IO6
	!
	! --- OUTPUT:
	!     Common block /COASTLN/ variables
	!     Common block /TIBL/ variables
	!        X2ZI1, X2ZI2, XUPGRD
	!
	! --- TIBLSET called by: SETUP
	! --- TIBLSET calls:     GETCELL
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	include 'coastln.puf'
	include 'comparm.puf'
	include 'grid.puf'
	include 'gridnest.puf'
	include 'map.puf'
	include 'methd.puf'
	include 'tibl.puf'
	!
	logical ldb,problem,lwr,ltest, lok
	logical :: lgoto_998_4 = .false. ! add by @creaqi tiblset modify_goto_in_if
	character*12 datatype,vrscst,labcst
	character*40 avar(2)
	! --- Set local variable for a difference (MET Grid Units) small
	! --- enough to be treated as ZERO
	data dsmall/0.000001/
	data zero/0.0/
	problem=.FALSE.
	! --- Transfer TIBL parameters from control file
	x2zi1=tibldist(1)*1000.
	x2zi2=tibldist(2)*1000.
	xupgrd=tibldist(3)*1000.*dgridi
	! --- Read user-specified coastline data (COASTLN.DAT)
	! ----------------------------------------------------
	! --- Header record
	read(io25,*,end=997) datatype,ncoast,iutmcst,vrscst,labcst
	! --- Perform QA checks
	if(datatype.NE.'COASTLN' .AND. datatype.NE.'coastln')then
		write(io6,*)'ERROR in subr. TIBLSET -- Data Type label',&
		' in COASTLN.DAT file does not match expected value ',&
		'-- Found ',datatype,' Expected COASTLN or coastln'
		problem=.TRUE.
	endif
	if(iutmcst.NE.iutmzn)then
		write(io6,*)'ERROR in subr. TIBLSET -- Value of UTM zone',&
		' in COASTLN.DAT file does not match control file value ',&
		'-- IUTMCST = ',iutmcst,' IUTMZN = ',iutmzn
		problem=.TRUE.
	endif
	if(ncoast.GT.mxcoast)then
		write(io6,*)'ERROR in subr. TIBLSET -- Number of coast ',&
		' lines in COASTLN.DAT file exceeds maximum parameter ',&
		'-- NCOAST = ',ncoast,' MXCOAST = ',mxcoast
		problem=.TRUE.
	endif
	if(.not. (PROBLEM)) then ! add by @creaqi goto 998 in fun modify_goto_pure
		! --- Data records:  Loop over coastline(s) provided
		problem=.FALSE.
		nsegcst=0
		icoast=0
		do ! insert do to replace label 10, add by @creaqi 2020-02-18 11:30:06.343635
			10    read(io25,*,end=990) avar
			if(avar(1)(1:1).EQ.'W' .OR. avar(1)(1:1).EQ.'w') then
				! ---    New coastline
				! --------------------
				if(icoast.GT.0) then
					! ---       Terminate previous coastline
					if(nsegcst.GT.npcoast(1,icoast)) then
						! ---          Previous coastline OK, set pointer to end of last segment
						npcoast(2,icoast)=nsegcst
						if(.not.LWR) then
							! ---             Points defined with water to the Left along coastal
							! ---             path, so these points were placed at the END of array
							! ---             in reverse order to comply with internal convention.
							! ---             Move the NEND points up.
							do k=1,nend
								ipos=npcoast(2,icoast)-nend+k
								iend=mxptcst-nend+k
								do i=1,2
									coastgrd(i,ipos)=coastgrd(i,iend)
								enddo
							enddo
							nend=0
						endif
					else
						! ---          Previous coastline has less than 2 points -- remove it!
						nsegcst=MAX(0,npcoast(1,icoast)-1)
						icoast=MAX(0,icoast-1)
						problem=.TRUE.
					endif
				endif
				! ---    Initiate new coastline
				icoast=icoast+1
				npcoast(1,icoast)=nsegcst+1
				if(avar(1).EQ.'WR') then
					LWR=.TRUE.
				else
					LWR=.FALSE.
					nend=0
				endif
			else
				! ---    Current Coastline
				! ------------------------
				! ---    Get position from arrays
				read(avar(1),'(f10.3)') xkm
				read(avar(2),'(f10.3)') ykm
				! ---    Convert coordinates from UTM (km) to MET GRID 1 Units
				xcstmg=(1000.*xkm-xorig)*dgridi
				ycstmg=(1000.*ykm-yorig)*dgridi
				if(nsegcst.LT.npcoast(1,icoast)) then
					! ---       First point along coast line:  store it
					nsegcst=nsegcst+1
					if(nsegcst.GT.mxptcst) then ! add by @creaqi goto 996
						write(io6,*)'ERROR in subr. TIBLSET -- No. of coast segments',&
						' exceeds maximum parameter MXPTCST = ',mxptcst
						write(*,*)
					endif ! add by @creaqi end of 996
					if(LWR) then
						coastgrd(1,nsegcst)=xcstmg
						coastgrd(2,nsegcst)=ycstmg
					else
						nend=nend+1
						iend=mxptcst+1-nend
						coastgrd(1,iend)=xcstmg
						coastgrd(2,iend)=ycstmg
					endif
				else
					! ---       Compute number of points to store (separation < grid cell)
					k=iend
					if(LWR) k=nsegcst
					dxstep=xcstmg-coastgrd(1,k)
					dystep=ycstmg-coastgrd(2,k)
					step=AMAX1(ABS(dxstep),ABS(dystep))
					! ---       Find cell and domain grid containing current point
					call GETCELL('TIBLSET     ','GRIDIJ',1,xcstmg,ycstmg,&
					zero,zero,icell,jcell,kg)
					! ---       Convert 'step' from grid 1 to grid kg cell lengths
					step=step*nestfac(kg)
					if(step.GT.0.5) then
						nstep=2*step
						dxstep=dxstep/FLOAT(nstep)
						dystep=dystep/FLOAT(nstep)
					else
						nstep=1
					endif
					! ---       Loop over points
					do istep=1,nstep
						nsegcst=nsegcst+1
						if(nsegcst.GT.mxptcst) then ! add by @creaqi goto 996
							write(io6,*)'ERROR in subr. TIBLSET -- No. of coast segments',&
							' exceeds maximum parameter MXPTCST = ',mxptcst
							write(*,*)
						endif ! add by @creaqi end of 996
						if(LWR) then
							coastgrd(1,nsegcst)=coastgrd(1,nsegcst-1)+dxstep
							coastgrd(2,nsegcst)=coastgrd(2,nsegcst-1)+dystep
						else
							nend=nend+1
							iend=mxptcst+1-nend
							coastgrd(1,iend)=xcstmg
							coastgrd(2,iend)=ycstmg
							coastgrd(1,iend)=coastgrd(1,iend+1)+dxstep
							coastgrd(2,iend)=coastgrd(2,iend+1)+dystep
						endif
					enddo
				endif
			endif
			! --- Next record
		enddo ! insert enddo to replace goto [10, add by @creaqi 2020-02-18 11:30:06.343635
		! goto 10 add by @creaqi 2020-02-18 11:30:06.343635
		! --- End of data reached in file   
		990   if(icoast.GT.0) then
			! ---    Terminate previous coastline
			if(nsegcst.GT.npcoast(1,icoast)) then
				! ---       Coastline OK, set pointer to end of last segment
				npcoast(2,icoast)=nsegcst
				if(.not.LWR) then
					! ---          Points defined with water to the Left along coastal
					! ---          path, so these points were placed at the END of array
					! ---          in reverse order to comply with internal convention.
					! ---          Move the NEND points up.
					do k=1,nend
						ipos=npcoast(2,icoast)-nend+k
						iend=mxptcst-nend+k
						do i=1,2
							coastgrd(i,ipos)=coastgrd(i,iend)
						enddo
					enddo
					nend=0
				endif
			else
				! ---       Coastline has less than 2 points -- remove it!
				nsegcst=MAX(0,npcoast(1,icoast)-1)
				icoast=MAX(0,icoast-1)
				problem=.TRUE.
			endif
		endif
		if(.not. (icoast.LE.0)) then ! add by @creaqi goto 997 in fun modify_goto_pure
			! --- Compute slope 'em' and intercept 'be' for each segment
			! ----------------------------------------------------------
			! --- Loop over coasts
			do ic=1,icoast
				! ---    Loop over segments in coast
				do is=npcoast(1,ic),npcoast(2,ic)-1
					dx=coastgrd(1,is+1)-coastgrd(1,is)
					dy=coastgrd(2,is+1)-coastgrd(2,is)
					if(ABS(dx).LE.dsmall) then
						! ---          Segment is assumed to be vertical (N/S)
						em=9.91e10
						be=9.91e10
					else
						em=dy/dx
						be=coastgrd(2,is)-em*coastgrd(1,is)
					endif
					ymxpb(1,is)=em
					ymxpb(2,is)=be
				enddo
				! ---    Place data for last segment into end-point location also
				ymxpb(1,npcoast(2,ic))=em
				ymxpb(2,npcoast(2,ic))=be
			enddo
			! --- Identify met grid cells touched by at least 1 coastline
			! -----------------------------------------------------------
			! --- Initialize LCOAST array
			do kg=1,ngrid
				do j=1,nym(kg)
					do i=1,nxm(kg)
						lcoast(i,j,kg)=.FALSE.
					enddo
				enddo
			enddo
			! --- Loop over coasts
			do ic=1,icoast
				! ---    Loop over segments in coast
				do is=npcoast(1,ic),npcoast(2,ic)-1
					! ---       Loop over MET grid domains
					do kg=1,ngrid
						xkg=nestfac(kg)*coastgrd(1,is)-rnxnest0(kg)
						ykg=nestfac(kg)*coastgrd(2,is)-rnynest0(kg)
						if(xkg.GE.0.0 .AND. ykg.GE.0.0) then
							ix=1+xkg
							iy=1+ykg
							if(ix.GE.1 .AND. ix.LE.nxm(kg)) then
								if(iy.GE.1 .AND. iy.LE.nym(kg)) then
									lcoast(ix,iy,kg)=.TRUE.
								endif
							endif
						endif
					enddo
				enddo
			enddo
			! --- Identify nearest land and water cell to each point
			! ------------------------------------------------------
			! --- Loop over coasts
			ltest=.FALSE.
			do ic=1,icoast
				! ---    Loop over points in coast
				do ip=npcoast(1,ic),npcoast(2,ic)
					lwcell(1,ip)=0
					lwcell(2,ip)=0
					lwmdom(1,ip)=0
					lwmdom(2,ip)=0
					dlandsq=2.*nlutibl**2
					dwatersq= dlandsq
					ixp=1+coastgrd(1,ip)
					iyp=1+coastgrd(2,ip)
					! ---       Set search range within grid limits
					ixlo=MAX(1,ixp-nlutibl)
					ixhi=MIN(nx,ixp+nlutibl)
					iylo=MAX(1,iyp-nlutibl)
					iyhi=MIN(ny,iyp+nlutibl)
					!c ---       Adjust search range near grid boundary
					!            if(ixlo.EQ.1) then
					!               ixhi=MAX(ixhi,2)
					!            elseif(ixhi.EQ.nx) then
					!               ixlo=MIN(ixlo,nx-1)
					!            endif
					!            if(iylo.EQ.1) then
					!               iyhi=MAX(iyhi,2)
					!            elseif(iyhi.EQ.ny) then
					!               iylo=MIN(iylo,ny-1)
					!            endif
					! ---       Loop over MET grid domains
					do kg=1,ngrid
						! ---          Set range of cells for current grid
						kxlo=(ixlo-1)*nestfac(kg)-NINT(rnxnest0(kg))+1
						kxhi=(ixhi-1)*nestfac(kg)-NINT(rnxnest0(kg))+nestfac(kg)
						kylo=(iylo-1)*nestfac(kg)-NINT(rnynest0(kg))+1
						kyhi=(iyhi-1)*nestfac(kg)-NINT(rnynest0(kg))+nestfac(kg)
						kxlo=MAX(1,kxlo)
						kxhi=MIN(nxm(kg),kxhi)
						kylo=MAX(1,kylo)
						kyhi=MIN(nym(kg),kyhi)
						! ---          Check
						lok=.true.
						if(kxhi.LT.1 .OR. kxlo.GT.nxm(kg))lok=.false.
						if(kyhi.LT.1 .OR. kylo.GT.nym(kg))lok=.false.
						if(lok) then
							! ---             Search range includes cells in this grid
							do i=kxlo,kxhi
								do j=kylo,kyhi
									index=nxm(kg)*(j-1)+i
									! ---                   Location of cell-center in outermost grid
									xgrd=(rnxnest0(kg)+i-.5)/FLOAT(nestfac(kg))
									ygrd=(rnynest0(kg)+j-.5)/FLOAT(nestfac(kg))
									distsq=(xgrd-coastgrd(1,ip))**2+&
									(ygrd-coastgrd(2,ip))**2
									if(ilandu(i,j,kg).GE.iwat1 .AND.&
										ilandu(i,j,kg).LE.iwat2) then
										! ---                      Water cell
										if(distsq.LT.dwatersq) then
											dwatersq=distsq
											lwcell(2,ip)=index
											lwmdom(2,ip)=kg
										endif
									else
										! ---                      Land cell
										if(distsq.LT.dlandsq) then
											dlandsq=distsq
											lwcell(1,ip)=index
											lwmdom(1,ip)=kg
										endif
									endif
								enddo
							enddo
						endif
					enddo
					! ---       Update test for finding both land and water cells
					if(lwcell(1,ip)*lwcell(2,ip).EQ.0) ltest=.TRUE.
				enddo
			enddo
			if(ltest) problem=.TRUE.
			! --- Documentation for LIST file
			write(io6,*)
			write(io6,*)'TIBLSET ---------------'
			write(io6,*)'Coastline derived from data provided in ',&
			'COASTLN.DAT file'
			write(io6,*)'Number of coastlines     = ',icoast
			write(io6,*)'Total number of segments = ',nsegcst
			write(io6,*)
			write(io6,*)'Met Grid cells containing coastline segments:'
			write(io6,*)'   (Grid North)'
			do kg=1,ngrid
				write(io6,*)'   MET Grid Domain ',kg
				do j=nym(kg),1,-1
					write(io6,'(1000L2)') (lcoast(i,j,kg),i=1,nxm(kg))
				enddo
			enddo
			write(io6,*)'Transition from TIBL Hts to CALMET Mixing Hts'
			write(io6,*)'starts at (m):  ',x2zi1
			write(io6,*)' stops at (m):  ',x2zi2
			write(io6,*)'Maximum distance upwind of source in search for'
			write(io6,*)'   coast (km):  ',tibldist(3)
			write(io6,*)'   (Met Grid):  ',xupgrd
			! --- Additional DEBUG Output Section
			! -----------------------------------
			if(LDB.OR.PROBLEM) then
				do icst=1,icoast
					write(io6,*)
					write(io6,*)'  Coastline ',icst
					write(io6,*)'  Located in array elements ',npcoast(1,icst),&
					' to ',npcoast(2,icst)
					write(io6,*)'  (x,y){MET Grid Units} and pointer to ',&
					'nearest LAND and WATER cell (>0)'
					do i=npcoast(1,icst),npcoast(2,icst)
						! ---          Nearest LAND cell:
						ipland=lwcell(1,i)
						kgl=lwmdom(1,i)
						ixl=MOD(ipland,nxm(kgl))
						if(ixl.EQ.0)ixl=nxm(kgl)
						iyl=1+(ipland-ixl)/nxm(kgl)
						! ---          Nearest WATER cell:
						ipwater=lwcell(2,i)
						kgw=lwmdom(2,i)
						ixw=MOD(ipwater,nxm(kgw))
						if(ixw.EQ.0)ixw=nxm(kgw)
						iyw=1+(ipwater-ixw)/nxm(kgw)
						write(io6,*) (coastgrd(k,i),k=1,2),(lwcell(k,i),k=1,2)
						write(io6,*) 'i,j,grid: Land ',ixl,iyl,kgl,&
						' Water ',ixw,iyw,kgw
					enddo
					write(io6,*)
					write(io6,*)'  (slope,intercept) {MET Grid Units}'
					do i=npcoast(1,icst),npcoast(2,icst)
						write(io6,*) (ymxpb(k,i),k=1,2)
					enddo
				enddo
			endif
			! --- Perform QA check on number of coasts processed
			if(icoast.NE.ncoast) then
				write(io6,*)'ERROR in subr. TIBLSET -- Number of coasts',&
				' in COASTLN.DAT file does not match expected value ',&
				'-- Found ',icoast,' Expected ',ncoast
				lgoto_998_4 = .true. !         goto 998
				if(lgoto_998_4) then ! add by @creaqi start of goto 998 modify_goto_related_if
					write(*,*)
				endif ! add by @creaqi end of goto 998 modify_goto_related_if
				write(*,*)
			endif
			if(ltest) then ! add by @creaqi goto 999
				write(io6,*)'FATAL Error in COASTLN.DAT Data'
				write(io6,*)'Met Grid Land Use not consistent with coast'
				write(*,*)
			endif ! add by @creaqi end of 999
			return
			996   write(io6,*)'ERROR in subr. TIBLSET -- No. of coast segments',&
			' exceeds maximum parameter MXPTCST = ',mxptcst
			write(*,*)
			if(.not.lgoto_998_4) then ! start second start of goto 998 modify_goto_related_if
				stop 'Halted in TIBLSET -- see list file.'
			endif !add by @creaqi label 997 modify_goto_pure
			997   write(io6,*)'FATAL Error: COASTLN.DAT file empty or incomplete'
			write(*,*)
			stop 'Halted in TIBLSET -- see list file.'
		endif !add by @creaqi label 998 modify_goto_pure
	endif ! add by @creaqi max lgoto_998_4 end of goto 998 modify_goto_related_if
	!998   write(io6,*)'FATAL Error in COASTLN.DAT Header'
	write(*,*)
	stop 'Halted in TIBLSET -- see list file.'
	999   write(io6,*)'FATAL Error in COASTLN.DAT Data'
	write(io6,*)'Met Grid Land Use not consistent with coast'
	write(*,*)
	stop 'Halted in TIBLSET -- see list file.'
	!
end
!----------------------------------------------------------------------
subroutine tiblon(ldb0,ifree,xs,ys,xe,ye,ixs,iys,mds,ixe,iye,mde,&
	zpht,ltibl)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 101111            TIBLON
	!                D. Strimaitis
	!
	! --- PURPOSE:  Screen coastline segments to determine if puff
	!               trajectory can encounter a TIBL, and compute
	!               initial TIBL properties if it does
	!
	! --- UPDATE
	!
	! --- V6.3-V6.402  101111  (DGS): Update GETCELL arguments
	! --- V5.751-V6.3  100212  (DGS): Add nested CALMET grids
	! --- V5.7-V5.751  050805  (DGS): Augment test for onshore trajectory
	!                                 to exclude parallel lines
	!                  050805  (DGS): Place water cell mixing ht (not ht0)
	!                                 into TIBL array locations upwind of
	!                                 the coast
	!                  050805  (DGS): Use land cell pointer in call
	!                                 to TIBLGRO so that shore properties
	!                                 are consistent with coastline as puff
	!                                 crosses coast
	! --- V5.0-V5.7    030402  (FRR): Add 2D met arrays (i2dmet)
	! --- V5.0-V5.0    990228e (DGS): recast loops over ixs,ixe and iys,iye
	!                                 to assure a positive range
	!                  990228e (DGS): fix fstep logic
	!                  990228e (DGS): obtain heat flux from ustar and L
	!                  990228e (DGS): include upwind fetch in search for
	!                                 coast for fresh emissions
	!
	! --- INPUTS:
	!             LDB0 - logical  - Flag controlling printing of internal
	!                               data (F=suppress, T=print)
	!            IFREE - integer  - Flag indicating if puff/slug is free
	!                               of the source (1=free)
	!            XS,YS - real     - Puff position at start of sampling step
	!                               in MET GRID Units (outermost)
	!            XE,YE - real     - Puff position at end of sampling step
	!                               in MET GRID Units (outermost)
	!          IXS,IYS - integer  - Cell for puff position (XS,YS)
	!              MDS - integer  - MET grid domain index for (IXS,IYS)
	!          IXE,IYE - integer  - Cell for puff position (XE,YE)
	!              MDE - integer  - MET grid domain index for (IXE,IYE)
	!             ZPHT - real     - Puff height (m)
	!
	!     Common block /COASTLN/ variables
	!         NCOAST, NPCOAST(2,mxcoast), COASTGRD(2,mxptcst),
	!         LCOAST(mxnx,mxny,mxmetdom), YMXPB(2,mxptcst)
	!     Common block /GRID/ variables
	!         NX, NY, DGRIDI
	!     Common block /METHD/ variables
	!         I2DMET, NEARS(mxnx,mxny,mxmetdom)
	!     Common block /METHR/ variables
	!         WSTAR(mxnx,mxny,mxmetdom), HTMIX(mxnx,mxny,mxmetdom),
	!         TEMPSS(mxss,mxmetdom),UMET(mxnx,mxny,mxnz,mxmetdom),
	!         VMET(mxnx,mxny,mxnz,mxmetdom),USTAR(mxnx,mxny,mxmetdom),
	!         XMONIN(mxnx,mxny,mxmetdom), temp2d(mxnx,mxny,mxmetdom)
	!     Common block /TIBL/ variables
	!         XUPGRD
	!
	!     Parameters:
	!         MXNXY, MXNX, MXNY, MXSS, MXCOAST, MXPTCST, MXMETDOM,  IO6
	!
	! --- OUPUT:
	!            LTIBL - logical  - Flag indicating if TIBL is treated this
	!                               step
	!
	!     Common block /TIBL/ variables
	!          MXTIBL,
	!          NTIBL,IXTIBL(mxtibl),IYTIBL(mxtibl),MDTIBL(mxtibl),
	!          HTIBL(mxtibl),TSTIBL(mxtibl)
	!
	! --- TIBLON called by: COMP
	! --- TIBLON calls:     PTLAPS, TIBLGRO, GETCELL
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	!
	include 'coastln.puf'
	include 'grid.puf'
	include 'methd.puf'
	include 'methr.puf'
	include 'tibl.puf'
	!
	logical ldb0,ldb,ltibl
	! --- Set minimum overwater potential temperature gradient
	data ptgrad0/0.0001/
	! --- Set initial TIBL ht at coast
	data ht0/20./
	! --- Set (1+2*beta), where beta is -ratio of heat flux at the TIBL
	! --- height to that at the surface inland (assumed 0.2 here)
	data r1p2b/1.4/
	! --- Set local variable for a difference (MET Grid Units) small
	! --- enough to be treated as ZERO
	data dsmall/0.000001/
	data zero/0.0/
	! --- Introduce ldb to allow override of ldb0
	ldb=ldb0
	! --- ldb=.TRUE.
	! --- Initialize intercept results to NULL values
	is0=0
	frac=10.*mxnxy
	tiblk=0.
	! --- Find nearest coastline segment that puff trajectory could cross
	! -------------------------------------------------------------------
	! --- Set sampling step vector components and length
	dxp=xe-xs
	dyp=ye-ys
	dsp=SQRT(dxp**2+dyp**2)
	dspm=dsp*dgrid
	! --- No TIBL is calculated if step length is not positive
	if(.not. (dsp.LE.0.)) then ! add by @creaqi goto 900 in fun modify_goto_pure
		! --- Set range of cells to check
		if(ifree.EQ.0) then
			! ---    Fresh emission
			xup=xs-dxp*xupgrd/dsp
			yup=ys-dyp*xupgrd/dsp
			call GETCELL('TIBLON_1    ','GRIDIJ',1,xup,yup,zero,zero,&
			ix1,iy1,md)
		else
			xup=xs
			yup=ys
			ix1=ixs
			iy1=iys
			md=mds
		endif
		ix2=ixe
		iy2=iye
		! --- Switch to outermost grid domain if step spans domains
		if(mds.NE.mde .OR. md.NE.mde) then
			! ---    Revise grid cell range used in search
			md=1
			ix1=xup+1
			if(ix1.LT.1) ix1=1
			if(ix1.GT.nx) ix1=nx
			iy1=yup+1
			if(iy1.LT.1) iy1=1
			if(iy1.GT.ny) iy1=ny
			ix2=xe+1
			if(ix2.LT.1) ix2=1
			if(ix2.GT.nx) ix2=nx
			iy2=ye+1
			if(iy2.LT.1) iy2=1
			if(iy2.GT.ny) iy2=ny
		endif
		ixlo=MIN(ix1,ix2)
		ixhi=MAX(ix1,ix2)
		iylo=MIN(iy1,iy2)
		iyhi=MAX(iy1,iy2)
		! --- Check for a coast segment in any one of these cells
		ltibl=.FALSE.
		do iy=iylo,iyhi
			do ix=ixlo,ixhi
				if(lcoast(ix,iy,md)) ltibl=.TRUE.
			enddo
		enddo
		if (.not. (.NOT.ltibl)) then ! add by @creaqi 900 state_same == True
			! --- Set slope, intercept of trajectory
			if(ABS(dxp).LE.dsmall) then
				! ---    Trajectory is assumed to be vertical (N/S)
				slope=9.91e10
				bintr=9.91e10
			else
				slope=dyp/dxp
				bintr=ys-slope*xs
			endif
			! --- Loop over coasts
			do ic=1,ncoast
				! ---    Loop over segments in coast
				do is=npcoast(1,ic),npcoast(2,ic)-1
					if(md.EQ.1) then
						! ---          Using MET grid domain 1 (outermost)
						ixc1=1+coastgrd(1,is)
						iyc1=1+coastgrd(2,is)
						ixc2=1+coastgrd(1,is+1)
						iyc2=1+coastgrd(2,is+1)
						! ---       else
						! ---          Step is totally within a sub-domain
						call GETCELL('TIBLON_2    ','GRIDIJ',1,coastgrd(1,is),&
						coastgrd(2,is),zero,zero,ixc1,iyc1,kg1)
						call GETCELL('TIBLON_3    ','GRIDIJ',1,coastgrd(1,is+1),&
						coastgrd(2,is+1),zero,zero,ixc2,iyc2,kg2)
						if(md.NE.kg1 .OR. md.NE.kg2) then
							write(io6,*)'FATAL ERROR:  TIBLON'
							write(io6,*)'Coast segment is not in expected '
							write(io6,*)'MET grid domain: ',md
							write(io6,*)'Segment starts in domain: ',kg1
							write(io6,*)'Segment   ends in domain: ',kg2
							write(*,*)
							stop 'Halted in TIBLON -- see list file.'
						endif
					endif
					!
					! ---       Loop over cells in sampling step
					do iy=iylo,iyhi
						do ix=ixlo,ixhi
							!
							if((ixc1.EQ.ix .AND. iyc1.EQ.iy) .OR.&
								(ixc2.EQ.ix .AND. iyc2.EQ.iy)) then
								! ---             Test this segment
								dxc=coastgrd(1,is+1)-coastgrd(1,is)
								dyc=coastgrd(2,is+1)-coastgrd(2,is)
								! ---             Coast Vector [CROSS] Puff Vector:
								cxp=dxc*dyp-dxp*dyc
								! ---             Screen out equal slopes (cxp=0
								! ---             if slopes are equal, but precision may
								! ---             let this slip through)
								if(cxp.GT.0.0 .AND. slope.NE.ymxpb(1,is)) then
									! ---                Trajectory is onshore, find intercept
									if(slope.GT.9.9e10) then
										xint=xs
										yint=ymxpb(1,is)*xint+ymxpb(2,is)
									elseif(ymxpb(1,is).GT.9.9e10) then
										xint=coastgrd(1,is)
										yint=slope*xint+bintr
									else
										xint=(bintr-ymxpb(2,is))/(ymxpb(1,is)-slope)
										yint=slope*xint+bintr
									endif
									! ---                Intercept must be within the coast segment
									if(ymxpb(1,is).GT.9.9e10) then
										frac1=(yint-coastgrd(2,is))/dyc
									else
										frac1=(xint-coastgrd(1,is))/dxc
									endif
									if(frac1.GE.0.0 .AND. frac1.LE.1.0) then
										! ---                   Save this segment if it is nearest start
										if(slope.GT.9.9e10) then
											frac2=(yint-ys)/dyp
										else
											frac2=(xint-xs)/dxp
										endif
										if(ABS(frac2).LT.ABS(frac)) then
											frac=frac2
											fraccst=frac1
											is0=is
										endif
									endif
								endif
							endif
							!
						enddo
					enddo
					!
				enddo
			enddo
			! --- DEBUG Output Section
			! ------------------------
			if(LDB) then
				write(io6,*)
				write(io6,*)'TIBLON ---------------'
				write(io6,*)'Coastline segments screened for intersection in'
				write(io6,*)'ix1,iy1,ix2,iy2: ',ix1,iy1,ix2,iy2
				write(io6,*)'Nearest coastline segment selected     = ',is0
				write(io6,*)'Coast crossed @ sampling step fraction = ',frac
				write(io6,*)
			endif
			! ---------------------
			! --- Interpret results
			! ---------------------
		endif ! add by @creaqi 900 state_same == True
	endif !add by @creaqi label 900 modify_goto_pure
	900   if(is0.EQ.0 .OR. frac.GE.1.0) then
		! ---    Valid intercept not found:  No TIBL this step
		! ----------------------------------------------------
		ltibl=.FALSE.
		return
	endif
	! --- Compute initial TIBL properties
	! -----------------------------------
	! --- Pick the cell data nearest the intercept
	ipt=is0
	if(NINT(fraccst).GE.1) ipt=is0+1
	! --- Nearest LAND cell:
	ipland=lwcell(1,ipt)
	mdland=lwmdom(1,ipt)
	ixl=MOD(ipland,nxm(mdland))
	if(ixl.EQ.0)ixl=nxm(mdland)
	iyl=1+(ipland-ixl)/nxm(mdland)
	! --- Nearest WATER cell:
	ipwater=lwcell(2,ipt)
	mdwater=lwmdom(2,ipt)
	ixw=MOD(ipwater,nxm(mdwater))
	if(ixw.EQ.0)ixw=nxm(mdwater)
	iyw=1+(ipwater-ixw)/nxm(mdwater)
	! --- Set over-land met data
	wstr=wstar(ixl,iyl,mdland)
	xmol=xmonin(ixl,iyl,mdland)
	! !!!c --- Abort if wstar not positive (no TIBL)
	! !!!      if(wstr.LE.0.0) then
	! --- Abort if Monin-Obukhov length is positive (no TIBL)
	if(xmol.GE.0.0) then
		ltibl=.FALSE.
		return
	endif
	ziland=htmix(ixl,iyl,mdland)
	ustr=ustar(ixl,iyl,mdland)
	! 
	! frr (09/01) new calmet format (2D temp)
	if(i2dmet.EQ.1) then
		tss=temp2d(ixl,iyl,mdland)
	elseif(i2dmet.EQ.0) then
		tss=tempss(nears(ixl,iyl,mdland),mdland)
	else
		write(*,*)'Subr. TIBLON:  Invalid I2DMET = ',i2dmet
		stop
	endif
	!     tss=tempss(nears(ixl,iyl,mdland),mdland)
	hbyrcp=-ustr**3*tss/(9.8*0.4*xmol)
	! --- Abort if sensible heat flux < 5 W/m**2 (no TIBL)
	if(hbyrcp.LE.0.004) then
		ltibl=.FALSE.
		return
	endif
	! --- Mean speed for TIBL growth taken equal to layer 1 speed
	u0=SQRT(umet(ixl,iyl,1,mdland)**2+vmet(ixl,iyl,1,mdland)**2)
	! --- Set over-water met data
	ziwater=htmix(ixw,iyw,mdwater)
	! --- Use pot. temperature (K) gradient of overwater layer from HT0
	! --- to HT0+mixed layer depth. Minimum gradient is PTGRAD0,
	! --- and minimum layer thickness is 100m.
	delz=AMAX1(ziwater,100.)
	call PTLAPS(ixw,iyw,mdwater,ht0,ptgrad0,delz,ptgrad,t0)
	! --- Compute K-factor for initial rate of TIBL growth
	tiblk=(r1p2b*hbyrcp)/(ptgrad*u0)
	! --- Additional DEBUG Output Section
	! -----------------------------------
	if(LDB) then
		write(io6,*)'ixw,iyw,mdwater      : ',ixw,iyw,mdwater
		write(io6,*)'ixl,iyl,mdland       : ',ixl,iyl,mdland
		write(io6,*)'wstr,tss             : ',wstr,tss
		write(io6,*)'zpht,ustr,xmol       : ',zpht,ustr,xmol
		write(io6,*)'u0,ptgrad,hbyrcp     : ',u0,ptgrad,hbyrcp 
		write(io6,*)'ziland,ziwater,tiblk : ',ziland,ziwater,tiblk
		write(io6,*)'step length (m)      : ',dspm
	endif
	! --- Fill the TIBL arrays
	! -------------------------
	! --- Case 1: Coast is upwind of start of step
	if(frac.LT.0.0) then
		! ---    Grow TIBL to the start of the current step
		xdist=-frac*dspm
		! ---    Initial growth rate is used for first X2ZI1 (m) from coast
		x1=AMIN1(xdist,x2zi1)
		ht1sq=2.*tiblk*x1+ht0**2
		if(xdist.GT.x1) then
			! ---       Augment growth rate for transition to inland mixing ht
			zik=htmix(ixs,iys,mds)**2/(2.*x2zi2)
			tiblk=AMAX1(tiblk,zik)
			! ---       Compute growth for additional distance to start of step
			ht1sq=2.*tiblk*(xdist-x1)+ht1sq
		endif
		ht1=AMIN1(SQRT(ht1sq),htmix(ixs,iys,mds))
		! ---    Compute TIBL ht during step
		tiblku=tiblk*u0
		call TIBLGRO(ldb,xs,ys,xe,ye,ixl,iyl,mdland,ht1,&
		-.001,tiblku)
		! --- Case 2:  Coast is within the step
	else
		! ---    First entry is interval to coast
		ntibl=1
		tstibl(ntibl)=AMIN1(frac,1.0)
		htibl(ntibl)=ziwater
		ixtibl(ntibl)=ixw
		iytibl(ntibl)=iyw
		mdtibl(ntibl)=mdwater
		! ---    Remaining entries filled if needed (use land cell here)
		if(frac.LT.1.0) then
			tiblku=tiblk*u0
			call TIBLGRO(ldb,xs,ys,xe,ye,ixl,iyl,mdland,ht0,&
			frac,tiblku)
		endif
	endif
	return
end
!----------------------------------------------------------------------
subroutine tiblgro(ldb0,xs,ys,xe,ye,ixr,iyr,mdr,ht0,&
	frac0,tiblku0)
	!----------------------------------------------------------------------
	!
	! --- CALPUFF    Version: 7.3.1        Level: 101111           TIBLGRO
	!                D. Strimaitis
	!
	! --- PURPOSE:  Grow an existing TIBL along the path of a puff, using
	!               current land use
	!
	! --- UPDATES
	! --- V6.3-V6.402   101111 (DGS): Update GETCELL arguments
	! --- V6.11-V6.3    100212 (DGS): Add nested CALMET grids
	! --- V5.751-V6.11  060309 (DGS): Test for small/negative heat flux
	!                                 during step
	! --- V5.711-V5.751 050805 (DGS): Change puff start/end cell indices
	!                                 to a cell override index ixr,iyr
	!                                 (-1,-1 if no override)
	! --- V5.7-V5.711  030625  (DGS): Fix ix,iy assignment logic to avoid
	!                                 undefined outcome
	! --- V5.4-V5.7    030402  (FRR): Add 2D met arrays (i2dmet)
	! --- V5.3-V5.4    000602  (DGS): add message to "stop"
	! --- V5.2-V5.3    991222  (DGS): Use MIN,MAX in place of MIN1,MAX1
	! --- V5.1-V5.2    991104  (JSS): Error messages written to list
	!                                 file as well as to screen
	! --- V5.0-V5.0    990228e (DGS): add adaptive step size
	!    (from 980515) 990228e (DGS): obtain heat flux from ustar and L
	!                  998228e (DGS): add transition to inland mixing hts.
	!                  998228e (DGS): allow for a fractional step
	!
	! --- INPUTS:
	!             LDB0 - logical  - Flag controlling printing of internal
	!                               data (F=suppress, T=print)
	!            XS,YS - real     - Puff position at start of sampling step
	!                               in MET GRID Units
	!            XE,YE - real     - Puff position at end of sampling step
	!                               in MET GRID Units
	!          IXR,IYR - integer  - Replacement Cell for grid properties
	!                               overrides computed cell if not (-1,-1)
	!              MDR - integer  - MET grid domain for [IYR,IYR]
	!              HT0 - real     - TIBL height (m) at the start of the step
	!            FRAC0 - real     - Fraction of step already assigned in TIBL
	!                               arrays (if greater than zero)
	!          TIBLKU0 - real     - TIBL rate K times U at the coast
	!
	!     Common block /GRID/ variables
	!         DGRID
	!     Common block /METHD/ variables
	!         NEARS(mxnx,mxny,mxmetdom), ZO(mxnx,mxny,mxmetdom),
	!         ILANDU(mxnx,mxny,mxmetdom), IWAT1, IWAT2 , I2DMET
	!     Common block /METHR/ variables
	!         WSTAR(mxnx,mxny,mxmetdom), HTMIX(mxnx,mxny,mxmetdom),
	!         XMONIN(mxnx,mxny,mxmetdom), IPGT(mxnx,mxny,mxmetdom),
	!         TEMPSS(mxss,mxmetdom), USTAR(mxnx,mxny,mxmetdom),
	!         TEMP2D(mxnx,mxny,mxmetdom)
	!
	!     Parameters:
	!         MXNX, MXNY, MXSS, MXMETDOM, IO6
	!
	! --- OUPUT:
	!
	!     Common block /TIBL/ variables
	!          MXTIBL
	!          NTIBL,IXTIBL(mxtibl),IYTIBL(mxtibl),MDTIBL(mxtibl),
	!          HTIBL(mxtibl),TSTIBL(mxtibl)
	!
	!
	! --- TIBLGRO called by: COMP, TIBLON
	! --- TIBLGRO calls:     PTLAPS, ADVECT, GETCELL
	!----------------------------------------------------------------------
	!
	include 'params.puf'
	!
	include 'grid.puf'
	include 'methd.puf'
	include 'methr.puf'
	include 'tibl.puf'
	!
	logical ldb0,ldb
	data zero/0.0/, delz/100./
	! --- Set minimum overwater potential temperature gradient
	data ptgrad0/0.0001/
	! --- Set (1+2*beta), where beta is -ratio of heat flux at the TIBL
	! --- height to that at the surface inland (assumed 0.2 here)
	data r1p2b/1.4/
	! --- Set minimum sub-step size (m)
	data ssmin/20./
	! --- Introduce ldb to allow override of ldb0
	ldb=ldb0
	! --- ldb=.TRUE.
	! --- Set sampling step vector components and length
	dxp=xe-xs
	dyp=ye-ys
	dspm=SQRT(dxp**2+dyp**2)*dgrid
	! --- Set fraction of step that remains
	if(frac0.LE.0.0) then      
		frac=1.0
		n1=1
	elseif(frac0.LT.1.0) then
		frac=1.0-frac0
		n1=2
	else
		write(io6,*)'FATAL ERROR:  TIBLGRO'
		write(io6,*)'Fraction of step already used cannot exceed 1'
		write(io6,*)'     frac0 = ',frac0
		write(*,*)
		stop 'Halted in TIBLGRO -- see list file.'
	endif
	! --- Compute number of sampling substeps (NTIBL) to use
	sdel=AMAX1(ht0,ssmin)
	ntibl=(frac*dspm/sdel)+n1
	ntibl=MIN(ntibl,mxtibl)
	ntibl=MAX(ntibl,n1)
	! --- Divide the sampling step into NTIBL pieces to resolve TIBL
	fstep=frac/FLOAT(ntibl+1-n1)
	dstepm=fstep*dspm
	delx=fstep*dxp
	dely=fstep*dyp
	xnew=xs+frac0*dxp
	ynew=ys+frac0*dyp
	hnew=ht0
	! --- Loop over sub-steps
	do it=n1,ntibl
		tstibl(it)=fstep
		! ---    Define sub-step
		xold=xnew
		yold=ynew
		hold=hnew
		xnew=xold+delx
		ynew=yold+dely
		! ---    Set cell for surface properties (choose overland cell if
		! ---    choice is needed)
		if(ixr.GT.0 .AND. iyr.GT.0) then
			ix=ixr
			iy=iyr
			md=mdr
		else
			call GETCELL('TIBLGRO_1   ','GRIDIJ',1,xold,yold,&
			zero,zero,ixo,iyo,mdo)
			call GETCELL('TIBLGRO_2   ','GRIDIJ',1,xnew,ynew,&
			zero,zero,ixn,iyn,mdn)
			ix=ixo
			iy=iyo
			if(ixo.NE.ixn .OR. iyo.NE.iyn) then
				if(ilandu(ixo,iyo,mdo).LT.iwat1 .OR.&
					ilandu(ixo,iyo,mdo).GT.iwat2) then
					ix=ixo
					iy=iyo
					md=mdo
				elseif(ilandu(ixn,iyn,mdn).LT.iwat1 .OR.&
					ilandu(ixn,iyn,mdn).GT.iwat2) then
					ix=ixn
					iy=iyn
					md=mdn
				endif
			endif
		endif
		! ---    Obtain cell properties
		! !!!         wstr=AMAX1(0.0,wstar(ix,iy,md))
		! !!!         hbyrcp=wstr**3*tempss(nears(ix,iy,md),md)/(9.8*zi)
		zi=htmix(ix,iy,md)
		ustr=AMAX1(0.0,ustar(ix,iy,md))
		xmol=xmonin(ix,iy,md)
		! frr (09/01) new calmet format (2D temp)
		if(i2dmet.EQ.1) then
			tss=temp2d(ix,iy,md)
		elseif(i2dmet.EQ.0) then
			tss=tempss(nears(ix,iy,md),md)
		else
			write(*,*)'Subr. TIBLGRO:  Invalid I2DMET = ',i2dmet
			stop
		endif
		hbyrcp=-ustr**3*tss/(9.8*0.4*xmol)
		! ---    Use pot. temperature (K) gradient above ZI (min is PTGRAD0)
		call PTLAPS(ix,iy,md,zi,ptgrad0,delz,ptgrad,t0)
		! ---    Mean speed for TIBL growth is average from 0 to last TIBL ht
		call ADVECT(ldb0,ix,iy,md,z0(ix,iy,md),xmonin(ix,iy,md),zi,&
		ipgt(ix,iy,md),hold,zero,hold,uadv,vadv)
		utibl=SQRT(uadv**2+vadv**2)
		! ---    Compute K-factor for rate of TIBL growth
		tiblk0=(r1p2b*hbyrcp)/(ptgrad*utibl)
		! ---    Compute the effective distance from coast to reach current ht
		! ---    using this K-factor
		! ---    Note:  Heat flux may be zero or negative here (set s0 large)
		! ---           Use sensible heat flux < 5 W/m**2 as cut-off
		if(hbyrcp.GT.0.004) then
			s0=0.5*hold**2/tiblk0
		else
			s0=x2zi1
		endif
		! ---    Adjust for transition to inland mixing ht
		if(s0.GE.x2zi1) then
			zik=zi**2/(2.*x2zi2)
			tiblk=AMAX1(tiblk0,zik)
		else
			tiblk=tiblk0
		endif
		! ---    Replace this rate if that found at the coast is larger
		tiblk=AMAX1(tiblk,tiblku0/utibl)
		! ---    Compute TIBL height at end of this sub-step
		hnew=SQRT(2.*tiblk*dstepm+hold**2)
		htibl(it)=hnew
		ixtibl(it)=ix
		iytibl(it)=iy
		mdtibl(it)=md
	enddo    
	! --- DEBUG Output Section
	! ------------------------
	if(LDB) then
		write(io6,*)
		write(io6,*)'TIBLGRO --------------'
		write(io6,*)'Replacement cell for Met properties ---'
		write(io6,*)'ixr,iyr,mdr          : ',ixr,iyr,mdr
		write(io6,*)'Initial TIBL height (m) is : ',ht0
		write(io6,*)'at step fraction     : ',frac0
		write(io6,*)'full step length (m) : ',dspm
		write(io6,*)'Properties of cell at end of step ---'
		write(io6,*)'wstr,tss             : ',wstar(ix,iy,md),tss
		write(io6,*)'ustr,xmol            : ',ustr,xmol
		write(io6,*)'utibl,ptgrad,hbyrcp  : ',utibl,ptgrad,hbyrcp 
		write(io6,*)'ziland,ziK           : ',zi,zik
		write(io6,*)'tiblku0,tiblk0,tiblk : ',tiblku0,tiblk0,tiblk
		do it=1,ntibl
			write(io6,*)'it,htibl,tstibl      : ',it,htibl(it),tstibl(it)
		enddo
	endif
	return
end

