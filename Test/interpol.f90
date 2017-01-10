  program interpol
  use ncroutines
  use module_write_output
!  implicit none
  integer :: i,j,k,it,ncid,sp,m,l,domain,ii,jj
  real, allocatable,dimension(:) :: lon
  real, allocatable,dimension(:) :: lat,time
  integer, allocatable,dimension(:) :: date
  integer, parameter :: nlat=96, nlon=144, ntime=60
  real, allocatable, dimension(:,:) :: region_codes
  real, allocatable, dimension(:,:,:) :: ENERGY, INDUSTRY, TRANSPORT, &
                    RESIDENTIAL, SHIPS,anthro,bb,biogenic,ocean
                                       
  real, allocatable, dimension(:,:,:) :: anthro_NAM,biogenic_NAM, bb_NAM, ocean_NAM
  real, allocatable, dimension(:,:,:) :: anthro_EUR,biogenic_EUR, bb_EUR, ocean_EUR
  real, allocatable, dimension(:,:,:) :: anthro_EAS,biogenic_EAS, bb_EAS, ocean_EAS
  real, allocatable, dimension(:,:,:) :: anthro_SAS,biogenic_SAS, bb_SAS, ocean_SAS
  real, allocatable, dimension(:,:,:) :: anthro_RBU,biogenic_RBU, bb_RBU, ocean_RBU
  real, allocatable, dimension(:,:,:) :: anthro_MDE,biogenic_MDE, bb_MDE, ocean_MDE
  real, allocatable, dimension(:,:,:) :: anthro_OCN,biogenic_OCN, bb_OCN, ocean_OCN
  real, allocatable, dimension(:,:,:) :: anthro_RST,biogenic_RST, bb_RST, ocean_RST
  character(len=100) :: filename, path,filename1
  character(len = 1000) :: outfile1,outfile2,outfile3
  character(len = 1000) :: outfile4,outfile5,outfile6
  character(len = 1000) :: outfile7,outfile8,outfile9
  character(len = 1000) :: outfile10,outfile11,outfile12
  character(len = 1000) :: outfile13,outfile14,outfile15
  character(len = 1000) :: outfile16,outfile17,outfile18
  character(len = 1000) :: outfile19,outfile20,outfile21
  character(len = 1000) :: outfile22,outfile23,outfile24
  character(len = 100), parameter :: outfile_method = 'create' 

  path = "/p/projects/IASS/alu/HTAP"
     print*, trim(path)
     write(filename,&
          '("aura_EUROPA.nc")')
    call nf90call(nf90_open(trim(filename), &
          NF90_COWRITE,ncid), &
          "Could not open "//trim(filename))

   allocate(region_codes(nlon,nlat))
   allocate(lat(nlat))
   allocate(lon(nlon))

   call read2das1dnc(ncid,"lat",(/1,1/),(/nlat,1/),lat)
   call read2das1dnc(ncid,"lon",(/1,1/),(/nlon,1/),lon)
   call read3das2dnc(ncid,"region_codes",(/1,1,1/),(/nlon,nlat,1/),region_codes)

    write(filename1,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.nc")')
     print*,filename1
!
   write(outfile1,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.NAM.ant.nc")')
   write(outfile2,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.NAM.bb.nc")')
   write(outfile3,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.NAM.bio.nc")')
!
   write(outfile4,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.EUR.ant.nc")')
   write(outfile5,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.EUR.bb.nc")')
   write(outfile6,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.EUR.bio.nc")')
!
   write(outfile7,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.EAS.ant.nc")')
   write(outfile8,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.EAS.bb.nc")')
   write(outfile9,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.EAS.bio.nc")')
!
   write(outfile10,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.SAS.ant.nc")')
   write(outfile11,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.SAS.bb.nc")')
   write(outfile12,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.SAS.bio.nc")')
!
   write(outfile13,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.RBU.ant.nc")')
   write(outfile14,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.RBU.bb.nc")')
   write(outfile15,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.RBU.bio.nc")')
!
   write(outfile16,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.MDE.ant.nc")')
   write(outfile17,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.MDE.bb.nc")')
   write(outfile18,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.MDE.bio.nc")')
!
   write(outfile19,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.OCN.ant.nc")')
   write(outfile20,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.OCN.bb.nc")')
   write(outfile21,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.OCN.bio.nc")')
!
   write(outfile22,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.RST.ant.nc")')
   write(outfile23,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.RST.bb.nc")')
   write(outfile24,'("emissions_htap2sectors-gfed3_2007-2011_CO_mol_1.9x2.5.RST.bio.nc")')
!
     call nf90call(nf90_open(trim(path)//"/"//trim(filename1), &
         NF90_COWRITE,ncid), &
         "Could not open "//trim(path)//"/"//trim(filename1))
!
! get dimensions of area grid and allocate necessary arrays
  allocate(ENERGY(nlon,nlat,ntime))
  allocate(INDUSTRY(nlon,nlat,ntime))
  allocate(TRANSPORT(nlon,nlat,ntime))
  allocate(RESIDENTIAL(nlon,nlat,ntime))
  allocate(SHIPS(nlon,nlat,ntime))
  allocate(anthro_NAM(nlon,nlat,ntime))
  allocate(biogenic(nlon,nlat,ntime))
  allocate(ocean(nlon,nlat,ntime))
  allocate(biogenic_NAM(nlon,nlat,ntime))
  allocate(bb(nlon,nlat,ntime))
  allocate(bb_NAM(nlon,nlat,ntime))
  allocate(time(ntime))
  allocate(date(ntime))
  allocate(anthro_EUR(nlon,nlat,ntime),biogenic_EUR(nlon,nlat,ntime),bb_EUR(nlon,nlat,ntime))
  allocate(anthro_EAS(nlon,nlat,ntime),biogenic_EAS(nlon,nlat,ntime),bb_EAS(nlon,nlat,ntime))
  allocate(anthro_SAS(nlon,nlat,ntime),biogenic_SAS(nlon,nlat,ntime),bb_SAS(nlon,nlat,ntime))
  allocate(anthro_RBU(nlon,nlat,ntime),biogenic_RBU(nlon,nlat,ntime),bb_RBU(nlon,nlat,ntime))
  allocate(anthro_MDE(nlon,nlat,ntime),biogenic_MDE(nlon,nlat,ntime),bb_MDE(nlon,nlat,ntime))
  allocate(anthro_OCN(nlon,nlat,ntime),biogenic_OCN(nlon,nlat,ntime),bb_OCN(nlon,nlat,ntime))
  allocate(anthro_RST(nlon,nlat,ntime),biogenic_RST(nlon,nlat,ntime),bb_RST(nlon,nlat,ntime))
  allocate(ocean_NAM(nlon,nlat,ntime),ocean_EUR(nlon,nlat,ntime),ocean_EAS(nlon,nlat,ntime),ocean_SAS(nlon,nlat,ntime),&
          ocean_RBU(nlon,nlat,ntime),ocean_MDE(nlon,nlat,ntime),ocean_OCN(nlon,nlat,ntime),ocean_RST(nlon,nlat,ntime))
  call read2das1dnc(ncid,"time",(/1,1/),(/ntime,1/),time)
  call read2das1dnc_cesm(ncid,"date",(/1,1/),(/ntime,1/),date)
  call read4das3dnc(ncid,"ENERGY",(/1,1,1,1/),(/nlon,nlat,ntime,1/),ENERGY)
  call read4das3dnc(ncid,"INDUSTRY",(/1,1,1,1/),(/nlon,nlat,ntime,1/),INDUSTRY)
  call read4das3dnc(ncid,"TRANSPORT",(/1,1,1,1/),(/nlon,nlat,ntime,1/),TRANSPORT)
  call read4das3dnc(ncid,"RESIDENTIAL",(/1,1,1,1/),(/nlon,nlat,ntime,1/),RESIDENTIAL)
  call read4das3dnc(ncid,"SHIPS",(/1,1,1,1/),(/nlon,nlat,ntime,1/),SHIPS)
  call read4das3dnc(ncid,"biogenic",(/1,1,1,1/),(/nlon,nlat,ntime,1/),biogenic)
  call read4das3dnc(ncid,"ocean",(/1,1,1,1/),(/nlon,nlat,ntime,1/),ocean)
  call read4das3dnc(ncid,"bb",(/1,1,1,1/),(/nlon,nlat,ntime,1/),bb)
  call nf90call(nf90_close(ncid))
  
! calculate what????
   j_loop: do j=1,96
    i_loop: do i=1,144
     it_loop: do it=1,60
! do north america
      if (region_codes(i,j) .eq. 3) then 
      ! sum_total=0.
      ! sum_nam=0.
      ! sum_nam=sum_rbu+o3_x_namiant(i,j,k)
      ! print*, sum_rbu/sum_total
     
            anthro_NAM(i,j,it)=ENERGY(i,j,it)+INDUSTRY(i,j,it)+TRANSPORT(i,j,it)+&
                          RESIDENTIAL(i,j,it)+ SHIPS(i,j,it)
            biogenic_NAM(i,j,it)=biogenic(i,j,it)+ocean(i,j,it)
            bb_NAM(i,j,it)=bb(i,j,it)
      endif 
! do europe
     if (region_codes(i,j) .eq. 4) then
            anthro_EUR(i,j,it)=ENERGY(i,j,it)+INDUSTRY(i,j,it)+TRANSPORT(i,j,it)+&
                          RESIDENTIAL(i,j,it)+ SHIPS(i,j,it)
            biogenic_EUR(i,j,it)=biogenic(i,j,it)+ocean(i,j,it)
            bb_EUR(i,j,it)=bb(i,j,it)
      endif
! do east asia
     if (region_codes(i,j) .eq. 6) then
            anthro_EAS(i,j,it)=ENERGY(i,j,it)+INDUSTRY(i,j,it)+TRANSPORT(i,j,it)+&
                          RESIDENTIAL(i,j,it)+ SHIPS(i,j,it)
            biogenic_EAS(i,j,it)=biogenic(i,j,it)+ocean(i,j,it)
            bb_EAS(i,j,it)=bb(i,j,it)
      endif
! do south asia
     if (region_codes(i,j) .eq. 5) then
            anthro_SAS(i,j,it)=ENERGY(i,j,it)+INDUSTRY(i,j,it)+TRANSPORT(i,j,it)+&
                          RESIDENTIAL(i,j,it)+ SHIPS(i,j,it)
            biogenic_SAS(i,j,it)=biogenic(i,j,it)+ocean(i,j,it)
            bb_SAS(i,j,it)=bb(i,j,it)
      endif
! do rusia
     if (region_codes(i,j) .eq. 14) then
            anthro_RBU(i,j,it)=ENERGY(i,j,it)+INDUSTRY(i,j,it)+TRANSPORT(i,j,it)+&
                          RESIDENTIAL(i,j,it)+ SHIPS(i,j,it)
            biogenic_RBU(i,j,it)=biogenic(i,j,it)+ocean(i,j,it)
            bb_RBU(i,j,it)=bb(i,j,it)
      endif
! do middle east
     if (region_codes(i,j) .eq. 11) then
            anthro_MDE(i,j,it)=ENERGY(i,j,it)+INDUSTRY(i,j,it)+TRANSPORT(i,j,it)+&
                          RESIDENTIAL(i,j,it)+ SHIPS(i,j,it)
            biogenic_MDE(i,j,it)=biogenic(i,j,it)+ocean(i,j,it)
            bb_MDE(i,j,it)=bb(i,j,it)
      endif
! do ocean
     if (region_codes(i,j) .eq. 2) then
            anthro_OCN(i,j,it)=ENERGY(i,j,it)+INDUSTRY(i,j,it)+TRANSPORT(i,j,it)+&
                          RESIDENTIAL(i,j,it)+ SHIPS(i,j,it)
            biogenic_OCN(i,j,it)=biogenic(i,j,it)+ocean(i,j,it)
            bb_OCN(i,j,it)=bb(i,j,it)
      endif
! do the rest 
!    if (region_codes(i,j) .ne. 2 .and. region_codes(i,j) .ne. 3 .and. region_codes(i,j) .and. 4 .and. &
!        region_codes(i,j) .ne. 5 .and. region_codes(i,j) .ne. 6 .and. region_codes(i,j) .and. 11 .and. &
!        region_codes(i,j) .ne. 14) then
            anthro_RST(i,j,it)=ENERGY(i,j,it)+INDUSTRY(i,j,it)+TRANSPORT(i,j,it)+&
                          RESIDENTIAL(i,j,it)+ SHIPS(i,j,it)-anthro_NAM(i,j,it)- &
                          anthro_EUR(i,j,it)-anthro_EAS(i,j,it)-anthro_SAS(i,j,it)- &
                          anthro_RBU(i,j,it)-anthro_MDE(i,j,it)-anthro_OCN(i,j,it)
            biogenic_RST(i,j,it)=biogenic(i,j,it)-biogenic_NAM(i,j,it)-biogenic_EUR(i,j,it)- &
                             biogenic_EAS(i,j,it)-biogenic_SAS(i,j,it)-biogenic_RBU(i,j,it)- &
                             biogenic_MDE(i,j,it)-biogenic_OCN(i,j,it)
            bb_RST(i,j,it)=bb(i,j,it)-bb_NAM(i,j,it)-bb_EUR(i,j,it)-bb_EAS(i,j,it)- &
                  bb_SAS(i,j,it)-bb_RBU(i,j,it)-bb_MDE(i,j,it)-bb_OCN(i,j,it)
 !     endif
!

     enddo it_loop 
    enddo i_loop 
   enddo j_loop 
!  call write_output_netcdf(outfile_method, outfile1,outfile2,outfile3, nlon, nlat, ntime, &
!
!       lat,lon,time, date, anthro_NAM,biogenic_NAM,bb_NAM) 
!  call write_output_netcdf_EUR(outfile_method, outfile4,outfile5,outfile6, nlon, nlat, ntime, &
!       lat,lon,time, date, anthro_EUR,biogenic_EUR,bb_EUR) 
!  call write_output_netcdf_EAS(outfile_method, outfile7,outfile8,outfile9, nlon, nlat, ntime, &
!       lat,lon,time, date, anthro_EAS,biogenic_EAS,bb_EAS) 
!  call write_output_netcdf_SAS(outfile_method, outfile10,outfile11,outfile12, nlon, nlat, ntime, &
!       lat,lon,time, date, anthro_SAS,biogenic_SAS,bb_SAS)
!  call write_output_netcdf_RBU(outfile_method, outfile13,outfile14,outfile15, nlon, nlat, ntime, &
!       lat,lon,time, date, anthro_RBU,biogenic_RBU,bb_RBU)
!  call write_output_netcdf_MDE(outfile_method, outfile16,outfile17,outfile18, nlon, nlat, ntime, &
!       lat,lon,time, date, anthro_MDE,biogenic_MDE,bb_MDE)
!  call write_output_netcdf_OCN(outfile_method, outfile19,outfile20,outfile21, nlon, nlat, ntime, &
!       lat,lon,time, date, anthro_OCN,biogenic_OCN,bb_OCN)
!  call write_output_netcdf_RST(outfile_method, outfile22,outfile23,outfile24, nlon, nlat, ntime, &
!       lat,lon,time, date, anthro_RST,biogenic_RST,bb_RST)
  deallocate( lat,lon, ENERGY, INDUSTRY, TRANSPORT, RESIDENTIAL, SHIPS, anthro_NAM, biogenic,bb, biogenic_NAM, bb_NAM, date, time, region_codes, &
        anthro_EUR,biogenic_EUR,bb_EUR,anthro_EAS,biogenic_EAS,bb_EAS,anthro_SAS,biogenic_SAS,bb_SAS, &
        anthro_RBU,biogenic_RBU, bb_RBU,anthro_MDE,biogenic_MDE, bb_MDE,anthro_OCN,biogenic_OCN, bb_OCN,anthro_RST,biogenic_RST, bb_RST)
101 continue
77 format(2f12.5)
78 format(2(i4,x),f12.5,e12.5)
 stop 
 end program interpol

!!************************************************************************
