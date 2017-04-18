 module module_write_output
  implicit none
 contains
  subroutine write_output_netcdf(outfile_method, outfile1, nlon, nlat, ntime, &
       lat,lon,time, date, NAM)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile1
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: NAM

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft


     print*, 'method create'
! write file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile1,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
         call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
! aura replace NF90_FLOAT with NF90_FLOAT 
          call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",NAM,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf
!---------------------------------------------------------------------------------------

  subroutine write_output_netcdf_EUR(outfile_method, outfile2,nlon, nlat, ntime, &
       lat,lon,time, date, EUR)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile2
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: EUR

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile2,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
      call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
      call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
      call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))

      call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
      call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
      call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
      call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
      call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
      call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGARemissions"))
      call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
              (/dimid_nlon/),varid))
      call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
      call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
      call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",EUR,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_EUR
!---------------------------------------------------------------------------

  subroutine write_output_netcdf_EAS(outfile_method, outfile3,nlon, nlat, ntime, &
       lat,lon,time, date, EAS)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile3
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: EAS

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile3,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",EAS,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_EAS
!-------------------------------------------------------------------------------------------------------------

  subroutine write_output_netcdf_SAS(outfile_method, outfile4,nlon, nlat, ntime, &
       lat,lon,time, date, SAS)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile4
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: SAS

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile4,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",SAS,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_SAS
!-------------------------------------------------------------------

  subroutine write_output_netcdf_RBU(outfile_method, outfile5, nlon, nlat, ntime, &
       lat,lon,time, date, RBU)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile5
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: RBU

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile5,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",RBU,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_RBU
!------------------------------------------------------------------------------------------------

  subroutine write_output_netcdf_MDE(outfile_method, outfile6, nlon, nlat, ntime, &
       lat,lon,time, date, MDE)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile6
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: MDE

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile6,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",MDE,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_MDE
!----------------------------------------------------------------------------------

  subroutine write_output_netcdf_OCN(outfile_method, outfile7,nlon, nlat, ntime, &
       lat,lon,time, date, OCN)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile7
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: OCN

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile7,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",OCN,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_OCN
!-------------------------------------------------------------------------------------

  subroutine write_output_netcdf_SEA(outfile_method, outfile8,nlon, nlat, ntime, &
       lat,lon,time, date, SEA)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile8
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: SEA

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile8,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",SEA,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_SEA
!-------------------------------------------------------------------------------------

  subroutine write_output_netcdf_NAF(outfile_method, outfile9,nlon, nlat, ntime, &
       lat,lon,time, date, NAF)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile9
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: NAF

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile9,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",NAF,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_NAF
!-------------------------------------------------------------------------------------

  subroutine write_output_netcdf_MCA(outfile_method, outfile10,nlon, nlat, ntime, &
       lat,lon,time, date, MCA)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile10
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: MCA

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile10,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",MCA,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_MCA
!-------------------------------------------------------------------------------------

  subroutine write_output_netcdf_RST(outfile_method, outfile11,nlon, nlat, ntime, &
       lat,lon,time, date, RST)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile11
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: RST

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile11,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",RST,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'total_emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_RST
! central asia
!-------------------------------------------------------------------------------------
  subroutine write_output_netcdf_CAS(outfile_method, outfile12, nlon, nlat, ntime, &
      lat,lon,time, date, CAS)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile12
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: CAS

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write total_emissions file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile12,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))

       call nf90call(nf90_def_var(ncidout,"total_emissions",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
!
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Jane"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"total_emissions",CAS,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'central asian emissions was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
    !
  end subroutine write_output_netcdf_CAS
!----------------------------------------------

!-------------------------------------------------------------------------------------------------------
 end module 
