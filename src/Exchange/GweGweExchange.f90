!> @brief This module contains the GweGweExchangeModule Module
!!
!! This module contains the code for connecting two GWE Models.
!! The methods are based on the simple two point flux approximation
!! with the option to use ghost nodes to improve accuracy.  This
!! exchange is used by GweGweConnection with the more sophisticated
!! interface model coupling approach when XT3D is needed.
!!
!<
module GweGweExchangeModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use BaseModelModule, only: BaseModelType, GetBaseModelFromList
  use BaseExchangeModule, only: BaseExchangeType, AddBaseExchangeToList
  use ConstantsModule, only: LENBOUNDNAME, NAMEDBOUNDFLAG, LINELENGTH, &
                             TABCENTER, TABLEFT, LENAUXNAME, DNODATA, &
                             LENMODELNAME
  use ListModule, only: ListType
  use ListsModule, only: basemodellist
  use DisConnExchangeModule, only: DisConnExchangeType
  use GweModule, only: GweModelType
  use TspMvtModule, only: TspMvtType
  use ObserveModule, only: ObserveType
  use ObsModule, only: ObsType
  use SimModule, only: count_errors, store_error, &
                       store_error_unit, ustop
  use SimVariablesModule, only: errmsg
  use BlockParserModule, only: BlockParserType
  use TableModule, only: TableType, table_cr

  implicit none

  private
  public :: GweExchangeType
  public :: gweexchange_create
  public :: GetGweExchangeFromList
  public :: CastAsGweExchange

  !> @brief Derived type for GwtExchangeType
  !!
  !! This derived type contains information and methods for
  !! connecting two GWT models.
  !!
  !<
  type, extends(DisConnExchangeType) :: GweExchangeType
    !
    ! -- names of the GWF models that are connected by this exchange
    character(len=LENMODELNAME) :: gwfmodelname1 = '' !< name of gwfmodel that corresponds to gwtmodel1
    character(len=LENMODELNAME) :: gwfmodelname2 = '' !< name of gwfmodel that corresponds to gwtmodel2
    !
    ! -- pointers to gwt models
    type(GweModelType), pointer :: gwemodel1 => null() !< pointer to GWT Model 1
    type(GweModelType), pointer :: gwemodel2 => null() !< pointer to GWT Model 2
    !
    ! -- GWT specific option block:
    integer(I4B), pointer :: inewton => null() !< unneeded newton flag allows for mvt to be used here
    integer(I4B), pointer :: iprflow => null() !< print flag for cell by cell flows
    integer(I4B), pointer :: ipakcb => null() !< save flag for cell by cell flows
    integer(I4B), pointer :: iAdvScheme !< the advection scheme at the interface:
                                        !! 0 = upstream, 1 = central, 2 = TVD
    !
    ! -- Mover transport package
    integer(I4B), pointer :: inmvt => null() !< unit number for mover transport (0 if off)
    type(TspMvtType), pointer :: mvt => null() !< water mover object
    !
    ! -- Observation package
    integer(I4B), pointer :: inobs => null() !< unit number for GWT-GWT observations
    type(ObsType), pointer :: obs => null() !< observation object
    !
    ! -- internal data
    real(DP), dimension(:), pointer, contiguous :: cond => null() !< conductance
    real(DP), dimension(:), pointer, contiguous :: simvals => null() !< simulated flow rate for each exchange
    !
    ! -- table objects
    type(TableType), pointer :: outputtab1 => null()
    type(TableType), pointer :: outputtab2 => null()

  contains

    procedure :: exg_df => gwe_gwe_df
    procedure :: exg_ar => gwe_gwe_ar
    procedure :: exg_rp => gwe_gwe_rp
    procedure :: exg_ad => gwe_gwe_ad
    procedure :: exg_fc => gwe_gwe_fc
    procedure :: exg_bd => gwe_gwe_bd
    procedure :: exg_ot => gwe_gwe_ot
    procedure :: exg_da => gwe_gwe_da
    procedure :: exg_fp => gwe_gwe_fp
    procedure :: connects_model => gwe_gwe_connects_model
    procedure :: use_interface_model
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: read_options
    procedure :: parse_option
    procedure :: read_mvt
    procedure :: gwe_gwe_bdsav
    procedure, private :: gwe_gwe_df_obs
    procedure, private :: gwe_gwe_rp_obs
    procedure, public :: gwe_gwe_save_simvals
    procedure, private :: validate_exchange
  end type GweExchangeType

contains

  !> @ brief Create GWT GWT exchange
  !!
  !! Create a new GWT to GWT exchange object.
  !!
  !<
  subroutine gweexchange_create(filename, id, m1id, m2id)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use BaseModelModule, only: BaseModelType
    use ListsModule, only: baseexchangelist
    use ObsModule, only: obs_cr
    use MemoryHelperModule, only: create_mem_path
    ! -- dummy
    character(len=*), intent(in) :: filename !< filename for reading
    integer(I4B), intent(in) :: id !< id for the exchange
    integer(I4B), intent(in) :: m1id !< id for model 1
    integer(I4B), intent(in) :: m2id !< id for model 2
    ! -- local
    type(GweExchangeType), pointer :: exchange
    class(BaseModelType), pointer :: mb
    class(BaseExchangeType), pointer :: baseexchange
    character(len=20) :: cint
    !
    ! -- Create a new exchange and add it to the baseexchangelist container
    allocate (exchange)
    baseexchange => exchange
    call AddBaseExchangeToList(baseexchangelist, baseexchange)
    !
    ! -- Assign id and name
    exchange%id = id
    write (cint, '(i0)') id
    exchange%name = 'GWE-GWE_'//trim(adjustl(cint))
    exchange%memoryPath = create_mem_path(exchange%name)
    !
    ! -- allocate scalars and set defaults
    call exchange%allocate_scalars()
    exchange%filename = filename
    exchange%typename = 'GWE-GWE'
    exchange%iAdvScheme = 0
    exchange%ixt3d = 1
    !
    ! -- set gwtmodel1
    mb => GetBaseModelFromList(basemodellist, m1id)
    select type (mb)
    type is (GweModelType)
      exchange%model1 => mb
      exchange%gwemodel1 => mb
    end select
    !
    ! -- set gwtmodel2
    mb => GetBaseModelFromList(basemodellist, m2id)
    select type (mb)
    type is (GweModelType)
      exchange%model2 => mb
      exchange%gwemodel2 => mb
    end select
    !
    ! -- Verify that gwt model1 is of the correct type
    if (.not. associated(exchange%gwemodel1)) then
      write (errmsg, '(3a)') 'Problem with GWE-GWE exchange ', &
        trim(exchange%name), &
        '.  First specified GWE Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Verify that gwf model2 is of the correct type
    if (.not. associated(exchange%gwemodel2)) then
      write (errmsg, '(3a)') 'Problem with GWE-GWE exchange ', &
        trim(exchange%name), &
        '.  Second specified GWE Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Create the obs package
    call obs_cr(exchange%obs, exchange%inobs)
    !
    ! -- return
    return
  end subroutine gweexchange_create

  !> @ brief Define GWE GWE exchange
  !!
  !! Define GWE to GWE exchange object.
  !!
  !<
  subroutine gwe_gwe_df(this)
    ! -- modules
    use SimVariablesModule, only: iout
    use InputOutputModule, only: getunit, openfile
    use GhostNodeModule, only: gnc_cr
    ! -- dummy
    class(GweExchangeType) :: this !<  GwtExchangeType
    ! -- local
    integer(I4B) :: inunit
    !
    ! -- open the file
    inunit = getunit()
    write (iout, '(/a,a)') ' Creating exchange: ', this%name
    call openfile(inunit, iout, this%filename, 'GWE-GWE')
    !
    call this%parser%Initialize(inunit, iout)
    !
    ! -- Ensure models are in same solution
    if (this%gwemodel1%idsoln /= this%gwemodel2%idsoln) then
      call store_error('ERROR.  TWO MODELS ARE CONNECTED '// &
                       'IN A GWE EXCHANGE BUT THEY ARE IN DIFFERENT '// &
                       'SOLUTIONS. GWE MODELS MUST BE IN SAME SOLUTION: '// &
                       trim(this%gwemodel1%name)//' '//trim(this%gwemodel2%name))
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- read options
    call this%read_options(iout)
    !
    ! -- read dimensions
    call this%read_dimensions(iout)
    !
    ! -- allocate arrays
    call this%allocate_arrays()
    !
    ! -- read exchange data
    call this%read_data(iout)
    !
    ! -- Read mover information
    if (this%inmvt > 0) then
      call this%read_mvt(iout)
      call this%mvt%mvt_df(this%gwemodel1%dis)
    end if
    !
    ! -- close the file
    close (inunit)
    !
    ! -- Store obs
    call this%gwe_gwe_df_obs()
    call this%obs%obs_df(iout, this%name, 'GWE-GWE', this%gwemodel1%dis)
    !
    ! -- validate
    call this%validate_exchange()
    !
    ! -- return
    return
  end subroutine gwe_gwe_df

  !> @brief validate exchange data after reading
  !<
  subroutine validate_exchange(this)
    class(GweExchangeType) :: this !<  GweExchangeType
    ! local

    ! Ensure gwfmodel names were entered
    if (this%gwfmodelname1 == '') then
      write (errmsg, '(3a)') 'GWE-GWE exchange ', trim(this%name), &
                            ' requires that GWFMODELNAME1 be entered in the &
                            &OPTIONS block.'
      call store_error(errmsg)
    end if
    if (this%gwfmodelname2 == '') then
      write (errmsg, '(3a)') 'GWE-GWE exchange ', trim(this%name), &
                            ' requires that GWFMODELNAME2 be entered in the &
                            &OPTIONS block.'
      call store_error(errmsg)
    end if

    ! Periodic boundary condition in exchange don't allow XT3D (=interface model)
    if (associated(this%model1, this%model2)) then
      if (this%ixt3d > 0) then
        write (errmsg, '(3a)') 'GWE-GWE exchange ', trim(this%name), &
          ' is a periodic boundary condition which cannot'// &
          ' be configured with XT3D'
        call store_error(errmsg)
      end if
    end if

    ! Check to see if dispersion is on in either model1 or model2.
    ! If so, then ANGLDEGX must be provided as an auxiliary variable for this
    ! GWE-GWE exchange (this%ianglex > 0).
    if (this%gwemodel1%indsp /= 0 .or. this%gwemodel2%indsp /= 0) then
      if (this%ianglex == 0) then
        write (errmsg, '(3a)') 'GWE-GWE exchange ', trim(this%name), &
          ' requires that ANGLDEGX be specified as an'// &
          ' auxiliary variable because dispersion was '// &
          'specified in one or both transport models.'
        call store_error(errmsg)
      end if
    end if

    if (this%ixt3d > 0 .and. this%ianglex == 0) then
      write (errmsg, '(3a)') 'GWE-GWE exchange ', trim(this%name), &
        ' requires that ANGLDEGX be specified as an'// &
        ' auxiliary variable because XT3D is enabled'
      call store_error(errmsg)
    end if

    if (count_errors() > 0) then
      call ustop()
    end if

  end subroutine validate_exchange

  !> @ brief Allocate and read
  !!
  !! Allocated and read and calculate saturated conductance
  !!
  !<
  subroutine gwe_gwe_ar(this)
    ! -- modules
    ! -- dummy
    class(GweExchangeType) :: this !<  GwtExchangeType
    ! -- local
    !
    ! -- If mover is active, then call ar routine
    if (this%inmvt > 0) call this%mvt%mvt_ar()
    !
    ! -- Observation AR
    call this%obs%obs_ar()
    !
    ! -- Return
    return
  end subroutine gwe_gwe_ar

  !> @ brief Read and prepare
  !!
  !! Read new data for mover and obs
  !!
  !<
  subroutine gwe_gwe_rp(this)
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(GweExchangeType) :: this !<  GweExchangeType
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- Read and prepare for mover
    if (this%inmvt > 0) call this%mvt%mvt_rp()
    !
    ! -- Read and prepare for observations
    call this%gwe_gwe_rp_obs()
    !
    ! -- Return
    return
  end subroutine gwe_gwe_rp

  !> @ brief Advance
  !!
  !! Advance mover and obs
  !!
  !<
  subroutine gwe_gwe_ad(this)
    ! -- modules
    ! -- dummy
    class(GweExchangeType) :: this !<  GweExchangeType
    ! -- local
    !
    ! -- Advance mover
    !if(this%inmvt > 0) call this%mvt%mvt_ad()
    !
    ! -- Push simulated values to preceding time step
    call this%obs%obs_ad()
    !
    ! -- Return
    return
  end subroutine gwe_gwe_ad

  !> @ brief Fill coefficients
  !!
  !! Calculate conductance and fill coefficient matrix
  !!
  !<
  subroutine gwe_gwe_fc(this, kiter, iasln, amatsln, rhssln, inwtflag)
    ! -- modules
    ! -- dummy
    class(GweExchangeType) :: this !<  GwtExchangeType
    integer(I4B), intent(in) :: kiter
    integer(I4B), dimension(:), intent(in) :: iasln
    real(DP), dimension(:), intent(inout) :: amatsln
    real(DP), dimension(:), intent(inout) :: rhssln
    integer(I4B), optional, intent(in) :: inwtflag
    ! -- local
    !
    ! -- Call mvt fc routine
    if (this%inmvt > 0) call this%mvt%mvt_fc(this%gwemodel1%x, this%gwemodel2%x)
    !
    ! -- Return
    return
  end subroutine gwe_gwe_fc

  !> @ brief Budget
  !!
  !! Accumulate budget terms
  !!
  !<
  subroutine gwe_gwe_bd(this, icnvg, isuppress_output, isolnid)
    ! -- modules
    use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
    use BudgetModule, only: rate_accumulator
    ! -- dummy
    class(GweExchangeType) :: this !<  GweExchangeType
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    ! -- local
    character(len=LENBUDTXT), dimension(1) :: budtxt
    real(DP), dimension(2, 1) :: budterm
    real(DP) :: ratin, ratout
    ! -- formats
    !
    ! -- initialize
    budtxt(1) = '    FLOW-JA-FACE'
    !
    ! -- Calculate ratin/ratout and pass to model budgets
    call rate_accumulator(this%simvals, ratin, ratout)
    !
    ! -- Add the budget terms to model 1
    budterm(1, 1) = ratin
    budterm(2, 1) = ratout
    call this%gwemodel1%model_bdentry(budterm, budtxt, this%name)
    !
    ! -- Add the budget terms to model 2
    budterm(1, 1) = ratout
    budterm(2, 1) = ratin
    call this%gwemodel2%model_bdentry(budterm, budtxt, this%name)
    !
    ! -- Call mvt bd routine
    if (this%inmvt > 0) call this%mvt%mvt_bd(this%gwemodel1%x, this%gwemodel2%x)
    !
    ! -- return
    return
  end subroutine gwe_gwe_bd

  !> @ brief Budget save
  !!
  !! Output individual flows to listing file and binary budget files
  !!
  !<
  subroutine gwe_gwe_bdsav(this)
    ! -- modules
    use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GweExchangeType) :: this !<  GwtExchangeType
    ! -- local
    character(len=LENBOUNDNAME) :: bname
    character(len=LENPACKAGENAME + 4) :: packname1
    character(len=LENPACKAGENAME + 4) :: packname2
    character(len=LENBUDTXT), dimension(1) :: budtxt
    character(len=20) :: nodestr
    integer(I4B) :: ntabrows
    integer(I4B) :: nodeu
    integer(I4B) :: i, n1, n2, n1u, n2u
    integer(I4B) :: ibinun1, ibinun2
    integer(I4B) :: icbcfl, ibudfl
    real(DP) :: ratin, ratout, rrate
    integer(I4B) :: isuppress_output
    ! -- formats
    !
    ! -- initialize local variables
    isuppress_output = 0
    budtxt(1) = '    FLOW-JA-FACE'
    packname1 = 'EXG '//this%name
    packname1 = adjustr(packname1)
    packname2 = 'EXG '//this%name
    packname2 = adjustr(packname2)
    !
    ! -- update output tables
    if (this%iprflow /= 0) then
      !
      ! -- update titles
      if (this%gwemodel1%oc%oc_save('BUDGET')) then
        call this%outputtab1%set_title(packname1)
      end if
      if (this%gwemodel2%oc%oc_save('BUDGET')) then
        call this%outputtab2%set_title(packname2)
      end if
      !
      ! -- set table kstp and kper
      call this%outputtab1%set_kstpkper(kstp, kper)
      call this%outputtab2%set_kstpkper(kstp, kper)
      !
      ! -- update maxbound of tables
      ntabrows = 0
      do i = 1, this%nexg
        n1 = this%nodem1(i)
        n2 = this%nodem2(i)
        !
        ! -- If both cells are active then calculate flow rate
        if (this%gwemodel1%ibound(n1) /= 0 .and. &
            this%gwemodel2%ibound(n2) /= 0) then
          ntabrows = ntabrows + 1
        end if
      end do
      if (ntabrows > 0) then
        call this%outputtab1%set_maxbound(ntabrows)
        call this%outputtab2%set_maxbound(ntabrows)
      end if
    end if
    !
    ! -- Print and write budget terms for model 1
    !
    ! -- Set binary unit numbers for saving flows
    if (this%ipakcb /= 0) then
      ibinun1 = this%gwemodel1%oc%oc_save_unit('BUDGET')
    else
      ibinun1 = 0
    end if
    !
    ! -- If save budget flag is zero for this stress period, then
    !    shut off saving
    if (.not. this%gwemodel1%oc%oc_save('BUDGET')) ibinun1 = 0
    if (isuppress_output /= 0) then
      ibinun1 = 0
    end if
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if (ibinun1 /= 0) then
      call this%gwemodel1%dis%record_srcdst_list_header( &
        budtxt(1), this%gwemodel1%name, this%name, &
        this%gwemodel2%name, this%name, this%naux, this%auxname, &
        ibinun1, this%nexg, this%gwemodel1%iout)
    end if
    !
    ! Initialize accumulators
    ratin = DZERO
    ratout = DZERO
    !
    ! -- Loop through all exchanges
    do i = 1, this%nexg
      !
      ! -- Assign boundary name
      if (this%inamedbound > 0) then
        bname = this%boundname(i)
      else
        bname = ''
      end if
      !
      ! -- Calculate the flow rate between n1 and n2
      rrate = DZERO
      n1 = this%nodem1(i)
      n2 = this%nodem2(i)
      !
      ! -- If both cells are active then calculate flow rate
      if (this%gwemodel1%ibound(n1) /= 0 .and. &
          this%gwemodel2%ibound(n2) /= 0) then
        rrate = this%simvals(i)
        !
        ! -- Print the individual rates to model list files if requested
        if (this%iprflow /= 0) then
          if (this%gwemodel1%oc%oc_save('BUDGET')) then
            !
            ! -- set nodestr and write outputtab table
            nodeu = this%gwemodel1%dis%get_nodeuser(n1)
            call this%gwemodel1%dis%nodeu_to_string(nodeu, nodestr)
            call this%outputtab1%print_list_entry(i, trim(adjustl(nodestr)), &
                                                  rrate, bname)
          end if
        end if
        if (rrate < DZERO) then
          ratout = ratout - rrate
        else
          ratin = ratin + rrate
        end if
      end if
      !
      ! -- If saving cell-by-cell flows in list, write flow
      n1u = this%gwemodel1%dis%get_nodeuser(n1)
      n2u = this%gwemodel2%dis%get_nodeuser(n2)
      if (ibinun1 /= 0) &
        call this%gwemodel1%dis%record_mf6_list_entry( &
        ibinun1, n1u, n2u, rrate, this%naux, this%auxvar(:, i), &
        .false., .false.)
      !
    end do
    !
    ! -- Print and write budget terms for model 2
    !
    ! -- Set binary unit numbers for saving flows
    if (this%ipakcb /= 0) then
      ibinun2 = this%gwemodel2%oc%oc_save_unit('BUDGET')
    else
      ibinun2 = 0
    end if
    !
    ! -- If save budget flag is zero for this stress period, then
    !    shut off saving
    if (.not. this%gwemodel2%oc%oc_save('BUDGET')) ibinun2 = 0
    if (isuppress_output /= 0) then
      ibinun2 = 0
    end if
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if (ibinun2 /= 0) then
      call this%gwemodel2%dis%record_srcdst_list_header( &
        budtxt(1), this%gwemodel2%name, this%name, this%gwemodel1%name, &
        this%name, this%naux, this%auxname, ibinun2, this%nexg, &
        this%gwemodel2%iout)
    end if
    !
    ! Initialize accumulators
    ratin = DZERO
    ratout = DZERO
    !
    ! -- Loop through all exchanges
    do i = 1, this%nexg
      !
      ! -- Assign boundary name
      if (this%inamedbound > 0) then
        bname = this%boundname(i)
      else
        bname = ''
      end if
      !
      ! -- Calculate the flow rate between n1 and n2
      rrate = DZERO
      n1 = this%nodem1(i)
      n2 = this%nodem2(i)
      !
      ! -- If both cells are active then calculate flow rate
      if (this%gwemodel1%ibound(n1) /= 0 .and. &
          this%gwemodel2%ibound(n2) /= 0) then
        rrate = this%simvals(i)
        !
        ! -- Print the individual rates to model list files if requested
        if (this%iprflow /= 0) then
          if (this%gwemodel2%oc%oc_save('BUDGET')) then
            !
            ! -- set nodestr and write outputtab table
            nodeu = this%gwemodel2%dis%get_nodeuser(n2)
            call this%gwemodel2%dis%nodeu_to_string(nodeu, nodestr)
            call this%outputtab2%print_list_entry(i, trim(adjustl(nodestr)), &
                                                  -rrate, bname)
          end if
        end if
        if (rrate < DZERO) then
          ratout = ratout - rrate
        else
          ratin = ratin + rrate
        end if
      end if
      !
      ! -- If saving cell-by-cell flows in list, write flow
      n1u = this%gwemodel1%dis%get_nodeuser(n1)
      n2u = this%gwemodel2%dis%get_nodeuser(n2)
      if (ibinun2 /= 0) &
        call this%gwemodel2%dis%record_mf6_list_entry( &
        ibinun2, n2u, n1u, -rrate, this%naux, this%auxvar(:, i), &
        .false., .false.)
      !
    end do
    !
    ! -- Set icbcfl, ibudfl to zero so that flows will be printed and
    !    saved, if the options were set in the MVT package
    icbcfl = 1
    ibudfl = 1
    !
    ! -- Call mvt bd routine
    !cdl todo: if(this%inmvt > 0) call this%mvt%mvt_bdsav(icbcfl, ibudfl, isuppress_output)
    !
    ! -- Calculate and write simulated values for observations
    if (this%inobs /= 0) then
      call this%gwe_gwe_save_simvals()
    end if
    !
    ! -- return
    return
  end subroutine gwe_gwe_bdsav

  !> @ brief Output
  !!
  !! Write output
  !!
  !<
  subroutine gwe_gwe_ot(this)
    ! -- modules
    use SimVariablesModule, only: iout
    use ConstantsModule, only: DZERO, LINELENGTH
    ! -- dummy
    class(GweExchangeType) :: this !<  GweExchangeType
    ! -- local
    integer(I4B) :: iexg, n1, n2
    integer(I4B) :: ibudfl
    real(DP) :: flow
    character(len=LINELENGTH) :: node1str, node2str
    ! -- format
    character(len=*), parameter :: fmtheader = &
     "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /,  &
       &2a16, 5a16, /, 112('-'))"
    character(len=*), parameter :: fmtheader2 = &
     "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /,  &
       &2a16, 4a16, /, 96('-'))"
    character(len=*), parameter :: fmtdata = &
                                   "(2a16, 5(1pg16.6))"
    !
    ! -- Call bdsave
    call this%gwe_gwe_bdsav()
    !
    ! -- Write a table of exchanges
    if (this%iprflow /= 0) then
      write (iout, fmtheader2) trim(adjustl(this%name)), this%id, 'NODEM1', &
        'NODEM2', 'COND', 'X_M1', 'X_M2', 'FLOW'
      do iexg = 1, this%nexg
        n1 = this%nodem1(iexg)
        n2 = this%nodem2(iexg)
        flow = this%simvals(iexg)
        call this%gwemodel1%dis%noder_to_string(n1, node1str)
        call this%gwemodel2%dis%noder_to_string(n2, node2str)
        write (iout, fmtdata) trim(adjustl(node1str)), &
          trim(adjustl(node2str)), &
          this%cond(iexg), this%gwemodel1%x(n1), &
          this%gwemodel2%x(n2), flow
      end do
    end if
    !
    !cdl Implement when MVT is ready
    ! -- Mover budget output
    ibudfl = 1
    if (this%inmvt > 0) call this%mvt%mvt_ot_bdsummary(ibudfl)
    !
    ! -- OBS output
    call this%obs%obs_ot()
    !
    ! -- return
    return
  end subroutine gwe_gwe_ot

  !> @ brief Read options
  !!
  !! Read the options block
  !!
  !<
  subroutine read_options(this, iout)
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENAUXNAME, DEM6
    use MemoryManagerModule, only: mem_allocate
    use SimModule, only: store_error, store_error_unit
    ! -- dummy
    class(GweExchangeType) :: this !<  GweExchangeType
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH) :: keyword
    logical :: isfound
    logical :: endOfBlock
    integer(I4B) :: ierr
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (iout, '(1x,a)') 'PROCESSING GWE-GWE EXCHANGE OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) then
          exit
        end if
        call this%parser%GetStringCaps(keyword)

        ! first parse option in base
        if (this%DisConnExchangeType%parse_option(keyword, iout)) then
          cycle
        end if

        ! it's probably ours
        if (this%parse_option(keyword, iout)) then
          cycle
        end if

        ! unknown option
        errmsg = "Unknown GWE-GWE exchange option '"//trim(keyword)//"'."
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end do

      write (iout, '(1x,a)') 'END OF GWE-GWE EXCHANGE OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine read_options

  !> @brief parse option from exchange file
  !<
  function parse_option(this, keyword, iout) result(parsed)
    use InputOutputModule, only: getunit, openfile
    class(GweExchangeType) :: this !< GweExchangeType
    character(len=LINELENGTH), intent(in) :: keyword !< the option name
    integer(I4B), intent(in) :: iout !< for logging
    logical(LGP) :: parsed !< true when parsed
    ! local
    character(len=LINELENGTH) :: fname
    integer(I4B) :: inobs, ilen
    character(len=LINELENGTH) :: subkey

    parsed = .true.

    select case (keyword)
    case ('GWFMODELNAME1')
      call this%parser%GetStringCaps(subkey)
      ilen = len_trim(subkey)
      if (ilen > LENMODELNAME) then
        write (errmsg, '(4x,a,a)') &
          'INVALID MODEL NAME: ', trim(subkey)
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end if
      if (this%gwfmodelname1 /= '') then
        call store_error('GWFMODELNAME1 has already been set to ' &
                         //trim(this%gwfmodelname1)// &
                         '. Cannot set more than once.')
        call this%parser%StoreErrorUnit()
      end if
      this%gwfmodelname1 = subkey(1:LENMODELNAME)
      write (iout, '(4x,a,a)') &
        'GWFMODELNAME1 IS SET TO: ', trim(this%gwfmodelname1)
    case ('GWFMODELNAME2')
      call this%parser%GetStringCaps(subkey)
      ilen = len_trim(subkey)
      if (ilen > LENMODELNAME) then
        write (errmsg, '(4x,a,a)') &
          'INVALID MODEL NAME: ', trim(subkey)
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end if
      if (this%gwfmodelname2 /= '') then
        call store_error('GWFMODELNAME2 has already been set to ' &
                         //trim(this%gwfmodelname2)// &
                         '. Cannot set more than once.')
        call this%parser%StoreErrorUnit()
      end if
      this%gwfmodelname2 = subkey(1:LENMODELNAME)
      write (iout, '(4x,a,a)') &
        'GWFMODELNAME2 IS SET TO: ', trim(this%gwfmodelname2)
    case ('PRINT_FLOWS')
      this%iprflow = 1
      write (iout, '(4x,a)') &
        'EXCHANGE FLOWS WILL BE PRINTED TO LIST FILES.'
    case ('SAVE_FLOWS')
      this%ipakcb = -1
      write (iout, '(4x,a)') &
        'EXCHANGE FLOWS WILL BE SAVED TO BINARY BUDGET FILES.'
    case ('MVT6')
      call this%parser%GetStringCaps(subkey)
      if (subkey /= 'FILEIN') then
        call store_error('MVT6 KEYWORD MUST BE FOLLOWED BY '// &
                         '"FILEIN" then by filename.')
        call this%parser%StoreErrorUnit()
      end if
      call this%parser%GetString(fname)
      if (fname == '') then
        call store_error('NO MVT6 FILE SPECIFIED.')
        call this%parser%StoreErrorUnit()
      end if
      this%inmvt = getunit()
      call openfile(this%inmvt, iout, fname, 'MVT')
      write (iout, '(4x,a)') &
        'WATER MOVER TRANSPORT INFORMATION WILL BE READ FROM ', trim(fname)
    case ('OBS6')
      call this%parser%GetStringCaps(subkey)
      if (subkey /= 'FILEIN') then
        call store_error('OBS8 KEYWORD MUST BE FOLLOWED BY '// &
                         '"FILEIN" then by filename.')
        call this%parser%StoreErrorUnit()
      end if
      this%obs%active = .true.
      call this%parser%GetString(this%obs%inputFilename)
      inobs = GetUnit()
      call openfile(inobs, iout, this%obs%inputFilename, 'OBS')
      this%obs%inUnitObs = inobs
    case ('ADVSCHEME')
      !cdl todo: change to ADV_SCHEME?
      call this%parser%GetStringCaps(subkey)
      select case (subkey)
      case ('UPSTREAM')
        this%iAdvScheme = 0
      case ('CENTRAL')
        this%iAdvScheme = 1
      case ('TVD')
        this%iAdvScheme = 2
      case default
        errmsg = "Unknown weighting method for advection: '"//trim(subkey)//"'."
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end select
      write (iout, '(4x,a,a)') &
        'CELL AVERAGING METHOD HAS BEEN SET TO: ', trim(subkey)
    case ('XT3D_OFF')
      !cdl todo: change to DSP_XT3D_OFF?
      this%ixt3d = 0
      write (iout, '(4x,a)') 'XT3D FORMULATION HAS BEEN SHUT OFF.'
    case ('XT3D_RHS')
      !cdl todo: change to DSP_XT3D_RHS?
      this%ixt3d = 2
      write (iout, '(4x,a)') 'XT3D RIGHT-HAND SIDE FORMULATION IS SELECTED.'
    case default
      parsed = .false.
    end select

  end function parse_option

  !> @ brief Read mover
  !!
  !! Read and process movers
  !!
  !<
  subroutine read_mvt(this, iout)
    ! -- modules
    use TspMvtModule, only: mvt_cr
    ! -- dummy
    class(GweExchangeType) :: this !<  GwtExchangeType
    integer(I4B), intent(in) :: iout
    ! -- local
    !
    ! -- Create and initialize the mover object  Here, fmi is set to the one
    !    for gwtmodel1 so that a call to save flows has an associated dis
    !    object.
    call mvt_cr(this%mvt, this%name, this%inmvt, iout, this%gwemodel1%fmi, &
                gwfmodelname1=this%gwfmodelname1, &
                gwfmodelname2=this%gwfmodelname2, &
                fmi2=this%gwemodel2%fmi)
    !
    ! -- Return
    return
  end subroutine read_mvt

  !> @ brief Allocate scalars
  !!
  !! Allocate scalar variables
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GweExchangeType) :: this !<  GwtExchangeType
    ! -- local
    !
    call this%DisConnExchangeType%allocate_scalars()
    !
    call mem_allocate(this%inewton, 'INEWTON', this%memoryPath)
    call mem_allocate(this%iprflow, 'IPRFLOW', this%memoryPath)
    call mem_allocate(this%ipakcb, 'IPAKCB', this%memoryPath)
    call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
    call mem_allocate(this%iAdvScheme, 'IADVSCHEME', this%memoryPath)
    this%inewton = 0
    this%iprpak = 0
    this%iprflow = 0
    this%ipakcb = 0
    this%inobs = 0
    this%iAdvScheme = 0
    !
    call mem_allocate(this%inmvt, 'INMVT', this%memoryPath)
    this%inmvt = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

  !> @ brief Deallocate
  !!
  !! Deallocate memory associated with this object
  !!
  !<
  subroutine gwe_gwe_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GweExchangeType) :: this !<  GwtExchangeType
    ! -- local
    !
    ! -- objects
    if (this%inmvt > 0) then
      call this%mvt%mvt_da()
      deallocate (this%mvt)
    end if
    call this%obs%obs_da()
    deallocate (this%obs)
    !
    ! -- arrays
    call mem_deallocate(this%cond)
    call mem_deallocate(this%simvals)
    !
    ! -- output table objects
    if (associated(this%outputtab1)) then
      call this%outputtab1%table_da()
      deallocate (this%outputtab1)
      nullify (this%outputtab1)
    end if
    if (associated(this%outputtab2)) then
      call this%outputtab2%table_da()
      deallocate (this%outputtab2)
      nullify (this%outputtab2)
    end if
    !
    ! -- scalars
    deallocate (this%filename)
    call mem_deallocate(this%inewton)
    call mem_deallocate(this%iprflow)
    call mem_deallocate(this%ipakcb)
    call mem_deallocate(this%inobs)
    call mem_deallocate(this%iAdvScheme)
    call mem_deallocate(this%inmvt)
    !
    ! -- deallocate base
    call this%DisConnExchangeType%disconnex_da()
    !
    ! -- return
    return
  end subroutine gwe_gwe_da

  !> @ brief Allocate arrays
  !!
  !! Allocate arrays
  !!
  !<
  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GweExchangeType) :: this !<  GweExchangeType
    ! -- local
    character(len=LINELENGTH) :: text
    integer(I4B) :: ntabcol, i
    !
    call this%DisConnExchangeType%allocate_arrays()
    !
    call mem_allocate(this%cond, this%nexg, 'COND', this%memoryPath)
    call mem_allocate(this%simvals, this%nexg, 'SIMVALS', this%memoryPath)
    !
    ! -- Initialize
    do i = 1, this%nexg
      this%cond(i) = DNODATA
    end do
    !
    ! -- allocate and initialize the output table
    if (this%iprflow /= 0) then
      !
      ! -- dimension table
      ntabcol = 3
      if (this%inamedbound > 0) then
        ntabcol = ntabcol + 1
      end if
      !
      ! -- initialize the output table objects
      !    outouttab1
      call table_cr(this%outputtab1, this%name, '    ')
      call this%outputtab1%table_df(this%nexg, ntabcol, this%gwemodel1%iout, &
                                    transient=.TRUE.)
      text = 'NUMBER'
      call this%outputtab1%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%outputtab1%initialize_column(text, 20, alignment=TABLEFT)
      text = 'RATE'
      call this%outputtab1%initialize_column(text, 15, alignment=TABCENTER)
      if (this%inamedbound > 0) then
        text = 'NAME'
        call this%outputtab1%initialize_column(text, 20, alignment=TABLEFT)
      end if
      !    outouttab2
      call table_cr(this%outputtab2, this%name, '    ')
      call this%outputtab2%table_df(this%nexg, ntabcol, this%gwemodel2%iout, &
                                    transient=.TRUE.)
      text = 'NUMBER'
      call this%outputtab2%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%outputtab2%initialize_column(text, 20, alignment=TABLEFT)
      text = 'RATE'
      call this%outputtab2%initialize_column(text, 15, alignment=TABCENTER)
      if (this%inamedbound > 0) then
        text = 'NAME'
        call this%outputtab2%initialize_column(text, 20, alignment=TABLEFT)
      end if
    end if
    !
    ! -- return
    return
  end subroutine allocate_arrays

  !> @ brief Define observations
  !!
  !! Define the observations associated with this object
  !!
  !<
  subroutine gwe_gwe_df_obs(this)
    ! -- dummy
    class(GweExchangeType) :: this !<  GweExchangeType
    ! -- local
    integer(I4B) :: indx
    !
    ! -- Store obs type and assign procedure pointer
    !    for gwt-gwt observation type.
    call this%obs%StoreObsType('flow-ja-face', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => gwe_gwe_process_obsID
    !
    ! -- return
    return
  end subroutine gwe_gwe_df_obs

  !> @ brief Read and prepare observations
  !!
  !! Handle observation exchanges exchange-boundary names.
  !!
  !<
  subroutine gwe_gwe_rp_obs(this)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GweExchangeType) :: this !<  GwtExchangeType
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    class(ObserveType), pointer :: obsrv => null()
    character(len=LENBOUNDNAME) :: bname
    logical :: jfound
    ! -- formats
10  format('Exchange "', a, '" for observation "', a, &
           '" is invalid in package "', a, '"')
20  format('Exchange id "', i0, '" for observation "', a, &
           '" is invalid in package "', a, '"')
    !
    do i = 1, this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      !
      ! -- indxbnds needs to be reset each stress period because
      !    list of boundaries can change each stress period.
      ! -- Not true for exchanges, but leave this in for now anyway.
      call obsrv%ResetObsIndex()
      obsrv%BndFound = .false.
      !
      bname = obsrv%FeatureName
      if (bname /= '') then
        ! -- Observation location(s) is(are) based on a boundary name.
        !    Iterate through all boundaries to identify and store
        !    corresponding index(indices) in bound array.
        jfound = .false.
        do j = 1, this%nexg
          if (this%boundname(j) == bname) then
            jfound = .true.
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call obsrv%AddObsIndex(j)
          end if
        end do
        if (.not. jfound) then
          write (errmsg, 10) trim(bname), trim(obsrv%ObsTypeId), trim(this%name)
          call store_error(errmsg)
        end if
      else
        ! -- Observation location is a single exchange number
        if (obsrv%intPak1 <= this%nexg .and. obsrv%intPak1 > 0) then
          jfound = .true.
          obsrv%BndFound = .true.
          obsrv%CurrentTimeStepEndValue = DZERO
          call obsrv%AddObsIndex(obsrv%intPak1)
        else
          jfound = .false.
        end if
        if (.not. jfound) then
          write (errmsg, 20) obsrv%intPak1, trim(obsrv%ObsTypeId), trim(this%name)
          call store_error(errmsg)
        end if
      end if
    end do
    !
    ! -- write summary of error messages
    if (count_errors() > 0) then
      call store_error_unit(this%inobs)
    end if
    !
    ! -- Return
    return
  end subroutine gwe_gwe_rp_obs

  !> @ brief Final processing
  !!
  !! Conduct any final processing
  !!
  !<
  subroutine gwe_gwe_fp(this)
    ! -- dummy
    class(GweExchangeType) :: this !<  GwtExchangeType
    !
    return
  end subroutine gwe_gwe_fp

  !> @brief Return true when this exchange provides matrix
  !! coefficients for solving @param model
  !<
  function gwe_gwe_connects_model(this, model) result(is_connected)
    class(GweExchangeType) :: this !<  GweExchangeType
    class(BaseModelType), pointer, intent(in) :: model !< the model to which the exchange might hold a connection
    logical(LGP) :: is_connected !< true, when connected

    is_connected = .false.
    ! only connected when model is GwtModelType of course
    select type (model)
    class is (GweModelType)
      if (associated(this%gwemodel1, model)) then
        is_connected = .true.
      else if (associated(this%gwemodel2, model)) then
        is_connected = .true.
      end if
    end select

  end function gwe_gwe_connects_model

  !> @brief Should interface model be used for this exchange
  !<
  function use_interface_model(this) result(useIM)
    class(GweExchangeType) :: this !<  GwtExchangeType
    logical(LGP) :: useIM !< true when interface model should be used

    useIM = (this%ixt3d > 0)

  end function

  !> @ brief Save simulated flow observations
  !!
  !! Save the simulated flows for each exchange
  !!
  !<
  subroutine gwe_gwe_save_simvals(this)
    ! -- dummy
    use SimModule, only: store_error, store_error_unit
    use ConstantsModule, only: DZERO
    use ObserveModule, only: ObserveType
    class(GweExchangeType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: n1
    integer(I4B) :: n2
    integer(I4B) :: iexg
    real(DP) :: v
    character(len=100) :: msg
    type(ObserveType), pointer :: obsrv => null()
    !
    ! -- Write simulated values for all gwt-gwt observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        do j = 1, obsrv%indxbnds_count
          iexg = obsrv%indxbnds(j)
          v = DZERO
          select case (obsrv%ObsTypeId)
          case ('FLOW-JA-FACE')
            n1 = this%nodem1(iexg)
            n2 = this%nodem2(iexg)
            v = this%simvals(iexg)
          case default
            msg = 'Error: Unrecognized observation type: '// &
                  trim(obsrv%ObsTypeId)
            call store_error(msg)
            call store_error_unit(this%inobs)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
    end if
    !
    return
  end subroutine gwe_gwe_save_simvals

  !> @ brief Obs ID processer
  !!
  !! Process observations for this exchange
  !!
  !<
  subroutine gwe_gwe_process_obsID(obsrv, dis, inunitobs, iout)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: urword
    use ObserveModule, only: ObserveType
    use BaseDisModule, only: DisBaseType
    ! -- dummy
    type(ObserveType), intent(inout) :: obsrv
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: inunitobs
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: n, iexg, istat
    integer(I4B) :: icol, istart, istop
    real(DP) :: r
    character(len=LINELENGTH) :: strng
    !
    strng = obsrv%IDstring
    icol = 1
    ! -- get exchange index
    call urword(strng, icol, istart, istop, 0, n, r, iout, inunitobs)
    read (strng(istart:istop), '(i10)', iostat=istat) iexg
    if (istat == 0) then
      obsrv%intPak1 = iexg
    else
      ! Integer can't be read from strng; it's presumed to be an exchange
      ! boundary name (already converted to uppercase)
      obsrv%FeatureName = trim(adjustl(strng))
      ! -- Observation may require summing rates from multiple exchange
      !    boundaries, so assign intPak1 as a value that indicates observation
      !    is for a named exchange boundary or group of exchange boundaries.
      obsrv%intPak1 = NAMEDBOUNDFLAG
    end if
    !
    return
  end subroutine gwe_gwe_process_obsID

  !> @ brief Cast polymorphic object as exchange
  !!
  !! Cast polymorphic object as exchange
  !!
  !<
  function CastAsGweExchange(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(GweExchangeType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (GweExchangeType)
      res => obj
    end select
    return
  end function CastAsGweExchange

  !> @ brief Get exchange from list
  !!
  !! Return an exchange from the list for specified index
  !!
  !<
  function GetGweExchangeFromList(list, idx) result(res)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    class(GweExchangeType), pointer :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsGweExchange(obj)
    !
    return
  end function GetGweExchangeFromList

end module GweGweExchangeModule

