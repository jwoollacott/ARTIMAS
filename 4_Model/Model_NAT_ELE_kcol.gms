$TITLE: Foresighted national model on Alternate/AEO Growth Path
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   PURPOSE:    Test baseline setting output equilibrium
*   FUNDING:    US Environmental Protection Agency,
*               Office of Air Quality Planning and Standards;
*               RTI International Inc.
*   AUTHORS:    Jared Woollacott, PhD
*               RTI International Inc.
*   INPUT:      ./FINAL/IMPLAN_AEO_***.gdx
*   OUTPUT:     ../../Baseline/Data/IMPLAN/***_OGA.gdx
*   VERSION:    November 2018
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *



*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   SET GLOBAL DEFAULTS
$SETGLOBAL  YEAR 2019
$IF NOT SET CASE    $SETGLOBAL  CASE REF2019
$IF NOT SET CTX     $SETGLOBAL  CTX
$IF NOT SET pk_phi  $SETGLOBAL  pk_phi  0
$IF NOT SET RPT     $SETGLOBAL  RPT  Y
$IF NOT SET BSCN    $SETGLOBAL  BSCN
$IF NOT SET ESCN    $SETGLOBAL  ESCN _MD1
$IF NOT SET CAPCP   $SETGLOBAL  CAPCP 0
$IF NOT SET PCP     $SETGLOBAL  PCP 0
$IF NOT SET inv_kegn $SETGLOBAL inv_kegn 1
$IF "%PCP%"==1      $SETGLOBAL  CAPCP 1 
$SETGLOBAL  TECHS N

*   LOAD ALL DATA
SETS        g, v, hd, h, e, ec, nec, ne, i
            tt      / 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065 /
            t(tt)   / 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060 /
            tp(tt)  / 2055, 2060 /
            tf(tt)  / 2060 /        ;
PARAMETERS  FD, VA, ID, Y_, ig, tr, ENDW,
            bopdef, incadj, FD0_bgp, y0_bgp,
            pcd0, pkd0, pld0, tk, tl, ty,
            tax0, tc, vinvh, EGN_shr            ;
SCALAR      GDPG                                ;

*   LOAD DATA
$GDXIN      '../1_Baseline/Set/Final/BSA_NAT%YEAR%_%CASE%%BSCN%.gdx'
$LOAD       FD VA ID Y_=Y g v hd h ne bopdef
$LOAD       ENDW incadj vinvh GDPG
$LOAD       pcd0 pkd0 pld0 tk tl tc ty
$GDXIN
PARAMETER qref_ ;
    qref_(tt)   = ((1 + GDPG)**5)**(ord(tt)-1)  ;

*   LOAD SETS
$IFTHEN.e   %techs%==Y
$CALL   'xls2gms  i=../2_Electricity/Out_x/AggModel_%CASE%_%YEAR%%ESCN%.xlsx r=Sets_%CASE%_%YEAR%!A1:A25 o=./Defines/Techs_%CASE%_%YEAR%%ESCN%.gms' ;
$CALL   'GDXXRW.exe ../2_Electricity/Out_x/AggModel_%CASE%_%YEAR%%ESCN%.xlsx o=./Data/EGN_shr_%CASE%_%YEAR%%ESCN%.gdx PAR=EGN_shr RNG=EGN_shr!B1:BA761 rdim=3 cdim=1'
$ENDIF.e

$GDXIN  './Data/EGN_shr_%CASE%_%YEAR%%ESCN%.gdx'
$LOAD   EGN_shr
$GDXIN

$INCLUDE    './Defines/Techs_%CASE%_%YEAR%%ESCN%_kcol.gms'
$IF NOT SET POL $SETGLOBAL  POL BAU
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *



*   ++++++++++++++      ALTERNATE CASES         +++++++++++++ *
*** --- HIGH RESOURCE & TECHNOLOGY ---    ***
$IF %CASE%==HIGHRT $SETGLOBAL POL HRT
*** --- HIGH MACRO GROWTH ---    ***
$IFTHEN "%POL%"=="HMG"
$SETGLOBAL POL HMG
GDPG    = GDPG * 1.2 ;
$ENDIF

$INCLUDE    './Defines/Data.gms'
$SETGLOBAL  MOD kcol
$INCLUDE    './Defines/Scenarios.gms'
DISPLAY qgrw ;
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *




*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   SPECIFY MODEL
$ONTEXT
$MODEL:DYN_AGP_kcol

$SECTORS:
    Y(r,s,t)        ! Output                        !
    Ye(r,etc,t)$y0(r,etc,t)     ! Output electricity techs      !
    inv(t)                      ! Investment good               !
    invs(s,t)$(not egn(s))      ! Investment good               !
    inve(k_egn,t)$inv_kegn(k_egn)  ! Investment good egn           !
    K(s,t)$(not egn(s))         ! Capital production            !
    Ke(k_egn,t)     ! Capital production egn                !
*    rkrs(ext)       ! Production of reserves to each period !
*   TRADE
    a(r,s,t)        ! Armington aggregation         !
    x(r,s,t)        ! Export & domestic markets     !
*   CONSUMPTION GOODS
    cons(r,h,t)     ! Consumption                   !
    gov(r,t)        ! Government good               !

$COMMODITIES:
*   INTERMEDIATE GOODS
    py(r,s,t)       ! Price of output                               !
    pye(r,etc,t)$y0(r,etc,t)    ! Price of elec tech output         !
*   CAPITAL GOODS
    pinv(t)                     ! price of aggregate investment     !
    rk(s,t)$(not egn(s))        ! Rental price of capital           !
    rke(k_egn,t)                ! Rental price of capital egn       !
    rkr0(ext)                   ! Price of resource capital         !
    pk(s,t)$(not egn(s))        ! Purchase price of capital         !
    pke(k_egn,t)                ! Purchase price of capital egn     !
    pka(s,t)$(pk_phi and not egn(s))    ! Capital adjustment cost   !
    pkae(k_egn,t)$pk_phi        ! Capital adjustment cost           !
    pkt                         ! Price of terminal capital         !
    pl(r,t)                     ! Wage price of labor               !
*   TRADE
    pa(r,s,t)       ! Price of Armington aggregate  !
    pd(r,s,t)       ! Price of domestic output      !
    pfx             ! Price of foreign exchange     !
*   CONSUMPTION GOODS
    pc(r,h,t)$c0(r,h,t) ! Px of cons                !
    pg(r,t)         ! Px of govt good (tax pmt)     !
    pghge(t)        ! Price of CO2                  !
    pcrte(plt,t)$(pcrt(plt) AND capcp) ! Price of criteria pollutants  !

$CONSUMERS:
    rh(r,h)         ! Representative household      !
    govt(t)         ! Government agent              !

$AUXILIARY:
    kterm           ! Terminal capital stock            !
    cabt(t)         ! Carbon abatement                  !
    cpabt(plt,t)$(pcrt(plt) AND pcp)   ! Criteria pollutant abatement      !
    pnum(t)         ! Numeraire price for period        !
    fxgov(t)        ! Fix public welfare                !
    nrs(ext)        ! Natural resource elastic supply   !

*-- GENERAL PRODUCTION
$PROD:y(r,s,t)$(not (egn(s) or ext(s) or trn(s)))      s:els_s(s)  mtl(s):0 mat.tl(mtl):0
+       tran(s):els_trn(s) trn.tl(tran):0 eva(s):els_eva(s)  va(eva):els_va(s) 
+       nrg(eva):els_nrg(s)  ele.tl(nrg):0 fue(nrg):els_fue(s) ef.tl(fue):0
    o:py(r,s,t)     q:(y0(r,s,t) * tfp_oth(r,s,t))    a:GOVT(t)  t:ty(r,s)
    i:pa(r,mat,t)   q:id0(r,mat,s,t)                        mat.tl:
    i:pa(r,trn,t)   q:id0(r,trn,s,t)                        trn.tl:
    i:pa(r,ec,t)    q:(id0(r,ec,s,t) - fp_hel(r,ec,s,t) )   ec.tl:
    i:pl(r,t)       q:ld0(r,s,t)                 va:  p:pld0(r,s)   a:GOVT(t)  t:tl(r)
    i:rk(s,t)       q:kd0(r,s,t)                 va:  p:pkd0(r,s)   a:GOVT(t)  t:tk(r)

*-- TRANSPORTATION
$PROD:y(r,s,t)$trn(s)   s:els_s(s)  mtl(s):0 mat.tl(mtl):0
+       tran(s):els_trn(s) trn.tl(tran):0 eva(s):els_eva(s)  va(eva):els_va(s) 
+       nrg(eva):els_nrg(s)  ele.tl(nrg):0 fue(nrg):els_fue(s) ef.tl(fue):0
    o:py(r,s,t)     q:(y0(r,s,t) * tfp_oth(r,s,t))    a:GOVT(t)  t:ty(r,s)
    i:pa(r,mat,t)   q:id0(r,mat,s,t)                        mat.tl:
    i:pa(r,trn,t)   q:id0(r,trn,s,t)                        trn.tl:
    i:pa(r,ec,t)    q:(id0(r,ec,s,t) - fp_hel(r,ec,s,t) )   ec.tl:
    i:pl(r,t)       q:ld0(r,s,t)                 va:  p:pld0(r,s)   a:GOVT(t)  t:tl(r)
    i:rk(s,t)       q:kd0(r,s,t)                 va:  p:pkd0(r,s)   a:GOVT(t)  t:tk(r)

*-- EXTRACTIVE INDUSTRIES
$PROD:y(r,ext,t)   s:0.0  y(s):els_s(ext) mtl(y):0    mat.tl(mtl):0  eva(y):0.25
+       tran(y):els_trn(ext)  trn.tl(tran):0  va(eva):1  nrg(eva):0.25
+       ele.tl(nrg):0 fue(nrg):els_fue(ext) ef.tl(fue):0 
    o:py(r,ext,t)   q:(y0(r,ext,t) * tfp_out(r,ext,t))    a:GOVT(t)  t:ty(r,ext)
    i:pa(r,mat,t)   q:(id0(r,mat,ext,t)                         / tfp_oth(r,ext,t))         mat.tl:
    i:pa(r,trn,t)   q:(id0(r,trn,ext,t)                         / tfp_oth(r,ext,t))         trn.tl:
    i:pa(r,ec,t)    q:((id0(r,ec,ext,t) - fp_hel(r,ec,ext,t))   / tfp_oth(r,ext,t))         ec.tl:
    i:pl(r,t)       q:(ld0(r,ext,t)                             / tfp_oth(r,ext,t)) va:     p:pld0(r,ext)   a:GOVT(t)  t:tl(r)
    i:rk(ext,t)     q:(kd0(r,ext,t)*(1-ext_shr(ext))            / tfp_oth(r,ext,t)) va:     p:pkd0(r,ext)   a:GOVT(t)  t:tk(r)
    i:rkr0(ext)     q:(kd0(r,ext,t)*ext_shr(ext))                                           p:pkd0(r,ext)   a:GOVT(t)  t:tk(r)

$PROD:y(r,egn,t)       s:els_grd
+       l1(s):els_l1  l2(s):els_l2  l3(s):els_l3 l4(s):els_l4
+       etc_l1.tl(l1):0  etc_l2.tl(l2):0  etc_l3.tl(l3):0 etc_l4.tl(l4):0
    o:py(r,egn,t)               q:y0(r,egn,t)     a:GOVT(t)    t:ty(r,egn)
    i:pye(r,etc,t)$y0(r,etc,t)  q:y0(r,etc,t)   etc.tl:

$PROD:ye(r,etc,t)$y0(r,etc,t)   s:0
    o:pye(r,etc,t)      q:(y0(r,etc,t) * tfp_out(r,etc,t))
    i:pa(r,g,t)         q:id0(r,g,etc,t)
    i:pcrte(pcrt,t)$capcp q:egn_emt(r,etc,pcrt,t)                         p:1e-6
    i:pl(r,t)           q:ld0(r,etc,t)                                  p:pld0(r,"EGN") a:GOVT(t)  t:tl(r)
    i:rke(k_egn,t)$etmap(etc,k_egn) q:(kd0(r,etc,t) / kci_etc(r,etc,t)) p:pkd0(r,"EGN") a:GOVT(t)  t:tk(r)

$PROD:a(r,s,t)          s:0         cap(s):els_arm(s)
    o:pa(r,s,t)         q:a0(r,s,t)
    i:pfx               q:(m0(r,s,"INT",t) * pref(t))   cap:
    i:pd(r,s,t)         q:(y0(r,s,t) - x0(r,s,"INT",t)) cap:    p:pref(t)
    i:pghge(t)$cgo(s)   q:GHGI(r,s,"DOM",t)                     p:1e-6

$PROD:x(r,s,t)      t:els_arx(s)
    o:pd(r,s,t)     q:(y0(r,s,t) - x0(r,s,"INT",t)) p:pref(t)
    o:pfx           q:(x0(r,s,"INT",t) * pref(t))
    i:py(r,s,t)     q:y0(r,s,t)

*   Capital Motion and Investment (non-egn)
$PROD:inv(t)    s:0.25
    o:pinv(t)       q:(sum((r,g), inv_g(r,g,t)))
    i:pa(r,g,t)     q:(inv_g(r,g,t))

$PROD:invs(s,t)$(not egn(s))
    o:pk(s,t+1)         q:(i1("USA") )
    o:pkt$tn(t)         q:(i1("USA") )
    o:pk(s,t)           q:(i0("USA") )
    i:pinv(t)           q:(sum(r, inv0(r)) * inv_cst(s,t))
    i:pka(s,t)$pk_phi   q:( ks0 * sum(r, kd0_shr(r,s,t))  * pka0 )

$PROD:k(s,t)$(not egn(s)) s:1
    o:pk(s,t+1)             q:( ks0 * sum(r, kd0_shr(r,s,t))  * (1 - de))
    o:pkt$tn(t)             q:( ks0 * sum(r, kd0_shr(r,s,t))  * (1 - de))
    o:pka(s,t)$pk_phi       q:( ks0 * sum(r, kd0_shr(r,s,t))  * pka0 )
    o:rk(s,t)               q:( ks0 * sum(r, kd0_shr(r,s,t))  * (ir1 + de1) )
    i:pk(s,t)               q:( ks0 * sum(r, kd0_shr(r,s,t)) )
    i:pa("USA","COM",t)$pk_phi  q:1     p:1e-6

*   Capital motion and investment (egn)
$PROD:inve(k_egn,t)$inv_kegn(k_egn)
    o:pke(k_egn,t+1)        q:(i1("USA") )
    o:pkt$tn(t)             q:(i1("USA") )
    o:pke(k_egn,t)          q:(i0("USA") )
    i:pinv(t)               q:(sum(r, inv0(r)))
    i:pkae(k_egn,t)$pk_phi  q:( ks0 * sum(r, kd0_shr_et(r,k_egn,t)) * pka0)

$PROD:ke(k_egn,t)   s:1     t:0
    o:pke(k_egn,t+1)        q:( ks0 * sum(r, kd0_shr_et(r,k_egn,t))  * (1 - de) )
    o:pkt$tn(t)             q:( ks0 * sum(r, kd0_shr_et(r,k_egn,t))  * (1 - de) )
    o:rke(k_egn,t)          q:( ks0 * sum(r, kd0_shr_et(r,k_egn,t))  * (ir1 + de1) )
    o:pkae(k_egn,t)$pk_phi  q:( ks0 * sum(r, kd0_shr_et(r,k_egn,t))  * pka0     )
    i:pke(k_egn,t)          q:( ks0 * sum(r, kd0_shr_et(r,k_egn,t)) )
    i:pa("USA","COM",t)$pk_phi    q:1     p:1e-6

$PROD:cons(r,h,t)   s:els_c(r,h,t)
+       tran(s):els_ctrn(r,h,t) trn.tl(tran):0    ce(s):els_ce(r,h,t)
+       mtl(ce):els_cmat(r,h,t) mat.tl(mtl):0   nrg(ce):els_cnrg(r,h,t)
+       ele.tl(nrg):0       fue(nrg):els_cfue(r,h,t)    ec.tl(fue):0
    o:pc(r,h,t)     q:c0(r,h,t)
    i:pa(r,s,t)     q:(cd0(r,s,h,t) - fp_hel(r,s,h,t))  p:pcd0(r,s)   a:GOVT(t)   t:tc(r)

$PROD:gov(r,t)      s:0
    o:pg(r,t)       q:g0(r,t)
    i:pa(r,s,t)     q:gd0(r,s,t)

$DEMAND:rh(r,h)     s:0.5
    d:pc(r,h,t)                 q:c0(r,h,t)     p:pref(t)
    e:pl(r,t)                   q:le0(r,h,t)
    e:pk(s,t0)$(not egn(s))     q:((k0(r,h)*(1 - xk_shr(t0))   - i0("USA")*xk_shr(t0) * kh_shr(r,h)) * kd0_shr(r,s,"2015"))
    e:pke(k_egn,t0)             q:((k0(r,h)*(1 - xk_shr(t0))   - i0("USA")*xk_shr(t0) * kh_shr(r,h)) * kd0_shr_et(r,k_egn,"2015"))

    e:pkt                   q:(-kh_shr(r,h))            r:kterm
    e:rk(s,t)$(not egn(s))  q:(sum(rr, kd0(rr,s,t)    * (xk_shr(t)-ext_shr(s)))  * ext_shk(s,t) * kh_shr(r,h))
    e:rke(k_egn,t)          q:(sum((rr,etc)$etmap(etc,k_egn), kdx0(rr,etc,t)) * kh_shr(r,h) )
    e:rkr0(ext)             q:(sum((rr,t), kd0(rr,ext,t) * ext_shr(ext) * ext_shk(ext,t)) * kh_shr(r,h))    r:nrs(ext)

    e:pfx           q:(sum(t, bopdef(r,h) * qref(t) * pref(t)))
    e:pfx           q:(sum(t, incadj(r,h) * qref(t) * pref(t)))
    e:pc(r,h,t)     q:(HH_n(r,h)/HH_n(r,"TOT"))                 r:fxgov(t)

$DEMAND:govt(t)
    d:pg(r,t)           q:g0(r,t)
    e:pfx               q:(sum( (r,h), -incadj(r,h) * qref(t) * pref(t)))
    e:pghge(t)          q:( sum((r,cgo), GHGI(r,cgo,"DOM",t)) )
    e:pghge(t)          q:(-sum((r,cgo), GHGI(r,cgo,"DOM",t)) )   r:cabt(t)
    e:pcrte(pcrt,t)$capcp     q:( sum(r, egn_emt(r,"TOT",pcrt,t)) )
    e:pcrte(pcrt,t)$pcp q:(-sum(r, egn_emt(r,"TOT",pcrt,t)) )     r:cpabt(pcrt,t)
    e:pc(r,h,t)         q:(-HH_n(r,h)/HH_n(r,"TOT"))                                    r:fxgov(t)

$CONSTRAINT:kterm
    sum(t$tn(t+1), sum( (r,h), c0(r,h,t+1) * cons(r,h,t+1)) * sum((r,g), inv_g(r,g,t)) * inv(t) 
                -  sum( (r,h), c0(r,h,t) * cons(r,h,t)) * sum((r,g), inv_g(r,g,t+1)) * inv(t+1) )  =e= 0  ;

$CONSTRAINT:cabt(t)
    pghge(t) =e= ctax(t) * pnum(t) ;
$CONSTRAINT:cpabt(plt,t)$(pcrt(plt) AND pcp)
    pcrte(plt,t) =e= cptax(plt,t)* pnum(t) ;
$CONSTRAINT:pnum(t)
    pnum(t) * sum( (r,h), cons(r,h,t) * c0(r,h,t)) =e= sum( (r,h), pc(r,h,t) * cons(r,h,t) * c0(r,h,t)) ;
$CONSTRAINT:fxgov(t)
    sum(r, g0(r,t)) =e= sum(r, pg(r,t) * gov(r,t) * g0(r,t) / pref(t) ) ;
$CONSTRAINT:nrs(ext)
    nrs(ext) =e= rkr0(ext)**els_nrs(ext) ;

$REPORT:
*   C+G
    v:hh_wel(r,h)           w:rh(r,h)
    v:hh_cons(r,s,h,t)      i:pa(r,s,t)     prod:cons(r,h,t)
    v:gv_wel(t)             w:govt(t)
    v:gv_cons(r,s,t)        i:pa(r,s,t)     prod:gov(r,t)
*   I+X
    v:inv_in(r,s,t)         i:pa(r,s,t)     prod:inv(t)
    v:invs_(s,t)$(not egn(s)) o:pk(s,t)       prod:invs(s,t)
    v:inve_(k_egn,t)          o:pke(k_egn,t)   prod:inve(k_egn,t)
    v:fex(r,s,t)            o:pfx           prod:x(r,s,t)
    v:fim(r,s,t)            i:pfx           prod:a(r,s,t)
*   K
    v:kd_n(r,s,t)           i:rk(s,t)       prod:y(r,s,t)
    v:kd_r(r,ext,t)         i:rkr0(ext)     prod:y(r,ext,t)
    v:kd_nele(r,etc,k_egn,t) i:rke(k_egn,t)  prod:ye(r,etc,t)
*   L
    v:ld_y(r,s,t)           i:pl(r,t)       prod:y(r,s,t)
    v:ld_ye(r,etc,t)        i:pl(r,t)       prod:ye(r,etc,t)
*   ID
    v:id_y(r,g,s,t)         i:pa(r,g,t)     prod:y(r,s,t)
    v:id_ye(r,g,etc,t)      i:pa(r,g,t)     prod:ye(r,etc,t)
*   Y
    v:Y_y(r,s,t)            o:py(r,s,t)     prod:y(r,s,t)
    v:Y_ye(r,etc,t)         o:pye(r,etc,t)  prod:ye(r,etc,t)

$OFFTEXT
$SYSINCLUDE MPSGESET DYN_AGP_kcol
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *



*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   SET LEVELS AND SOLVE
y.l(r,s,t)      = qgrw(t)                   ;
ye.l(r,etc,t)   = qgrw(t)                   ;
k.l(s,t)$(not egn(s))   = qref(t) * (1 - xk_shr(t)) ;
ke.l(k_egn,t)           = qref(t) * (1 - xk_shr(t)) ;
* rkrs.l(ext)             = 1 ;
nrs.l(ext)              = 1 ;

kterm.l                 = sum(t$tn(t), sum( r, sum( h, k0(r,h) ) + i0(r) )
                                * (1-de) + sum(r, qref(t)*i1(r)) )          ;
inv.l(t)                = qref(t)               ;
inve.l(k_egn,t)         = qref(t) * kd0_shr_et("USA",k_egn,t)           ;
invs.l(s,t)$(not egn(s))= qref(t) * sum(r, kd0_shr(r,s,t))              ;
cons.l(r,h,t)           = qgrw(t)               ;

gov.l(r,t)      = qgrw(t)   ;
a.l(r,s,t)      = qgrw(t)   ;
x.l(r,s,t)      = qgrw(t)   ;

*   Set price levels
py.l(r,s,t)     = pref(t)   ;
pye.l(r,etc,t)  = pref(t)   ;
pc.l(r,h,t)     = pref(t)   ;

pl.l(r,t)               = pref(t)   ;
pinv.l(t)               = pref(t)   ;
rk.l(s,t)$(not egn(s))  = pref(t)   ;
rke.l(k_egn,t)          = pref(t)   ;
rkr0.l(ext)             = 1         ;
pk.l(s,t)$(not egn(s))  = pref(t)  * (1 + ir) * (ir1 + de1) / (ir + de)     ;
pke.l(k_egn,t)          = pref(t)  * (1 + ir) * (ir1 + de1) / (ir + de)     ;
pkt.l                   = sum( tn, pref(tn))  * ( (1 + ir) * (ir1 + de1) /
                            (ir + de) ) / ( 1 + ir)                         ;
pg.l(r,t)   = pref(t)   ;
pa.l(r,s,t) = pref(t)   ;
pd.l(r,s,t) = pref(t)   ;
pfx.fx      = 1         ;
pnum.l(t)   = pref(t)   ;

*   POLICY
pghge.l(t)              =  0    ;
pghge.lo(t)             =  0    ;
pcrte.l(pcrt,t)$pcp     =  0    ;
pcrte.lo(pcrt,t)$pcp    =  0    ;
cabt.l(t)               =  0    ;
cabt.lo(t)              = -1    ;
cabt.up(t)              =  1    ;
cpabt.l(pcrt,t)$pcp     =  0    ;
cpabt.lo(pcrt,t)$pcp    = -1    ;
cpabt.up(pcrt,t)$pcp    =  1    ;

$INCLUDE DYN_AGP_kcol.gen
DYN_AGP_kcol.savepoint   =   2   ;
DYN_AGP_kcol.workspace   = 256   ;
DYN_AGP_kcol.iterlim     =   0   ;
$IFTHEN "%CTX%"==""
    SOLVE   DYN_AGP_kcol using mcp   ;
$ENDIF 
EXECUTE_LOADPOINT 'DYN_AGP_kcol_p1.gdx'  ;

$IFTHEN "%CTX%"=="_CTX"
ctax(t)$(t.val>2015)= %ctax% *1.05**(t.val-2015) ;
ctax(tp)            = ctax("2050")*(1+GDPG)**(tp.val-2050)  ;
DISPLAY ctax ;
$ENDIF
$IF NOT SET ctax $SET ctax 

DYN_AGP_kcol.iterlim     = 1e5   ;
SOLVE   DYN_AGP_kcol using mcp   ;
$INCLUDE    ./Report.gms

*   --- META: Report scenario solution behavior
PARAMETER meta ;
    meta("modelstat")   = DYN_AGP_kcol.modelstat ;
    meta("iterusd")     = DYN_AGP_kcol.iterusd ;
file lp_disp / lp_disp.txt / ;
put lp_disp ;
put '/////////////////////////////////' /
    '////                         ////' /
    '////                         ////' /
    '////                         ////' /
    '////                         ////' /
    '----    LAST SOLVE FOR Ka%pk_phi%%BSCN%_%POL%%CTX%%CTAX%'          /
    '----||  --> SOLVE STAT = ' DYN_AGP_kcol.modelstat /
    '----||  --> ITERATIONS = ' DYN_AGP_kcol.iterusd   /
    '////                         ////' /
    '////                         ////' /
    '////                         ////' /
    '////                         ////' /
    '/////////////////////////////////' /;
putclose lp_disp ;
execute 'cat lp_disp.txt' ;
execute 'rm lp_disp.txt' ;


    FILE sol_stat / FAILED_SOLVE_Ka%pk_phi%%BSCN%_%POL%%CTX%%CTAX%.txt / ;
if(meta("modelstat")>1,
    put  sol_stat ;
    put  'AEO%YEAR%_%CASE%_Ka%pk_phi%%BSCN%_%POL%%CTX%%CTAX%     ' DYN_AGP_kcol.modelstat /;
); 
if(meta("modelstat")=1,
    execute 'rm FAILED_SOLVE_Ka%pk_phi%%BSCN%_%POL%%CTX%%CTAX%.txt' ;
);

$IFTHEN "%RPT%"=="Y"
EXECUTE_UNLOAD './Out_x/Results_AEO%YEAR%_%CASE%_Ka%pk_phi%%BSCN%_%POL%%CTX%%CTAX%_kcol.gdx', FD1 VA1 VA2 Y1 Y2 ID1 Px PLCY meta wel ENV;
* EXECUTE_UNLOAD './Out_x/EMF34_MC/Results_AEO%YEAR%_%CASE%_Ka%pk_phi%%BSCN%_%POL%%CTX%%CTAX%.gdx', FD1 VA1 Y1 Y2 ID1 Px PLCY meta wel;
$ENDIF

PARAMETER oil_use ;
    oil_use(r,"TRG",t) = id0(r,"OIL","TRG",t)       ;
    oil_use(r,"TRW",t) = id0(r,"OIL","TRW",t)       ;
    oil_use(r,"TRA",t) = id0(r,"OIL","TRA",t)       ;
    oil_use(r,"RES_OIL",t) = sum(h, cd0(r,"OIL",h,t))   ;
    oil_use(r,"RES_TRG",t) = sum(h, cd0(r,"TRG",h,t))   ;
DISPLAY ke.l, pye.l, pke.l ;
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *








