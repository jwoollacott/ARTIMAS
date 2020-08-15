$TITLE: Foresighted national model of the US
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   AUTHORS:    Jared Woollacott, PhD
*               RTI International Inc.
*   INPUT:      ./FINAL/IMPLAN_AEO_***.gdx
*   OUTPUT:     ../../Baseline/Data/IMPLAN/***_OGA.gdx
*   VERSION:    November 2018
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *

PARAMETER time ;
	time("start","%system.time%") = 1 ;

*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
$ontext
--- GLOBAL DEFAULTS ---
CASE        Which AEO is being run              e.g. REF2019
CTX         Is a carbon tax being imposed       0/1
CTAX        Level of carbon tax (used w/ CTX)
pk_phi      Capital adjustment cost pct
rpt         Do you want to report results
bscn        Baseline scenario 
escn        Electricity scenario
capcp       Cap criteria pollutants
pcp         Price criteria pollutants 
inv_kegn    Separate investment dynamics for EGN
CES         Clean energy standard
BNK         Does the CES allow for banking
$offtext

$SETGLOBAL  YEAR 2019
$IF NOT SET CASE        $SETGLOBAL  CASE REF2019
$IF NOT SET CTX         $SETGLOBAL  CTX
$IF NOT SET CTAX        $SETGLOBAL  CTAX
$IF NOT SET pk_phi      $SETGLOBAL  pk_phi  0
$IF NOT SET RPT         $SETGLOBAL  RPT  Y
$IF NOT SET BSCN        $SETGLOBAL  BSCN
$IF NOT SET ESCN        $SETGLOBAL  ESCN _MD1
$IF NOT SET CAPCP       $SETGLOBAL  CAPCP 0
$IF NOT SET PCP         $SETGLOBAL  PCP 0
$IF NOT SET inv_kegn    $SETGLOBAL  inv_kegn 1
$IF "%PCP%"==1          $SETGLOBAL  CAPCP 1
$IF NOT SET CES         $SETGLOBAL  CES 1
$IF NOT SET rCES        $SETGLOBAL  rCES 0
$IF NOT SET BNK         $SETGLOBAL  BNK 0
$IF NOT SET RUN         $SETGLOBAL  RUN tmp
$IF NOT SET ACP         $SETGLOBAL  ACP 0
$IF NOT SET TECHS       $SETGLOBAL  TECHS N

*   LOAD ALL DATA
SETS        g, v, hd, h, e, ec, nec, ne, i, ig
            tt      / 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065 /
            t(tt)   / 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060 /
            tp(tt)  / 2055, 2060 /
            tf(tt)  / 2060 /        ;
ALIAS       (t,t_)  ;
PARAMETERS  FD, VA, ID, Y_, tr, ENDW,
            bopdef, incadj, FD0_bgp, y0_bgp,
            pcd0, pkd0, pld0, tk, tl, ty,
            tax0, tc, vinvh, EGN_shr            ;
SCALAR      GDPG                                ;

*   LOAD DATA
$GDXIN      '../1_Baseline/Set/Final/BSA_NAT%YEAR%_%CASE%%BSCN%.gdx'
$LOAD       FD VA ID Y_=Y g v hd h ne bopdef
$LOAD       ENDW incadj vinvh GDPG
$LOAD       pcd0 pkd0 pld0 tk tl tc ty ig
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

*   Temporal vintaging parameters
PARAMETER   k_deps     Capital depreciation schedule 
            de1_egn EGN specific depreciation rates     ;
    de1_egn(k_egn)  = 0.1 ;

    k_deps(k_egn,t,tt)                    = 0 ;
    k_deps(k_egn,tt,tt)                   = 1 ;
    k_deps(k_egn,t,tt)$(tt.val ge t.val)  = (1-de1_egn(k_egn))**(tt.val-t.val) ;

$INCLUDE    './Defines/Data.gms'
$SETGLOBAL  MOD kcol
DISPLAY     qgrw ;

*   Record baseline electricity investment levels
PARAMETER   ye_1    Generation technology is active (1)     ;
    ye_1(r,etc,t_,t)    = 0 ;
    ye_1(r,etc,t_,t)$((t_.val le t.val AND sum(k_egn$etmap(etc,k_egn), inv_kegn(k_egn))) OR t0(t_)) = 1 ;
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *



*   ++++++++++++++      ALTERNATE CASES         +++++++++++++ *
*** --- Clean Energy Standard ---    ***
SCALAR      rCES        ration by whoselae sales flag           ;
PARAMETER   CES_shr(*,t)  Clean energy standard generation share
            CES         CES policy flag
            CEC0        Clean energy credit -- relative quantity
            CEC         Clean energy credit -- CES-weighted quantity
            CEC_chk     Check the CEC market
            BNK         Allow banking in the CEC market
            ACP         Alternative compliance payment
            py0         Baseline output price                   ;
SETS        CET(etc)    Clean energy technology
            tCES(tt)    CES policy periods
            tBNK(tt)    Banking periods         ;
            tCES(tt) = t(tt) - t0(tt)   ;
            tBNK(tt) = tCES(tt)         ;
            CET(etc) = etc_BIO(etc) + etc_SOL(etc) + etc_WND(etc) + etc_GEO(etc) +
                       etc_NUC(etc) + etc_GAS(etc) + etc_WAT(etc) ;
CES(t)              = 0 ;
ACP(t)              = 0 ;
rCES                = 0 ;
BNK(t)              = 0 ;

*   Set nominal CEC values for share calculation
CEC0(etc,t)         = 0 ;
CEC0(cet,t)         = 1 ;
CEC0(etc_gas,t)     = 1.9/2.2 - sum(r, EGN_shr(r,"2015",etc_gas,"CO2i") * EGN_shr(r,"2015",etc_gas,"netgen")) /
                      sum(r, EGN_shr(r,"2015",etc_gas,"netgen"))  ;
CES_shr("BAU0",t)   = sum((r,CET), yq0(r,CET,t) * CEC0(CET,t)) / sum((r,etc), yq0(r,etc,t)) ;

CEC(cet,t)          = CEC0(CET,t) / ces_shr("BAU0",t)      ;
CEC_chk("crd",t)    = sum((r,etc), CEC(etc,t)       * 1e-9 * yq0(r,etc,t)) ;
CEC_chk("gen",t)    = sum((r,etc), yq0(r,etc,t))    * 1e-9   ;
DISPLAY cec_chk, CEC0, CEC ;
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *




*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   SPECIFY MODEL
$ONTEXT
$MODEL:DYN_AGP

$SECTORS:
    Y(r,s,t)                    ! Output                        !
    Ye(r,etc,t_,t)$(y0(r,etc,t) AND (t_.val le t.val AND (sum(k_egn$etmap(etc,k_egn), inv_kegn(k_egn)) OR t0(t_))))     ! Output electricity techs      !
    inv(t)                      ! Investment good               !
    invs(s,t)$(not egn(s))      ! Investment good               !
    inve(k_egn,t)$inv_kegn(k_egn)  ! Investment good egn           !
    K(s,t)$(not egn(s))         ! Capital production            !
*   TRADE
    a(r,s,t)        ! Armington aggregation         !
    x(r,s,t)        ! Export & domestic markets     !
*   CONSUMPTION GOODS
    cons(r,h,t)     ! Consumption                   !
    gov(r,t)        ! Government good               !
    cec_bank(r,t)$(CES(t) AND BNK(t)) ! CEC banking !

$COMMODITIES:
*   INTERMEDIATE GOODS
    py(r,s,t)       ! Price of output                               !
    pye(r,etc,t)$y0(r,etc,t)    ! Price of elec tech output         !
*   CAPITAL GOODS
    pinv(t)                     ! price of aggregate investment     !
    rk(s,t)$(not egn(s))        ! Rental price of capital           !
    rke(k_egn,t_,t)$(t_.val le t.val AND (inv_kegn(k_egn) OR t0(t_)))   ! Rental price of capital egn       !
    rkr0(ext)                   ! Price of resource capital         !
    pk(s,t)$(not egn(s))        ! Purchase price of capital         !
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
    pcec(r,t)$CES(t) ! Price of Clean Energy Credit  !
    pfe(k_egn,t)$(ff_egn(k_egn) AND inv_kegn(k_egn))  !   FF control on ELE investment !

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
    rcec(r,t)$(CES(t) AND (rCES OR ACP(t)))  ! Real CEC adjustment -- make CES a % of sales not generation !

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

$PROD:ye(r,etc,t_,t)$(y0(r,etc,t) AND ((t_.val le t.val AND sum(k_egn$etmap(etc,k_egn), inv_kegn(k_egn))) OR t0(t_)))   t:0 s:0
    o:pye(r,etc,t)                                           q:(y0(r,etc,t_) * tfp_out(r,etc,t))
    i:pa(r,g,t)                                              q:id0(r,g,etc,t_)
    i:pcrte(pcrt,t)$capcp                                    q:egn_emt(r,etc,pcrt,t_)                 p:1e-6
    i:pl(r,t)                                                q:ld0(r,etc,t_)                          p:pld0(r,"EGN") a:GOVT(t)  t:tl(r)
    i:rke(k_egn,t_,t)$(etmap(etc,k_egn) AND t_.val le t.val) q:(kd0(r,etc,t_) / kci_etc(r,etc,t_))    p:pkd0(r,"EGN") a:GOVT(t)  t:tk(r)

    o:pcec(r,t)$(CES(t) AND (cec(etc,t)-1)>0)  q:( (cec(etc,t)-1) * yq0(r,etc,t_) * 1e-9 )  p:1e-6
    i:pcec(r,t)$(CES(t) AND (cec(etc,t)-1)<0)  q:( (1-cec(etc,t)) * yq0(r,etc,t_) * 1e-9 )  p:1e-6

$PROD:cec_bank(r,t)$(CES(t) AND BNK(t))
    o:pcec(r,t+1)   q:1
    o:pcec(r,t)     q:1

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
$PROD:inv(t)    s:0.5
    o:pinv(t)       q:(sum((r,g), inv_g(r,g,t)))
    i:pa(r,g,t)     q:(inv_g(r,g,t))

$PROD:invs(s,t)$(not egn(s))
    o:pk(s,t+1)         q:(i1("USA") )
    o:pkt$tn(t)         q:(i1("USA") )
    o:pk(s,t)           q:(i0("USA") )
    i:pinv(t)           q:(sum(r, inv0(r)) * inv_cst(s,t))
    i:pka(s,t)$pk_phi   q:( ks0 * sum(r, kd0_shr(r,s,t))  * pka0 )

$PROD:k(s,t)$(not egn(s)) s:0
    o:pk(s,t+1)             q:( ks0 * sum(r, kd0_shr(r,s,t))  * (1 - de))
    o:pkt$tn(t)             q:( ks0 * sum(r, kd0_shr(r,s,t))  * (1 - de))
    o:pka(s,t)$pk_phi       q:( ks0 * sum(r, kd0_shr(r,s,t))  * pka0 )
    o:rk(s,t)               q:( ks0 * sum(r, kd0_shr(r,s,t))  * (ir1 + de1) )
    i:pk(s,t)               q:( ks0 * sum(r, kd0_shr(r,s,t)) )
    i:pa("USA","COM",t)     q:0.1     p:1e-6

*   Capital motion and investment (egn)
$PROD:inve(k_egn,t)$inv_kegn(k_egn)     s:els_inv_ff   t:els_inv_t
    o:pkt                               q:((i1("USA") + i0("USA") * (1-de1_egn(k_egn))**5) * sum(tn, k_deps(k_egn,t,tn)))
    o:rke(k_egn,t,t_)$(t_.val>t.val)    q:((i1("USA") * k_deps(k_egn,t+1,t_) + i0("USA") * k_deps(k_egn,t,t_)) * (ir1 + de1))
    o:rke(k_egn,t,t)                    q:( i0("USA") * (ir1 + de1) )
    i:pinv(t)                           q:(sum(r, inv0(r)))
    i:pfe(k_egn,t)$ff_egn(k_egn)        q:1

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

    e:rk(s,t)$(not egn(s))          q:(sum(rr, kd0(rr,s,t)    * (xk_shr(s,t)-ext_shr(s)))  * ext_shk(s,t) * kh_shr(r,h))
    e:rke(k_egn,"2015",t)           q:(  k0(r,h)*(1 - xk_shr(k_egn,"2015")) * kd0_shr_et(r,k_egn,"2015") * (ir1+de1) * k_deps(k_egn,"2015",t)  )
    e:rke(k_egn,"2015",t)           q:(sum((rr,etc)$etmap(etc,k_egn),   kdx0(rr,etc,t)) * kh_shr(r,h) )
    e:rkr0(ext)                     q:(sum((rr,t), kd0(rr,ext,t) * ext_shr(ext) * ext_shk(ext,t)) * kh_shr(r,h))    r:nrs(ext)
    e:pfe(k_egn,t)$(ff_egn(k_egn) AND inv_kegn(k_egn) AND ff_egn_l(k_egn,t))    q:(ff_egn_l(k_egn,t) * kh_shr(r,h))

    e:pk(s,t0)$(not egn(s)) q:(             (k0(r,h)*(1 - xk_shr(s,t0)) - i0("USA")*xk_shr(s,t0) * kh_shr(r,h)) * kd0_shr(r,s,"2015"))
    e:pkt                   q:(sum(k_egn,    k0(r,h)*(1 - xk_shr(k_egn,"2015")) * kd0_shr_et(r,k_egn,"2015") * sum(tn, k_deps(k_egn,"2015",tn))))
    e:pkt                   q:(sum((rr,etc), kdx0(rr,etc,"2060")) * (1-de) / (ir1+de1) * kh_shr(r,h))
    e:pkt                   q:(sum((rr,s),   kdx0(rr,  s,"2060")) * (1-de) / (ir1+de1) * kh_shr(r,h))
    e:pkt                   q:(-kh_shr(r,h))            r:kterm

    e:pfx           q:(sum(t, bopdef(r,h) * qref(t) * pref(t)))
    e:pfx           q:(sum(t, incadj(r,h) * qref(t) * pref(t)))
    e:pc(r,h,t)     q:(HH_n(r,h)/HH_n(r,"TOT"))                 r:fxgov(t)

$DEMAND:govt(t)
    d:pg(r,t)                       q:g0(r,t)
    e:pfx                           q:(sum( (r,h), -incadj(r,h) * qref(t) * pref(t)))
    e:pghge(t)                      q:( sum((r,cgo), GHGI(r,cgo,"DOM",t)) )
    e:pghge(t)                      q:(-sum((r,cgo), GHGI(r,cgo,"DOM",t)) )         r:cabt(t)
    e:pcrte(pcrt,t)$capcp           q:( sum(r, egn_emt(r,"TOT",pcrt,t)) )
    e:pcrte(pcrt,t)$pcp             q:(-sum(r, egn_emt(r,"TOT",pcrt,t)) )           r:cpabt(pcrt,t)
    e:pc(r,h,t)                     q:(-HH_n(r,h)/HH_n(r,"TOT"))                    r:fxgov(t)
    e:pcec(r,t)$(CES(t) AND (rCES OR ACP(t)))   q:(sum(etc,cec(etc,t) * yq0(r,etc,t) * 1e-9))   r:rcec(r,t)

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
$CONSTRAINT:rcec(r,t)$(CES(t) AND rCES AND NOT ACP(t))
    CES_shr("POL",t) =e= sum((CET,t_)$ye_1(r,CET,t_,t), ye(r,CET,t_,t) * yq0(r,CET,t_) * CEC0(CET,t)) / 
                         sum((etc,t_)$ye_1(r,etc,t_,t), ye(r,etc,t_,t) * yq0(r,etc,t_) * py(r,"egn",t));
$CONSTRAINT:rcec(r,t)$(CES(t) AND ACP(t))
    0 =g= pcec(r,t) - ACP(t) ;

$REPORT:
*   C+G
    v:hh_wel(r,h)           w:rh(r,h)
    v:hh_cons(r,s,h,t)      i:pa(r,s,t)     prod:cons(r,h,t)
    v:gv_wel(t)             w:govt(t)
    v:gv_cons(r,s,t)        i:pa(r,s,t)     prod:gov(r,t)
*   I+X
    v:inv_in(r,s,t)         i:pa(r,s,t)     prod:inv(t)
    v:invs_(s,t)$(not egn(s)) o:pk(s,t)       prod:invs(s,t)
    v:fex(r,s,t)            o:pfx           prod:x(r,s,t)
    v:fim(r,s,t)            i:pfx           prod:a(r,s,t)
*   K
    v:kd_n(r,s,t)               i:rk(s,t)           prod:y(r,s,t)
    v:kd_r(r,ext,t)             i:rkr0(ext)         prod:y(r,ext,t)
    v:kd_nele(r,etc,k_egn,t,t_) i:rke(k_egn,t,t_)   prod:ye(r,etc,t,t_)
*   L
    v:ld_y(r,s,t)           i:pl(r,t)       prod:y(r,s,t)
    v:ld_ye(r,etc,t,t_)     i:pl(r,t)       prod:ye(r,etc,t,t_)
*   ID
    v:id_y(r,g,s,t)         i:pa(r,g,t)     prod:y(r,s,t)
    v:id_ye(r,g,etc,t,t_)   i:pa(r,g,t)     prod:ye(r,etc,t,t_)
    v:id_pcec(r,etc,t,t_)   i:pcec(r,t)     prod:ye(r,etc,t,t_)
*   Y
    v:Y_y(r,s,t)            o:py(r,s,t)     prod:y(r,s,t)
    v:Y_ye(r,etc,t,t_)      o:pye(r,etc,t)  prod:ye(r,etc,t,t_)

$OFFTEXT
$SYSINCLUDE MPSGESET DYN_AGP
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *



*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   SET LEVELS AND SOLVE
y.l(r,s,t)              = qgrw(t)                   ;
ye.l(r,etc,t_,t)$(t_.val le t.val)   = sum(k_egn, k_deps(k_egn,t_,t)) / sum((k_egn,t_.local), k_deps(k_egn,t_,t)) ;
k.l(s,t)$(not egn(s))   = qref(t) * (1 - xk_shr(s,t)) ;
nrs.l(ext)              = 1 ;

kterm.l                 = sum( (r,h,trm), k0(r,h) * qref(trm) ) ;
inv.l(t)                = qref(t)               ;
inve.l(k_egn,t)         = qref(t) * kd0_shr_et("USA",k_egn,t) ;
invs.l(s,t)$(not egn(s))= qref(t) * sum(r, kd0_shr(r,s,t))              ;
cons.l(r,h,t)           = qgrw(t)               ;

gov.l(r,t)      = qgrw(t)   ;
a.l(r,s,t)      = qgrw(t)   ;
x.l(r,s,t)      = qgrw(t)   ;

*   Set price levels
py.l(r,s,t)     = pref(t)   ;
pye.l(r,etc,t)  = pref(t)   ;
pc.l(r,h,t)     = pref(t)   ;
py0(r,"egn",t)  = pref(t)   ;

pl.l(r,t)               = pref(t)   ;
pinv.l(t)               = pref(t)   ;
rk.l(s,t)$(not egn(s))  = pref(t)   ;
rke.l(k_egn,t,t_)$(t_.val ge t.val) = pref(t_)  ;
rkr0.l(ext)             = 1         ;
pk.l(s,t)$(not egn(s))  = pref(t)  * (1 + ir) * (ir1 + de1) / (ir + de)     ;
pkt.l                   = sum( tn, pref(tn))  * ( (1 + ir) * (ir1 + de1) /
                            (ir + de) ) / ( 1 + ir)                         ;
pg.l(r,t)   = pref(t)   ;
pa.l(r,s,t) = pref(t)   ;
pd.l(r,s,t) = pref(t)   ;
pfx.fx      = 1         ;
pnum.l(t)   = pref(t)   ;

*   POLICY
pghge.l(t)              =  0        ;
pghge.lo(t)             =  0        ;
pcrte.l(pcrt,t)$pcp     =  0        ;
pcrte.lo(pcrt,t)$pcp    =  0        ;
cabt.l(t)               =  0        ;
cabt.lo(t)              = -1        ;
cabt.up(t)              =  1        ;
cpabt.l(pcrt,t)$pcp     =  0        ;
cpabt.lo(pcrt,t)$pcp    = -1        ;
cpabt.up(pcrt,t)$pcp    =  1        ;
rcec.l(r,t)             =  0        ;

parameter ye_tst ;
set etgrp / GEO, SOL, WND, NUC, GAS, BIO, COL / ;
ye_tst("GEO","PRE",t)     = sum((r,etc_geo,t_)$(t_.val le t.val), ye.l(r,etc_geo,t_,t) * yq0(r,etc_geo,t_)) ;
ye_tst("SOL","PRE",t)     = sum((r,etc_sol,t_)$(t_.val le t.val), ye.l(r,etc_sol,t_,t) * yq0(r,etc_sol,t_)) ;
ye_tst("WND","PRE",t)     = sum((r,etc_wnd,t_)$(t_.val le t.val), ye.l(r,etc_wnd,t_,t) * yq0(r,etc_wnd,t_)) ;
ye_tst("NUC","PRE",t)     = sum((r,etc_nuc,t_)$(t_.val le t.val), ye.l(r,etc_nuc,t_,t) * yq0(r,etc_nuc,t_)) ;
ye_tst("GAS","PRE",t)     = sum((r,etc_gas,t_)$(t_.val le t.val), ye.l(r,etc_gas,t_,t) * yq0(r,etc_gas,t_)) ;
ye_tst("BIO","PRE",t)     = sum((r,etc_bio,t_)$(t_.val le t.val), ye.l(r,etc_bio,t_,t) * yq0(r,etc_bio,t_)) ;
ye_tst("COL","PRE",t)     = sum((r,etc_col,t_)$(t_.val le t.val), ye.l(r,etc_col,t_,t) * yq0(r,etc_col,t_)) ;
ye_tst(etgrp,"PRE",t)     = ye_tst(etgrp,"PRE",t) / sum((r,etc,t_)$(t_.val le t.val), ye.l(r,etc,t_,t) * yq0(r,etc,t_)) ;

* kci_etc(r,etc_GEO,t)$(t.val > 2015)      = 0.5 ;
* kci_etc(r,etc_BIO,t)$(t.val > 2015)      = 0.5 ;

$INCLUDE DYN_AGP.gen
DYN_AGP.savepoint   =   2   ;
DYN_AGP.workspace   = 1024  ;
DYN_AGP.iterlim     = 1e5   ;
DYN_AGP.reslim      = 4e3   ;
* DYN_AGP.iterlim     =   0   ;
DISPLAY ff_egn_l ;
$IFTHEN.z "%SCN%"=="0"
SOLVE DYN_AGP using mcp     ;
$ENDIF.z

*   BASELINE SOLN
$IFTHEN.x NOT "%SCN%"=="0"
EXECUTE_LOADPOINT 'DYN_AGP_p1.gdx'  ;
$ENDIF.x

$INCLUDE DYN_AGP.gen
SOLVE DYN_AGP using mcp         ;
DYN_AGP.savepoint   =   0       ;
ff_egn_l(k_egn,t)$ff_egn(k_egn) = max(inve.l(k_egn,t),1e-4)  ;

*REPORT 
$INCLUDE ./Report_tv.gms
$INCLUDE ./Defines/CES_scenarios.gms
$SETGLOBAL POL CES
rCES                = %rCES%    ;
ACP(t)$CES(t)       = %ACP% / pref(t) ;

*   LOAD MODEL & SEED VARIABLES
$INCLUDE DYN_AGP.gen
cec_bank.l(r,t)$(CES(t) AND BNK(t)) = 0         ;
pcec.l(r,t)$CES(t)                  = pref(t)   ;
py0(r,"egn",t)                      = py.l(r,"egn",t)    ;

$IFTHEN.y NOT "%SCN%"=="0"
* els_grd = 1.00 ;
* els_l1  = 7.00 ;
* els_l2  = 7.00 ;
* els_l3  = 7.00 ;
* els_l4  = 7.00 ;
* 
els_inv_ff      = 0.5  ;
els_inv_t       = 0.1 ;

* kci_etc(r,etc_GEO,t)$(t.val > 2015) = 0.5 ; 
* kci_etc(r,etc_WAT,t)$(t.val > 2015) = 0.9 ; 
$ENDIF.y

*   RE-SOLVE MODEL
DISPLAY CEC, ff_egn ;
SOLVE   DYN_AGP using mcp   ;
DISPLAY CEC, pcec.l, cec_bank.l, rcec.l, BNK, ACP, rcec.l  ;
$INCLUDE    ./Report_tv.gms
$INCLUDE    ../5_Analyses_x/CES/ARTIMAS_CES_Reporting_tv.gms

*   Check ELE shares
ye_tst("GEO","PST",t)     = sum((r,etc_geo,t_)$(t_.val le t.val), ye.l(r,etc_geo,t_,t) * yq0(r,etc_geo,t_)) ;
ye_tst("SOL","PST",t)     = sum((r,etc_sol,t_)$(t_.val le t.val), ye.l(r,etc_sol,t_,t) * yq0(r,etc_sol,t_)) ;
ye_tst("WND","PST",t)     = sum((r,etc_wnd,t_)$(t_.val le t.val), ye.l(r,etc_wnd,t_,t) * yq0(r,etc_wnd,t_)) ;
ye_tst("NUC","PST",t)     = sum((r,etc_nuc,t_)$(t_.val le t.val), ye.l(r,etc_nuc,t_,t) * yq0(r,etc_nuc,t_)) ;
ye_tst("GAS","PST",t)     = sum((r,etc_gas,t_)$(t_.val le t.val), ye.l(r,etc_gas,t_,t) * yq0(r,etc_gas,t_)) ;
ye_tst("BIO","PST",t)     = sum((r,etc_bio,t_)$(t_.val le t.val), ye.l(r,etc_bio,t_,t) * yq0(r,etc_bio,t_)) ;
ye_tst("COL","PST",t)     = sum((r,etc_col,t_)$(t_.val le t.val), ye.l(r,etc_col,t_,t) * yq0(r,etc_col,t_)) ;
ye_tst(etgrp,"PST",t)     = ye_tst(etgrp,"PST",t) / sum((r,etc,t_)$(t_.val le t.val), ye.l(r,etc,t_,t) * yq0(r,etc,t_)) ;
DISPLAY ye_tst, CES_shr, els_grd ;


*   --- META: Report scenario solution behavior
PARAMETER meta ;
    meta("modelstat")   = DYN_AGP.modelstat ;
    meta("iterusd")     = DYN_AGP.iterusd   ;
file lp_disp / lp_disp.txt / ;
put lp_disp ;
put '/////////////////////////////////' /
    '////                         ////' /
    '////                         ////' /
    '////                         ////' /
    '////                         ////' /
    '----    LAST SOLVE FOR %POL%%CTX%%CTAX%_SCN%SCN%' /
    '----||  --> SOLVE STAT = ' DYN_AGP.modelstat /
    '----||  --> ITERATIONS = ' DYN_AGP.iterusd   /
    '////                         ////' /
    '////                         ////' /
    '////                         ////' /
    '////                         ////' /
    '/////////////////////////////////' /;
putclose lp_disp ;
execute 'cat lp_disp.txt' ;
execute 'rm lp_disp.txt' ;

*   FLAG FAILED SOLVES
FILE sol_stat / FAILED_SOLVE_%POL%%CTX%%CTAX%_SCN%SCN%.txt / ;
if(meta("modelstat")>1,
    put  sol_stat ;
    put  'AEO%YEAR%_%CASE%_%POL%%CTX%%CTAX%_CES     ' DYN_AGP.modelstat /;
); 
if(meta("modelstat")=1,
    execute 'rm FAILED_SOLVE_%POL%%CTX%%CTAX%_SCN%SCN%.txt' ;
);

$IFTHEN "%RPT%"=="Y"
EXECUTE_UNLOAD './Out_x/ARTIMAS_tv0_Results_AEO%YEAR%_%CASE%_CES_%RUN%.gdx', 
                FD1 VA1 VA2 Y1 Y2 ID1 Px PLCY meta wel ENV NRG report eprc  ;
execute 'gdxxrw.exe ./Out_x/ARTIMAS_tv0_Results_AEO%YEAR%_%CASE%_CES_%RUN%.gdx o=../5_Analyses_x/CES/CES_DATA_Template_Round1_ARTIMAS_tv0_he.xlsx par=report rng=SCN%SCN%_Report!A1' ;
$ENDIF
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *

PARAMETER report ;
report(t) = rke.l("k_GAS","2020",t) / pref(t);

FILE rpt / report_%tax%.txt / ;
put  rpt ;
loop(t, put t.tl,@5,report(t) /);
$EXIT






