*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   SIMPLE DYNAMICS
SETS
            r       / USA /
            trd     / DOM, INT /
            ftr     / FEX, FIM /
            e(g)    Energy sectors      / "OGE", "COL", "ETD", "GAS",  "OIL"  /
            ec(g)   Energy less OGE     / "ETD", "GAS", "OIL", "COL" /
            ef(g)   Energy fuels        / "GAS", "OIL", "COL" /
            ext(g)  Extractive sectors  / "AGR", "COL", "MIN", "OIL", "GAS" /
            egn(g)  / "EGN" /
            ogx(g)  / "OGE" /
            cgo(g)  / "COL", "OIL", "GAS" /
            coge(g) / COL, OIL, GAS, ETD /
            go(g)   / "OIL", "GAS" /
            col(g)  / "COL" /
            oil(g)  / "OIL" /
            gas(g)  / "GAS" /
            ele(g)  / "EGN", "ETD" /
            trn(g)  / "TRG", "TRW", "TRA" /
            mat(g)  Materials
*   EGN sector inputs
            etci    / "netgen", "COST_tot", "CSTi",
                         "Ki_tot", "Li_tot", "Mi_tot", "COLi", "GASi", "OILi", "BIOi", "OTHi", "TAXi",
                         "Ki_gen", "Li_gen", "Mi_gen", "Ki_NOX", "Li_NOX", "Mi_NOX", "Ki_SOX",
                         "Li_SOX", "Mi_SOX", "Ki_PM", "Li_PM", "Mi_PM", "Ki_HG", "Li_HG", "Mi_HG",
                         "PMi", "HGi", "SOxi", "NOxi", "CO2i", "N2Oi", "CH4i", "Fgasi"                  /
            etot(etci) / "Ki_tot", "Li_tot", "Mi_tot", "COLi", "GASi", "OILi", "BIOi", "OTHi", "TAXi"   /
            eprt(etci) / "Ki_gen", "Li_gen", "Mi_gen", "Ki_NOX", "Li_NOX", "Mi_NOX", "Ki_SOX", "Li_SOX",
                         "Mi_SOX", "Ki_PM", "Li_PM", "Mi_PM", "Ki_HG", "Li_HG", "Mi_HG", "TAXi"         /
            emit(etci) / "PMi", "HGi", "SOxi", "NOxi", "CO2i", "N2Oi", "CH4i", "Fgasi"                  /
            kele       / "k_BIO", "k_COL", "k_GAS", "k_GEO", "k_NUC", "k_OIL", "k_OTH", "k_SOL", 
                         "k_WAT", "k_WND"   /
            egn_tp     / egn_BIO, egn_COL, egn_GAS, egn_GEO, egn_NUC, egn_OIL, egn_OTH, egn_SOL, egn_WAT, egn_WND /

*            inv_kegn(k_egn)     k_egn stocks that can be expanded
*                       / k_BIO, T13, T21, T26, T33, k_GAS, k_GEO, k_NUC, k_OIL, k_OTH, k_SOL, k_WAT, k_WND /
*            ff_egn(k_egn)   k_egn stocks that are limited by a FF   / k_WAT, k_NUC, k_GEO, k_BIO /

            inv_kegn(k_egn)     k_egn stocks that can be expanded
                       / T1*T2, T13, T21, T26, T33, k_GAS, T42*T44, t45, t46, k_OIL, k_OTH, k_SOL, T57*T60, k_WND /
            ff_egn(k_egn)   k_egn stocks that are limited by a FF   / T57*T60, t45, t46, T42*T44, T1*T2 /

            t0, tn, trm, se ;
            mat(g)  = g(g) - ec(g) - trn(g) ;

    SET     kxm         Capital markets     / set.g, set.k_egn /
            kx_egn(kxm)                     / "EGN" /        
            etmap       Map EGN technologies to their capital markets ;
            etmap(etc,etc)$(etc_COL(etc))   = yes ;
            etmap(etc,etc)$(etc_NUC(etc))   = yes ;
            etmap(etc,etc)$(etc_BIO(etc))   = yes ;
            etmap(etc,etc)$(etc_GEO(etc))   = yes ;
            etmap(etc,etc)$(etc_WAT(etc))   = yes ;

            etmap(etc,"k_GAS")$(etc_GAS(etc)) = yes ;
            etmap(etc,"k_OIL")$(etc_OIL(etc)) = yes ;
            etmap(etc,"k_OTH")$(etc_OTH(etc)) = yes ;
            etmap(etc,"k_SOL")$(etc_SOL(etc)) = yes ;
            etmap(etc,"k_WND")$(etc_WND(etc)) = yes ;

    ALIAS   (g,s), (g,gg), (v,w), (h,hh), (r,rr), (cgo, cgo_), (mat,mat_), (etc,etc_), (ef,ef_), (ext,ext_) ;
            t0(tt)  = YES$(ord(tt)  = 1)            ;
            tn(t)   = YES$(ord(t)   = card(t))      ;
            trm(tt) = YES$(ord(tt)  = card(tt))     ;

    SCALAR  ir1 interest          / 0.050 /
            bg1 balanced growth rate
            de1 depreciation
            ir, bg, de            Period rates   ;
            bg1 = GDPG ;

    PARAMETERS  
        k0      PV of household capital endowment
        ks0     Aggregate capital supply 
        ke0     Household capital earnings
        kd0_shr     Sector share of period capital demand
        kd0_shr_et  Sector share of period capital demand -- egn technologies
        pka0    capital adjustment cost
        pk_phi  capital adjustment cost scale factor
        i0      Intra-period investment
        i1      Inter-period investment
        pref    Reference price
        qref    Reference quantity
        inv0    Aggregate investment
        qgrw    Macro growth scaling factor
        iprft   Investment zero-profit check
        kmkt    Capital zero-profit check
        inv_g0  Benchmark investment goods demand
        inv_g   Period-specific investment goods demand
        lambda  Fraction of vintage-t capital surviving in period tt
        ff_egn_l Level of fixed factor for EGN techs ;
        ke0(r,h)    = ENDW(r,"KAP",h)               ;
        inv_g0(r,g) = sum( t0, FD(r,g,"INV",t0))    ;

****    NEED TO REDEFINE FRACTION OF GOVEX AS INVESTMENT -- WILL BOOST DEPRECIATION RATE
**--    BEA TABLE 1.5.5 GDP, expanded detail

*   Calculate depreciation implied by invest Q, capital, growth & int rates
        de1 = ( sum(r, sum(g, inv_g0(r,g)) * ir1
                - sum(h, ke0(r,h)) * bg1 ) )    /
                sum(r, sum(h, ke0(r,h))
                - sum(g, inv_g0(r,g)) )            ;

        ir  = (1 + ir1)**5 - 1 ; bg  = (1 + bg1)**5 - 1 ; de  = 1 - (1 - de1)**5 ;
        lambda(s,t,tt) = (1 - de1)**(tt.val - t.val) ;
        lambda(s,t,tt)$(t.val > tt.val) = 0          ;
        inv0(r)     = sum(g, inv_g0(r,g))      ;

        qref(tt)    = (1 + bg)**(ord(tt)-1)                     ;
        qgrw(tt)    = qref(tt) / qref_(tt)                      ;
        pref(tt)    = (1 / (1 + ir))**(ord(tt)-1)               ;

*   Calculate intra- and inter-period investment
        i0(r)       = inv0(r) / (ir - bg) * ( (ir+de) / (ir1+de1) - (bg+de)/(bg1+de1) ) ;
        i1(r)       = inv0(r) / (ir - bg) * ( (bg+de)/(bg1+de1)*(1+ir) - (ir+de)/(ir1+de1)*(1+bg)) ;

*   Calculate present value of capital endowment
        k0(r,h)     = ke0(r,h) * ( 1/( ir1 + de1 ) - i0(r) / sum(hh,ENDW(r,"KAP",hh))) ;

*   Check investment and capital zero profit
        iprft(r)    = inv0(r) - ( (1+ir)*(ir1+de1)/(ir+de) ) * ( i0(r) + i1(r) / (1+ir)) ;
        kmkt(r)     = ( sum( h, ke0(r,h)) / (ir1+de1) - i0(r))*(bg+de)-i0(r)*(1-de) - i1(r);

        pk_phi  = %pk_phi% ;
        pka0    = pk_phi * sqr(de+bg)/2;

DISPLAY incadj, de, t0, tn, trm, k0, ke0, ir1, de1, iprft, i0, i1, kmkt, etmap, lambda, pka0 ;


PARAMETER inv_test, inv_adj;
    inv_adj(r,h)  = ke0(r,h) * (bg1 + de1) / (ir1 + de1) - vinvh(r,h)   ;
    incadj(r,h)   = incadj(r,h) + inv_adj(r,h)  ;
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *




*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   -- Define model calibration parameters
PARAMETERS
     y0     Total output
    id0     Intermediate demand
    ld0     Labor demand
    kd0     Capital demand
     m0     Imports
     x0     Exports
     a0     Domestic market - Armington output
     c0     Total household consumption
    cd0     Household consumption demand
     g0     Total govt consumption
    gd0     Govt consumption demand
    le0     Household labor endowment
    BOP     total balance of payments
    BOP_shr BOP share to each HH        
    ext_shr Share of sector capital dedicated to resource   ;

 y0(r,s,t)      = y_(r,s,t)         ;
id0(r,g,s,t)    = ID(r,g,s,t)       ;
ld0(r,s,t)      = VA(r,"LAB",s,t)   ;
kd0(r,s,t)      = VA(r,"KAP",s,t)   ;
ext_shr(s)      = 0.00              ;
ext_shr(ext)    = 0.10              ;
ext_shr(go)     = 0.80              ;
ext_shr("AVG")  = 0.10              ;

*   -- Trade
m0(r,g,"DOM",t) = -FD(r,g,"DIM",t)  ;
m0(r,g,"INT",t) = -FD(r,g,"FIM",t)  ;

x0(r,g,"DOM",t) =  FD(r,g,"DEX",t)  ;
x0(r,g,"INT",t) =  FD(r,g,"FEX",t)  ;

a0(r,s,t)       = y0(r,s,t) + m0(r,s,"INT",t) - x0(r,s,"INT",t) ;

*   -- Consumption
c0(r,h,t)       = sum( g, FD(r,g,h,t) * pcd0(r,g) ) ;
cd0(r,g,h,t)    = FD(r,g,h,t)                       ;
g0(r,t)         = sum( g, FD(r,g,"GOV",t) )         ;
gd0(r,g,t)      = FD(r,g,"GOV",t)                   ;

*   -- Investment: rescale to initial period level
inv_g(r,s,t)    = FD(r,s,"INV",t) * sum( (rr,g,t0), FD(rr,g,"INV",t0) )
                                  / sum( (rr,g), FD(rr,g,"INV",t) ) ;

*   -- Prices - biofuel prices equal to bulk chem sector
pld0(r,"BIO")   = pld0(r,"BCH")     ;
pkd0(r,"BIO")   = pkd0(r,"BCH")     ;
pcd0(r,"BIO")   = pcd0(r,"BCH")     ;

PARAMETER ydom ; 
    ydom(r,s,t) = y0(r,s,t) - x0(r,s,"INT",t);
DISPLAY id0, x0, m0, a0, ydom ;
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *



*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   -- Disaggregate EGN sector
PARAMETER   kci_etc     Total factor productivity for electric generation technologies
            tfp_oth     Total factor productivity for other sectors
            tfp_out     Total factor productivity for other sectors applied to output
            inv_cst     Investment cost multiplier
            ext_shk     Extractive sector resource base shock
            fp_hel      Productivity for energy inputs
            Yq0         Ele tech output quantity (mwh)
            els_nrs     Elasticity of supply of natural resource reserves
            kx0, kdx0, xk_shr, kh_shr                                               ;

EGN_shr(r,tp,etc,etci)  = EGN_shr(r,"2050",etc,etci) ;


DISPLAY egn_shr ;
Y0(r,etc,t)             = EGN_shr(r,t,etc,"CSTi") * Y0(r,"EGN",t) ;
kci_etc(r,etc,t)        = 1 ;
tfp_oth(r,s,t)          = 1 ;
tfp_out(r,s,t)          = 1 ;
tfp_out(r,etc,t)        = 1 ;
inv_cst(s,t)            = 1 ;
ext_shk(s,t)            = 1 ;
els_nrs(ext)            = 0.15 ;
*   Aurora (2014): estimates of gas supply, avg of Tbl 4 50th pctl LR estimates for latter est pd
els_nrs("GAS")          = 0.30 ;
fp_hel(r,g,s,t)         = 0 ;
fp_hel(r,g,h,t)         = 0 ;

id0(r,mat,etc,t)    = Y0(r,etc,t) * ( EGN_shr(r,t,etc,"Mi_tot")
                        + EGN_shr(r,t,etc,"OTHi") ) *
                        id0(r,mat,"EGN",t) / sum(mat_, id0(r,mat_,"EGN",t)) ;
id0(r,"GAS",etc,t)  = Y0(r,etc,t) * EGN_shr(r,t,etc,"GASi")     ;
id0(r,"OIL",etc,t)  = Y0(r,etc,t) * EGN_shr(r,t,etc,"OILi")     ;
id0(r,"COL",etc,t)  = Y0(r,etc,t) * EGN_shr(r,t,etc,"COLi")     ;
id0(r,"BIO",etc,t)  = Y0(r,etc,t) * EGN_shr(r,t,etc,"BIOi")     ;
ld0(r,etc,t)        = Y0(r,etc,t) * EGN_shr(r,t,etc,"Li_tot")   ;
kd0(r,etc,t)        = Y0(r,etc,t) * EGN_shr(r,t,etc,"Ki_tot")   ;

parameter  egn_disp ;
egn_disp(r,etc,t) = EGN_shr(r,t,etc,"Ki_tot") ;
display kd0, egn_disp ;

*   Egn physical quantities
PARAMETER   yq0_tot, egn_emti, egn_emt;
SET     plti    Pollutants intensities  / NOxi, SOxi, PMi, Fgasi, N2Oi , CH4i, HGi, CO2i /
        plt     Pollutants              / NOx, SOx, PM, Fgas, N2O , CH4, HG, CO2 / 
        pco2(plt)                       / CO2 / 
        pghg(plt)                       / CO2, N2O, CH4, Fgas /
        ptox(plt)                       / HG /
        pcrt(plt)                       
        pmap    Map pltnt measures      / NOxi.NOx, SOxi.SOx, PMi.PM, Fgasi.Fgas, N2Oi.N2O , CH4i.CH4, HGi.HG, CO2i.CO2 /  ;
        pcrt(plt)   = plt(plt) - pghg(plt) - ptox(plt) ;

    Yq0(r,etc,t)            = EGN_shr(r,t,etc,"netgen") ;
    yq0_tot(r,t)            = sum(etc, Yq0(r,etc,t))    ;
    egn_emti(r,etc,plt,t)   = EGN_shr(r,t,etc,plt)      ;
    egn_emt(r,etc,  plt,t)  = sum(plti$pmap(plti,plt),           EGN_shr(r,t,etc,"netgen")     * EGN_shr(r,t,etc,plti)      );
    egn_emt(r,"COL",plt,t)  = sum((etc_col,plti)$pmap(plti,plt), EGN_shr(r,t,etc_col,"netgen") * EGN_shr(r,t,etc_col,plti)  );
    egn_emt(r,"GAS",plt,t)  = sum((etc_gas,plti)$pmap(plti,plt), EGN_shr(r,t,etc_gas,"netgen") * EGN_shr(r,t,etc_gas,plti)  );
    egn_emt(r,"OIL",plt,t)  = sum((etc_OIL,plti)$pmap(plti,plt), EGN_shr(r,t,etc_OIL,"netgen") * EGN_shr(r,t,etc_OIL,plti)  );
    egn_emt(r,"TOT",plt,t)  = sum((etc,plti)$pmap(plti,plt),     EGN_shr(r,t,etc,"netgen")     * EGN_shr(r,t,etc,plti)      );

display yq0_tot ;
Yq0(r,etc,t)    = sum(etc_, EGN_shr(r,t,etc_,"netgen")) * Y0(r,etc,t) / sum(etc_, Y0(r,etc_,"2015"))   ;
yq0_tot(r,t)    = sum(etc, Yq0(r,etc,t))        ;
display yq0_tot ;

*   Adjust input values to sum to EGN total exactly
id0(r,"GAS",etc,t)  = id0(r,"GAS","EGN",t) * id0(r,"GAS",etc,t) / sum(etc_, id0(r,"GAS",etc_,t))  ;
id0(r,"OIL",etc,t)  = id0(r,"OIL","EGN",t) * id0(r,"OIL",etc,t) / sum(etc_, id0(r,"OIL",etc_,t))  ;
id0(r,"COL",etc,t)  = id0(r,"COL","EGN",t) * id0(r,"COL",etc,t) / sum(etc_, id0(r,"COL",etc_,t))  ;
id0(r,"BIO",etc,t)  = id0(r,"BIO","EGN",t) * id0(r,"BIO",etc,t) / sum(etc_, id0(r,"BIO",etc_,t))  ;
ld0(r,etc,t)        = ld0(r,"EGN",t)       * ld0(r,etc,t)       / sum(etc_, ld0(r,etc_,t)      )  ;
kd0(r,etc,t)        = kd0(r,"EGN",t)       * kd0(r,etc,t)       / sum(etc_, kd0(r,etc_,t)      )  ;

xk_shr(s,tt)    = (1 - de1)**(tt.val-2010) / (1 + bg1)**(tt.val-2010)           ;
xk_shr(k_egn,tt)= (1 - de1_egn(k_egn))**(tt.val-2010) / (1 + bg1)**(tt.val-2010);
kdx0(r,etc,t)   = kd0(r,etc,t) * sum(k_egn$etmap(etc,k_egn), xk_shr(k_egn,t))   ;
kdx0(r,s,t)     = kd0(r,s,t)   * xk_shr(s,t)            ;
kdx0(r,"EGN",t) = sum(etc, kdx0(r,etc,t) )              ;
kx0(t)          = sum( (r,s), kdx0(r,s,t))              ;
kh_shr(r,h)     = k0(r,h)/sum((rr,hh), k0(rr,hh))       ;
ks0             = sum( r, sum( h, k0(r,h) ) + i0(r) )   ;

Y0(r,etc,t) = sum(g, id0(r,g,etc,t)) + ld0(r,etc,t) * pld0(r,"EGN") + kd0(r,etc,t) * pkd0(r,"EGN") ;
DISPLAY id0, kd0, ld0, y0, pld0, kh_shr, xk_shr ;
*   -- Endowments
le0(r,h,t)          = sum(s, ld0(r,s,t) ) * qgrw(t) *
                            ENDW(r,"LAB",h)  / sum( hh, ENDW(r,"LAB",hh))    ;
BOP(r,t)            = sum( h, bopdef(r,h)) * qref(t)            ;
BOP_shr(r,h)        = bopdef(r,h)   / sum( hh, bopdef(r,hh))    ;
kd0_shr(r,s,t)      = kd0(r,s,t)    / sum((rr,g), kd0(r,g,t))   ;
kd0_shr_et(r,k_egn,t) = sum(etc$etmap(etc,k_egn), kd0(r,etc,t)) / sum(s, kd0(r,s,t));

*   Initialize EGN investment fixed factor to reference investment growth
ff_egn_l(k_egn,t)   = 1 ;
*   qref(t) * kd0_shr_et("USA",k_egn,t) ;
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
DISPLAY kd0_shr, kd0_shr_et, k_egn, etmap ;


*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   SET ELASTICITIES
PARAMETERS
    els_s       top-level elasticity
    els_trn     transportation modes
    els_eva     energy-value-add elasticity
    els_va      value-add elasticity
    els_nrg     energy elasticity
    els_fue     fuel elasticity
    els_arm     Armington import elasticity     
    els_arx     Armington export elasticity 
    els_l1, els_l2, els_l3, els_l4  load elasticities   
    els_grd     Elasticity across grid gen categories   
    els_c       elasticity of consumption - top-level
    els_ctrn    transportation modes 
    els_ce      consumption-energy 
    els_cmat    materials consumption 
    els_cnrg    ele-fuel consumption
    els_cfue    fuel consumption
    els_inv_ff  Investment fixed factor
    els_inv_t   Investment depreciation     ;

els_s(s)        = 0.10  ;
els_trn(s)      = 0.20  ;
els_eva(s)      = 0.25  ;
els_va(s)       = 0.75  ;
els_nrg(s)      = 0.75  ;
els_fue(s)      = 1.00  ;
els_arm(s)      = 2.00  ;
els_arx(s)      = 2.00  ;

els_s("EGN")    = 0.00  ;
els_eva(ele)    = 0.00  ;
els_va("ETD")   = 0.25  ;
els_va("EGN")   = 0.00  ;
els_nrg(ele)    = 0.00  ;
els_fue(ele)    = 0.00  ;
els_arm("EGN")  = 0.00  ;
els_arm("ETD")  = 0.75  ;
els_arx("EGN")  = 0.00  ;

els_c(r,h,t)    = 0.20 ;
els_ctrn(r,h,t) = 0.20 ;
els_ce(r,h,t)   = 0.40 ;
els_cmat(r,h,t) = 0.20 ;
els_cnrg(r,h,t) = 0.20 ;
els_cfue(r,h,t) = 0.40 ;

els_inv_ff      = 0.40 ;
els_inv_t       = 0.00 ;

els_grd = 0.50 ;
els_l1  = 5.00 ;
els_l2  = 5.00 ;
els_l3  = 5.00 ;
els_l4  = 5.00 ;


*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *



*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   GHG EMISSIONS - AGGREGATE
*   SOURCE:  US EPA GHG Inventory - 2016
PARAMETER   eprc, edmd          AEO energy prices and demand
            cptax               Criteria pollutant prices
            ctax, GHGI_, GHGI   Greenhouse gas inventory data               ;
SCALAR      capcp               Cap ELE criteria pollutants at current intensities
            pcp                 Tax ELE criteria pollutants at specified price  ;
    pcp     = %PCP% ;
    capcp   = %CAPCP% ;

$GDXIN  ../1_Baseline/Set/Data/AEO%YEAR%_%CASE%_sum.gdx
$LOAD   eprc edmd=ED
$GDXIN
* $CALL 'GDXXRW.exe ../0_Data/EPA/GHGI-2016_Tbl3-5.xlsx o=./Data/GHGI.gdx PAR=GHGI_ RNG=Tbl3.5!C3:AD26 RDIM=1 CDIM=1'
$GDXIN  ./Data/GHGI.gdx
$LOAD   GHGI_
$GDXIN

display edmd, eprc ;
SET     tval        / val2015, val2020, val2025, val2030, val2035, val2040, val2045, val2050 /
        tmap        / 2015.val2015, 2020.val2020, 2025.val2025, 2030.val2030, 2035.val2035,
                        2040.val2040, 2045.val2045, 2050.val2050 /  ;
edmd(s,e,t)         = sum(tmap(t,tval), edmd(s,e,tval) )            ;
edmd("RES",e,t)     = sum(tmap(t,tval), edmd("RES",e,tval) )        ;
eprc(s,e,t)         = sum(tmap(t,tval), eprc(s,e,tval) )            ;
eprc("OIL","OIL",t) = eprc("COM","OIL",t)                           ;
eprc("RES",e,t)     = sum(tmap(t,tval), eprc("RES",e,tval) )        ;

eprc("AVG",cgo,t)$(t.val < 2055) = ( sum(g$eprc(g,cgo,t), eprc(g,cgo,t) * edmd(g,cgo,t) )
                        + eprc("RES",cgo,t) * edmd("RES",cgo,t) ) 
                        / (sum(g$eprc(g,cgo,t), edmd(g,cgo,t)) + edmd("RES",cgo,t))  ;
eprc("AVG",cgo,t)$(t.val > 2050) = eprc("AVG",cgo,"2050") ;

GHGI("USA","COL","DOM","_") = GHGI_("Coal","2015")        * 1e-3 ;
GHGI("USA","GAS","DOM","_") = GHGI_("Natural Gas","2015") * 1e-3 ;
GHGI("USA","OIL","DOM","_") = GHGI_("Petroleum","2015")   * 1e-3 ;
GHGI("USA","TOT","DOM","_") = GHGI_("Total","2015")       * 1e-3 ;

GHGI(r,cgo,"INT",t)         = GHGI(r,cgo,"DOM","_") * m0(r,cgo,"INT",t) / (y0(r,cgo,"2015") - x0(r,cgo,"INT","2015"))   ;
GHGI(r,cgo,"DOM",t)         = GHGI(r,cgo,"DOM","_") * ( a0(r,cgo,t) * eprc("AVG",cgo,"2015") ) / ( a0(r,cgo,"2015") * eprc("AVG",cgo,t) ) ;
GHGI(r,"TOT","DOM",t)       = sum(cgo, GHGI(r,cgo,"DOM",t) ) ;

*   Calculate BCA rates
*   -- Domestic matrix inversions to get embodied carbon 

DISPLAY GHGI, egn_emt, egn_shr ;
ctax(t)         = 1e-6 ;
cptax(pcrt,t)   = 1e-6 ;

*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *


*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
*   Household counts
*   SOURCE:  Census HINC-01
PARAMETER   HHLD_, HH_n     Number of households by group   ;
* $CALL 'GDXXRW.exe ../0_Data/Census/Census2015_hinc01.xlsx o=./Data/Census.gdx PAR=HHLD_ RNG=hhlds!A1 RDIM=1 CDIM=1'
$GDXIN  ./Data/Census.gdx
$LOAD   HHLD_
$GDXIN  

HH_n(r,h)       = HHLD_(r,h)            ;
HH_n(r,"TOT")   = sum(h, HH_n(r,h))     ;
parameter hh_tst ;
    hh_tst(r,h) = HH_n(r,h)/HH_n(r,"TOT") ;
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
