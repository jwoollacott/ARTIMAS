*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
PARAMETERS FD1, VA1, VA2, ID1, Px, Y1, Y2, PLCY, WEL, ENV, NRG ;
*   WELFARE
    WEL(r,"WEL",h,"%POL%%CTX%","")          = hh_wel.l(r,h) * sum(t, c0(r,h,t) * pref(t))   ;
    WEL(r,"WEL",h,"%POL%%CTX%",t)           = hh_wel.l(r,h) * c0(r,h,t) * pref(t)           ;
    WEL(r,"WEL","TOT","%POL%%CTX%","")      = sum(h, hh_wel.l(r,h) * sum(t, c0(r,h,t) * pref(t)))   ;
    WEL("USA","GOVTRN","","%POL%%CTX%",t)   = fxgov.l(t)    ;

*   FINAL DEMAND
    FD1(r,g,h,t,"%POL%%CTX%")           = hh_cons.l(r,g,h,t)      ;
    FD1(r,g,"GOV",t,"%POL%%CTX%")       = gv_cons.l(r,g,t)        ;
    FD1(r,g,"INV_in",t,"%POL%%CTX%")    = inv_in.l(r,g,t)         ;
    FD1("USA",g,"INV",t,"%POL%%CTX%")$(not egn(g))          = invs.l(g,t)       * sum(r, inv0(r))   ;
    FD1("USA",k_egn,"INV",t,"%POL%%CTX%")$inv_kegn(k_egn)   = inve.l(k_egn,t)   * sum(r, inv0(r))   ;

    FD1(r,g,"FEX",t,"%POL%%CTX%")       =  fex.l(r,g,t) / pref(t) ;
    FD1(r,g,"FIM",t,"%POL%%CTX%")       = -fim.l(r,g,t) / pref(t) ;

    WEL(r,"GDP","exp","%POL%%CTX%",t)    = sum(g, sum(h, FD1(r,g,h,t,"%POL%%CTX%")) + FD1(r,g,"GOV",t,"%POL%%CTX%") +
                                            FD1(r,g,"INV_in",t,"%POL%%CTX%") + sum(ftr, FD1(r,g,ftr,t,"%POL%%CTX%")) ) ;

*   VALUE-ADD
    VA1(r,"KAPr",ext,t,"%POL%%CTX%")    = kd_r.l(r,ext,t)               ;
    VA1(r,"KAPn",s,t,"%POL%%CTX%")      = kd_n.l(r,s,t)                 ;
    VA1(r,"LAB",s,t,"%POL%%CTX%")       = ld_y.l(r,s,t)                 ;
    VA1(r,"LAB" ,k_egn,t,"%POL%%CTX%")  = sum(etc$etmap(etc,k_egn), ld_ye.l(r,etc,t))           ;
    VA1(r,"KAPn",k_egn,t,"%POL%%CTX%")  = sum(etc$etmap(etc,k_egn), kd_nele.l(r,etc,k_egn,t))   ;
    VA2(r,"KAPn",etc,t,"%POL%%CTX%")    = sum(k_egn$etmap(etc,k_egn), kd_nele.l(r,etc,k_egn,t)) ;

*   INTERMEDIATE DEMAND
    ID1(r,g,s,t,"%POL%%CTX%")           = ID_y.l(r,g,s,t)               ;
    ID1(r,g,"EGN",t,"%POL%%CTX%")       = sum(etc, ID_ye.l(r,g,etc,t))  ;

*   PRICES
    Px(r,h,t,"%POL%%CTX%")              = pc.l(r,h,t)  / pnum.l(t)      ;
    Px(r,s,t,"%POL%%CTX%")              = Pa.l(r,s,t)  / pnum.l(t)      ;
    Px(r,etc,t,"%POL%%CTX%")            = pye.l(r,etc,t)/ pnum.l(t)     ;
    Px(r,"LAB",t,"%POL%%CTX%")          = pl.l(r,t)    / pnum.l(t)      ;
    Px(r,"GHG",t,"%POL%%CTX%")          = pghge.l(t)   / pnum.l(t)      ;

    Px("KAPp",s,t,"%POL%%CTX%")$(not egn(s)) = pk.l(s,t)/ pnum.l(t)     ;
    Px("KAPp",k_egn,t,"%POL%%CTX%")     = pke.l(k_egn,t)/ pnum.l(t)     ;

    Px("KAP",s,t,"%POL%%CTX%")          = rk.l(s,t)     / pnum.l(t)     ;
    Px("KAP",k_egn,t,"%POL%%CTX%")      = rke.l(k_egn,t)/ pnum.l(t)     ;
    Px("KAP",ext,"nrs","%POL%%CTX%")    = rkr0.l(ext) ;

*   OUTPUT
    Y1(r,s,t,"%POL%%CTX%")              = Y.l(r,s,t) *   y0(r,s,t) * tfp_oth(r,s,t)    ;
    Y1(r,"ELE",t,"%POL%%CTX%")          = sum(etc, yq0(r,etc,t) ) * y.l(r,"EGN",t)     ;

    Y1(r,"egn_BIO",t,"%POL%%CTX%")      = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc_BIO, Ye.l(r,etc_BIO,t) * Yq0(r,etc_BIO,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y1(r,"egn_COL",t,"%POL%%CTX%")      = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc_COL, Ye.l(r,etc_COL,t) * Yq0(r,etc_COL,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y1(r,"egn_GAS",t,"%POL%%CTX%")      = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc_GAS, Ye.l(r,etc_GAS,t) * Yq0(r,etc_GAS,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y1(r,"egn_GEO",t,"%POL%%CTX%")      = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc_GEO, Ye.l(r,etc_GEO,t) * Yq0(r,etc_GEO,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y1(r,"egn_NUC",t,"%POL%%CTX%")      = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc_NUC, Ye.l(r,etc_NUC,t) * Yq0(r,etc_NUC,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y1(r,"egn_OIL",t,"%POL%%CTX%")      = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc_OIL, Ye.l(r,etc_OIL,t) * Yq0(r,etc_OIL,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y1(r,"egn_OTH",t,"%POL%%CTX%")      = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc_OTH, Ye.l(r,etc_OTH,t) * Yq0(r,etc_OTH,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y1(r,"egn_SOL",t,"%POL%%CTX%")      = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc_SOL, Ye.l(r,etc_SOL,t) * Yq0(r,etc_SOL,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y1(r,"egn_WAT",t,"%POL%%CTX%")      = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc_WAT, Ye.l(r,etc_WAT,t) * Yq0(r,etc_WAT,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y1(r,"egn_WND",t,"%POL%%CTX%")      = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc_WND, Ye.l(r,etc_WND,t) * Yq0(r,etc_WND,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;

    Y2(r,"ldg","l1",t,"%POL%%CTX%")     = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc$etc_l1(etc), Ye.l(r,etc,t) * Yq0(r,etc,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y2(r,"ldg","l2",t,"%POL%%CTX%")     = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc$etc_l2(etc), Ye.l(r,etc,t) * Yq0(r,etc,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y2(r,"ldg","l3",t,"%POL%%CTX%")     = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc$etc_l3(etc), Ye.l(r,etc,t) * Yq0(r,etc,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y2(r,"ldg","l4",t,"%POL%%CTX%")     = Y1(r,"ELE",t,"%POL%%CTX%") * sum(etc$etc_l4(etc), Ye.l(r,etc,t) * Yq0(r,etc,t)) / sum(etc, Ye.l(r,etc,t) * Yq0(r,etc,t)) ;
    Y2(r,etc, "all",t,"%POL%%CTX%")     = Y1(r,"ELE",t,"%POL%%CTX%") *                      Ye.l(r,etc,t) * Yq0(r,etc,t)  / sum(etc_,Ye.l(r,etc_,t)* Yq0(r,etc_,t));
    Y2(r,etc, "ally",t,"%POL%%CTX%")    = Ye.l(r,etc,t) ;

*   POLICY & environment measures
    PLCY("abt_lev","GHG",t)     = cabt.l(t)        * sum((r,cgo), GHGI(r,cgo,"DOM",t) ) ;
    PLCY("abt_lev",pcrt,t)$pcp  = cpabt.l(pcrt,t)  * sum(r,   egn_emt(r,"TOT",pcrt,t) ) ;
    PLCY("abt_pct","GHG",t)     = cabt.l(t)             ;
    PLCY("abt_pct",pcrt,t)$pcp  = cpabt.l(pcrt,t)       ;
    PLCY("cabt_px","GHG",t)     = pghge.l(t)            ;
    PLCY("cabt_px",pcrt,t)$pcp  = pcrte.l(pcrt,t)       ;

    PLCY("CES_shr","pct",t)     = sum((r,CET), ye.l(r,CET,t) * yq0(r,CET,t) * CEC(CET,t)) / 
                                    sum((r,etc), ye.l(r,etc,t) * yq0(r,etc,t))              ;
    PLCY("CEC_bnk","ann",t)     = sum(r, cec_bank.l(r,t)) ;
    PLCY("CEC_bnk","cum",t)     = sum((r,tt)$(tt.val < t.val), cec_bank.l(r,t)) ;
*   UNITS: 0.278 MM Mwh per EJ

    ENV(r,"egn_COL",plt,t) = sum(etc_COL, egn_emt(r,etc_COL,plt,t) * Ye.l(r,etc_COL,t));
    ENV(r,"egn_GAS",plt,t) = sum(etc_GAS, egn_emt(r,etc_GAS,plt,t) * Ye.l(r,etc_GAS,t));
    ENV(r,"egn_OIL",plt,t) = sum(etc_OIL, egn_emt(r,etc_OIL,plt,t) * Ye.l(r,etc_OIL,t));
    ENV(r,"egn_BIO",plt,t) = sum(etc_BIO, egn_emt(r,etc_BIO,plt,t) * Ye.l(r,etc_BIO,t));
    ENV(r,"egn_TOT",plt,t) = sum(etc,     egn_emt(r,etc,plt,t)     * Ye.l(r,etc,t))    ;
    ENV(r,"TOT","GHG",t)   = (1-cabt.l(t))            * sum(cgo, GHGI(r,cgo,"DOM",t) ) ;

    ENV(r,cgo,"CO2",t)  = a.l(r,cgo,t) * GHGI(r,cgo,"DOM",t) * 1e9;
*   -- BACK OUT & REPLACE ELE EMITS
    ENV(r,cgo,"CO2",t)  = ENV(r,cgo,"CO2",t) * (1 - ID1(r,cgo,"EGN",t,"%POL%%CTX%") / (a.l(r,cgo,t) * a0(r,cgo,t))) ;

    ENV(r,"COL","CO2",t)= ENV(r,"COL","CO2",t) + ENV(r,"egn_COL","CO2",t);
    ENV(r,"GAS","CO2",t)= ENV(r,"GAS","CO2",t) + ENV(r,"egn_GAS","CO2",t);
    ENV(r,"OIL","CO2",t)= ENV(r,"OIL","CO2",t) + ENV(r,"egn_OIL","CO2",t);
    ENV(r,"TOT","CO2",t)= sum(cgo, ENV(r,cgo,"CO2",t))  ;

    NRG(r,cgo,t)        = a.l(r,cgo,t) * a0(r,cgo,t)    ;
*   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *


