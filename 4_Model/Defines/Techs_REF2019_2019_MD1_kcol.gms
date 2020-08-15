* -----------------------------------------------------
* XLS2GMS 2.8      24.4.6 r52609 Released Jun 26, 2015 VS8 x86 32bit/MS Windows 
* Erwin Kalvelagen, GAMS Development Corp.
* -----------------------------------------------------
* Application: Microsoft Excel
* Version:     16.0
* Workbook:    R:\CGE_Modeling\2_Electricity\Out_x\AggModel_REF2019_2019_MD1.xlsx
* Sheet:       Sets_REF2019_2019
* Range:       $A$1:$A$25
* -----------------------------------------------------
Set etc   / T1*T64 /
    etc_BIO(etc)     generation from BIO source     / T1  * T2/
    etc_COL(etc)     generation from COL source     / T3  * T33/
    etc_GAS(etc)     generation from GAS source     / T34 * T41/
    etc_GEO(etc)     generation from GEO source     / T42 * T44/
    etc_NUC(etc)     generation from NUC source     / T45 * T46/
    etc_OIL(etc)     generation from OIL source     / T47 * T49/
    etc_OTH(etc)     generation from OTH source     / T50 * T52/
    etc_SOL(etc)     generation from SOL source     / T53 * T56/
    etc_WAT(etc)     generation from WAT source     / T57 * T60/
    etc_WND(etc)     generation from WND source     / T61 * T64/
*    k_egn     Existing capital group for EGN  /  k_BIO, T3 * T33, k_GAS, k_GEO, k_NUC, k_OIL, k_OTH, k_SOL, k_WAT, k_WND /
    k_egn     Existing capital group for EGN  / T1*T2, T3*T33, k_GAS, T42*T44, T45, T46, k_OIL, k_OTH, k_SOL, T57*T60, k_WND /

     etc_L1(etc)     generation load 1  /  T38,  T46,  T57,  T58,  T59,  T60,  T61,  T63 /
     etc_L2(etc)     generation load 2  /  T15,  T16,  T18,  T19,  T22,  T23,  T25,  T27,  T28,  T29,  T30,  T31,  T33,  T35,  T36,  T37,  T42,  T44, T43,  T45,  T46,  T53,  T54,  T55,  T62,  T63,  T64,  T7 /
     etc_L3(etc)     generation load 3  /  T1,  T10,  T11,  T12,  T13,  T14,  T16,  T17,  T18,  T2,  T20,  T21,  T22,  T24,  T25,  T26,  T3,  T32,  T34,  T39,  T4,  T40,  T41,  T5,  T52,  T6,  T8,  T9 /  
     etc_L4(etc)     generation load 4  /  T39,  T47,  T48,  T49,  T50,  T51,  T56 /    ;
* -----------------------------------------------------
