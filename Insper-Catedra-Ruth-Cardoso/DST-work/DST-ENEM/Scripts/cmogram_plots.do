
global path_base "C:\Users\tuffyli\OneDrive - Insper\Dados_HV"

cd "$path_base

use "C:\Users\tuffyli\OneDrive - Insper\Dados_HV\dados_cmogram.dta", clear


cmogram dmedia dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )
	
cmogram dmedia_rd dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

	
cap mkdir "graphs_cmogram"

foreach var in dmedia dmedia_rd dmedia_cn dmedia_ch dmedia_lc ///
               dmedia_mt dmedia_d1 dmedia_d2 dmedia_easy ///
               dmedia_hard dmedia_escm0 dmedia_escm1 ///
               dmedia_bra1 dmedia_bra0 dmedia_2018 {

    cmogram `var' dist_km, ///
        cut(0) scatter low lineat(0) ///
        graphopts1(mcolor("230 97 97")) ///
        graphopts2(mcolor("0 150 160")) ///
        lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
        lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
        graphopts( ///
            xtitle("Distance to Border (km)") ///
            ytitle("Average ENEM Score") ///
            graphregion(fcolor(white)) ///
        )

    graph export "graphs_cmogram/`var'_cmogram.png", replace width(1600)
}
	
	
	
	
	
	
	

cap log close
/*
rdplot dm dist, c(0) weights(n) covs(lat lon seg) p(2)
rdbwselect dm dist , c(0) weights(n) p(2)
rdbwselect dm dist if seg==2, c(0) weights(n) p(2)
predict double yhatm2 if e(sample)
scatter score demvoteshare, msize(tiny) xline(0.5) xtitle("Democrat vote share") ///
ytitle("ADA score") || ///
line yhatm2 demvoteshare if democrat ==1, sort color(red) || ///
line yhatm2 demvoteshare if democrat ==0, sort color(red) legend(off) ///
title("Quadratic")
graph export lee_xc2.png, replace

predict double yhatm2_w if e(sample)
scatter score demvoteshare, msize(tiny) xline(0.5 0.4 0.6) xtitle("Democrat vote share") ///
ytitle("ADA score") || ///
line yhatm2_w`' demvoteshare if democrat ==1, sort color(red) || ///
line yhatm2_w demvoteshare if democrat ==0, sort color(red) legend(off) ///
title("Quadratic around window")
