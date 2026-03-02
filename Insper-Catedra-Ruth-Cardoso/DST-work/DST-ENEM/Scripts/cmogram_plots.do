
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
	
	
	
* dmedia
cmogram dmedia dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-10 15)) ///
        ylabel(-10(5)15) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_cmogram.png", replace width(1600)


* dmedia_rd
cmogram dmedia_rd dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(45 105)) ///
        ylabel(45(15)105) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_rd_cmogram.png", replace width(1600)

* dmedia_cn
cmogram dmedia_cn dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-25 -8)) ///
        ylabel(-25(5)-5) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_cn_cmogram.png", replace width(1600)


* dmedia_ch
cmogram dmedia_ch dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-65 -45)) ///
        ylabel(-65(5)-45) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_ch_cmogram.png", replace width(1600)


* dmedia_lc
cmogram dmedia_lc dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-10 10)) ///
        ylabel(-10(5)10) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_lc_cmogram.png", replace width(1600)


* dmedia_mt
cmogram dmedia_mt dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-20 10)) ///
        ylabel(-20(5)10) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_mt_cmogram.png", replace width(1600)


* dmedia_d1
cmogram dmedia_d1 dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-10 20)) ///
        ylabel(-5(5)20) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_d1_cmogram.png", replace width(1600)



* dmedia_d2
cmogram dmedia_d2 dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-20 0)) ///
        ylabel(-20(5)0) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_d2_cmogram.png", replace width(1600)



* dmedia_easy
cmogram dmedia_easy dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(0 10)) ///
        ylabel(0(5)10) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_easy_cmogram.png", replace width(1600)



* dmedia_hard
cmogram dmedia_hard dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-5 5)) ///
        ylabel(-5(5)5) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_hard_cmogram.png", replace width(1600)



* dmedia_escm0
cmogram dmedia_escm0 dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-18 20)) ///
        ylabel(-20(10)20) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_escm0_cmogram.png", replace width(1600)


* dmedia_escm1
cmogram dmedia_escm1 dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-20 20)) ///
        ylabel(-20(10)20) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_escm1_cmogram.png", replace width(1600)


* dmedia_bra1
cmogram dmedia_bra1 dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-20 20)) ///
        ylabel(-20(10)20) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_bra1_cmogram.png", replace width(1600)


* dmedia_bra0
cmogram dmedia_bra0 dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(-20 10)) ///
        ylabel(-20(10)10) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_bra0_cmogram.png", replace width(1600)

* dmedia_2018
cmogram dmedia_2018 dist_km, cut(0) scatter low lineat(0) ///
    graphopts1(mcolor("230 97 97")) ///
    graphopts2(mcolor("0 150 160")) ///
    lowopts1(lcolor("200 40 40") lwidth(medthick)) ///
    lowopts2(lcolor("0 90 130") lwidth(medthick)) ///
    graphopts( ///
        yscale(range(0 20)) ///
        ylabel(0(5)20) ///
        xtitle("Distance to Border (km)") ///
        ytitle("Average ENEM Score") ///
        graphregion(fcolor(white)) ///
    )

graph export "graphs_cmogram/dmedia_2018_cmogram.png", replace width(1600)

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
