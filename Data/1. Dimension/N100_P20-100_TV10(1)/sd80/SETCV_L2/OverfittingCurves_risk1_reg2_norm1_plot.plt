set key on
set size square 1.0,1.0
set lmargin 1
set bmargin 5
set linestyle 1 lt 1 lw 2
set linestyle 2 lt 2 lw 2
set nologscale x
set nologscale y
set xrange [0.000000:81.000000]
set yrange [0.500000:1.000000]
set title "prediction accuracy, cutoff=2.526022 (risk=1, reg=2, norm=1)"
set xlabel "nr of covariates"
set ylabel ""
set terminal postscript eps color
set output "OverfittingCurves_risk1_reg2_norm1.eps"
plot "OverfittingCurves_risk1_reg2_norm1_data1.txt" using 1:2 title "training set" with lines ls 1,"OverfittingCurves_risk1_reg2_norm1_data2.txt" using 1:2 title "validation set" with lines ls 2
