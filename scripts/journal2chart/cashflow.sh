#!/bin/sh
# a

ledger -f targets/all.journal -J reg "$@" ^income -M --collapse --plot-total-format="%(format_date(date, \"%Y-%m-%d\")) %(abs(quantity(scrub(display_total))))\n" > ledgeroutput1.tmp
ledger -f targets/all.journal -J reg "$@" ^expenses -M --collapse > ledgeroutput2.tmp

(cat <<EOF) | gnuplot
  set terminal svg size 1280,720
  set xdata time
  set timefmt "%Y-%m-%d"
  # for the last year
  # set xrange ["$(date --date='last year' +%Y)-12-20":"$(date +%Y)-12-10"]
  # set xtics nomirror "$(date +%Y)-01-01",2592000 format "%b" rotate by -45
  set xtics nomirror scale 0 center rotate by -45
  unset mxtics
  set mytics 2
  set grid xtics ytics mytics
  set title "Cashflow"
  set ylabel "Accumulative Income and Expenses"
  set style fill transparent solid 0.6 noborder
  plot "ledgeroutput1.tmp" using 1:2 with filledcurves x1 title "income" linecolor rgb "light-salmon", '' using 1:2:2 with labels font "Courier,8" offset 0,0.5 textcolor linestyle 0 notitle, "ledgeroutput2.tmp" using 1:2 with filledcurves y1=0 title "expenses" linecolor rgb "seagreen", '' using 1:2:2 with labels font "Courier,8" offset 0,0.5 textcolor linestyle 0 notitle
EOF

rm ledgeroutput*.tmp
