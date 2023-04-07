#!/bin/sh

ledger -f targets/all.journal -J reg "$@" ^assets:bank -M --collapse > ledgeroutput1.tmp
# ledger -f ../years/all.journal -J reg ^Liabilities -M --collapse --plot-total-format="%(format_date(date, \"%Y-%m-%d\")) %(abs(quantity(scrub(display_total))))\n" > ledgeroutput2.tmp

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
  set title "Wealthgrow"
  set ylabel "Amount"
  set style fill transparent solid 0.6 noborder
  plot "ledgeroutput1.tmp" using 1:2 with filledcurves x1 title "Assets" linecolor rgb "goldenrod", '' using 1:2:2 with labels font "Courier,8" offset 0,0.5 textcolor linestyle 0 notitle #, "ledgeroutput2.tmp" using 1:2 with filledcurves y1=0 title "Liabilities" linecolor rgb "violet", '' using 1:2:2 with labels font "Courier,8" offset 0,0.5 textcolor linestyle 0 notitle
EOF

rm ledgeroutput*.tmp
