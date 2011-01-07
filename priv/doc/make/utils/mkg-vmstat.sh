#!/bin/sh
#

##
## Copyright (c) 2005-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http:##www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##

IN_DATA=$1
OUT_DATA=$2

cat <<'EOD' |
#proc getdata
file: -
filter:
 ##set sum1 = $arith(@@15+@@16+@@18)
 ##set sum2 = $arith(@@15+@@16)
 ##print @2 @@sum1 @@sum2 @16

#proc areadef
title: CPU Usage (<_IN_DATA_>)
titledetails: size=12 align=C color=black style=R adjust=0,0.3
rectangle: 0 0 6 4
xautorange: datafield=1
//xautorange: datafield=1 nearest=10second
xscaletype: time hh:mm
yrange: 0 100

#proc xaxis
stubs: datematic
label: Time

#proc yaxis
stubs: inc 10
label: CPU Usage (%)
labeldetails: adjust=-0.2,0

#proc lineplot
xfield: 1
yfield: 2
fill: red
legendlabel: wait

#proc lineplot
xfield: 1
yfield: 3
fill: blue
legendlabel: user

#proc lineplot
xfield: 1
yfield: 4
fill: green
legendlabel: sys

#proc legend
location: max max
EOD
    sed -e 's!<_IN_DATA_>!'"$IN_DATA"'!g' \
        > tmp-$$.plot


if [ "$COMSPEC" != "" ]; then
    PLOTICUS=ploticus
else
    PLOTICUS=pl
fi
egrep -v "i|#" $IN_DATA |
    $PLOTICUS -svg -o $OUT_DATA tmp-$$.plot


rm -f tmp-$$.plot
