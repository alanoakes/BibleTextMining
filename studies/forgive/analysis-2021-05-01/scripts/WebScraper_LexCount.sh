#!/bin/bash

ArrayStrongs=( H3722 H5375 H5545 H5546 H5547 G859 G863 G5483 )

for i in ${ArrayStrongs[@]}; do
	echo "https://www.blueletterbible.org/lang/lexicon/lexicon.cfm?strongs=$i" |
		wget --max-redirect 0 --no-http-keep-alive --no-cache --no-cookies  --header="Accept: text/html" \
			--user-agent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:21.0) Gecko/20100101 Firefox/21.0" \
			-O- -i- |
		hxnormalize -x |
		hxselect -i "div#lexCount" |
		lynx -stdin -dump -nolist -width=1024 |
		sed '1d' | 
		sed 's/: /\n/g' | 
		sed '1d' | 
		sed 's/x), /\n/g' | 
		sed 's/x)\.//g' | 
		sed 's/ (/,/g' |
		sed "s/^/${i},/" >> ../data/LexCountOut.csv
done

# file prep for analysis
sort -t , -k 2 LexCountOut.csv >> ../data/LexCountOut.csv
sed -i '1 i\source,target,value' ../data/LexCountOut.csv 
