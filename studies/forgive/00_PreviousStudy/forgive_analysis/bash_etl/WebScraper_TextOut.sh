#!/bin/bash
# Resource: https://www.joyofdata.de/blog/using-linux-shell-web-scraping/
# Resource (wget): https://unix.stackexchange.com/questions/410493/stop-wget-reusing-existing-connection
# Data: https://www.blueletterbible.org/lang/lexicon/lexicon.cfm?page=1&strongs=H5375&ss=1#lexResults

ArrayStrongs=( H3722 H5375 H5545 H5546 H5547 G859 G863 G5483 )

for i in ${ArrayStrongs[@]}; do
	for j in {1..15}; do

		 echo "https://www.blueletterbible.org/lang/lexicon/lexicon.cfm?page=$j&strongs=$i&ss=1#lexResults" |
		 	wget --max-redirect 0 --no-http-keep-alive --no-cache --no-cookies	--header="Accept: text/html" \
		 		--user-agent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:21.0) Gecko/20100101 Firefox/21.0" \
		 		-O- -i- |
		 	hxnormalize -x |
		 	hxselect -i "div.bibleTable" |
		 	lynx -stdin -dump -nolist -width=1024 |
		 	grep "Unchecked Copy Box " |
		 	sed 's/   Unchecked Copy Box //g' |
			sed 's/ - /,/g' |
			sed 's/\([HG][0-9]\{3,4\}\),\([A-Za-z]\{3\}\) \([0-9]\{1,2\}\):\([0-9]\{1,2\}\),\(.*$\)/"\1","\2",\3,\4,"\5"/g' |
			sed "s/^/${i} - /"  >> ../data/TextOutput.csv

	done
done

sed -i '1 i\Strongs,Book,Chapter,Verse,Text' ../data/TextOutput.csv
