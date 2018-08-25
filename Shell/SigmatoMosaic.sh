echo "This shell script converts sigma_out.gvector.txt GCF identifiers to organism specific taxonomy ids (Taxids) with corresponding relative abundances."
sleep 5
filecount=1

##Perform incorrect file format detection.
for var in "$@"
do
        if ! head -1 "$var" | egrep -q '^#\s\+\sGvectorName\sTotalNumberReads\sNumberMappedReads\sNumberUnmappedReads$' 
	then
		echo "Incorrect file format detected.  Please input correct sigma.out_gvector.txt file(s)."
		echo "This program is now terminating."
		exit 1
	fi	
done

##Start shell scipt.
for var in "$@"
do
echo "Running "$var"."
sleep 5
cat "$var" | egrep '^\*' | sed 's/^.\s//' | sed 's/\s.*//' > relabunsubscripts.txt
while read lines
do
	subscript=$(echo "$lines")
	TrueGCF=$(cat $var | sed 's/@//1' | sed 's/*//1' | sed 's/\s//' | egrep ^$subscript | tr '\n' ' ' | sed 's/^.\s//' | sed 's/ [0-9].*[ ].*$//' | sed 's/\s//' | sed 's/[^A-Za-z]*$//' | egrep -o "GCF.*" | sed 's/ory.*/y/' | sed 's/_genomic.*//') 
	relabun=$(cat $var | sed 's/@//1' | sed 's/*//1' | sed 's/\s//' | egrep ^$subscript[^0-9] | tr '\n' ' ' | sed 's/^.\s//' | sed 's/^[^ ]* //' | sed 's/^.\s//' | sed 's/[0-9]*\.[0-9]*\s//1' | sed 's/ $//' | sed 's/^.*\s//')
	echo -n "$TrueGCF" > GCF_variables.txt
	echo -n "-" >> GCF_variables.txt
	echo "$relabun" >> GCF_variables.txt
	cat GCF_variables.txt | sed 's/\(.*\)-/\1 /' >> GCF_variables1.txt
	while read line
	do
		GCFonly=$(echo "$line" | sed 's/ .*$//')
		first=(${GCFonly:4:3})
		second=(${GCFonly:7:3})
		third=(${GCFonly:10:3})
		wget ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/"$first"/"$second"/"$third"/"$GCFonly"/"$GCFonly"_assembly_report.txt
		newtaxid=$(cat "$GCFonly"_assembly_report.txt | egrep 'Taxid' | sed 's/[^0-9]//g')
 		echo -n "$GCFonly" >> taxid.txt
		echo -n "-" >> taxid.txt
		echo -n "$newtaxid" >> taxid.txt
		if egrep -q "$GCFonly" GCF_variables1.txt
		then			
			relabunonly=$(echo "$line" | sed 's/^.* //')	
			echo -n "-" >> taxid.txt
			echo "$relabunonly" >> taxid.txt
			cat taxid.txt | rev | sed 's/-/ /' | sed 's/-/ /' | rev >> taxid_with_relabun.txt
			echo "$GCFonly corresponding relative abundance has been added to taxid_with_relabun.txt"  		
		fi
		rm taxid.txt
		rm "$GCFonly"_assembly_report.txt 
	done < GCF_variables1.txt
	while read line
	do
		GCFonlyy=$(echo "$line" |  sed 's/ .*$//') 
		GCFonlyy1=$(echo "$GCFonlyy" | sed 's/-/$/')
		taxidonly=$(echo "$line" | sed 's/^[^ ]*//' | sed 's/ //1' | sed 's/ .*$//')
		relabunonly1=$(echo "$line" |  sed 's/^.* //')
		echo "$GCFonlyy1" >> temp.txt
		wget "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy&id="$taxidonly"&mode=text&report=xml"
		filelength=$(pcregrep -M '^ *<Taxon>.*\n.*\n.*\n.*\n.*\n' efetch.fcgi\?db\=taxonomy\&id\="$taxidonly"\&mode\=text\&report\=xml | wc -l)
		increment=5
		start=10
		low=5
		for ((i=start ; i<=filelength ; i+=increment))
        	do
        		rank=$(pcregrep -M '^ *<Taxon>.*\n.*\n.*\n.*\n.*\n' efetch.fcgi\?db\=taxonomy\&id\="$taxidonly"\&mode\=text\&report\=xml | head -$i | tail -$low | tr '\n' ' ' | egrep -o '<Rank>.*<\/Rank>' | sed 's/<Rank>//' | sed 's/<\/Rank>//')
			if [ "$rank" != "no rank" ]
			then 
				pcregrep -M '^ *<Taxon>.*\n.*\n.*\n.*\n.*\n' efetch.fcgi\?db\=taxonomy\&id\="$taxidonly"\&mode\=text\&report\=xml | head -$i | tail -$low  | tr '\n' ' ' | egrep -o '<TaxId>.*<\/TaxId>' | sed 's/<TaxId>//' | sed 's/<\/TaxId>//' >> temp.txt	
			fi
        	done
		first=$(pcregrep -M '^ *<Taxon>.*\n.*\n.*\n.*\n.*\n' efetch.fcgi\?db\=taxonomy\&id\="$taxidonly"\&mode\=text\&report\=xml | head -5 | tr '\n' ' ' | egrep -o '<Rank>.*<\/Rank>' | sed 's/<Rank>//' | sed 's/<\/Rank>//')
		if [ "$first" != "no rank" ]
		then 
			pcregrep -M '^ *<Taxon>.*\n.*\n.*\n.*\n.*\n' efetch.fcgi\?db\=taxonomy\&id\="$taxidonly"\&mode\=text\&report\=xml | head -5 | tr '\n' ' ' | egrep -o '<TaxId>.*<\/TaxId>' | sed 's/<TaxId>//' | sed 's/<\/TaxId>//' >> temp.txt		
		fi

		echo "test" >> temp.txt
		echo "$relabunonly1" >> temp.txt
		cat temp.txt | tr '\n' ' ' >> mosaic_large.txt
		echo "" >> mosaic_large.txt	
		rm temp*
        	rm efetch.fcgi\?db\=taxonomy\&id\="$taxidonly"\&mode\=text\&report\=xml	
	done < taxid_with_relabun.txt
	rm taxid_with_relabun.txt
	rm GCF_variables.txt
	rm GCF_variables1.txt
done < relabunsubscripts.txt
echo "%" >> mosaic_large.txt
sleep 5
done

cat mosaic_large.txt | tr ' \t\n\r' '-' >> mosaic_large_new.txt
sed -i 's/--/\n/g' mosaic_large_new.txt
sed -i 's/-/,/g' mosaic_large_new.txt
sed -i 's/%,/%\n/' mosaic_large_new.txt
rm mosaic_large.txt
mv mosaic_large_new.txt mosaic_large.txt


#mosaic_large.txt has all GCFs with associated taxonomic ranks with relative abundances from all input files.
#Sort mosaic_large on species level taxids, compress to only uniq GCF species only taxid enumeration, and correctly enumerate.

cat mosaic_large.txt | sed 's/%//' | sed 's/,/- /' | rev | sed 's/,/-/' | sed 's/^[^-]*-//1' | sed 's/,/-/' | sed 's/,/- /' | rev | sed 's/ [^ ]* -//' | rev |  sed 's/-/\t/' | sed 's/-/\t/' | rev | sort -k2 | uniq | sed '/^\s*$/d' > mosaic_large_uniq.txt

cat mosaic_large_uniq.txt | rev | sed 's/^[^\t]*\t/1\t/' | rev > mosaic_large_uniq_new.txt
rm mosaic_large_uniq.txt
mv mosaic_large_uniq_new.txt mosaic_large_uniq.txt

cp mosaic_large_uniq.txt mosaic_large_uniq1.txt

linecounter=1
lastlinecounter=0
enumcounter=2

while read lines
do
        lastline=$(cat mosaic_large_uniq1.txt | head -$lastlinecounter | tail -1)
        lastlinetaxidonly=$(cat mosaic_large_uniq1.txt | head -$lastlinecounter | tail -1 | sed 's/\s/-/1' | sed 's/^.*-//' | sed 's/\s.*$//')
        lastlineenumonly=$(cat mosaic_large_uniq1.txt | head -$lastlinecounter | tail -1 | rev | sed 's/\s.*//')

        taxidonly=$(echo "$lines" |  sed 's/\s/-/1' | sed 's/^.*-//' | sed 's/\s.*$//')
        enumonly=$(echo "$lines" | rev | sed 's/\s.*//')

        if [ "$linecounter" -ne 1 ]
        then
                if [ "$lastlinetaxidonly" == "$taxidonly" ]
                then
                        sed -i "${linecounter}s/.$/"$enumcounter"/" mosaic_large_uniq.txt
                        ((enumcounter++))
                else
                        ((enumcounter=2))
                fi
        fi

        ((lastlinecounter++))
        ((linecounter++))

done < mosaic_large_uniq.txt


#Utilize mosaic_large_uniq.txt as a dictionary and update enumerations of mosaic_large.txt

linecounter1=1

while read lines
do
	originalGCF=$(echo "$lines" | sed 's/,/-/' | rev | sed 's/,/-/' | sed 's/^[^-]*-//1' | sed 's/,/-/' | sed 's/,/-/' | rev | sed 's/-[^-]*//' | sed 's/-/\t/' | sed 's/-/\t/' | sed 's/\s.*//')
	originalspecies=$(echo "$lines" | sed 's/,/-/' | rev | sed 's/,/-/' | sed 's/^[^-]*-//1' | sed 's/,/-/' | sed 's/,/-/' | rev | sed 's/-[^-]*//' | sed 's/-/\t/' | sed 's/\t/-/g' | sed 's/^[^-]*-//' | sed 's/-.*$//')	
	
	while read line
	do
		uniqGCF=$(echo "$line" | sed 's/\t/-/g' | sed 's/-.*$//')
		uniqspecies=$(echo "$line" | sed 's/\t/-/g' | sed 's/^[^-]*-//' | sed 's/-.*$//')	
		uniqenum=$(echo "$line" | sed 's/\t/-/g' | sed 's/^.*-//')
		
		if [ "$originalGCF" == "$uniqGCF" ]
		then
			sed -i "${linecounter1}s/test/"$uniqenum"/" mosaic_large.txt
		fi


	done < mosaic_large_uniq.txt
		
((linecounter1++))

done < mosaic_large.txt


#After updating enumeration, remove GCF.

cat mosaic_large.txt | sed 's/^[^,]*,//' | sed 's/^t[^,]*,//' > mosaic_large_withoutGCF.txt
cat mosaic_large_withoutGCF.txt | sed 's/%//' | sed '/^\s*$/d' | rev | sed 's/^[^,]*,//' | rev | sort -n | uniq > mosaic_large_withoutGCF_withoutrelabun_uniq.txt


#Compress species specific enumeration.

linecounter2=1
dotcounter=0

while read lines
do
	if ! [[ "$lines"  == *"%"* ]]
	then
		uncompressedspecies=$(echo "$lines" | rev | sed 's/^[^,]*,//' | rev)
               	uncompressedrelabun=$(echo "$lines" |  rev | sed 's/,.*$//' | rev)
              	uncompressedraw=$(echo "$uncompressedrelabun" / 100 | bc -l | sed 's/^/0/')
	
		while read line
		do
			compressedspecies=$(echo "$line" | sed 's/\s.*//')

			if [ "$compressedspecies" == "$uncompressedspecies" ]
			then
				sed -i "${linecounter2}s/$/\\t"$uncompressedraw"/" mosaic_large_withoutGCF_withoutrelabun_uniq.txt
			fi
		
			((linecounter2++))

		done < mosaic_large_withoutGCF_withoutrelabun_uniq.txt
	else
                ((dotcounter++))
                sed -i "/\([^.]*\.\)\{"$dotcounter"\}/! s/$/\t0.00000000000000000000/" mosaic_large_withoutGCF_withoutrelabun_uniq.txt
        fi

	linecounter2=1

done < mosaic_large_withoutGCF.txt


#Add header.

sed -i '1s/^/\t\t\n/' mosaic_large_withoutGCF_withoutrelabun_uniq.txt

for var in "$@"
do
	sed -i "1s/$/\\t"$var"/" mosaic_large_withoutGCF_withoutrelabun_uniq.txt
done

sed -i '1s/^/FullTaxonomicRank\t/' mosaic_large_withoutGCF_withoutrelabun_uniq.txt


#Clean up and return final file.

mv mosaic_large_withoutGCF_withoutrelabun_uniq.txt final.txt
cat final.txt | column -t > final1.txt
rm relabunsubscripts.txt
rm final.txt
rm mosaic*
mv final1.txt mosaic.txt
echo "Please check mosaic.txt for the mosaic-specific output file."
echo "Finished."
