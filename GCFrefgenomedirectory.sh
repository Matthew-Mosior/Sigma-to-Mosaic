echo "This shell script will create directories for each unique read."
for var in "$@"
do
	cd "$var"
	for file in GCF*
	do
		echo "Performing directory creation for $file"
		mkdir "$file"_directory
		echo "Moving $file to correct directory:"
		mv $file "$file"_directory
	done
done
echo "finished"	
