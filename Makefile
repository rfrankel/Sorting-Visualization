# The array to use 
ARRAY = "2 5 9 8 1 4 6 7 0 3"
# The filename of the file to put the list of lists
LLSEQFNAME = "svsequential4.txt"
# The filename of the file to put the list of lists
LLPARFNAME = "svparallel4.txt"
# The name of the output weave file, sequential
WEAVESEQFNAME = "svseqbright4.png"
# The name of the output weave file, parallel
WEAVEPARFNAME = "svparbright4.png"
# The name of the output json file for a sequential sort
JSONSEQNAME = "qsortoutput3.json"
# The name of the output json file for a parallel sort
JSONPARNAME = "qsortparoutput3.json"
# The final html file output name
FINALSEQNAME = "testseqmake3.html"
# The final html file output name
FINALPARNAME = "testparmake3.html"

compiledemoseq: qsortclocked.hs
	cabal build qsortclocked

compiledemopar: qsortpar.hs
	cabal build qsortpar

dumpjsonseq: compiledemoseq
	rm -f $(JSONSEQNAME)
	./dist/build/qsortclocked/qsortclocked $(LLSEQFNAME) $(ARRAY) >> $(JSONSEQNAME)

dumpjsonpar: compiledemopar
	rm -f $(JSONPARNAME)
	./dist/build/qsortpar/qsortpar $(LLPARFNAME) $(ARRAY) >> $(JSONPARNAME)

brightweaveseq: dumpjsonseq
	../../contrib/sortvis/sortvis weave -F $(LLSEQFNAME) --line-width=10 -x 720 -y 240 --background=eeeeee --gradient-end=393b79 --gradient-start=dadaeb -r -o $(WEAVESEQFNAME)

brightweavepar: dumpjsonpar
	../../contrib/sortvis/sortvis weave -F $(LLPARFNAME) --line-width=10 -x 720 -y 240 --background=eeeeee --gradient-end=393b79 --gradient-start=dadaeb -r -o $(WEAVEPARFNAME)


# Note not dependent on making the weave because the shell script does that internally
treevisseq: dumpjsonseq
	rm -f $(FINALSEQNAME)
	./heretree.sh $(JSONSEQNAME) $(LLSEQFNAME) >> $(FINALSEQNAME)

treevispar: dumpjsonpar
	rm -f $(FINALPARNAME)
	./heretree.sh $(JSONPARNAME) $(LLPARFNAME) >> $(FINALPARNAME)

