# The array to use 
ARRAY = "2 5 9 8 1 4 6 7 0 3"
# The filename of the file to put the list of lists
LLFNAME = "svsequential4.txt"
# The name of the output weave file
WEAVEFNAME = "svseqbright4.png"
# The name of the output json file
JSONNAME = "qsortoutput3.json"
# The final html file output name
FINALNAME = "testmake3.html"

compiledemo: qsortclocked.hs
	ghc qsortclocked.hs

dumpjson: compiledemo
	rm -f $(JSONNAME)
	./qsortclocked $(LLFNAME) $(ARRAY) >> $(JSONNAME)

brightweave: dumpjson
	../../contrib/sortvis/sortvis weave -F $(LLFNAME) --line-width=10 -x 720 -y 240 --background=eeeeee --gradient-end=393b79 --gradient-start=dadaeb -r -o $(WEAVEFNAME)

# Note not dependent on making the weave because the shell script does that internally
treevis: dumpjson
	rm -f $(FINALNAME)
	./heretree.sh $(JSONNAME) $(LLFNAME) >> $(FINALNAME)

