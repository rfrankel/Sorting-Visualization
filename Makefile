# The array to use 
# Edit these two variables to vary the array 
# and the subdirectory from which the gist/block will be created

ARRAY = "2 5 9 8 1 4 6 7 0 3"
# The subdirectory to make the gist for submission to bl.ocks.org
GISTDIR = tentwo

# ------------- Derived names; don't edit this --------------

# The filename of the file to put the list of lists
LLFNAME = "listoflists.txt"
# The name of the output weave file
WEAVEFNAME = "weave.png"
# The name of the output json file for a sequential sort
JSONNAME = "historytree.json"
# The final html file output name
FINALNAME = "index.html"


compiledemoseq: qsortclocked.hs
	cabal build qsortclocked

compiledemopar: qsortpar.hs
	cabal build qsortpar

dumpjsonseq: compiledemoseq        
	mkdir -p $(GISTDIR)/seq/
	cd $(GISTDIR)/seq/; rm -f $(JSONNAME); \
../../dist/build/qsortclocked/qsortclocked $(LLFNAME) $(ARRAY) >> $(JSONNAME)


dumpjsonpar: compiledemopar
	mkdir -p $(GISTDIR)/par/
	cd $(GISTDIR)/par/; rm -f $(JSONNAME); \
../../dist/build/qsortpar/qsortpar $(LLFNAME) $(ARRAY) >> $(JSONNAME)

brightweaveseq: dumpjsonseq
	cd $(GISTDIR)/seq/; \
../../../../contrib/sortvis/sortvis weave -F $(LLFNAME) --line-width=10 -x 720 -y 240 --background=eeeeee --gradient-end=393b79 --gradient-start=dadaeb -r -o $(WEAVEFNAME)


brightweavepar: dumpjsonpar
	cd $(GISTDIR)/par/; \
../../contrib/sortvis/sortvis weave -F $(LLFNAME) --line-width=10 -x 720 -y 240 --background=eeeeee --gradient-end=393b79 --gradient-start=dadaeb -r -o $(WEAVEFNAME)


# Note not dependent on making the weave because the shell script does that internally
treevisseq: dumpjsonseq
	cd $(GISTDIR)/seq/; rm -f $(FINALNAME); \
../../heretree.sh $(JSONNAME) $(LLFNAME) $(WEAVEFNAME) >> $(FINALNAME)
	@echo Now take screenshot with aspect ratio 1.91666.... 
	@echo Save it as thumb.orig.png and run \'make sizethumbseq\'

treevispar: dumpjsonpar
	cd $(GISTDIR)/par/; rm -f $(FINALNAME); \
../../heretree.sh $(JSONNAME) $(LLFNAME) $(WEAVEFNAME) >> $(FINALNAME)
	@echo Now take screenshot with aspect ratio 1.91666.... 
	@echo Save it as thumb.orig.png and run \'make sizethumbpar\'

# This function will only shrink a larger image, not grow a smaller one
# and they resize to the smallest fitting dimension, and then clip the rest off.
# So if the aspect ratio is not as directed above, stuff will be clipped.
sizethumbseq:
	cd $(GISTDIR)/seq/; convert thumb.orig.png -resize 230x120^ \
 -gravity center -extent 230x120  thumbnail.png 


sizethumbpar:
	cd $(GISTDIR)/par/;  convert thumb.orig.png -resize 230x120^ \
 -gravity center -extent 230x120  thumbnail.png 
