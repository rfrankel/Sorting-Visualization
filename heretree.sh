#!/bin/bash
# Another 'cat' here document, using parameter substitution.

# Try it with no command-line parameters,   ./scriptname
# Try it with one command-line parameter,   ./scriptname Mortimer
# Try it with one two-word quoted command-line parameter,
#                           ./scriptname "Mortimer Jones"

CMDLINEPARAM=2     #  Expect at least two command-line parameter.

if [ $# -ge $CMDLINEPARAM ]
then
  JSONFILE=$1          #  If more than two command-line param,
  LLFNAME=$2          #+ then just take the first two
else
  JSONFILE="qsortpivot.json"  #  Default, if no command-line parameter.
  LLFNAME="qtest.txt"
fi  

# Make png version of name 

LLNAME=${LLFNAME%.txt}
LLFLOPPIC=${LLNAME}flop.png
LLPIC=${LLNAME}.png

`../../contrib/sortvis/sortvis weave -F $LLFNAME --line-width=10 -x 720 -y 240 --background=eeeeee --gradient-end=393b79 --gradient-start=dadaeb -r -o $LLFLOPPIC`

`convert -flop $LLFLOPPIC $LLPIC`

HIGHTICK=`wc -l < $LLFNAME` 
NUMTICKS=`expr $HIGHTICK - 1`

cat <<Endofmessage
<!DOCTYPE html>
<meta charset="utf-8">
<head>
<link href='http://fonts.googleapis.com/css?family=Droid+Sans' rel='stylesheet' type='text/css'>
<style>

/* Weave Brush styles */

body {
  font: 10px sans-serif;
  shape-rendering: crispEdges;
}

.grid-background {
  fill: #ddd;
}

.grid line {
  stroke: #fff;
}

.grid .minor line {
  stroke-opacity: .5;
}

.grid text {
  display: none;
}

.axis line {
  stroke: #000;
}

.axis path,
.grid path {
  display: none;
}

.brush .extent {
  stroke: #000;
  fill-opacity: .125;
  shape-rendering: crispEdges;
}

.handle {
 stroke-width: 2.75px;
}

/* Tree styles */

.node {
  cursor: pointer;
}

.node circle {
  fill: #fff;
  stroke: steelblue;
  stroke-width: 2.75px;
}

.node text {
  font: 12px 'Droid Sans';sans-serif;
}

.link {
  fill: none;
  stroke: #ccc;
  stroke-width: 3px;
}

</style>
</head>
<body>
<script src="http://d3js.org/d3.v3.js"></script>
<script>

var tmargin = {top: 20, right: 150, bottom: 20, left: 100},
    twidth = 800 - tmargin.right - tmargin.left,
    theight = 800 - tmargin.top - tmargin.bottom;

var wmargin = {top: 20, right: 200, bottom: 20, left: 200},
    wwidth = 640 - wmargin.right - wmargin.left,
    wheight = 760 - wmargin.top - wmargin.bottom;

var x = d3.scale.linear()
    .range([0, wwidth]);

// Sortvis makes weave ends that are 2% of the total
var weavend = wheight*.02;
// Number of levels in this tree, and pixels per level
var numLevels = $NUMTICKS ;
var levelPix = (wheight-2*weavend)/numLevels;

// --- Appearance Settings ----

var lgCircSize = 10,
    smCircSize = 3.5,
    smCircStroke = "steelblue",
    smCircFill = "#fff",
    //lgCircStroke = "#7b4173",
    //lgCircFill = "#d39ed6", 
    lgCircStroke = "#17becf",
    lgCircFill = "#9edae5", 
    treeSqueezeFactor = 2;

var y = d3.scale.linear()
    .domain([0, numLevels])
    .range([weavend, wheight-weavend]);

var svg = d3.select("body").append("svg")
    .attr("width", twidth + tmargin.right + tmargin.left + wwidth + wmargin.right + wmargin.left)
    .attr("height", wheight + wmargin.top + wmargin.bottom + theight + tmargin.top + tmargin.bottom)
  .append("g")
    .attr("transform", "translate(" + (twidth + tmargin.right + tmargin.left + wmargin.left) + "," + wmargin.top + ")");

// -------------  Brushed Weave Pattern
	
var defs = svg.append('svg:defs')
defs.append('svg:pattern')
    .attr('id', 'weavepat')
    .attr('patternUnits', 'userSpaceOnUse')
    .attr('width', '240')
    .attr('height', '720')
    .append('svg:image')
    .attr('xlink:href', '$LLPIC')
    .attr('x', 0)
    .attr('y', 0)
    .attr('width', 240)
    .attr('height', 720);

svg.append("rect")
    .attr("class", "grid-background")
    .attr("width", wwidth)
    .attr("height", wheight)
    .style("fill", "url(#weavepat)")

svg.append("g")
    .attr("class", "grid")
    .call(d3.svg.axis().scale(y).orient("left").ticks(numLevels*2).tickSize(-wwidth))
  .selectAll(".tick")
    .data(y.ticks(numLevels), function(d) { return d; })
  .exit()
    .classed("minor", true);

svg.append("g")
    .attr("class", "axis")
    .call(d3.svg.axis().scale(y).orient("left").ticks(numLevels).tickSize(lgCircSize*2));

var brush = d3.svg.brush()
    .y(y)
    .extent([-.5, 0])
    .on("brush", brushed)
    .on("brushend", brushended);

var gBrush = svg.append("g")
    .attr("class", "brush")
    .call(brush);
gBrush.selectAll("rect")
    .attr("width", wwidth);

var handle3 = gBrush.append("rect")
    .attr("class", "handle")
    .attr("width", wwidth)
    .attr("fill", lgCircFill)
    .attr("stroke", lgCircStroke)
    .attr("y", y(0))
    .attr("height", lgCircSize/3);

var handle1 = gBrush.append("circle")
    .attr("class", "handle")
    .attr("r", lgCircSize)
    .attr("fill", lgCircFill)
    .attr("stroke", lgCircStroke)
    .attr("cy", y(0))
    .attr("transform", "translate( " + -1*lgCircSize/2 + " ,0 )");

var handle2 = gBrush.append("circle")
    .attr("class", "handle")
    .attr("r", lgCircSize)
    .attr("fill", lgCircFill)
    .attr("stroke", lgCircStroke)
    .attr("cy", y(0))
    .attr("transform", "translate(" + wwidth + ", 0 )");

// Undo the translations applied to our original SVG
//function deTransX(coord) {
// return coord - (twidth + tmargin.right + tmargin.left + wmargin.left) 
//}

//function deTransY(coord) {
// return coord - wmargin.top 
//}

var curveFunction = d3.svg.line()
    .x(function(d) { return d.x; })
    .y(function(d) { return d.y; })
    .interpolate("cardinal-open");

function brushed() {
  var extent0 = brush.extent(),
      extent1;

  // if dragging, preserve the width of the extent
  if (d3.event.mode === "move") {
    //console.log("Extent: %o",extent0);
    var d0 = d3.round(extent0[0]),
        d1 = d0 + Math.round((extent0[1] - extent0[0]));
    //console.log("Start of brush extent: %d",d0)
    extent1 = [d0, d1];
  }

  // (old way) otherwise, if resizing, round both dates
  else {
    //extent1 = extent0.map(function(n) { return d3.round(n,0)});
    //console.log("Extent: %o",extent1);

    // Make the width at least one
    var d1 = d3.round(extent0[1],0),
        d0 = d3.round(extent0[0],0);
    if (d0 >= d1) 
       d0 = d1 - 1;
    if (d0 < d1 && (d1-d0) > 1)
       d1 = d0 + 1
    extent1 = [d0, d1];
  }

  handle1.attr("cy", y(extent1[1]));
  handle2.attr("cy", y(extent1[1]));
  handle3.attr("y", y(extent1[1]));
  d3.select(this).call(brush.extent(extent1)); 
}

function brushended() {
  var extent = brush.extent(),

  coloredNodes = new Array();
  root.children.forEach(function(d) {colorOnTick(extent[1],d,coloredNodes);});
  //console.log("coloredNodes is %O",coloredNodes);
 update(root);

    var curvedata = coloredNodes.map(function(d) { return { "x": d.x, "y": d.y}});
    // repeating first & last point because cardinal-open seems to need it.
    // (last not repeated exactly to make a smoother glide in)
    curvedata.unshift({"x":coloredNodes[0].x,"y":coloredNodes[0].y});
    curvedata.push({"x":twidth + wmargin.left + lgCircSize*3,"y":y(extent[1])-tmargin.top});
    curvedata.push({"x":twidth + wmargin.left + wwidth,"y":y(extent[1])-tmargin.top});

    console.log("curvedata is %O",curvedata);
    //console.log("Curve is %s",curveFunction(curvedata));

    ansPath = treegr.append("path")
            .attr("class","line")
            .attr("d",curveFunction(curvedata))     
            .attr("stroke",lgCircStroke)
            .attr("fill","none")  
            .attr("stroke-width",5)
            .attr("stroke-opacity",0.1);
    
    oldPath.transition()
	.duration(duration)
	.attr("stroke-opacity",1e-6)
	.remove();     

    oldPath = ansPath;
}


function colorOnTick(tck,d,coloredNodes) {
      //console.log("Considering node %O", d);
      if (d.Pp && d.tick <= tck)  {
        //console.log("Pivot -- at tick level %d", tck);
        d.radius = lgCircSize;
        d.fillstyle = lgCircFill;
        d.strokestyle = lgCircStroke;
          coloredNodes.push(d);
       } else
         if (d.tick >= tck && d.parent.tick < tck && d.name != "{}") {
         //console.log("Parent tick %d less than tck %d", d.parent.tick, tck);
         //console.log("NonPivot: at tick level %d", (d.parent.tick + 1));
         d.radius = lgCircSize;
         d.fillstyle = lgCircFill;
         d.strokestyle = lgCircStroke;
         coloredNodes.push(d);
      } else {
        //console.log("Normal node: at tick level %d", d.tick);
         d.radius = smCircSize;
         d.fillstyle = smCircFill;
         d.strokestyle = smCircStroke;
      }

    if (d.children) {
	d.children.forEach(function(d) {colorOnTick(tck,d,coloredNodes);});
    }
  }


// ----------- Tree -------------------

var i = 0,
    duration = 350, // changed from 750
    root;

var tree = d3.layout.tree()
    .size([theight, twidth])
    .separation(function separation(a, b) {
    if (a.parent == b.parent) 
     if ((a.tick - a.depth) > 1) 
      return ((a.tick - a.parent.tick)/treeSqueezeFactor)
     else if ((b.tick - b.depth) > 1)
      return ((b.tick - b.parent.tick)/treeSqueezeFactor) 
     else 
      return 1
    else 
     return 2
    }
);

// Old separation function
//    if ((a.parent == b.parent) && (((a.tick - a.depth) > 0) || ((b.tick - b.depth) > 0)))

var diagonal = d3.svg.diagonal()
    .projection(function(d) { return [d.x, d.y]; });

var treegr = svg.append("g")
    .attr("transform", "translate(" + -1*(tmargin.right + wwidth + wmargin.right + wmargin.left) + "," + weavend + ")");

// setting up variable to be used in event handler
var oldPath = treegr.append("path")
    .attr("d","M0,0")
    .attr("fill","none")  
    .attr("stroke-opacity",0.0);


d3.json("$JSONFILE", function(error, flare) {
  root = flare;
  root.x0 = theight / 2;
  root.y0 = 0;

  function setInitialState(d) {
    d.radius = smCircSize;
    if (d.children) {
      d.children.forEach(setInitialState);
    }
  }

  setInitialState(root);
  //root.children.forEach(setInitialState);
  update(root);
});

d3.select(self.frameElement).style("height", "800px");

function update(source) {

  // Compute the new tree layout.
  var nodes = tree.nodes(root).reverse(),
      links = tree.links(nodes);

  // Normalize for fixed-depth or show influence of tick
    nodes.forEach(function(d) { d.y = d.tick * levelPix; });

    
  // Update the nodes…
  var node = treegr.selectAll("g.node")
      .data(nodes, function(d) { return d.id || (d.id = ++i); });

  // Enter any new nodes at the parent's previous position.
  var nodeEnter = node.enter().append("g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + source.x0 + "," + source.y0 + ")"; })
      .on("click", click);

  nodeEnter.append("circle")
      .attr("r", 1e-6)
      .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; });

  nodeEnter.append("text")
      .attr("x", function(d) { return d.children || d._children || (d.name == "{}") ? (d.radius + 6.5) * -1 : d.radius + 6.5; })
      .attr("dy", ".35em")
      .attr("text-anchor", function(d) { return d.children || d._children || (d.name == "{}") ? "end" : "start"; })
      .text(function(d) { return d.name; })
      .style("fill-opacity", 1e-6);

  // Transition nodes to their new position.
  var nodeUpdate = node.transition()
      .duration(duration)
      .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });

  nodeUpdate.select("circle")
      .attr("r", function(d) { return d.radius; })
      .style("stroke", function(d) { return d.strokestyle; })
      .style("fill", function(d) { return d.fillstyle; });

  nodeUpdate.select("text")
      .attr("x", function(d) { return d.children || d._children || (d.name == "{}") ? (d.radius + 6.5) * -1 : d.radius + 6.5; })
      .style("fill-opacity", 1);

  // Transition exiting nodes to the parent's new position.
  var nodeExit = node.exit().transition()
      .duration(duration)
      .attr("transform", function(d) { return "translate(" + source.x + "," + source.y + ")"; })
      .remove();

  nodeExit.select("circle")
      .attr("r", 1e-6);

  nodeExit.select("text")
      .style("fill-opacity", 1e-6);

  // Update the links…
  var link = treegr.selectAll("path.link")
      .data(links, function(d) { return d.target.id; });

  // Enter any new links at the parent's previous position.
  link.enter().insert("path", "g")
      .attr("class", "link")
      .attr("d", function(d) {
        var o = {x: source.x0, y: source.y0};
        return diagonal({source: o, target: o});
      });

  // Transition links to their new position.
  link.transition()
      .duration(duration)
      .attr("d", diagonal);

  // Transition exiting nodes to the parent's new position.
  link.exit().transition()
      .duration(duration)
      .attr("d", function(d) {
        var o = {x: source.x, y: source.y};
        return diagonal({source: o, target: o});
      })
      .remove();

  // Stash the old positions for transition.
  nodes.forEach(function(d) {
    d.x0 = d.x;
    d.y0 = d.y;
  });
}

// Toggle children on click.
function click(d) {
    tickExtent = getTickExtent(d);
    //console.log("Tick extent of node is %O", tickExtent(d));
    if (tickExtent[0] != tickExtent[1]) 
        gBrush.call(brush.extent(tickExtent));
    else 
        gBrush.call(brush.extent([tickExtent[1]-1,tickExtent[1]]));  
}

// Determine the extent of ticks represented in this subtree
function getTickExtent(d) {

    function getHighestTick(d) {
        if (d.children) {
           //console.log("Child ticks are %O", d.children.map(function(child) {return getHighestTick(child)}));
           return maxInArray(d.children.map(function(child) {return getHighestTick(child)}));
        } else {
           //console.log("Click value of node is %d", d.tick);
           return d.tick
        }
    } 

  return [d.tick, getHighestTick(d)];    
}

// As discussed http://stackoverflow.com/questions/1379553/how-might-i-find-the-largest-number-contained-in-a-javascript-array

function maxInArray( array ){
    return Math.max.apply( Math, array );
};

</script>

</body>
Endofmessage

exit