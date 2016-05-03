# Data Products
J Faleiro  
May 1, 2015  

# Required libraries


```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(googleVis, devtools)
```

# Shiny & rCharts

## rCharts example

rCharts is not on CRAN, so...


```r
library(devtools)
install_github('rCharts', 'ramnathv', ref = 'dev')
```

```
## Warning: Username parameter is deprecated. Please use ramnathv/rCharts
```

```
## Skipping install for github remote, the SHA1 (8d3fe35b) has not changed since last install.
##   Use `force = TRUE` to force installation
```

This setup is required, mainly to import CSS files that sets up width/height for the plots. It imports CSS files and JavaScript libraries from online resources. 


```r
library(rCharts)
## utility function to add required assets such as CSS and JS libraries
add_lib_assets <- function(lib, cdn = F,css=NULL) {
    assets = get_assets(get_lib(lib), cdn = cdn)
    if(!is.null(css)){assets$css=c(assets$css,css)}
    styles <- lapply(assets$css, function(style) {
        sprintf("<link rel='stylesheet' href=%s>", style)
    })

    scripts <- lapply(assets$jshead, function(script) {
        sprintf("<script type='text/javascript' src=%s></script>", script)
    })
    cat(paste(c(styles, scripts), collapse = "\n"))
}

# get assets from online repositories 
add_lib_assets("NVD3",cdn=TRUE,css="http://rawgithub.com/ramnathv/rCharts/master/inst/libraries/nvd3/css/rNVD3.css") 
```

<link rel='stylesheet' href=http://nvd3.org/assets/css/nv.d3.css>
<link rel='stylesheet' href=http://rawgithub.com/ramnathv/rCharts/master/inst/libraries/nvd3/css/rNVD3.css>
<script type='text/javascript' src=http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js></script>
<script type='text/javascript' src=http://d3js.org/d3.v3.min.js></script>
<script type='text/javascript' src=http://timelyportfolio.github.io/rCharts_nvd3_tests/libraries/widgets/nvd3/js/nv.d3.min-new.js></script>
<script type='text/javascript' src=http://nvd3.org/assets/lib/fisheye.js></script>

```r
add_lib_assets("Polycharts",cdn=TRUE)
```

<script type='text/javascript' src=http://ramnathv.github.io/rCharts/libraries/widgets/polycharts/js/polychart2.standalone.js></script>

Static plot, hover over data samples for more details


```r
names(iris) = gsub("\\.", "", names(iris))
r1<-rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
r1$print("polyScatter")
```


<div id = 'polyScatter' class = 'rChart polycharts'></div>
<script type='text/javascript'>
    var chartParams = {
 "dom": "polyScatter",
"width":    800,
"height":    400,
"layers": [
 {
 "x": "SepalWidth",
"y": "SepalLength",
"data": {
 "SepalLength": [    5.1,    4.9,    4.7,    4.6,      5,    5.4,    4.6,      5,    4.4,    4.9,    5.4,    4.8,    4.8,    4.3,    5.8,    5.7,    5.4,    5.1,    5.7,    5.1,    5.4,    5.1,    4.6,    5.1,    4.8,      5,      5,    5.2,    5.2,    4.7,    4.8,    5.4,    5.2,    5.5,    4.9,      5,    5.5,    4.9,    4.4,    5.1,      5,    4.5,    4.4,      5,    5.1,    4.8,    5.1,    4.6,    5.3,      5,      7,    6.4,    6.9,    5.5,    6.5,    5.7,    6.3,    4.9,    6.6,    5.2,      5,    5.9,      6,    6.1,    5.6,    6.7,    5.6,    5.8,    6.2,    5.6,    5.9,    6.1,    6.3,    6.1,    6.4,    6.6,    6.8,    6.7,      6,    5.7,    5.5,    5.5,    5.8,      6,    5.4,      6,    6.7,    6.3,    5.6,    5.5,    5.5,    6.1,    5.8,      5,    5.6,    5.7,    5.7,    6.2,    5.1,    5.7,    6.3,    5.8,    7.1,    6.3,    6.5,    7.6,    4.9,    7.3,    6.7,    7.2,    6.5,    6.4,    6.8,    5.7,    5.8,    6.4,    6.5,    7.7,    7.7,      6,    6.9,    5.6,    7.7,    6.3,    6.7,    7.2,    6.2,    6.1,    6.4,    7.2,    7.4,    7.9,    6.4,    6.3,    6.1,    7.7,    6.3,    6.4,      6,    6.9,    6.7,    6.9,    5.8,    6.8,    6.7,    6.7,    6.3,    6.5,    6.2,    5.9 ],
"SepalWidth": [    3.5,      3,    3.2,    3.1,    3.6,    3.9,    3.4,    3.4,    2.9,    3.1,    3.7,    3.4,      3,      3,      4,    4.4,    3.9,    3.5,    3.8,    3.8,    3.4,    3.7,    3.6,    3.3,    3.4,      3,    3.4,    3.5,    3.4,    3.2,    3.1,    3.4,    4.1,    4.2,    3.1,    3.2,    3.5,    3.6,      3,    3.4,    3.5,    2.3,    3.2,    3.5,    3.8,      3,    3.8,    3.2,    3.7,    3.3,    3.2,    3.2,    3.1,    2.3,    2.8,    2.8,    3.3,    2.4,    2.9,    2.7,      2,      3,    2.2,    2.9,    2.9,    3.1,      3,    2.7,    2.2,    2.5,    3.2,    2.8,    2.5,    2.8,    2.9,      3,    2.8,      3,    2.9,    2.6,    2.4,    2.4,    2.7,    2.7,      3,    3.4,    3.1,    2.3,      3,    2.5,    2.6,      3,    2.6,    2.3,    2.7,      3,    2.9,    2.9,    2.5,    2.8,    3.3,    2.7,      3,    2.9,      3,      3,    2.5,    2.9,    2.5,    3.6,    3.2,    2.7,      3,    2.5,    2.8,    3.2,      3,    3.8,    2.6,    2.2,    3.2,    2.8,    2.8,    2.7,    3.3,    3.2,    2.8,      3,    2.8,      3,    2.8,    3.8,    2.8,    2.8,    2.6,      3,    3.4,    3.1,      3,    3.1,    3.1,    3.1,    2.7,    3.2,    3.3,      3,    2.5,      3,    3.4,      3 ],
"PetalLength": [    1.4,    1.4,    1.3,    1.5,    1.4,    1.7,    1.4,    1.5,    1.4,    1.5,    1.5,    1.6,    1.4,    1.1,    1.2,    1.5,    1.3,    1.4,    1.7,    1.5,    1.7,    1.5,      1,    1.7,    1.9,    1.6,    1.6,    1.5,    1.4,    1.6,    1.6,    1.5,    1.5,    1.4,    1.5,    1.2,    1.3,    1.4,    1.3,    1.5,    1.3,    1.3,    1.3,    1.6,    1.9,    1.4,    1.6,    1.4,    1.5,    1.4,    4.7,    4.5,    4.9,      4,    4.6,    4.5,    4.7,    3.3,    4.6,    3.9,    3.5,    4.2,      4,    4.7,    3.6,    4.4,    4.5,    4.1,    4.5,    3.9,    4.8,      4,    4.9,    4.7,    4.3,    4.4,    4.8,      5,    4.5,    3.5,    3.8,    3.7,    3.9,    5.1,    4.5,    4.5,    4.7,    4.4,    4.1,      4,    4.4,    4.6,      4,    3.3,    4.2,    4.2,    4.2,    4.3,      3,    4.1,      6,    5.1,    5.9,    5.6,    5.8,    6.6,    4.5,    6.3,    5.8,    6.1,    5.1,    5.3,    5.5,      5,    5.1,    5.3,    5.5,    6.7,    6.9,      5,    5.7,    4.9,    6.7,    4.9,    5.7,      6,    4.8,    4.9,    5.6,    5.8,    6.1,    6.4,    5.6,    5.1,    5.6,    6.1,    5.6,    5.5,    4.8,    5.4,    5.6,    5.1,    5.1,    5.9,    5.7,    5.2,      5,    5.2,    5.4,    5.1 ],
"PetalWidth": [    0.2,    0.2,    0.2,    0.2,    0.2,    0.4,    0.3,    0.2,    0.2,    0.1,    0.2,    0.2,    0.1,    0.1,    0.2,    0.4,    0.4,    0.3,    0.3,    0.3,    0.2,    0.4,    0.2,    0.5,    0.2,    0.2,    0.4,    0.2,    0.2,    0.2,    0.2,    0.4,    0.1,    0.2,    0.2,    0.2,    0.2,    0.1,    0.2,    0.2,    0.3,    0.3,    0.2,    0.6,    0.4,    0.3,    0.2,    0.2,    0.2,    0.2,    1.4,    1.5,    1.5,    1.3,    1.5,    1.3,    1.6,      1,    1.3,    1.4,      1,    1.5,      1,    1.4,    1.3,    1.4,    1.5,      1,    1.5,    1.1,    1.8,    1.3,    1.5,    1.2,    1.3,    1.4,    1.4,    1.7,    1.5,      1,    1.1,      1,    1.2,    1.6,    1.5,    1.6,    1.5,    1.3,    1.3,    1.3,    1.2,    1.4,    1.2,      1,    1.3,    1.2,    1.3,    1.3,    1.1,    1.3,    2.5,    1.9,    2.1,    1.8,    2.2,    2.1,    1.7,    1.8,    1.8,    2.5,      2,    1.9,    2.1,      2,    2.4,    2.3,    1.8,    2.2,    2.3,    1.5,    2.3,      2,      2,    1.8,    2.1,    1.8,    1.8,    1.8,    2.1,    1.6,    1.9,      2,    2.2,    1.5,    1.4,    2.3,    2.4,    1.8,    1.8,    2.1,    2.4,    2.3,    1.9,    2.3,    2.5,    2.3,    1.9,      2,    2.3,    1.8 ],
"Species": [ "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "setosa", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "versicolor", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica", "virginica" ] 
},
"facet": "Species",
"color": "Species",
"type": "point" 
} 
],
"facet": {
 "type": "wrap",
"var": "Species" 
},
"guides": [],
"coord": [],
"id": "polyScatter" 
}
    _.each(chartParams.layers, function(el){
        el.data = polyjs.data(el.data)
    })
    var graph_polyScatter = polyjs.chart(chartParams);
</script>

Slightly dynamic, you can select different types (grouped, stacked) for display.


```r
hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart")
n1$print("nvd3mbar")
```


<div id = 'nvd3mbar' class = 'rChart nvd3'></div>
<script type='text/javascript'>
 $(document).ready(function(){
      drawnvd3mbar()
    });
    function drawnvd3mbar(){  
      var opts = {
 "dom": "nvd3mbar",
"width":    800,
"height":    400,
"x": "Hair",
"y": "Freq",
"group": "Eye",
"type": "multiBarChart",
"id": "nvd3mbar" 
},
        data = [
 {
 "Hair": "Black",
"Eye": "Brown",
"Sex": "Male",
"Freq":             32 
},
{
 "Hair": "Brown",
"Eye": "Brown",
"Sex": "Male",
"Freq":             53 
},
{
 "Hair": "Red",
"Eye": "Brown",
"Sex": "Male",
"Freq":             10 
},
{
 "Hair": "Blond",
"Eye": "Brown",
"Sex": "Male",
"Freq":              3 
},
{
 "Hair": "Black",
"Eye": "Blue",
"Sex": "Male",
"Freq":             11 
},
{
 "Hair": "Brown",
"Eye": "Blue",
"Sex": "Male",
"Freq":             50 
},
{
 "Hair": "Red",
"Eye": "Blue",
"Sex": "Male",
"Freq":             10 
},
{
 "Hair": "Blond",
"Eye": "Blue",
"Sex": "Male",
"Freq":             30 
},
{
 "Hair": "Black",
"Eye": "Hazel",
"Sex": "Male",
"Freq":             10 
},
{
 "Hair": "Brown",
"Eye": "Hazel",
"Sex": "Male",
"Freq":             25 
},
{
 "Hair": "Red",
"Eye": "Hazel",
"Sex": "Male",
"Freq":              7 
},
{
 "Hair": "Blond",
"Eye": "Hazel",
"Sex": "Male",
"Freq":              5 
},
{
 "Hair": "Black",
"Eye": "Green",
"Sex": "Male",
"Freq":              3 
},
{
 "Hair": "Brown",
"Eye": "Green",
"Sex": "Male",
"Freq":             15 
},
{
 "Hair": "Red",
"Eye": "Green",
"Sex": "Male",
"Freq":              7 
},
{
 "Hair": "Blond",
"Eye": "Green",
"Sex": "Male",
"Freq":              8 
} 
]
  
      if(!(opts.type==="pieChart" || opts.type==="sparklinePlus" || opts.type==="bulletChart")) {
        var data = d3.nest()
          .key(function(d){
            //return opts.group === undefined ? 'main' : d[opts.group]
            //instead of main would think a better default is opts.x
            return opts.group === undefined ? opts.y : d[opts.group];
          })
          .entries(data);
      }
      
      if (opts.disabled != undefined){
        data.map(function(d, i){
          d.disabled = opts.disabled[i]
        })
      }
      
      nv.addGraph(function() {
        var chart = nv.models[opts.type]()
          .width(opts.width)
          .height(opts.height)
          
        if (opts.type != "bulletChart"){
          chart
            .x(function(d) { return d[opts.x] })
            .y(function(d) { return d[opts.y] })
        }
          
         
        
          
        

        
        
        
      
       d3.select("#" + opts.id)
        .append('svg')
        .datum(data)
        .transition().duration(500)
        .call(chart);

       nv.utils.windowResize(chart.update);
       return chart;
      });
    };
</script>

## Google Vis Example

Initialization


```r
library(googleVis)
op <- options(gvis.plot.tag='chart')
```

Static plot:


```r
CityPopularity$Mean=mean(CityPopularity$Popularity)
CC <- gvisComboChart(CityPopularity, xvar='City',
          yvar=c('Mean', 'Popularity'),
          options=list(seriesType='bars',
                       width=450, height=300,
                       title='City Popularity',
                       series='{0: {type:\"line\"}}'))
plot(CC)
```

<!-- ComboChart generated in R 3.2.4 by googleVis 0.5.10 package -->
<!-- Sun May  1 15:24:12 2016 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataComboChartID3ea431c57b00 () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "New York",
450,
200 
],
[
 "Boston",
450,
300 
],
[
 "Miami",
450,
400 
],
[
 "Chicago",
450,
500 
],
[
 "Los Angeles",
450,
600 
],
[
 "Houston",
450,
700 
] 
];
data.addColumn('string','City');
data.addColumn('number','Mean');
data.addColumn('number','Popularity');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartComboChartID3ea431c57b00() {
var data = gvisDataComboChartID3ea431c57b00();
var options = {};
options["allowHtml"] = true;
options["seriesType"] = "bars";
options["width"] =    450;
options["height"] =    300;
options["title"] = "City Popularity";
options["series"] = {0: {type:"line"}};

    var chart = new google.visualization.ComboChart(
    document.getElementById('ComboChartID3ea431c57b00')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "corechart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartComboChartID3ea431c57b00);
})();
function displayChartComboChartID3ea431c57b00() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartComboChartID3ea431c57b00"></script>
 
<!-- divChart -->
  
<div id="ComboChartID3ea431c57b00" 
  style="width: 450; height: 300;">
</div>

Some animation:


```r
M <- gvisMotionChart(Fruits, 'Fruit', 'Year', options=list(width=600, heigth=400))
plot(M)
```

<!-- MotionChart generated in R 3.2.4 by googleVis 0.5.10 package -->
<!-- Sun May  1 15:24:12 2016 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataMotionChartID3ea47152c774 () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "Apples",
2008,
"West",
98,
78,
20,
"2008-12-31" 
],
[
 "Apples",
2009,
"West",
111,
79,
32,
"2009-12-31" 
],
[
 "Apples",
2010,
"West",
89,
76,
13,
"2010-12-31" 
],
[
 "Oranges",
2008,
"East",
96,
81,
15,
"2008-12-31" 
],
[
 "Bananas",
2008,
"East",
85,
76,
9,
"2008-12-31" 
],
[
 "Oranges",
2009,
"East",
93,
80,
13,
"2009-12-31" 
],
[
 "Bananas",
2009,
"East",
94,
78,
16,
"2009-12-31" 
],
[
 "Oranges",
2010,
"East",
98,
91,
7,
"2010-12-31" 
],
[
 "Bananas",
2010,
"East",
81,
71,
10,
"2010-12-31" 
] 
];
data.addColumn('string','Fruit');
data.addColumn('number','Year');
data.addColumn('string','Location');
data.addColumn('number','Sales');
data.addColumn('number','Expenses');
data.addColumn('number','Profit');
data.addColumn('string','Date');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartMotionChartID3ea47152c774() {
var data = gvisDataMotionChartID3ea47152c774();
var options = {};
options["width"] =    600;
options["height"] =    500;
options["state"] = "";
options["heigth"] =    400;

    var chart = new google.visualization.MotionChart(
    document.getElementById('MotionChartID3ea47152c774')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "motionchart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartMotionChartID3ea47152c774);
})();
function displayChartMotionChartID3ea47152c774() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartMotionChartID3ea47152c774"></script>
 
<!-- divChart -->
  
<div id="MotionChartID3ea47152c774" 
  style="width: 600; height: 500;">
</div>

```r
options(op)
```



```r
demo(googleVis)
```



	demo(googleVis)
	---- ~~~~~~~~~

> ## ---- googleVis demo ----
> ## ---- pauseFunction ----
> pause <- function(){  
+   invisible(readline("\nPress <return> to continue: ")) 
+ }

> ## ---- testData ----
> df=data.frame(country=c("US", "GB", "BR"), 
+               val1=c(10,13,14), 
+               val2=c(23,12,32))

> ## ---- LineChart ----
> Line <- gvisLineChart(df)

> plot(Line)

```
## starting httpd help server ...
```

```
##  done
```


> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- TwoAxis ----
> Line2 <- gvisLineChart(df, "country", c("val1","val2"),
+                        options=list(
+                          series="[{targetAxisIndex: 0},
+                                  {targetAxisIndex:1}]",
+                          vAxes="[{title:'val1'}, {title:'val2'}]"
+                        ))

> plot(Line2)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- SettingOptions ----
> Line3 <-  gvisLineChart(df, xvar="country", yvar=c("val1","val2"),
+                         options=list(
+                           title="Hello World",
+                           titleTextStyle="{color:'red', 
+                                            fontName:'Courier', 
+                                            fontSize:16}",                         
+                           backgroundColor="#D3D3D3",                          
+                           vAxis="{gridlines:{color:'red', count:3}}",
+                           hAxis="{title:'Country', titleTextStyle:{color:'blue'}}",
+                           series="[{color:'green', targetAxisIndex: 0},	
+                                    {color: 'orange',targetAxisIndex:1}]",
+                           vAxes="[{title:'val1'}, {title:'val2'}]",
+                           legend="bottom",
+                           curveType="function",
+                           width=500,
+                           height=300                         
+                         ))

> plot(Line3)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- CustomizingLines ----
> Dashed <-  gvisLineChart(df, xvar="country", yvar=c("val1","val2"),
+                         options=list(
+                           series="[{color:'green', targetAxisIndex: 0, 
+                           lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
+                           {color: 'blue',targetAxisIndex: 1, 
+                           lineWidth: 2, lineDashStyle: [4, 1]}]",
+                           vAxes="[{title:'val1'}, {title:'val2'}]"
+                         ))

> plot(Dashed)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- EditButton ----
> Line4 <-  gvisLineChart(df, "country", c("val1","val2"),
+                         options=list(gvis.editor="Edit me!"))

> plot(Line4)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- BarChart ----
> Bar <- gvisBarChart(df)

> plot(Bar)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- ColumnChart ----
> Column <- gvisColumnChart(df)

> plot(Column)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- AreaChart ----
> Area <- gvisAreaChart(df)

> plot(Area)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- SteppedAreaChart ----
> SteppedArea <- gvisSteppedAreaChart(df, xvar="country", 
+                                     yvar=c("val1", "val2"),
+                                     options=list(isStacked=TRUE))

> plot(SteppedArea)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- ComboChart ----
> Combo <- gvisComboChart(df, xvar="country",
+                         yvar=c("val1", "val2"),
+                         options=list(seriesType="bars",
+                                      series='{1: {type:"line"}}'))

> plot(Combo)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- ScatterChart ----
> Scatter <- gvisScatterChart(women, 
+                             options=list(
+                               legend="none",
+                               lineWidth=2, pointSize=0,
+                               title="Women", vAxis="{title:'weight (lbs)'}",
+                               hAxis="{title:'height (in)'}", 
+                               width=300, height=300))

> plot(Scatter)

> ## ---- ScatterChartPoints ----
> M <- matrix(nrow=6,ncol=6)

> M[col(M)==row(M)] <- 1:6

> dat <- data.frame(X=1:6, M)

> SC <- gvisScatterChart(dat, 
+                        options=list(
+                          title="Customizing points",
+                          legend="right",
+                          pointSize=30,
+                          series="{
+                               0: { pointShape: 'circle' },
+                               1: { pointShape: 'triangle' },
+                               2: { pointShape: 'square' },
+                               3: { pointShape: 'diamond' },
+                               4: { pointShape: 'star' },
+                               5: { pointShape: 'polygon' }
+                               }"))

> plot(SC)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- BubbleChart ----
> Bubble <- gvisBubbleChart(Fruits, idvar="Fruit", 
+                           xvar="Sales", yvar="Expenses",
+                           colorvar="Year", sizevar="Profit",
+                           options=list(
+                             hAxis='{minValue:75, maxValue:125}'))

> plot(Bubble)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- CandlestickChart ----
> Candle <- gvisCandlestickChart(OpenClose, 
+                                options=list(legend='none'))

> plot(Candle)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- PieChart ----
> Pie <- gvisPieChart(CityPopularity)

> plot(Pie)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- Gauge ----
> Gauge <-  gvisGauge(CityPopularity, 
+                     options=list(min=0, max=800, greenFrom=500,
+                                  greenTo=800, yellowFrom=300, yellowTo=500,
+                                  redFrom=0, redTo=300, width=400, height=300))

> plot(Gauge)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- IntensityMap ----
> Intensity <- gvisIntensityMap(df)

> plot(Intensity)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- GeoChart ----
> Geo=gvisGeoChart(Exports, locationvar="Country", 
+                  colorvar="Profit",
+                  options=list(projection="kavrayskiy-vii"))

> plot(Geo)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- USStateData ----
> require(datasets)

> states <- data.frame(state.name, state.x77)

> GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
+                           options=list(region="US", 
+                                        displayMode="regions", 
+                                        resolution="provinces",
+                                        width=600, height=400))

> plot(GeoStates)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- GeoChartHurricaneAndrew ----
> GeoMarker <- gvisGeoChart(Andrew, "LatLong", 
+                           sizevar='Speed_kt',
+                           colorvar="Pressure_mb", 
+                           options=list(region="US"))

> plot(GeoMarker)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- GoogleMapHurricaneAndrew ----
> AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
+                      options=list(showTip=TRUE, 
+                                   showLine=TRUE, 
+                                   enableScrollWheel=TRUE,
+                                   mapType='terrain', 
+                                   useMapTypeControl=TRUE))

> plot(AndrewMap)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- Table ----
> Table <- gvisTable(Stock, 
+                    formats=list(Value="#,###"))

> plot(Table)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- TableWithPages ----
> PopTable <- gvisTable(Population, 
+                       formats=list(Population="#,###",
+                                    '% of World Population'='#.#%'),
+                       options=list(page='enable'))

> plot(PopTable)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- OrgChart ----
> Org <- gvisOrgChart(Regions, 
+                     options=list(width=600, height=250,
+                                  size='large', allowCollapse=TRUE))

> plot(Org)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- TreeMap ----
> Tree <- gvisTreeMap(Regions,  
+                     "Region", "Parent", 
+                     "Val", "Fac", 
+                     options=list(fontSize=16))

> plot(Tree)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- AnnotationChart ----
> Anno <- gvisAnnotationChart(Stock, 
+                             datevar="Date",
+                             numvar="Value", 
+                             idvar="Device",
+                             titlevar="Title", 
+                             annotationvar="Annotation",
+                             options=list(
+                               width=600, height=350,
+                               fill=10, displayExactValues=TRUE,
+                               colors="['#0000ff','#00ff00']")
+ )

> plot(Anno)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- SankeyChart ----
> datSK <- data.frame(From=c(rep("A",3), rep("B", 3)),
+                     To=c(rep(c("X", "Y", "Z"),2)),
+                     Weight=c(5,7,6,2,9,4))

> Sankey <- gvisSankey(datSK, from="From", to="To", weight="Weight",
+                      options=list(
+                        sankey="{link: {color: { fill: '#d799ae' } },
+                             node: { color: { fill: '#a61d4c' },
+                             label: { color: '#871b47' } }}"))

> plot(Sankey)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> # ---- CalendarChart ----
> Cal <- gvisCalendar(Cairo, 
+                     datevar="Date", 
+                     numvar="Temp",
+                     options=list(
+                       title="Daily temperature in Cairo",
+                       height=320,
+                       calendar="{yearLabel: { fontName: 'Times-Roman',
+                                fontSize: 32, color: '#1A8763', bold: true},
+                                cellSize: 10,
+                                cellColor: { stroke: 'red', strokeOpacity: 0.2 },
+                                focusedCellColor: {stroke:'red'}}")
+ )

> plot(Cal)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> # ---- Timeline ----
> datTL <- data.frame(Position=c(rep("President", 3), rep("Vice", 3)),
+                     Name=c("Washington", "Adams", "Jefferson",
+                            "Adams", "Jefferson", "Burr"),
+                     start=as.Date(x=rep(c("1789-03-29", "1797-02-03", 
+                                           "1801-02-03"),2)),
+                     end=as.Date(x=rep(c("1797-02-03", "1801-02-03", 
+                                         "1809-02-03"),2)))

> Timeline <- gvisTimeline(data=datTL, 
+                          rowlabel="Name",
+                          barlabel="Position",
+                          start="start", 
+                          end="end",
+                          options=list(timeline="{groupByRowLabel:false}",
+                                       backgroundColor='#ffd', 
+                                       height=350,
+                                       colors="['#cbb69d', '#603913', '#c69c6e']"))

> plot(Timeline)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- Histogram ----
> set.seed(123)

> datHist=data.frame(A=rpois(100, 20),
+                    B=rpois(100, 5),
+                    C=rpois(100, 50))

> Hist <- gvisHistogram(datHist, options=list(
+   legend="{ position: 'top', maxLines: 2 }",
+   colors="['#5C3292', '#1A8763', '#871B47']",
+   width=400, height=360))

> plot(Hist)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- gvisMerge ----
> G <- gvisGeoChart(Exports, "Country", "Profit", 
+                   options=list(width=300, height=300))

> T <- gvisTable(Exports, 
+                options=list(width=220, height=300))

> GT <- gvisMerge(G,T, horizontal=TRUE) 

> plot(GT)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## Flash charts
> ##  ---- GeoMap ----
> Geo=gvisGeoMap(Exports, locationvar="Country", numvar="Profit",
+                options=list(height=350, dataMode='regions'))

> plot(Geo)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- GeoMap ----
> AndrewGeo <- gvisGeoMap(Andrew, 
+                         locationvar="LatLong", 
+                         numvar="Speed_kt", 
+                         hovervar="Category", 
+                         options=list(height=350, 
+                                      region="US", 
+                                      dataMode="markers"))

> plot(AndrewGeo)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- AnnotatedTimeLine ----
> AnnoTimeLine  <- gvisAnnotatedTimeLine(Stock, 
+                                        datevar="Date",
+                                        numvar="Value", 
+                                        idvar="Device",
+                                        titlevar="Title", 
+                                        annotationvar="Annotation",
+                                        options=list(displayAnnotations=TRUE,
+                                                     width="600px", height="350px"))

> plot(AnnoTimeLine)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## ---- MotionChart ----
> Motion=gvisMotionChart(Fruits, 
+                        idvar="Fruit", 
+                        timevar="Year")

> plot(Motion)

> ## ---- pause ----
> pause()

Press <return> to continue: 

> ## You can change some of displaying settings via the browser,
> ## e.g. the level of opacity of non-selected items, or the chart type.
> ## The state string from the 'Advanced' tab can be used to set those
> ## settings via R. Just copy and past the string from the browser into
> ## the argument state of the options list.
> ## Here is an example of a motion chart, with an initial line chart
> ## displayed.
> 
> ## ---- MotionChartSettings ----
> myStateSettings <-'
+ {"xZoomedDataMin":1199145600000,"colorOption":"2",
+ "duration":{"timeUnit":"Y","multiplier":1},"yLambda":1,
+ "yAxisOption":"4","sizeOption":"_UNISIZE",
+ "iconKeySettings":[],"xLambda":1,"nonSelectedAlpha":0,
+ "xZoomedDataMax":1262304000000,"iconType":"LINE",
+ "dimensions":{"iconDimensions":["dim0"]},
+ "showTrails":false,"uniColorForNonSelected":false,
+ "xAxisOption":"_TIME","orderedByX":false,"playDuration":15000,
+ "xZoomedIn":false,"time":"2010","yZoomedDataMin":0,
+ "yZoomedIn":false,"orderedByY":false,"yZoomedDataMax":100}
+ '

> M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(state=myStateSettings))

> plot(M)

> ## See demo(package='googleVis') for other available demos.
