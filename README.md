# ORBA
Auxiliary functions for ordination-and-regression based approach (ORBA),
published in Rydgren et al. 2018 J.Appl.Ecol. (https://doi.org/10.1111/1365-2664.13254)

---

## DiMat
creation of (n-dimensional) distance matrix from an ordination object based on plot group=factor affiliation of study plots

### Description:
Function 'DiMat' extracts study plot scores from an ordination object (DCA or NMDS) and creates a (n-dimensional) distance matrix (Euclidean distances are default)
between all study plots and all centroids of plot groups=factors.  
Centroids can be supplied manually (as data frame with centroid scores for all dimensions=ordination axes), or can be obtained internally
through function ['ordispider'](https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/ordihull "'ordispider' documentation") from ['vegan'-package](https://cran.r-project.org/web/packages/vegan/index.html "vegan: Community Ecology Package").

### Usage:
DiMat(ord, gc, gf, ndim, met="euclidean",...)

### Arguments:
'ord' - ordination object (DCA or NMDS) from 'vegan'-package  
'gc' - data frame with axis scores centroids of all plot groups=factors, as many dimensions (i.e. columns) as ordination axes, ordered after levels of 'gf'  
'gf' - vector with plot group=factor affiliation of points (i.e. study plots), as many entries as points  
'ndim' - number of dimensions (i.e. ordination axes) for which distances should be calculated  
'met' - method for distance matrix calculation, possible values=all 'dist' methods (default=Euclidean)  
'...' - other arguments passed on (especially plot weighing (argument 'w') and centroid- or spatial median-selection (argument 'spiders') for 'ordispider'-function)  

### Value:
Output is a distance matrix, i.e. an object of class 'dist'.

---

## DiGroup
grouping of distances from the full distance matrix ('DiMat') based on plot group=factor affiliation of study plots

### Description:
Function 'DiGroup' partitions output distance matrix from 'DiMat', returning a list of plot group=factor-wise matrices of distances of study plots to their plot group=factor centroid.  
Additional arguments add extra matrices (centroid×centroid, study plot×centroid (i.e. all study plots to all centroids), study plot× study plot) to the output list.

### Usage:
DiGroup(dimat,gf,ccdi=FALSE,pcdi=FALSE,ppdi=FALSE, plID=NULL)

### Arguments:
'dimat' - distance matrix from 'DiMat', i.e. study plots first, plot group=factor centroids last  
'gf' - as in 'DiMat'; vector with plot group=factor affiliation of points (i.e. study plots), as many entries as points  
'ccdi' - logical, should centroid x centroid distance matrix be added as an element of output list? (default=FALSE)  
'pcdi' - logical, should plot x centroid distance matrix be added as an element of output list? (default=FALSE)  
'ppdi' - logical, should plot x plot distance matrix be added as an element of output list? (default=FALSE)  
'plID - a vector with study plot ID's, as many entries as points (default=NULL)  

### Value:
Output is a list of plot group=factor-wise matrices of distances of study plots to their plot group=factor centroid.
Optional additional list elements are matrices of centroid × centroid, study plot × centroid (i.e. all study plots to all plot group=factor centroids) and study plot × study plot distances.

