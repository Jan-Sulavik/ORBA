####ORBA - DiMat (n-dimensional distance matrix from ordination object)####

DiMat<-function (ord, gc, gf, ndim=1, met="euclidean",...)  
  #'ord' - ordination object (DCA or NMDS) from 'vegan';
  #'gc' - data frame with axis scores centroids of all plot groups=factors, as many dimensions (i.e. columns) as ordination axes, ordered after levels of 'gf'; 
  #'gf' - vector with plot group=factor affiliation of points (i.e. study plots), as many entries as points;
  #'ndim' - number of dimensions (i.e. ordination axes) for which distances should be calculated (default=1 i.e. the first ordination axis);
  #'met' - method for distance matrix calculation, possible values=all 'dist' methods (default=Euclidean)
  #'...' - other arguments passed on (especially plot weighing (argument 'w') and centroid- or spatial median-selection (argument 'spiders') for 'ordispider'-function)
{ 
  ##select which type ordination (DCA or NMDS) + extract scores
  if (any(attr(ord,"class")=="decorana")){
    ordscores<-as.data.frame(cbind(vegan::scores(ord,display="sites",origin=F)[,1:4]))} #origin=F is important
  if (any(attr(ord,"class")=="monoMDS")){
    ordscores<-as.data.frame(cbind(ord$points))}
  
  ##check if n-dim isn't nonsense (i.e.>max. number of dimensions)
  if (ndim>ncol(ordscores)) stop ("Number of dimensions requested > max. number of dimensions in ordination","\n")
  
  ##if centroids are not supplied, 'DiMat' will calculate its own from 'ordispider' (plot is necessary, empty created)
  if (missing(gc))  {
    message("Plot group=factor centroids calculated by function 'ordispider' from 'vegan'-package")
    vegan::ordiplot(ord,type="n") #empty plot to avoid error if no plot present ('ordispider' requires a plot to be calculated)
    if (any(attr(ord,"class")=="decorana")){
      gc<-vegan::ordispider(ord,gf,label=F,col="white",origin=F,...)}
    if (any(attr(ord,"class")=="monoMDS")){
      gc<-vegan::ordispider(ord,gf,label=F,col="white",...)}
    #preparation of centroids as a normal dataframe
    ce<-as.numeric(gc) #change 'ordispider'-object to normal dataframe (via 'as.numeric' and 'matrix')
    ce<-matrix(ce,nrow=nrow(ordscores),ncol=ncol(ordscores),byrow=F)
    ce<-ce[order(gf),] #order by factor-level order of group=factor affiliation (proper ordering)
    ce<-unique(ce) #only n-group centroids (unique values for n-groups)
    ce<-as.data.frame(ce) #from 'matrix'
  } 
  else  {
    message("Group centroids used as supplied")
    ce <- gc
  } 
  ##proper names 
  if (attr(ord,"class")=="decorana"){ #point scores (if DCA)
    colnames(ordscores)<-c(paste("DCA",c(1:ncol(ordscores)),sep=""))
  }
  if (any(attr(ord,"class")=="monoMDS")){ #point scores (if NMDS)
    colnames(ordscores)<-c(paste("NMDS",c(1:ncol(ordscores)),sep=""))
  }
  if (attr(ord,"class")=="decorana"){ #centroid scores (if DCA)
    colnames(ce)<-c(paste("DCA",c(1:ncol(ce)),sep=""))
    rownames(ce)<-c(paste("C_",as.character(levels(gf)),sep=""))}
  if (any(attr(ord,"class")=="monoMDS")){ #centroid scores (if NMDS)
    colnames(ce)<-c(paste("NMDS",c(1:ncol(ce)),sep=""))
    rownames(ce)<-c(paste("C_",as.character(levels(gf)),sep=""))}
  
  ##bind dataframes
  allscor<-rbind.data.frame(ordscores,ce) #bind plot and centroid axis scores
  
  ##distance matrix calculation
  EuDiMat<- stats::dist(allscor[,1:ndim],method=met)
  message (paste (ndim, "D distance matrix produced",sep=""))
  return(EuDiMat)
}
