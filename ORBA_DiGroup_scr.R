####ORBA - DiGroup (grouping of distances according to plot group/factor affiliation)####

DiGroup<-function(dimat,gf,ccdi=FALSE,pcdi=FALSE,ppdi=FALSE, plID=NULL)  
  #'dimat' - distance matrix from 'DiMat', i.e. study plots first, plot group=factor centroids last;
  #'gf' - as in 'DiMat'; vector with plot group=factor affiliation of points (i.e. study plots), as many entries as points;
  #'ccdi' - logical, should centroid x centroid distance matrix be added as an element of output list? (default=FALSE)
  #'pcdi' - logical, should plot x centroid distance matrix be added as an element of output list? (default=FALSE)
  #'ppdi' - logical, should plot x plot distance matrix be added as an element of output list? (default=FALSE)
  #'plID - a vector with study plot ID's, as many entries as points (default=NULL)
{
  ##'nof' - number of groups=factors, i.e. number of centroids; calculated as unique values in 'gf'
  message (paste("Number of plot groups/factors calculated from 'gf' (",(length(unique(gf)))," groups)",sep=""))
  nof<-length(unique(gf))
  
  ##subset of distance matrix
  di_subs<-as.matrix(dimat)[,((ncol(as.matrix(dimat))-nof+1):(ncol(as.matrix(dimat))))] #last 'nof' columns of 'dimat'
  
  ##if 'plID' is not specified, default taken
  if (missing(plID)) {
    message ("Plot ID's missing (argument 'plID' not specified), replaced by default")
    plID<-rownames(di_subs)[1:(nrow(di_subs)-nof)] #to get correct length (i.e. without centroids)
  }
  
  ##distances in plot groups - listing
  L<-nof
  list_dist<-vector("list",L) #empty list (list has to be used due to diff. number of elements per group)
  for (i in 1:L)  {
    lel<-di_subs[(which(gf==levels(gf)[i])),i] #distances of groups to their centroid (row - plots selected by belonging to group, column - centroid of group selected by plot group ID=factor level)
    list_dist[[i]]<-lel #temporary placeholder
    names(list_dist)[i]<-as.character(levels(gf)[i]) #name list elements by plot group ID=factor level
    names(list_dist[[i]])<-plID[gf==levels(gf)[i]] #singular elements (i.e. plots) receive names from plot ID vector
    rm(lel) #clean temporary objec 'lel'
  }
  #print summary statistics
  for (i in 1:(length(list_dist)))
  {
    print(paste("group/factor:", names(list_dist)[i])) #names first
    print(summary(list_dist[[i]],digits=4)) #summaries second (if 'c' in same line - sum.stats coerced to character)
    flush.console() #'flush.console' to print output of summary statistics before terminating function and returning list
  }
  ##if ccdi=TRUE -> centroid x centroid distances added as 'nof+1'th element of list
  if (ccdi==T) {
    cxc<-as.matrix(dimat)[((nrow(as.matrix(dimat))-nof+1):(nrow(as.matrix(dimat)))),((ncol(as.matrix(dimat))-nof+1):(ncol(as.matrix(dimat))))] #subset of 'dimat' - last [nof] rows and columns
    colnames(cxc)<-as.character(levels(gf)[1:nof]) #rename by factor levels
    rownames(cxc)<-colnames(cxc)
    list_dist[[nof+1]]<-cxc #adding as 'nof'+1'th element to output list
    names(list_dist)[nof+1]<-c("CxC_dm")
    rm(cxc) #clean
    message (paste("Centroid x centroid distance matrix added as element",(nof+1),"of the output list ('CxC_dm')"))
  }
  ## if pcdi=TRUE -> plot x centroid distances added as last element of list
  if (pcdi==T) {
    pxc<-as.matrix(dimat)[1:(nrow(as.matrix(dimat))-nof),
                          ((ncol(as.matrix(dimat))-nof+1):(ncol(as.matrix(dimat))))] 
    #first 1:(nrow-'nof') rows (i.e. plots)
    #last 'nof' columns from distance matrix (i.e. centroids)
    ##rename rows and columns
    colnames(pxc)<-as.character(levels(gf)[1:nof])
    rownames(pxc)<-plID
    list_dist[[length(list_dist)+1]]<-pxc #adding as last element to output list
    names(list_dist)[length(list_dist)]<-c("PxC_dm")
    rm(pxc) #clean
    message (paste("Plot x centroid distance matrix added as element ",
                   length(list_dist)," of the output list ('PxC_dm')",sep=""))
  }
  ## if ppdi=TRUE -> plot x plot distances added as last element of list
  if (ppdi==T) {
    pxp<-as.matrix(dimat)[1:(nrow(as.matrix(dimat))-nof),
                          1:(ncol(as.matrix(dimat))-nof)] 
    #first 1:(nrow-'nof') rows (i.e. plots)
    #first 1:(ncol-'nof') columns (i.e. plots)
    ##rename rows and columns
    colnames(pxp)<-plID
    rownames(pxp)<-plID
    list_dist[[length(list_dist)+1]]<-pxp #adding as last element to output list
    names(list_dist)[length(list_dist)]<-c("PxP_dm")
    rm(pxp) #clean
    message (paste("Plot x plot distance matrix added as element ",
                   length(list_dist)," of the output list ('PxP_dm')",sep=""))
  }
  ##output list
  rm(i,L) #clean 'for'-loop arguments
  return(list_dist)
}