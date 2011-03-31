#to create abundance tables
              
#----Not Needed----#
#abundance.list = list(
#  title = "Abundance Table",
#  help = "abundance",
#  type = "text",                      # either text or graphic
#  variableType = NULL,
#  assignto = TRUE,
#  action = list(
#    beginning = "abundance(",
#    ending = ")"
#    ),
#  arguments = list(
#    arguments =list(
#      object=EMPTY.list,
#      weighted=FALSE.list,
#      assignmentCol = list(
#        type="gdroplist",
#        items = c(0,1,2,3)
#        #names(OTUset_sampleData)
#        #names(aData(OTUset))
#        ),
#      collab = list(
#        type="gdroplist",
#        items = c('','"o"', '"l"', '"7"','"c"', '"u"',  '"]"')
#        )
#      )
#    )
#  )

#-------Not needed=======#
#abundanceDF.list = list(
#  title = "Abundance Table",
#  help = "abundance",
#  type = "text",                      # either text or graphic
#  variableType = NULL,
#  assignto = TRUE,
#  action = list(
#    beginning = "as.data.frame(abundance(",
#    ending = "))"
#    ),
#  arguments = list(
#    arguments =list(
#      object = EMPTY.list
#      )
#    )
#  )

richness = function(abundance_table) {
	r<-apply(abundance_table,2,function(i){estimateR(i)})
	r
}

richness.list = list(
  title = "Richness Estimation",
  help = "o_estimateR",
  type = "text",                      # either text or graphic
  variableType = NULL,
  assignto = TRUE,
  action = list(
    beginning = "richness(",
    ending = ")"
    ),
  arguments = list(
    arguments =list(
      abundance_table = EMPTY.list
    )
  )
)

diversityfun = function(abundance_table, index) {
	r<-diversity(t(abundance_table), index=index)
	r
} 

diversity.list = list(
  title = "Diversity Estimation",
  help = "o_diversity",
  type = "text",                      # either text or graphic
  variableType = NULL,
  assignto = TRUE,
  action = list(
    beginning = "diversityfun(",
    ending = ")"
    ),
  arguments = list(
    arguments =list(
      abundance_table = EMPTY.list,
      index = list(
      type="gdroplist",
      items = c('"shannon"', '"simpson"', '"invsimpson"') 
      )
    )
  )
)

cluster = function(abundance_table, distance_method, cluster_method) {
  d<-vegdist(abundance_table, method=distance_method)
  rn<-row.names(as.matrix(d))
  clust<-hclust(d,method=cluster_method)
  plot(clust,labels=rn, sub=NA, xlab=NA)
  return(clust)
}

  cluster.list = list(
  title = "Cluster Analysis",
  help = "hclust",
  type = "graphic",                      # either text or graphic
  variableType = NULL,
  assignto = NULL,
  action = list(
    beginning = "cluster(",
    ending = ")"
    ),
  arguments = list(
    arguments =list(
      abundance_table = EMPTY.list,
      distance_method = list(
        type="gdroplist",
        items = c('"manhattan"', '"euclidean"', '"canberra"', '"bray"', 
        '"kulczynski"', '"gower"', '"morisita"', '"horn"', '"mountford"', 
        '"jaccard"', '"raup"', '"binomial"', '"chao"', '"altGower"')
      ),
      cluster_method = list(
        type="gdroplist",
        c('"ward"', '"single"', '"complete"', '"average"', '"mcquitty"') 
      )
    )
  )
)

principle_component = function(abundance_table, plot_2d, plot_3d, scree_plot) {
  abundance.pc = prcomp(abundance_table, scale=T)
  
  #assign(as.character(svalue(abundance_table)), OTUset <<- readOTUset(dirPath=path, level=svalue(level),
  #otufile=OTUfilename,
  #samplefile=SAMPLEfilename), envir = .GlobalEnv)

  if(plot_2d==TRUE) {
    dev.new()
    biplot(abundance.pc)
  }
  
  if(plot_3d==TRUE) {
    plot(bpca(abundance_table, method='hj', lambda.end=3), rgl.use=TRUE,
    var.col='brown', var.factor=1, var.cex=1,
    obj.names=TRUE, obj.cex=.8,
    simple.axes=FALSE, box=TRUE)
  }
  
  if(scree_plot==TRUE) {
    dev.new()
    plot(abundance.pc$sdev^2, type="o", pch=16, col="red")
  }

}

#principle_component(abundance_prob, plot_2d, plot_3d, scree_plot)

prcomp.list = list(
  title = "Principle Component Analysis",
  help = "prcomp",
  variableType = NULL,
  assignto = NULL,
  action = list(
    beginning = "principle_component(",
    ending = ")"
    ),
  arguments = list(
    arguments =list(
      abundance_table = EMPTY.list,
      plot_2d = list(
        type="gradio",
        items=c("TRUE", "FALSE")
      ),
      plot_3d = list(
        type="gradio",
        items=c("TRUE", "FALSE")
      ),
      scree_plot = list(
        type="gradio",
        items=c("TRUE", "FALSE")
      )
    )
  )
)

bootstrap.list = list(
  title = "Bootstrap Parameter Estimation",
  help = "bootstrap",
  variableType = NULL,
  assignto = NULL,
  action = list(
    beginning = "bootstrap(",
    ending = ")"
    ),
  arguments = list(
    arguments =list(
      abundance_table = EMPTY.list,
      confidence_level = list(
        type = "gedit",
        text = .95
      ), 
      iterations = list(
        type = "gedit",
        text = 5000
      ),  
      bs_mean = list(
        type="gradio",
        items=c("TRUE", "FALSE")
      ),
      bs_median = list(
        type="gradio",
        items=c("TRUE", "FALSE")
      ),
      bs_standard_deviation = list(
        type="gradio",
        items=c("TRUE", "FALSE")
      ),
      bs_coefficient_of_variation = list(
        type="gradio",
        items=c("TRUE", "FALSE")
      )
    )
  )
)

#beta_diversity = function(abundance_table, indecies) {


#diversity = betadiver(x=abundance_table, index=svalue(indecies))

#betadiver(x, index = NA, order = FALSE, help = FALSE, ...)
#}

#betadiversity.list = list(
#  title = "Beta Diversity",
#  help = "betadiver",
#  variableType = NULL,
#  assignto = NULL,
#  action = list(
#    beginning = "beta_diversity(",
#    ending = ")"
#    ),
#  arguments = list(
#    arguments =list(
#      abundance_table = EMPTY.list,     
#      indecies = list(
#        type="gdroplist",
#        items = c("w = (b+c)/(2*a+b+c)", "-1 = (b+c)/(2*a+b+c)", "c = (b+c)/2", "wb = b+c", "r = 2*b*c/((a+b+c)^2-2*b*c)","I = log(2*a+b+c)-2*a*log(2)/(2*a+b+c)-((a+b)*log(a+b)+(a+c)*log(a+c))/(2*a+b+c)","e = exp(log(2*a+b+c)-2*a*log(2)/(2*a+b+c)-((a+b)*log(a+b)+(a+c)*log(a+c))/(2*a+b+c))-1","t = (b+c)/(2*a+b+c)","me = (b+c)/(2*a+b+c)","j = a/(a+b+c)","sor = 2*a/(2*a+b+c)","m = (2*a+b+c)*(b+c)/(a+b+c)","-2 = pmin(b,c)/(pmax(b,c)+a)","co = (a*c+a*b+2*b*c)/(2*(a+b)*(a+c))","cc = (b+c)/(a+b+c)","g = (b+c)/(a+b+c)","-3 = pmin(b,c)/(a+b+c)","l = (b+c)/2","19 = 2*(b*c+1)/((a+b+c)^2+(a+b+c))","hk = (b+c)/(2*a+b+c)","rlb = a/(a+c)","sim = pmin(b,c)/(pmin(b,c)+a)","gl = 2*abs(b-c)/(2*a+b+c)","z = (log(2)-log(2*a+b+c)+log(a+b+c))/log(2)")
#    )
#  )
#)

 
#  heatmap.list = list(
#  title = "Heat Map",
#  help = "heatmap",
#  type = "text",                      # either text or graphic
#  variableType = NULL,
#  assignto = NULL,
#  action = list(
#    beginning = "heatmap(",
#    ending = ")"
#    ),              
#  arguments = list(
#    arguments =list(
#      x = EMPTY.list
#      )
#    )
#  )