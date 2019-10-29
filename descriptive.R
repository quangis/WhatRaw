# [SC] tutorials for the igraph package
# https://kateto.net/netscix2016.html
# https://kateto.net/network-visualization

libraries <- c("data.table", "ggplot2", "reshape2", "igraph", "FactoMineR", "factoextra")

for(mylibrary in libraries){
	## [SC] installing gplots package
	if (!(mylibrary %in% rownames(installed.packages()))) {
		install.package(mylibrary)
	}
	library(mylibrary, character.only = TRUE)
}

source <- "C:/Users/Enkhbold/PycharmProjects/WhatRaw/"

whatIntentFile <- "what_raw_intents.csv"
whatAdjectivesFile <- "what_raw_adjectives.csv"
whatObjectsFile <- "what_raw_objects.csv"

analyzeWhats <- function(){
	whatIntentsDF <- read.csv(paste0(source, whatIntentFile), stringsAsFactors=FALSE)
	whatAdjectivesDF <- read.csv(paste0(source, whatAdjectivesFile), stringsAsFactors=FALSE)
	whatObjectsDF <- read.csv(paste0(source, whatObjectsFile), stringsAsFactors=FALSE)

	
	#op <- par(mfrow=c(1, 1))

	##############################################
	## [SC] plot frequencies of object intents and type intents

	# [SC] plurals to singular forms
	for(index in 1:nrow(whatIntentsDF)) {
		if (endsWith(whatIntentsDF$intent[index], "ies")) {
			whatIntentsDF$intent[index] <- paste0(substring(whatIntentsDF$intent[index], 1, nchar(whatIntentsDF$intent[index])-3), "y")
		}
		else if (endsWith(whatIntentsDF$intent[index], "s")) {
			whatIntentsDF$intent[index] <- substring(whatIntentsDF$intent[index], 1, nchar(whatIntentsDF$intent[index])-1)
		}
	}

	whatIntentsDF <- cbind(whatIntentsDF, count=1)
	
	whatIntentsAggDF <- aggregate(count ~ intent + code, whatIntentsDF, sum)
	whatIntentsAggDF <- whatIntentsAggDF[order(whatIntentsAggDF$count, decreasing=TRUE),]

	# [SC] Frequency of what intents
	barplot(whatIntentsAggDF$count, names.arg=whatIntentsAggDF$intent, las=2
		, main="Frequency of what intents", ylab="frequency"
		, cex.names=0.8
	)
	grid(nx=15)
	print("Proportions by excluding the long tail:")
	print(sum(subset(whatIntentsAggDF, whatIntentsAggDF$count > 2)$count)/sum(whatIntentsAggDF$count))
	
	##############################################
	## [SC] adjective - intent cooccurence matrix
	
	# [SC] filter by distance to the intent word
	whatAdjectivesDF <- subset(whatAdjectivesDF, whatAdjectivesDF$distance == 1)
	
	# [SC] everything to lower case
	whatAdjectivesDF$intent <- tolower(whatAdjectivesDF$intent)
	whatAdjectivesDF$adjective <- tolower(whatAdjectivesDF$adjective)
	
	# [SC] plurals to singular forms
	for(index in 1:nrow(whatAdjectivesDF)) {
		if (endsWith(whatAdjectivesDF$intent[index], "ies")) {
			whatAdjectivesDF$intent[index] <- paste0(substring(whatAdjectivesDF$intent[index], 1, nchar(whatAdjectivesDF$intent[index])-3), "y")
		}
		else if (endsWith(whatAdjectivesDF$intent[index], "s")) {
			whatAdjectivesDF$intent[index] <- substring(whatAdjectivesDF$intent[index], 1, nchar(whatAdjectivesDF$intent[index])-1)
		}

		if (endsWith(whatAdjectivesDF$adjective[index], "ies")) {
			whatAdjectivesDF$adjective[index] <- paste0(substring(whatAdjectivesDF$adjective[index], 1, nchar(whatAdjectivesDF$adjective[index])-3), "y")
		}
		else if (endsWith(whatAdjectivesDF$adjective[index], "s")) {
			whatAdjectivesDF$adjective[index] <- substring(whatAdjectivesDF$adjective[index], 1, nchar(whatAdjectivesDF$adjective[index])-1)
		}
	}

	whatAdjectivesDF <- cbind(whatAdjectivesDF, count=1)
	
	whatAdjectivesAggDF <- aggregate(count ~ intent + adjective + code, whatAdjectivesDF, sum)
	whatAdjectivesAggDF <- whatAdjectivesAggDF[order(whatAdjectivesAggDF$count, decreasing=TRUE),]
	
  tempOneDF <- aggregate(count ~ adjective, whatAdjectivesAggDF, sum)
  tempOneDF <- tempOneDF[order(tempOneDF$count, decreasing = TRUE),]
  # [SC] frequency of adjectives in what intents
  barplot(tempOneDF$count, names.arg=tempOneDF$adjective, las=2
        , main=paste0("Frequency of adjectives in what intents"), ylab="frequency"
	      , cex.names=0.8
  )
  grid(nx=15)
	  
  # [TODO] intent meaning entropy measure
  # [TODO] unique adjectives
  tempTwoDF <- aggregate(count ~ intent, whatAdjectivesAggDF, sum)
  tempTwoDF <- tempTwoDF[order(tempTwoDF$count, decreasing=TRUE),]
  # [SC] how frequent are adjective for intents
  barplot(tempTwoDF$count, names.arg=tempTwoDF$intent, las=2
	        , main=paste0("Frequency of what intents with adjectives"), ylab="frequency"
	        , cex.names=0.8
  )
  grid(nx=15)
  
  # [SC] build intent-adjective co-occurence matrix
  intents <- unique(whatAdjectivesAggDF$intent)
  adjectives <- unique(whatAdjectivesAggDF$adjective)
  aiCoocM <- matrix(data=0, nrow=length(adjectives), ncol=length(intents), dimnames=list(adjectives, intents))
  for(index in 1:nrow(whatAdjectivesAggDF)){
    currRow = whatAdjectivesAggDF[index,]
    aiCoocM[currRow$adjective, currRow$intent] = currRow$count
  }
  	
  longData<-melt(aiCoocM)
  longData<-longData[longData$value!=0,]
  # [SC] print is necessary if ggplotn is inside a loop
  # [SC] adjective-intent co-occurence visualization
  print(ggplot(longData, aes(x = Var1, y = Var2)) +
    geom_raster(aes(fill=value)) +
    #scale_fill_gradient(low="grey90", high="red") +
    labs(x="Adjective", y="Intent", title=paste0("Adjective - Intent Co-occurence")) +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=90, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11)))
  	
  # [SC] change frequencies to proportions
  for(colIndex in 1:ncol(aiCoocM)){
    if (sum(aiCoocM[,colIndex]) != 0) {
      aiCoocM[,colIndex] <- aiCoocM[,colIndex]/sum(aiCoocM[,colIndex])
    }
  }
  write.table(aiCoocM, paste0(source, "intentAdjCooc.txt"), quote=FALSE, sep=";", row.names=TRUE, col.names=TRUE)
	
	##############################################
	## [SC] co-occurence matrix for object and adjective that modifies the object
	
	objVC <- unique(subset(whatObjectsDF, whatObjectsDF$distance == 1)$object)
	adjVC <- unique(subset(whatObjectsDF, whatObjectsDF$distance > 1)$object)
	adjObjM <- matrix(data=0, nrow=length(adjVC), ncol=length(objVC), dimnames=list(adjVC, objVC))
	
  intentVC <- unique(whatObjectsDF$intent)
  for(intentVal in intentVC) {
    intentDF <- subset(whatObjectsDF, whatObjectsDF$intent == intentVal & whatObjectsDF$distance != 0)
    intentDF <- intentDF[order(intentDF$distance),]
    
    currObj = intentDF[intentDF$distance==1, "object"]
    
    for(index in 1:nrow(intentDF)){
      currRow <- intentDF[index,]
      
      if (currRow$distance > 1) {
        adjObjM[currRow$object, currObj] = adjObjM[currRow$object, currObj] + 1
      }
    }
  }
  
  # [SC] change frequencies to proportions
  toRemoveVC <- numeric()
  for(colIndex in 1:ncol(adjObjM)){
    if (sum(adjObjM[,colIndex]) != 0) {
      adjObjM[,colIndex] <- adjObjM[,colIndex]/sum(adjObjM[,colIndex])
    }
    else {
      toRemoveVC <- c(toRemoveVC, colIndex)
    }
  }
  adjObjM <- adjObjM[,-toRemoveVC]
  write.table(adjObjM, paste0(source, "objAdjCooc.txt"), quote=FALSE, sep=";", row.names=TRUE, col.names=TRUE)
	
	
	##############################################
	## [SC] intent - object cooccurence matrix
	
	# [SC] filter by distance to the intent word
	whatObjectsDF <- subset(whatObjectsDF, whatObjectsDF$distance == 1)

	# [SC] everything to lower case
	whatObjectsDF$intent <- tolower(whatObjectsDF$intent)
	whatObjectsDF$relation <- tolower(whatObjectsDF$relation)
	whatObjectsDF$object <- tolower(whatObjectsDF$object)
	
	# [SC] plurals to singular forms
	for(index in 1:nrow(whatObjectsDF)) {
	  if (endsWith(whatObjectsDF$intent[index], "ies")) {
	    whatObjectsDF$intent[index] <- paste0(substring(whatObjectsDF$intent[index], 1, nchar(whatObjectsDF$intent[index])-3), "y")
	  }
	  else if (endsWith(whatObjectsDF$intent[index], "s")) {
	    whatObjectsDF$intent[index] <- substring(whatObjectsDF$intent[index], 1, nchar(whatObjectsDF$intent[index])-1)
	  }
	  
	  if (endsWith(whatObjectsDF$object[index], "ies")) {
	    whatObjectsDF$object[index] <- paste0(substring(whatObjectsDF$object[index], 1, nchar(whatObjectsDF$object[index])-3), "y")
	  }
	  else if (endsWith(whatObjectsDF$object[index], "s")) {
	    whatObjectsDF$object[index] <- substring(whatObjectsDF$object[index], 1, nchar(whatObjectsDF$object[index])-1)
	  }
	}
	
	whatObjectsDF <- cbind(whatObjectsDF, count=1)
	
	whatObjectsAggDF <- aggregate(count ~ intent + object + code, whatObjectsDF, sum)
	whatObjectsAggDF <- whatObjectsAggDF[order(whatObjectsAggDF$count, decreasing=TRUE),]
	
  tempOneDF <- aggregate(count ~ object, whatObjectsAggDF, sum)
  tempOneDF <- tempOneDF[order(tempOneDF$count, decreasing = TRUE),]
  # [SC] frequency of objects in what intents
  barplot(tempOneDF$count, names.arg=tempOneDF$object, las=2
	        , main=paste0("Frequency of objects in what intents"), ylab="frequency"
	        , cex.names=0.8
  )
  grid(nx=15)
	  
  tempTwoDF <- aggregate(count ~ intent, whatObjectsAggDF, sum)
  tempTwoDF <- tempTwoDF[order(tempTwoDF$count, decreasing = TRUE),]
  # [SC] how frequent are intents with objects
  barplot(tempTwoDF$count, names.arg=tempTwoDF$intent, las=2
	        , main=paste0("Frequency of what intents with objects"), ylab="frequency"
	        , cex.names=0.8
  )
  grid(nx=15)
	  
  # [SC] build intent-object co-occurence matrix
  intents <- unique(whatObjectsAggDF$intent)
  objects <- unique(whatObjectsAggDF$object)
  aiCoocM <- matrix(data=0, nrow=length(objects), ncol=length(intents), dimnames=list(objects, intents))
  for(index in 1:nrow(whatObjectsAggDF)){
	  currRow = whatObjectsAggDF[index,]
	  aiCoocM[currRow$object, currRow$intent] = currRow$count
  }
	  
  longData<-melt(aiCoocM)
  longData<-longData[longData$value!=0,]
  # [SC] print is necessary if ggplotn is inside a loop
  # [SC] intent-object co-occurence visualization
  print(ggplot(longData, aes(x = Var1, y = Var2)) +
	        geom_raster(aes(fill=value)) +
	        #scale_fill_gradient(low="grey90", high="red") +
	        labs(x="Object", y="Intent", title=paste0("Object - Intent Co-occurence")) +
	        theme_bw() + theme(axis.text.x=element_text(size=9, angle=90, vjust=0.3),
	                            axis.text.y=element_text(size=9),
	                            plot.title=element_text(size=11)))
	  
  # [SC] change frequencies to proportions
  for(colIndex in 1:ncol(aiCoocM)){
	  if (sum(aiCoocM[,colIndex]) != 0) {
	    aiCoocM[,colIndex] <- aiCoocM[,colIndex]/sum(aiCoocM[,colIndex])
	  }
  }
	write.table(aiCoocM, paste0(source, "intentObjCooc.txt"), quote=FALSE, sep=";", row.names=TRUE, col.names=TRUE)
	
	##############################################
	## [SC] intent - object relation cooccurence matrix
	
	whatRelationsAggDF <- aggregate(count ~ intent + relation + code, whatObjectsDF, sum)
	whatRelationsAggDF <- whatRelationsAggDF[order(whatRelationsAggDF$count, decreasing=TRUE),]
	  
  tempOneDF <- aggregate(count ~ relation, whatRelationsAggDF, sum)
  tempOneDF <- tempOneDF[order(tempOneDF$count, decreasing = TRUE),]
  # [SC] Frequency of relations for what intents
  barplot(tempOneDF$count, names.arg=tempOneDF$relation, las=2
	        , main=paste0("Frequency of relations for what intents"), ylab="frequency"
	        , cex.names=0.8
  )
  grid(nx=15)
	  
  # [SC] building intent-relation co-occurence matrix
  intents <- unique(whatRelationsAggDF$intent)
  relations <- unique(whatRelationsAggDF$relation)
  aiCoocM <- matrix(data=0, nrow=length(intents), ncol=length(relations), dimnames=list(intents, relations))
  for(index in 1:nrow(whatRelationsAggDF)){
    currRow <- whatRelationsAggDF[index,]
    aiCoocM[currRow$intent, currRow$relation] = currRow$count
  }
	  
  longData <- melt(aiCoocM)
  longData <- longData[longData$value!=0,]
  # [SC] print is necessary if ggplotn is inside a loop
  # [SC] visualizing intent-relation co-occurence matrix
  print(ggplot(longData, aes(x = Var1, y = Var2)) +
	      geom_raster(aes(fill=value)) +
	      #scale_fill_gradient(low="grey90", high="red") +
	      labs(x="Intent", y="Relation", title=paste0("Relation - Intent Co-occurence")) +
	      theme_bw() + theme(axis.text.x=element_text(size=9, angle=90, vjust=0.3),
	                          axis.text.y=element_text(size=9),
	                          plot.title=element_text(size=11)))
	  
  # [SC] change frequencies to proportions
  for(rowIndex in 1:nrow(aiCoocM)){
	  if (sum(aiCoocM[rowIndex,]) != 0) {
      aiCoocM[rowIndex,] <- aiCoocM[rowIndex,]/sum(aiCoocM[rowIndex,])
	  }
  }
  write.table(t(aiCoocM), paste0(source, "intentRelationCooc.txt"), quote=FALSE, sep=";", row.names=TRUE, col.names=TRUE)
	
	##############################################
	## [SC] adjective - object cooccurence matrix
	
	# [SC] merge adjective and object dataframes without losing any rows
	whatAdjObjDF <- merge(whatAdjectivesDF, whatObjectsDF, all=TRUE)
	
	# [SC] add intents that dont have adjectives and objects
	tempDF <- subset(whatIntentsDF, whatIntentsDF$qid %in% setdiff(whatIntentsDF$qid, whatAdjObjDF$qid))
	tempDF <- cbind(tempDF, distance=NA)
	tempDF <- cbind(tempDF, adjective=NA)
	tempDF <- cbind(tempDF, relation=NA)
	tempDF <- cbind(tempDF, object=NA)
	whatAdjObjDF <- rbind(whatAdjObjDF, tempDF)
	
	whatAdjObjAggDF <- aggregate(count ~ adjective + object + code, whatAdjObjDF, sum)
	whatAdjObjAggDF <- whatAdjObjAggDF[order(whatAdjObjAggDF$count, decreasing=TRUE),]
	  
  # [SC] building adjective-object co-occurence matrix
  adjectives <- unique(whatAdjObjAggDF$adjective)
  objects <- unique(whatAdjObjAggDF$object)
  aiCoocM <- matrix(data=0, nrow=length(adjectives), ncol=length(objects), dimnames=list(adjectives, objects))
  for(index in 1:nrow(whatAdjObjAggDF)){
	  currRow <- whatAdjObjAggDF[index,]
	  aiCoocM[currRow$adjective, currRow$object] = currRow$count
  }
	  
  longData <- melt(aiCoocM)
  longData <- longData[longData$value!=0,]
  # [SC] print is necessary if ggplotn is inside a loop
  # [SC] visualizing adjective-object co-occurence matrix
  print(ggplot(longData, aes(x = Var1, y = Var2)) +
	        geom_raster(aes(fill=value)) +
	        #scale_fill_gradient(low="grey90", high="red") +
	        labs(x="Adjective", y="Object", title=paste0("Adjective - Object Co-occurence")) +
	        theme_bw() + theme(axis.text.x=element_text(size=9, angle=90, vjust=0.3),
	                            axis.text.y=element_text(size=9),
	                            plot.title=element_text(size=11)))
	
	##############################################
	## [SC] network of adjective-intent-relation-objects
	
	# [SC] creating a list of nodes
	nodesDF <- data.frame(term=unique(whatAdjectivesDF$adjective), type="adjective", color="orange")
	nodesDF <- rbind(nodesDF, data.frame(term=unique(whatObjectsDF$object), type="object", color="cyan"))
	nodesDF <- rbind(nodesDF, data.frame(term=unique(c(whatIntentsDF$intent,whatObjectsDF$intent,whatAdjectivesDF$intent)), type="intent", color="green"))
	nodesDF <- rbind(nodesDF, data.frame(term=unique(whatObjectsDF$relation), type="relation", color="purple"))
	nodesDF <- cbind(nodesDF, id=1:nrow(nodesDF))
	nodesDF <- nodesDF[,c(4,1,2,3)]
	
	# [SC] creating a list of edges
	edgesDF <- data.frame(Source=NA, Target=NA, Freq=NA, Type=NA, IntentId=NA)
	
	intentNodesDF <- subset(nodesDF, nodesDF$type=="intent")
	
	# [SC] create adjective-intent edges
	adjNodesDF <- subset(nodesDF, nodesDF$type=="adjective")
	aggrDF <- aggregate(count ~ intent + adjective, whatAdjectivesDF, sum)
	for(rowIndex in 1:nrow(aggrDF)){
	  intentId <- intentNodesDF[intentNodesDF$term == aggrDF$intent[rowIndex],"id"]
	  adjId <- adjNodesDF[adjNodesDF$term == aggrDF$adjective[rowIndex],"id"]
	  
	  edgesDF <- rbind(edgesDF, data.frame(Source=adjId, Target=intentId, Freq=aggrDF$count[rowIndex], Type=1, IntentId=intentId))
	}
	
	# [SC] create intent-object edges
	objNodesDF <- subset(nodesDF, nodesDF$type=="object")
	aggrDF <- aggregate(count ~ intent + object, whatObjectsDF, sum)
	for(rowIndex in 1:nrow(aggrDF)){
	  intentId <- intentNodesDF[intentNodesDF$term == aggrDF$intent[rowIndex],"id"]
	  objId <- objNodesDF[objNodesDF$term == aggrDF$object[rowIndex],"id"]
	  
	  edgesDF <- rbind(edgesDF, data.frame(Source=intentId, Target=objId, Freq=aggrDF$count[rowIndex], Type=2, IntentId=intentId))
	}
	
	# [SC] create intent-relation and relation-object edges
	relNodesDF <- subset(nodesDF, nodesDF$type=="relation")
	aggrDF <- aggregate(count ~ intent + relation + object, whatObjectsDF, sum)
	for(rowIndex in 1:nrow(aggrDF)){
	  intentId <- intentNodesDF[intentNodesDF$term == aggrDF$intent[rowIndex],"id"]
	  relId <- relNodesDF[relNodesDF$term == aggrDF$relation[rowIndex],"id"]
	  objId <- objNodesDF[objNodesDF$term == aggrDF$object[rowIndex],"id"]
	  
	  edgesDF <- rbind(edgesDF, data.frame(Source=intentId, Target=relId, Freq=aggrDF$count[rowIndex], Type=3, IntentId=intentId))
	  edgesDF <- rbind(edgesDF, data.frame(Source=relId, Target=objId, Freq=aggrDF$count[rowIndex], Type=4, IntentId=intentId))
	}
	
	edgesDF <- edgesDF[-1,]
	#edgesDF <- aggregate(Freq ~ Source + Target + IntentId + Type, edgesDF, sum)
	
	# [SC] create a network of adjective-intent tuples
	net <- graph_from_data_frame(d=subset(edgesDF, edgesDF$Type == 1), vertices=subset(nodesDF, nodesDF$type != "relation" & nodesDF$type != "object"), directed=T)
	net <- simplify(net, remove.multiple=T, remove.loops=T, edge.attr.comb=c(Freq="sum"))
	# [SC] plot the network
	pdf(paste0(source, "images/adjective-intent_tuples.pdf"), width = 30, height = 30)
	l <- layout_with_fr(net)
	plot(net, vertex.size=5, edge.arrow.width=0.3, vertex.label=V(net)$term, edge.width=E(net)$Freq, layout=l)
	dev.off()
	
	# [SC] create a network of intent-object tuples
	net <- graph_from_data_frame(d=subset(edgesDF, edgesDF$Type == 2), vertices=subset(nodesDF, nodesDF$type != "relation" & nodesDF$type != "adjective"), directed=T)
	net <- simplify(net, remove.multiple=T, remove.loops=T, edge.attr.comb=c(Freq="sum"))
	# [SC] plot the network
	pdf(paste0(source, "images/intent-object_tuples.pdf"), width = 30, height = 30)
	l <- layout_with_fr(net)
	plot(net, vertex.size=5, edge.arrow.width=0.3, vertex.label=V(net)$term, edge.width=E(net)$Freq, layout=l)
	dev.off()
	
	# [SC] create a network of intent-relation-object tuples
	net <- graph_from_data_frame(d=subset(edgesDF, !(edgesDF$Type %in% c(1,2))), vertices=subset(nodesDF, nodesDF$type != "adjective"), directed=T)
	net <- simplify(net, remove.multiple=T, remove.loops=T, edge.attr.comb=c(Freq="sum"))
	# [SC] plot the network
	pdf(paste0(source, "images/intent-relation-object_tuples.pdf"), width = 30, height = 30)
	l <- layout_with_fr(net)
	plot(net, vertex.size=5, edge.arrow.width=0.3, vertex.label=V(net)$term, edge.width=E(net)$Freq, layout=l)
	dev.off()
	
	# [SC] create a network of adjective-intent-object tuples
	net <- graph_from_data_frame(d=subset(edgesDF, edgesDF$Type %in% c(1,2)), vertices=subset(nodesDF, nodesDF$type != "relation"), directed=T)
	net <- simplify(net, remove.multiple=T, remove.loops=T, edge.attr.comb=c(Freq="sum"))
	# [SC] plot the network
	pdf(paste0(source, "images/adjective-intent-object_tuples.pdf"), width = 30, height = 30)
	l <- layout_with_fr(net)
	plot(net, vertex.size=5, edge.arrow.width=0.3, vertex.label=V(net)$term, edge.width=E(net)$Freq, layout=l)
	dev.off()
	
	# [SC] create a network of adjective-intent-relation-object tuples
	net <- graph_from_data_frame(d=subset(edgesDF, edgesDF$Type != 2), vertices=nodesDF, directed=T)
	net <- simplify(net, remove.multiple=T, remove.loops=T, edge.attr.comb=c(Freq="sum"))
	# [SC] plot the network
	pdf(paste0(source, "images/adjective-intent-relation-object_tuples.pdf"), width = 30, height = 30)
	l <- layout_with_fr(net)
	plot(net, vertex.size=5, edge.arrow.width=0.3, vertex.label=V(net)$term, edge.width=E(net)$Freq, layout=l)
	dev.off()
	
	# [SC] create a network of adjective-intent-relation-object tuples for individual intent
	subEdgesDF <- subset(edgesDF, edgesDF$Type %in% c(1,3,4))
	tempDF <- subset(intentNodesDF, type=="intent")
	for(index in 1:nrow(tempDF)){
	  intentId <- tempDF$id[index]
	  intentSubEdgesDF <- subset(subEdgesDF, subEdgesDF$IntentId == intentId)
	  intentSubNodesDF <- subset(nodesDF, nodesDF$id %in% c(intentSubEdgesDF$Target, intentSubEdgesDF$Source))
	  
	  net <- graph_from_data_frame(d=intentSubEdgesDF, vertices=intentSubNodesDF, directed=T)
	  net <- simplify(net, remove.multiple=T, remove.loops=T, edge.attr.comb=c(Freq="sum"))
	  pdf(paste0(source, "images/", tempDF$term[index], "_tuples.pdf"), width = 7, height = 7)
	  l <- layout_with_fr(net)
	  plot(net, edge.arrow.width=0.3, vertex.label=V(net)$term, edge.width=E(net)$Freq)
	  dev.off()
	}
	
	##############################################
	
	# [SC] estimate how often a specific intent may have certain adjective/object structure
	adjObjStatsDF <- data.frame(intent=NA, All=NA, Solo=NA, SoloAdj=NA, SoloObj=NA, AdjObj=NA)
	intentsVC <- unique(whatAdjObjDF$intent)
	for(intentVal in intentsVC) {
	  allCount <- nrow(subset(whatAdjObjDF, whatAdjObjDF$intent == intentVal))
	  soloCount <- nrow(subset(whatAdjObjDF, whatAdjObjDF$intent == intentVal & is.na(whatAdjObjDF$adjective) & is.na(whatAdjObjDF$object)))
	  adjCount <- nrow(subset(whatAdjObjDF, whatAdjObjDF$intent == intentVal & !is.na(whatAdjObjDF$adjective) & is.na(whatAdjObjDF$object)))
	  objCount <- nrow(subset(whatAdjObjDF, whatAdjObjDF$intent == intentVal & is.na(whatAdjObjDF$adjective) & !is.na(whatAdjObjDF$object)))
	  adjObjCount <- nrow(subset(whatAdjObjDF, whatAdjObjDF$intent == intentVal & !is.na(whatAdjObjDF$adjective) & !is.na(whatAdjObjDF$object)))
	  
	  adjObjStatsDF <- rbind(adjObjStatsDF, data.frame(intent=intentVal, All=allCount, Solo=soloCount, SoloAdj=adjCount, SoloObj=objCount, AdjObj=adjObjCount))
	}
	adjObjStatsDF <- adjObjStatsDF[-1,]
	adjObjStatsDF <- cbind(adjObjStatsDF, AllAdj=(adjObjStatsDF$SoloAdj + adjObjStatsDF$AdjObj))
	adjObjStatsDF <- cbind(adjObjStatsDF, AllObj=(adjObjStatsDF$SoloObj + adjObjStatsDF$AdjObj))
	
	adjObjPropDF <- data.frame(adjObjStatsDF)
	adjObjPropDF$Solo <- adjObjPropDF$Solo/adjObjPropDF$All
	adjObjPropDF$SoloAdj <- adjObjPropDF$SoloAdj/adjObjPropDF$All
	adjObjPropDF$SoloObj <- adjObjPropDF$SoloObj/adjObjPropDF$All
	adjObjPropDF$AdjObj <- adjObjPropDF$AdjObj/adjObjPropDF$All
	adjObjPropDF$AllAdj <- adjObjPropDF$AllAdj/adjObjPropDF$All
	adjObjPropDF$AllObj <- adjObjPropDF$AllObj/adjObjPropDF$All
	setnames(adjObjPropDF, old = c("Solo", "SoloAdj", "SoloObj", "AdjObj", "AllAdj", "AllObj"), new = c("SoloP", "SoloAdjP", "SoloObjP", "AdjObjP", "AllAdjP", "AllObjP"))
	adjObjStatsDF <- merge(adjObjStatsDF, adjObjPropDF)
	
	adjObjStatsDF <- cbind(adjObjStatsDF, IntentP=adjObjStatsDF$All/sum(adjObjStatsDF$All))
	
	write.table(adjObjStatsDF, paste0(source, "intentStateP.txt"), quote=FALSE, sep=";", row.names=FALSE, col.names=TRUE)
	
	# [SC] printing important stats
	print(adjObjStatsDF)
	
	# [SC] identify intents with definite adjective/object structure
	print("####################### Solos:")
	print(subset(adjObjStatsDF, SoloP == 1)$intent)
	print("####################### SoloAdj: ")
	print(subset(adjObjStatsDF, SoloAdjP == 1)$intent)
	print("####################### SoloObj: ")
	print(subset(adjObjStatsDF, SoloObjP == 1)$intent)
	print("####################### AdjObj: ")
	print(subset(adjObjStatsDF, AdjObjP == 1)$intent)
	
	# [SC] use k-means clusters on intents that have varying adjective/object structure
	cols <- c("SoloP", "SoloAdjP", "SoloObjP", "AdjObjP")
	clusterCount <- 5
	tempDF <- subset(adjObjStatsDF, !(SoloP == 1 | SoloAdjP == 1 | SoloObjP == 1 | AdjObjP == 1))
	rownames(tempDF) <- tempDF$intent
	tempDF <- tempDF[,cols]
	clust <- kmeans(tempDF, clusterCount, iter.max=500)
	# [SC] plot clusters after PCA dimension reduction
	plot(fviz_cluster(clust, tempDF))
	
	# [SC] map individual intents to clusters
	tempDF <- cbind(tempDF, intent=rownames(tempDF))
	tempDF <- merge(tempDF, data.frame(cluster=clust$cluster, intent=names(clust$cluster)))
	#print(tempDF[order(tempDF$cluster),])
	
	# [SC] plot barplot of proportions for each cluster
	subOp <- par(mfrow=c(ceiling(clusterCount/2), 2))
	for(clusterIndex in 1:clusterCount){
	  clusterSubDF <- subset(tempDF, cluster == clusterIndex)
	  avgValues <- numeric()
	  for(col in cols){
	    avgValues <- c(avgValues, mean(clusterSubDF[,col]))
	  }
	  barplot(avgValues, names.arg=cols, las=1
	          , main=paste0(clusterIndex, "-", paste0(clusterSubDF$intent, collapse=",")), ylab="Proportions"
	          , cex.names=1
	          , cex.main=0.8
	  )
	  grid(nx=15)
	}
	par(mfrow=c(1,1))
	
	#par(op)
}

analyzeWhats()