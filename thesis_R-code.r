##R-kód k diplomovej práci "Evaluácia zrozumiteľnosti zhlukovania" (2018)- Bc. Oliver Genský
##popis kódu je v diplomovej práci VŠE
##účelom tohoto súboru je zjednodušenie opakovania úkonov z práce tým, že kód nieje nutné prepisovať, ale je možné ho aplikovať pomocou copy-paste

##3.4.2.	Vloženie dát to nástroja R

	delim = ","  # "\t" for tabulator
	dec = "."    # ","  for comma
	myDataFrame <- read.csv("C:/Users/Gensky/Diplomka/CSVs/telco.csv", header=TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)

##3.4.3.	Potrebné knižnice
	packages <-     c("ggplot2", "psych", "tidyr", "sqldf", "scales", 
					"NbClust","factoextra","mclust","clusterCrit","kmed", 
					"ggpubr","cluster","kmed","clusterCrit","dplyr","plyr", 
					"rpart.plot","Gmedian","psych")
	
	install.packages (packages)
	
	lapply(packages, library, character.only = TRUE)
	
	library("clusterCrit")
	library("NbClust")
	library("mclust")
	library("psych")
	library("tidyr")
	library("ggplot2")
	library("sqldf")
	library("kmed")
	library("ggpubr")
	library("factoextra")
	library("cluster")
	library("kmed")
	library("clusterCrit")
	library("dplyr")
	library("plyr")
	library("rpart.plot")
	library("Gmedian")
	library("psych")

##3.4.4.	Zobrazenie dát
	dataDescription <- describe(myDataFrame)
	dataDescription <- format(dataDescription, decimal.mark=",")
	write.table(dataDescription, file = 
	"C:/Users/Gensky/Diplomka/CSVs/dataDescription.csv",
	sep = "\t", row.names = TRUE, col.names = TRUE)
	
	#iba numerické data
	myDataFrameNumericOnly <- unlist(lapply(myDataFrame, is.numeric))  
	myDataFrameNumeric <- myDataFrame[ , myDataFrameNumericOnly]
	
	#zobrazenie grafu
	myDataFrameNumeric %>% gather() %>% head()
	ggplot(gather(myDataFrameNumeric), aes(value)) + 
       geom_histogram(bins = 20) + 
       facet_wrap(~key, scales = 'free_x')

	pairs.panels(myDataFrameNumeric[ , c(-1)])
	
##3.5.1.	Výber vhodných atribútov
	myDataFrameNums <- sqldf("SELECT cast(REPLACE([phone.number],'-','') 
							as decimal(16.2)) as phone_number, * ,
								case when [international.plan] = 'yes' then 1 
								else 0 end as international_plan,
								case when [voice.mail.plan] = 'yes' then 1 
								else 0 end as voicemail_plan
							FROM myDataFrame")
							
	myDataFrameNums [colnames(myDataFrameNums)=="state"] <- NULL
	myDataFrameNums [colnames(myDataFrameNums)=="area.code"] <- NULL
	myDataFrameNums [colnames(myDataFrameNums)=="account.length"] <- NULL
	myDataFrameNums [colnames(myDataFrameNums)=="churn"] <- NULL
	myDataFrameNums [colnames(myDataFrameNums)=="international.plan"] <- NULL
	myDataFrameNums [colnames(myDataFrameNums)=="voice.mail.plan"] <- NULL
	
	myDataFrameNums [colnames(myDataFrameNums)=="total.day.charge"] <- NULL
	myDataFrameNums [colnames(myDataFrameNums)=="total.eve.charge"] <- NULL
	myDataFrameNums [colnames(myDataFrameNums)=="total.night.charge"] <- NULL
	myDataFrameNums [colnames(myDataFrameNums)=="total.intl.charge"] <- NULL
	myDataFrameNums [colnames(myDataFrameNums)=="voicemail_plan"] <- NULL
	
##3.5.2.	Vysporiadanie sa s chýbajúcimi hodnotami
	sapply(myDataFrameNums, function(x) sum(is.na(x)))
	
	myDataFrameOmited <- na.omit(myDataFrameNums)	
	
##3.5.3.	Štandardizácia
	myDataFrameStandardized <- myDataFrameNums
	myDataFrameStandardized[, -c(1)] <- scale(myDataFrameNums[, -c(1)])

	myDataFrameStandardized %>% gather() %>% head()
	ggplot(gather(myDataFrameStandardized), aes(value)) + 
		geom_histogram(bins = 20) + 
		facet_wrap(~key, scales = 'free_x')

##3.5.4.	Použitie váh pre zohľadnenie empirickej znalosti
	myDataFrameStandardized$international_plan <- myDataFrameStandardized$international_plan/10
	myDataFrameStandardized$number.vmail.messages <- myDataFrameStandardized$number.vmail.messages/1.05
	myDataFrameStandardized$customer.service.calls <- myDataFrameStandardized$customer.service.calls /1.05
	
##3.5.5.	Rozdelenie vstupných dát na testovaciu a trénovaciu časť
	train_ind <- sample(seq_len(nrow(myDataFrameStandardized)), 3000)
	trainMyDataFrameSt <- myDataFrameStandardized[train_ind, ]
	testMyDataFrameSt <- myDataFrameStandardized [-train_ind, ]
	# testovacia časť sa bude vytvárať ako podmnožina trénovacej

##3.6.1.	Výber vhodného počtu zhlukov

##3.6.2.1.	K-means
	set.seed(20)
	clusters <- kmeans(trainMyDataFrameSt[, 2:12], 6,algorithm = "Hartigan-Wong")
	trainMyDataFrameSt$seg <- as.factor(clusters$cluster)
	str(clusters) #informácie o vytvorených zhlukoch
	
	set.seed(20)
	for(i in 2:8) {
	nam <- paste("clusters_", i, sep = "")
	assign(nam, kmeans(trainMyDataFrameSt[, 2:12],i,algorithm = "Hartigan-Wong"))
	}

##3.6.2.2.	K-medians
	set.seed(20)
	clusters <- kGmedian(trainMyDataFrameSt[, 2:12], ncenters=6)
	trainMyDataFrameSt$seg <- as.factor(clusters$cluster)
	
	set.seed(20)
	for(i in 2:8) {
		nam <- paste("clusters_", i, sep = "")
		assign(nam, kGmedian(trainMyDataFrameSt[, 2:12], ncenters=i))
	}

##3.6.2.3.	K-medoids
	num     <- as.matrix(trainMyDataFrameSt[,2:12])
	mrwdist <- distNumeric(num, num, method = "mrw") 
	#Dostupné vzdialenostné metriky: 
	-Manhattan weighted by rank ("mrw")
	-Squared Euclidean weighted by variance ("sev")
	-Squared Euclidean weighted by rank ("ser")
	-Squared Euclidean ("se")
	# mrwdist[1:11,1:11] #len zobrazenie
	clusters <- fastkmed(mrwdist, ncluster = 2, iterate = 10)
	trainMyDataFrameSt$seg <- as.factor(result$cluster) #vloženie do datasetu
	
	
	set.seed(20)
	num <- as.matrix(trainMyDataFrameSt[,2:12])
	mrwdist <- distNumeric(num, num, method = "mrw")  
	for(i in 2:8) {
	nam <- paste("clusters_", i, sep = "")
	assign(nam, fastkmed(mrwdist, ncluster = i, iterate = 10))
	}
	rm(num)
	rm(mrwdist)

##3.6.2.4.	K-modes
	set.seed(20)
	clusters <- list()
	clusters$cluster <- kmodes(trainMyDataFrameSt[, 2:12],6)
	trainMyDataFrameSt$seg <- as.factor(clusters$cluster)
	
	set.seed(20)
	for(i in 2:8) {
	nam <- paste("clusters_", i, sep = "")
	tmplist <- list()
	tmplist[['cluster']] <- kmodes(trainMyDataFrameSt[, 2:12],i)
	assign(nam, tmplist)
	}
	rm(tmplist
	
##3.6.2.5.	Hierarchické zhlukovanie
	clusters <- hclust(dist(trainMyDataFrameSt[, 2:12]), method = 'ward.D2')
	#dostupné metódy výpočtu vzdialeností:"ward.D2","mcquitty","single","complete","centroid","average".
	
	clusterCut <- cutree(clusters, 6)
	trainMyDataFrameSt$seg <- as.factor(clusterCut $cluster)
	#ďalej len info
	str(clusters) 
	table(clusterCut)
	table(clusterCut, trainMyDataFrameSt$international_plan)
	
	
	set.seed(20)
	clusters <- hclust(dist(trainMyDataFrameSt[, 2:12]), method = 'ward.D2')
	for(i in 2:8) {
			nam <- paste("clusters_", i, sep = "")
			clusters_tmp <- list("cluster" = 0)
			clusters_tmp$cluster <- cutree(clusters, i)
			assign(nam, clusters_tmp)
	}
	
##3.6.2.6.	GMM
	BIC <- mclustBIC(trainMyDataFrameSt[, 2:12])
	# plot(BIC)
	mod1 <- Mclust(myDataFrameStandardized[, -c(1)], x = BIC)
	summary(mod1, parameters = TRUE)
	# plot(mod1, what = "classification")
	
	set.seed(20)
	for(i in 2:8) {
			nam <- paste("clusters_", i, sep = "")
			mclust_tmp <-Mclust(trainMyDataFrameSt [, 2:12], G=i)
			mclust_tmp$classification <- as.integer(unname(mclust_tmp$classification))
			names(mclust_tmp)[14]<-"cluster"
			assign(nam, mclust_tmp)
			rm(mclust_tmp)
	}
	
##3.6.3.1.	Interné metriky
	#FÁZA 1:
	set.seed(20)
	IntEvaluation <- data.frame("Criterium" = c("Min_Max_Size_Diff","C_index","Dunn","Silhouette","Davies_Bouldin","WWS"), 
					"2" = 0, "3" = 0,"4" = 0, "5" = 0, "6" = 0, "7" = 0, "8" = 0,    stringsAsFactors = FALSE)
	
	#FÁZA 2:
	for(i in 2:8) {
			nam_train <- paste("clusters_", i, sep = "")
			original_cluster_tmp <- get(nam_train, envir = .GlobalEnv)
			
			intIdx<-list()
			intIdx[1] <-as.numeric(max(table(original_cluster_tmp$cluster))-   min(table(original_cluster_tmp$cluster))) 
			
			intIdx[2:6] <-intCriteria(data.matrix(trainMyDataFrameSt[,2:12]), 
					as.integer(original_cluster_tmp$cluster), 
					c("C_index","Dunn","Silhouette","Davies_Bouldin","trace_w"))
					
			IntEvaluation[,i] <- as.factor(unlist(intIdx, use.names=FALSE))
			colnames(IntEvaluation)[i] <- i
	
	#PREMAZANIE DOČASNÝCH OBJEKTOV:        
			rm(original_cluster_tmp)
			rm(intIdx)
	}

	
	
	IntEvaluation <- format(IntEvaluation, decimal.mark=",")
	write.table(IntEvaluation, 
				file = "C:/Users/Gensky/Diplomka/CSVs/ IntEvaluation.txt", 
				sep = "\t", row.names = FALSE, col.names = TRUE)
				
##3.6.3.2.	Externé metriky
	#FÁZA 1:
	set.seed(20)
	ExtEvaluation <- data.frame("Criterium" = c("Czekanowski_Dice","Precision","Rand","Jaccard"), "2" = 999, "3" = 999,"4" = 999, "5" = 999, "6" = 999, "7" = 999, "8" = 999,      stringsAsFactors = FALSE)
	
	#FÁZA 2 :
	for(i in 2:8) {
	for(j  in 1:5) {
	
	#vytvorenie testovacej vzorky
	smp_size <- floor(0.80* nrow(trainMyDataFrameSt))
	test_ind <- sample(seq_len(nrow(trainMyDataFrameSt)), size = smp_size)
	testMyDataFrameSt <- trainMyDataFrameSt[test_ind, ]
	
	#vytiahnutie zhlukovaní z globálneho prostredia 
	nam_train <- paste("clusters_", i, sep = "")
	original_cluster_tmp <- get(nam_train, envir = .GlobalEnv)
	
	#zhlukovanie, treba prehodiť na požadovaný algoritmus
	#GMMs      
	mclust_tmp  <-Mclust(testMyDataFrameSt [, 2:12], G=i)
	mclust_tmp$classification <- as.integer(unname(mclust_tmp$classification))
	testMyDataFrameSt$seg <- as.factor(mclust_tmp$classification)
	
	
			#K-means
				#kmeans_tmp <-kmeans(testMyDataFrameSt[, 2:12], i)
				#testMyDataFrameSt$seg <- as.factor(kmeans_tmp$cluster)
			#K-medoid
				#num <- as.matrix(testMyDataFrameSt[,2:12])
				#mrwdist <- distNumeric(num, num, method = "mrw") 
				#kmedoid_tmp <- fastkmed(mrwdist, ncluster = i, iterate = 10)
				#testMyDataFrameSt$seg <- as.factor(kmedoid_tmp$cluster)
				#rm(num)
				#rm(mrwdist)
			#HClust
				#clusters <- hclust(dist(testMyDataFrameSt[, 2:12]), 
				method = 'ward.D2')
				#testMyDataFrameSt$seg <- cutree(clusters, i)
				#rm(clusters)
			#K-median
				#kmedian_tmp <- kGmedian(testMyDataFrameSt [, 2:12], ncenters=i)
				#testMyDataFrameSt$seg <- as.factor(kmedian_tmp$cluster)
	
	
	
	#FÁZA 3:
	#úprava tréningových dát tak, aby objekt v testovacích dátach bol v rovnakom riadku v trenovacich
	trainMyDataFrameSt1 <- trainMyDataFrameSt
	trainMyDataFrameSt1$seg <- as.factor(original_cluster_tmp $cluster) 
	trainMyDataFrameSt2 <- trainMyDataFrameSt1[(trainMyDataFrameSt1$phone_number %in% testMyDataFrameSt$phone_number),]
	testMyDataFrameSt <- testMyDataFrameSt[order(match(testMyDataFrameSt[,1], trainMyDataFrameSt2 [,1])),]
	
	#vytvorenie vektorov
	test_cluster_tmp <- pull(testMyDataFrameSt, seg)
	test_cluster_tmp <- as.integer(as.vector(test_cluster_tmp))
	train_cluster_tmp <- pull(trainMyDataFrameSt2, seg)
	train_cluster_tmp <- as.integer(as.vector(train_cluster_tmp))
	
	#výpočet externých validačných hodnôt    
	extIdx <- extCriteria(train_cluster_tmp, test_cluster_tmp, c("Czekanowski_Dice","Precision","Rand","Jaccard"))
	extIdx <- pmin(as.numeric(as.character(ExtEvaluation[,i])) , as.numeric(as.character(extIdx)))
	ExtEvaluation[,i] <- as.factor(unlist(extIdx, use.names=FALSE))
	colnames(ExtEvaluation)[i] <- i
	
	#premazanie dočasných objektov
	rm(test_cluster_tmp)
	rm(train_cluster_tmp)
	rm(original_cluster_tmp) 
	rm(testMyDataFrameSt)
	rm(extIdx)
	rm(original_cluster_tmp) 
	rm(nam_train)
	} }

	
	ExtEvaluation <- format(ExtEvaluation, decimal.mark=",")
	write.table(ExtEvaluation, 
            file = "C:/Users/Gensky/Diplomka/CSVs/ExtEvaluation.txt", 
            sep = "\t", row.names = FALSE, col.names = TRUE)

##3.7.1.1.	Z-skóre
	#FÁZA 1 (Z-skóre matica):
	Z <- data.frame(matrix(0, nrow=(ncol(trainMyDataFrameSt)-2), ncol=nlevels(trainMyDataFrameSt[,'seg'])))
	rownames(Z) <- colnames(trainMyDataFrameSt[,-c(1,ncol(trainMyDataFrameSt))])
	colnames(Z) <- levels(trainMyDataFrameSt[,'seg'])
	for(j in 1:nlevels(trainMyDataFrameSt[,'seg'])) {
			for(i in 2:(ncol(trainMyDataFrameSt)-1))    {
				sd_all      <- sd(trainMyDataFrameSt[,i])
				mean_all    <- mean(trainMyDataFrameSt[,i])
				mean_clust  <- mean(trainMyDataFrameSt[trainMyDataFrameSt$seg == j,i])
			
				Z[i-1,j]    <-( mean_clust - mean_all)/sd_all
														}
													}
	###########################################################################
	seg_counts <- as.vector(count(trainMyDataFrameSt, 'seg'))
	###########################################################################
	
	#FÁZA 2 (zobrazenie Z-skóre matice pomocou grafov):
	
	plots <- list()
	for(i in 1:ncol(Z)) {
	df <- data.frame(variable=as.vector(rownames(Z)),Z_score=as.vector(Z[,i]))
	
	Label_name <- paste("Z-scores of Cluster _", i, sep = "")
	Label_name <- paste(Label_name, seg_counts[i,2], sep = "  |Count: ")
		
	df$mpg_grp <- factor(ifelse(df$Z_score < 0, "low", "high"), 
						levels = c("low", "high"))
	
	p<-ggbarplot(df, x = "variable", y = "Z_score",
				fill = "mpg_grp",           
				color = "white",            
				palette = "jco",            
				sort.val = "asc",          
				sort.by.groups = FALSE,     
				x.text.angle = 90,          
				ylab = Label_name,
				legend.title = " Group: ",
				rotate = TRUE,
				ggtheme = theme_minimal()
	)
	plots[[i]] <- p
	}
	
	multiplot(plotlist = plots, cols = 2)
						

##3.7.1.2.	Heat mapa
	install.packages("RColorBrewer")
	library("RColorBrewer")
	heatmap(as.matrix(Z), Rowv = NA, Colv = NA, col=rev(brewer.pal(7,"RdYlBu")))

##3.7.1.3.	Porovnanie priemerných hodnôt zhlukov
	#ÚVODNÁ FÁZA:
	myDataFrameNumsCut      <- myDataFrameNums[(myDataFrameNums$phone_number %in% trainMyDataFrameSt$phone_number),]
	myDataFrameNumsCut      <- myDataFrameNumsCut[order(match(myDataFrameNumsCut[,1], trainMyDataFrameSt [,1])),]
	myDataFrameNumsCut$seg  <- trainMyDataFrameSt$seg 
	
	#FÁZA 1(NÁPOČET):
	AVGs <- data.frame(matrix(0, nrow=(ncol(myDataFrameNumsCut)-2), ncol=nlevels(myDataFrameNumsCut[,'seg'])))
	rownames(AVGs) <- colnames(myDataFrameNumsCut[,-c(1,ncol(myDataFrameNumsCut))])
	colnames(AVGs) <- levels(myDataFrameNumsCut[,'seg'])
	for(j in 1:nlevels(myDataFrameNumsCut[,'seg'])) {
		for(i in 2:(ncol(myDataFrameNumsCut)-1)) {
		
		mean_clust <- mean(myDataFrameNumsCut[myDataFrameNumsCut$seg == j,i])
		
		AVGs[i-1,j] <- mean_clust
		}
	}
	###########################################################################
	var_means <- colMeans(myDataFrameNumsCut[2:12])
	###########################################################################
	#FÁZA 2 (ZOBRAZENIE):
	plots <- list()
	for(i in 1:nrow(AVGs)) {
	df <- data.frame(cluster = as.vector(colnames(AVGs)), Average =as.vector(t(AVGs[i,])))
	#df <- as.vector(AVGs[,i]))
	Label_name_y <- gsub("\\."," ",rownames(AVGs[i,]))
	Label_name_x <- paste("clusters ", round(var_means[i], digits =2), sep = "| Average:")
		
	df$mpg_grp <- factor(ifelse(df$Average < var_means[i], "Under Average", "Above Average"), 
						levels = c("Under Average", "Above Average"))
	
	p<-ggbarplot(df, x = "cluster", y = "Average",
			fill = "mpg_grp",           
			color = "white",            
			palette = "jco",            
			sort.val = "desc",          
			sort.by.groups = FALSE,     
			x.text.angle = 0,          
			ylab = Label_name_y,
			xlab = Label_name_x,          
			legend.title = " Group: ",
			rotate = FALSE,
	
			ggtheme = theme_minimal()
	)
	plots[[i]] <- p + geom_hline(yintercept= var_means[i])
	}
	
	multiplot(plotlist = plots, cols = 2)
	
##3.7.1.4.	Rozhodovací strom
	fit <- ctree(seg~., data = myDataFrameNumsCut[2:13], maxdepth = 5)
	png("C:/Users/Gensky/Diplomka/Pictures/tree.png", res=80, height=800, width=1600) 
	plot(fit) 
	dev.off()

	
	
##PRÍLOHA A (zdroj: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
	
	# Multiple plot function
	multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	library(grid)
	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)
	numPlots = length(plots)
	
	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
						ncol = cols, nrow = ceiling(numPlots/cols))
	}
	
	if (numPlots==1) {
		print(plots[[1]])
	
	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
	
		# Make each plot, in the correct location
		for (i in 1:numPlots) {
		# Get the i,j matrix positions of the regions that contain this subplot
		matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
	
		print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
										layout.pos.col = matchidx$col))
		}
	}
	}

##PRÍLOHA B
	myDataFrame$total.charge <- myDataFrame$total.day.charge + myDataFrame$total.eve.charge + myDataFrame$total.night.charge + myDataFrame$total.intl.charge
	
	myDataFrame <- sqldf("SELECT cast(REPLACE([phone.number],'-','') 
							as decimal(16.2)) as phone_number, *                          FROM myDataFrame")
	
	myDataFrame2 <- myDataFrame[(myDataFrame$phone_number %in% trainMyDataFrameSt$phone_number),]
	myDataFrame2 <- myDataFrame2[order(match(myDataFrame2$phone_number, trainMyDataFrameSt$phone_number)),]
	
	myDataFrame2$seg <- trainMyDataFrameSt$seg
	
	myDataFrame2<- format(myDataFrame2, decimal.mark=",")
	write.table(myDataFrame2, file = "C:/Users/Gensky/Diplomka/CSVs/ myDataFrame2.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
