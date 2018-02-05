## Convert Corpus of Contemporary English (COCA) DB format to CWB format for CQPweb import
## Author: Fabian Vetter
##
## This script converts BYU COCA in db format to xml for CQPweb
## Output: 1 XML file per inputfile (all stored in folder output) + 1 file containing meta data (cocameta17.txt)
## Sentence units are added based on ".?!" and PoS-tag zzq (for spoken data)
## PoS tags are converted to upper case
## If you encounter problems with fread and embedded NULs, consider using either read.CSV with skipNul=T or fread in combination with sed (e.g. fread("sed 's/\\0//g' mycsv.csv"))
##
##
## The script requires the following files + directory structure in the working directory
##
## {wd}/subgenreCodes.txt
## {wd}/db			# containing directories & db files from original COCA
## {wd}/lexicon		# containing lexicon file for original COCA
## {wd}/sources		# containing sources file for original COCA
## {wd}/coca2017_sources_qpj	# containing sources file for COCA 2017 update
## {wd}/coca2017_lexicon_qpj	# containing lexicon file for COCA 2017 update
## {wd}/coca2017_db_qpj		# containing directories & db files from COCA 2017 update
##
##
##
## best run this script from unix command line with:
## nohup Rscript coca_conversion.R > coca_conversion.log 2>&1 &
##
## later in CQPweb, consider using the following curl command to update the CWB text position records
## curl -u user:pass -k 'https://SERVER/CQPWEBDIR/CORPUSNAME/execute.php?function=populate_corpus_cqp_positions&locationAfter=index.php%3FthisQ%3DmanageMetadata%26uT%3Dy&uT=y' >> curloutput &



rm(list=ls(all=TRUE))

library(data.table)
setwd("/home/vetterf/coca_conv")
outputdir = paste0(getwd(),"/output")


# load subgenres file
subgenres.file = as.data.table(read.csv(file=paste0(getwd(),"/subgenreCodes.txt"),header=F,sep="\t",quote=""))
colnames(subgenres.file) = c("subGenre","subGenre_titel")

#### FOR COCA original data 
{
    sources.file = as.data.table(read.csv(file=paste0(getwd(),"/sources/coca-sources.txt"),header=T,sep="\t",quote=""))
    sources.file = sources.file[2:nrow(sources.file)]
    lexicon.file = as.data.table(read.csv(file=paste0(getwd(),"/lexicon/lexicon.txt"),header=T,sep="\t",quote=""))
    lexicon.file = lexicon.file[2:nrow(lexicon.file)]
    
    # convert columns
    set(sources.file, j='textID', value=as.integer(as.character(sources.file[['textID']])))
    set(lexicon.file, j='wordID', value=as.integer(as.character(lexicon.file[['wordID']])))
    
    # get file list
    fl = list.files(paste0(getwd(),"/db"),recursive = T)
    
    # loop through files in db
    for(myf in fl){
       
    	print(myf)
        f = paste0(outputdir,"/",myf,".xml")
    		# load DB FILE
    		db.file = as.data.table(read.csv(file=paste0(getwd(),"/db/",myf),sep="\t",header=F))
    		colnames(db.file) = c("textID","ID","wordID")
    		db.file = db.file[wordID!="10"] # remove <p> information; no use in cqpweb
    		
    		# merge db file with lexicon and meta data to create corpus
    		corp = merge(db.file,lexicon.file,by="wordID")
    		corp = merge(corp,sources.file,by="textID")
    		
    		rm("db.file")
    		gc()
    		
    		
    		# convert columns
    		set(corp, j='subGenre', value=as.integer(as.character(corp[['subGenre']])))
    		set(corp, j='year', value=as.integer(as.character(corp[['year']])))
    		
    		# add info on subgenres and decade
    		corp = merge(corp,subgenres.file,by="subGenre",all.x=T)
    		corp$decade = paste0(substr(corp$year,1,3),0)
    		set(corp, j='decade', value=paste0(substr(corp[['decade']],1,3),0))
    		
    		# reorder corpus
    		corp <- corp[lemma!="" & PoS!=""] # remove textID entries like "#00202001\t\t\r\n"
    		
    		setkey(corp,textID,ID)
    		
    		# add sentence units
    		set(corp, j='textIDfac', value=as.factor(corp[['textID']]))
    		corp[['sentID']] <- 0
    		set(corp, j='sentID', value=as.integer(corp[['sentID']]))
    		endpositions = which(corp[['word']] %in% c(".","?","!"))
    		if(endpositions[length(endpositions)] != nrow(corp)){endpositions = c(endpositions,nrow(corp))}
    		startpositions = which(corp[['Pos']] == "zzq")
    		endpositions = sort(c(endpositions,(startpositions-1)))
    		mylen = length(endpositions)
    		corp[endpositions][['sentID']]<-seq(1:mylen) # assign sentence ID
    		# wrap in invisible, not interested in output
    		# add sentence unit information based on PoS tag zzq or '.?!'
    		invisible(lapply(1:length(endpositions),function(x){
    			endpos = endpositions[x]
    			if(x==1){startpos=1
    			} else if(x == length(endpositions)){
    				startpos = endpositions[x-1]+1
    				endpos = nrow(corp)
    			} else
    					startpos = endpositions[x-1]+1
    			newsentID = corp[endpos]$sentID
    			corp[startpos:endpos, sentID := newsentID]
    			}))
    		  
    		corp[["helper"]] <- 2
    		set(corp,j='PoS',value=toupper(corp[['PoS']]))
    		#set(corp, j='output', value=paste0(corp[['word']],'\t',corp[['lemma']],'\t',corp[['PoS']]))
    		# merge word pos and lemma columns for output
    		corp$output = paste0(corp$word,rep("\t",n=nrow(corp)),corp$lemma,rep("\t",n=nrow(corp)),corp$PoS)
    		
    		
    		# it's faster to directly construct output in dt than looping through texts and sentences
    		sentIDcorp = rbindlist(list(corp[endpositions,c(2,14,15)],corp[endpositions,c(2,14,15)]))
    		setkey(sentIDcorp,textID,sentID)
    		sentIDcorp$helper = c(1,3)
    		sentIDcorp$output = c("<s>","</s>")
    		setkey(sentIDcorp,textID,sentID)
    		
    		textIDcorp = corp[c(1,which(diff(corp$textID)!=0),1+which(diff(corp$textID)!=0),nrow(corp))]
    		setkey(textIDcorp,textID,sentID)
    		
    		textIDcorp$helper = c(0,4)
    		textIDcorp$output = c("","</text>")
    		textIDcorp[helper==0]$output <- paste0('<text ','id="',textIDcorp[helper==0]$textID,'" year="',textIDcorp[helper==0]$year,'" decade="',textIDcorp[helper==0]$decade,'" genre="',textIDcorp[helper==0]$genre,'" subgenre="',textIDcorp[helper==0]$subgenre_titel,'" source="',textIDcorp[helper==0]$source,'" title="',textIDcorp[helper==0]$title,'" >')
    		
    		mcorp  = rbindlist(list(corp,sentIDcorp,textIDcorp),fill=T)
        setkey(mcorp,textID,sentID,helper,ID)
        
        
        rm("sentIDcorp","textIDcorp")
        gc()
        
    		
    		xmlheader = '<?xml version="1.0" encoding="ISO-8859-1"?>'
    		write(xmlheader,file=f,append=TRUE)
    		write(mcorp$output,file=f,append=TRUE)
    		
    		
    		# if you want single files for each text, you can use this function
    		# adapt it to suit your needs - and copy to coca 2017 part
    		# invisible(lapply(1:length(levels(corp$textIDfac)),function(x){
    		# 	currtext = levels(corp$textIDfac)[x]
    		#   year= corp[textIDfac == currtext]$year[1]
    		#   genre = corp[textIDfac == currtext]$genre[1]
    		#   subgenre = corp[textIDfac == currtext]$subGenre_titel[1]
    		#   source = corp[textIDfac == currtext]$source[1]
    		#   title = corp[textIDfac == currtext]$title[1]
    		#   texthead <- paste0('<text ','id="',currtext,'" year="',year,'" genre="',genre,'" subgenre="',subgenre,'" source="',source,'" title="',title,'" >')
    		#   
    		#   
    		#   #write(texthead,file=f,append=TRUE)
    		#   
    		#   #sentenceIDs = unique(corp[textIDfac==currtext]$sentID)
    		#   # concat text into vector
    		#   #invisible(lapply(1:length(sentenceIDs),function(y){
    		#    # currsentID = sentenceIDs[y]
    		#   #  write("<s>",file=f,append=TRUE)
    		#   #  write(corp[textIDfac==currtext & sentID == currsentID][["output"]],file=f,append=TRUE)
    		#   #  write("</s>",file=f,append=TRUE)
    		#     #mystring = c("<s>",corp[textIDfac==currtext & sentID == currsentID][["output"]],"</s>")
    		#     #mystring
    		#   #}))
    		#   
    		#   #myvec= c(texthead,mytext,"</text>")
    		#   
    		#   #output text to file
    		#   
    		#   write("</text>",file=f,append=TRUE)
    		#   #close(f)
    		# }))
    		rm("mcorp")
    		gc()
    }
}

#### FOR COCA 2017
{
  sources.file = as.data.table(read.csv(file=paste0(getwd(),"/coca2017_sources_qpj/sources.txt"),header=T,sep="\t",quote=""))
  sources.file = sources.file[2:nrow(sources.file)]
  colnames(sources.file)=c('textID','genre','year','nw','title','author','source','dater','pubInfo')
  lexicon.file = as.data.table(read.csv(file=paste0(getwd(),"/coca2017_lexicon_qpj/lexicon.txt"),header=T,sep="\t",quote=""))
  lexicon.file = lexicon.file[2:nrow(lexicon.file)]
  
  # convert columns
  set(lexicon.file, j='wordID', value=as.integer(as.character(lexicon.file[['wordID']])))
  set(sources.file, j='textID', value=as.integer(as.character(sources.file[['textID']])))
  
  # get file list
  fl = list.files(paste0(getwd(),"/coca2017_db_qpj"),recursive = T)
  # loop through files in db
  for(myf in fl){
    
    
    f = paste0(outputdir,"/",myf,".xml")
    # load DB FILE
    db.file = as.data.table(read.csv(file=paste0(getwd(),"/coca2017_db_qpj/",myf),sep="\t",header=F))
    colnames(db.file) = c("textID","ID","wordID")
    db.file = db.file[wordID!="10"] # remove <p> information; no use in cqpweb
    
    # merge db file with lexicon and meta data to create corpus
    corp = merge(db.file,lexicon.file,by="wordID")
    corp = merge(corp,sources.file,by="textID")
    
    rm("db.file")
    gc()
    
    
    # convert columns
    set(corp, j='subGenre', value=as.integer(as.character(corp[['subGenre']])))
    set(corp, j='year', value=as.integer(as.character(corp[['year']])))
    
    # add info on subgenres and decade
    corp = merge(corp,subgenres.file,by="subGenre",all.x=T)
    corp$decade = paste0(substr(corp$year,1,3),0)
    set(corp, j='decade', value=paste0(substr(corp[['decade']],1,3),0))
    
    # reorder corpus
    corp <- corp[lemma!="" & PoS!=""] # remove textID entries like "#00202001\t\t\r\n"
    
    setkey(corp,textID,ID)
    
    # add sentence units
    set(corp, j='textIDfac', value=as.factor(corp[['textID']]))
    corp[['sentID']] <- 0
    set(corp, j='sentID', value=as.integer(corp[['sentID']]))
    endpositions = which(corp[['word']] %in% c(".","?","!"))
    if(endpositions[length(endpositions)] != nrow(corp)){endpositions = c(endpositions,nrow(corp))}
    startpositions = which(corp[['Pos']] == "zzq")
    endpositions = sort(c(endpositions,(startpositions-1)))
    mylen = length(endpositions)
    corp[endpositions][['sentID']]<-seq(1:mylen) # assign sentence ID
    # wrap in invisible, not interested in output
    # add sentence unit information based on PoS tag zzq or '.?!'
    invisible(lapply(1:length(endpositions),function(x){
      endpos = endpositions[x]
      if(x==1){startpos=1
      } else if(x == length(endpositions)){
        startpos = endpositions[x-1]+1
        endpos = nrow(corp)
      } else
        startpos = endpositions[x-1]+1
      newsentID = corp[endpos]$sentID
      corp[startpos:endpos, sentID := newsentID]
    }))
    
    corp[["helper"]] <- 2
    set(corp,j='PoS',value=toupper(corp[['PoS']]))
	# sometimes crashes; hence the DF syntax below
    #set(corp, j='output', value=paste0(corp[['word']],'\t',corp[['lemma']],'\t',corp[['PoS']]))
    # merge word pos and lemma columns for output
    corp$output = paste0(corp$word,rep("\t",n=nrow(corp)),corp$lemma,rep("\t",n=nrow(corp)),corp$PoS)
    
    
    # it's faster to directly construct output in dt than looping through texts and sentences
    sentIDcorp = rbindlist(list(corp[endpositions,c(2,18,19)],corp[endpositions,c(2,18,19)]))
    setkey(sentIDcorp,textID,sentID)
    sentIDcorp$helper = c(1,3)
    sentIDcorp$output = c("<s>","</s>")
    setkey(sentIDcorp,textID,sentID)
    
    textIDcorp = corp[c(1,which(diff(corp$textID)!=0),1+which(diff(corp$textID)!=0),nrow(corp))]
    setkey(textIDcorp,textID,sentID)
    
    textIDcorp$helper = c(0,4)
    textIDcorp$output = c("","</text>")
    textIDcorp[helper==0]$output <- paste0('<text ','id="',textIDcorp[helper==0]$textID,'" year="',textIDcorp[helper==0]$year,'" decade="',textIDcorp[helper==0]$decade,'" genre="',textIDcorp[helper==0]$genre,'" subgenre="',textIDcorp[helper==0]$subgenre_titel,'" source="',textIDcorp[helper==0]$source,'" title="',textIDcorp[helper==0]$title,'" >')
    
    
    mcorp  = rbindlist(list(corp,sentIDcorp,textIDcorp),fill=T)
    setkey(mcorp,textID,sentID,helper,ID)
    
    
    rm("sentIDcorp","textIDcorp")
    gc()
    
    xmlheader = '<?xml version="1.0" encoding="ISO-8859-1"?>'
    write(xmlheader,file=f,append=TRUE)
    write(mcorp$output,file=f,append=TRUE)

    rm("mcorp")
    gc()
  }
  
}


#### CREATE META DATA
{
  # prepare data from original coca
  subgenres.file$subGenre = as.factor(as.character(subgenres.file$subGenre))
  cocameta = merge(sources.file,subgenres.file,by="subGenre",all.x=T) # some texts don't have subgenre annotated, hence all.x=T
  cocameta$subGenre<-NULL
  colnames(cocameta)[6] <- "subGenre_title" #sry, typo
  cocameta$author <- NA # for later compat.
  
  # load meta data from coca 2017 update
  # IMPORTANT: Consider converting file to UTF8 beforehand and delete empty lines
  sources.file17 = as.data.table(read.csv(file=paste0(getwd(),"/coca2017_sources_qpj/sources.txt"),header=T,sep="\t",quote=""))
  sources.file17 = sources.file17[2:nrow(sources.file17)]
  colnames(sources.file17)=c('textID','genre','year','nw','title','author','source','dater','pubInfo')
  
  # no point in keeping meta data that the old coca doesn't share
  sources.file17$nw <- NULL # will be generated by cqp later anyways
  sources.file17$dater <- NULL 
  sources.file17$pubInfo <- NULL
  sources.file17$subGenre_title <- NA #new meta contains no subgenre info.
  
  
  cocameta17 = rbindlist(list(cocameta,sources.file17)) # some texts don't have subgenre annotated, hence all.x=T
  rm(sources.file17)
  rm(cocameta)
  
  set(cocameta17, j='decade', value=paste0(substr(cocameta17[['year']],1,3),0))
  setcolorder(cocameta17,c("textID","year","decade","genre","subGenre_title","source","title","author"))
  
  
  cocameta17$subGenre_title <- sub(":", "_", cocameta17$subGenre_title)
  
  # replace commas, if desired
  #cocameta17[, (names(cocameta17)) := lapply(.SD, gsub, pattern="[,]", replacement="_")]
  
  # replace nonalphanum characters in subgenre title (non alphanum characters shouldn't be in classification columns)
  cocameta17$subGenre_title <- gsub(patter="[^[:alnum:]_\\s]",replacement="_",cocameta17$subGenre_title)
  
  # delete single and double quotes from table for compatibility reasons
  cocameta17[, (names(cocameta17)) := lapply(.SD, gsub, pattern="[\"\']", replacement="")]
  
  # consider adding quotes if you haven't deleted them beforehand.
  write.table(cocameta17,file=paste0(getwd(),"/cocameta17.txt"),quote=F,na="NA",row.names = F,fileEncoding = "UTF8",sep="\t",col.names=F)
  
  # in CQPweb use the following order for handles:
  # "year","decade","genre","subGenre_title","source","title","author"

}