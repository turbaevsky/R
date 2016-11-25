s <- read.csv('~/Desktop/test.csv')
s <- s[[1]]


#s <- toupper(s)
s <- gsub("&|\\s+or\\s+|\\s+and\\s+",",",s)
#s <- gsub("SERIES|INSTALLED|MODEL","",s)
s <- gsub("\\([^\\(||),MoaeA]+\\)","",s) #Remove brackets in the brackets
#s <- s[1:1000]

sourceDB <- function(s){
    d <- c()
    for (line in s){
### Check if there more than one brackets
        while (grepl("\\)",line)[[1]]){
            startLine <- regexpr("\\(",line)
            endLine <- regexpr("\\)",line)
            brackets <- substring(line,startLine,endLine)
            if (grepl(",|\\d",brackets)){
                d <- c(d,substring(line,1,endLine))
                line <- sub("^,","",substring(line,endLine+1))
            }
            if (!grepl(",|\\d",brackets)){
                line <- sub("\\(","[",line)
                line <- sub("\\)","]",line)
            }
            #else d <- c(d,line)
        }}
    return(d)
}

trans <- function(d){
    r <- c()
### Working with types
    for (l in d){
        st <- regexpr("\\(",l)
        en <- regexpr("\\)",l)
        sstr <- substring(l,st+1,en-1)
        main <- trimws(substring(l,1,st-1),'l')
                                        #print(paste(d,main,sstr,sep='|'))
                                        # Work with substring #
        if (grepl(",",sstr) || grepl("/",sstr)){
            if (grepl(",",sstr)) types <- strsplit(sstr,",")
            if (grepl("/",sstr) && !(grepl(",",sstr))) types <- strsplit(sstr,"/")
            subt <- c()
            for (t in types[[1]]){
                t <- trimws(t)
                subt <- c(subt,t)
                if (grepl("^-",t)){
                    smain <- sub("-.*","",subt[1])
                    r <- c(r,paste(main,smain,t,sep=''))
                                        #print(paste(main,smain,t))
                }
                else r <- c(r,paste(main,t,sep=''))
            }
            subt <- c()
        }
        else r <- c(r,paste(main,sstr,sep=''))
    }
    return(r)
}

### main ###
print('Working with source DB...')
d <- sourceDB(s)
print('Working with transformed DB...')
r <- trans(d)
write.csv(r,'intResilt.csv')
write.csv(as.data.frame(table(r)),'super.csv')
