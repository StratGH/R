library(XML)
library(xml2)
library(tools)
# init
host_name <- 'http://www.gks.ru'

# all stat years and path to it db
years_v <-    c(2003, 2004, 2005, 2006,
                2007, 2008, 2009, 2010,
                2011, 2012, 2013, 2014,
                2015, 2016)
db_names <- c('B03_14', 'B04_14', 'B05_14p', 'B06_14p',
              'B07_14p', 'B08_14p', 'B09_14p', 'B10_14p',
              'B11_14p', 'B12_14p', 'B13_14p', 'B14_14p',
              'B15_14p', 'B16_14p')



years <- data.frame(years_v, db_names)

#load doc with stat data and convert containing table into dataframe
loadGKSData <- function(ref){
    ext <- file_ext(ref)
    if(ext == "doc"){
        ref <- gsub("/%3Cextid%3E/%3Cstoragepath%3E::\\|","",ref)
        getTableFromDoc_doc(ref)
    }else if(ext == "docx"){
        getTableFromDoc(ref)
    }else if(ext == "htm"){
        getTableFromHtm(ref)
    }else
        print("Неподдерживаемый тип источника")
    
}

toLocalEncoding <- function(x, sep=",", quote=TRUE, encoding="utf-8"){
    rawcsv <- tempfile()
    write.csv(x, file = rawcsv)
    result <- read.csv(rawcsv, encoding = "UTF-8")
    unlink(rawcsv)
    result
}

#dialog with user and return reference to needed stat doc
getGKSDataRef <- function(){
    path <- '/bgd/regl/'
    params <- '/?List&Id='
    id <- -1
    year <- readline(prompt = paste("Введите год от", years$years_v[1],
                                    "до", tail(years$years_v, n = 1), " "))
    db_name <- as.character(years$db_names[years_v == year])
    # go through tree until we got a link to doc instead of id in stat db
    while(TRUE){
        url <- paste(host_name, path, db_name, params, id, sep = '')
        download.file(url, destfile = 'test_xml.xml')
        test <- readLines('test_xml.xml')
        xml <- xmlTreeParse(url, useInternalNodes = T)
        names <- xpathSApply(xml, "//name", xmlValue)
        Encoding(names) <- "UTF-8" 
        refs <- xpathSApply(xml, "//ref", xmlValue)
        for(i in 1:length(names))
            print(paste(i, names[i]))
        num <- readline(prompt = "Введите номер ")
        ref <- refs[as.numeric(num)]
        if(substr(ref, 1, 1) != "?")
            return(ref)
        id <- substr(ref, 2, nchar(ref))
    }
}

getTableFromHtm <- function(ref) {
    url <- paste(host_name, ref, sep = "")
    doc <- htmlParse(url, encoding = "Windows-1251")
    if(length(xpathSApply(doc,"//table", xmlValue)) == 0){
        print("Нет таблицы в источнике")
        return()
    }
    
    assign("dataGKS", readHTMLTable(doc, trim = TRUE, which = 1,
                                    stringsAsFactors = FALSE,
                                    as.data.frame = TRUE), .GlobalEnv)
    names(dataGKS) <<- gsub("[\r\n]", "", names(dataGKS))
    dataGKS[, 1] <<- gsub("[\r\n]", "", dataGKS[, 1])
    dataGKS <<- toLocalEncoding(dataGKS)
}

##get tables from doc
getTableFromDoc <- function(word_doc) {
    
    tmpd <- tempdir()
    tmpf <- tempfile(tmpdir=tmpd, fileext=".zip")
    file.copy(word_doc, tmpf)
    unzip(tmpf, exdir=sprintf("%s/docdata", tmpd))
    
    doc <- read_xml(sprintf("%s/docdata/word/document.xml", tmpd))
    
    unlink(tmpf)
    unlink(sprintf("%s/docdata", tmpd), recursive=TRUE)
    
    ns <- xml_ns(doc)
    
    tbls <- xml_find_all(doc, ".//w:tbl", ns=ns)
    
    lapply(tbls, function(tbl) {
        
        cells <- xml_find_all(tbl, "./w:tr/w:tc", ns=ns)
        rows <- xml_find_all(tbl, "./w:tr", ns=ns)
        dat <- data.frame(matrix(xml_text(cells), 
                                 ncol=(length(cells)/length(rows)), 
                                 byrow=TRUE), 
                          stringsAsFactors=FALSE)
        colnames(dat) <- dat[1,]
        dat <- dat[-1,]
        rownames(dat) <- NULL
        assign("dataGKS", dat, .GlobalEnv)
    })
}



getTableFromDoc_doc <- function(word_doc) {
    
    word_dir <- shell('reg query "HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\WINWORD.EXE" /v PATH', intern = TRUE)
    winword_dir <- c(strsplit(word_dir[grep("PATH",word_dir)],"  ")[[1]])
    winword_dir <- winword_dir[length(winword_dir)]
    
    
    tmpd <- tempdir()
    tmpf2_1 <- tempfile(tmpdir=tmpd, fileext=".doc")
    download.file(paste(host_name, word_doc,sep=""),
                  tmpf2_1, mode = "wb")
    tmpf2_2 <- tempfile(tmpdir=tmpd, fileext=".docx")
    
    conv_to_docx <- paste('"',winword_dir, 'wordconv.exe" -oice -nme ',tmpf2_1, ' ',tmpf2_2, sep="")
    shell(conv_to_docx)#code 5 
    tmpf <- tempfile(tmpdir=tmpd, fileext=".zip")
    file.copy(tmpf2_2, tmpf)
    
    unzip(tmpf, exdir=sprintf("%s/docdata", tmpd))
    
    doc <- read_xml(sprintf("%s/docdata/word/document.xml", tmpd))
    
    unlink(tmpf)
    unlink(tmpf2_1)
    unlink(tmpf2_2)
    unlink(sprintf("%s/docdata", tmpd), recursive=TRUE)
    
    ns <- xml_ns(doc)
    
    tbls <- xml_find_all(doc, ".//w:tbl", ns=ns)
    
    lapply_result <- lapply(tbls, function(tbl) {
        
        cells <- xml_find_all(tbl, "./w:tr/w:tc", ns=ns)
        rows <- xml_find_all(tbl, "./w:tr", ns=ns)
        dat <- data.frame(matrix(xml_text(cells), 
                                 ncol=(length(cells)/length(rows)), 
                                 byrow=TRUE), 
                          stringsAsFactors=FALSE)
        colnames(dat) <- dat[1,]
        dat <- dat[-1,]
        rownames(dat) <- NULL
        return(dat)
    })
    if (length(lapply_result)>1){
      dataGKS <- lapply_result[[1]]
      for (i in 2:length(lapply_result)){
        dataGKS <- rbind(dataGKS,lapply_result[[i]])
      }
      assign("dataGKS", dataGKS, .GlobalEnv)
    }else{
      assign("dataGKS", lapply_result, .GlobalEnv)
    }
    
}

Add_OKATO <- function(dann_frame_main, id){
    dann_frame <- dann_frame_main
    dann_frame[,id] <- toupper(dann_frame[,id])
    Subj_table <- read.table(file = "subjects.csv", sep=",",col.names=c("Full_name","OKATO","Short_name"))
    OKATO <- c()
    for (i in 1:nrow(dann_frame[id])){
        
        ma <- FALSE
        fed_okr <- grepl("ФЕДЕРАЛЬНЫЙ",as.character(dann_frame[i,id]))||
          grepl("ДОЛГАНО-НЕНЕЦКИЙ",as.character(dann_frame[i,id]))||
          grepl("КОМИ-ПЕРМЯЦКИЙ",as.character(dann_frame[i,id]))
        if (!fed_okr)
            for (j in 1:length(Subj_table$Short_name)){
                if (as.character(Subj_table$Short_name[j])=='НЕНЕЦКИЙ'){
                    yane <- grepl('ЯМАЛО-НЕНЕЦКИЙ',as.character(dann_frame[i,id]))
                    if (yane){
                        ma <- FALSE
                    }else ma <- grepl(as.character(Subj_table$Short_name[j]),as.character(dann_frame[i,id]))
                }else ma <- grepl(as.character(Subj_table$Short_name[j]),as.character(dann_frame[i,id]))
                
                if (ma){
                    OKATO <- c(OKATO, as.character(Subj_table$OKATO[j]))
                    break;
                }
            }
        if (!ma){
            OKATO <- c(OKATO, NA)
        }
    }
    
    new_dann_frame <- data.frame(dann_frame_main,OKATO)
    return(new_dann_frame)
}


info_region <- function(dann,ind_name = "",id = 1){
  show("Регионы:")
  show(dann[,id])
  region <- readline(prompt = paste("Введите название региона ")) 
  index_region <- 0
  
  for (i in 1:nrow(dann)){
    gre <- grepl(region,as.character(dann[i,1]))
    if (gre){
      index_region <- i
      break;
    }
  }
  
  if (index_region==0){
    show("Регион не найден")
    return()
  }else{
    dann_region <- dann[index_region,]
    year <- colnames(dann_region)[-(1:id)]
    year <- year[-length(year)]
    Value <- c()
    for (i in (id+1):(ncol(dann_region)-1)){
      Value <- c(Value,as.vector(dann_region[1,i]))
    }
    new_dann <- data.frame(year,Value)
    
    l.1 <- data.frame(Region = region, OKATO = as.vector(dann_region[1,ncol(dann_region)]))
    l.2 <- ind_name
    l.3 <- data.frame(year,Value)
    
    return(list(l.1,l.2,l.3))
  }
}

Vrem_ryad <- function(mode){
  path <- '/bgd/regl/'
  params <- '/?List&Id='
  id <- -1
  db_name <- 'B16_14p'
  # go through tree until we got a link to doc instead of id in stat db
  while(TRUE){
    url <- paste(host_name, path, db_name, params, id, sep = '')
    download.file(url, destfile = 'test_xml.xml')
    test <- readLines('test_xml.xml')
    xml <- xmlTreeParse(url, useInternalNodes = T)
    names <- xpathSApply(xml, "//name", xmlValue)
    Encoding(names) <- "UTF-8" 
    refs <- xpathSApply(xml, "//ref", xmlValue)
    for(i in 1:length(names))
      print(paste(i, names[i]))
    num <- readline(prompt = "Введите номер ")
    num <- as.numeric(num)
    ind_name <- names[num]
    ref <- refs[num]
    if(substr(ref, 1, 1) != "?")
      break;
    id <- substr(ref, 2, nchar(ref))
  }
  loadGKSData(ref)
  
  if (mode==1){
    new_dann <- Add_OKATO(dataGKS,1)
    return(new_dann)
  }else
  if (mode==2){
    new_dann <- Add_OKATO(dataGKS,1)
    return(info_region(new_dann, ind_name))
    
  }
  
}
Vrem_ryad(1)
dann <- Add_OKATO(dann,1)

# примеры ----------------------------------------------------------------------

Sorted_region(dataGKS,1)


getGKSDataRef()
2008

loadGKSData(getGKSDataRef())
2016
3
2



sdf <- Add_OKATO(dataGKS,2)
sdf[-(1:71),]
if (is.na(sdf$OKATO))sdf[-is.na(sdf$OKATO),]
word_doc <- '/bgd/regl/b16_14p/IssWWW.exe/Stg/d01/02-01.doc'
grepl(c("федеральный","Коми-Пермяцкий"),as.character(dataGKS[60,2]))
?grepl
