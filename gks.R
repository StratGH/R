library(XML)
library(xml2)
library(tools)
# init
host_name <- 'http://www.gks.ru'

# all stat years and path to it db
years_v <-    c(2003, 2004, 2005, 2006,
              2007, 2008, 2009, 2010,
              2011, 2012, 2013, 2014,
              2015)
db_names <- c('B03_14', 'B04_14', 'B05_14p', 'B06_14p',
              'B07_14p', 'B08_14p', 'B09_14p', 'B10_14p',
              'B11_14p', 'B12_14p', 'B13_14p', 'B14_14p',
              'B15_14p')



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
  db_name <- years$db_names[years_v == year]
  # go through tree until we got a link to doc instead of id in stat db
  while(TRUE){
    url <- paste(host_name, path, db_name, params, id, sep = '')
    xml <- xmlTreeParse(url, useInternalNodes=T)
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


Add_OKATO <- function(dann_frame, id){
  Sub_RF <- c('Адыгея', 'Алтай', 'Башкортостан', 'Бурятия', 'Дагестан', 'Ингушетия', 'Кабардино-Балкарская', 'Калмыкия',
              'Карачаево-Черкесская', 'Карелия', 'Коми', 'Крым', 'Марий Эл', 'Мордовия', 'Саха', 'Северная Осетия — Алания',
              'Татарстан', 'Тыва', 'Удмуртская', 'Хакасия', 'Чеченская', 'Чувашская', 'Алтайский', 'Забайкальский', 'Камчатский',
              'Краснодарский', 'Красноярский', 'Пермский', 'Приморский', 'Ставропольский', 'Хабаровский', 'Амурская', 'Архангельская',
              'Астраханская', 'Белгородская', 'Брянская', 'Владимирская', 'Волгоградская', 'Вологодская', 'Воронежская', 'Ивановская', 
              'Иркутская', 'Калининградская', 'Калужская', 'Кемеровская', 'Кировская', 'Костромская', 'Курганская', 'Курская', 'Ленинградская',
              'Липецкая', 'Магаданская', 'Московская', 'Мурманская', 'Нижегородская', 'Новгородская', 'Новосибирская', 'Омская', 'Оренбургская', 
              'Орловская', 'Пензенская', 'Псковская', 'Ростовская', 'Рязанская', 'Самарская', 'Саратовская', 'Сахалинская', 'Свердловская',
              'Смоленская', 'Тамбовская', 'Тверская', 'Томская', 'Тульская', 'Тюменская', 'Ульяновская', 'Челябинская', 'Ярославская', 'Москва',
              'Санкт-Петербург', 'Севастополь', 'Еврейская', 'Ненецкий', 'Ханты-Мансийский', 'Чукотский', 'Ямало-Ненецкий')
  
  
  Sub_okato <- c('79', '84', '80', '81', '82', '26', '83', '85', '91', '86', '87', '35', '88', '89', '98', '90', '92',
                 '93', '94', '95', '96', '97', '1', '76', '30', '3', '4', '57', '5', '7', '8', '10', '11', '12', '14',
                 '15', '17', '18', '19', '20', '24', '25', '27', '29', '32', '33', '34', '37', '38', '41', '42', '44',
                 '46', '47', '22', '49', '50', '52', '53', '54', '56', '58', '60', '61', '36', '63', '64', '65', '66',
                 '68', '28', '69', '70', '71', '73', '75', '78', '45', '40', '67', '99', '111', '7110', '77', '7114')
  
  OKATO <- c()
  
  for (i in 1:nrow(dann_frame[id])){
    
    ma <- FALSE
    fed_okr <- grepl("федеральный",dann_frame[i,id])||grepl("Долгано-Ненецкий",dataGKS[i,id])
    if (!fed_okr)
      for (j in 1:length(Sub_RF)){
        if (Sub_RF[j]=='Ненецкий'){
          yane <- grepl('Ямало-Ненецкий',dann_frame[i,id])
          if (yane){
            ma <- FALSE
          }
        }else ma <- grepl(Sub_RF[j],dann_frame[i,id])
        
        if (ma){
          OKATO <- c(OKATO, Sub_okato[j])
          break;
        }
      }
    if (!ma){
      OKATO <- c(OKATO, NA)
    }
  }
  
  new_dann_frame <- data.frame(dann_frame,OKATO)
  return(new_dann_frame)
}



getGKSDataRef()
2008
loadGKSData(getGKSDataRef())
2015
17
2
2
1


sdf <- Add_OKATO(dataGKS,2)








word_dir <- shell('reg query "HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\WINWORD.EXE" /v PATH', intern = TRUE)
winword_dir <- c(strsplit(word_dir[grep("PATH",word_dir)],"  ")[[1]])
winword_dir <- winword_dir[length(winword_dir)]
conv_to_docx <- paste('"',winword_dir, 'wordconv.exe" -oice -nme C:\\1\\02-04.doc C:\\1\\02-04.docx', sep="")
shell(conv_to_docx)


in_c <- function(fr){
  d <- "c('"
  for (i in 1:(nrow(fr[1])-1)){
    n_fr <- fr[i,1]
    n_fr <- gsub("Республика ","",n_fr)
    n_fr <- gsub(" край","",n_fr)
    n_fr <- gsub(" область","",n_fr)
    n_fr <- gsub(" округ","",n_fr)
    n_fr <- gsub(" республика","",n_fr)
    n_fr <- gsub(" автономный","",n_fr)
    n_fr <- gsub(" автономная","",n_fr)
    d <- paste(d, n_fr,sep="")
    d <- paste(d, "', '",sep="")
  }
  n_fr <- fr[nrow(fr[1]),1]
  n_fr <- gsub("Республика ","",n_fr)
  n_fr <- gsub(" край","",n_fr)
  n_fr <- gsub(" область","",n_fr)
  n_fr <- gsub(" округ","",n_fr)
  n_fr <- gsub(" республика","",n_fr)
  n_fr <- gsub(" автономный","",n_fr)
  n_fr <- gsub(" автономная","",n_fr)
  d <- paste(d, n_fr,sep="")
  d <- paste(d, "')",sep="")
  return(d)
} 


install.packages("xlsx", dep = T)
library("xlsx")
#Sys.setlocale("LC_ALL","Russian_Russia")
#Sys.setlocale(,"ru_RU")
dann <- read.xlsx("C:\\Users\\Zhura\\Desktop\\Практика\\r_gks_stat_data-master\\1.xlsx",sheetIndex = 1, encoding = "UTF-8")

Sub_RF <- c('Республика Адыгея', 'Республика Алтай', 'Республика Башкортостан', 'Республика Бурятия',
            'Республика Дагестан', 'Республика Ингушетия', 'Кабардино-Балкарская республика', 'Республика Калмыкия',
            'Карачаево-Черкесская республика', 'Республика Карелия', 'Республика Коми', 'Республика Крым',
            'Республика Марий Эл', 'Республика Мордовия', 'Республика Саха (Якутия)', 
            'Республика Северная Осетия — Алания', 'Республика Татарстан', 'Республика Тыва', 
            'Удмуртская республика', 'Республика Хакасия', 'Чеченская республика', 'Чувашская республика',
            'Алтайский край', 'Забайкальский край', 'Камчатский край', 'Краснодарский край',
            'Красноярский край', 'Пермский край', 'Приморский край', 'Ставропольский край', 
            'Хабаровский край', 'Амурская область', 'Архангельская область', 'Астраханская область',
            'Белгородская область', 'Брянская область', 'Владимирская область', 'Волгоградская область',
            'Вологодская область', 'Воронежская область', 'Ивановская область', 'Иркутская область',
            'Калининградская область', 'Калужская область', 'Кемеровская область', 'Кировская область',
            'Костромская область', 'Курганская область', 'Курская область', 'Ленинградская область', 
            'Липецкая область', 'Магаданская область', 'Московская область', 'Мурманская область', 
            'Нижегородская область', 'Новгородская область', 'Новосибирская область', 'Омская область',
            'Оренбургская область', 'Орловская область', 'Пензенская область', 'Псковская область',
            'Ростовская область', 'Рязанская область', 'Самарская область', 'Саратовская область', 
            'Сахалинская область', 'Свердловская область', 'Смоленская область', 'Тамбовская область', 
            'Тверская область', 'Томская область', 'Тульская область', 'Тюменская область', 'Ульяновская область',
            'Челябинская область', 'Ярославская область', 'Москва', 'Санкт-Петербург', 'Севастополь',
            'Еврейская автономная область', 'Ненецкий автономный округ', 'Ханты-Мансийский автономный округ - Югра',
            'Чукотский автономный округ', 'Ямало-Ненецкий автономный округ')


Sub_RF <- c('Адыгея', 'Алтай', 'Башкортостан', 'Бурятия', 'Дагестан', 'Ингушетия', 'Кабардино-Балкарская', 'Калмыкия',
            'Карачаево-Черкесская', 'Карелия', 'Коми', 'Крым', 'Марий Эл', 'Мордовия', 'Саха', 'Северная Осетия — Алания',
            'Татарстан', 'Тыва', 'Удмуртская', 'Хакасия', 'Чеченская', 'Чувашская', 'Алтайский', 'Забайкальский', 'Камчатский',
            'Краснодарский', 'Красноярский', 'Пермский', 'Приморский', 'Ставропольский', 'Хабаровский', 'Амурская', 'Архангельская',
            'Астраханская', 'Белгородская', 'Брянская', 'Владимирская', 'Волгоградская', 'Вологодская', 'Воронежская', 'Ивановская', 
            'Иркутская', 'Калининградская', 'Калужская', 'Кемеровская', 'Кировская', 'Костромская', 'Курганская', 'Курская', 'Ленинградская',
            'Липецкая', 'Магаданская', 'Московская', 'Мурманская', 'Нижегородская', 'Новгородская', 'Новосибирская', 'Омская', 'Оренбургская', 
            'Орловская', 'Пензенская', 'Псковская', 'Ростовская', 'Рязанская', 'Самарская', 'Саратовская', 'Сахалинская', 'Свердловская',
            'Смоленская', 'Тамбовская', 'Тверская', 'Томская', 'Тульская', 'Тюменская', 'Ульяновская', 'Челябинская', 'Ярославская', 'Москва',
            'Санкт-Петербург', 'Севастополь', 'Еврейская', 'Ненецкий', 'Ханты-Мансийский', 'Чукотский', 'Ямало-Ненецкий')


Sub_okato <- c('79', '84', '80', '81', '82', '26', '83', '85', '91', '86', '87', '35', '88', '89', '98', '90', '92',
               '93', '94', '95', '96', '97', '1', '76', '30', '3', '4', '57', '5', '7', '8', '10', '11', '12', '14',
               '15', '17', '18', '19', '20', '24', '25', '27', '29', '32', '33', '34', '37', '38', '41', '42', '44',
               '46', '47', '22', '49', '50', '52', '53', '54', '56', '58', '60', '61', '36', '63', '64', '65', '66',
               '68', '28', '69', '70', '71', '73', '75', '78', '45', '40', '67', '99', '111', '7110', '77', '7114')


OKATO <- data.frame(Sub_RF,Sub_okato)

in_c(dann[-(1:42),])
