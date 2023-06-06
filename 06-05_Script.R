############################################################################
# EU & public consultations
############################################################################
# Note: For the results of this study, only line 1-712 are relevant.

# create a new folder with all files
wd_all <-'/Users/selinak./Desktop/Thesis/all txt files'
setwd(wd_all)

# install and load packages
packages <- c("quanteda", "readtext", "qdap", "stringr", "tm", "ggplot2", "ggthemes", "RColorBrewer", "dendextend", "circlize",
              "tidyverse", "DT", "pdftools")
# install packages that are not installed yet
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# packages loading
invisible(lapply(packages, library, character.only = TRUE))

# read files
files_all <- list.files(pattern = "txt$") # all .txt files
txtcorpus_all <- c(files_all) # all .txt files
dir_all <- list.files(wd_all) #all .txt files
for(i in 1:length(dir_all)){
  x <-read.table(dir_all[i], sep='\n', quote="", stringsAsFactors = FALSE, fileEncoding = "latin1", skipNul=T)
  j <- as.character(x[,1])
  j <- paste(j,collapse='')
  txtcorpus_all[i]<-j
}

author_all <- c(files_all)
names(txtcorpus_all) <- author_all 
groups_corp_all <- corpus(txtcorpus_all, docvars = data.frame(authors = names(txtcorpus_all)), unique_docnames=F) # load the data as a corpus
summary(groups_corp_all) # --> output: corpus consists of and shows 77 documents
tagged_corp_all <- corpus(c(files_all))
sect_corp_all <- corpus_segment(tagged_corp_all, "*")

# create a dataframe
data_dir_all <- system.file(groups_corp_all, package = "readtext")
data_frame_all <- readtext(paste0(data_dir_all, 
                              '/Users/selinak./Desktop/Thesis/all txt files'),
                       text_field = "texts")
names(data_frame_all)

files_all <- list.files(path=wd_all,
                        pattern=".*txt", recursive = T)

files_all_txt <- lapply(files_all, function(x) {
  tmp <- readLines(x, skipNul=T)
  tmp <- str_c(tmp, collapse = "")
  tmp <- iconv(tmp, "ASCII", "UTF-8", sub="")
  return(tmp)
})

tm::tm_map
files_all_corpus <- VCorpus(VectorSource(files_all_txt)) 
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))}) 
files_all_corpus <- tm_map(files_all_corpus, toSpace, "/") # replacing "/" with space
files_all_corpus <- tm_map(files_all_corpus, toSpace, "@") # 
files_all_corpus <- tm_map(files_all_corpus, toSpace, "\\|") #
files_all_corpus <- tm_map(files_all_corpus, toSpace, "[^[:graph:]]") 
files_all_corpus <- tm_map(files_all_corpus, content_transformer(tolower)) 
files_all_corpus <- tm_map(files_all_corpus, stripWhitespace) 
files_all_corpus <- tm_map(files_all_corpus, removePunctuation)
files_all_corpus <- tm_map(files_all_corpus, removeNumbers) # removing numbers
files_all_corpus <- tm_map(files_all_corpus,removeWords,tm::stopwords(kind="smart"))
files_all_corpus <- tm_map(files_all_corpus, function(x)removeWords(x,tm::stopwords(kind="smart")))
files_all_corpus <- tm_map(files_all_corpus, stemDocument) # text stemming (reducing words to their root form)
tm_map(files_all_corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte')) 
# --> output: <<VCorpus>> Metadata:  corpus specific: 0, document level (indexed): 0 Content:  documents: 77

tdm_files_all <- TermDocumentMatrix(files_all_corpus) # term document matrix
dtm_m_all <- as.matrix(tdm_files_all) 
dtm_v_all <- sort(rowSums(dtm_m_all),decreasing=TRUE) # in order of decreasing value of frequency
dtm_d_all <- data.frame(word = names(dtm_v_all),freq=dtm_v_all) 
dtm_d2_all<-dtm_d_all[order(dtm_d_all[,2], decreasing=T),] 
head(dtm_d_all, 50) # top 50 most frequent words

### VISUALISATION: ranking of most frequent words

# colours by theory

# add new variable: NPM & DEG 
dtm_d_all_theory <- dtm_d_all
dtm_d_all_theory <- tibble::rowid_to_column(dtm_d_all_theory, "ID")
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 3] <- "NPM" # Replace word by NPM
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 4] <- "NPM" # Replace word by NPM
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 5] <- "NPM" # Replace word by NPM
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 6] <- "NPM" # Replace word by NPM
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 9] <- "NPM" # Replace word by NPM
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 10] <- "NPM" # Replace word by NPM
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 19] <- "NPM" # Replace word by NPM
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 23] <- "NPM" # Replace word by NPM

# or this comes later after replacing
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 21] <- "Uncategorised" # Replace word if neither theory
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 38] <- "Uncategorised" # Replace word if neither theory
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 42] <- "Uncategorised" # Replace word if neither theory
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 88] <- "Uncategorised" # Replace word if neither theory
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 119] <- "Uncategorised" # Replace word if neither theory
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 97] <- "Uncategorised" # Replace word if neither theory
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 22] <- "Uncategorised" # Replace word if neither theory
dtm_d_all_theory$ID[dtm_d_all_theory$ID == 16] <- "Uncategorised" # Replace word if neither theory

dtm_d_all_theory$ID <- ifelse(!dtm_d_all_theory$ID %in% c("NPM", "Uncategorised"), "DEG", dtm_d_all_theory$ID) ##

# dtm_d_all_theory$ID[!dtm_d_all_theory$ID == "NPM"] <- "DEG" # Replace remaining words by DEG
colnames(dtm_d_all_theory)[colnames(dtm_d_all_theory) == "ID"] <- "Theory"

brewer.pal(8, "Blues") # colour palette: blue tones

ggplot(dtm_d_all_theory[1:50,], aes(x=reorder(word, freq), y=freq, fill=Theory))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#9ECAE1", "#084594", "black"))+
  coord_flip()+
  theme_gdocs()+
  geom_text(aes(label=freq),colour="white",hjust=1.25, size=5.0, family="Arial")+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        text=element_text(size=12, family="Arial"))+
  xlab("Word stems")+
  ylab("Absolute frequencies")+
  ggtitle("The 50 most frequent word stems")

### VISUALISATION: cluster analysis

tdm_files_all2 <- TermDocumentMatrix(files_all_corpus,control=list(weighting=weightTf))
tdm_files_all2.m<-as.matrix(tdm_files_all2)  
term.freq_all<-rowSums(tdm_files_all2.m)

tdm_files_all3 <- removeSparseTerms(tdm_files_all2, sparse=0.5) 
hc <- hclust(dist(tdm_files_all3, method="euclidean"), method="complete")
plot(hc,yaxt='n', main='Cluster Analysis')

# grey shades 
pal_grey <- brewer.pal(8, "Greys")
pal_grey <- pal_grey[-(1:4)]
labelColors <- c(pal_grey)
text(x = 10, y = 10, family = "Arial") ## font

dend.change <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(
      names(clusMember) == a$label)]]
    attr(n, 'nodePar') <- c(a$nodePar, lab.col =
                              labCol)
  }
  n
}

hcd <- as.dendrogram(hc)
clusMember <- cutree(hc,4)
clusDendro <- dendrapply(hcd, dend.change)
op = par(family="Arial") # saving defaults in `op`

plot(clusDendro, main = "Dendrogram", type = "triangle",yaxt='n')

# install and load packages
packages_dendrogram <- c("ggraph", "igraph", "colormap", "kableExtra")
# install packages that are not installed yet
installed_packages <- packages_dendrogram %in% rownames(installed.packages())
if (any(installed_packages == F)) {
  install.packages(packages_dendrogram[!installed_packages])
}

# packages loading
invisible(lapply(packages_dendrogram, library, character.only = T))

# libraries
options(knitr.table.format = "html")

#install.packages("ggdendro")
#library(ggdendro)
 
theories <- c("DEG", "DEG", NA, "DEG", "DEG", "DEG", "DEG", "DEG", "DEG", "DEG", 
              "DEG", NA, "DEG", "DEG", NA, "DEG", "DEG", "DEG", "DEG", NA, 
              NA, NA, NA, "DEG", "DEG", "NPM", "NPM", "NPM", "NPM", "NPM", 
              "NPM", NA, "DEG", "DEG", "DEG", "NPM", "NPM", "DEG", "DEG")

#barcolor <- colormap(colormap = colormaps$viridis, nshades = 3, format = "hex", alpha = 1, reverse = F)
#barcolor <- barcolor[as.numeric(as.factor(theories))]
barcolor <- c("#9ECAE1", "#084594") ##
barcolor <- barcolor[as.numeric(as.factor(theories))] ##

#colored_bars(colors = barcolor, dend=clusDendro, rowLabels = "Theory", horiz=F)
colored_bars(colors = barcolor, rowLabels = "Theory", y_shift=-75, horiz=F)
#colored_bars(colors = barcolor, hcd, rowLabels = "Theory")

# Add the legend  
legend("topright", legend = c('DEG', 'NPM', 'Uncategorised'), pch = 15, pt.cex = 3, cex = 1, 
       inset = c(0, 0),
       title = "Theory", 
       col = c("#9ECAE1", "#084594", "white"),
       bty="o",
       bg = "#F0F0F0",
       box.lwd=0,
       horiz=F)

par(op) # reset plotting parameters

############################################################################################################################################################ 
### count number of text pages in pdf files 

library(pdftools)

# consultations 2019

pdfInfo2019_1 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from Yubico AB (Company business) F484842.pdf')
file2019_1 <- pdfInfo2019_1$pages
file2019_1

pdfInfo2019_2 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from Workday (Company business) F487185.pdf')
file2019_2 <- pdfInfo2019_2$pages
file2019_2

pdfInfo2019_3 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from MyData.org (NGO) F473525 (less than 100 words).pdf')
file2019_3 <- pdfInfo2019_3$pages
file2019_3

pdfInfo2019_4 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from Maria-Amor DOMINGUEZ (EU citizen) F486881 (less than 100 words).pdf')
file2019_4 <- pdfInfo2019_4$pages
file2019_4

pdfInfo2019_5 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from InfoCert SpA F487183 (Company business) .pdf')
file2019_5 <- pdfInfo2019_5$pages
file2019_5

pdfInfo2019_6 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from Global Legal Entity Identifier Foundation (GLEIF) (Other) F486642.pdf')
file2019_6 <- pdfInfo2019_6$pages
file2019_6

pdfInfo2019_7 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from Eurosmart (Business association) F487173.pdf')
file2019_7 <- pdfInfo2019_7$pages
file2019_7

pdfInfo2019_8 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from Danish Construction Association (Dansk Byggeri) (Business association) F487076.pdf')
file2019_8 <- pdfInfo2019_8$pages
file2019_8

pdfInfo2019_9 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from Asociación Nacional de Establecimientos Financieros de Crédito (Business association) F484271.pdf')
file2019_9 <- pdfInfo2019_9$pages
file2019_9

pdfInfo2019_10 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from Anonymous F487140 (less than 100 words).pdf')
file2019_10 <- pdfInfo2019_10$pages
file2019_10

pdfInfo2019_11 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/Feedback from AC Camerfirma SA (Company business) F487310.pdf')
file2019_11 <- pdfInfo2019_11$pages
file2019_11

pdfInfo2019_12 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-Spanish Feedback from_Anonymous F487196 (other).pdf')
file2019_12 <- pdfInfo2019_12$pages
file2019_12

pdfInfo2019_13 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-Spanish Feedback from_ Ministerio de Trabajo, Migraciones y Seguridad Social (Public authority) F486915 (public).pdf')
file2019_13 <- pdfInfo2019_13$pages
file2019_13

pdfInfo2019_14 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-Spanish Feedback from_ Anonymous F487184 (other).pdf')
file2019_14 <- pdfInfo2019_14$pages
file2019_14

pdfInfo2019_15 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-Spanish Feedback from_ Anonymous F486939 (other).pdf')
file2019_15 <- pdfInfo2019_15$pages
file2019_15

pdfInfo2019_16 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-Slovak Feedback from_ OZ Bez bariéry - Národná platforma proti bariéram (NGO) F483637 (other).pdf')
file2019_16 <- pdfInfo2019_16$pages
file2019_16

pdfInfo2019_17 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-Italian Feedback from_ Anonymous F473531 (less than 100 words) (other).pdf')
file2019_17 <- pdfInfo2019_17$pages
file2019_17

pdfInfo2019_18 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-German Feedback from_ BvDP (Business association) F485549 (private).pdf')
file2019_18 <- pdfInfo2019_18$pages
file2019_18

pdfInfo2019_19 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-German Feedback from_ Bundesnotarkammer F487262 (other).pdf')
file2019_19 <- pdfInfo2019_19$pages
file2019_19

pdfInfo2019_20 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-German Feedback from_ Anonymous F487203 (other).pdf')
file2019_20 <- pdfInfo2019_20$pages
file2019_20

pdfInfo2019_21 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-German Feedback from United Internet I 1&1 (Company business) F487055 (private).pdf')
file2019_21 <- pdfInfo2019_21$pages
file2019_21

pdfInfo2019_22 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-German Feedback from Bundesdruckerei GmbH (Company business) F487162 (private).pdf')
file2019_22 <- pdfInfo2019_22$pages
file2019_22

pdfInfo2019_23 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2019 english/2019 English-Bulgarian Feedback from_ Даниел Кузманов (EU citizen) F473520 (less than 100 words) (other).pdf')
file2019_23 <- pdfInfo2019_23$pages
file2019_23

# total page number for the consultations in 2019: 124 pages of 23 pdf files
sum(file2019_1+file2019_2+file2019_3+file2019_4+file2019_5+file2019_6+file2019_7+file2019_8+file2019_9+
  file2019_10+file2019_11+file2019_12+file2019_13+file2019_14+file2019_15+file2019_16+file2019_17+file2019_18+file2019_19+
  file2019_20+file2019_21+file2019_22+file2019_23)

# consultations 2020

pdfInfo2020_1 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/2020 English-French Feedback from Anonymous F549055 (other).pdf')
file2020_1 <- pdfInfo2020_1$pages
file2020_1

pdfInfo2020_2 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/2020 English-French Feedback from CNIL (French Data Protection Authority) (Public authority) F549054 (public).pdf')
file2020_2 <- pdfInfo2020_2$pages
file2020_2

pdfInfo2020_3 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/2020 English-French Feedback from Conseil national du numérique (Public authority) F547048 (public).pdf')
file2020_3 <- pdfInfo2020_3$pages
file2020_3

pdfInfo2020_4 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/2020 English-French Feedback from Conseil supérieur du notariat F548663 (other).pdf')
file2020_4 <- pdfInfo2020_4$pages
file2020_4

pdfInfo2020_5 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/2020 English-French Feedback from Jean-Jacques Vaultier (EU citizen) F543935 (other).pdf')
file2020_5 <- pdfInfo2020_5$pages
file2020_5

pdfInfo2020_6 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/2020 English-German Feedback from BvDP (Company business) F547352 (private).pdf')
file2020_6 <- pdfInfo2020_6$pages
file2020_6

pdfInfo2020_7 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/2020 English-German Feedback from eco - Verband der Internetwirtschaft e.V. (Business association) F547025 (private).pdf')
file2020_7 <- pdfInfo2020_7$pages
file2020_7

pdfInfo2020_8 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/2020 English-German Feedback from GISAD i.G. F548915 (other).pdf')
file2020_8 <- pdfInfo2020_8$pages
file2020_8

pdfInfo2020_9 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/2020 English-Spanish Feedback from Anonymous F541541 (other).pdf')
file2020_9 <- pdfInfo2020_9$pages
file2020_9

pdfInfo2020_10 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/2020 Feedback from Legal Studio (Company business) F548996 (form filled out)_(private).pdf')
file2020_10 <- pdfInfo2020_10$pages
file2020_10

pdfInfo2020_11 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from 1&1 (Company business) F548611.pdf')
file2020_11 <- pdfInfo2020_11$pages
file2020_11

pdfInfo2020_12 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from ACN - Alliance pour la Confiance Numérique (Alliance for Digital Trust) (Business association) F548675.pdf')
file2020_12 <- pdfInfo2020_12$pages
file2020_12

pdfInfo2020_13 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from AIG CEV (NGO) F547568.pdf')
file2020_13 <- pdfInfo2020_13$pages
file2020_13

pdfInfo2020_14 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Alberto ZANINI (EU citizen) F543707.pdf')
file2020_14 <- pdfInfo2020_14$pages
file2020_14

pdfInfo2020_15 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Anonymous F539174.pdf')
file2020_15 <- pdfInfo2020_15$pages
file2020_15

pdfInfo2020_16 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Anonymous F543486.pdf')
file2020_16 <- pdfInfo2020_16$pages
file2020_16

pdfInfo2020_17 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Anonymous F548902.pdf')
file2020_17 <- pdfInfo2020_17$pages
file2020_17

pdfInfo2020_18 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Anonymous F548950.pdf')
file2020_18 <- pdfInfo2020_18$pages
file2020_18

pdfInfo2020_19 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from ARIADNEXT (Company business) F549060.pdf')
file2020_19 <- pdfInfo2020_19$pages
file2020_19

pdfInfo2020_20 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Association for promotion of digital verification (Other) F545872.pdf')
file2020_20 <- pdfInfo2020_20$pages
file2020_20

pdfInfo2020_21 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Better Identity Coalition (Company business) F548763.pdf')
file2020_21 <- pdfInfo2020_21$pages
file2020_21

pdfInfo2020_22 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Bundesdruckerei GmbH (Company business) F546495.pdf')
file2020_22 <- pdfInfo2020_22$pages
file2020_22

pdfInfo2020_23 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Center for Data Innovation (NGO) F547234.pdf')
file2020_23 <- pdfInfo2020_23$pages
file2020_23

pdfInfo2020_24 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from City of Amsterdam (Public authority) F547522.pdf')
file2020_24 <- pdfInfo2020_24$pages
file2020_24

pdfInfo2020_25 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from CLR Labs (Company business) F544741.pdf')
file2020_25 <- pdfInfo2020_25$pages
file2020_25

pdfInfo2020_26 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Deutsche Telekom AG (Company business) F548665.pdf')
file2020_26 <- pdfInfo2020_26$pages
file2020_26

pdfInfo2020_27 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Developers Alliance (Business association) F547377.pdf')
file2020_27 <- pdfInfo2020_27$pages
file2020_27

pdfInfo2020_28 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Erste Group Bank AG (Company business) F548968.pdf')
file2020_28 <- pdfInfo2020_28$pages
file2020_28

pdfInfo2020_29 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from European Payment Institutions Federation (EPIF) (Company business) F549030.pdf')
file2020_29 <- pdfInfo2020_29$pages
file2020_29

pdfInfo2020_30 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from European Signature Dialog (Company business) F547552.pdf')
file2020_30 <- pdfInfo2020_30$pages
file2020_30

pdfInfo2020_31 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Eurosmart (Business association) F549006.pdf')
file2020_31 <- pdfInfo2020_31$pages
file2020_31

pdfInfo2020_32 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from FIDO Alliance (Company business) F548762.pdf')
file2020_32 <- pdfInfo2020_32$pages
file2020_32

pdfInfo2020_33 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Finance Denmark (Business association) F547018.pdf')
file2020_33 <- pdfInfo2020_33$pages
file2020_33

pdfInfo2020_34 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Global Legal Entity Identifier Foundation (GLEIF) (NGO) F547545.pdf')
file2020_34 <- pdfInfo2020_34$pages
file2020_34

pdfInfo2020_35 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from idnow (Other) F540380.pdf')
file2020_35 <- pdfInfo2020_35$pages
file2020_35

pdfInfo2020_36 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Insurance Europe (Business association) F548927.pdf')
file2020_36 <- pdfInfo2020_36$pages
file2020_36

pdfInfo2020_37 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from International Trade and Forfaiting Association (ITFA) (Business association) F548913.pdf')
file2020_37 <- pdfInfo2020_37$pages
file2020_37

pdfInfo2020_38 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from KNB (Business association) F548866.pdf')
file2020_38 <- pdfInfo2020_38$pages
file2020_38

pdfInfo2020_39 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Marco SCOGNAMIGLIO (EU citizen) F548781.pdf')
file2020_39 <- pdfInfo2020_39$pages
file2020_39

pdfInfo2020_40 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Ministry of Social Affairs and Health (Public authority) F547510.pdf')
file2020_40 <- pdfInfo2020_40$pages
file2020_40

pdfInfo2020_41 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from OneSpan, Inc_ (Company business) F546747.pdf')
file2020_41 <- pdfInfo2020_41$pages
file2020_41

pdfInfo2020_42 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Onfido Ltd. (Company business) F548633.pdf')
file2020_42 <- pdfInfo2020_42$pages
file2020_42

pdfInfo2020_43 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from OpenID Foundation (eKYC & Identity Assurance WG) (NGO) F548976.pdf')
file2020_43 <- pdfInfo2020_43$pages
file2020_43

pdfInfo2020_44 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from ORANGE (Company business) F547499.pdf')
file2020_44 <- pdfInfo2020_44$pages
file2020_44

pdfInfo2020_45 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Poste Italiane (Other) F548999.pdf')
file2020_45 <- pdfInfo2020_45$pages
file2020_45

pdfInfo2020_46 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from SGM CONSULTING - EVROTRUST (Company business) F548844.pdf')
file2020_46 <- pdfInfo2020_46$pages
file2020_46

pdfInfo2020_47 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Swiss Finance Council (Business association) F549007.pdf')
file2020_47 <- pdfInfo2020_47$pages
file2020_47

pdfInfo2020_48 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Tallinn University of Technology (Academic research Institution) F548621.pdf')
file2020_48 <- pdfInfo2020_48$pages
file2020_48

pdfInfo2020_49 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Thales DIS (Company business) F548993.pdf')
file2020_49 <- pdfInfo2020_49$pages
file2020_49

pdfInfo2020_50 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Tomáš Šedivec (EU citizen) F539560.pdf')
file2020_50 <- pdfInfo2020_50$pages
file2020_50

pdfInfo2020_51 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Universidad de Murcia (Academic research Institution) F549050.pdf')
file2020_51 <- pdfInfo2020_51$pages
file2020_51

pdfInfo2020_52 <- pdf_info('/Users/selinak./Desktop/Thesis/public consultations/all (atlas.ti)/2020 english/Feedback from Yubico AB (Company business) F543642.pdf')
file2020_52 <- pdfInfo2020_52$pages
file2020_52

# total page number for the consultations in 2020: 183 pages of 52 pdf files
sum(file2020_1+file2020_2+file2020_3+file2020_4+file2020_5+file2020_6+file2020_7+file2020_8+file2020_9+
      file2020_10+file2020_11+file2020_12+file2020_13+file2020_14+file2020_15+file2020_16+file2020_17+file2020_18+file2020_19+
      file2020_20+file2020_21+file2020_22+file2020_23+file2020_24+file2020_25+file2020_26+file2020_27+file2020_28+file2020_29+
      file2020_30+file2020_31+file2020_32+file2020_33+file2020_34+file2020_35+file2020_36+file2020_37+file2020_38+file2020_39+
      file2020_40+file2020_41+file2020_42+file2020_43+file2020_44+file2020_45+file2020_46+file2020_47+file2020_48+file2020_49+
      file2020_50+file2020_51+file2020_52)

############################################################################################################################################################ 

### Classical Dictionary Analysis  

files_all_corpus_df <- data.frame(text=unlist(sapply(files_all_corpus, `[`, "content")), 
                              stringsAsFactors=F)

# retrieve the document names from files_all for NPM dictionary (by merging datasets) first 

'install.packages("tidyverse")
library(tidyverse)'
files_all_txt2 <- as.data.frame(files_all) 
files_all_txt2 <- rename(files_all_txt2, "Document Name" = files_all) # document per actor
files_all_txt2 <- tibble::rowid_to_column(files_all_txt2, "ID")

# merge files_all_corpus_df with files_all_txt2 for ID
files_all_corpus_df_ID <- tibble::rowid_to_column(files_all_corpus_df, "ID")
files_all_corpus_df_ID2 <- merge(files_all_txt2, files_all_corpus_df_ID, by = "ID", all = TRUE)

files_all_corpus_df_c <- corpus(files_all_corpus_df_ID2)

'docvars(files_all_corpus_df_c, "doc_id") <- files_all_corpus_df_ID2$ID

# Manually keep ID information 
for (i in 1:length(files_all_corpus_df_c)) {
  attr(files_all_corpus_df_c[[i]], "ID") <- files_all_corpus_df_ID2$ID[i]
}
'

library(quanteda)
toks_all_tagged_corp <- tokens(files_all_corpus_df_c, remove_punct = TRUE) # tokenise corpus

# create a dictionary based on the words of the clusters: get relevant key words/phrases
cluster1 <- c("electron*", "servic*") # electronic services
cluster2 <- c("public*", "secur*") # public security
cluster3 <- c("commiss*", "implement*") # Commission implementation
cluster4 <- c("nation*", "standard*") # national standard
cluster5 <- c("issu*", "level*") # issuance level 
cluster6 <- c("develop*", "support*") # development support
cluster7 <- c("market*", "includ*") # market inclusion
cluster8 <- c("system*", "creat*") # system creation
cluster9 <- c("framework*", "solut") # solution framework?
cluster10 <- c("eid*", "eida*") # electronic identification systems
cluster11 <- c("digit*", "ident*") # digital identity
cluster12 <- c("provid*", "trust*") # providers of trust services
cluster13 <- c("member*", "state*", "identif") # member state identification?
cluster14 <- c("data*", "requir*") # data requirements
cluster15 <- c("european*", "regul*") # European regulation

###
##
# (1) dictionary with NPM-related terms 
##
###

dict_NPM <- dictionary(list(eid = cluster10,
                            digital_identity = cluster11,
                            providers_trust = cluster12,
                            data_requirements = cluster14))
print(dict_NPM)

# select tokens surrounding keywords related to cluster 1
# and only keep these tokens and their context of ±10 tokens
toks_tagged_corp_cluster1 <- tokens_keep(toks_all_tagged_corp, pattern = phrase(cluster1), window = 10) 

# apply the dictionary
dict_all_npm_toks <- tokens_lookup(toks_all_tagged_corp, dict_NPM) 
head(dict_all_npm_toks)
dict_all_npm_toks_dfm <- dfm(dict_all_npm_toks)
dict_all_npm_toks_dfm <- convert(dict_all_npm_toks_dfm, to="data.frame") 
print(dict_all_npm_toks_dfm) # data frame: overview of mentions

dict_all_npm_toks_dfm2 <- tibble::rowid_to_column(dict_all_npm_toks_dfm, "ID")
dict_all_npm_toks_dfm2 <- dict_all_npm_toks_dfm2[-c(2)]
dict_all_npm_toks_dfm3 <- merge(files_all_txt2, dict_all_npm_toks_dfm2, by = "ID", all = TRUE)

# data frame as an HTML widget
'install.packages("devtools")
library(devtools)
devtools::install_github("kbenoit/quanteda.dictionaries") 
library(quanteda.dictionaries)
install.packages("htmlwidgets", type = "binary")
library(htmlwidgets)'
install.packages("DT", type = "binary")
library(DT)
DT::datatable(dict_all_npm_toks_dfm3)

###
##
# (2) dictionary with DEG-related terms 
##
###

dict_DEG <- dictionary(list(electronic_services = cluster1,
                            public_security = cluster2,
                            commission_implementation = cluster3,
                            national_standard = cluster4,
                            issuance_level = cluster5,
                            development_support = cluster6,
                            market_inclusion = cluster7,
                            system_creation = cluster8,
                            solution_framework = cluster9,
                            member_state_identification = cluster13,
                            european_regulation = cluster15))
print(dict_DEG)

# select tokens surrounding keywords related to cluster 1
# and only keep these tokens and their context of ±10 tokens
toks_tagged_corp_cluster2 <- tokens_keep(toks_all_tagged_corp, pattern = phrase(cluster2), window = 10) 

# apply the dictionary
dict_all_deg_toks <- tokens_lookup(toks_all_tagged_corp, dict_DEG) 
head(dict_all_deg_toks)
dict_all_deg_toks_dfm <- dfm(dict_all_deg_toks)
dict_all_deg_toks_dfm <- convert(dict_all_deg_toks_dfm, to="data.frame") 
print(dict_all_deg_toks_dfm) # data frame: overview of mentions

dict_all_deg_toks_dfm2 <- tibble::rowid_to_column(dict_all_deg_toks_dfm, "ID")
dict_all_deg_toks_dfm2 <- dict_all_deg_toks_dfm2[-c(2)]
dict_all_deg_toks_dfm3 <- merge(files_all_txt2, dict_all_deg_toks_dfm2, by = "ID", all = TRUE)

# data frame as an HTML widget
'install.packages("devtools")
library(devtools)
devtools::install_github("kbenoit/quanteda.dictionaries") 
library(quanteda.dictionaries)
install.packages("htmlwidgets", type = "binary")
library(htmlwidgets)'
library(DT)
DT::datatable(dict_all_deg_toks_dfm3)

'
# subset the parts of the data frame containing the words of the dictionary (e.g. cluster 1)

library(stringr)
cluster1_npm_feedback <- files_all_corpus_df[str_detect(files_all_corpus_df$text, paste(cluster1, collapse="|")),]
# output: all documents with these words assigned a specific meaning/value 
# dict_all_toks_dfm3[2:3]
'

# TEXT EXTRACTION: who mentions which key words?

' 
# match document names first?
library(tidyverse)
files_all_corpus_df_ID <- tibble::rowid_to_column(files_all_corpus_df, "ID")
files_all_corpus_df_ID2 <- merge(files_all_txt2, files_all_corpus_df_ID, by = "ID", all = TRUE)

library(readtext)
files_all_corpus_df_ID3 <- corpus(files_all_corpus_df_ID2, docvars = data.frame(authors = names(files_all_corpus_df_ID2)), unique_docnames=F) # load the data as a corpus
'

library(quanteda)
toks_all_tagged_corp <- tokens(files_all_corpus_df_c, remove_punct = TRUE) # tokenise corpus

# extract statements based on cluster 1-15: phrase matching (keyword-in-context) --> 538 matches

cluster1_kw <- kwic(toks_all_tagged_corp, pattern = phrase("electron* servic*"), valuetype = "regex", window = 3) # 12 matches

cluster2_kw <- kwic(toks_all_tagged_corp, pattern = phrase("public* secur*"), valuetype = "regex", window = 3) # 2 matches

cluster3_kw <- kwic(toks_all_tagged_corp, pattern = phrase("commiss* implement*"), valuetype = "regex", window = 3) # 17 matches

cluster4_kw <- kwic(toks_all_tagged_corp, pattern = phrase("nation* standard*"), valuetype = "regex", window = 3) # 2 matches

cluster5_kw <- kwic(toks_all_tagged_corp, pattern = phrase("issu* level*"), valuetype = "regex", window = 3) # 1 match

cluster6_kw <- kwic(toks_all_tagged_corp, pattern = phrase("support* develop*"), valuetype = "regex", window = 3) # 1 match

cluster7_kw <- kwic(toks_all_tagged_corp, pattern = phrase("market* includ*"), valuetype = "regex", window = 3) # 4 matches

cluster8_kw <- kwic(toks_all_tagged_corp, pattern = phrase("system* creat*"), valuetype = "regex", window = 3) # 3 matches

cluster9_kw <- kwic(toks_all_tagged_corp, pattern = phrase("solut* framework*"), valuetype = "regex", window = 3) # 3 matches

cluster10_kw <- kwic(toks_all_tagged_corp, pattern = phrase("eida* eid*"), valuetype = "regex", window = 3) # 36 matches

cluster11_kw <- kwic(toks_all_tagged_corp, pattern = phrase("digit* ident*"), valuetype = "regex", window = 3) # 408 matches

cluster12_kw <- kwic(toks_all_tagged_corp, pattern = phrase("provid* trust*"), valuetype = "regex", window = 3) # 33 matches

cluster13_kw <- kwic(toks_all_tagged_corp, pattern = phrase("identif* member* state*"), valuetype = "regex", window = 3) # 3 matches

cluster14_kw <- kwic(toks_all_tagged_corp, pattern = phrase("data* requir*"), valuetype = "regex", window = 3) # 8 matches

cluster15_kw <- kwic(toks_all_tagged_corp, pattern = phrase("european* regul*"), valuetype = "regex", window = 2) # 7 matches

############################################################################
# public consultations (exclusive look at stakeholder feedback)
############################################################################

### set one working directory for both public consultations (.txt files) ###
wd <-'/Users/selinak./Desktop/Thesis/public consultations/all'
setwd(wd)

### VISUALISATION: term frequency (see Kwartler chapter 3)
install.packages("tm")
library(tm)

install.packages("quanteda")
library(quanteda)
install.packages('readtext')
library(readtext)

# read files
txtfiles <- list.files(pattern = "txt$") # all .txt files
txtcorpus<-c(txtfiles) # all .txt files
dir <- list.files(wd) #all .txt files
#Sys.setlocale("LC_ALL", "C")
for(i in 1:length(dir)){
  x <-read.table(dir[i], sep='\n', quote="", stringsAsFactors = FALSE, fileEncoding = "latin1", skipNul=T)
  j <- as.character(x[,1])
  j <- paste(j,collapse='')
  txtcorpus[i]<-j
}

author <- c(txtfiles)
names(txtcorpus) <- author 
groups_corp <- corpus(txtcorpus, docvars = data.frame(authors = names(txtcorpus)), unique_docnames=F) # load the data as a corpus
summary(groups_corp) # --> output: corpus consists of and shows 75 documents
tagged_corp <- corpus(c(txtfiles))
sect_corp <- corpus_segment(tagged_corp, "*")

# create a dataframe
library(readtext)
data_dir <- system.file(groups_corp, package = "readtext")
data_frame <- readtext(paste0(data_dir, 
                              '/Users/selinak./Desktop/Thesis/public consultations/all'),
                       text_field = "texts")
names(data_frame)
txtfiles <- list.files(pattern = ".txt", recursive = T) 

install.packages('qdap')
library(qdap)
library(stringr)
library(tm)

files_txt <- lapply(txtfiles, function(x) {
  tmp <- readLines(x, skipNul=T)
  tmp <- str_c(tmp, collapse = "")
  tmp <- iconv(tmp, "ASCII", "UTF-8", sub="")
  return(tmp)
})

# retrieve the document names of the stakeholders' feedback from txtfiles (--> merge datasets later) 
install.packages("tidyverse")
library(tidyverse)
txtfiles2 <- as.data.frame(txtfiles) 
txtfiles2 <- rename(txtfiles2, "Document Name" = txtfiles) # document per stakeholder
txtfiles2 <- tibble::rowid_to_column(txtfiles2, "ID")

'files_txt2 <- as.data.frame(files_txt)
library(tidyr)
files_txt2 <- gather(files_txt2, Initial, Statement, factor_key=TRUE)
files_txt2 <- tibble::rowid_to_column(files_txt2, "ID")
files_txt2 <- files_txt2[-c(2)]'

## merged <- merge(txtfiles2, files_txt2, by = "ID", all = TRUE)
## --> files_txt as list again for next function
## files_txt_merged <- as.list(merged)

tm::tm_map
files_corpus <- VCorpus(VectorSource(files_txt)) 
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))}) 
files_corpus <- tm_map(files_corpus, toSpace, "/") # replacing "/" with space
files_corpus <- tm_map(files_corpus, toSpace, "@") # 
files_corpus <- tm_map(files_corpus, toSpace, "\\|") #
files_corpus <- tm_map(files_corpus, toSpace, "[^[:graph:]]") 
files_corpus <- tm_map(files_corpus, content_transformer(tolower)) 
files_corpus <- tm_map(files_corpus, stripWhitespace) 
files_corpus <- tm_map(files_corpus, removePunctuation)
files_corpus <- tm_map(files_corpus, removeNumbers) # removing numbers
files_corpus <- tm_map(files_corpus,removeWords,tm::stopwords(kind="smart"))
files_corpus <- tm_map(files_corpus, function(x)removeWords(x,tm::stopwords(kind="smart")))
files_corpus <- tm_map(files_corpus, stemDocument) # text stemming (reducing words to their root form)
tm_map(files_corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte')) 
# --> output: <<VCorpus>> Metadata:  corpus specific: 0, document level (indexed): 0 Content:  documents: 75

tdm_files <- TermDocumentMatrix(files_corpus) # term document matrix 
dtm_m <- as.matrix(tdm_files) 
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE) # in order of decreasing value of frequency
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v) 
dtm_d2<-dtm_d[order(dtm_d[,2], decreasing=T),] 
head(dtm_d, 50) # top 50 most frequent words

### VISUALISATION: ranking of most frequent words

install.packages("ggplot2")
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)

ggplot(dtm_d[1:50,], aes(x=reorder(word, freq), y=freq))+
  geom_bar(stat="identity", fill='black')+
  coord_flip()+
  theme_gdocs()+
  geom_text(aes(label=freq),colour="white",hjust=1.25, size=5.0, family="Times New Roman")+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        text=element_text(size=12, family="Times New Roman"))+
  xlab("Words") +
  ylab("Word frequencies") +
  ggtitle("The 50 most frequent words")

install.packages("SnowballC") # for text stemming
install.packages("stringr") 
library(SnowballC)
library(stringr)
library(tm)

dtm <- DocumentTermMatrix(files_corpus)
dtm2 <- as.matrix(dtm) 
mostfreqterm <- findMostFreqTerms(dtm)
frequent <- colSums(dtm2)
frequent <- sort(frequent,decreasing = TRUE) # most frequent words here 
frequent_d <- data.frame(word = names(frequent),freq=frequent) 
head(frequent_d, 5) # extra 
words <-names(frequent)

# Comparing and Contrasting Corpora in Word Clouds: 2019 vs 2020 consultations

library(tm)
library(wordcloud)

length(list.files(wd)) # total number of files in that folder

files2019 <- list.files(path=wd,
                               pattern="2019+.*txt", recursive = T)
files2020 <- list.files(path=wd,
                               pattern="2020+.*txt", recursive = T)
#install.packages('readr')
#library(readr)
filelist2019 <- lapply(files2019, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
filelist2020 <- lapply(files2020, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
names(filelist2019) <- c("one","two","three")
names(filelist2020) <- c("one","two","three")
invisible(lapply(names(filelist2019), function(x) assign(x,filelist2019[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console
invisible(lapply(names(filelist2020), function(x) assign(x,filelist2020[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

###
install.packages("stopwords")
library(stopwords)
custom.stopwords <- c(stopwords(source="smart"))
library(tm)
clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

filelist2019.vec<-clean.vec(filelist2019)
filelist2020.vec<-clean.vec(filelist2020)
filelist2019.vec <- paste(filelist2019.vec, collapse=" ")
filelist2020.vec <- paste(filelist2020.vec, collapse=" ")
allfiles <- c(filelist2019.vec, filelist2020.vec)
filelistcorpus <- VCorpus(VectorSource(allfiles))
filelisttdm <- TermDocumentMatrix(filelistcorpus)
filelisttdm.m <- as.matrix(filelisttdm)
colnames(filelisttdm.m) <- c("2019 Public Consultations","2020 Public Consultations")

### VISUALISATION: polarised tag plot

library(RColorBrewer)
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:4)]

library(plotrix)
common.words <- subset(filelisttdm.m, filelisttdm.m[, 1] > 0 & filelisttdm.m[, 2] > 0)
tail(common.words)
difference <- abs(common.words[, 1] - common.words[, 2])
common.words <- cbind(common.words, difference)
common.words <- common.words[order(common.words[, 3], decreasing = TRUE), ]
top30.df <- data.frame(x = common.words[1:30, 1],y = common.words[1:30, 2], labels = rownames(common.words[1:30, ]))
top30.df <- top30.df[order(top30.df$x, -rank(top30.df$y), decreasing = TRUE), ]
top30.df_reverse <- top30.df[order(top30.df$y, -rank(top30.df$x), decreasing = TRUE), ] # reverse

pyramid.plot(top30.df$x, top30.df$y,
             labels = top30.df$labels,
             lxcol = brewer.pal(8, "Blues"), rxcol = brewer.pal(8, "Blues"), 
             gap = 50, top.labels = c("2019 Public Consultations", "Words", "2020 Public Consultations"),
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL)

pyramid.plot(top30.df_reverse$x, top30.df_reverse$y,
             labels = top30.df_reverse$labels,
             lxcol = brewer.pal(8, "Blues"), rxcol = brewer.pal(8, "Blues"), 
             gap = 50, top.labels = c("2019 Public Consultations", "Words", "2020 Public Consultations"),
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL)

### VISUALISATION: cluster analysis 
library(tm)
tdm_files2 <- TermDocumentMatrix(files_corpus,control=list(weighting=weightTf))
tdm_files2.m<-as.matrix(tdm_files2)  
term.freq2<-rowSums(tdm_files2.m)

tdm_files3 <- removeSparseTerms(tdm_files2, sparse=0.65) 
hc <- hclust(dist(tdm_files3, method="euclidean"), method="complete")
plot(hc,yaxt='n', main='Cluster Analysis')

dend.change <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(
      names(clusMember) == a$label)]]
    attr(n, 'nodePar') <- c(a$nodePar, lab.col =
                              labCol)
    }
  n
  }

hcd <- as.dendrogram(hc)
clusMember <- cutree(hc,4)
labelColors <- c(pal)
clusDendro <- dendrapply(hcd, dend.change)
plot(clusDendro, main = "Dendrogram",
     type = "triangle",yaxt='n')

install.packages("dendextend")
library(dendextend)
install.packages("circlize")
library(circlize)
hcd<-color_labels(hcd,37, col = c(pal))
hcd<-color_branches(hcd,37, col = c(pal))
circlize_dendrogram(hcd, labels_track_height = 0.5,
                    dend_track_height = 0.4) 

### Classical Dictionary Analysis 
files_corpus_df <- data.frame(text=unlist(sapply(files_corpus, `[`, "content")), 
                              stringsAsFactors=F)
files_corpus_df_c <- corpus(files_corpus_df)

library(quanteda)
toks_tagged_corp <- tokens(files_corpus_df_c, remove_punct = TRUE) # tokenise corpus

# create a dictionary based on the words of the clusters: get relevant key words/phrases
cluster1 <- c("regul*", "trust*") # regulatory trust?
cluster2 <- c("electron*", "servic*") # electronic services?
cluster3 <- c("requir*", "secur*", "eida*") # secure eIDAS requirements?
cluster4 <- c("eid*", "ident*") # identity verification through eID?
cluster5 <- c("signatur*", "qualifi*", "certif*") # qualified digital signature certification?
cluster6 <- c("authent*", "user*") # user authentication?
cluster7 <- c("option", "scheme") # scheme options?
cluster8 <- c("member*", "state*") # member state?
cluster9 <- c("privat*", "provid*") # private provider?
cluster10 <- c("document*", "person*", "process") # personal document processing
cluster11 <- c("data*", "public*") # public data?
cluster12 <- c("author*", "order*") # authoritative ordering?
cluster13 <- c("enabl*", "addit*", "make") # enabling additional capacities?
cluster14 <- c("european*", "compani*") # european companies?
cluster15 <- c("identif*", "system*") # identification system?
cluster16 <- c("implement*", "level*") # implementation level?
cluster17 <- c("standard*", "creat*") # standard creation?
cluster18 <- c("issu*", "protect*") # issuance protection?
cluster19 <- c("increas*", "citizen*") # increasing citizen engagement/participation?
cluster20 <- c("busi*", "import*") # business importance?
cluster21 <- c("technic*", "current*", "inform*") # current technical information?
cluster22 <- c("market*", "assess*") # market assessment?
cluster23 <- c("high*", "support*") # high support?
cluster24 <- c("countri*", "lack*") # country-specific lack?
cluster25 <- c("impact*", "recognit*") # impactful/impact of recognition?
cluster26 <- c("open*", "promot*", "signific*") # promoting significant openness/transparency?
cluster27 <- c("achiev*", "provis*") # achievement provision?
cluster28 <- c("benefit*", "extend*", "revis*") # extended benefit revision?
cluster29 <- c("interoper*", "singl*") # single interoperability?
cluster30 <- c("adopt*", "technolog*") # technology adoption?
cluster31 <- c("develop*", "access*", "onlin*") # online access development?
cluster32 <- c("framework*", "nation*") # national framework?
cluster33 <- c("sector*", "solut*") # sectoral solution?

dict <- dictionary(list(electronic_service = cluster2,
                        
                        electronic_identity = cluster4))
print(dict)

# select tokens surrounding keywords related to cluster 1
# and only keep these tokens and their context of ±10 tokens
toks_tagged_corp_cluster1 <- tokens_keep(toks_tagged_corp, pattern = phrase(cluster1), window = 10) 

# apply the dictionary
dict_toks <- tokens_lookup(toks_tagged_corp, dict) 
head(dict_toks)
dict_toks_dfm <- dfm(dict_toks)
dict_toks_dfm <- convert(dict_toks_dfm, to="data.frame") 
print(dict_toks_dfm) # data frame: overview of mentions

# add missing document names as a column 
dict_toks_dfm2 <- tibble::rowid_to_column(dict_toks_dfm, "ID")
dict_toks_dfm2 <- dict_toks_dfm2[-c(2)]
dict_toks_dfm3 <- merge(txtfiles2, dict_toks_dfm2, by = "ID", all = TRUE)

# data frame as an HTML widget
'install.packages("devtools")
library(devtools)
devtools::install_github("kbenoit/quanteda.dictionaries") 
library(quanteda.dictionaries)
install.packages("htmlwidgets", type = "binary")
library(htmlwidgets)'
install.packages("DT", type = "binary")
library(DT)
DT::datatable(dict_toks_dfm3)

# subset the parts of the data frame containing the words of the dictionary (cluster 1: electronic_service)
library(stringr)
cluster1_feedback<-files_corpus_df[str_detect(files_corpus_df$text, paste(electronic_service, collapse="|")),]
# output: all documents with these words assigned a specific meaning/value 
# dict_toks_dfm3[2:3]

'### which stakeholders mention which key words ###

library(quanteda)
toks <- tokens(tagged_corp_all)
private_kw <- kwic(toks, phrase('privat*'))
View(private_kw)'

'# Comparing and Contrasting Corpora in Word Clouds

library(tm)
library(wordcloud)

length(list.files(wd)) # total number of files in that folder

files2019 <- list.files(path=wd,
                        pattern="2019+.*txt", recursive = TRUE)
files2020 <- list.files(path=wd,
                        pattern="2020+.*txt", recursive = TRUE)
#install.packages('readr')
#library(readr)
filelist2019 <- lapply(files2019, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
filelist2020 <- lapply(files2020, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
names(filelist2019) <- c("one","two","three")
names(filelist2020) <- c("one","two","three")
invisible(lapply(names(filelist2019), function(x) assign(x,filelist2019[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console
invisible(lapply(names(filelist2020), function(x) assign(x,filelist2020[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}
###
install.packages("stopwords")
library(stopwords)
custom.stopwords <- c(stopwords(source="smart"))
clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

filelist2019.vec<-clean.vec(filelist2019)
filelist2020.vec<-clean.vec(filelist2020)
filelist2019.vec <- paste(filelist2019.vec, collapse=" ")
filelist2020.vec <- paste(filelist2020.vec, collapse=" ")
allfiles <- c(filelist2019.vec, filelist2020.vec)
filelistcorpus <- VCorpus(VectorSource(allfiles))
filelisttdm <- TermDocumentMatrix(filelistcorpus)
filelisttdm.m <- as.matrix(filelisttdm)
colnames(filelisttdm.m) <- c("2019","2020")

### VISUALISATION: polarised tag plot

pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:4)]

library(plotrix)
common.words <- subset(filelisttdm.m, filelisttdm.m[, 1] > 0 & filelisttdm.m[, 2] > 0)
tail(common.words)
difference <- abs(common.words[, 1] - common.words[, 2])
common.words <- cbind(common.words, difference)
common.words <- common.words[order(common.words[, 3], decreasing = TRUE), ]
top50.df <- data.frame(x = common.words[1:50, 1],y = common.words[1:50, 2], labels = rownames(common.words[1:50, ]))
top50.df <- top50.df[order(top50.df$x, -rank(top50.df$y), decreasing = TRUE), ]

pyramid.plot(top50.df$x, top50.df$y,
             labels = top50.df$labels,
             lxcol = brewer.pal(8, "Blues"), rxcol = brewer.pal(8, "Blues"), 
             gap = 50, top.labels = c("2019 Consultations", "Words", "2020 Consultations"),
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL)'

###########################################################################
###
##
# Per stakeholder group: private stakeholders vs rest (both years together)
##
###
############################################################################

length(list.files(wd)) # total number of files in that folder
#library(tidyverse)

files_private <- list.files(path=wd, 
                            pattern="+(private).*txt", full.names = TRUE, recursive = TRUE)
filelist_private <- lapply(files_private, read.csv, 
                           sep = '\t', fileEncoding="latin1",
                           check.names=F, quote="", skipNul=T)

files_nonprivate <- list.files(path=wd, 
                               pattern=".txt", full.names = TRUE, recursive=TRUE) %>%
  stringr::str_subset(., "(private)", negate = TRUE)
filelist_nonprivate <- lapply(files_nonprivate, read.csv, 
                              sep = '\t', fileEncoding="latin1",
                              check.names=F, quote="", skipNul=T)

names(filelist_private) <- c("one","two","three")
names(filelist_nonprivate) <- c("one","two","three")

invisible(lapply(names(filelist_private), function(x) assign(x,filelist_private[[x]],envir=.GlobalEnv)))
invisible(lapply(names(filelist_nonprivate), function(x) assign(x,filelist_nonprivate[[x]],envir=.GlobalEnv)))

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}
custom.stopwords <- c(stopwords('english'))
clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

filelist_private.vec<-clean.vec(filelist_private)
filelist_nonprivate.vec<-clean.vec(filelist_nonprivate)

filelist_private.vec <- paste(filelist_private.vec, collapse=" ")
filelist_nonprivate.vec <- paste(filelist_nonprivate.vec, collapse=" ")

allstakeholdersfiles <- c(filelist_private.vec, filelist_nonprivate.vec)
allstakeholderscorpus <- VCorpus(VectorSource(allstakeholdersfiles))
allstakeholderscorpustdm <- TermDocumentMatrix(allstakeholderscorpus)
allstakeholderscorpustdm.m <- as.matrix(allstakeholderscorpustdm)
colnames(allstakeholderscorpustdm.m) <- c("Private stakeholders", "Non-private stakeholders")

'### VISUALISATION: commonality cloud 

pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:4)]

commonality.cloud(allstakeholderscorpustdm.m, scale=c(2, .5), random.order=FALSE,colors=pal) # no limit on common terms 
commonality.cloud(allstakeholderscorpustdm.m, scale=c(2, .5), max.words=50, random.order=FALSE,colors=pal)

### VISUALISATION: comparison cloud

comparison.cloud(allstakeholderscorpustdm.m,
                 random.order=FALSE,title.size=1,
                 scale=c(2.5, .5),rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90") # no word limit 

comparison.cloud(allstakeholderscorpustdm.m, max.words=50,random.order=FALSE,title.size=1.0,
                 scale=c(2, .5), rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90")
'
### VISUALISATION: polarised tag plot

library(plotrix)
common.words <- subset(allstakeholderscorpustdm.m, allstakeholderscorpustdm.m[, 1] > 0 & allstakeholderscorpustdm.m[, 2] > 0)
tail(common.words)
difference <- abs(common.words[, 1] - common.words[, 2])
common.words <- cbind(common.words, difference)
common.words <- common.words[order(common.words[, 3], decreasing = TRUE), ]
top50.df <- data.frame(x = common.words[1:50, 1],y = common.words[1:50, 2], labels = rownames(common.words[1:50, ]))

pyramid.plot(top50.df$x, top50.df$y,
             labels = top50.df$labels,
             lxcol = brewer.pal(8, "Blues"), rxcol = brewer.pal(8, "Blues"), 
             gap = 50, top.labels = c("Private stakeholders",
                                      "Words", "All other stakeholders"),
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL)

'
#
##
### EU vs stakeholders (see below)
##
#

# create a new folder with all files
wd_all <-'/Users/selinak./Desktop/Thesis/all txt files'
setwd(wd_all)

### VISUALISATION: term frequency (see Kwartler chapter 3)
library(tm)
library(quanteda)
library(readtext)

# read files
txtfiles_all <- list.files(pattern = "txt$") # all .txt files
txtcorpus_all <- c(txtfiles_all) # all .txt files
dir_all <- list.files(wd_all) #all .txt files

for(i in 1:length(dir_all)){
  x <-read.table(dir_all[i], sep='\n', quote="", stringsAsFactors = FALSE, fileEncoding="latin1", skipNul=T)
  j <- as.character(x[,1])
  j <- paste(j,collapse='')
  txtcorpus_all[i]<-j
}

author_all <- c(txtfiles_all)
names(txtcorpus_all) <- author_all 
groups_corp_all <- corpus(txtcorpus_all, docvars = data.frame(authors = names(txtcorpus_all)), unique_docnames=F) # load the data as a corpus
summary(groups_corp_all) # --> output: corpus consists of and shows 53 documents
tagged_corp_all <- corpus(c(txtfiles_all))
sect_corp_all <- corpus_segment(tagged_corp_all, "*")

# create a dataframe
library(readtext)
data_dir_all <- system.file(groups_corp_all, package = "readtext")
data_frame_all <- readtext(paste0(data_dir_all, 
                              '/Users/selinak./Desktop/Thesis/all txt files'),
                           text_field = "texts")
names(data_frame_all)
txtfiles_all <- list.files(pattern = ".txt", recursive = TRUE) 

library(qdap)
library(stringr)
library(tm)

files_txt_all <- lapply(txtfiles_all, function(x) {
  tmp_all <- readLines(x, skipNul=T)
  tmp_all <- str_c(tmp_all, collapse = "")
  tmp_all <- iconv(tmp_all, "ASCII", "UTF-8", sub="")
  return(tmp_all)
})
# --> warning messages

files_corpus_all <- VCorpus(VectorSource(files_txt_all))
toSpace_all <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))}) 
files_corpus_all <- tm_map(files_corpus_all, toSpace_all, "/") # additionally, replacing "/" with space
files_corpus_all <- tm_map(files_corpus_all, toSpace_all, "@") # 
files_corpus_all <- tm_map(files_corpus_all, toSpace_all, "\\|") #
files_corpus_all <- tm_map(files_corpus_all, toSpace_all, "[^[:graph:]]") 
files_corpus_all <- tm_map(files_corpus_all, content_transformer(tolower)) 
files_corpus_all <- tm_map(files_corpus_all, stripWhitespace) 
files_corpus_all <- tm_map(files_corpus_all, removePunctuation)
files_corpus_all <- tm_map(files_corpus_all, removeNumbers) # additionally, removing numbers
files_corpus_all <- tm_map(files_corpus_all,removeWords,stopwords('english'))
files_corpus_all <- tm_map(files_corpus_all, function(x)removeWords(x,stopwords()))
files_corpus_all <- tm_map(files_corpus_all, stemDocument) # additionally, text stemming (reducing words to their root form)
tm_map(files_corpus_all, function(x) iconv(x, to='UTF-8-MAC', sub='byte')) 
# --> output: <<VCorpus>> Metadata:  corpus specific: 0, document level (indexed): 0 Content:  documents: 77

tdm_files_all <- TermDocumentMatrix(files_corpus_all) # term document matrix 
dtm_m_all <- as.matrix(tdm_files_all) 
dtm_v_all <- sort(rowSums(dtm_m_all),decreasing=TRUE) # in order of decreasing value of frequency
dtm_d_all <- data.frame(word = names(dtm_v_all),freq=dtm_v_all) 
dtm_d2_all <- dtm_d_all[order(dtm_d_all[,2], decreasing=T),] 
head(dtm_d_all, 50) # top 50 most frequent words

### VISUALISATION: ranking of most frequent words

install.packages("ggplot2")
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)

ggplot(dtm_d_all[1:50,], aes(x=reorder(word, freq), y=freq))+
  geom_bar(stat="identity", fill='black')+
  coord_flip()+
  theme_gdocs()+
  geom_text(aes(label=freq),colour="white",hjust=1.25, size=5.0, family="Times New Roman")+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        text=element_text(size=12, family="Times New Roman"))+
  xlab("Words") +
  ylab("Word frequencies") +
  ggtitle("The 50 most frequent words")

### VISUALISATION: word cloud for most frequent words

install.packages("wordcloud")
install.packages("SnowballC") # for text stemming
install.packages("stringr") 
library(wordcloud)
library(SnowballC)
library(stringr)
library(tm)

dtm_all <- DocumentTermMatrix(files_corpus_all)
dtm2_all <- as.matrix(dtm_all) 
mostfreqterm_all <- findMostFreqTerms(dtm_all)
frequent_all <- colSums(dtm2_all)
frequent_all <- sort(frequent_all,decreasing = TRUE) # most frequent words 
frequent_d_all <- data.frame(word = names(frequent_all),freq=frequent_all) 
head(frequent_d_all, 5) # extra 
words_all <-names(frequent_all)
graphics.off()

wordcloud(words_all[1:50],random.order = FALSE, scale=c(2.5, .5), rot.per=0.40, colors=brewer.pal(8, "Blues"), frequent_all[1:100], family = "Times New Roman", font = 11) # in blue shades
wordcloud(words_all,random.order = FALSE, scale=c(2, .5), rot.per=0.40, colors=brewer.pal(5, "Blues"), frequent_all[1:100], family = "Times New Roman", font = 11) # in blue shades
'

'
###
##
# 2019 vs 2020 vs EU (2014, 2021)
##
###

# Comparing and Contrasting Corpora in Word Clouds

library(tm)
library(wordcloud)

length(list.files(wd_all)) # total number of files in that folder

files2019_all <- list.files(path=wd_all,
                            pattern="2019+.*txt", recursive = TRUE)
files2020_all <- list.files(path=wd_all,
                            pattern="2020+.*txt", recursive = TRUE)
files_EC_all <- list.files(path=wd_all,
                           pattern="+EC.*txt", recursive = TRUE)

filelist2019_all <- lapply(files2019_all, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
filelist2020_all <- lapply(files2020_all, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
filelist_EC_all <- lapply(files_EC_all, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)

names(filelist2019_all) <- c("one","two","three")
names(filelist2020_all) <- c("one","two","three")
names(filelist_EC_all) <- c("one","two")

invisible(lapply(names(filelist2019_all), function(x) assign(x,filelist2019_all[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console
invisible(lapply(names(filelist2020_all), function(x) assign(x,filelist2020_all[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console
invisible(lapply(names(filelist_EC_all), function(x) assign(x,filelist_EC_all[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

custom.stopwords <- c(stopwords('english'))
clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

filelist2019_all.vec <- clean.vec(filelist2019_all)
filelist2020_all.vec <- clean.vec(filelist2020_all)
filelist_EC_all.vec <- clean.vec(filelist_EC_all)

filelist2019_all.vec <- paste(filelist2019_all.vec, collapse=" ")
filelist2020_all.vec <- paste(filelist2020_all.vec, collapse=" ")
filelist_EC_all.vec <- paste(filelist_EC_all.vec, collapse=" ")

allfiles_all <- c(filelist2019_all.vec, filelist2020_all.vec, filelist_EC_all.vec)
filelistcorpus_all <- VCorpus(VectorSource(allfiles_all))
filelisttdm_all <- TermDocumentMatrix(filelistcorpus_all)
filelisttdm_all.m <- as.matrix(filelisttdm_all)
colnames(filelisttdm_all.m) <- c("2019","2020", "European Commission")

### VISUALISATION: commonality cloud 

pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:4)]

# commonality.cloud(filelisttdm_all.m, max.words=500, random.order=FALSE) # in black
commonality.cloud(filelisttdm_all.m, scale=c(2.5, .5), random.order=FALSE,colors=pal, family = "Times New Roman", font = 11) # no limit on common terms 
commonality.cloud(filelisttdm_all.m, scale=c(2.5, .5), max.words=50, random.order=FALSE,colors=pal, family = "Times New Roman", font = 11)

### VISUALISATION: comparison cloud 2019 vs 2020 vs EC (both documents)

comparison.cloud(filelisttdm_all.m,
                 random.order=FALSE,title.size=1,
                 scale=c(2.5, .5),rot.per=.1,
                 colors=c("darkblue", "darkgreen", "purple"),
                 family = "Times New Roman", font = 11,
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90") # no word limit 

comparison.cloud(filelisttdm_all.m, max.words=50,random.order=FALSE,title.size=1.0,
                 scale=c(2, .5), rot.per=.1,
                 colors=c("darkblue", "darkgreen", "purple"),
                 family = "Times New Roman", font = 11,
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90")'

'
###
##
# Stakeholders (2019, 2020) vs EC (2014, 2021)
##
###

# Comparing and Contrasting Corpora in Word Clouds

library(tm)
library(wordcloud)

length(list.files(wd_all)) # total number of files in that folder
pattern2019or2020 = c('2019', '2020')
files_2019_2020_all <- list.files(path=wd_all,
                            pattern=paste0(pattern2019or2020, collapse="|"), recursive = TRUE)
files_EC_all <- list.files(path=wd_all,
                           pattern="+EC.*txt", recursive = TRUE)

filelist_2019_2020_all <- lapply(files_2019_2020_all, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
filelist_EC_all <- lapply(files_EC_all, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)

names(filelist_2019_2020_all) <- c("one","two","three")
names(filelist_EC_all) <- c("one","two")

invisible(lapply(names(filelist_2019_2020_all), function(x) assign(x,filelist_2019_2020_all[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console
invisible(lapply(names(filelist_EC_all), function(x) assign(x,filelist_EC_all[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

custom.stopwords <- c(stopwords('english'))
clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

filelist_2019_2020_all.vec <- clean.vec(filelist_2019_2020_all)
filelist_EC_all.vec <- clean.vec(filelist_EC_all)

filelist_2019_2020_all.vec <- paste(filelist_2019_2020_all.vec, collapse=" ")
filelist_EC_all.vec <- paste(filelist_EC_all.vec, collapse=" ")

allfiles_20192020_20142021_all <- c(filelist_2019_2020_all.vec, filelist_EC_all.vec)
filelistcorpus_20192020_20142021_all <- VCorpus(VectorSource(allfiles_20192020_20142021_all))
filelisttdm_20192020_20142021_all <- TermDocumentMatrix(filelistcorpus_20192020_20142021_all)
filelisttdm_20192020_20142021_all.m <- as.matrix(filelisttdm_20192020_20142021_all)
colnames(filelisttdm_20192020_20142021_all.m) <- c("Public Consultations", "European Commission")

### VISUALISATION: commonality cloud 

pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:4)]

# commonality.cloud(filelisttdm_20192020_20142021_all.m, max.words=500, random.order=FALSE) # in black
commonality.cloud(filelisttdm_20192020_20142021_all.m, scale=c(2.5, .5), random.order=FALSE,colors=pal, family = "Times New Roman", font = 11) # no limit on common terms 
commonality.cloud(filelisttdm_20192020_20142021_all.m, scale=c(2.5, .5), max.words=50, random.order=FALSE,colors=pal, family = "Times New Roman", font = 11)

### VISUALISATION: comparison cloud 2019 vs 2020 vs EC (both documents)

comparison.cloud(filelisttdm_20192020_20142021_all.m,
                 random.order=FALSE,title.size=1,
                 scale=c(2.5, .5),rot.per=.1,
                 colors=c("darkblue", "darkgreen", "purple"),
                 family = "Times New Roman", font = 11,
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90") # no word limit 

comparison.cloud(filelisttdm_20192020_20142021_all.m, max.words=50,random.order=FALSE,title.size=1.0,
                 scale=c(2, .5), rot.per=.1,
                 colors=c("darkblue", "darkgreen", "purple"),
                 family = "Times New Roman", font = 11,
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90")

### VISUALISATION: polarised tag plot

library(plotrix)
common.words_20192020_20142021 <- subset(filelisttdm_20192020_20142021_all.m, filelisttdm_20192020_20142021_all.m[, 1] > 0 & filelisttdm_20192020_20142021_all.m[, 2] > 0)
tail(common.words_20192020_20142021)
difference_20192020_20142021 <- abs(common.words_20192020_20142021[, 1] - common.words_20192020_20142021[, 2])
common.words_20192020_20142021 <- cbind(common.words_20192020_20142021, difference_20192020_20142021)
common.words_20192020_20142021 <- common.words_20192020_20142021[order(common.words_20192020_20142021[, 3], decreasing = TRUE), ]
top50.df_20192020_20142021 <- data.frame(x = common.words_20192020_20142021[1:50, 1],y = common.words_20192020_20142021[1:50, 2], labels = rownames(common.words_20192020_20142021[1:50, ]))

pyramid.plot(top50.df_20192020_20142021$x, top50.df_20192020_20142021$y,
             labels = top50.df_20192020_20142021$labels,
             lxcol = brewer.pal(8, "Blues"), rxcol = brewer.pal(8, "Blues"), 
             gap = 65, top.labels = c("Public Consultations",
                                      "Words", "European Commission"),
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL)'

###
##
# EC proposals 2014 & 2021
##
###

# create a new folder with all files
wd_all <-'/Users/selinak./Desktop/Thesis/all txt files'
setwd(wd_all)

files_EC2014 <- list.files(path=wd_all,
                            pattern="2014+.*txt", recursive = TRUE)
files_EC2021 <- list.files(path=wd_all,
                           pattern="2021+.*txt", recursive = TRUE)

filelist_EC2014 <- lapply(files_EC2014, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
filelist_EC2021 <- lapply(files_EC2021, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)

names(filelist_EC2014) <- c("one","two")
names(filelist_EC2021) <- c("one")

invisible(lapply(names(filelist_EC2014), function(x) assign(x,filelist_EC2014[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console
invisible(lapply(names(filelist_EC2021), function(x) assign(x,filelist_EC2021[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

library(stopwords)
custom.stopwords <- c(stopwords(source="smart"))
library(tm)
clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

filelist_EC2014.vec <- clean.vec(filelist_EC2014)
filelist_EC2021.vec <- clean.vec(filelist_EC2021)

filelist_EC2014.vec <- paste(filelist_EC2014.vec, collapse=" ")
filelist_EC2021.vec <- paste(filelist_EC2021.vec, collapse=" ")

allfiles20142021_all <- c(filelist_EC2014.vec, filelist_EC2021.vec)
filelistcorpus20142021_all <- VCorpus(VectorSource(allfiles20142021_all))
filelisttdm20142021_all <- TermDocumentMatrix(filelistcorpus20142021_all)
filelisttdm20142021_all.m <- as.matrix(filelisttdm20142021_all)
colnames(filelisttdm20142021_all.m) <- c("European Commission 2014", "European Commission 2021")

tdm_files_EC <- TermDocumentMatrix(filelistcorpus20142021_all) # term document matrix 
dtm_m_EC <- as.matrix(tdm_files_EC) 
dtm_v_EC <- sort(rowSums(dtm_m_EC),decreasing=TRUE) # in order of decreasing value of frequency
dtm_d_EC <- data.frame(word = names(dtm_v_EC),freq=dtm_v_EC) 
dtm_d2_EC<-dtm_d[order(dtm_d_EC[,2], decreasing=T),] 
head(dtm_d_EC, 50) # top 50 most frequent words

### VISUALISATION: ranking of most frequent words

install.packages("ggplot2")
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)

ggplot(dtm_d_EC[1:50,], aes(x=reorder(word, freq), y=freq))+
  geom_bar(stat="identity", fill='black')+
  coord_flip()+
  theme_gdocs()+
  geom_text(aes(label=freq),colour="white",hjust=1.25, size=5.0, family="Times New Roman")+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        text=element_text(size=12, family="Times New Roman"))+
  xlab("Words") +
  ylab("Word frequencies") +
  ggtitle("The 50 most frequent words")

'
### VISUALISATION: Commonality cloud and comparison cloud 
install.packages("wordcloud")
library(wordcloud)
#commonality.cloud(filelisttdm20142021_all.m, max.words=500, random.order=FALSE) # in black
commonality.cloud(filelisttdm20142021_all.m, scale=c(2.5, .5), random.order=FALSE,colors=pal, family = "Times New Roman", font = 11) # no limit on common terms 
commonality.cloud(filelisttdm20142021_all.m, scale=c(2.5, .5), max.words=50, random.order=FALSE,colors=pal, family = "Times New Roman", font = 11)

comparison.cloud(filelisttdm20142021_all.m,
                 random.order=FALSE,title.size=1,
                 scale=c(2.5, .5),rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 family = "Times New Roman", font = 11,
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90") # no word limit 

comparison.cloud(filelisttdm20142021_all.m, max.words=50,random.order=FALSE,title.size=1.0,
                 scale=c(2, .5), rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 family = "Times New Roman", font = 11,
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90")
'
### VISUALISATION: polarised tag plot

library(plotrix)
common.words_20142021 <- subset(filelisttdm20142021_all.m, filelisttdm20142021_all.m[, 1] > 0 & filelisttdm20142021_all.m[, 2] > 0)
tail(common.words_20142021)
difference_20142021 <- abs(common.words_20142021[, 1] - common.words_20142021[, 2])
common.words_20142021 <- cbind(common.words_20142021, difference_20142021)
common.words_20142021 <- common.words_20142021[order(common.words_20142021[, 3], decreasing = TRUE), ]
top30.df_20142021 <- data.frame(x = common.words_20142021[1:30, 1],y = common.words_20142021[1:30, 2], labels = rownames(common.words_20142021[1:30, ]))

top30.df_20142021 <- top30.df_20142021[order(top30.df_20142021$x, -rank(top30.df_20142021$y), decreasing = TRUE), ] # ranked
top30.df_20142021_reverse <- top30.df_20142021[order(top30.df_20142021$y, -rank(top30.df_20142021$x), decreasing = TRUE), ] # reverse

pyramid.plot(top30.df_20142021$x, top30.df_20142021$y,
             labels = top30.df_20142021$labels,
             lxcol = brewer.pal(8, "Blues"), rxcol = brewer.pal(8, "Blues"), 
             gap = 65, top.labels = c("European Commission 2014",
                                      "Words", "European Commission 2021"),
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL)

### VISUALISATION: cluster analysis 

files_EC <- list.files(path=wd_all,
                       pattern="+EC.*txt", recursive = T)

library(qdap)
library(stringr)
library(tm)

files_EC_txt <- lapply(files_EC, function(x) {
  tmp <- readLines(x, skipNul=T)
  tmp <- str_c(tmp, collapse = "")
  tmp <- iconv(tmp, "ASCII", "UTF-8", sub="")
  return(tmp)
})


tm::tm_map
files_EC_corpus <- VCorpus(VectorSource(files_EC_txt)) 
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))}) 
files_EC_corpus <- tm_map(files_EC_corpus, toSpace, "/") # replacing "/" with space
files_EC_corpus <- tm_map(files_EC_corpus, toSpace, "@") # 
files_EC_corpus <- tm_map(files_EC_corpus, toSpace, "\\|") #
files_EC_corpus <- tm_map(files_EC_corpus, toSpace, "[^[:graph:]]") 
files_EC_corpus <- tm_map(files_EC_corpus, content_transformer(tolower)) 
files_EC_corpus <- tm_map(files_EC_corpus, stripWhitespace) 
files_EC_corpus <- tm_map(files_EC_corpus, removePunctuation)
files_EC_corpus <- tm_map(files_EC_corpus, removeNumbers) # removing numbers
files_EC_corpus <- tm_map(files_EC_corpus,removeWords,tm::stopwords(kind="smart"))
files_EC_corpus <- tm_map(files_EC_corpus, function(x)removeWords(x,tm::stopwords(kind="smart")))
files_EC_corpus <- tm_map(files_EC_corpus, stemDocument) # text stemming (reducing words to their root form)
tm_map(files_EC_corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte')) 
# --> output: <<VCorpus>> Metadata:  corpus specific: 0, document level (indexed): 0 Content:  documents: 2

tdm_files_EC <- TermDocumentMatrix(files_EC_corpus) # term document matrix

#### beforehand see above

'library(tm)
tdm_files_EC2 <- TermDocumentMatrix(files_EC_corpus,control=list(weighting=weightTf))
tdm_files_EC2.m<-as.matrix(tdm_files_EC2)  
term.freq_EC<-rowSums(tdm_files_EC2.m)

tdm_files_EC3 <- removeSparseTerms(tdm_files_EC2, sparse=0.99) 
hc <- hclust(dist(tdm_files_EC3, method="euclidean"), method="complete")
plot(hc,yaxt='n', main='Cluster Analysis')'

######################################

'
###
##
# Consultations 2019 vs EC proposal (2014)
##
###

files2019_all <- list.files(path=wd_all,
                            pattern="2019+.*txt", recursive = TRUE)
files_EC2014 <- list.files(path=wd_all,
                           pattern="2014+.*txt", recursive = TRUE)

filelist2019_all <- lapply(files2019_all, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
filelist_EC2014 <- lapply(files_EC2014, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)

names(filelist2019_all) <- c("one","two","three")
names(filelist_EC2014) <- c("one","two")

invisible(lapply(names(filelist2019_all), function(x) assign(x,filelist2019_all[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console
invisible(lapply(names(filelist_EC2014), function(x) assign(x,filelist_EC2014[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

custom.stopwords <- c(stopwords('english'))
clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

filelist2019_all.vec <- clean.vec(filelist2019_all)
filelist_EC2014.vec <- clean.vec(filelist_EC2014)

filelist2019_all.vec <- paste(filelist2019_all.vec, collapse=" ")
filelist_EC2014.vec <- paste(filelist_EC2014.vec, collapse=" ")

allfiles20192014_all <- c(filelist2019_all.vec, filelist_EC2014.vec)
filelistcorpus20192014_all <- VCorpus(VectorSource(allfiles20192014_all))
filelisttdm20192014_all <- TermDocumentMatrix(filelistcorpus20192014_all)
filelisttdm20192014_all.m <- as.matrix(filelisttdm20192014_all)
colnames(filelisttdm20192014_all.m) <- c("2019", "European Commission")

### VISUALISATION: Commonality cloud and comparison cloud 

# commonality.cloud(filelisttdm20192014_all.m, max.words=500, random.order=FALSE) # in black
commonality.cloud(filelisttdm20192014_all.m, scale=c(2.5, .5), random.order=FALSE,colors=pal, family = "Times New Roman", font = 11) # no limit on common terms 
commonality.cloud(filelisttdm20192014_all.m, scale=c(2.5, .5), max.words=50, random.order=FALSE,colors=pal, family = "Times New Roman", font = 11)

comparison.cloud(filelisttdm20192014_all.m,
                 random.order=FALSE,title.size=1,
                 scale=c(2.5, .5),rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 family = "Times New Roman", font = 11,
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90") # no word limit 

comparison.cloud(filelisttdm20192014_all.m, max.words=50,random.order=FALSE,title.size=1.0,
                 scale=c(2, .5), rot.per=.1,
                 colors=c("darkblue", "darkgreen", "purple"),
                 family = "Times New Roman", font = 11,
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90")

### VISUALISATION: polarised tag plot

library(plotrix)
common.words_20192014 <- subset(filelisttdm20192014_all.m, filelisttdm20192014_all.m[, 1] > 0 & filelisttdm20192014_all.m[, 2] > 0)
tail(common.words_20192014)
difference_20192014 <- abs(common.words_20192014[, 1] - common.words_20192014[, 2])
common.words_20192014 <- cbind(common.words_20192014, difference_20192014)
common.words_20192014 <- common.words_20192014[order(common.words_20192014[, 3], decreasing = TRUE), ]
top50.df_20192014 <- data.frame(x = common.words_20192014[1:50, 1],y = common.words_20192014[1:50, 2], labels = rownames(common.words_20192014[1:50, ]))

pyramid.plot(top50.df_20192014$x, top50.df_20192014$y,
             labels = top50.df_20192014$labels,
             lxcol = brewer.pal(8, "Blues"), rxcol = brewer.pal(8, "Blues"), 
             gap = 65, top.labels = c("2019",
                                      "Words", "EC 2014"),
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL)
'

'###
##
# Consultations 2020 vs EC proposal (2021) 
##
###

files2020_all <- list.files(path=wd_all,
                            pattern="2020+.*txt", recursive = TRUE)
files_EC2021 <- list.files(path=wd_all,
                           pattern="2021+.*txt", recursive = TRUE)

filelist2020_all <- lapply(files2020_all, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
filelist_EC2021 <- lapply(files_EC2021, read.csv, sep = '\t', fileEncoding='latin1',check.names=F, quote="", skipNul=T)
###
names(filelist2020_all) <- c("one","two","three")
names(filelist_EC2021) <- c("one")

invisible(lapply(names(filelist2020_all), function(x) assign(x,filelist2020_all[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console
invisible(lapply(names(filelist_EC2021), function(x) assign(x,filelist_EC2021[[x]],envir=.GlobalEnv))) #the invisible function keeps lapply from spitting out the data.frames to the console

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

custom.stopwords <- c(stopwords('english'))
clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

filelist2020_all.vec <- clean.vec(filelist2020_all)
filelist_EC2021.vec <- clean.vec(filelist_EC2021)

filelist2020_all.vec <- paste(filelist2020_all.vec, collapse=" ")
filelist_EC2021.vec <- paste(filelist_EC2021.vec, collapse=" ")

allfiles20202021_all <- c(filelist2020_all.vec, filelist_EC2021.vec)
filelistcorpus20202021_all <- VCorpus(VectorSource(allfiles20202021_all))
filelisttdm20202021_all <- TermDocumentMatrix(filelistcorpus20202021_all)
filelisttdm20202021_all.m <- as.matrix(filelisttdm20202021_all)
colnames(filelisttdm20202021_all.m) <- c("2020", "European Commission")

### VISUALISATION: Commonality cloud and comparison cloud 

# commonality.cloud(filelisttdm20202021_all.m, max.words=500, random.order=FALSE) # in black
commonality.cloud(filelisttdm20202021_all.m, scale=c(2.5, .5), random.order=FALSE,colors=pal, family = "Times New Roman", font = 11) # no limit on common terms 
commonality.cloud(filelisttdm20202021_all.m, scale=c(2.5, .5), max.words=50, random.order=FALSE,colors=pal, family = "Times New Roman", font = 11)

comparison.cloud(filelisttdm20202021_all.m,
                 random.order=FALSE,title.size=1,
                 scale=c(2.5, .5),rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 family = "Times New Roman", font = 11,
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90") # no word limit 

comparison.cloud(filelisttdm20202021_all.m, max.words=50,random.order=FALSE,title.size=1.0,
                 scale=c(2, .5), rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 family = "Times New Roman", font = 11,
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90")

### VISUALISATION: polarised tag plot

library(plotrix)
common.words_20202021 <- subset(filelisttdm20202021_all.m, filelisttdm20202021_all.m[, 1] > 0 & filelisttdm20202021_all.m[, 2] > 0)
tail(common.words_20202021)
difference_20202021 <- abs(common.words_20202021[, 1] - common.words_20202021[, 2])
common.words_20202021 <- cbind(common.words_20202021, difference_20202021)
common.words_20202021 <- common.words_20202021[order(common.words_20202021[, 3], decreasing = TRUE), ]
top50.df_20202021 <- data.frame(x = common.words_20202021[1:50, 1],y = common.words_20202021[1:50, 2], labels = rownames(common.words_20202021[1:50, ]))

pyramid.plot(top50.df_20202021$x, top50.df_20202021$y,
             labels = top50.df_20202021$labels,
             lxcol = brewer.pal(8, "Blues"), rxcol = brewer.pal(8, "Blues"), 
             gap = 65, top.labels = c("2020",
                                      "Words", "EC 2021"),
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL)'

###
##
# Per stakeholder group (2019): private stakeholders vs rest 
##
###

length(list.files(wd_all)) # total number of files in that folder
files2019_private <- list.files(path=wd_all, 
                            pattern="2019.+private", full.names = TRUE, recursive = TRUE)

filelist2019_private <- lapply(files2019_private, read.csv, 
                           sep = '\t', fileEncoding="latin1",
                           check.names=F, quote="", skipNul=T)
files2019_nonprivate <- list.files(path=wd_all, 
                               pattern="2019+.*txt", full.names = TRUE, recursive=TRUE) %>%
  stringr::str_subset(., "(private)", negate = TRUE)

filelist2019_nonprivate <- lapply(files2019_nonprivate, read.csv, 
                              sep = '\t', fileEncoding="latin1",
                              check.names=F, quote="", skipNul=T)

names(filelist2019_private) <- c("one","two","three")
names(filelist2019_nonprivate) <- c("one","two","three")

invisible(lapply(names(filelist2019_private), function(x) assign(x,filelist2019_private[[x]],envir=.GlobalEnv)))
invisible(lapply(names(filelist2019_nonprivate), function(x) assign(x,filelist2019_nonprivate[[x]],envir=.GlobalEnv)))

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}
custom.stopwords <- c(stopwords('english'))
clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

filelist2019_private.vec<-clean.vec(filelist2019_private)
filelist2019_nonprivate.vec<-clean.vec(filelist2019_nonprivate)

filelist2019_private.vec <- paste(filelist2019_private.vec, collapse=" ")
filelist2019_nonprivate.vec <- paste(filelist2019_nonprivate.vec, collapse=" ")

allstakeholdersfiles2019 <- c(filelist2019_private.vec, filelist2019_nonprivate.vec)
allstakeholderscorpus2019 <- VCorpus(VectorSource(allstakeholdersfiles2019))
allstakeholderscorpustdm2019 <- TermDocumentMatrix(allstakeholderscorpus2019)
allstakeholderscorpustdm.m2019 <- as.matrix(allstakeholderscorpustdm2019)
colnames(allstakeholderscorpustdm.m2019) <- c("Private stakeholders", "Non-private stakeholders")

### VISUALISATION: Commonality cloud and comparison cloud 

pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:4)]

commonality.cloud(allstakeholderscorpustdm.m2019, scale=c(2, .5), random.order=FALSE,colors=pal) # no limit on common terms 
commonality.cloud(allstakeholderscorpustdm.m2019, scale=c(2, .5), max.words=50, random.order=FALSE,colors=pal)

comparison.cloud(allstakeholderscorpustdm.m2019,
                 random.order=FALSE,title.size=1,
                 scale=c(2.5, .5),rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90") # no word limit 

comparison.cloud(allstakeholderscorpustdm.m2019, max.words=50,random.order=FALSE,title.size=1.0,
                 scale=c(2, .5), rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90")

### VISUALISATION: polarised tag plot

library(plotrix)
common.words2019 <- subset(allstakeholderscorpustdm.m2019, allstakeholderscorpustdm.m2019[, 1] > 0 & allstakeholderscorpustdm.m2019[, 2] > 0)
tail(common.words2019)
difference2019 <- abs(common.words2019[, 1] - common.words2019[, 2])
common.words2019 <- cbind(common.words2019, difference2019)
common.words2019 <- common.words2019[order(common.words2019[, 3], decreasing = TRUE), ]
top50.df_2019 <- data.frame(x = common.words2019[1:50, 1],y = common.words2019[1:50, 2], labels = rownames(common.words2019[1:50, ]))

pyramid.plot(top50.df_2019$x, top50.df_2019$y,
             labels = top50.df_2019$labels,
             lxcol = brewer.pal(8, "Blues"), rxcol = brewer.pal(8, "Blues"), 
             gap = 50, top.labels = c("Private stakeholders",
                                      "Words", "All other stakeholders"),
             main = "Words in Common (Public Consultations 2019)", laxlab = NULL,
             raxlab = NULL, unit = NULL)

###
##
# Per stakeholder group (2020): private stakeholders vs rest 
##
###

length(list.files(wd_all)) # total number of files in that folder
files2020_private <- list.files(path=wd_all, 
                                pattern="2020.+private", full.names = TRUE, recursive = TRUE)

filelist2020_private <- lapply(files2020_private, read.csv, 
                               sep = '\t', fileEncoding="latin1",
                               check.names=F, quote="", skipNul=T)

files2020_nonprivate <- list.files(path=wd_all, 
                                   pattern="2020+.*txt", full.names = TRUE, recursive=TRUE) %>%
  stringr::str_subset(., "(private)", negate = TRUE)

filelist2020_nonprivate <- lapply(files2020_nonprivate, read.csv, 
                                  sep = '\t', fileEncoding="latin1",
                                  check.names=F, quote="", skipNul=T)

names(filelist2020_private) <- c("one","two","three")
names(filelist2020_nonprivate) <- c("one","two","three")

invisible(lapply(names(filelist2020_private), function(x) assign(x,filelist2020_private[[x]],envir=.GlobalEnv)))
invisible(lapply(names(filelist2020_nonprivate), function(x) assign(x,filelist2020_nonprivate[[x]],envir=.GlobalEnv)))

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}
custom.stopwords <- c(stopwords('english'))
clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

filelist2020_private.vec<-clean.vec(filelist2020_private)
filelist2020_nonprivate.vec<-clean.vec(filelist2020_nonprivate)

filelist2020_private.vec <- paste(filelist2020_private.vec, collapse=" ")
filelist2020_nonprivate.vec <- paste(filelist2020_nonprivate.vec, collapse=" ")

allstakeholdersfiles2020 <- c(filelist2020_private.vec, filelist2020_nonprivate.vec)
allstakeholderscorpus2020 <- VCorpus(VectorSource(allstakeholdersfiles2020))
allstakeholderscorpustdm2020 <- TermDocumentMatrix(allstakeholderscorpus2020)
allstakeholderscorpustdm.m2020 <- as.matrix(allstakeholderscorpustdm2020)
colnames(allstakeholderscorpustdm.m2020) <- c("Private stakeholders", "Non-private stakeholders")

### VISUALISATION: Commonality cloud and comparison cloud 

pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:4)]

commonality.cloud(allstakeholderscorpustdm.m2020, scale=c(2, .5), random.order=FALSE,colors=pal) # no limit on common terms 
commonality.cloud(allstakeholderscorpustdm.m2020, scale=c(2, .5), max.words=50, random.order=FALSE,colors=pal)

comparison.cloud(allstakeholderscorpustdm.m2020,
                 random.order=FALSE,title.size=1,
                 scale=c(2.5, .5),rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90") # no word limit 
# --> Warning message:
## In comparison.cloud(allstakeholderscorpustdm.m2020, random.order = FALSE,  :
### interoperability could not be fit on page. It will not be plotted.

comparison.cloud(allstakeholderscorpustdm.m2020, max.words=50,random.order=FALSE,title.size=1.0,
                 scale=c(2, .5), rot.per=.1,
                 colors=c("darkblue", "darkgreen"),
                 use.r.layout=FALSE,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90")

### VISUALISATION: polarised tag plot

library(plotrix)
common.words2020 <- subset(allstakeholderscorpustdm.m2020, allstakeholderscorpustdm.m2020[, 1] > 0 & allstakeholderscorpustdm.m2020[, 2] > 0)
tail(common.words2020)
difference2020 <- abs(common.words2020[, 1] - common.words2020[, 2])
common.words2020 <- cbind(common.words2020, difference2020)
common.words2020 <- common.words2020[order(common.words2020[, 3], decreasing = TRUE), ]
top50.df_2020 <- data.frame(x = common.words2020[1:50, 1],y = common.words2020[1:50, 2], labels = rownames(common.words2020[1:50, ]))

pyramid.plot(top50.df_2020$x, top50.df_2020$y,
             labels = top50.df_2020$labels,
             lxcol = brewer.pal(8, "Blues"), rxcol = brewer.pal(8, "Blues"), 
             gap = 50, top.labels = c("Private stakeholders",
                                      "Words", "All other stakeholders"),
             main = "Words in Common (Public Consultations 2020)", laxlab = NULL,
             raxlab = NULL, unit = NULL)
