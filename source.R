transform<-function(Contacts,HT,X,Y,x,y,Chemin,obs){
#transforamtion .csv issu de tadarida ver .xlsx pour analyse des sons
#version 1 du 20200324
#Vincent Parmain
#library(stringr)
#pour debugage du script uniquement:
#Contacts <- read.csv(file.choose(), header=TRUE, sep=";", fileEncoding="utf-8") 
#HT<-read.delim(file.choose(), sep="\t", header=TRUE)
#X<-2.428045
#Y<-43.098007
#obs<-"vincent"
#Chemin<-"E:/2022_loubatiere/sons/castillou_bas/kal"
#Creer un identifiant unique pour les lignes du tableau, juste pour s?curit? et s'y retrouver
#Contacts<-contacts
Contacts$ID<-seq(1,nrow(Contacts))

#Creer et ajouter les colonnes de verif des sequences + autres colonnes pour calculs ci-apres
nblig<-nrow(Contacts)
verif<-setNames(data.frame(matrix(ncol = 10, nrow = nblig)), 
                c("Lien", "Verif", "Comment", "GrpeTax", "Taxon1", "Taxon2", "Taxon3", "Taxon4","X_RGF93","Y_RGF93"))
Contacts<-cbind(Contacts,verif)
Contacts$Lien<-as.character(Contacts$Lien)
Contacts$Verif<-as.character(Contacts$Verif)
Contacts$Comment<-as.character(Contacts$Comment)
Contacts$GrpeTax<-as.character(Contacts$GrpeTax)
Contacts$Taxon1<-as.character(Contacts$Taxon1)
Contacts$Taxon2<-as.character(Contacts$Taxon2)
Contacts$Taxon3<-as.character(Contacts$Taxon3)
Contacts$Taxon4<-as.character(Contacts$Taxon4)
Contacts$X_RGF93<-as.numeric(Contacts$X_RGF93)
Contacts$Y_RGF93<-as.numeric(Contacts$Y_RGF93)

rm(verif)

#Creation colonne Nb_contact 
concatname<-table(Contacts$"nom.du.fichier") #cree une table d'occurrence avec une ligne par nom de fichier et son nb d'occurrence
concatname<-as.data.frame(concatname) #convertit la table en data.frame
colnames(concatname) <- c("nom.du.fichier","Nb_contact") #change le nom des colonnes
Contacts<-merge(Contacts,concatname,by="nom.du.fichier")  #fait la jointure avec donn?es Contacts
rm(concatname) #nettoie

#Remplissage colonne Contact sans la boucle (bcp plus rapide)
Contacts1<-subset(Contacts, Nb_contact =="1",ID) #cree une sous-table avec seulement les lignes avec 1 contact, et seulement la colonne ID
Contacts1$Contact<-"Premier"

if(any((Contacts$Nb_contact=="2")==1)){
  Contacts2<-subset(Contacts, Nb_contact =="2",ID) #Idem avec seulement les lignes avec 2 contacts
  Contacts2$Contact<-c("Premier", "Deuxieme")  
}

if(any((Contacts$Nb_contact=="3")==1)){
  Contacts3<-subset(Contacts, Nb_contact =="3",ID)
  Contacts3$Contact<-c("Premier", "Deuxieme", "Troisieme")
}

if(any((Contacts$Nb_contact=="4")==1)){
  Contacts4<-subset(Contacts, Nb_contact =="4",ID)
  Contacts4$Contact<-c("Premier", "Deuxieme", "Troisieme","Quatrieme")
}
if(any((Contacts$Nb_contact=="5")==1)){
  Contacts5<-subset(Contacts, Nb_contact =="5",ID)
  Contacts5$Contact<-c("Premier", "Deuxieme", "Troisieme","Quatrieme","Cinquième")
}
if(any((Contacts$Nb_contact=="6")==1)){
  Contacts6<-subset(Contacts, Nb_contact =="6",ID)
  Contacts6$Contact<-c("Premier", "Deuxieme", "Troisieme","Quatrieme","Cinquième", "Sixième")
}
#Agrege toutes ces nouvelles tables en testant leur existence!
Contacts7<-Contacts1
if(exists("Contacts2")){
  Contacts7<-rbind(Contacts7,Contacts2)
}
if(exists("Contacts3")){
  Contacts7<-rbind(Contacts7,Contacts3)
}
if(exists("Contacts4")){
  Contacts7<-rbind(Contacts7,Contacts4)
}
if(exists("Contacts5")){
  Contacts7<-rbind(Contacts7,Contacts5)
}
if(exists("Contacts6")){
  Contacts7<-rbind(Contacts7,Contacts6)
}
Contacts<-merge(Contacts,Contacts7,by="ID",all.x=TRUE)

rm(Contacts1)
if(exists("Contacts2")){
  rm(Contacts2)
}
if(exists("Contacts3")){
  rm(Contacts3)
}
if(exists("Contacts4")){
  rm(Contacts4)
}
if(exists("Contacts5")){
  rm(Contacts5)
}
if(exists("Contacts6")){
  rm(Contacts6)
}
rm(Contacts7)


#Ajouter colonne Duree
Contacts$Duree<-Contacts$temps_fin - Contacts$temps_debut   #cree la colone avec formule en fin de tableau

#reecrire date et heure lisible
Contacts$DAT_HEUR<-str_sub(Contacts$"nom.du.fichier",-19,-5)#-15-1 si pas kaleidoscope
#Contacts$DAT_HEUR<-str_extract(Contacts$"nom.du.fichier","_.{8}_.{6}")
Contacts$DAT_HEUR<-chartr(Contacts$DAT_HEUR, old="_", new=" ")
Contacts$DAT_HEUR<-str_c(str_sub(Contacts$DAT_HEUR, 1,4),"/",str_sub(Contacts$DAT_HEUR, 5,6),"/",str_sub(Contacts$DAT_HEUR,7,8)," "
                         ,str_sub(Contacts$DAT_HEUR,10,11), ":",str_sub(Contacts$DAT_HEUR, 12,13),":"
                         ,str_sub(Contacts$DAT_HEUR, 14,15))

Contacts$Date<-str_sub(Contacts$DAT_HEUR,1,10)
Contacts$Heure<-str_sub(Contacts$DAT_HEUR,12,19)
Contacts$Date<-strptime(Contacts$Date,"%Y/%m/%d") #conversion colonne en format date lisible par R

#Idem pour generer Date_Nuit, mais sans boucle, bcp plus rapide
Datnuit<-subset(Contacts,Contacts$Heure > "12:00",c(ID,Date))
Datnuit$Date_Nuit<-as.Date(Datnuit$Date)
Datnuit2<-subset(Contacts,Contacts$Heure <= "12:00",c(ID,Date))
Datnuit2$Date_Nuit<-as.Date(Datnuit2$Date) - 1

Datnuit3<-rbind(Datnuit,Datnuit2)
Datnuit3<-subset(Datnuit3, select = -Date )
Contacts<-merge(Contacts,Datnuit3,by="ID",all.x=TRUE)
rm(Datnuit,Datnuit2,Datnuit3)


#suite
Contacts$Date<-as.character(Contacts$Date) #repasser la colonne en format caract?re, pour ?viter mise en forme auto par R ? cause des manips sur T/H
Contacts$Heure<-as.character(Contacts$Heure) #repasser la colonne en format caract?re, pour ?viter mise en forme auto par R ? cause des manips sur T/H
Contacts$Date_Nuit<-as.character(Contacts$Date_Nuit) #repasser la colonne en format caract?re, pour ?viter mise en forme auto par R ? cause des manips sur T/H
Contacts$Date<-gsub("-","/",Contacts$Date) #remplacer le caract?re "-" par "/"
Contacts$Date_Nuit<-gsub("-","/",Contacts$Date_Nuit) #remplacer le caract?re "-" par "/"

Contacts$DAT_HEUR<-as.POSIXct(Contacts$DAT_HEUR, format= "%Y/%m/%d %H:%M:%S", tz="UTC")#definir comme date heure pour int?gration des donn?es HOBO

#Ajouter Lien hypertexte de la colonne Lien

Contacts$Lien<-paste("=LIEN_HYPERTEXTE(", '"',
                     gsub("/","\\\\",Chemin),
                     '\\',
                     gsub("/","-",Contacts$Date_Nuit),
                     '\\',
                     Contacts$"nom.du.fichier",'.wav")'
                     ,sep="")

for (i in 1:nrow(Contacts)){
  Contacts$tri[i]<-sample(x=10000000, size=1,replace=FALSE)/10000000
}

############################################################################
#lire fichier HOBO et déterminer le pas de temps d'acquisition des donnes  #
############################################################################

  HT[,2]<-as.character(HT[,2])
  HT[,2]<-as.POSIXct(HT[,2], format= "%Y/%m/%d %H:%M:%S", tz="UTC")
  deltaT<-HT[2,2]-HT[1,2]#definir le pas de temps d'acquisition
  deltaT<-as.numeric(deltaT)
  
  
  #creer colonnes et completer modele HOBO
  nomHOBO<-colnames(HT[3])
  nomHOBO<-str_sub(nomHOBO, start=nchar(nomHOBO)-8, end=nchar(nomHOBO)-1)
  Contacts$Modele_TH<-paste("HOBO", nomHOBO)
  Contacts$Temp<-NA
  Contacts$Hygr<-NA
  
  
  #boucle pour chercher T et H correspondant a deltaT/2 soit ligne vrai dans HTp
  for (i in 1:nrow(Contacts)){
    a<-Contacts$DAT_HEUR[i]
    #b<-abs(difftime(a, HT[,1], units="secs"))<=7.5*60
    b<- (-(deltaT/2)*60)<=difftime(a, HT[,2], units="secs") & difftime(a, HT[,2], units="secs")<((deltaT/2)*60)
    HTp<-cbind(HT,b)
    
    Temp<-HTp[HTp$b==TRUE,3]  #ca ne passe pas en direct, pourquoi?
    Hygr<-HTp[HTp$b==TRUE,4]
    Contacts$Temp[i]<-Temp
    Contacts$Hygr[i]<-Hygr
    
    rm(HTp,Temp, Hygr, b)#nettoyer
  }



######################################################################
######  Fin du script pour l'integration des donn?es HOBO  ###########
######################################################################

Contacts$Duree<-Contacts$temps_fin - Contacts$temps_debut   #R?tablir colonne dur?e si besoin
Contacts<-subset(Contacts, select = -DAT_HEUR ) #Supprimer la colonne DAT_HEUR qui ne sert que pour importer donn?es T/H
Contacts<-subset(Contacts, select = -ID ) #Idem colonne ID
Contacts<-Contacts[order(Contacts$"nom.du.fichier"), ] #remettre le tableau dans l'ordre (tri sur colonne ID)
Contacts$Verif<-"NON"#remplir colonne verif
Contacts$X_RGF93<-as.numeric(X)#remplir coordonnees
Contacts$Y_RGF93<-as.numeric(Y)
Contacts$X_WGS84<-as.numeric(x)
Contacts$Y_WGS84<-as.numeric(y)

Contacts$observateur_taxon<-obs
Contacts$Comment<-""
Contacts$GrpeTax<-""
Contacts$Taxon1<-""
Contacts$Taxon2<-""
Contacts$Taxon3<-""
Contacts$Taxon4<-""
Contacts$observateur_probabilite <-""
Contacts$validateur_taxon <-""
Contacts$validateur_probabilite<-""



#Remet les colonnes dans un ordre pratique

  Contacts<-Contacts[,c(
                        "nom.du.fichier",
                        "Lien",
                        "Contact",
                        "Nb_contact",
                        "temps_debut",
                        "temps_fin",
                        "Duree",
                        "frequence_mediane",
                        "tadarida_taxon",
                        "tadarida_probabilite",
                        "tadarida_taxon_autre",
                        "Verif",
                        "Comment",
                        "GrpeTax",
                        "Taxon1",
                        "Taxon2",
                        "Taxon3",
                        "Taxon4",
                        "observateur_taxon",
                        "observateur_probabilite",
                        "validateur_taxon",
                        "validateur_probabilite",
                        "X_RGF93",
                        "Y_RGF93",
                        "X_WGS84",
                        "Y_WGS84",
                        "Date",
                        "Heure",
                        "Date_Nuit",
                        "Modele_TH",
                        "Temp",
                        "Hygr")]

#rm(list=(ls()[ls()!="Contacts"]))

return(Contacts)

}

#fonctions de conversion rgf93 <=> WGS84
#avec sf
rgf2wgs<-function(X,Y){
  loc<-st_sfc(st_point(c(X,Y)))%>% st_set_crs(2154)%>% st_transform(4326)
  return(loc[[1]])
}

wgs2rgf<-function(X,Y){
  loc<-st_sfc(st_point(c(X,Y)))%>% st_set_crs(4326)%>% st_transform(2154)
  return(loc[[1]])
}
