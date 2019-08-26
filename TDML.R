##CHARGEMENT DES DONNEES
	#On charge les librairies
	library(kernlab)
	library(caret)
	library(tidyverse)
	#On charge les donnees
	data(spam)
	View(spam)
	#On génère une graine
	set.seed(100)
	#On enregistre les donnée test
	ind_train <- createDataPartition(spam$type,p=0.8,list=F)
	length(ind_train)
	head(ind_train)

##CREATION DES DONNEES D ENTRAINEMENT
	#on assigne la matrice spam qui contient toutes les lignes qui sont dans ind-train et toutes les colonnes de spam
	Dtrain<-spam[ind_train,]; dim(Dtrain)
	#Creer la table des indices de test à partir de toutes les données qui sont pas dans Dtrain
	Dtest<-spam[-ind_train,]; dim(Dtest)

#paramètre d'apprentissage pour la validation croisée
param_train<-trainControl(method="cv",number=10)

##METHODE GLM
	##Validation croisee
		#on lance la fonction train avec la méthode "glm"
		fit_glm<-train(type~.,data=Dtrain,method="glm",trControl=param_train)
		#détail des resultats, le taux pour chaque fold, avec le champ resample
		fit_glm$resample
		#donne le taux moyen de tous les taux d'erreur
		fit_glm

	#On cree la variable qui contiendra la prediction
	pred_glm<-predict(fit_glm,newdata=Dtest)
	head(pred_glm)
	table(pred_glm)
	#Compter le nb de fois où on se trompe quand on utilise les données de test avec mon algo entrainé
	#affiche la matric de confusion
	table(predite=pred_glm,observee=Dtest$type)
	#la moyenne des erreurs sur la variable type
	mean(pred_glm!=Dtest$type)

head(Dtest$type) --> affiche
##METHODE RF
	##Validation croisee
		#on lance la fonction train avec la méthode "rf"
		fit_rf<-train(type~.,data=Dtrain,method="rf",trControl=param_train)
		#Affichage des taux
		fit_rf$resample
		#donne le taux moyen de tous les taux d'erreur
		fit_rf


	#On cree la variable qui contiendra la prediction
	pred_rf<-predict(fit_rf,newdata=Dtest)
	head(pred_rf)
	table(pred_rf)
	#Compter le nb de fois où on se trompe quand on utilise les données de test avec mon algo entrainé
	#affiche la matric de confusion
	table(predite=pred_rf,observee=Dtest$type)
	#la moyenne des erreurs sur la variable type
	mean(pred_rf!=Dtest$type)

	#Afficher les 6 premieres lignes 
	#on observe qu'il y a 1 erreur
	head(Dtest$type)
	head(pred_rf)

	
