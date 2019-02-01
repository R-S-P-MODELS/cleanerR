loadNamespace("plyr")
loadNamespace("data.table")
require(plyr)
require(data.table)
#'\code{candidates} Asks for a dataframe and some parameters and returns how close the collums chosen can predict the goal collum
#' Should be used mostly with generate_candidates or preferably best_vector in case you only want the best combination possible for prediction
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param vec a vector of collums you wish to test if can be used to predict the values
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#'@export
candidates=function(df,goal,vec,repetitions,trigger=1){
y=list()
### nova linha
if(class(df)[1]=="data.table")
	candidates_table(df,goal,vec,repetitions,trigger)
else{
df=df[which(!is.na(df[,goal]) ),]
### fim da nova
if(length(vec)==1)
	x=length(unique(df[,vec] )   )
else
	x=nrow(unique(df[,vec] )   )
#vec[length(vec)+1]=goal
vec=append(vec,goal)
m=plyr::count(df[,vec])
z=nrow(m)
#y[[1]]=z-x
valores_m=as.numeric(names(table(m[,ncol(m)])))
#print(valores_m)
#if(trigger==1)
#  aceitacao=sum(valores_m<=1) # aceitação =0 eu aceito senao existe chaves unicas em vec+goal
#else
aux_trigger=as.numeric(table(plyr::count(df[,vec])$freq)[which(names(table(plyr::count(df[,vec])$freq))==1)])/sum(as.numeric(table(plyr::count(df[,vec])$freq)))
if(length(aux_trigger>0))
aceitacao=as.numeric( (1-aux_trigger)<trigger  )
else
aceitacao=0


y[[2]]=aceitacao
# como z-x deve ser 0 e aceitacao retornar 0 para aceitarmos
#print(c(z,x,aceitacao))
#cat("o valor de z é:",z,"x vale",x,"aceitacao",aceitacao,"o vetor já sera",vec,"\n")
w=abs(z-x)
#print(c(w,vec))
y[[1]]=w
w=as.numeric(w>repetitions)
#return(w)
return(y)
}
#return( w +aceitacao) # apenas valor 0 aceita vec para esta goal
}
#'\code{candidates_table} candidates implementation that asks for a data.table object
#' @param df A data.table with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param vec a vector of collums you wish to test if can be used to predict the values
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#'@export
candidates_table=function(df,goal,vec,repetitions,trigger=1){
y=list()
### nova linha
auxiliar=which(!is.na(df[,..goal]) )
df=df[auxiliar,]
### fim da nova
#if(length(vec)==1)
#	x=length(unique(df[,..vec] )   )
#else
	x=nrow(unique(df[,..vec] )   )
#vec[length(vec)+1]=goal
vec=append(vec,goal)
m=plyr::count(df[,..vec])
z=nrow(m)
#y[[1]]=z-x
valores_m=as.numeric(names(table(m[,ncol(m)])))
#print(valores_m)
aux_trigger=as.numeric(table(plyr::count(df[,..vec])$freq)[which(names(table(plyr::count(df[,..vec])$freq))==1)])/sum(as.numeric(table(plyr::count(df[,..vec])$freq)))
if(length(aux_trigger>0))
aceitacao=as.numeric( (1-aux_trigger)<trigger  )
else
aceitacao=0
y[[2]]=aceitacao
# como z-x deve ser 0 e aceitacao retornar 0 para aceitarmos
#print(c(z,x,aceitacao))
#cat("o valor de z é:",z,"x vale",x,"aceitacao",aceitacao,"o vetor já sera",vec,"\n")
w=abs(z-x)
#print(c(w,vec))
y[[1]]=w
w=as.numeric(w>repetitions)
#return(w)
return(y)

#return( w +aceitacao) # apenas valor 0 aceita vec para esta goal
}

#'\code{generate_candidates} Asks for a dataframe and some parameters and returns all possible combinations of collums for prediction that satisfy a given error in input
#'in a list the first element of the list are the combinations while the second is its measure of error,to get the best parameters call best_vector
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#' @examples
#' #The generate_candidates function generates all sets of maximum length maxi.
#' #Maxi is a measure of error.
#' #This measure of error is related to the repetitions parameter.
#' #This parameter should range from 0 (rejects anything less to 100 percent accuracy)
#' #To number of rows of the dataframe to accept all.

#' #Lets generate a dataset
#' e=sample(1:5,1e4,replace=TRUE)
#' e1=sample(1:5,1e4,replace=TRUE)
#' e2=sample(1:5,1e4,replace=TRUE)
#' e=data.frame(e,e1,e2,paste(LETTERS[e],LETTERS[e1]),paste(LETTERS[e],LETTERS[e1],LETTERS[e2])   )
#' names(e)=c("random1","random2","random3","2randoms","3randoms")
#' #We can then generate all candidates to predict the 5 collumn
#' #We shall determine the reject part to 80 percent of the dataframe length
#' z=generate_candidates(df=e, goal=5, maxi=4, repetitions=0.8*nrow(e), trigger = 1)
#' #We can see z is a list
#' #z[[1]] is another list that contains all sets that satisfy our request
#' #z[[2]] is a measure of error, the smaller the more accurate
#' #Lets then order z[[1]] by z[[2]]
#' m=z[[1]][order(z[[2]])]
#' print(m)
#' #We can see then that m[[1]] holds the best set for prediction, while m[[length(m)]] the worst
#' #To prove it we can do the following
#' cat("The best set to predict",names(e)[5],"is ",names(e)[m[[1]]],"\n"   )
#' cat("Its expected accuracy is",MeanAccuracy(e,m[[1]],5),"\n"  )
#' cat("The worst set to predict",names(e)[5],"is ",names(e)[m[[length(m)]]],"\n"   )
#' cat("Its expected accuracy is",MeanAccuracy(e,m[[length(m)]],5),"\n"  )

#'@export
generate_candidates=function(df,goal,maxi,repetitions,trigger=1){
if(class(df)[1]=="data.table")
	generate_candidates_table(df,goal,maxi,repetitions,trigger)
else{
groups=1:ncol(df)
groups=groups[-goal]
if(length(groups)>maxi)
  loop=maxi
else
  loop=length(groups)
###versao otimiziada mas que gera todas as combinacoes
	#z= lapply(1:length(groups), function(x) combn(groups, x))
  z=lapply(1:loop, function(x) combn(groups, x))
###traducao do lapply
#if(length(groups)>maxi ){
#		loop=maxi
#}
#else
#	loop=length(groups)
#z=list()
#or(i in 1:loop){
#	z[[i]]=combn(groups, i)


#}
vetores_candidatos=list()
erros=c()
cont=1
if(length(z)>maxi)
	maximo=length(z)
else
	maximo=maxi
for(i in 1:maximo){
	print(i)
	data=data.frame(z[[i]])
	for(j in 1:ncol(data)){
		vetor=data[,j]
		#print(candidatos(df,goal,vetor))
		lista_auxiliar=candidates(df,goal,vetor,repetitions,trigger)
		if(lista_auxiliar[[1]]<=repetitions & lista_auxiliar[[2]]==0){
			vetores_candidatos[[cont]]=vetor
			erros[cont]=lista_auxiliar[[1]]
			cont=cont+1

		}

	}


}
retorno=list(vetores_candidatos,erros)
#return(vetores_candidatos)
return(retorno)
}}
#'\code{generate_candidates_table} Asks for a data.table and some parameters and returns all possible combinations of collums for prediction that satisfy a given error in input
#'in a list the first element of the list are the combinations while the second is its measure of error,to get the best parameters call best_vector
#' @param df A data.table with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#'@export
generate_candidates_table=function(df,goal,maxi,repetitions,trigger=1){
groups=1:ncol(df)
groups=groups[-goal]
if(length(groups)>maxi)
  loop=maxi
else
  loop=length(groups)
###versao otimiziada mas que gera todas as combinacoes
	#z= lapply(1:length(groups), function(x) combn(groups, x))
  z=lapply(1:loop, function(x) combn(groups, x))
###traducao do lapply
#if(length(groups)>maxi ){
#		loop=maxi
#}
#else
#	loop=length(groups)
#z=list()
#or(i in 1:loop){
#	z[[i]]=combn(groups, i)


#}
vetores_candidatos=list()
erros=c()
cont=1
if(length(z)>maxi)
	maximo=length(z)
else
	maximo=maxi
for(i in 1:maximo){
	print(i)
	data=data.table(z[[i]])
	for(j in 1:ncol(data)){
		vetor=unlist(data[,..j])
		#print(candidatos(df,goal,vetor))
		lista_auxiliar=candidates_table(df,goal,vetor,repetitions,trigger)
		if(lista_auxiliar[[1]]<=repetitions & lista_auxiliar[[2]]==0){
			vetores_candidatos[[cont]]=vetor
			erros[cont]=lista_auxiliar[[1]]
			cont=cont+1

		}

	}


}
retorno=list(vetores_candidatos,erros)
#return(vetores_candidatos)
return(retorno)
}



#'\code{best_vector} Asks for a dataframe and some parameters and returns the best combination of collums to predict the missing value
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#' @param ratio Rejects collumns that the ratio of unique values to total values is higher than this value, primary keys have ratio equal to 1
#' @examples
#' #The Best Vector Function shall do the following
#' #Take a dataframe and a goal collumn to predict
#' #Tests every combination of vectors limited by a parameter length
#' #Returns the best set to predict the goal
#' #Then to run some experiments first lets build a dataframe
#' e=sample(1:2,1e2,replace=TRUE)
#' e1=sample(1:2,1e2,replace=TRUE)
#' e2=sample(1:2,1e2,replace=TRUE)
#' e=data.frame(e,e1,e2,paste(LETTERS[e],LETTERS[e1]),paste(LETTERS[e],LETTERS[e1],LETTERS[e2])   )
#' #We can easily see that to predict the last collumn you need the first three.
#' #Lets Check what the function will find
#' z=best_vector(e,5,3,nrow(e),1)
#' print(z)
#' #Lets now check what is the best set if we use only 2 collumns maximum
#' z1=best_vector(e,5,2,nrow(e),1)
#' print(z1)
#' #We could also predict which collumn is best to predict the fourth one
#' z2=best_vector(e,4,2,nrow(e),1)
#' print(z2)
#' #We could also take a look at the dataset iris.
#' #Since this dataset does not repeat lines we must use trigger=0
#' #To predict Species
#' z3=best_vector(iris,5,2,nrow(iris),0)
#' print(names(iris))[z3]
#' #We can check the accuracy of these predictions with the accuracy functions
#' print(MeanAccuracy(iris,z3,5))
#' print(MeanAccuracy(e,z2,4))
#' print(MeanAccuracy(e,z1,5))
#' print(MeanAccuracy(iris,z,5))
#'@export
best_vector=function(df,goal,maxi,repetitions,trigger=1,ratio=0.99){
meta=names(df)[goal]
lista=as.numeric(which(sapply(sapply(df,unique),length)/nrow(df)<ratio))
df_aux=df
if(class(df)[1]=="data.table"){
	#df=df[,..lista]
	#goal=which(names(df)==meta)

	best_vector_table(df,goal,maxi,repetitions,trigger,ratio)
	}
else{
df=df[,lista]
goal=which(names(df)==meta)
z=generate_candidates(df,goal,maxi,repetitions,trigger)
if(length(z[[1]])>0){
solution=unlist(z[[1]][min(which(z[[2]]==min(z[[2]])))])

  return( which(names(df)[solution]==names(df_aux)) )


}
else{
  cat("Could not find any candidate, please use trigger=0\n")
  return(0)

}
}
}
#'\code{best_vector_table} Asks for a data.table and some parameters and returns the best combination of collums to predict the missing value
#' @param df A data.table with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#' @param ratio Rejects collumns that the ratio of unique values to total values is higher than this value, primary keys have ratio equal to 1
#'@export
best_vector_table=function(df,goal,maxi,repetitions,trigger=1,ratio=0.99){
meta=names(df)[goal]
lista=as.numeric(which(sapply(sapply(df,unique),length)/nrow(df)   <ratio))
df_aux=df
df=df[,..lista]
goal=which(names(df)==meta)
z=generate_candidates_table(df,goal,maxi,repetitions,trigger)
if(length(z[[1]])>0){
	solution=unlist(z[[1]][min(which(z[[2]]==min(z[[2]])))])
  return( which(names(df)[solution]==names(df_aux)) )

}
else{
  cat("Could not find any candidate, please use trigger=0\n")
  return(0)

}

}



#'\code{NA_VALUES} Asks for a dataframe and returns a table of how many missing values are in each collum
#' @param df A dataframe with the missing values you wish to fill
#' @examples
#' #This function is used to detect how many NA values are in a dataframe
#' # the use is pretty much always the same
#' #Lets consider the dataset iris
#' i=iris
#' print(NA_VALUES(i)  )
#' #Since it has no missing values it shows none, now lets insert some NA_VALUES there
#' i[sample(1:nrow(i),0.3*nrow(i)),1]=NA
#' i[sample(1:nrow(i),0.2*nrow(i)),2]=NA
#' i[sample(1:nrow(i),0.5*nrow(i)),3]=NA

#' print(NA_VALUES(i))
#' #For every dataframe the user just uses this

#' @export
NA_VALUES=function(df){
return(apply(apply(df,2,is.na),2,sum) )

}



#'\code{Complete_dataset} Asks for a dataframe, a vector of collumn indices and the goal collumn and  returns the data frame with the values filled
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param rows The collumns you wish to use to predict the missing values
#' @examples
#' #The Complete_dataset Function shall do the following
#' #Take a dataframe and a goal collumn to predict
#' #Takes a set of vectors to use for prediction
#' #Use this set to predict with accuracy given by MeanAccuracy function
#' #Then to run some experiments first lets build a dataframe
#' e=sample(1:5,1e4,replace=TRUE)
#' e1=sample(1:5,1e4,replace=TRUE)
#' e2=sample(1:5,1e4,replace=TRUE)
#' e=data.frame(e,e1,e2,paste(LETTERS[e],LETTERS[e1]),paste(LETTERS[e],LETTERS[e1],LETTERS[e2])   )
#' #Now we got a dataframe lets create a copy of it
#' ce=e
#' ce[sample(1:nrow(e),0.3*nrow(e)),5]=NA
#' #So 30 percent of the data is now missing
#' #Lets try to recover it then with Complete_dataset
#' #First we must choose a set of vectors to use
#' #Lets first try with best_vector
#' vector_c=best_vector(ce,5,4,nrow(ce),1)
#' ce1=Complete_dataset(ce,rows=vector_c,goal=5)
#' #We can see how many values are still missing with NA_VALUES
#' print(NA_VALUES(ce1) )
#' #And check how many we got wrong by
#' print(sum(ce1[,5]!=e[,5]) )

#' #If the user wanted he of course could choose a set of his own, for example
#' user_set=c(1,3)
#' ce1=Complete_dataset(ce,rows=user_set,goal=5)
#' #We can see how many values are still missing with NA_VALUES
#' print(NA_VALUES(ce1) )
#' #And check how many we got wrong by
#' print(sum(ce1[,5]!=e[,5]) )
#' #But we can see that is not the best solution
#' #To see how to check the best sets take a look at generate_candidates

#' # The process could be done for the 4 collum as well

#' ce=e
#' ce[sample(1:nrow(e),0.5*nrow(e)),4]=NA
#' #So 50 percent of the data is now missing
#' #Lets try to recover it then with Complete_dataset
#' vector_c=best_vector(ce,4,4,nrow(ce),1)
#' ce1=Complete_dataset(ce,rows=vector_c,goal=4)
#' #We can see how many values are still missing with NA_VALUES
#' print(NA_VALUES(ce1) )
#' #And check how many we got wrong by
#' print(sum(ce1[,4]!=e[,4]) )
#' #Here we can easily see e holds the original data
#' #ce1 is the recovered data

#'@export
Complete_dataset=function(df,rows,goal){
if(class(df)[1]=="data.table")
	Complete_dataset_table(df,rows,goal)
else{
  #lets suppose df has missing values on only goal collum
  test=list()
  df2=df
  vectors=which(is.na(df[,goal]))
  df1=df[-vectors,] # for plyr::count
  aux=c(rows,goal)
  frequency_table=plyr::count(df1[,aux])
  for(i in vectors){
    test=list()
    for(j in 1:length(rows)  )
      {
         test[[j]]=which(df[i,rows[j]]==frequency_table[,j]   )
        # print(c(df [i,rows[j]],frequency_table[,j] )  )
    }
 #   print(test)
    prob_table=frequency_table[Reduce(intersect,test),c(ncol(frequency_table)-1,ncol(frequency_table))]
    if(nrow(prob_table)>0){
    prob=cumsum(prob_table[,2])
    prob=prob/max(prob)
    #print(c(prob,prob_table))
    df2[i,goal]=prob_table[min(which(runif(1)<prob)),1]}
  }
  return(df2)
}}

#'\code{Complete_dataset_table} Asks for a data.table, a vector of collumn indices and the goal collumn and  returns the data frame with the values filled
#' @param df A data.table with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param rows The collumns you wish to use to predict the missing values
#'@export
Complete_dataset_table=function(df,rows,goal){
  #lets suppose df has missing values on only goal collum
  test=list()
  df2=df
  vectors=which(is.na(df[,..goal]))
  df1=df[-vectors,] # for plyr::count
  aux=c(rows,goal)
  frequency_table=plyr::count(df1[,..aux])
  for(i in vectors){
    test=list()
    for(j in 1:length(rows)  )
      {
	 forget_this=rows[j]
         test[[j]]=which(unlist(df[i,..forget_this])==frequency_table[,j]   )
        # print(c(df [i,rows[j]],frequency_table[,j] )  )
    }
   # print(test)
    prob_table=frequency_table[Reduce(intersect,test),c(ncol(frequency_table)-1,ncol(frequency_table))]
    if(nrow(prob_table)>0){
    prob=cumsum(prob_table[,2])
    prob=prob/max(prob)
    #print(c(prob,prob_table))
    df2[i,names(df2)[goal] := prob_table[min(which(runif(1)<prob)),1] ]}
  }
  return(df2)
}

#'\code{autoComplete} Asks for a dataframe, a vector of collumn indices and the goal collumn and  returns the data frame with the values filled
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param ratio Rejects collumns that the ratio of unique values to total values is higher than this value, primary keys have ratio equal to 1
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#' @examples
#' #The auto Complete Function shall do the following
#' #Take a dataframe and a goal collumn to predict
#' #Tests every combination of vectors limited by a parameter length
#' #Use the best set to predict with accuracy given by MeanAccuracy function
#' #Then to run some experiments first lets build a dataframe
#' e=sample(1:5,1e4,replace=TRUE)
#' e1=sample(1:5,1e4,replace=TRUE)
#' e2=sample(1:5,1e4,replace=TRUE)
#' e=data.frame(e,e1,e2,paste(LETTERS[e],LETTERS[e1]),paste(LETTERS[e],LETTERS[e1],LETTERS[e2])   )
#' #Now we got a dataframe lets create a copy of it
#' ce=e
#' ce[sample(1:nrow(e),0.3*nrow(e)),5]=NA
#' #So 30 percent of the data is now missing
#' #Lets try to recover it then with autocomplete
#' ce1=autoComplete(df=ce,goal=5,maxi=3,repetitions=nrow(ce),trigger=1)
#' #We can see how many values are still missing with NA_VALUES
#' print(NA_VALUES(ce1) )
#' #And check how many we got wrong by
#' print(sum(ce1[,5]!=e[,5]) )

#' # The process could be done for the 4 collum as well

#' ce=e
#' ce[sample(1:nrow(e),0.5*nrow(e)),4]=NA
#' #So 50 percent of the data is now missing
#' #Lets try to recover it then with autocomplete
#' ce1=autoComplete(df=ce,goal=4,maxi=4,repetitions=nrow(ce),trigger=1)
#' #We can see how many values are still missing with NA_VALUES
#' print(NA_VALUES(ce1) )
#' #And check how many we got wrong by
#' print(sum(ce1[,4]!=e[,4]) )
#' #Here we can easily see e holds the original data
#' #ce1 is the recovered data

#'@export
autoComplete=function(df,goal,maxi,repetitions,trigger=1,ratio=0.99){
if(class(df)[1]=="data.table")
	autoComplete_table(df,goal,maxi,repetitions,trigger,ratio=0.99)
else{
  z1=best_vector(df,goal = goal,maxi = maxi,repetitions = repetitions,trigger = trigger,ratio=ratio)
  df2=Complete_dataset(df = df,rows = z1,goal = goal)
  return(df2)
}}


#'\code{autoComplete_table} Asks for a data.table, a vector of collumn indices and the goal collumn and  returns the data frame with the values filled
#' @param df A data.table with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param ratio Rejects collumns that the ratio of unique values to total values is higher than this value, primary keys have ratio equal to 1
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#'@export
autoComplete_table=function(df,goal,maxi,repetitions,trigger=1,ratio=0.99){
  z1=best_vector_table(df,goal = goal,maxi = maxi,repetitions = repetitions,trigger = trigger,ratio = ratio)
  df2=Complete_dataset_table(df = df,rows = z1,goal = goal)
  return(df2)
}

#'\code{MeanAccuracy} Asks for a dataframe, a vector of collumn indices and the goal collumn the expected value of accuracy of filling missing values if the dataset is representative
#' @param df A dataframe that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#' @examples
#' #The Mean accuracy function tells its user the expected accuracy.
#' #Code with two ## is working code but takes longer than 5 seconds
#' #Given a set and a goal to predict it supposes the following.
#' #All missing values are representative of the dataset.
#' #Lets first Consider the iris dataset
#' #It has the following parameters
#' print(names(iris))
#' #As we can see the 5 collumn is species
#' #Lets use Sepal.Length to predict Species and see Mean accuracy
#' print(MeanAccuracy(iris,1,5))
#' #Now lets use both sepal parameters
#' ##print(MeanAccuracy(iris,1:2,5))
#' #And when using a Petal parameter as well
#' print(MeanAccuracy(iris,1:3,5))
#' #We can see that iris even in the Mean case scenario species can be defined by these 3
#' #Now lets take a look at the mtcars dataset
#' ##print(names(mtcars))
#' #Predicting gear using mpg
#' ##print(MeanAccuracy(mtcars,1,10))
#' #But if we try to predict mpg using gear
#' ##print(MeanAccuracy(mtcars,10,1))
#' #So using the Mean accuracy function we can know whats the mean case accuracy
#' #If the user requires he can also predict more than 1 goal for example
#' ##print(MeanAccuracy(mtcars,c(1,3,5),c(10,11)))
#' #In this case we want to use mpg,disp,drat to predict a pair gear,carb
#' #To check the confidence of predicted values the user should use all three accuracy functions

#'@export
MeanAccuracy=function(df,VECTORS,goal){
if(class(df)[1]=="data.table")
	MeanAccuracy_table(df,VECTORS,goal)
else{
 lixo=plyr::count(df[,c(VECTORS,goal)  ])
  lixo1=unique(df[,VECTORS])
  lista=list()
  if(length(VECTORS)>1)
    tamanho=nrow(lixo1)
  else
    tamanho=length(lixo1)
   for(j in 1:tamanho){
     vec=c()
     for(i in 1:nrow(lixo)){
    if(length(VECTORS)>1   )
     vec[i]=sum(lixo[i,1:ncol(lixo1)]==lixo1[j,1:ncol(lixo1)])==ncol(lixo1)
    else
      vec[i]=lixo[i,1]==lixo1[j]
     }
   lista[[j]]=vec
   }
  lista=lapply(lista,which)
  maximo=sum(lixo$freq)
  prob=0
  #print()
for(i in 1:length(lista)){
  if(length(lista[[i]])==1 )
    prob=prob+lixo$freq[lista[[i]]]/maximo # probabilidade de escolhelo 100%
  else{
    prob_auxiliar=lixo$freq[unlist(lista[[i]])]
    acumulado=sum(prob_auxiliar)
    Pe=prob_auxiliar/acumulado #probabilidade de ser escolhido
    Pa=prob_auxiliar/maximo # probabilidade global
    prob=prob+sum(Pe*Pa)
  }
}
  return(prob)
} }


#'\code{MeanAccuracy_table} Asks for a data.table, a vector of collumn indices and the goal collumn the expected value of accuracy of filling missing values if the dataset is representative
#' @param df A data.table that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#'@export
MeanAccuracy_table=function(df,VECTORS,goal){
 auxiliar_table=c(VECTORS,goal)
 lixo=plyr::count(df[,..auxiliar_table ])
  lixo1=unique(df[,..VECTORS])
  lista=list()
  #if(length(VECTORS)>1)
    tamanho=nrow(lixo1)

 # else
#print(tamanho)
  #  tamanho=length(lixo1)
   for(j in 1:tamanho){
     vec=c()
     for(i in 1:nrow(lixo)){
   # if(length(VECTORS)>1   )
     vec[i]=sum(lixo[i,1:ncol(lixo1)]==lixo1[j,1:ncol(lixo1)])==ncol(lixo1)
  #   print(vec)
   # else
    #  vec[i]=lixo[i,1]==lixo1[j]
     }
   lista[[j]]=vec
   }
   # print(lista)
  lista=lapply(lista,which)
  maximo=sum(lixo$freq)
  prob=0
  #print()
  #print(lista)
for(i in 1:length(lista)){
  if(length(lista[[i]])==1 )
    prob=prob+lixo$freq[lista[[i]]]/maximo # probabilidade de escolhelo 100%
  else{
    prob_auxiliar=lixo$freq[unlist(lista[[i]])]
    acumulado=sum(prob_auxiliar)
    Pe=prob_auxiliar/acumulado #probabilidade de ser escolhido
    Pa=prob_auxiliar/maximo # probabilidade global
    prob=prob+sum(Pe*Pa)
  }
}
  return(prob)
}

#'\code{BestAccuracy} Asks for a dataframe, a vector of collumn indices and the goal collumn and returns the maximum possible value of accuracy of filling missing values
#' @param df A dataframe that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#' @examples
#' #The Best accuracy function tells its user the best accuracy possible.
#' #Code with two ## is working code but takes longer than 5 seconds
#' #Given a set and a goal to predict it supposes the following.
#' #All missing values are contained in the possible values with lowest uncertainty.
#' #Lets first Consider the iris dataset
#' #It has the following parameters
#' print(names(iris))
#' #As we can see the 5 collumn is species
#' #Lets use Sepal.Length to predict Species and see Best accuracy
#' print(BestAccuracy(iris,1,5))
#' #Now lets use both sepal parameters
#' print(BestAccuracy(iris,1:2,5))
#' #And when using a Petal parameter as well
#' ##print(BestAccuracy(iris,1:3,5))
#' #We can see that iris even in the best case scenario species can be defined by these 3
#' #Now lets take a look at the mtcars dataset
#' ##print(names(mtcars))
#' #Predicting gear using mpg
#' ##print(BestAccuracy(mtcars,1,10))
#' #But if we try to predict mpg using gear
#' ##print(BestAccuracy(mtcars,10,1))
#' #So using the best accuracy function we can know whats the best case accuracy
#' #If the user requires he can also predict more than 1 goal for example
#' ##print(BestAccuracy(mtcars,c(1,3,5),c(10,11)))
#' #In this case we want to use mpg,disp,drat to predict a pair gear,carb
#' #To check the confidence of predicted values the user should use all three accuracy functions

#'@export
BestAccuracy=function(df,VECTORS,goal){
if(class(df)[1]=="data.table")
	BestAccuracy_table(df,VECTORS,goal)
else{
  auxiliar_vector=c()
  lixo=plyr::count(df[,c(VECTORS,goal)  ])
  lixo1=unique(df[,VECTORS])
  lista=list()
  if(length(VECTORS)>1)
    tamanho=nrow(lixo1)
  else
    tamanho=length(lixo1)
  for(j in 1:tamanho){
    vec=c()
    for(i in 1:nrow(lixo)){
      if(length(VECTORS)>1   )
        vec[i]=sum(lixo[i,1:ncol(lixo1)]==lixo1[j,1:ncol(lixo1)])==ncol(lixo1)
      else
        vec[i]=lixo[i,1]==lixo1[j]
    }
    lista[[j]]=vec
  }
  lista=lapply(lista,which)
  maximo=sum(lixo$freq)
  prob=0
  #print()
  for(i in 1:length(lista)){
    if(length(lista[[i]])==1 )
      #prob=prob+lixo$freq[lista[[i]]]/maximo # probabilidade de escolhelo 100%
      auxiliar_vector[i]=1
    else{
      prob_auxiliar=lixo$freq[unlist(lista[[i]])]
      acumulado=sum(prob_auxiliar)
      Pe=prob_auxiliar/acumulado #probabilidade de ser escolhido
      Pa=prob_auxiliar/maximo # probabilidade global
      prob=prob+sum(Pe*Pa)
      auxiliar_vector[i]=sum(Pe*Pe)
    }
  }
  return(max(auxiliar_vector))
}}

#'\code{BestAccuracy_table} Asks for a data.table, a vector of collumn indices and the goal collumn and returns the maximum possible value of accuracy of filling missing values
#' @param df A data.table that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#'@export
BestAccuracy_table=function(df,VECTORS,goal){
  auxiliar_vector=c()
  auxiliar_table=c(VECTORS,goal)
 lixo=plyr::count(df[,..auxiliar_table ])
  lixo1=unique(df[,..VECTORS])
  lista=list()
  #if(length(VECTORS)>1)
    tamanho=nrow(lixo1)

 # else
#print(tamanho)
  #  tamanho=length(lixo1)
   for(j in 1:tamanho){
     vec=c()
     for(i in 1:nrow(lixo)){
   # if(length(VECTORS)>1   )
     vec[i]=sum(lixo[i,1:ncol(lixo1)]==lixo1[j,1:ncol(lixo1)])==ncol(lixo1)
  #   print(vec)
   # else
    #  vec[i]=lixo[i,1]==lixo1[j]
     }
   lista[[j]]=vec
   }
   # print(lista)
  lista=lapply(lista,which)
  maximo=sum(lixo$freq)
  prob=0
  #print()
  #print(lista)
  for(i in 1:length(lista)){
    if(length(lista[[i]])==1 )
      #prob=prob+lixo$freq[lista[[i]]]/maximo # probabilidade de escolhelo 100%
      auxiliar_vector[i]=1
    else{
      prob_auxiliar=lixo$freq[unlist(lista[[i]])]
      acumulado=sum(prob_auxiliar)
      Pe=prob_auxiliar/acumulado #probabilidade de ser escolhido
      Pa=prob_auxiliar/maximo # probabilidade global
      prob=prob+sum(Pe*Pa)
      auxiliar_vector[i]=sum(Pe*Pe)
    }
  }
  return(max(auxiliar_vector))
}

#'\code{WorstAccuracy} Asks for a dataframe, a vector of collumn indices and the goal collumn and returns the minimum possible value of accuracy of filling missing values
#' @param df A dataframe that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#' @examples
#' #The Worst accuracy function tells its user the worst case accuracy possiblle.
#' #Code with two ## is working code but takes longer than 5 seconds
#' #Given a set and a goal to predict it supposes the following.
#' #All missing values are contained in the possible values with highest uncertainty.
#' #Lets first Consider the iris dataset
#' #It has the following parameters
#' print(names(iris))
#' #As we can see the 5 collumn is species
#' #Lets use Sepal.Length to predict Species and see Worst accuracy
#' print(WorstAccuracy(iris,1,5))
#' #Now lets use both sepal parameters
#' print(WorstAccuracy(iris,1:2,5))
#' #And when using a Petal parameter as well
#' ##print(WorstAccuracy(iris,1:3,5))
#' #We can see that iris even in the worst case scenario species can be defined by these 3
#' #Now lets take a look at the mtcars dataset
#' ##print(names(mtcars))
#' #Predicting gear using mpg
#' ##print(WorstAccuracy(mtcars,1,10))
#' #But if we try to predict mpg using gear
#' ##print(WorstAccuracy(mtcars,10,1))
#' #So using the Worst accuracy function we can know whats the worst case accuracy
#' #If the user requires he can also predict more than 1 goal for example
#' ##print(WorstAccuracy(mtcars,c(1,3,5),c(10,11)))
#' #In this case we want to use mpg,disp,drat to predict a pair gear,carb
#' #To check the confidence of predicted values the user should use all three accuracy functions

#'@export
WorstAccuracy=function(df,VECTORS,goal){
  if(class(df)[1]=="data.table")
	WorstAccuracy_table(df,VECTORS,goal)
  else{
  auxiliar_vector=c()
  lixo=plyr::count(df[,c(VECTORS,goal)  ])
  lixo1=unique(df[,VECTORS])
  lista=list()
  if(length(VECTORS)>1)
    tamanho=nrow(lixo1)
  else
    tamanho=length(lixo1)
  for(j in 1:tamanho){
    vec=c()
    for(i in 1:nrow(lixo)){
      if(length(VECTORS)>1   )
        vec[i]=sum(lixo[i,1:ncol(lixo1)]==lixo1[j,1:ncol(lixo1)])==ncol(lixo1)
      else
        vec[i]=lixo[i,1]==lixo1[j]
    }
    lista[[j]]=vec
  }
  lista=lapply(lista,which)
  maximo=sum(lixo$freq)
  prob=0
  #print()
  for(i in 1:length(lista)){
    if(length(lista[[i]])==1 )
      #prob=prob+lixo$freq[lista[[i]]]/maximo # probabilidade de escolhelo 100%
      auxiliar_vector[i]=1
    else{
      prob_auxiliar=lixo$freq[unlist(lista[[i]])]
      acumulado=sum(prob_auxiliar)
      Pe=prob_auxiliar/acumulado #probabilidade de ser escolhido
      Pa=prob_auxiliar/maximo # probabilidade global
      prob=prob+sum(Pe*Pa)
      auxiliar_vector[i]=sum(Pe*Pe)
    }
  }
  return(min(auxiliar_vector))
  } }



#'\code{WorstAccuracy_table} Asks for a data.table, a vector of collumn indices and the goal collumn and returns the minimum possible value of accuracy of filling missing values
#' @param df A data.table that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#'@export
WorstAccuracy_table=function(df,VECTORS,goal){
  auxiliar_vector=c()
    auxiliar_table=c(VECTORS,goal)
 lixo=plyr::count(df[,..auxiliar_table ])
  lixo1=unique(df[,..VECTORS])
  lista=list()
  #if(length(VECTORS)>1)
    tamanho=nrow(lixo1)

 # else
#print(tamanho)
  #  tamanho=length(lixo1)
   for(j in 1:tamanho){
     vec=c()
     for(i in 1:nrow(lixo)){
   # if(length(VECTORS)>1   )
     vec[i]=sum(lixo[i,1:ncol(lixo1)]==lixo1[j,1:ncol(lixo1)])==ncol(lixo1)
  #   print(vec)
   # else
    #  vec[i]=lixo[i,1]==lixo1[j]
     }
   lista[[j]]=vec
   }
   # print(lista)
  lista=lapply(lista,which)
  maximo=sum(lixo$freq)
  prob=0
  #print()
  #print(lista)
  for(i in 1:length(lista)){
    if(length(lista[[i]])==1 )
      #prob=prob+lixo$freq[lista[[i]]]/maximo # probabilidade de escolhelo 100%
      auxiliar_vector[i]=1
    else{
      prob_auxiliar=lixo$freq[unlist(lista[[i]])]
      acumulado=sum(prob_auxiliar)
      Pe=prob_auxiliar/acumulado #probabilidade de ser escolhido
      Pa=prob_auxiliar/maximo # probabilidade global
      prob=prob+sum(Pe*Pa)
      auxiliar_vector[i]=sum(Pe*Pe)
    }
  }
  return(min(auxiliar_vector))
}
