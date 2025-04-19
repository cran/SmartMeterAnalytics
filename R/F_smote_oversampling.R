#' Synthetic minority oversampling (SMOTE)
#'
#' Performs oversampling by creating new instances.
#'
#' SMOTE is used to generate synthetic datapoints of a smaller class, for example
#' to overcome the problem of imbalanced classes in classification.
#'
#' @author Ilya Kozlovskiy, Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#'
#' @param Variables
#'    the \link{data.frame} of independent variables that should be used to create new instances
#' @param Classes
#'    the class labels in the prediction problem
#' @param subset_use
#'    a specific subset only is used for the oversampling. If \link{NULL}, everything is used.
#' @param k
#'    the number of neigbours for generation
#' @param use_nearest
#'    should only the nearest neighbours be used? (very slow)
#' @param proportions
#'    to which proportion (of the biggest class) should the classes be equalized
#' @param equalise_with_undersampling
#'    should additional undersampling be performed?
#' @param safe
#'    should a safe version of SMOTE be used?
#' @importFrom FNN knn.index knn
#' @importFrom stats runif
#'
#' @return a list containing new independent variables \link{data.frame} and new class labels
#' @export

smote=function(Variables,
               Classes,
               subset_use=NULL,
               k=5,
               use_nearest=TRUE,
               proportions=0.9,
               equalise_with_undersampling=FALSE,
               safe=FALSE
){

  #check inputs
  if(!(is.data.frame(Variables)|is.matrix(Variables))){
    stop("Variables must be of type data.frame or matrix")
  }

  #use the whole set for oversampling
  if(is.null(subset_use)) subset_use=seq_along(Classes)

  #check if there are are least one numeric feature vector
  numeric_Variables = sapply(Variables,is.numeric)
  if(sum(numeric_Variables) == 0) {
    warning("No SMOTE done since there are no numeric Variables present")
    result=list(Variables=Variables,Classes=Classes)
  }
  other_Variables = !numeric_Variables

  A_Klassen=length(levels(Classes)) #number of classes
  H_Klassen=rep(0,A_Klassen)         #frequencies of each class
  for(i in 1:A_Klassen){
    H_Klassen[i]=sum(Classes==levels(Classes)[i])
  }

  #iterate over all classes in descending order (from largest to smallest class)
  kli=order(H_Klassen)
  for(l in 1:(A_Klassen-1)){

    to_create=proportions*H_Klassen[kli[A_Klassen]]-H_Klassen[kli[l]]
    New_Data=NULL
    created=0

    if(to_create>k){
      New_Data=NULL
      N_iterations=to_create %/% k
      to_create=N_iterations * k
      ind_of_class=which(Classes[subset_use]==levels(Classes)[kli[l]])
      #compute indeces to be used
      if(use_nearest){
        ind_of_class_neighbors = FNN::knn.index( data=Variables[ind_of_class,numeric_Variables,drop=FALSE], k= k, algorithm = "k")
        ind_of_class_neighbors = cbind(ind_of_class, ind_of_class_neighbors)
        #ind_to_use=knnx.index(data=Variables[ind_of_class,],query=Variables[sample(ind_of_class,N_iterations,replace=TRUE),],k=k+1,algorithm="k")
        ind_to_use = t(replicate( N_iterations,  ind_of_class_neighbors[ sample(nrow(ind_of_class_neighbors),1) , ]) )
      } else {
        if(length(ind_of_class)>=k+1){
          ind_to_use=t(replicate(N_iterations,sample(ind_of_class,k+1)))
        } else{
          ind_to_use=t(replicate(N_iterations,sample(ind_of_class,k+1,replace=TRUE)))
          warning("Low number of minority samples for smote")
        }
      }

      created=0
      for(i in 1:N_iterations){
        di=ind_to_use[i,]
        oi=di[1]
        di=di[-1]
        for(j in 1:k){
          t=runif(k)
          st = sum(t)
          New_vec_numeric= (1-t)*Variables[oi,numeric_Variables,drop=FALSE]+t %*% as.matrix(Variables[di,numeric_Variables,drop=FALSE])
          New_vec_others = sapply(Variables[c(oi,di),!numeric_Variables,drop=FALSE], function(x) sample(x,1))
          New_vec = Variables[oi,,drop=FALSE]
          New_vec[,numeric_Variables] = New_vec_numeric
          New_vec[,!numeric_Variables] = New_vec_others

          if(safe){
            klass=FNN::knn(train=Variables[],test=New_vec,cl=Classes,k=5,algorithm="k")

            if(klass==levels(Classes)[kli[l]]){
              New_Data=rbind(New_Data,New_vec)
            }
          } else {
            New_Data=rbind(New_Data,New_vec)
          }
        }
      }

      created=nrow(New_Data)

      if(equalise_with_undersampling && A_Klassen == 2){
        to_remove=H_Klassen[kli[A_Klassen]]-H_Klassen[kli[l]]-created
        rows_to_remove=sample(which(Classes==levels(Classes)[kli[A_Klassen]]),to_remove)
      } else {
        rows_to_remove=length(Classes)+1
      }

      Variables=rbind(Variables[-rows_to_remove,,drop=FALSE],New_Data)
      Classes=factor(c(as.character(Classes[-rows_to_remove]),rep(levels(Classes)[kli[l]],created)))
    }
  }
  result=list(Variables=Variables,Classes=Classes)
  return(result)
}
