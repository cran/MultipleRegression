#' Funcao para obter os valores preditos pelo modelo de regressao
#'
#' @description
#' Esta funcao prediz os valores do modelo ajustado.
#'
#'
#' @usage PredictModel(object,newdata=NULL)
#'
#' @param object   Objeto criado pela funcao `FitModel`
#' @param newdata Objeto do tipo `data.frame` com os valores das variaveis explicativas
#' a serem preditas.
#'
#' @return Retorna os valores preditos pelo modelo de regressao.
#' @seealso \code{\link{FitModel}}, \code{\link{CreateModel}}, \code{\link{PredictModel}}
#' @references Tutoriais onlines:
#' https://www.youtube.com/channel/UCDGyvLCJnv9RtTY1YMBMVNQ
#' @examples
#' data("DadosPalma")
#' X=DadosPalma[,-7]
#' Y=DadosPalma[,7]
#' model=CreateModel(X,Intercept = TRUE, Simple =TRUE, InteractionLL = TRUE)
#' model
#' Ajust=FitModel(X,Y,Model=model)
#' Ajust
#' PredictModel(Ajust)
#'
#' @export



PredictModel=function(object,newdata=NULL){
  predict(object$model,newdata)
}
