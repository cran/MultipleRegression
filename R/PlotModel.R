#' Funcao obtencao de graficos associados ao ajuste do modelo
#'
#' @description
#' Esta funcao plota graficos importantes associados ao modelo ajustado.
#'
#'
#' @usage PlotModel(object,plot=3)
#'
#' @param object   Objeto criado pela funcao `FitModel`
#' @param plot valor numerico indicando o grafico desejado, pode ser:
#'   \itemize{
#' \item plot=1 -> Residuals vs Fitted.
#' \item plot=2 -> QQ-plot dos residuos.
#' \item plot=3 -> Histograma dos residuos.
#' \item plot=4 -> Grafico com o ajuste dos valores observados com os preditos.
#'   }
#' @return A funcao retorna graficos sobre o ajuste do modelo regressao.
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
#' PlotModel(Ajust,plot=1)
#' PlotModel(Ajust,plot=2)
#' PlotModel(Ajust,plot=3)
#' PlotModel(Ajust,plot=4)
#'
#' @export

PlotModel=function(object,plot=3){
  if(plot==1){
    plot(object$model,1)
  }

  if(plot==2){
    plot(object$model,2)
  }
  if(plot==3){
    hist(residuals(object$model),main="Residuals")
  }

  if(plot==4){
  Real=object$model$fitted.values+object$model$residuals
  Predict=object$model$fitted.values
  plot(Real~Predict)
  lines(sort(Predict),predict(lm(Real~Predict),newdata=data.frame(Predict=sort(Predict))),col="red")
  }
}
