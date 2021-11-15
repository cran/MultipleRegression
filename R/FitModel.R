#' Funcao para ajusta modelos de regressoes multiplas
#'
#' @description
#' Esta funcao ajusta modelos de regressao multipla e estima estatisticas importantes.
#'
#'
#' @usage FitModel(X,Y,Rep=NULL,Model,Design=1)
#'
#' @param X   Matriz contendo em cada coluna as variaveis explicativas, ou um
#' vetor contendo os nomes das variaveis explicativas.
#' @param Y Vetor contendo a variavel resposta.
#' @param Rep Vetor contendo a identificacao das repeticoes/blocos se o experimento
#' tiver delineamento estatistico.
#' @param Model Modelo de regressal a ser ajustado (do tipo formula).
#' @param Design Indica o delineamento do experimento, pode ser:
#'   \itemize{
#' \item design 1 -> Experimento  sem repeticao.
#' \item design 2 -> Experimento no delineamento inteiramente casualizado (DIC).
#' \item design 3 -> Experimento no delineamento em blocos casualizados (DBC).
#'   }
#' @return A funcao retorna a anova, significancia de coeficientes e avaliadores
#' da qualidade do ajuste do modelo regressao.
#' @seealso \code{\link{lm}}, \code{\link{CreateModel}}, \code{\link{FitModel}}
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
#'
#' @export

FitModel=function(X,Y,Rep=NULL,Model,Design=1){
  model=Model
  if(Design!=1){stop("O pacote ainda nao analisa este delineamento")}
  if(Design==1){
    m=lm(model,data=data.frame(X,Y=Y))

    Anova2=anova(m)
    Resumo=summary(m)

    Anova1=rbind(Regression=colSums(Anova2[-nrow(Anova2),]),
                 Residuals=Anova2[nrow(Anova2),])

    sig= 1- pf(q =Anova1[1,3]/Anova1[2,3],df1 = Anova1[1,1],df2=Anova1[2,1])
    Anova1[1,5]=sig

    Result=list(ANOVAglobal=Anova1,ANOVA=Anova2,Summary=Resumo,model=m,AIC=AIC(m),BIC=BIC(m))

  }

  return(Result)

}
