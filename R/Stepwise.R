#' Funcao excluir coeficientes de regressao pela metodologia Stepwise
#'
#' @description
#' Esta funcao exclui coeficientes de regressao pela metodologia Stepwise.
#'
#'
#' @usage Stepwise(object, Save=NULL,scope=NULL, scale = 0,
#' direction = c("both", "backward", "forward"),
#' trace = 1, keep = NULL, steps = 1000, k = 2)
#'
#' @param object an object representing a model of an appropriate class (mainly "lm" and "glm"). This is used as the initial model in the Stepwise search.
#' @param Save Nome do aquivo a ser salvo o relatorio da metodologia Stepwise (Ex: "Resultado.txt").
#' Se for `NULL` nao sera salvo o relatorio de exclusao dos coeficientes.
#' @param scope defines the range of models examined in the Stepwise search. This should be either a single formula, or a list containing components upper and lower, both formulae. See the details for how to specify the formulae and how they are used.
#' @param scale used in the definition of the AIC statistic for selecting the models, currently only for lm, aov and glm models. The default value, 0, indicates the scale should be estimated: see extractAIC.
#' @param direction the mode of Stepwise search, can be one of "both", "backward", or "forward", with a default of "both". If the scope argument is missing the default for direction is "backward". Values can be abbreviated.
#' @param trace if positive, information is printed during the running of step. Larger values may give more detailed information.
#' @param keep a filter function whose input is a fitted model object and the associated AIC statistic, and whose output is arbitrary. Typically keep will select a subset of the components of the object and return them. The default is not to keep anything.
#' @param steps the maximum number of steps to be considered. The default is 1000 (essentially as many as required). It is typically used to stop the process early.
#' @param k the multiple of the number of degrees of freedom used for the penalty. Only k = 2 gives the genuine AIC: k = log(n) is sometimes referred to as BIC or SBC.
#'
#' @return Retorna resultados do modelo de regressao obtido pela metodologia Stepwise.
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
#' Stepwise(Ajust)
#'
#' @export

Stepwise=function(object,Save=NULL, scope=NULL, scale = 0,
                  direction = c("both", "backward", "forward"),
                  trace = 1, keep = NULL, steps = 1000, k = 2){

  if(is.null(Save)){Nome="om.txt"}
  if(is.null(Save)==FALSE){Nome=Save}
sink(Nome)
  m=(step(object$model, scope=NULL, scale = 0,
         direction = c("both", "backward", "forward"),
         trace = 1, keep = NULL, steps = 1000, k = 2))
sink()

if(is.null(Save)){file.remove(Nome)}

  Anova2=anova(m)
  Resumo=summary(m)

  Anova1=rbind(Regression=colSums(Anova2[-nrow(Anova2),]),
               Residuals=Anova2[nrow(Anova2),])

  sig= 1- pf(q =Anova1[1,3]/Anova1[2,3],df1 = Anova1[1,1],df2=Anova1[2,1])
  Anova1[1,5]=sig

  Result=list(ANOVAglobal=Anova1,ANOVA=Anova2,Summary=Resumo,model=m)
  return(Result)
}
