#' Funcao para criar modelos de regressoes multiplas
#'
#' @description
#' Esta funcao cria modelos de regressao multipla indicando se deseja-se considerar
#' efeitos, lineares, quadraticos e de interacoes.
#'
#'
#' @usage CreateModel( X, Intercept=TRUE, Simple=FALSE, Quadratic=FALSE,
#'   InteractionLL=FALSE, InteractionQL=FALSE, InteractionLQ=FALSE,
#'   InteractionQQ=FALSE)
#'
#' @param X   Matriz contendo em cada coluna as variaveis explicativas, ou um
#' vetor contendo os nomes das variaveis explicativas.
#' @param Intercept Indica se deseja-se considerar o intercepto no modelo. Pode ser TRUE (default) ou FALSE.
#' @param Simple Indica se deseja-se considerar efeitos simples (lineares) no modelo. Pode ser TRUE (default) ou FALSE.
#' @param Quadratic Indica se deseja-se considerar efeitos quadraticos no modelo.  Pode ser TRUE ou FALSE(default).
#' @param InteractionLL Indica se deseja-se considerar efeitos de interacao entre efeitos lineares no modelo.  Pode ser TRUE ou FALSE(default).
#' @param InteractionLQ Indica se deseja-se considerar efeitos de interacao entre efeitos lineares com os quadraticos no modelo.  Pode ser TRUE ou FALSE(default).
#' @param InteractionQL Indica se deseja-se considerar efeitos de interacao entre efeitos quadraticos com os lineares no modelo.  Pode ser TRUE ou FALSE(default).
#' @param InteractionQQ Indica se deseja-se considerar efeitos de interacao entre efeitos quadraticos  no modelo.  Pode ser TRUE ou FALSE(default).
#' @return A funcao retorna o modelo de regressao que sera ajustado
#' @seealso \code{\link{lm}}, \code{\link{PredictModel}}, \code{\link{FitModel}}
#' @references Tutoriais onlines:
#' https://www.youtube.com/channel/UCDGyvLCJnv9RtTY1YMBMVNQ
#' @examples
#' data("DadosPalma")
#' X=DadosPalma[,-7]
#' model=CreateModel(X,Intercept = TRUE, Simple =TRUE, InteractionLL = TRUE)
#' model
#'
#' @importFrom graphics hist lines
#' @importFrom stats anova as.formula lm pf predict residuals step AIC BIC
#' @export

CreateModel=function(X,
                     Intercept=TRUE,
                     Simple=FALSE,
                     Quadratic=FALSE,
                     InteractionLL=FALSE,
                     InteractionQL=FALSE,
                     InteractionLQ=FALSE,
                     InteractionQQ=FALSE){
Names=X
  if(is.matrix(X)|is.data.frame(X)){Names=colnames(X)}
simples=Names
quadratico=paste0("I(",Names,"^2)")
interacaoSS=NULL
for( i in 1:(length(Names)-1)){
  for(j in (i+1):length(Names)){
    interacaoSS=c(interacaoSS,paste0(simples[i],":",simples[j]))
  }
}

interacaoQS=NULL
for( i in 1:(length(Names)-1)){
  for(j in (i+1):length(Names)){
    interacaoQS=c(interacaoQS,paste0(quadratico[i],":",simples[j]))
  }
}

interacaoSQ=NULL
for( i in 1:(length(Names)-1)){
  for(j in (i+1):length(Names)){
    interacaoSQ=c(interacaoSQ,paste0(simples[i],":",quadratico[j]))
  }
}

interacaoQQ=NULL
for( i in 1:(length(Names)-1)){
  for(j in (i+1):length(Names)){
    interacaoQQ=c(interacaoQQ,paste0(quadratico[i],":",quadratico[j]))
  }
}


if(Intercept==TRUE){res="Y~1"}
if(Intercept==FALSE){res="Y~-1"}

if(Simple==TRUE){
for(a in 1:length(simples)){
 res= paste(res,simples[a],sep="+")
}
}

if(Quadratic==TRUE){
  for(a in 1:length(quadratico)){
    res= paste(res,quadratico[a],sep="+")
  }
}

if(InteractionLL==TRUE){
  for(a in 1:length(interacaoSS)){
    res= paste(res,interacaoSS[a],sep="+")
  }
}

if(InteractionQL==TRUE){
  for(a in 1:length(interacaoQS)){
    res= paste(res,interacaoQS[a],sep="+")
  }
}
if(InteractionLQ==TRUE){
  for(a in 1:length(interacaoSQ)){
    res= paste(res,interacaoSQ[a],sep="+")
  }
}


if(InteractionQQ==TRUE){
  for(a in 1:length(interacaoQQ)){
    res= paste(res,interacaoQQ[a],sep="+")
  }
}


as.formula(res)
}


