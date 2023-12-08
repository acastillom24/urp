envel.pois <- function(modelo=fit.model,iden=0,nome=seq(along = model.matrix(modelo)[,1]),sim=100,conf=.90,res="D",offvar=NULL,quad=T,maxit=20) {
  
  #
  # Descrição e detalhes:
  # A saída será o gráfico de probabilidade normal com envelopes simulados para um ajuste da distribuição de
  # Poisson ou "quase-Poisson".
  #
  # A opção res="C" faz o gráfico de probabilidade meio-normal com envelopes simulados utilizando a distância de Cook,
  # possibilitando a detecção de pontos simultaneamente aberrantes e/ou influentes.
  #
  # Vale ressaltar que na "quase-Poisson" o método quase-verossimilhança assume apenas a função da variância em relação
  # à média, e portanto o ajuste corresponderia a qualquer distribuição que tiver tal função de variância e não apenas a
  # Poisson. Na geração de envelopes estamos assumindo que a distribuição é a Poisson, mas os resíduos do ajuste são
  # corrigidos pela super ou subdispersão. Se ajustar pelo R é necessário usar a familia quasipoisson ao invés da
  # quasi(variance="mu") pois caso contrário a função não consegue perceber que o ajuste é da distribuição
  # "quase-Poisson" e parará a execução.
  #
  # Os dados devem estar disponíveis pelo comando attach( ).
  #
  # Argumentos obrigatórios:
  # modelo: deve-se informar o objeto onde está o ajuste do modelo, caso não seja informado, a função procurará
  # 	  o ajuste no objeto fit.model;
  # 
  # Argumentos opcionais:
  # iden: caso deseje, informe o número de observações que irá querer destacar. O padrão é não destacar ninguém (iden=0).
  #	Qualquer valor que não seja um inteiro positivo (por ex., negativo ou decimal) fará com que a função pergunte
  #	o número de pontos após a execução;
  # nome: esse argumento só é utilizado caso seja destacado algum ponto no gráfico. Caso não seja informado nada, os pontos
  #	identificados serão os números da ordem em que estão no banco de dados (os índices). Caso se queira, pode-se
  #	informar um vetor de nomes ou de identificações alternativas. Obrigatoriamente esse vetor deve ter o mesmo
  #	comprimento do banco de dados;
  # sim: número de simulações para gerar a banda de confiança. Atkinson sugere um mínimo de 20 simulações.
  #      O padrão é de 100;
  # conf: nível de confiança do envelope. O padrão é de 90%;
  # res: permite-se a escolha dos resíduos. As opções dos resíduos são: "Q" quantil (ver Dunn e Smyth, 1996), "D" componente
  #      do desvio, "P" Pearson padronizado, "A" Anscombe, "W" Williams e "C" distância de Cook. A opção padrão é a "D";
  # offvar: o S-Plus não armazena no objeto do ajuste o offset, portanto se estiver utilizando offset precisa informá-lo
  #	  aqui. Quando estiver no R, a função ignora essa opção e pega o offset diretamente do ajuste;
  # quad: o padrão (quad=T, True) faz um gráfico quadrado, enquanto quad=F (False) faz um gráfico utilizando a área máxima
  #       disponível;
  # maxit: essa opção é utilizada nos ajustes de cada simulação e indica o máximo de iterações permitidas nos ajustes.
  #	 O padrão é maxit=20.
  #
  # Autor: Frederico Zanqueta Poleto <fred@poleto.com>, arquivo disponível em http://www.poleto.com
  #
  # Referências:
  # DUNN, K. P., and SMYTH, G. K. (1996). Randomized quantile residuals. J. Comput. Graph. Statist. 5, 1-10
  #    [http://www.statsci.org/smyth/pubs/residual.html e http://www.statsci.org/smyth/pubs/residual.ps]
  # MCCULLAGH, P. e NELDER, J. A. (1989). Generalized Linear Models. 2ª ed. Chapman and Hall, London.
  # PAULA, G. A. (2003). Modelos de Regressão com apoio computacional. IME-USP, São Paulo. [Não publicado,
  #    disponível em http://www.ime.usp.br/~giapaula/Book.pdf]
  #
  # Exemplos:
  # envel.pois(ajuste,sim=1000,conf=.95,maxit=50)
  # envel.pois(ajuste,res="C")
  #
  
  if(class(modelo)[1] != "glm") {
    stop(paste("\nA classe do objeto deveria ser glm e nao ",class(modelo),"!!!\n"))
  }
  quasi<-F
  if(modelo$family[[1]] != "Poisson" & modelo$family[[1]] != "poisson") {
    if(modelo$family[[1]] == "quasipoisson" || ( modelo$family[[1]] == "Quasi-likelihood" & modelo$family[[3]] == "Identity: mu") ) {
      quasi<-T
    } else {
      stop(paste("\nA familia do objeto deveria ser Poisson e nao ",modelo$family[[1]],"!!!\n"))
    }
  }
  
  if(length(iden)>1) {
    iden<--1
  }
  
  alfa<-(1-conf)/2
  X <- model.matrix(modelo)
  n <- nrow(X)
  p <- ncol(X)
  w <- modelo$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  
  #para evitar divisão por 0 ao studentizar os residuos, mas tentando manter o valor exagerado da alavanca
  h[round(h,15)==1]<-0.999999999999999
  
  m<-predict(modelo,type="response")
  y<-modelo$y
  
  fi <- 1
  if(quasi==T) {
    fi <- 1/summary(modelo)$dispersion
  }
  
  if(is.null(version$language) == F) {
    offvar<-modelo$offset
  }
  if(is.null(offvar)) {
    offvar<-rep(0,n)
  } else {
    if (length(offvar)!=n) {
      stop(paste("\nO vetor do offset deve conter o mesmo comprimento das variaveis do modelo !!!\n"))
    }
  }
  
  if(res=="Q") {
    cat("Ao utilizar o residuo Quantil para distribuicoes discretas, sugere-se plotar pelo menos 4 graficos para evitar conclusoes viesadas pela aleatoriedade que esta sendo incluida.\n")
    tipo<-"Resíduo Quantil"
    r<-qnorm( runif(n=n,min=ppois(y-1,m),max=ppois(y,m)) *sqrt(fi) )
  } else {
    if(res=="D") {
      tipo<-"Resíduo Componente do Desvio"
      r<-resid(modelo,type="deviance")*sqrt(fi*(1-h))
    } else {
      if(res=="P") {
        tipo<-"Resíduo de Pearson Padronizado"
        r<-resid(modelo,type="pearson")*sqrt(fi*(1-h))
      } else {
        if(res=="A") {
          tipo<-"Resíduo de Anscombe"
          r<-1.5*sqrt(fi)*( y^(2/3) - m^(2/3) )/(m^(1/6))
        } else {
          if(res=="W") {
            tipo<-"Resíduo de Williams"
            r<-sign(y-m)*sqrt((1-h)*(( resid(modelo,type="deviance")*sqrt(fi/(1-h)) )^2)+(h*( resid(modelo,type="pearson")*sqrt(fi/(1-h)) )^2))
          } else {
            if(res=="C") {
              tipo<-"Distância de Cook"
              r<-(h/((1-h)*p))*((resid(modelo,type="pearson")/sqrt(1-h))^2)
            } else {
              stop(paste("\nVoce nao escolheu corretamente um dos residuos disponiveis!!!\n"))
            }
          }
        }
      }
    }
  }
  
  link<-modelo$family[[2]]
  
  e <- matrix(0,n,sim)
  e1 <- numeric(n)
  e2 <- numeric(n)
  
  if (is.null(version$language) == T) {
    #No S-Plus, a opção start é para entrar com o preditor linear
    pm<-predict(modelo)
  } else {
    #No R, a opção start é para entrar com os coeficientes
    pm<-coef(modelo)
  }
  mu<-m
  for(i in 1:sim) {
    resp <- rpois(n,mu)
    if ( (is.null(version$language) == T && link == "Log: log(mu)") | (is.null(version$language) == F && link == "log") ) {
      fit <- glm(resp ~ X-1+offset(offvar),family=poisson,maxit=maxit,start=pm)
    } else {
      if ( (is.null(version$language) == T && link == "Square Root: sqrt(mu)") | (is.null(version$language) == F && link == "sqrt") ) {
        fit <- glm(resp ~ X-1+offset(offvar),family=poisson(link=sqrt),maxit=maxit,start=pm)
      } else {
        if ( (is.null(version$language) == T && link == "Identity: mu") | (is.null(version$language) == F && link == "identity") ) {
          fit <- glm(resp ~ X-1+offset(offvar),family=poisson(link=identity),maxit=maxit,start=pm)
        } else {
          stop(paste("\nEsta funcao so aceita as ligacoes: canonica (log), raiz quadrada e identidade!!!\nLigacao ",link," desconhecida!!!\n"))
        }
      }
    }
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    h[round(h,15)==1]<-0.999999999999999
    m <- predict(fit,type="response")
    y <- fit$y
    e[,i] <- 
      sort( if(res=="Q") {
        qnorm( runif(n=n,min=ppois(y-1,m),max=ppois(y,m)) )
      } else {
        if(res=="D") {
          resid(fit,type="deviance")/sqrt(1-h)
        } else {
          if(res=="P") {
            resid(fit,type="pearson")/sqrt(1-h)
          } else {
            if(res=="A") {
              1.5*( y^(2/3) - m^(2/3) )/(m^(1/6))
            } else {
              if(res=="W") {
                sign(y-m)*sqrt((1-h)*(( resid(fit,type="deviance")/sqrt(1-h) )^2)+(h*( resid(fit,type="pearson")/sqrt(1-h) )^2))
              } else {
                if(res=="C") {
                  (h/((1-h)*p))*((resid(fit,type="pearson")/sqrt(1-h))^2)
                } else {
                  stop(paste("\nVoce nao escolheu corretamente um dos residuos disponiveis!!!\n"))
                }
              }
            }
          }
        }
      })
  }
  
  for(i in 1:n) {
    eo <- sort(e[i,])
    e1[i] <- quantile(eo,alfa)
    e2[i] <- quantile(eo,1-alfa)
  }
  
  med <- apply(e,1,median)
  
  if(quad==T) {
    par(pty="s")
  }
  if(res=="C") {
    #Segundo McCullagh e Nelder (1989, pág.407) e Paula (2003, pág.57) deve-se usar qnorm((n+1:n+.5)/(2*n+1.125))
    #Segundo Neter et alli (1996, pág.597) deve-se usar qnorm((n+1:n-.125)/(2*n+0.5))
    qq<-qnorm((n+1:n+.5)/(2*n+1.125))
    plot(qq,sort(r),xlab="Quantil Meio-Normal",ylab=tipo, ylim=range(r,e1,e2), pch=16)
  } else {
    qq<-qnorm((1:n-.375)/(n+.25))
    plot(qq,sort(r),xlab="Quantil da Normal Padrão",ylab=tipo, ylim=range(r,e1,e2), pch=16)
  }
  lines(qq,e1,lty=1)
  lines(qq,e2,lty=1)
  lines(qq,med,lty=2)
  nome<-nome[order(r)]
  r<-sort(r)
  while ( (!is.numeric(iden)) || (round(iden,0) != iden) || (iden < 0) ) {
    cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
    out <- readline()
    iden<-as.numeric(out)
  }
  if(iden>0) {identify(qq,r,n=iden,labels=nome)}
  if(quad==T) {
    par(pty="m")
  }
  cat("Banda de ",conf*100,"% de confianca, obtida por ",sim," simulacoes.\n")
}