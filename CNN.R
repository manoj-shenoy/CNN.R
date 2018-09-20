update.packages("h2o",repos="https://cran.rstudio.com/")
      # repos = "https://cran.r-project.org/web/packages/h2o/index.html")
library(h2o)

h2o.init()

?h2o.deeplearning()

amat.data.full = read.csv("amat.csv",header = T)

#index.amat=sample(1:nrow(amat.data),round(0.75*nrow(amat.data)))
attach(amat.data.full)

# Calculating rolling returns for upto 5 days 
ret.1D = diff(Close, lag = 1)/Close[-length(Close)]
ret.2D =diff(Close, lag = 2)/Close[-length(Close)]
ret.3D =diff(Close, lag = 3)/Close[-length(Close)]
ret.4D =diff(Close, lag = 4)/Close[-length(Close)]
ret.5D =diff(Close, lag = 5)/Close[-length(Close)]

amat.data = data.frame(cbind(Day,ret.1D,ret.2D,ret.3D,ret.4D,ret.5D,Close),
                       fix.empty.names = T)
typeof(amat.data)
amat.data[,1]



head(amat.data)
train.amat = amat.data[7:2600,]

test.amat = amat.data[2601:2766,]

# head(test.amat)

amat.data.h2o=as.h2o(train.amat)
CC
amat.dl = h2o.deeplearning(x=1:6,y=7,
                           training_frame = amat.data.h2o,
                           epochs = 100,nfolds = 10,stopping_metric = "deviance",
                           activation = "Rectifier"
                          )
amat.dl

# now make a prediction
#predictions = h2o.predict(amat.dl, as.h2o(test.amat[,2:4]))

predictions = h2o.predict(amat.dl,as.h2o(test.amat[7]))
                          

# amat.pred.table = cbind.data.frame(data.frame(amat.data[1]),amat.data[5],
#                                    data.frame(predictions))

# length(predictions)
# write.csv(amat.pred.table,"Amat.Predictions.csv")
# sum((as.data.frame(predictions) - as.data.frame(amat.data.h2o[5]))^2)/nrow(predictions)
# sum(abs(diff))
plot(as.data.frame(test.amat[1]),as.data.frame(predictions))

plot(x=test.amat[1],y=test.amat[7],type='l')

tail(predictions)

tail(train.amat[7])

h2o.shutdown()

# =============== Deep learning for prediction

# repos = "https://cran.r-project.org/web/packages/h2o/index.html")
library(h2o)

h2o.init()

?h2o.deeplearning()

amat.data.full = read.csv("amat.csv",header = T)

#index.amat=sample(1:nrow(amat.data),round(0.75*nrow(amat.data)))
attach(amat.data.full)

# Calculating rolling returns for upto 5 days 
ret.1D = diff(Close, lag = 1)/Close[-length(Close)]
ret.2D =diff(Close, lag = 2)/Close[-length(Close)]
ret.3D =diff(Close, lag = 3)/Close[-length(Close)]
ret.4D =diff(Close, lag = 4)/Close[-length(Close)]
ret.5D =diff(Close, lag = 5)/Close[-length(Close)]

amat.data = data.frame(cbind(Day,ret.1D,ret.2D,ret.3D,ret.4D,ret.5D,Close),
                       fix.empty.names = T)
typeof(amat.data)
amat.data[,1]



head(amat.data)
train.amat = amat.data[7:2600,]

test.amat = amat.data[2601:2766,]

amat.data.h2o=as.h2o(train.amat)

amat.dl = h2o.deeplearning(x=1,y=7,
                           training_frame = amat.data.h2o,
                           epochs = 100,nfolds = 10,stopping_metric = "MSE",
                           activation = "Rectifier"
)
amat.dl

# now make a prediction
#predictions = h2o.predict(amat.dl, as.h2o(test.amat[,2:4]))

head(test.amat)

predictions = h2o.predict(amat.dl,as.h2o(test.amat[1]))


# amat.pred.table = cbind.data.frame(data.frame(amat.data[1]),amat.data[5],
#                                    data.frame(predictions))

# length(predictions)
# write.csv(amat.pred.table,"Amat.Predictions.csv")
# sum((as.data.frame(predictions) - as.data.frame(amat.data.h2o[5]))^2)/nrow(predictions)
# sum(abs(diff))
plot(as.data.frame(test.amat[1]),as.data.frame(predictions))

plot(x=test.amat[1],y=test.amat[7],type='l')

tail(predictions)

tail(train.amat[7])

h2o.shutdown()


#---------------------------------------------------------------------------
#------------------ Fundamental data Analysis -----------------#

# getting Financial data into R
#options(repos = c(CRAN = "https://cran.revolutionanalytics.com"))
#install.packages("Quandl")#,repos = "https://www.cran.rstudio.com/")

# Parameters for ZACKS/FC datatable. For ZACKS/FR datatable,not required
#-------------------------------------------------------------------------------
# ROA=Netinc/Totassets, ROE=net_income_loss/ tot_share_holder_equity
# PE=(Price/tot_share_holder_equity)/diluted_net_eps, 
# EBITDA/Share = ebitda/tot_share_holder_equity
# P/CF=price/tot_cash_flow,
# Retention ratio=[1 - (comm_stock_div_paid/tot_share_holder_equity)]/diluted_net_eps 
# ROCE = oper_income/(tot_share_holder_equity+tot_asset-tot_curr_liab)

#------ h2o --Generic Function for stock prediction using Fundamental data & Deep learning-------
#------------------------------------------------------------------------------------------
# options(repos = c(CRAN = "https://cran.revolutionanalytics.com"))
# install.packages(c("curl,httr"))

library(RCurl)
library(curl)
library(httr)
library(Quandl)
Quandl.api_key("vqHfJUtARnKTS4bczZYm")

library(h2o)
h2o.init() #----- Initialize a h2o Cluster------------

Stock.Pred.h2o=function(ticker,start.period, param.index1,param.index2,
                        distribution=c("AUTO","gaussian","poisson","gamma", "tweedie","laplace"),
                        activation=c("Tanh", "TanhWithDropout","Rectifier","RectifierWithDropout",
                                    "Maxout","MaxoutWithDropout"),seed=seed)
                         
  {
    # stock.quandl ->> variable which stores Fundamental data
    stock.quandl=Quandl.datatable("ZACKS/FR", ticker=ticker,per_end_date.gt=start.period,
                                  qopts.columns=c("m_ticker", "per_end_date",
                                     "ebit_margin","asset_turn","ret_equity",
                                     "ret_asset","ret_invst","free_cash_flow_per_share"
                                     ))
    
    stock.price=Quandl(stringi::stri_join("WIKI/",ticker),collapse = "quarterly",
                        start_date=start.period,order = 'asc')
    
    stock.ret.1Q = diff(stock.price[,12],lag = 1)/stock.price[-nrow(stock.price),12]
    
    stock.quandl=cbind(stock.quandl[7:31,],stock.ret.1Q[1:25])
    
    train.stock.quandl=stock.quandl[1:round(0.85*nrow(stock.quandl)),]
    test.stock.quandl=stock.quandl[round(0.85*nrow(stock.quandl)):nrow(stock.quandl)-1,]
    
    train.stock.h2o = as.h2o(train.stock.quandl)
    
    # Training the model using multiple distributions - Gaussian, Poisson & Gamma
    Close.index=9
    stock.dl = h2o.deeplearning(x=param.index1:param.index2,y=Close.index,
                               training_frame = train.stock.h2o,
                               epochs = 400,nfolds = 3,stopping_metric = "MSE",
                               activation = activation,seed = seed,
                               input_dropout_ratio = 0.1,distribution = distribution)
    #stock.dl
    predictions.stock = h2o.predict(stock.dl,
                                   as.h2o(test.stock.quandl[,param.index1:param.index2]))
    
    # res.comparison = cbind(predictions.stock,
    #                        test.stock.quandl[,9],deparse.level = 1)
    #h2o.shutdown()
    return(predictions.stock)
  }
# --------------------------STOCK - APPLE------------------------------------

simulations = function(no.of.simulations,distribution=c("AUTO","gaussian","poisson",
                                      "gamma", "tweedie", "laplace"),
                       activation=c("Tanh", "TanhWithDropout","Rectifier",
                                    "RectifierWithDropout",
                                    "Maxout","MaxoutWithDropout"))
      {
        for (seed in 1:no.of.simulations) 
          {
            Stock.Pred=Stock.Pred.h2o(ticker = 'AAPL',start.period = "2011-01-01",
                                      param.index1 = 3,param.index2 = 8,
                                      distribution=distribution,
                                      activation=activation,
                                      seed = seed)
            print(Stock.Pred)
        }
      }

sim=simulations(5,distribution = "gaussian",activation = "Tanh")


# ------------------------ STOCK - VISA--------------------------------------

simulations = function(no.of.simulations,distribution=c("AUTO","gaussian","poisson",
                                                               "gamma", "tweedie", "laplace"),
                       activation=c("Tanh", "TanhWithDropout","Rectifier",
                                    "RectifierWithDropout",
                                    "Maxout","MaxoutWithDropout"))
{
  for (seed in 1:no.of.simulations) 
  {
    Stock.Pred=Stock.Pred.h2o(ticker = 'V',start.period = "2011-01-01",
                              param.index1 = 3,param.index2 = 8,
                              distribution=distribution,
                              activation=activation,
                              seed = seed)
    print(Stock.Pred)
  }
}

simulations(5,distribution = "gaussian",activation = "Tanh")
