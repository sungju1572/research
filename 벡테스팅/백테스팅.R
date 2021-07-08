####포트폴리오 백테스트####

#전통적인 60 대 40 포트폴리오 백테스트
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)
library(timeSeries)


ticker = c('SPY', 'TLT')
getSymbols(ticker)


prices = do.call(cbind,
                 lapply(ticker, function(x) Ad(get(x))))
rets = Return.calculate(prices) %>% na.omit()  #수익률 계산 (오늘값/어제값)

#상관성 확인 -> 매우낮은 상관관계, 강한 분산효과 기대 가능!
cor(rets)

#백테스트 실행
portfolio = Return.portfolio(R = rets,               #자산의 수익률
                             weights = c(0.6, 0.4),  #리밸런싱 비중
                             rebalance_on = 'years', #리밸런싱 시기
                             verbose = TRUE)         #리스트로 return


a <- portfolio$returns


b <- abs(portfolio$BOP.Weight - portfolio$EOP.Weight)


portfolios = cbind(rets, portfolio$returns) %>%
  setNames(c('주식', '채권', '60대 40'))

charts.PerformanceSummary(portfolios,
                          main = '60대 40 포트폴리오')

#회전율 계산
turnover = xts(
  rowSums(abs(portfolio$BOP.Weight -
                timeSeries::lag(portfolio$EOP.Weight)),
          na.rm = TRUE),
  order.by = index(portfolio$BOP.Weight))

chart.TimeSeries(turnover)


#시점 선택 전략 백테스트
library(quantmod)
library(PerformanceAnalytics)

#S&P 500(주식) / 미국단기채(현금)
symbols = c('SPY', 'SHY')
getSymbols(symbols, src = 'yahoo')


prices = do.call(cbind,
                 lapply(symbols, function(x) Ad(get(x))))
rets = na.omit(Return.calculate(prices))

ep = endpoints(rets, on = 'months')

print(ep)


wts = list() #시점별 비중 리스트

lookback = 10 #이평값

ep = endpoints(rets, on = 'months')
wts = list()
lookback = 10

for (i in (lookback+1) : length(ep)) {
  sub_price = prices[ep[i-lookback] : ep[i] , 1]
  sma = mean(sub_price)
  wt = rep(0, 2)
  wt[1] = ifelse(last(sub_price) > sma, 1, 0)
  wt[2] = 1 - wt[1]
  
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
}

wts = do.call(rbind, wts)


#시점선택 백테스팅
Tactical = Return.portfolio(rets,              #수익률
                            wts,               #비중  
                            verbose = TRUE)


portfolios = na.omit(cbind(rets[,1], Tactical$returns)) %>%
  setNames(c('매수 후 보유', '시점 선택 전략'))

charts.PerformanceSummary(portfolios,
                          main = "Buy & Hold vs Tactical")

#회전율
turnover = xts(rowSums(abs(Tactical$BOP.Weight -
                             timeSeries::lag(Tactical$EOP.Weight)),
                       na.rm = TRUE),
               order.by = index(Tactical$BOP.Weight))

chart.TimeSeries(turnover)


#동적 자산배불 포트폴리오
library(quantmod)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)

symbols = c('SPY', # 미국 주식
            'IEV', # 유럽 주식 
            'EWJ', # 일본 주식
            'EEM', # 이머징 주식
            'TLT', # 미국 장기채
            'IEF', # 미국 중기채
            'IYR', # 미국 리츠
            'RWX', # 글로벌 리츠
            'GLD', # 금
            'DBC'  # 상품
)
getSymbols(symbols, src = 'yahoo')

prices = do.call(cbind, lapply(symbols, function(x) Ad(get(x)))) %>%
  setNames(symbols)

rets = Return.calculate(prices) %>% na.omit()


#백테스트에 사용되는 각종 값을 사전에 정의
ep = endpoints(rets, on = 'months')                 #매월 말일의 위치 구하기
wts = list()                                        #매월의 투자비중이 들어갈 빈 리스트를 wts에 설정
lookback = 12                                       #수익률을 측정할 과거 n기간을 12개월로 설정
wt_zero = rep(0, 10) %>% setNames(colnames(rets))   #비중이 들어갈 0으로 이우어진 벡터 생성


#매월 말 투자 규칙에 따라 포트폴리오의 비중을 구하는 백테스트 과정
for (i in (lookback+1) : length(ep)) {
  sub_ret = rets[ep[i-lookback] : ep[i] , ]
  cum = Return.cumulative(sub_ret)
  
  K = rank(-cum) <= 5
  covmat = cov(sub_ret[, K])
  
  wt = wt_zero
  wt[K] = optimalPortfolio(covmat,
                           control = list(type = 'minvol',     #최소분산 포트폴리오
                                          constraint = 'user',
                                          LB = rep(0.10, 5),   #최소비중
                                          UB = rep(0.30, 5)))  #최대비중
  
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
}

wts = do.call(rbind, wts)

head(wts)

#계산된 비중으로 백테스트

GDAA = Return.portfolio(rets, wts, verbose = TRUE)
charts.PerformanceSummary(GDAA$returns, main = '동적자산배분')



#투자비중 변화
wts %>% fortify.zoo() %>%
  gather(key, value, -Index) %>%
  mutate(Index = as.Date(Index)) %>%
  mutate(key = factor(key, levels = unique(key))) %>%
  ggplot(aes(x = Index, y = value)) +
  geom_area(aes(color = key, fill = key),
            position = 'stack') +
  xlab(NULL) + ylab(NULL) +  theme_bw() +
  scale_x_date(date_breaks="years", date_labels="%Y",
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1, size = 8),
        panel.grid.minor.x = element_blank()) +
  guides(color = guide_legend(byrow = TRUE))


#회전율 계산
GDAA$turnover = xts(
  rowSums(abs(GDAA$BOP.Weight -
                timeSeries::lag(GDAA$EOP.Weight)),
          na.rm = TRUE),
  order.by = index(GDAA$BOP.Weight))

chart.TimeSeries(GDAA$turnover)


#매수 & 매도당 발생하는 세금, 수수료, 시장충격 등 총 비용을 0.3%으로 가정
fee = 0.0030
GDAA$net = GDAA$returns - GDAA$turnover*fee

#그래프
cbind(GDAA$returns, GDAA$net) %>%
  setNames(c('No Fee', 'After Fee')) %>%
  charts.PerformanceSummary(main = 'GDAA')
           