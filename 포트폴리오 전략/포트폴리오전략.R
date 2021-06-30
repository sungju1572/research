library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

#quantmod 패키지 : 주식데이터 패키기 

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

#주식데이터 가져오기 
#주식 데이터, 환율 데이터, 원자재 가격, 그 외 FRED에서 공개한 데이터는 다 가져올 수 있다.
getSymbols(symbols, src = 'yahoo')


#수정종가만 가져오고 리스트로 묶기
prices = do.call(cbind,
                 lapply(symbols, function(x) Ad(get(x)))) %>%
  setNames(symbols)

#수익률 계산
rets = Return.calculate(prices) %>% na.omit()



library(tidyr)
library(dplyr)
library(corrplot)

#etf간 상관성 구하기
cor(rets) %>%
  corrplot(method = 'color', type = 'lower',
           addCoef.col = 'black', number.cex = 0.7,
           tl.cex = 1, tl.srt = 0, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar = c(0,0,0.5,0))

#분산-공분산 행렬
covmat = cov(rets)

##최소비용 포트폴리오
#slsqp()

#목적함수
objective = function(w) {
  obj = t(w) %*% covmat %*% w
  return(obj)
}

#제약조건 
hin.objective = function(w) {
  return(w)
}

#자산군합은 1이라는 제약조건
heq.objective = function(w) {
  sum_w = sum(w)
  return( sum_w - 1 )
}

#실행
library(nloptr)

result = slsqp( x0 = rep(0.1, 10),    #초기값 (0.1, 10개)
                fn = objective,       #목적함수
                hin = hin.objective,  #부등위 제약조건
                heq = heq.objective)  #등위 제약조건

#최적화된 지점의 해, 최소분산 포트폴리오를 구성하는 자산들의 투자 비중
print(result$par)

#산출된 값을 목적함수 fn에 입력하였을 때 나오는 결괏값으로써, 포트폴리오의 분산을 의미
print(result$value)

w_1 = result$par %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_1)
sum(w_1)


#solve.QP()

Dmat = covmat                                    #분산-공분산 행렬
dvec = rep(0, 10)                                #벡터부분 (포트폴리오 최적화에서는 역할 x)
Amat = t(rbind(rep(1, 10), diag(10), -diag(10))) #제약조건(좌변)
bvec = c(1, rep(0, 10), -rep(1, 10))             #제약조건(우변) (비중의 합이1, 각 비중이 0보다큼)
meq = 1                                          #등위 제약조건 개수 (비중의 합이 1)

#실행
library(quadprog)
result = solve.QP(Dmat, dvec, Amat, bvec, meq)

#최적화된 지점의해, 즉 최소분산 포트폴리오를 구성하는 자산들의 투자비용
print(result$solution)
#$solution 에서 산출된 값을 목적함수에 입력했을 떄 나오는 결과값, 포트폴리오의 분산
print(result$value)


w_2 = result$solution %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_2)
sum(w_2)


#optimalPortfolio() 
library(RiskPortfolios)

w_3 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                                      constraint = 'lo')) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w_3) 
sum(w_3)


#결과값들 비교
library(ggplot2)

data.frame(w_1) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w_1)) +
  geom_col() +
  xlab(NULL) + ylab(NULL)


#제약조건 추가 slsqp()
result = slsqp( x0 = rep(0.1, 10),
                fn = objective,
                hin = hin.objective,
                heq = heq.objective,
                lower = rep(0.05, 10), #최소투자비용
                upper = rep(0.20, 10)) #최대투자비용

w_4 = result$par %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_4)

#제약조건 추가 solve.QP()
Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(rep(1, 10), diag(10), -diag(10)))
bvec = c(1, rep(0.05, 10), -rep(0.20, 10)) #최소 최대 투자비용
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w_5 = result$solution %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_5)

#제약조건 추가 optimalProtfolio()
w_6 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                                      constraint = 'user', #제약조건 직접입력
                                      LB = rep(0.05, 10),
                                      UB = rep(0.20, 10))) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w_6)


#최소 최대 비중제약 결과비교
data.frame(w_4) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w_4)) +
  geom_col() +
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab(NULL)

#각 자산별 제약조건 추가
Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(rep(1, 10), diag(10), -diag(10))) 
bvec = c(1, c(0.10, 0.10, 0.05, 0.05, 0.10,   #각자산별 제약조건추가
              0.10, 0.05, 0.05, 0.03, 0.03),
         -c(0.25, 0.25, 0.20, 0.20, 0.20,
            0.20, 0.10, 0.10, 0.08, 0.08))  


meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w_7 = result$solution %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w_7)


######최대분산 포트폴리오
Dmat = covmat                                   #분산-공분산 행렬
dvec = rep(0, 10)                               #벡터부분 (포트폴리오 최적화에서는 역할 x)
Amat = t(rbind(sqrt(diag(covmat)), diag(10)))   #제약조건(좌변)
bvec = c(1, rep(0, 10))                         #제약조건(우변)
meq = 1                                         #등위제약조건 (비중의 합이 1)



result = solve.QP(Dmat, dvec, Amat, bvec, meq)  #solce.QP() 함수 사용해서 최대분산 포트폴리오 구성

w = result$solution %>%
  round(., 4) %>%            #반올림
  setNames(colnames(rets))   

print(w)

#최적화
w = (w / sum(w)) %>%
  round(., 4)

print(w)


#그래프
data.frame(w) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w)) +
  geom_col() +
  geom_col() +
  xlab(NULL) + ylab(NULL)



#optimalPortfolio() 함수 사용
w = optimalPortfolio(covmat,                                        #Min -DR 방법을 사용
                     control = list(type = 'maxdiv',
                                    constraint = 'lo')) %>%
  round(., 4)

print(w)


##최소& 최대 비중 제약조건 추가
Dmat = covmat                                            #분산-공분산 행렬
dvec = rep(0, 10)                                        #벡터부분 (포트폴리오 최적화에서는 역할 x)
Alb = -rep(0.05, 10) %*% matrix(1, 1, 10) + diag(10)     # -rep(0.05, 10) : -lb(최소) 부분 / matrix(1, 1, 10) : e^T 부분 / diag(10) : I(단위행렬) 부분 
Aub = rep(0.20, 10) %*% matrix(1, 1, 10) - diag(10)      # rep(0.20, 10) : ub(최대) 부분 / matrix(1, 1, 10) : e^T 부분 / diag(10) : I(단위행렬) 부분

Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0, 10), rep(0, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution 
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)


#최소 & 최대 [5%, 20%] 제약조건 넣어서 그래프 그려보기
data.frame(w) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w)) +
  geom_col() +
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab(NULL)



##자산별제약조건 추가
Dmat = covmat
dvec = rep(0, 10)
Alb = -c(0.10, 0.10, 0.05, 0.05, 0.10,            #최소부분 자산별 제약조건
         0.10, 0.05, 0.05, 0.03, 0.03) %*%        
  matrix(1, 1, 10) + diag(10)
Aub = c(0.25, 0.25, 0.20, 0.20, 0.20,             #최대부분 자산별 제약조건
        0.20, 0.10, 0.10, 0.08, 0.08) %*%
  matrix(1, 1, 10) - diag(10)

Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0, 10), rep(0, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution 
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)

#그래프
data.frame(w) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w)) +
  geom_col() +
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab(NULL)



#optimalPortfolio 함수로 제약조건 추가해보기
w_6 = optimalPortfolio(covmat,
                       control = list(type = 'maxdiv',
                                      constraint = 'user', #제약조건 직접입력
                                      LB = c(0.10, 0.10, 0.05, 0.05, 0.10,0.10, 0.05, 0.05, 0.03, 0.03),
                                      UB = c(0.25, 0.25, 0.20, 0.20, 0.20,0.20, 0.10, 0.10, 0.08, 0.08) %>%
  round(., 4) %>%
  setNames(colnames(rets))))

print(w_6)


##위험기여 포트폴리오
#위험기여도 함수 만들기
get_RC = function(w, covmat) {
  port_vol = t(w) %*% covmat %*% w
  port_std = sqrt(port_vol)
  
  MRC = (covmat %*% w) / as.numeric(port_std)
  RC = MRC * w
  RC = c(RC / sum(RC))
  
  return(RC)
}

#주식 60% 채권 40% 포트폴리오 위험기여도 확인
ret_stock_bond = rets[, c(1, 5)]                    #데이터 : 미국주식 수익률(SPY), 미국 장기채(TLT)
cov_stock_bond = cov(ret_stock_bond)                #분산-공분산 행렬
RC_stock_bond = get_RC(c(0.6, 0.4), cov_stock_bond) #위험기여도 구하기
RC_stock_bond = round(RC_stock_bond, 4)             #반올림

print(RC_stock_bond)


#rp() 함수를 써서 최적화하기
library(cccp)

opt = rp(x0 = rep(0.1, 10),   #최적화를 위한 초기입력값(동일비중 10%씩 넣기)
         P = covmat,          #분산-공분산 행렬
         mrc = rep(0.1, 10))  #목표로하는 자산별 위험기여도 값(동일비중 10%)


w = getx(opt) %>% drop()      #벡터형태로 만들기
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)

get_RC(w, covmat) %>% round(.,4)


#자산별로 다른 비중을 가지는 포트폴리오 만들기 
opt = rp(x0 = rep(0.1, 10),
         P = covmat,
         mrc = c(0.15, 0.15, 0.15, 0.15, 0.10,
                 0.10, 0.05, 0.05, 0.05, 0.05))


w = getx(opt) %>% drop()
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

get_RC(w, covmat)
print(w)

#인덱스 포트폴리오 구성
# 상위 200 종목의 시가총액비중 계산
library(stringr)
library(dplyr)

KOR_ticker = read.csv('C:\\Users\\sjyoo\\Desktop\\연구\\퀀트 포폴\\KOR_ticker.csv',row.names = 1, stringsAsFactors = FALSE) 

#코스피 200중 상위 200개만 가져오기
KOSPI200 = KOR_ticker %>% 
  filter(시장구분 == 'KOSPI') %>%
  slice(1:200) %>%
  mutate(시가총액비중 = 시가총액 / sum(시가총액))



#시각화
library(ggplot2)

KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  xlab('종목명') +
  ylab('시가총액비중(%)') +
  scale_y_continuous(labels = scales::percent)


#그림 안이쁨 -> 수정하기
KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +   #시가총액비중으로 x축 정리
  geom_point() +
  xlab('종목명') +
  ylab('시가총액비중(로그 스케일링)') +
  scale_y_log10() +                                                     #y축 로그값으로 스케일링
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +  #x중 일부종목만 표시
  theme(axis.text.x = element_text(angle = 60, hjust = 1))              #x축 글자 회전+ 위치조정

#1억이있을때 코스피 200을 따라가는(복제하는) 포폴만드는법
KOSPI200 = KOSPI200 %>%
  mutate(매수금액 = 100000000 * 시가총액비중,
             매수주수 = 매수금액 / 종가)

KOSPI200 %>% select(매수금액, 매수주수) %>% head()


#소수점 밑 버리기(반올림이나 올림하면 보유금액보다 커질수있음)
KOSPI200 = KOSPI200 %>% mutate(매수주수 = floor(매수주수))
KOSPI200 %>% select(매수금액, 매수주수) %>% head()

#실제매수금액 구하기
inv_money = KOSPI200 %>% mutate(실제매수금액 = 종가 * 매수주수) %>%
  summarize(sum(실제매수금액))

print(inv_money)

##팩터를 이용한 인핸스드 포트폴리오 구성
#PBR을 이용해 비중조절하기
KOSPI200 = KOSPI200 %>% select(종목명, PBR, 시가총액비중) %>%
  mutate(PBR = as.numeric(PBR)) 

#단순가감법
KOSPI200 = KOSPI200 %>%
  mutate(랭킹 = rank(PBR),
           조절비중 = ifelse(랭킹 <= 100, 시가총액비중 + 0.0005, 시가총액비중 - 0.0005),  #상위 100개 비중증가 / 하위 100개 비중감소
           조절비중 = ifelse(조절비중 < 0, 0, 조절비중),                                  #0미만인 종목은 투자비중 0 으로 조정
           조절비중 = 조절비중 / sum(조절비중),                                           #비중의 합을 1로 맞추기위해 다시계산
           차이 = 조절비중 - 시가총액비중) 
library(tidyr)

head(KOSPI200)


KOSPI200 %>% filter(조절비중 == 0)

#시각화
KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  geom_point(data = KOSPI200, aes(x = reorder(종목명, -시가총액비중), y = 조절비중),
             color = 'red', shape = 4) +
  xlab('종목명') +
  ylab('비중(%)') +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

#
KOSPI200_mod = KOSPI200 %>% arrange(PBR)

KOSPI200_mod %>% 
  ggplot(aes(x = reorder(종목명, PBR), y = 차이)) +
  geom_point() +
  geom_col(aes(x = reorder(종목명, PBR), y = PBR /10000), fill = 'blue', alpha = 0.2) +
  xlab('종목명') +
  ylab('차이(%)') +
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~. * 10000, name = "PBR")) +
  scale_x_discrete(breaks = KOSPI200_mod[seq(1, 200, by = 10), '종목명']) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#Z-score 정규화로 펙터에 대한 노출 극대화
KOSPI200_tilt = KOSPI200 %>%
  select(종목명, PBR, 시가총액비중, 랭킹) %>%
  mutate(zscore = -scale(랭킹),
         cdf = pnorm(zscore),
         투자비중 = 시가총액비중 * cdf,
         투자비중 = 투자비중 / sum(투자비중),
         차이 = 투자비중 - 시가총액비중)

head(KOSPI200_tilt)

#시각화
KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  geom_point(data = KOSPI200_tilt, aes(x = reorder(종목명, -시가총액비중), y = 투자비중),
             color = 'red', shape = 4) +
  xlab('종목명') +
  ylab('비중(%)') +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

#차이 시각화
KOSPI200_tilt %>%
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 차이)) +
  geom_point() +
  geom_hline(aes(yintercept = 0.005), color = 'red') + 
  geom_hline(aes(yintercept = -0.005), color = 'red') +
  xlab('종목명') +
  ylab('비중 차이(%)') +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
#차이가 너무큼! -> 제약조건 걸필요있음

#제약조건 50bp설정

while (max(abs(KOSPI200_tilt$차이)) > (0.005 + 0.00001)) {
  KOSPI200_tilt = KOSPI200_tilt %>%
    mutate_at(vars(투자비중), list(~ifelse(차이 < -0.005, 시가총액비중 - 0.005, 투자비중))) %>%  #차이가 50bp 미만이면 -50bp
    mutate_at(vars(투자비중), list(~ifelse(차이 > 0.005, 시가총액비중 + 0.005, 투자비중))) %>%  #차이가 50bp 초과이면 +50bp
    mutate(투자비중 = 투자비중 / sum(투자비중), 
               차이 = 투자비중 - 시가총액비중)
}

head(KOSPI200_tilt)

#결과 시각화
KOSPI200_tilt %>%
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 차이)) +
  geom_point() +
  geom_hline(aes(yintercept = 0.005), color = 'red') + 
  geom_hline(aes(yintercept = -0.005), color = 'red') +
  xlab('종목명') +
  ylab('비중 차이(%)') +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

#비중 시각화
KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  geom_point(data = KOSPI200_tilt, aes(x = reorder(종목명, -시가총액비중), y = 투자비중),
             color = 'red', shape = 4) +
  xlab('종목명') +
  ylab('비중(%)') +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

#PBR 시각화
KOSPI200_tilt_mod = KOSPI200_tilt %>% arrange(PBR)

KOSPI200_tilt_mod %>% 
  ggplot(aes(x = reorder(종목명, PBR), y = 차이)) +
  geom_point() +
  geom_col(aes(x = reorder(종목명, PBR), y = PBR /2000), fill = 'blue', alpha = 0.2) +
  xlab('종목명') +
  ylab('차이(%)') +
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~. * 2000, name = "PBR")) +
  scale_x_discrete(breaks = KOSPI200_mod[seq(1, 200, by = 10), '종목명']) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
