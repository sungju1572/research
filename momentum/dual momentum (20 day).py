from matplotlib import pyplot as plt


def data_preprocessing(read_df, base_date):   
    read_df = read_df[read_df['Date'] >= base_date].copy()
    read_df.reset_index(inplace=True, drop = True)
    read_df['STD_YM'] = read_df['Date'].map(lambda x : datetime.datetime.strptime(x, '%Y-%m-%d').strftime('%Y-%m'))
    read_df['1M_RET'] = 0.0
    ym_keys = list(read_df['STD_YM'].unique())
    return read_df, ym_keys

def create_trade_book(sample, sample_codes):
    book = pd.DataFrame()
    book = sample[sample_codes].copy()
    book['STD_YM'] = book.index.map(lambda x : datetime.datetime.strptime(x,'%Y-%m-%d').strftime('%Y-%m'))
    for c in sample_codes:
        book['p '+c] = ''
        book['r '+c] = ''
    return book

# 상대모멘텀 Positioning
def positions(book, s_codes):
    std_ym = ''
    buy_phase = False
    for s in s_codes : 
        print(s)
        for i in book.index:
            if book.loc[i,'p '+s] == '' and book.shift(1).loc[i,'p '+s] == 'ready ' + s:
                std_ym = book.loc[i,'date']
                buy_phase = True
            
            if book.loc[i,'p '+s] == '' and book.loc[i,'date'] == std_ym and buy_phase == True : 
                book.loc[i,'p '+s] = 'buy ' + s
            
            if book.loc[i,'p '+ s] == '' :
                std_ym = None
                buy_phase = False
    return book
acc_rtn_exam = []

def multi_returns(book, s_codes):
    # 손익 계산
    rtn = 1.0
    buy_dict = {}
    num = len(s_codes)
    sell_dict = {}
    
    
    for i in book.index:
        for s in s_codes:
            if book.loc[i, 'p ' + s] == 'buy '+ s and \
            book.shift(1).loc[i, 'p '+s] == 'ready '+s and \
            book.shift(2).loc[i, 'p '+s] == '' or  book.loc[i, 'p ' + s] == 'ready '+s and \
            book.shift(1).loc[i, 'p '+s] == 'ready '+s and \
            book.shift(2).loc[i, 'p '+s] == '' or book.loc[i, 'p ' + s] == 'ready '+s and \
            book.shift(1).loc[i, 'p '+s] == 'ready '+s and \
            book.shift(2).loc[i, 'p '+s] == 'ready '+s :     # long 진입
                buy_dict[s] = book.loc[i, s]
            elif book.loc[i, 'p '+ s] == '' and book.shift(1). loc[i, 'p '+s] == 'buy '+ s:     # long 청산
                sell_dict[s] = book.loc[i, s]
                # 손익 계산
                rtn = (sell_dict[s] / buy_dict[s]) -1
                book.loc[i, 'r '+s] = rtn
                print('개별 청산일 : ',i,' 종목코드 : ', s , 'long 진입가격 : ', buy_dict[s], ' |  long 청산가격 : ', sell_dict[s], ' | return:', round(rtn * 100, 2),'%') # 수익률 계산.

            if book.loc[i, 'p '+ s] == '':     # zero position || long 청산.
                buy_dict[s] = 0.0
                sell_dict[s] = 0.0


    acc_rtn = 1.0        
    for i in book.index:
        rtn  = 0.0
        count = 0
        for s in s_codes:
            if book.loc[i, 'p '+ s] == '' and book.shift(1).loc[i,'p '+ s] == 'buy '+ s:  # 청산. 이떄 수익률이 나오니깐.
                count += 1
                rtn += book.loc[i, 'r '+s]
        if (rtn != 0.0) & (count != 0) :
            # 내가 후보로 삼은 universe에서  1/n 이라고 가정.
            acc_rtn *= (rtn /count )  + 1
            print('누적 청산일 : ',i,'청산 종목수 : ',count, \
                  '청산 수익률 : ',round((rtn /count),4),'누적 수익률 : ' ,round(acc_rtn, 4)) # 수익률 계산.
            acc_rtn_exam.append(round(acc_rtn, 4))
        book.loc[i,'acc_rtn'] = acc_rtn
    
    print ('누적 수익률 :', round(acc_rtn, 4))




import pandas as pd
import numpy as np
import datetime


read_df = pd.read_csv('C:/Users/Schbi/Desktop/연구/us_etf/new_us_etf_stock.csv')


price_df, ym_keys = data_preprocessing(read_df, base_date='2010-01-02')
month_last_df = pd.DataFrame(columns=['Date','Code','Adj Close'])
month_last_ret_df = pd.DataFrame(columns=['Date','Code','1M_RET'])



s_codes = list(price_df['Code'].unique())

for s in s_codes:
    print(s)
    for ym in ym_keys:
        try : 
            # 절대 모멘텀 사용 종가
            month_last_df= month_last_df.append(price_df.loc[price_df[(price_df['STD_YM'] == ym) \
                                                                      & (price_df['Code'] == s)].index[-1]\
                                                             ,['Date','Code','Adj Close']\
                                                            ]
                                               )
            
            # 상대 모멘텀 사용 수익률
            m_ret = price_df.loc[price_df[(price_df['STD_YM'] == ym) & (price_df['Code'] == s)].index[-1] , 'Adj Close'] \
            / price_df.loc[price_df[(price_df['STD_YM'] == ym) & (price_df['Code'] == s)].index[0] , 'Adj Close']
            price_df.loc[(price_df['STD_YM'] == ym) & (price_df['Code'] == s), ['1M_RET']] = m_ret
            
            
            month_last_ret_df= month_last_ret_df.append(price_df.loc[price_df[(price_df['STD_YM'] == ym) \
                                                                      & (price_df['Code'] == s)].index[-1]\
                                                             ,['Date','Code','1M_RET']\
                                                            ]
                                               )
        except IndexError:
            pass







price_df.loc[price_df[(price_df['STD_YM'] == "2010-01") \
                                                                      & (price_df['Code'] == "AAPL")].index[-1]\
                                                             ,['Date','Code','1M_RET']\
                                                            ]

    
month_last_df.reset_index(inplace=True,drop=True)

###############일단위 
p1=price_df.copy()
p2=price_df['Adj Close'].copy()


month_p1 = p1.loc[:,['Date','Code','Adj Close']]

month_ret_p1 = p1.loc[:,['Date','Code']]


aa=[]

for a,i in p2.iteritems():
    if a<=28384:
        num=p2[a]/p2[a+1]
        aa.append(num)  
    


month_p2 = month_p1.drop([0])
month_ret_p2= month_ret_p1.drop([0])

month_ret_p2["1M_RET"] = aa    
    



##듀얼모멘텀

bf_month = 20
month_p2.reset_index(inplace=True,drop=True)
month_last_tmp_df = month_p2.pivot('Date','Code','Adj Close').copy()
abs_momentum_filter =  month_last_tmp_df / month_last_tmp_df.shift(bf_month) -1
abs_momentum_filter = abs_momentum_filter.where(abs_momentum_filter > 0.0 , np.nan)
abs_momentum_filter.fillna(0,inplace=True)
abs_momentum_filter[abs_momentum_filter != 0] = 1


# 유니버스 내 상위 20% 상대 모멘텀
ratio = 0.2   # 상위 20%에 드는 종목들만.
month_ret_p2.reset_index(inplace=True,drop=True)
month_ret_p2 = month_ret_p2.pivot('Date','Code','1M_RET').copy()
rel_momentum_filter = month_ret_p2.rank(axis=1, ascending=False, method="max", pct=True) # 투자종목 선택할 rank
rel_momentum_filter = rel_momentum_filter.where( rel_momentum_filter < ratio , np.nan)
rel_momentum_filter.fillna(0,inplace=True)
rel_momentum_filter[rel_momentum_filter != 0] = 1

#듀얼 모멘텀.
dual_mementum_filter = abs_momentum_filter * rel_momentum_filter # 절대 모멘텀 * 상대 모멘텀 => 듀얼 모멘텀


abs_momentum_filter
rel_momentum_filter




#trading 부분.
sig_dict = dict()
for date in dual_mementum_filter.index:
    ticker_list = list(dual_mementum_filter.loc[date,dual_mementum_filter.loc[date,:] >= 1.0].index)
    sig_dict[date] = ticker_list
    
stock_c_matrix = price_df.pivot('Date','Code','Adj Close').copy()
book = create_trade_book(stock_c_matrix, s_codes)
book['STD_YM'] = book.index.map(lambda x : datetime.datetime.strptime(x,'%Y-%m-%d').strftime('%Y-%m'))

#ready 표시
for date,values in sig_dict.items():
    for stock in values:
        book.loc[date,'p '+ stock] = 'ready ' + stock
        

# # trading 부분 만들기
book["date"]= book.index

book = positions(book, s_codes)

multi_returns(book, s_codes)

#누적 수익률 df로 저장
acc_rtn_exam

acc_df = pd.DataFrame(columns = ["acc"])

acc_df["acc"]  = acc_rtn_exam

#그래프
plt.plot(acc_df)
plt.xlabel("trade_count")
plt.ylabel("acc")
plt.title("20-daily-moment")


book2 = book[["AAPL","AMZN","BND","GLD","SLV","GDX","MSFT","SPY","USM","USO","WMT"]].copy()
book2.loc[book2.index[-1]].sum()/book2.loc["2010-01-04"].sum()




