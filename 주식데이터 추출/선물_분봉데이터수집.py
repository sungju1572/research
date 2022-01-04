import sys
from datetime import datetime 
import pandas as pd
import sqlite3
import time

import sys
from PyQt5.QtWidgets import *
from PyQt5.QAxContainer import *
from PyQt5.QtCore import *
import time

today = datetime.today().strftime("%Y%m%d")

TR_REQ_TIME_INTERVAL = 0.2

class Kiwoom(QAxWidget):
    def __init__(self):
        super().__init__()
        self._create_kiwoom_instance()
        self._set_signal_slots()
        
        # 키움증권 로그인
        self.kiwoom = QAxWidget("KHOPENAPI.KHOpenAPICtrl.1")
        self.kiwoom.dynamicCall("CommConnect()")
        self.kiwoom.OnEventConnect.connect(self._event_connect)
        
        #윈도우설정
        self.setWindowTitle("PyStock")
        self.setGeometry(300, 300, 290, 300)
        
        #버튼1
        btn1 = QPushButton("미니코스피", self) 
        btn1.move(40, 30)
        btn1.clicked.connect(self.btn1_clicked)
        
        #버튼2
        btn2 = QPushButton("코스피200선물", self)
        btn2.move(150, 30)
        btn2.clicked.connect(self.btn2_clicked)

        #텍스트창
        self.text_edit = QTextEdit(self)
        self.text_edit.setGeometry(30, 100, 230, 200)
        self.text_edit.setEnabled(False)
        
        #ticker
        textlabel = QLabel("ticker:", self)
        textlabel.move(80, 70)
        
        self.combo = QComboBox(self)
        self.combo.addItem("1")
        self.combo.addItem("3")
        self.combo.addItem("5")
        self.combo.addItem("10")
        self.combo.addItem("15")
        self.combo.addItem("30")
        self.combo.addItem("60")
        self.combo.addItem("120")
        
        self.combo.move(150, 70)
        

        
    def _event_connect(self, err_code):
        if err_code == 0:
            print("connected")
        else:
            print("disconnected")

    def _create_kiwoom_instance(self):
        self.setControl("KHOPENAPI.KHOpenAPICtrl.1")

    def _set_signal_slots(self):
        self.OnEventConnect.connect(self._event_connect)
        self.OnReceiveTrData.connect(self._receive_tr_data)

    def comm_connect(self):
        self.dynamicCall("CommConnect()")
        self.login_event_loop = QEventLoop()
        self.login_event_loop.exec_()


        self.login_event_loop.exit()

    def get_code_list_by_market(self, market):
        code_list = self.dynamicCall("GetCodeListByMarket(QString)", market)
        code_list = code_list.split(';')
        return code_list[:-1]

    def get_master_code_name(self, code):
        code_name = self.dynamicCall("GetMasterCodeName(QString)", code)
        return code_name

    def set_input_value(self, id, value): #SetInputValue : tr입력값을 서버통신전에 입력
        self.dynamicCall("SetInputValue(QString, QString)", id, value)

    def comm_rq_data(self, rqname, trcode, next, screen_no): #CommRqData : tr을 서버로 통신
        self.dynamicCall("CommRqData(QString, QString, int, QString", rqname, trcode, next, screen_no)
        self.tr_event_loop = QEventLoop() #이벤트 루프 돌리기 (바로 반환되지 않기 때문에 이벤트를 줄때까지 루프)
        self.tr_event_loop.exec_()

    def _comm_get_data(self, code, real_type, field_name, index, item_name):
        ret = self.dynamicCall("CommGetData(QString, QString, QString, int, QString", code,
                               real_type, field_name, index, item_name)
        return ret.strip()

    def _get_repeat_cnt(self, trcode, rqname): #GetRepeatCnt : 반환된 데이터로 for문이나 while 문돌리기위해 
        ret = self.dynamicCall("GetRepeatCnt(QString, QString)", trcode, rqname)
        return ret

    def _receive_tr_data(self, screen_no, rqname, trcode, record_name, next, unused1, unused2, unused3, unused4): #이벤트가 발생했을때 처리
        if next == '2': #PrevNext가 2이면 데이터가 더 존재한다는것 -> 연속조회
            self.remained_data = True
        else:
            self.remained_data = False

        if rqname == "OPT50029_req": #rqname 이 "OPT50029_req"일때 _OPT50029 메서드 호출
            self._OPT50029(rqname, trcode)

        try:
            self.tr_event_loop.exit() #이벤트 루프(위와동일)
        except AttributeError:
            pass

            
    def _OPT50029(self, rqname, trcode): #tr이 호출되면 먼저 데이터 갯수 가져오기
        data_cnt = self._get_repeat_cnt(trcode, rqname)

        for i in range(data_cnt): #반복문을통해 하나씩 얻어오기
            date = self._comm_get_data(trcode, "", rqname, i, "체결시간")
            open = self._comm_get_data(trcode, "", rqname, i, "시가")
            high = self._comm_get_data(trcode, "", rqname, i, "고가")
            low = self._comm_get_data(trcode, "", rqname, i, "저가")
            close = self._comm_get_data(trcode, "", rqname, i, "현재가")
            volume = self._comm_get_data(trcode, "", rqname, i, "거래량")
            
            
            
            self.ohlcv['date'].append(date)
            self.ohlcv['open'].append(abs(float(open)))
            self.ohlcv['high'].append(abs(float(high)))
            self.ohlcv['low'].append(abs(float(low)))
            self.ohlcv['close'].append(abs(float(close)))
            self.ohlcv['volume'].append(abs(float(volume)))

 
    def btn1_clicked(self): #버튼클릭시 동작
            tick_unit = self.combo.currentText()
            kiwoom.set_input_value("종목코드", "105S1000")
            kiwoom.set_input_value("시간단위", tick_unit)
            kiwoom.comm_rq_data("OPT50029_req", "OPT50029", 0, "0101")

            
            while kiwoom.remained_data == True:
                time.sleep(TR_REQ_TIME_INTERVAL)
                kiwoom.set_input_value("종목코드", "105S1000")
                kiwoom.set_input_value("시간단위", tick_unit)
                kiwoom.comm_rq_data("OPT50029_req", "OPT50029", 2, "0101")
                
                self.text_edit.append("processing")
           
            if kiwoom.remained_data == False:
                self.text_edit.append("done")
                
            df = pd.DataFrame(kiwoom.ohlcv, columns=['date','open', 'high', 'low', 'close', 'volume'])
            
            
            
            
            #df1 = df[df["date"].str[0:8]==today]
            
            df1 = df[df["date"].str[0:4]=="2021"]
            
            
            df1.rename(columns={"date" : "minute_time"}, inplace=True)

            df1 = df1[['minute_time', 'open', 'close', 'low', 'high', 'volume']]
            
            df1['minute_time']= df1['minute_time'].str[0:4] +"-"+ df1['minute_time'].str[4:6] +"-"+ df1['minute_time'].str[6:8] +" "+ df1['minute_time'].str[8:10] +":"+ df1['minute_time'].str[10:12] +":"+ df1['minute_time'].str[12:14]
            
        
            
            df1 = df1[::-1]      
            
            
            """
            
                        
            df1["year"]= df1["date"].str[0:8]
            df1["time"]= df1["date"].str[8:]
            
            del df1["date"]
            
            df1 = df1[["year","time","open","high","low","close","volume"]]
            
            df1["time"] = df1["time"].str[0:4]
            
            df2 = pd.DataFrame(columns=[["year","time","open","high","low","close","volume"]])
            
                
            
            for i in range(0,len(df1),5):
                 high = max([df1["high"].loc[i],
                             df1["high"].loc[i+1],
                             df1["high"].loc[i+2],
                             df1["high"].loc[i+3],
                             df1["high"].loc[i+4]])
                 
                 low = min([df1["low"].loc[i],
                            df1["low"].loc[i+1],
                            df1["low"].loc[i+2],
                            df1["low"].loc[i+3],
                            df1["low"].loc[i+4]])
                 
                 volume = sum([df1["volume"].loc[i],
                               df1["volume"].loc[i+1],
                               df1["volume"].loc[i+2],
                               df1["volume"].loc[i+3],
                               df1["volume"].loc[i+4]])
                 
                 df2 = df2.append(pd.Series([df1["year"].loc[i],
                                            df1["time"].loc[i+4],
                                            df1["open"].loc[i],
                                            high,
                                            low, 
                                            df1["close"].loc[i+4],
                                            volume],
                                           index = df2.columns), ignore_index=True) 
            
            print(df2)
            """
            print(df1)
        
            
        
        
            
            df1.to_csv("{0}_{1}.csv".format("mini_futures",today),index=False)            
           
            
    def btn2_clicked(self): #버튼클릭시 동작
            tick_unit = self.combo.currentText()    
            kiwoom.set_input_value("종목코드", "101S3000")
            kiwoom.set_input_value("시간단위", tick_unit)
            kiwoom.comm_rq_data("OPT50029_req", "OPT50029", 0, "0101")
            
            while kiwoom.remained_data == True:
                time.sleep(TR_REQ_TIME_INTERVAL)
                kiwoom.set_input_value("종목코드", "101S3000")
                kiwoom.set_input_value("시간단위", tick_unit)
                kiwoom.comm_rq_data("OPT50029_req", "OPT50029", 2, "0101")
                
                self.text_edit.append("processing")
           
            if kiwoom.remained_data == False:
                self.text_edit.append("done")

            
            df = pd.DataFrame(kiwoom.ohlcv, columns=['date','open', 'high', 'low', 'close', 'volume'])
            
            #df1 = df[df["date"].str[0:8]==today]
            df1 = df[df["date"].str[0:4]=="2021"]
            
            
            
            df1.rename(columns={"date" : "minute_time"}, inplace=True)

            df1 = df1[['minute_time', 'open', 'close', 'low', 'high', 'volume']]
            
            df1['minute_time']= df1['minute_time'].str[0:4] +"-"+ df1['minute_time'].str[4:6] +"-"+ df1['minute_time'].str[6:8] +" "+ df1['minute_time'].str[8:10] +":"+ df1['minute_time'].str[10:12] +":"+ df1['minute_time'].str[12:14]
            
        
            
            df1 = df1[::-1]      

            print(df1)
            
            df1.to_csv("{0}_{1}.csv".format("kospi_200_futures",today),index=False)  
            
            
            
if __name__ == "__main__":
    app = QApplication(sys.argv)
    kiwoom = Kiwoom()
    kiwoom.show()
    kiwoom.ohlcv = {'date': [], 'open': [], 'high': [], 'low': [], 'close': [], 'volume': []}
    sys.exit(app.exec_())