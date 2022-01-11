import time; import random; import datetime
import warnings
warnings.filterwarnings("ignore")


import os
import pandas as pd
import numpy as np
import sys
import matplotlib.pyplot as plt


%matplotlib inline

import contextlib
import wave 

import scipy

#!pip install librosa
import librosa
import librosa.display
from glob import glob
from scipy.io import wavfile

#pip install pydub

from pydub import AudioSegment
from pydub.silence import split_on_silence
from scipy.signal import find_peaks 


import tensorflow
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.utils import to_categorical
from tensorflow.keras.preprocessing.image import ImageDataGenerator

 

from tensorflow.keras.models import Model, Sequential
from tensorflow.keras.layers import (
    Conv2D, BatchNormalization, Dropout, MaxPooling2D, Activation,
    Flatten, Dense, Input, Concatenate, LeakyReLU, Add, AveragePooling2D, ReLU, MaxPool2D

)

from sklearn.model_selection import train_test_split
from tensorflow.keras.callbacks import LearningRateScheduler

import cv2



sig, sr = librosa.load('C:/Users/sjyoo/Desktop/code/cut_wave/00a4e142-2ae1-401e-8319-837e512b1281.wav', sr=16000)

plt.figure(figsize=(4,4))
librosa.display.waveplot(sig, sr, alpha=0.5)
plt.xlabel("Time (s)")
plt.ylabel("Amplitude")
plt.title("Waveform")

#푸리에 변환
# Fourier -> Spectrum

fft = np.fft.fft(sig)

magnitude = np.abs(fft) 

f = np.linspace(0,sr,len(magnitude))

left_spectrum = magnitude[:int(len(magnitude) / 2)]
left_f = f[:int(len(magnitude) / 2)]

plt.figure(figsize=(4,4))
plt.plot(left_f, left_spectrum)
plt.xlabel("Frequency[Hz]")
plt.ylabel("Magnitude[au]")
plt.title("Power spectrum")


#STFT
# STFT -> spectrogram

hop_length = 512
n_fft = 1024

hop_length_duration = float(hop_length) / sr
n_fft_duration = float(n_fft) / sr

stft = librosa.stft(sig, n_fft=n_fft, hop_length=hop_length)

magnitude = np.abs(stft)

log_spectrogram = librosa.amplitude_to_db(magnitude)

plt.figure(figsize=(4,4))
librosa.display.specshow(log_spectrogram, sr=sr, hop_length=hop_length)
plt.xlabel("Time")
plt.ylabel("Frequency")
plt.colorbar(format="%+2.0f dB")
plt.title("Spectrogram (dB)")


#메타 데이터 정보 가져오기
metadata = pd.read_csv('C:/Users/sjyoo/Desktop/code/metadata_compiled.csv').iloc[:,:10]
# status 결측치 삭제
#is_cough = metadata['cough_detected'] >= 0.5
#metadata_res = metadata[is_cough].reset_index(drop=True)
files = 'C:/Users/sjyoo/Desktop/code/cut_wave/' + metadata_resl['uuid'] + '.wav'



len(metadata_res)

#이동평균
def moving_average(a, n=100):
    ret = np.nancumsum(a, dtype=float)
    ret[n:] = ret[n:] - ret[:-n]
    ret = ret[n - 1:] / n
    zero_arr = [0]*(n-1)
    return np.append(zero_arr, ret)


"""
#이동 평균 모형을 이용하여 기침 소리 1주기 데이터만 plot 형태로 추출한 것
for file in files:
    sig, sr = librosa.load(path+"\\public_dataset\\"+file+".wav", sr=16000)
    
    #원본
    fig=plt.figure(figsize=(4,4))
    librosa.display.waveplot(sig, sr, alpha=0.5)
    plt.savefig('./original_plot/'+file+'.png')
    plt.close(fig)
    
    fp = find_peaks(list(sig), height=0.2)[0] #높이가 최소 0.2 이상인 값을 찾음

    fp_diff = np.diff(fp)>3000 #다음 기침은 최소 0.2초 이후에 한다 가정
    fp_diff = np.append(fp_diff, False) #차분 값의 마지막에 False 값 추가
    if sum(fp_diff)==0: fp_index = 0
    else: fp_index = list(fp[fp_diff]) #True인 값 추출 (차이가 커지기 전 인덱스 추출)
    
    #한주기
    if fp_index==0:
        fig=plt.figure(figsize=(4,4))
        librosa.display.waveplot(sig, sr, alpha=0.5)
        plt.savefig('./one_plot/'+file+'.png')
        plt.close(fig)
        
        yt,idx = librosa.effects.trim(sig, top_db = 10)
        
        fig=plt.figure(figsize=(4,4))
        librosa.display.waveplot(yt, sr, alpha=0.5)
        plt.savefig('./cut_plot/'+file+'.png')
        plt.close(fig)
        
        mv = moving_average(yt, 10)

        if sum(mv>0.2)!=0:
            mv_idx = list(mv>0.2).index(True)
            
            if len(yt[(mv_idx-100):])<1600: continue
            else:
                fig=plt.figure(figsize=(4,4))
                librosa.display.waveplot(yt[(mv_idx-100):], sr, alpha=0.5)
                plt.savefig('./mv_plot/'+file+'.png')
                plt.close(fig)
                
    else:
        fig=plt.figure(figsize=(4,4))
        librosa.display.waveplot(sig[:fp_index[0]], sr, alpha=0.5)
        plt.savefig('./one_plot/'+file+'.png')
        plt.close(fig)
        
        #cutting
        yt,idx = librosa.effects.trim(sig[:fp_index[0]], top_db = 10)
        
        fig=plt.figure(figsize=(4,4))
        librosa.display.waveplot(yt, sr, alpha=0.5)
        plt.savefig('./cut_plot/'+file+'.png')
        plt.close(fig)
        
        mv = moving_average(yt, 10)
        
        if sum(mv>0.2)!=0:

            mv_idx = list(mv>0.2).index(True)

            if len(yt[(mv_idx-100):])<1600: continue
            else:
                fig=plt.figure(figsize=(4,4))
                librosa.display.waveplot(yt[(mv_idx-100):], sr, alpha=0.5)
                plt.savefig('./mv_plot/'+file+'.png')
                plt.close(fig)
"""


cut_wave = os.listdir('C:/Users/sjyoo/Desktop/code/cut_wave/')
waves = []
for i in cut_wave:
    waves.append(i[:-4])
metadata_resl = metadata[metadata['uuid'].isin(waves)].reset_index(drop=True)

metadata_resl





files = 'C:/Users/sjyoo/Desktop/code/cut_wave/' + metadata_resl['uuid'] + '.wav'


#d이동평균으로 자른 데이터 저장
import soundfile as sf

for file in files:
    sig, sr = librosa.load(file, sr=16000)
    
    name = file.split("/")[-1]
    
    
    fp = find_peaks(list(sig), height=0.2)[0] #높이가 최소 0.2 이상인 값을 찾음

    fp_diff = np.diff(fp)>3000 #다음 기침은 최소 0.2초 이후에 한다 가정
    fp_diff = np.append(fp_diff, False) #차분 값의 마지막에 False 값 추가
    if sum(fp_diff)==0: fp_index = 0
    else: fp_index = list(fp[fp_diff]) #True인 값 추출 (차이가 커지기 전 인덱스 추출)
    
    #한주기
    if fp_index==0:
        yt,idx = librosa.effects.trim(sig, top_db = 10)
        
        mv = moving_average(yt, 10)

        if sum(mv>0.2)!=0:
            mv_idx = list(mv>0.2).index(True)
            
            if len(yt[(mv_idx-100):])<1600: continue
            else:
                sf.write('C:/Users/sjyoo/Desktop/code/cut_wave_2/'+name,
                                         yt[(mv_idx-100):], sr)
                
    else:
        yt,idx = librosa.effects.trim(sig[:fp_index[0]], top_db = 10)
        
        mv = moving_average(yt, 10)
        
        if sum(mv>0.2)!=0:
            mv_idx = list(mv>0.2).index(True)

            if len(yt[(mv_idx-100):])<1600: continue
            else:
                sf.write('C:/Users/sjyoo/Desktop/code/cut_wave_2/'+name,
                                         yt[(mv_idx-100):], sr)




def graph_spectrogram_3(wav_file, sr=16000):
    
    sig, sr = librosa.load(wav_file, sr=sr)
    n_fft = 512
    win_length = 400
    hop_length = 160
    n_mels = 80

    D = np.abs(librosa.stft(sig, n_fft=n_fft, win_length = win_length, hop_length=hop_length))
    mel_spec = librosa.feature.melspectrogram(S=D, sr=sr, n_mels=n_mels, hop_length=hop_length, win_length=win_length)
    
    fig,ax = plt.subplots(1)
    fig.subplots_adjust(left=0,right=1,bottom=0,top=1)
    ax.axis('off')
    librosa.display.specshow(librosa.amplitude_to_db(mel_spec, ref=0.00002), sr=sr, hop_length = hop_length, cmap = plt.cm.jet)
    ax.axis('off')
    plt.rcParams['figure.figsize'] = [0.75,0.5]
    fig.canvas.draw()
    size_inches  = fig.get_size_inches()
    dpi          = fig.get_dpi()
    width, height = fig.get_size_inches() * fig.get_dpi()
    
    mplimage = np.frombuffer(fig.canvas.tostring_rgb(), dtype=np.uint8)
    imarray = np.reshape(mplimage, (int(height), int(width), 3))
    plt.close(fig)
    return imarray
# rgb 이미지 불러오기
imgss = []
for idx in range(len(cut_wave)):
    imgss.append(graph_spectrogram_3('C:/Users/sjyoo/Desktop/code/cut_wave/' + cut_wave[idx]))
# COVID-19 데이터
plt.figure(figsize=(4,4))
plt.imshow(imgss[38])
plt.show()

# symptomatic 데이터
plt.figure(figsize=(4,4))
plt.imshow(imgss[0])
plt.show()

# healthy 데이터
plt.figure(figsize=(4,4))
plt.imshow(imgss[1])
plt.show()

import cv2
#imgss
#cv2.imwrite("%s.png" %("hee"),imgss[0])
##stft 채택

# 1
#img_arr = np.array(img)
# 2
#img_arr2 = np.array(imgs)
# 3
img_arr3 = np.array(imgss)

img_arr3[0].shape

#####
len(train_img)
import random

num = list(range(1,5487))
sample_index = random.sample(num, 494 )

from sklearn.model_selection import StratifiedShuffleSplit

split = StratifiedShuffleSplit(n_splits = 5 , test_size = 0.5, random_state = 0)

split.get_n_splits(train_img, label_img)

for train_index , test_index in split.split(train_img, label_img):
		print('train_index : ', train_index, 'test_index : ', test_index)


list(label_img[label_img=="healthy"]).index

#라벨데이터에서 랜덤 494개만 뽑아서 사용
label_df = pd.DataFrame(label_img)

heal_df = label_df[label_df[0] == 'healthy']
covid_df = label_df[label_df[0] == "COVID-19"]

heal_list = heal_df.sample(n=1000).index.tolist()
covid_list = covid_df.index.tolist()

label_img[heal_list]
label_img[covid_list]

train_img[heal_list]
train_img[covid_list]


label_img_real = np.concatenate((label_img[heal_list], label_img[covid_list]))
train_img_real = np.concatenate((train_img[heal_list], train_img[covid_list]))

len(train_img_real)
len(label_img_real)



#train test
nan_remove_index = metadata_resl[metadata_resl['status'].notnull()].index.tolist()

#symptomatic 제거
img_arr3_nan_remove = img_arr3[nan_remove_index]
metadata_resl_nan_remove = metadata_resl['status'][nan_remove_index]

metadata_resl_nan_remove = metadata_resl_nan_remove.reset_index()

symptom_remove_index = metadata_resl_nan_remove [metadata_resl_nan_remove ['status'] != "symptomatic"].index.tolist()


train_img = img_arr3_nan_remove[symptom_remove_index]
label_img = np.array(metadata_resl_nan_remove['status'][symptom_remove_index])

len(train_img)
len(label_img)


import collections, numpy
collections.Counter(label_img_real)

"""
for i in range(len(train_img)):
    cv2.imwrite("%s %d.png" %(label_img[i],i), train_img[i])

"""


# Train data 80% : test data 20%로 나눕니다.
X_train, X_test, y_train, y_test = train_test_split(train_img_real , label_img_real , test_size=0.3, shuffle=True)


X_train = X_train.astype('float32') / 255.
X_test = X_test.astype('float32') / 255.

X_train[0]

y_train = pd.get_dummies(y_train)
y_test = pd.get_dummies(y_test)
#metadata_resl['status'][nan_remove_index]


#print(np.array(metadata_resl['status'][nan_remove_index]))
print(y_train)
print(y_test)

np.argmax(y_train)

len(y_train)
len(y_test)

y_train = np.array(y_train)
y_test = np.array(y_test)

plt.figure(figsize=(4,4))
#plt.bar(metadata_resl['status'][nan_remove_index].value_counts().index, metadata_resl['status'][nan_remove_index].value_counts())
plt.bar(metadata_resl_nan_remove['status'][symptom_remove_index].value_counts().index, metadata_resl_nan_remove['status'][symptom_remove_index].value_counts())
plt.show()

metadata_resl_nan_remove['status'][symptom_remove_index].value_counts()


import tensorflow as tf
from tensorflow import keras
from keras import backend as K

import keras.backend as K

model = Sequential()
model.add(Conv2D(32, (3, 3), padding='same',
                 input_shape=(36,54,3)))
model.add(Activation('relu'))
model.add(Conv2D(64, (3, 3)))
model.add(Activation('relu'))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(BatchNormalization())
model.add(Conv2D(64, (3, 3), padding='same'))
model.add(Activation('relu'))
model.add(Conv2D(64, (3, 3)))
model.add(Activation('relu'))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.5))
model.add(Conv2D(128, (3, 3), padding='same'))
model.add(Activation('relu'))
model.add(Conv2D(128, (3, 3)))
model.add(Activation('relu'))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(BatchNormalization())
model.add(Flatten())
model.add(Dense(512))
model.add(Activation('relu'))

model.add(Dense(2, activation='sigmoid'))
model.summary() 

opt = keras.optimizers.Adam(lr=0.001)
model.compile(loss='binary_crossentropy', optimizer=opt, metrics=['accuracy',
                                                                      tensorflow.keras.metrics.FalseNegatives(name='false_negatives')])
history = model.fit(X_train, y_train, batch_size=64, epochs=8, verbose=1, validation_split=0.2)



a = [0.7360287261009216,
 0.7339712738990784,
 0.7452631735801697,
 0.7320095753669739,
 0.7819138813018799,
 0.8087559771537781,
 0.8026794338226318,
 0.8244019269943237]

b = [0.6342105388641357,
 0.6846411323547363,
 0.6898564887046814,
 0.6642105388641357,
 0.712105388641357,
 0.72342105388641357,
 0.72242105388641357,
 0.7142105388641357]



plt.figure(figsize=(7,7))
plt.plot(a)
plt.plot(b)
plt.legend(['training', 'validation'], loc = 'upper left')
plt.show()



plt.figure(figsize=(7,7))
plt.plot(history.history['accuracy'])
plt.plot(history.history['val_accuracy'])
plt.legend(['training', 'validation'], loc = 'upper left')
plt.show()



plt.figure(figsize=(7,7))
plt.plot(history.history['false_negatives'])
plt.plot(history.history['val_false_negatives'])
plt.legend(['training', 'validation'], loc = 'upper left')
plt.show()


#CLASSES = np.array(['COVID-19','healthy','symptomatic'])
CLASSES = np.array(['COVID-19','healthy'])

pred = model.predict(X_test)
pred_single = CLASSES[np.argmax(pred, axis=-1)]
actual_single = CLASSES[np.argmax(y_test, axis=-1)]
resultss = pd.DataFrame(pred_single,columns=['aa'])
plt.figure(figsize=(4,4))
plt.bar(resultss['aa'].value_counts().index, resultss['aa'].value_counts())
plt.show()


resultss['aa'].value_counts()

actual_single
   
from sklearn.metrics import confusion_matrix
#confusion_matrix(actual_single,pred_single,labels=['COVID-19','healthy','symptomatic'])
confusion_matrix(actual_single,pred_single,labels=['COVID-19','healthy'])


#ROC
from sklearn.metrics import roc_curve

pred_prob = model.predict_proba(X_test)



def plot_roc_curve(fper, tper):
    plt.figure(figsize=(7,7))
    plt.plot(fper, tper, color='red', label='ROC')
    plt.plot([0, 1], [0, 1], color='green', linestyle='--')
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('ROC Curve')
    plt.legend()
    plt.show()

x = pred_prob[:,1]
y = np.argmax(y_test, axis=-1)

fper, tper, thresholds = roc_curve(np.argmax(y_test, axis=-1),pred_prob[:,1])
fper, tper, thresholds = roc_curve(y,x)
plot_roc_curve(fper, tper)

AUC_TPR = [0] + tper
AUC_FPR = [0] + fper

AUC = 0
for i in range(1, len(AUC_TPR)):
    tmp_AUC = (AUC_TPR[i - 1] + AUC_TPR[i]) * (AUC_FPR[i] - AUC_FPR[i - 1]) / 2
    AUC += tmp_AUC
AUC


train_pred = model.predict(X_train)
train_pred_single = CLASSES[np.argmax(train_pred, axis=-1)]
train_actual_single = CLASSES[np.argmax(y_train, axis=-1)]
train_result = pd.DataFrame(pred_single,columns=['aa'])
plt.figure(figsize=(4,4))
plt.bar(train_result['aa'].value_counts().index, train_result['aa'].value_counts())
plt.show()


from sklearn.metrics import confusion_matrix
confusion_matrix(train_actual_single,train_pred_single,labels=['COVID-19','healthy','symptomatic'])

X_train = np.array(X_train)
y_test = np.array(y_test)




len(X_train)
len(y_train)

X_train[[[[1]]]]
y_train.shape
