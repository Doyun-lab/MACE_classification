import numpy as np
import pandas as pd

import os

from keras.datasets import mnist
from keras.utils import np_utils
from keras.models import Sequential
from keras.layers import Dense, Conv2D, MaxPooling2D, Dropout, Flatten
from keras.callbacks import ModelCheckpoint, EarlyStopping
from keras.preprocessing.image import array_to_img, img_to_array, load_img

import tensorflow as tf
from tensorflow.keras.models import Sequential, Model
from tensorflow.keras.layers import Embedding, Dropout, Conv1D, MaxPooling1D, GlobalMaxPooling1D, GlobalAveragePooling1D, Dense, concatenate, Input, Flatten
from tensorflow.keras.callbacks import EarlyStopping, ModelCheckpoint
from tensorflow.keras.models import load_model

conv_input = Input((30, 5))
conv1 = Conv1D(32, 5, activation='relu')(conv_input)
conv2 = Conv1D(32, 5, activation='relu')(conv1)
pool1 = MaxPooling1D((3))(conv2)
conv3 = Conv1D(64, 5, activation='relu')(pool1)
conv4 = Conv1D(64, 3, activation='relu')(conv3)
pool2 = GlobalMaxPooling1D()(conv4)
dense_cnn = Dense(50, activation='relu')(pool2)

dense_input = Input((94))
dense1 = Dense(units=512, kernel_initializer='uniform', activation='relu')(dense_input)
dense2 = Dense(units=256, kernel_initializer='uniform', activation='relu')(dense1)
dense3 = Dense(units=128, kernel_initializer='uniform', activation='relu')(dense2)
dense4 = Dense(units=50, kernel_initializer='uniform', activation='relu')(dense3)
dense5 = Dense(units=25, kernel_initializer='uniform', activation='relu')(dense4)

cc1 = concatenate([dense_cnn, dense5], axis=1)
output1 = Dense(10, activation='relu')(cc1)
output2 = Dense(1, activation='sigmoid')(output1)

concat_model = Model(inputs=[conv_input, dense_input], outputs=output2)

import keras
adam = keras.optimizers.Adam(lr=0.0005, beta_1=0.9, beta_2=0.999, epsilon=None, decay=0.0, amsgrad=False)
concat_model.compile(loss='binary_crossentropy', optimizer=adam,
                     metrics=['accuracy', tf.keras.metrics.Precision(name='precision')\
                          ,tf.keras.metrics.Recall(name='recall')\
                          ,tf.keras.metrics.FalsePositives(name='false_positives')\
                          ,tf.keras.metrics.FalseNegatives(name='false_negatives')\
                          ,tf.keras.metrics.TruePositives(name='true_positives')\
                          ,tf.keras.metrics.TrueNegatives(name='true_negatives')])

concat_model_history = concat_model.fit([x_train, x_train_emr_scale], y_train, epochs = 100, validation_split=0.2)
