import tensorflow as tf

def ssim_L1_loss(y_true, y_pred):
    ssim_loss=1 - tf.reduce_mean(tf.image.ssim(y_true, y_pred, 1.0))
    L1_loss=tf.keras.losses.MeanAbsoluteError()
    return ssim_loss*0.84 + (1-0.84)*L1_loss(y_true, y_pred)
