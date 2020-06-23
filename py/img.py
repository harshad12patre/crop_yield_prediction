import PIL as pillow
from PIL import Image
import numpy as np
import matplotlib.pyplot as plt
from sklearn import svm

im=Image.open("img.jpg").convert("L")
imarr=np.array(im)
flatim=imarr.flatten('F')

clf=svm.SVC()
#X,y=im.size
X = imarr
y = np.random.randint(2, size=imarr.shape[0])
clf.fit(X, y)

#how to fit the numpy array to clf
#clf.fit(flatim[:-1],flatim[:-1])
# I HAVE NO IDEA WHAT I"M DOING HERE!
print("prediction:", clf.predict(X[-2:-1]))
plt.imshow(im,cmap=plt.cm.gray_r,interpolation='nearest')
plt.show()
