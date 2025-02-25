Im = OpenImageR::readImage('Training/1AT.jpg')
dim(Im)

red = Im[,,1]
green = Im[,,2]
blue = Im[,,3]
OpenImageR::imageShow(Im)
OpenImageR::imageShow(red)
OpenImageR::imageShow(blue)
OpenImageR::imageShow(green)


# We need to concatenate this in one long verctor

youhavetodo = c(as.vector(red), as.vector(green), as.vector(blue))

m = cbind(as.vector(red), as.vector(green), as.vector(blue))
dim(m)

out = princomp(m)

out$scores #PC


pc1 = matrix(out$scores[,1], nrow=nrow(red), ncol=ncol(red))  
OpenImageR::imageShow(pc1)


pc2 = matrix(out$scores[,2], nrow=nrow(red), ncol=ncol(green))  # Da igual el color.Por eso pone uno y luego otro.
OpenImageR::imageShow(pc2)

pc3 = matrix(out$scores[,3], nrow=nrow(red), ncol=ncol(red))  
OpenImageR::imageShow(pc3)

out$sdev^2/sum(out$sdev^2)

