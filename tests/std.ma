G = 9.81
pi = 3.141592653589793

deg2Rad(x) = x * pi / 180
rad2Deg(x) = x * 180 / pi

slopedNaturalForce(fg, degPitch) = fg * cos(deg2Rad(degPitch))
slopedDownForce   (fg, degPitch) = fg * sin(deg2Rad(degPitch))
slopedGravityForce(fn, degPitch) = fn / cos(deg2Rad(degPitch))