G = 9.81
pi = 3.141592653589793

deg2Rad(x) = x * pi / 180
rad2Deg(x) = x * 180 / pi

slopedNaturalForce(fg, pitch) = fg * cos(pitch)
slopedDownForce   (fg, pitch) = fg * sin(pitch)
slopedGravityForce(fn, pitch) = fn / cos(pitch)