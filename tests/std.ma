G = 9.81
pi = 3.141592653589793

# Single line comment
deg2Rad(x) = x * pi / 180
rad2Deg(x) = x * 180 / pi

slopedNaturalForce(fg, degPitch) = {
    radPitch = deg2Rad(degPitch);
    fg * cos(radPitch)
}
slopedDownForce   (fg, degPitch) = {
    radPitch = deg2Rad(degPitch);
    fg * sin(radPitch)
}
slopedGravityForce(fn, degPitch) = {
    radPitch = deg2Rad(degPitch);
    fn / cos(radPitch)
}    