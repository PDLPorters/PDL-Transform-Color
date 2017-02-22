=head1 NAME

PDL::Transform::Color - Useful color system conversions for PDL

=head1 SYNOPSIS

     ### Shrink an RGB image with proper linear interpolation:
     ### DEcode the sRGB image values, then interpolate, then ENcode sRGB
     $im = rpic("big_colorimage.jpg");
     $im2 = $im->invert(t_srgb())->match([500,500],{m=>'g'})->apply(t_srgb());

=head1 DESCRIPTION

PDL::Transform::Color includes a variety of useful color conversion
transformations.  It can be used for simple hacks on machine-native
color representations (RGB <-> HSV, etc.) or for more sophisticated
manipulation of absolute color standards including large-gamut or
perceptual systems.

If you aren't familiar with PDL::Transform, you should read that POD
now, as this is a subclass of PDL::Transform.  Transforms represent
and encapsulate vector transformations -- one- or two-way vector
functions that may be applied, composed, or (if possible) inverted.
They are created through constructor methods that often allow
parametric adjustment at creation time.  A Transform object may be
applied to vectors, or used to resample a scientific data set.

The color transforms in this module can be used for converting between
proper color systems, for gamma-converting pixel values, or for
generating (or interpreting) pseudocolor from one or two input
parameters.

The module uses linearized sRGB (lsRGB) as a fundamental color basis.
sRGB is the standard color system used by most consumer- to mid-grade
computer equipment, so casual users can use simple RGB without
much regard for gamuts, colorimetric standards, etc.

Most of the transform generators convert from lsRGB to various
other systems.  Notable simple ones are HSV (Hue, Saturation, Value),
HSL (Hue, Saturation, Lightness), and CMYK (Cyan, Magenta, Yellow,
blacK).

If you just want to "manipulate some RGB images" and not learn about
the esoterica of color representations, you can treat all the routines
as working "from RGB" on the interval [0,1], and use C<t_srgb> to
export images or colors to "24-bit color" that your computer probably
expects.  If you care about the esoterica, read on.

The output transfer function for sRGB is nonlinear -- the luminance of
a pixel on-screen varies slightly faster than the square of the input
value -- which is inconvenient for blending, merging, and manipulating
color.  So the internal model is a floating-point linear system
representing pixels as 3-vectors of the sRGB primary color
brightnesses (lsRGB).  

Note that, in general, RGB representations are limited to a particular
narrow gamut of physically accessible values, because it is not
possible for light to have a negative intensity. Therefore no trio of
physical primary colors, which form a basis for an RGB color space,
can represent every perceivable color.  But in digital representation,
there is no hard limit on the values of the RGB vectors -- they can be
negative or arbitrarily large.  This permits representation of
out-of-gamut values using negative or over-unity intensities.  So
floating-point lsRGB allows you to represent literally any color value
that the human eye can perceive, and many that it can't.  This is
useful even though many such colors can't be rendered on a monitor.
For example, you can change between several color representations and
not be limited by the formal gamut of each representation -- only by
the final export standard.

Three major output formats are supported: sRGB (standard "24-bit
color" with the industry standard transfer function); bRGB (bytescaled
RGB with a controllable gamma function (default 2.2, matching most
CRTs and calibrated flat monitors); or CMYK (direct linear inversion
of the RGB values, with byte scaling). These are achieved by applying
the transforms C<t_srgb>, C<t_brgb>, and C<t_cmyk>, respectively.

The C<t_srgb> export routine will translate represented colors in
floating-point lsRGB to byte-encoded sRGB (or, if inverted, vice
versa), using the correct (slightly more complicated than gamma
functions) nonlinear scaling.  In general, you can use C<!t_srgb> to
import existing images you may have found lying around the net;
manipulate their hue, etc.; and re-export with C<t_srgb>.

If you prefer to work with direct gamma functions or straight 
scaling, you can import/export from/to byte values with C<t_brgb> 
instead.  For example, to export a color in the CIE RGB system 
(different primaries than sRGB), use C<t_brgb() x t_ciergb>.

There are also some pseudocolor transformations, which convert a
single data value to normalized RGB.


=head1 OVERVIEW OF COLOR THEORY

Beacuse of the biophysics of the human eye, color is well represented
as a 3-vector of red, green, and blue brightness values representing
brightness in the long, middle, and short portions of the visible
spectrum.  However, the absorption/sensitivity bands overlap
significantly, therefore no physical light (of any wavelength) can
form a proper "primary color" (orthonormal basis element) of this
space.  While any vector in color space can be represented as a linear
sum of three indepenent basis vectors ("primary colors"), there is no
such thing as a negative intensity and therefore any tricolor
representation of the color space is limited to a "gamut" that can be
formed by E<positive> linear combinations of the primaries.

RGB color representations therefore require the specification of 
particular primary colors, and the choice depends on the technical
solution.  The most "standard" representation is the CIE RGB standard
developed in 1931, which uses primary wavelengths of 700nm (red), 
546.1 nm (green), and 435.8 nm (blue). The C<PDL::Transform::Color>
transformations are all relative to this RGB basis.  Negative values
are permitted, allowing representation of all colors -- possible or 
impossible.

CIE defined several other important color systems: first, an XYZ
system based on nonphysical primaries X, Y, and Z that approximate R,
G, and B.  The XYZ gamut extends to all colors detectable to the human
eye, at the cost of including many impossible ones.  The Y primary
of this system corresponds closely to green, and is used by CIE as a 
proxy for overall luminance.  

The CIE also separated "chrominance" and "luminance" signals, in a 
separate system called "xyY", which represente color as sum-normalized
vectors "x=X/(X+Y+Z), "y=Y/(X+Y+Z)", and "z=Z/(X+Y+Z)".  By construction,
x+y+z=1, so "x" and "y" alone describe the color gamut of the system, and
"Y" stands in for overall luminance.

Many other representations exist that separate chromatic value from
brightness.  In general, these can be divided into polar coordinates
that represent hue as a single value divorced from the rgb basis, and
those that represent it as a combination of two values like the 'x'
and 'y' of the CIE xyY space.  These are all based on the Munsell and
Ostwald color systems, which were worked out at about the same time
as the CIE system.

Modern display devices generally produce physical brightnesses that
are proportional not to their input signal, but to a nonlinear
function of the input signal.  The most common nonlinear function is a
simple power law ("gamma function"): output is approximately
proportional to the "gamma" power of the input.  Raising a signal
value to the power "1/gamma" is C<gamma-encoding> it, and raising it
to the power "gamma" is C<gamma-decoding> it.

The sRGB 24-bit color standard specifies a slightly more complicated
transfer curve, that consists of a linear segment spliced onto a
horizontally-offset power law with gamma=2.4.  This reduces
quantization noise for very dark pxels, but approximates an overall
power law with gamma=2.2.  Hence, C<t_brgb> (which supports general
power law transfer functions) defaults to an output gamma of 2.2, but
C<t_srgb> yields a more accurate export transfer in typical use.

A linear RGB system is specified exactly by the chrominance (CIE XYZ or 
xyY) coordinates of the three primaries, and a white point chrominance.  
Real RGB systems typically use dynamic range compression via a nonlinear
transfer function which is most typically a gamma function.   A 
built-in database tracks about 15 named systems, which can be requested
by name -- or you can specify your own with a standard hash format
(see C<get_rgb>). 

Provision exists for converting between different linearized RGB
systems with different primaries and different white points.

=head1 STANDARD OPTIONS

=over 3

=item gamma

This is a gamma correction factor used to get physical values from the
represented RGB values in the RGB space.  Most color manipulation is
performed gamma=1 space -- i.e. if you specify a gamma to a conversion
transform, the normalized RGB values are decoded to linear physical
values before processing in the forward direction, or encoded after
processing in the reverse direction.

For example, to square the normalized floating-point linear sRGB
values before conversion to bRGB, use C<t_brgb(gamma=>2)>.  The
"gamma" option specifies that the desired brightness of the output device
varies as the square of the pixel value in the stored data.   

Contrariwise, the C<t_brgb> export transform has a C<display_gamma> option
that specifies the gamma function for the output bytes.  Therefore, 
C<t_brgb(display_gamma=>2)> square-roots the data before export (so that
squaring them yields the desired brightness). 

The C<gamma> option is probably not necessary for most
transformations: it's best to work in a linear space and encode/decode
to represented values for import/export.  For example, generic images
found on the internet are typically in the sRGB system, and can be
imported to lsRGB via the C<!t_srgb> transform or exported with C<t_srgb> -- 
or other gamma-corrected 24-bit color systems can be created directly
with C<t_brgb> and its C<display_gamma> option.

=back

=head1 AUTHOR

Copyright 2017, Craig DeForest (deforest@boulder.swri.edu).  This
module may be modified and distributed under the same terms as PDL
itself.  The module comes with NO WARRANTY.

=head1 FUNCTIONS

=cut

use PDL::Transform;

package PDL::Transform::Color;

use PDL::Core ':Internal';  # load "topdl" (internal routine)

@ISA = ( 'Exporter', 'PDL::Transform' );
our $VERSION = '0.1';
$VERSION = eval $VERSION;

BEGIN {
    package PDL::Transform::Color;
    use base 'Exporter';
    @EXPORT_OK = qw/ t_gamma t_brgb t_srgb t_shift_illuminant t_shift_rgb t_cmyk t_rgi t_cieXYZ t_xyz t_xyY t_xyy t_lab t_xyz2lab t_hsl t_hsv /;
    @EXPORT = @EXPORT_OK;
    %EXPORT_TAGS = (Func=>[@EXPORT_OK]);
};

use strict;
use PDL;
use PDL::Transform;
use PDL::MatrixOps;
use PDL::Options;
use PDL::NiceSlice;

use Carp;

our $PI = $PDL::Transform::PI;
our $DEG2RAD = $PDL::Transform::DEG2RAD;
our $RAD2DEG = $PDL::Transform::RAD2DEG;


# Some matrix values of use in RGB conversions...

# Matrix to convert CIE RGB to CIE XYZ
our($crgb2cxyz_mat) =
    pdl(  [0.49000, 0.31000, 0.20000], 
	  [0.17697, 0.81240, 0.01063],
	  [0.00000, 0.01000, 0.99000]
      ) / 0.17697;
our($crgb2ciexyz_inv) = $crgb2cxyz_mat->inv;

# Matrix to convert CIE XYZ to sRGB
our($srgb2cxyz_inv) =
    pdl( [ 3.2410, -1.5374, -0.4986],
	 [-0.9692,  1.8760,  0.0416],
	 [ 0.0556, -0.2040,  1.0570]
    );
our($srgb2cxyz_mat) = $srgb2cxyz_inv->inv;


sub _strval {
    my($me) = shift;
    $me->stringify();
}

sub _new { new('PDL::Transform::Color',@_) }

sub new {
    my($class) = shift;
    my($parse) = pop;
    my($name) = pop;
    my($me) = PDL::Transform::new($class);
    $me->{name} = $name;
    $me->{u_opt} = {@_};
    $me->{idim} = 3;
    $me->{odim} = 3;
    
    my %opt = parse($parse, $me->{u_opt});
    $me->{params} = \%opt;
    
    return $me;
}


## Compose with gamma correction if necessary
sub gammify {
    my $me = shift;

    if( exists($me->{params}->{gamma}) && 
	defined($me->{params}->{gamma}) && 
	$me->{params}->{gamma} != 1 ) {

	# Decode gamma from source
	return ( $me x t_gamma($me->{params}->{gamma}) );
 
   } else {

	return $me;

    }
}

##############################

=head2 PDL::Transform::Color::t_gamma 

=for usage

    $t = PDL::Transform::Color::t_gamma($gamma);

=for ref

This is an internal generator that is used to implement the standard
C<gamma> parameter for all color transforms.

In the forward direction, C<t_gamma> applies the gamma correction
indicated -- e.g. if the C<$gamma> parameter at generation time is 2,
then the forward direction squares its input, and in reverse direciton
takes the square root.

Gamma correction is implemented using a sign-tolerant approach: 
negative-going values get the same power-law curve applied, but in the 
negative direction.

=cut

sub t_gamma {
    my $gamma = shift;
    my ($me) = _new("gamma",{});

    $me->{params} = {gamma=>$gamma};
    $me->{name} .= sprintf("=%g",$gamma);
    $me->{idim} = 3;
    $me->{odim} = 3;

    $me->{func} = sub {
	my ($in, $opt) = @_;
	my $out = $in->new_or_inplace;
	if($opt->{gamma} != 1) {
	    $out *= ($in->abs + ($in==0)) ** ($opt->{gamma}-1);
	}
	$out;
    };

    $me->{inv} = sub {
	my ($in, $opt) = @_;
	my $out = $in->new_or_inplace;
	if($opt->{gamma} != 1) {
	    $out *= ($in->abs + ($in==0)) ** (1.0/$opt->{gamma} - 1);
	}
    };
	
    $me;
}

##############################

=head2 t_brgb

=for usage

    $t = t_brgb();

=for ref

Convert normalized sRGB (closed domain: 0 to 1) to byte-scaled RGB
(semi-open domain: 0 inclusive to 256 exclusive, floating point).  By
default, C<t_brgb> prepares byte values tuned for a display gamma of
2.2, which approximates sRGB (the standard output color coding for
most computer displays).  The difference between C<t_brgb> and
C<t_srgb> is that C<t_srgb> uses the actual spliced-curve
approximation specified in the sRGB standard, while C<t_brgb> uses a
simple gamma law for export.

C<t_brgb> accepts the following options, all of which may be abbreviated:

=over 3

=item gamma (default 1)

If set, this is a gamma-encoding value for the original lsRGB, which 
is decoded before the transform.

=item display_gamma (default 2.2)

If set, this is the gamma of the display for which the output is
intended.  The default compresses the brightness vector before output
(taking approximately the square root).  This matches the "standard
gamma" applied by MacOS and Microsoft Windows displays, and approximates
the sRGB standard.  See also C<t_srgb>.

=item clip (default 1)

If set, the output is clipped to [0,256) in the forward direction and 
to [0,1] in the reverse direction.

=item byte (default 1)

If set, the output is converted to byte type in the forward direction.
This is a non-reversible operation, because precision is lost in the
conversion to bytes. (The reverse transform always creates a floating
point value, since lsRGB exists on the interval [0,1] and an integer
type would be useless.)

=back

=cut

sub t_brgb {
    my($me) = _new(@_,'encode bytescaled RGB',
		   {clip=>1,
		    byte=>1,
		    gamma=>1.0,
		    display_gamma=>2.2,
		   }
	);

    $me->{func} = sub {
	my($in, $opt) = @_;
	my $out = $in->new_or_inplace;

	if($opt->{display_gamma} != 1) {
	    $out *= ($out->abs)**(1.0/$opt->{display_gamma} - 1);
	}
	
	$out *= 255.0;

	if($opt->{byte}) {
	    $out = byte($out->rint->clip(0,255));
	} elsif($opt->{clip}) {
	    $out->inplace->clip(0,255.49999);
	} 

	$out;
    };

    $me->{inv} = sub {
	my($in,$opt) = @_;

	my $out = $in / 255.0;

	if($opt->{display_gamma} != 1) {
	    $out *= ($out->abs)**($opt->{display_gamma}-1);
	}

	if($opt->{clip}) {
	    $out->inplace->clip(0,1);
	}
	$out;
    };
    
    return gammify($me);
}

=head2 t_srgb 

=for ref

Converts linearized sRGB (the internal base representation) to sRGB -
the typical RGB encoding used by most computing devices.  Since most
computer terminals use sRGB, the representation's gamut is well matched
to most computer monitors.

sRGB is a spliced standard, rather having a direct gamma correction.
Hence there is no way to adjust the output gamma.  If you want to do 
that, use C<t_brgb> instead.

C<t_srgb> accepts the following options, all of which may be abbreviated:

=over 3

=item gamma (default 1)

If set, this is a gamma-encoding value for the original lsRGB, which 
is decoded before the transform.

=item byte (default 1) 

If set, this causes the output to be clipped to the range [0,255] and rounded
to a byte type PDL ("24-bit color").  (The reverse transform always creates 
a floating point value, since lsRGB exists on the interval [0,1] and an integer
type would be useless.)

=item clip (default 0)

If set, this causes output to be clipped to the range [0,255] even if the 
C<byte> option is not set.

=back

=cut
  
sub t_srgb {
    my($me) = _new(@_,'encode 24-bit sRGB',
		   {clip=>0,
		    byte=>1,
		    gamma=>1.0
		   }
	);
    $me->{func} = sub {
	my($in,$opt) = @_;
	
	# Convert from CIE RGB to sRGB primaries
	my($rgb) = $in->new_or_inplace();

	# Slow and lame -- would work far better as a pp routine...
	$rgb .= 
	    (  ($in <= 0.00304) * 12.92 * $rgb ) +
	    (  ($in  > 0.00304) * (
		   (  1.055 * ($in * ($in->abs ** (1.0/2.4 - 1) ) ) )  - 0.055
	       )
	    );

	my $out;

	$rgb *= 255;
	if($opt->{byte}) {
	    $out = byte( $rgb->rint->clip(0,255) );
	} elsif($opt->{clip}) {
	    $out = $rgb->clip(0,255.49999);
	} else {
	    $out = $rgb;
	}

	$out;
    };

    $me->{inv} = sub {
	my($in,$opt) = @_;
	
	my $rgb = $in / pdl(255.0);

	my $rgb2 = ($rgb+0.055)/1.055;
	$rgb .=
	    ( ($rgb <= 0.03928) * $rgb / 12.92 ) +
	    ( $rgb   > 0.03928) * (
		$rgb2 * (($rgb2->abs)**1.4)
	    );

	$rgb;
    };

    return gammify($me);
}

##############################
# Reference illuminants
# (aka "white points")

=head2 PDL::Transform::Color::xyy_from_D

=usage

     $xyy = PDL::Transform::Color::xyy_from_D($D_value)

=for ref

This utility routine generates CIE xyY system colorimetric values for
standard CIE illuminants.  The illuminants are calculated from a
standard formula and correspond to black body temperatures between
4,000K and 250,000K.  The D value is the temperature in K divided by
100, e.g. broad daylight is D65, corresponding to 6500 Kelvin.

This is used for calculating standard reference illuminants, to convert
RGB values between illuminants.  

For example, sRGB uses a D65 illuminant, but many other color standards
refer to a D50 illuminant.

The colorimetric values are xy only; the Y coordinate can be specified via
an option, or defaults to 0.5.

This routine is mainly used by C<xyy_from_illuminant>, which handles most
of the CIE-recognized standard illuminant sources including the D's.

See C<t_xyy> for a description of the CIE xyY absolute colorimetric system.

C<xyy_from_D> accepts the following options:

=over 3 

=item Y - the Y value of the output xyY coordinate 

=back

=cut
    
sub xyy_from_D {
    my $D = pdl(shift);
    my $u_opt = shift || {};
    my %opt = parse({
	Y=>1
		    },
	$u_opt);

    die "cie_xy_from_D: D must be between 40 and 250" if(any($D< 40) || any($D > 250));
    my $T = $D*100;

    my $Xd;
    $Xd = ($D<=70) * ( 0.244063 + 0.09911e3/$T + 2.9678e6/$T/$T - 4.6070e9/$T/$T/$T ) +
	  ($D> 70) * ( 0.237040 + 0.24748e3/$T + 1.9018e6/$T/$T - 2.0064e9/$T/$T/$T );

    return pdl( $Xd, -3*$Xd*$Xd + 2.870*$Xd - 0.275, $opt{Y} )->mv(-1,0)->sever;
}

# xy data for FL3.x standards, from CIE "Colorimetry" 3rd edition Table T.8.2
my $fl3tab = [
    [],
    [0.4407, 0.4033],
    [0.3808, 0.3734],
    [0.3153, 0.3439],
    [0.4429, 0.4043],
    [0.3749, 0.3672],
    [0.3488, 0.3600],
    [0.4384, 0.4045],
    [0.3820, 0.3832],
    [0.3499, 0.3591],
    [0.3455, 0.3460],
    [0.3245, 0.3434],
    [0.4377, 0.4037],
    [0.3830, 0.3724],
    [0.3447, 0.3609],
    [0.3127, 0.3288]
    ];
# xy data for FLx standards, from CIE "Colorimetry" 3rd edition Table T.7
my $fltab = [
    [],
    [0.3131, 0.3371],
    [0.3721, 0.3751],
    [0.4091, 0.3941],
    [0.4402, 0.4031],
    [0.3138, 0.3452],
    [0.3779, 0.3882],
    [0.3129, 0.3292],
    [0.3458, 0.3586],
    [0.3741, 0.3727],
    [0.3458, 0.3588],
    [0.3805, 0.3769],
    [0.4370, 0.4042]
    ];
# xy data for HPx standards, from CIE "Colorimetry" 3rd edition table T.9
my $hptab = [
    [],
    [0.5330, 0.4150],
    [0.4778, 0.4158],
    [0.4302, 0.4075],
    [0.3812, 0.3797],
    [0.3776, 0.3713]
    ];
    


=head2 PDL::Transform::Color::xyy_from_illuminant

=usage

     $xyy = PDL::Transform::Color::xyy_from_illuminant($name)

=for ref

This utility routine generates CIE xyY system colorimetric values for
standard CIE illuminants.  The illuminants are looked up in a table
populated from the CIE publication E<Colorimatry>, 3rd edition.

The illuminant of a system is equivalent to its white point -- it is
the location in xyY absolute colorimetric space that corresponds to
"white".  

CIE recognizes many standard illuminants, and (as of 2017) is in the
process of creating a new set -- the "L" series illuminants -- that is 
meant to represent LED lighting.

Proper treatment of an illuminant requires a full spectral representation,
which the CIE specifies for each illuminant.  Analysis of that spectrum is 
a major part of what CIE calls "Color rendering index (CRI)" for a particular
light source.  PDL::Transform::Color is a strictly tri-coordinate system
and does not handle the nuances of spectral effects on CRI.  In effect, 
all illuminants are treated as having a CRI of unity (perfect).

Illuminants that are understood are:

=over 3

=item * a 3-PDL in CIE xyY coordinates

=item * a CIE standard name

=back

The CIE names are:

=over 3

=item A - a gas-filled tungsten filament lamp at 2856K

=item B - not supported (deprecated by CIE)

=item C - early daylight simulant, replaced by the D<n> sources

=item D[n] - Blackbody radiation at 100[n] Kelvin (e.g. D65)

=item F[n] - Fluorescent lights of various types (n=1-12 or 3.1-3.15)

=item HP[n] - High Pressure discharge lamps (n=1-5) 

=item L[n] - LED lighting (not yet supported)

=back

=cut

sub xyy_from_illuminant {
    my $name = shift;
    if(UNIVERSAL::isa($name,"PDL")) {
	if(($name->nelem==2 || $name->nelem==3) && $name->dim(0)==$name->nelem) {
	    return $name;
	} else {
	    die "xyy_from_illuminant:  PDL must be a 2-PDL or a 3-PDL";
	}
    }
    my $u_opt = shift || {};
    my %opt = parse({ 
	Y=>1
		    }, $u_opt);
    if($name =~ m/^A/i) {
	return pdl(0.44758, 0.40745, $opt{Y});
    } elsif($name =~ m/^B/) {
	die "Illuminant B is not supported (deprecated by CIE)";
    } elsif($name =~ m/^C/) {
	return pdl(0.31006, 0.31616, $opt{Y});
    } elsif( $name =~ m/^D(.*)$/i) {
	return xyy_from_D($1,$u_opt);
    } elsif( $name =~ m/^E/i) {
	return pdl(0.33333,0.33333,$opt{Y});
    } elsif( $name =~ m/^FL?([\d+])(\.[\d])?$/i) {
	my $flno = $1+0;
	my $flsubno = $2+0;
	die "Illuminant $name not recognized (FL1-FL12, or FL3.1-FL3.15)"
	    if($flno < 1 || $flno > 12 || 
	       ($flsubno && $flno != 3) ||
	       ($flsubno > 15)
	    );

	if($flno==3 && $flsubno) {
	    return pdl(@{$fl3tab->[$flsubno]},$opt{Y});
	} else {
	    return pdl(@{$fltab->[$flno]},$opt{Y});
	}
    } elsif( $name =~ m/^HP?(\d)/i ) {
	my $hpno = $1+0;
	die "Unknown HP illuminant no. $hpno" if($hpno<1 || $hpno > 5);
	return pdl(@{$hptab->[$hpno]}, $opt{Y});
    } elsif( $name =~ m/^L/i) {
	die "Illuminant L is not (yet) supported";
    } else {
	die "Unknown illuminant $name";
    }
}


##############################
# Database of standard RGB color systems from Bruce Lindbloom
# Make a database of xyY values of primaries, illuminants, and standard gammas for common RGB systems
# Also stash matrices for converting those systems to lsRGB.
#
# Columns:  gamma, illuminant, xyY for R (3 cols), xyY for G (3 cols), xyY for B (3 cols), abbrev char count
our $rgbtab_src = {
    "Adobe"        => [2.2, "D65", 0.6400, 0.3300, 0.297361, 0.2100, 0.7100, 0.627355, 0.1500, 0.0600, 0.075285, 2],
    "Apple"        => [1.8, "D65", 0.6250, 0.3400, 0.244634, 0.2800, 0.5950, 0.672034, 0.1550, 0.0700, 0.083332, 2],
    "Best"         => [2.2, "D50", 0.7347, 0.2653, 0.228457, 0.2150, 0.7750, 0.737352, 0.1300, 0.0350, 0.034191, 3],
    "Beta"         => [2.2, "D50", 0.6888, 0.3112, 0.303273, 0.1986, 0.7551, 0.663786, 0.1265, 0.0352, 0.032941, 3],
    "Bruce"        => [2.2, "D65", 0.6400, 0.3300, 0.240995, 0.2800, 0.6500, 0.683554, 0.1500, 0.0600, 0.075452, 2],
    "BT 601"       => [2.2, "D65", 0.6300, 0.3400, 0.299000, 0.3100, 0.5950, 0.587000, 0.1550, 0.0700, 0.114000, 3],
    "BT 709"       => [2.2, "D65", 0.6300, 0.3400, 0.212600, 0.3100, 0.5950, 0.715200, 0.1550, 0.0700, 0.072200, 3],
    "CIE"          => [2.2, "E",   0.7350, 0.2650, 0.176204, 0.2740, 0.7170, 0.812985, 0.1670, 0.0090, 0.010811, 2],
    "ColorMatch"   => [1.8, "D50", 0.6300, 0.3400, 0.274884, 0.2950, 0.6050, 0.658132, 0.1500, 0.0750, 0.066985, 2],
    "Don 4"        => [2.2, "D50", 0.6960, 0.3000, 0.278350, 0.2150, 0.7650, 0.687970, 0.1300, 0.0350, 0.033680, 1],
    "ECI v2"       => [1.0, "D50", 0.6700, 0.3300, 0.320250, 0.2100, 0.7100, 0.602071, 0.1400, 0.0800, 0.077679, 2],
    "Ekta PS5"     => [2.2, "D50", 0.6950, 0.3050, 0.260629, 0.2600, 0.7000, 0.734946, 0.1100, 0.0050, 0.004425, 2],
    "NTSC"         => [2.2, "C",   0.6700, 0.3300, 0.298839, 0.2100, 0.7100, 0.586811, 0.1400, 0.0800, 0.114350, 1],
    "PAL"          => [2.2, "D65", 0.6400, 0.3300, 0.222021, 0.2900, 0.6000, 0.706645, 0.1500, 0.0600, 0.071334, 2],
    "ProPhoto"     => [1.8, "D50", 0.7347, 0.2653, 0.288040, 0.1596, 0.8404, 0.711874, 0.0366, 0.0001, 0.000086, 2],
    "ROMM"         => [1.8, "D50", 0.7347, 0.2653, 0.288040, 0.1596, 0.8404, 0.711874, 0.0366, 0.0001, 0.000086, 2],
    "SECAM"        => [2.2, "D65", 0.6400, 0.3300, 0.222021, 0.2900, 0.6000, 0.706645, 0.1500, 0.0600, 0.071334, 2],
    "SMPTE-C"      => [2.2, "D65", 0.6300, 0.3400, 0.212395, 0.3100, 0.5950, 0.701049, 0.1550, 0.0700, 0.086556, 2],
    "sRGB"         => [2.2, "D65", 0.6300, 0.3400, 0.212395, 0.3100, 0.5950, 0.701049, 0.1550, 0.0700, 0.086556, 2],
    "wgRGB"        => [2.2, "D50", 0.7350, 0.2650, 0.258187, 0.1150, 0.8260, 0.724938, 0.1570, 0.0180, 0.016875, 1]
};

##############################
# RGB color systems in more code-approachable form.  Parse the table to create hash refs by name, and an
# abbrev table that allows abbreviated naming
#
our $rgbtab = {};
our $rgb_abbrevs = {};
for my $k(keys %$rgbtab_src) {
    my $v = $rgbtab_src->{$k};
    $rgbtab->{$k} = {
	gamma  => $v->[0],
	w_name => $v->[1],
	w      => xyy_from_illuminant($v->[1]),
	r      => pdl($v->[2],$v->[3],$v->[4]),
	g      => pdl($v->[5],$v->[6],$v->[7]),
	b      => pdl($v->[8],$v->[9],$v->[10])
    };
    my $str = $k;
    $str =~ tr/A-Z/a-z/;
    $str =~ s/\s\-//g;
    for my $i($v->[11]..length($str)){
	$rgb_abbrevs->{substr($str,0,$i)} = $k;
    }
}

# Gets an rgb descriptor hash from an input that might be a hash or a name.
# If it's a hash, check to make sure it's copacetic.

=head2 PDL::Transform::Color::get_rgb

=for usage 

    my $rgb_hash = get_rgb( $name );

=for ref

C<PDL::Transform::Color::get_rgb> is an internal routine that retrieves a set of
RGB primary colors from an internal database.  There are several named RGB systems,
with different primary colors for each.  The primary colors are represented as 
CIE xyY values in a returned hash ref.  

The return value is a hash ref with the following fields:

=over 3

=item gamma - the specified gamma of that RGB system (or 2.2, for sRGB)

=item w_name - the name of the illuminant / white-point for that system

=item w - the xyY value of the illuminant / white-point for that system

=item r - the xyY value of the red primary color at unit intensity

=item g - the xyY value of the green primary color at unit intensity

=item b - the xyY value of the blue primary color at unit intensity

=back

Recognized RGB system names are:

=over 3

=item Adobe - Adobe's 1998 RGB, intended to encompass nearly all of the CMYK gamut (gamma=2.2, white=D65)

=item Apple - Apple's display standard from c. 1990 - c. 2010 (gamma=1.8, white=D65)

=item Best - Wide-gamut RGB developed by Don Hutcheson (L<www.hutchcolor.com>) (gamma=2.2, white=D50)

=item Beta - Bruce Lindbloom's optimized ultra-wide-gamut RGB (gamma=2.2, white=D50)

=item Bruce - Bruce Fraser's conservative-gamut RGB space for 8-bit editing (gamma=2.2, white=D65)

=item BT 601 - ITU-R standard BT.601 (used for MPEG & SDTV) (gamma=2.2, white=D65)

=item BT 709 - ITU-R standard BT.709 (used for HDTV) (gamma=2.2, white=D65)

=item CIE - CIE 1931 calibrated color space (based on physical emission lines) (gamma=2.2, white=E)

=item ColorMatch - quasi-standard from c.1990 -- matches Radius Pressview CRT monitors.  (gamma=1.8, white=D50)

=item Don 4 - wide-gamut D50 working space gets the Ektachrome color gamut (gamma=2.2, white=D50)

=item ECI v2 - RGB standard from the European Color Initiative (gamma=1, white=D50)

=item Ekta PS5 - developed by Joseph Holms (L<www.josephholmes.com>) for scanned Ektachrome slides (gamma=2.2, white=D50)

=item NTSC - Never The Same Color (U.S. analog TV standard) (gamma=2.2, white=C)

=item PAL - Pictures Always Lovely (U.K. analog TV standard) (gamma = 2.2, white=D65)

=item ProPhoto - Wide gamut from Kodak, designed for photo output. (gamma=1.8, white=D60)

=item ROMM - Synonym for ProPhoto (gamma=1.8, white=D60)

=item SECAM - Systeme Electronique Contre les AMericains (French analog TV standard) (gamma=2.2, white=D65)

=item SMPTE-C - Soc. Motion Pict. & TV Engineers (current U.S. TV standard) (gamma=2.2, white=D65)

=item sRGB - Standard for consumer computer monitors (gamma~2.2, white=D65)

=item wgRGB - Wide Gamut RGB (gamma=2.2, white=D50)

=back
    
=cut
    
sub get_rgb {
    my $new_rgb = shift;
    unless(ref $new_rgb) {
	$new_rgb=~tr/A-Z/a-z/; $new_rgb =~ s/\s\-//g;
	my $new_rgb_name = $rgb_abbrevs->{$new_rgb};
	if($rgbtab->{$new_rgb_name}) {
	    $new_rgb = $rgbtab->{$new_rgb_name};
	} else {
	    die "Unknown RGB system '$new_rgb'\nKnown ones are:\n\t".join("\n\t",((sort keys %$rgbtab),""));
	}
    } elsif(ref $new_rgb eq 'HASH') {
	my $bad = 0;
	for my $k(qw/w r g b/) {
	    $bad = 1 unless( exists($new_rgb->{$k}) and defined($new_rgb->{$k}) and UNIVERSAL::isa($new_rgb->{$k},"PDL") and $new_rgb->{$k}->nelem==3 and $new_rgb->{$k}->dim(0)==3);
	}
	$new_rgb->{gamma} = 1 unless defined($new_rgb->{gamma});
	die "Incorrect RGB primaries hash -- see docs" if($bad);
    } else {
	die "bad RGB specification -- see docs";
    }
    return $new_rgb;
}


=head2 t_shift_illuminant

=for ref

C<t_new_illuminant> shifts a color from an old RGB system to a new one
with a different white point.  It accepts either a PDL containing a
CIE xyY representation of the new illuminant, or a name of the new illuminant,
and some options.

Because this is shifting RGB to RGB in the same representation, gamma
transformations get re-encoded afterward: if you use, for example,
C<gamma=>2>, then the RGB values are squared, then transformed, then 
square-rooted.

Options are:

=over 3

=item gamma (default=1)

If present, this is the gamma coefficient for the representation of
both the source and destination RGB spaces.

=item from (default="D65")

If present, this is the xyY or name of the OLD illuminant.  The default 
is D65, the illuminant for sRGB (and therefore lsRGB as well).

=item basis (default="sRGB")

If present, this needs to be either "sRGB" or "XYZ" (case insensitive).  
If it's sRGB, the input and output are treated as standard lsRGB coordinates.
If it's XYZ, then the input and output are in CIE XYZ coordinates.

=item method (default="Bradford")

This can be "Bradford", "Von Kries", "XYZ", or a 3x3 matrix Ma (see
C<http://www.brucelindbloom.com/index.html?WorkingSpaceInfo.html>)

=back

=cut

sub t_shift_illuminant {
    my $new_illuminant = shift;
    my($me) = _new(@_, 'New illuminant',
		   {gamma =>1,
		    from => "D65",
		    basis => 'rgb',
		    method=>"Bradford"
		   }
	);

    unless(UNIVERSAL::isa($new_illuminant, 'PDL')) {
	$new_illuminant = xyy_from_illuminant($new_illuminant);
    }
    unless(UNIVERSAL::isa($me->{params}->{from}, 'PDL')) {
	$me->{params}->{from} = xyy_from_illuminant($me->{params}->{from});
    }
    $me->{params}->{to} = $new_illuminant;

    if(UNIVERSAL::isa($me->{params}->{method},"PDL")) {
	print "PDL\n";
	if($me->{params}->{method}->ndims==2 && 
	   $me->{params}->{method}->dim(0)==3 &&
	   $me->{params}->{method}->dim(1)==3) {
	    $me->{params}->{Ma} = $me->{params}->{method}->copy;
	} else {
	    die "t_new_illuminant: method must be a 3x3 matrix or {Bradford|Von Kries|XYZ}";
	}
    } elsif( $me->{params}->{method} =~ m/^B/i || length($me->{params}->{method})==0) {
	# Bradford
	$me->{params}->{Ma} = pdl( [  0.8951000,  0.2664000, -0.1614000 ],
				   [ -0.7502000,  1.7135000,  0.0367000 ],
				   [  0.0389000, -0.0685000,  1.0296000 ]
	    );
    } elsif($me->{params}->{method} =~ m/^[VK]/i) {
	# von Kries or Kries
	$me->{params}->{Ma} = pdl( [  0.4002400,  0.7076000, -0.0808100 ],
				   [ -0.2263000,  1.1653200,  0.0457000 ],
				   [  0.0000000,  0.0000000,  0.9182200 ]
	    );
    } elsif($me->{params}->{method} =~ m/^[XC]/i) {
	# XYZ or CIE
	$me->{params}->{Ma} = pdl( [1, 0, 0], [0, 1, 0], [0, 0, 1] );
    } else {
	print "Unknown method '$me->{params}->{method}'\n";
    }

    $me->{params}->{Ma_inv} = $me->{params}->{Ma}->inv;

    $me->{func} = sub {
	my($in, $opt) = @_;
	my $rhgabe_fr = ( $opt->{Ma} x $opt->{from}->(*1) )->((0))->sever;
	my $rhgabe_to = ( $opt->{Ma} x $opt->{to}  ->(*1) )->((0))->sever;
	my $M = $opt->{Ma_inv} x ( ( $rhgabe_to / $rhgabe_fr )->(*1) * $opt->{Ma} );

	if($opt->{basis} =~ m/^X/i) {
	    return  ((  $M x $in->(*1) )->((0))->sever);
	} else {
	    return  ((  ( $srgb2cxyz_inv x $M x $srgb2cxyz_mat ) x $in->(*1)  )->((0))->sever);
	}
	
    };

    $me->{inv} = sub {
	my($in, $opt) = @_;
	my $rhgabe_fr = ( $opt->{Ma} x $opt->{from}->(*1) )->((0))->sever;
	my $rhgabe_to = ( $opt->{Ma} x $opt->{to}  ->(*1) )->((0))->sever;
	my $M = $opt->{Ma_inv} x ( ( $rhgabe_fr / $rhgabe_to )->(*1) * $opt->{Ma} );

	if($opt->{basis} =~ m/^X/i) {
	    return (( $M x $in->(*1)  )->((0))->sever);
	} else {
	    return (( ( $srgb2cxyz_inv x $M x $srgb2cxyz_mat ) x $in->(*1)  )->((0))->sever);
	}
    };

    if(exists($me->{params}->{gamma}) &&
       defined($me->{params}->{gamma}) &&
       $me->{params}->{gamma} != 1) {
	return (  t_gamma(1.0/$me->{params}->{gamma}) x $me x t_gamma($me->{params}->{gamma}) );
    } else {
	return $me;
    }
}

=head2 t_shift_rgb 

=for usage

  $t = t_shift_rgb("NTSC",{from=>"sRGB"});

=for ref

Shifts the primary color basis of the lsrgb TO the destination system.
Most named RGB systems have an associated preferred gamma, but that is
ignored by default: the RGB values are treated as if they are all
linear representations.  You can specify EITHER the name of the system
OR the specific RGB parameters for that system.  

The RGB parameters, if you specify them, need to be in the form of a
hash ref.  The hash keys should be the same as would be returned by
C<PDL::Transform::Color::get_rgb>.  All the keys must be present,
except for gamma (which is ignored).

Alternatively, you can use the name of a known system.  These are listed in the
documentation for C<PDL::Transform::Color::get_rgb>.

C<t_shift_rgb> takes several options.  

=over 3

=item gamma (default 1)

The input triplets are assumed to be encoded with this gamma function.
The default assumes linear representation.

=item ogamma (default gamma)

The output triplets are assumed to need encoding with this gamma function.

=item use_system_gammas (default 0)

This overrides the settings of "gamma" and "ogamma", and
encodes/decodes according to the original system.

=item wp_method (default undef)

This is the whitepoint shift method used to change illuminant value between
systems with different whitepoints.  See C<t_shift_illuminant> for an
explanation.

=item from (default "sRGB")

This is the RGB system to convert from, in the same format as the
system to convert to (names or a hash ref as described).

=back

=cut

sub t_shift_rgb {
    my $new_rgb = shift;
    my($me) = _new(@_, 'New RGB system',
		   {gamma =>1,
		    ogamma=>undef,
		    use_system_gammas=>0,
		    wp_method=>undef,
		    from=>"sRGB"
		   }
	);


    my $to_rgb   = get_rgb($new_rgb);
    my $from_rgb = get_rgb($me->{params}->{from});

    my ($from_gamma, $to_gamma);
    if($me->{params}->{use_system_gammas}) {
	$from_gamma = $me->{params}->{from_rgb}->{gamma};
	$to_gamma   = $me->{params}->{to_rgb}->{gamma};
    } else {
	$from_gamma = $me->{params}->{gamma};
	$to_gamma   = $me->{params}->{ogamma} // $me->{params}->{gamma};
    }

    my $out = 
	!t_xyz(rgb_system=>$to_rgb, gamma=>$me->{params}->{gamma}, use_system_gamma=>$me->{params}->{use_system_gamma}) x 
	t_shift_illuminant($to_rgb->{w},basis=>"XYZ",from=>$from_rgb->{w},method=>$me->{params}->{wp_method}) x 
	t_xyz(rgb_system=>$from_rgb, gamma=>$me->{params}->{gamma}, use_system_gamma=>$me->{params}->{use_system_gamma});

    return $out;

}		    



##############################


=head2 t_rgi

=for ref

Convert RGB to RG chroma with a separate intensity channel.

Note that intensity is just the average of the R, G, and B values.
If you want perceptible luminance, use t_rgl or t_ycbcr instead.

=cut

sub t_rgi {
    my($me) = _new(@_, 'RGI',
		   {gamma=>1,
		   }
	);

    $me->{func} = sub {
	my($in,$opt) = @_;
	my $i = $in->sumover->(*1);
	my $out = zeroes($in);
	$out->(0:1) .= $in(0:1) / ($i+($i==0));
	$out->(2) .= $i/3;
	if($in->is_inplace) {
	    $in .= $out;
	    return $in;
	}
	return $out;
    };
    $me->{inv} = sub {
	my($in,$opt) = @_;
	my $out = zeroes($in);
	$out->(0:1) .= $in(0:1);
	$out->((2)) .= 1 - $in(0:1)->sumover;
	$out *= $in->(2) * 3;
	if($in->is_inplace) {
	    $in .= $out;
	    return $in;
	}
	return $out;
    };

    return $me;
}


##############################

=head2 t_cieXYZ, t_xyz

=for ref

The C<t_cieXYZ> transform (also C<t_xyz>, which is a synonym)
converts the module-native lsRGB to the CIE XYZ representation.  CIE
XYZ is a nonphysical RGB-style system that minimally represents every
physical color it is possible for humans to perceive in steady
illumintion.  It is related to sRGB by a linear transformation
(i.e. matrix multiplication) and forms the basis of many other color
systems (such as CIE xyY).

CIE XYZ values are defined in such a way that they are positive
definite for all human-perceptible colors, at the cost that the
primaries are nonphysical (they correspond to no possible spectral
color)

C<t_ciexyz> accepts the following options:

=over 3

=item gamma (default 1) 

This is taken to be a coded gamma value in the original lsRGB, which
is decoded before conversion to the CIE XYZ system.

=item rgb_system (default undef)

If present, this must be either the name of an RGB system or an RGB system
descriptor hash as described in C<t_shift_rgb>.  If none is specified, then 
the standard linearized sRGB used by the rest of the module is assumed.

=item use_system_gamma (default 0)

If this flag is set, and C<rgb_system> is set also, then the RGB side
of the transform is taken to be gamma-encoded with the default value for
that RGB system.  Unless you explicitly specify an RGB system (with a name
or a hash), this flag is ignored.

=back

=cut


*t_cieXYZ = \&t_xyz;

sub t_xyz {
    my ($me) = _new(@_, 'CIE XYZ',
		    {gamma=>1,
		     rgb_system=>undef,
		     use_system_gamma=>0
		    }
	);

    # shortcut the common case
    unless(defined($me->{params}->{rgb_system})) {

	$me->{params}->{mat} = $srgb2cxyz_mat;
	$me->{params}->{inv} = $srgb2cxyz_inv;
		
    } else {
	my $rgb = get_rgb($me->{params}->{rgb_system});

	my ($xr,$yr) = ($rgb->{r}->((0)),$rgb->{r}->((1)));
	my ($xg,$yg) = ($rgb->{g}->((0)),$rgb->{g}->((1)));
	my ($xb,$yb) = ($rgb->{b}->((0)),$rgb->{b}->((1)));
	
	my $Xr = $xr / ($yr + ($yr==0));
	my $Yr = 1;
	my $Zr = (1 - $xr - $yr)/($yr+($yr==0));
	my $Xg = $xg / ($yg + ($yg==0));
	my $Yg = 1;
	my $Zg = (1 - $xg - $yg)/($yg+($yg==0));
	my $Xb = $xb / ($yb + ($yb==0));
	my $Yb = 1;
	my $Zb = (1 - $xb - $yb)/($yb+($yb==0));

	my $M = pdl( [ $Xr, $Xg, $Xb ], [$Yr, $Yg, $Yb], [$Zr, $Zg, $Zb] );
	my $Minv = $M->inv;

	my ($xw, $yw, $Yw) = ($rgb->{w}->((0)),$rgb->{w}->((1)),$rgb->{w}->((2)));
	my $Xw = $xw * $Yw / ($yw + ($yw==0));
	my $Zw = (1 - $xw - $yw)*$Yw / ($yw+($yw==0));
	my $XYZw = pdl($Xw,$Yw,$Zw);

	my $Srgb = ($Minv x $XYZw->(*1))->((0)); # row vector
	$M *= $Srgb;
	$me->{params}->{mat} = $M;
	$me->{params}->{inv} = $M->inv;

	if($me->{params}->{use_system_gamma}) {
	    $me->{params}->{gamma} = $rgb->{gamma};
	}
    }

    # func and inv get linearized versions (gamma handled below)
    $me->{func} = sub {
	my($in, $opt) = @_;

	my $out = ( $opt->{mat} x $in->(*1) )->((0))->sever;
	
	if($in->is_inplace) {
	    $in .= $out;
	    $out = $in;
	}
	return $out;
    };

    $me->{inv} = sub {
	my($in, $opt) = @_;
	my $out = ( $opt->{inv} x $in->(*1) )->((0))->sever;

	if($in->is_inplace) {
	    $in .= $out;
	    $out = $in;
	}
	return $out;
    };

    return gammify($me);
}

=head2 t_xyy

=head2 t_xyY

=for ref

Convert from sRGB to CIE xyY.  The C<xyY> system is part of the CIE
1931 color specification.  Luminance is in the 2 coordinate, and
chrominance x and y are in the 0 and 1 coordinates.

This is the coordinate system in which "chromaticity diagrams" are
plotted.  It is capable of representing every illuminant color that
can be perceived by the typical human eye, and also many that can't,
with positive-definite coordinates.

Most of the domain space (which runs over [0-1] in all three dimensions)
is inaccessible to most displays, because RGB gamuts are generally 
smaller than the actual visual gamut, which in turn is a subset of the
actual xyY data space. 

=cut

*t_xyY = \&t_xyy;

sub t_xyy {
    my ($me) = _new(@_, 'CIE xyY',
		    {gamma=>1,
		    }
	);

    $me->{func} = sub {
	my($XYZ, $opt) = @_;
	my $out = $XYZ/$XYZ->sumover->(*1);
	$out->((2)) .= $XYZ->((1));
	if($XYZ->is_inplace) {
	    $XYZ .= $out;
	    $out = $XYZ;
	}
	return $out;
    };

    $me->{inv} = sub {
	my($in,$opt) = @_;
	# make xYy
	my $XYZ = zeroes($in);

	# stuff X and Z in there.
	my $in1 = $in->((1))+($in->((1))==0);
	$XYZ->((0)) .= $in->((0)) * $in->((2)) / $in1;
	$XYZ->((1)) .= $in->((2));
	$XYZ->((2)) .= $in->((2)) * (1 - $in->((0)) - $in->((1))) / $in1;
	
	if($in->is_inplace) {
	    $in .= $XYZ;
	    $XYZ = $in;
	}
	return $XYZ;
    };
    return gammify( $me x t_xyz() );
}

=head2 
	

######################################################################

=head2 t_cmyk

converts rgb to cmyk in the most straightforward way (by subtracting
RGB values from unity).  

CMYK and other process spaces are very complicated; this transform
presents only a relatively simple conversion that does not take into
account ink gamut variation or many other effects.

There *is* a provision for halftone gamma correction: "htgamma", which
works exactly like the rgb gamma correction but is applied to the CMYK
output.

Options:

=over 3

=item gamma (default 1)

The standard gamma affecting the RGB cube

=item htgamma (default 1)

A "halftone gamma" that is suitable for non-wash output processes
such as halftoning. it acts on the CMYK values themselves.

=item byte (default 0)

If present, the CMYK side is scaled to 0-255 and converted to a byte type.

=back

=cut
;
sub t_cmyk {
    my($me) = _new(@_, "CMYK",
		   {gamma=>1,
		    pigment=>0,
		    density=>2,
		    htgamma=>1,
		    clip=>0,
		    byte=>0
		   }
	);
    $me->{idim} = 3;
    $me->{odim} = 4;

    $me->{func} = sub {
	my($in,$opt) = @_;
	my $out = zeroes( 4, $in->((0))->dims );
	
	my $Kp = $in->maximum->(*1);
	(my $K = $out->(3)) .= 1 - $Kp;
	$out->(0:2) .= ($Kp - $in->(0:2)) / $Kp;
	$out->((3))->where($Kp==0) .= 1;
	$out->(0:2)->mv(0,-1)->where($Kp==0) .= 0;

	if(defined($opt->{htgamma}) && $opt->{htgamma} != 1) {
	    $out *= ($out->abs) ** ($opt->{htgamma} - 1);
	}

	if($opt->{clip}) {
	    $out->inplace->clip(0,1);
	}

	if($opt->{byte}) {
	    $out = (256*$out)->clip(0,255.99999);
	}
	return $out;
    };

    $me->{inv} = sub {
	my($in,$opt) = @_;
	my $out = zeroes( 3, $in->((0))->dims );

	$in = $in->new_or_inplace;
	
	if($opt->{byte}) {
	    $in = $in / pdl(256); # makes copy
	}

	if(defined($opt->{htgamma}) && $opt->{htgamma} != 1) {
	    $in *= ($in->abs) ** (1.0/$opt->{htgamma} - 1);
	}
	my $Kp = 1.0 - $in->(3);
	$out .= $Kp * ( 1 - $in->(0:2) );
	return $out;
    };

    return gammify($me);

}

=head2 t_cielab or t_lab

=for usage

    $t = t_cielab();

=for ref

Convert RGB to CIE Lab colors.  C<Lab> stands for Lightness, 
"a", and "b", representing the overall luminance detection and 
two opponent systems (a: red/green, and b:yellow/blue) in the human 
eye.  Lab colors are approximately perceptually uniform:  they're
mapped using a nonlinear transformation involving cube roots.  Lab 
has the property that Euclidean distances of equal size in the space
yield approximately equal perceptual shifts in the represented color.

Lightness runs 0-100, and the a and b opponent systems run -100 to +100.

The Lab space includes the entire CIE XYZ gamut and many "impossible colors".
that cannot be represented directly with physical light.  Many of these 
"impossible colors" (also "chimeric colors") can be experienced directly
using visual fatigue effects, and can be classified using Lab.

Lab is easiest to convert directly from XYZ space, so the C<t_lab> constructor
returns a compound transform of C<t_xyz2lab> and C<t_xyz>.

=cut

sub f_lab {
    my $in = shift;
    my $delta = 6/29;
    my $delta3 = $delta * $delta * $delta;
    return ( 
	($in >  $delta3) * ( $in * (($in->abs+($in==0)) ** (0.333-1)) ) +
	($in <= $delta3) * ( $in / (3 * $delta * $delta) + 4/29 )
	);
}


sub f_lab_inv {
    my $in = shift;
    my $delta = 6/29;

    return (
	($in >  $delta) * ($in*$in*$in) +
	($in <= $delta) * (3 * $delta * $delta * ($in - 4/29))
	);
}

=head2 t_xyz2lab

=for usage

    $t = t_xyz2lab();

=for ref

Converts CIE XYZ to CIE Lab.

=cut
    
sub t_xyz2lab {
	
    my ($me) = _new(@_,'XYZ->Lab',
		    {
			white=>"D65",
		    }
	);

    # get and store illuminant XYZ
    my $wp_xyy = xyy_from_illuminant($me->{params}->{white});
    $me->{params}->{wp_xyz} = $wp_xyy->copy;
    $me->{params}->{wp_xyz}->(2) .= 1 - $wp_xyy->(0) - $wp_xyy->(1);
    $me->{params}->{wp_xyz} *= $wp_xyy->(2);
    

    # input is XYZ by the time it gets here
    $me->{func} = sub {
	my($in,$opt) = @_;
	my($out) = zeroes($in);

	my $wp = $opt->{wp_xyz} + ($opt->{wp_xyz}==0);
	
	my $FYp = f_lab(  $in->((1)) / $wp->((1))  );
	    
	$out->((0)) .= 116 * $FYp - 16;
	$out->((1)) .= 500 * ( f_lab( $in->((0)) / $wp->((0)) ) - $FYp   );
	$out->((2)) .= 200 * ( $FYp - f_lab( $in->((2)) / $wp->((2)) ) );

	if($in->is_inplace) {
	    $in .= $out;
	    $out = $in;
	}
	return $out;
    };

    $me->{inv} = sub {
	my($in,$opt) = @_;
	my($out) = zeroes($in);

	my $Lterm = ($in->((0))+16)/116;
	
	$out->((0)) .= $opt->{wp_xyz}->((0)) * f_lab_inv( $Lterm + $in->((1))/500 );
	$out->((1)) .= $opt->{wp_xyz}->((1)) * f_lab_inv( $Lterm );
	$out->((2)) .= $opt->{wp_xyz}->((2)) * f_lab_inv( $Lterm - $in->((2))/200 );

	if($in->is_inplace) {
	    $in .= $out;
	    $out = $in;
	}
	return $out;
    };
    
    return $me;
}


sub t_lab {
    my ($me) = _new(@_, 'Lab',
		    {
			gamma => 1.0,
			white=>'D65',
		    }
	);
    return ( 
	t_xyz2lab(white=>$me->{params}->{white} )  x
	t_xyz( gamma=>$me->{params}->{gamma})
	);
}


=head2 t_hsl and t_hsv

=for usage

    $rgb = $hsl->invert($t_hsl());

=for ref

HSL stands for Hue, Saturation, Lightness.  It's not an absolute
color space, simply derived from each RGB (by default, linearized
sRGB).  it has the same gamut as the host RGB system.  The coordinates
are hexagonal and follow the nearest face of the cube.

HSL is a double-cone system, so iso-L surfaces are close to the plane
perpendicular to the double-diagonal white/illuminant line R=G=B.
This has the effect of reducing saturation at high lightness levels,
but maintains luminosity independent of saturation.  Maximum
saturation occurs when S=1 and L=0.5; at higher values of L, colors
grow less saturated and more pastel, so that L follows total
luminosity of the output.

HSV is a stacked sinfle-cone system: iso-V surfaces are parallel to
the bright faces of the RGB cube, so maximal bright saturation occurs
when S=1 and V=1.  This means that output luminosity drops with
saturation, but due to the Helmholtz-Kolrausch effect (linking
saturation to apparent brightness) the *perceived* brightness is less
S-dependent: V follows total *apparent brightness* of the output,
though output luminosity drops with S.

You can represent out-of-gamut values in either system, by using
saturations greater than unity.

Hue, Saturation, and (Lightness or Value) each run from 0 to 1.  

You can encode the Lightness or Value with a gamma value ("lgamma") if 
desired.

Options:

=over 3

=item gamma (default 1)

Treat the base RGB as gamma-encoded (default 1 is linear)

=item lgamma (default 1)

Treat the L coordinate as gamma-encoded (default 1 is linear).

=item hsv (default 0 if called as "t_hsl", 1 if called as "t_hsv")

Sets which of the HSL/HSV transform is to be used.

=back

=cut

sub t_hsl {
    my($me) = _new(@_,"HSL",
		   {gamma=>1,
		    lgamma=>1,
		    hsv=>0
		   }
	);

    $me->{name} = "HSV" if($me->{params}->{hsv});
    
    $me->{func} = sub {
	my($in, $opt) = @_;
	my $out = zeroes($in);
	
	my $Cmax = $in->maximum;
	my $Cmin = $in->minimum;
	my $maxdex = $in->qsorti->((2))->sever;
	my $Delta = ( $Cmax - $Cmin );

	my $dexes = ($maxdex->(*1) + pdl(0,1,2)) % 3;

	my $H = $out->((0));
	$H .= (  ($in->index1d($dexes->(1)) - $in->index1d($dexes->(2)))->((0))/($Delta+($Delta==0)) + 2 * $dexes->((0))  )  /  6;
	$H += ($H<0);

	# Lightness and Saturation
	my $L = $out->((2));
	if($opt->{hsv}) {
	    $L .= $Cmax;
	    $out->((1)) .= $Delta / ($L + ($L==0));
	} else {
	    $L .= ($Cmax + $Cmin)/2;
	    $out->((1)) .= $Delta / (1 - (2*$L-1)->abs + ($L==0 | $L==1));
	}
	

	if( $opt->{lgamma} != 1 ){
	    $L .= $L * (($L->abs + ($L==0)) ** (1.0/$opt->{lgamma} - 1));
	}

	if($in->is_inplace) {
	    $in .= $out;
	    $out = $in;
	}
	return $out;
    };

    $me->{inv} = sub {
	my($in,$opt) = @_;

	my $H = $in->((0))*6;
	my $S = $in->((1));
	my $L = $in->((2));

	if($opt->{lgamma} != 1) {
	    $L = $L * (($L->abs + ($L==0)) ** ($opt->{lgamma}-1));
	}
	
	my $ZCX = zeroes($in);
	my $C = $ZCX->((1));
	my $m;
	if($opt->{hsv}) {
	    $C .= $L * $S;
	    $m = $L - $C;
	} else {
	    $C .= (1 - (2*$L - 1)->abs) * $S;
	    $m = $L - $C/2;
	}

	$ZCX->((2)) .= $C * (1 - ($H % 2 - 1)->abs);

	my $dexes = pdl( [1,2,0], [2,1,0], [0,1,2], [0,2,1], [2,0,1], [1,0,2] )->mv(1,0)->sever;
	my $dex = $dexes->index1d($H->floor->(*1,*1))->((0))->sever; # 3x(threads)
	my $out = $ZCX->index1d($dex)->sever + $m->(*1);

	if($in->is_inplace) {
	    $in .= $out;
	    $out = $in;
	}

	return $out;
    };

    return gammify($me);
}

=head2 t_hsv

=for ref

Implements the HSV transfromation, which is similar to HSL except that it fades
to light instead of to grey.  See the documentation for C<t_hsl> for details.

=cut

sub t_hsv {
        my($me) = _new(@_,"HSL",
		   {gamma=>1,
		    lgamma=>1,
		    hsv=>1
		   }
	);
	return t_hsl(%{$me->{params}});
}
		    


1;
