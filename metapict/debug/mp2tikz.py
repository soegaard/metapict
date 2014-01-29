# mp2tikz.py
# (c) 2012 JL Diaz
#
# This module contains classes and functions to implement Jonh Hobby's
# algorithm to find a smooth curve which  passes through a serie of given
# points. The algorithm is used in METAFONT and MetaPost, but the source code
# of these programs is hard to read. I tried to implement it in a more 
# modern way, which makes the algorithm more understandandable and perhaps portable
# to other languages
#
# It can be imported as a python module in order to generate paths programatically
# or used from command line to convert a metapost path into a tikz one
#
# For the second case, the use is:
#
# $ python mp2tikz.py <metapost path> <options>
#
# Where:
#  <metapost path> is a path using metapost syntax with the following restrictions:
#    * All points have to be explicit (no variables or expressions)
#    * All joins have to be "curved" ( .. operator)
#    * Options in curly braces next to the nodes are ignored, except
#      for {curl X} at end points
#    * tension can be specified using metapost syntax
#    * "cycle" as end point denotes a cyclic path, as in metapost
#    Examples:
#      (0,0) .. (60,40) .. (40,90) .. (10,70) .. (30,50) .. cycle
#      (0,0) .. (60,40) .. (40,90) .. (10,70) .. (30,50)
#      (0,0){curl 10} .. (60,40) .. (40,90) .. (10,70) .. (30,50)
#      (0,0) .. (60,40) .. (40,90) .. tension 3 .. (10,70) .. (30,50) .. cycle
#      (0,0) .. (60,40) .. (40,90) .. tension 1 and 3 .. (10,70) .. (30,50) .. cycle
#
#  <options> can be:
#     tension = X. The given tension is applied to all segments in the path by default
#        (but tension given at specific points override this setting at those points)
#     curl = X. The given curl is applied by default to both ends of the open path
#        (but curl given at specific endings override this setting at that point)
#     any other options are considered tikz options.
#  
#   The script prints in standard output a tikz command which draws the given path
#   using the given options. In this path all control points are explicit, as computed
#   by the string using Hobby's algorith. 
#  
#   For example:
#
#   $ python mp2tikz.py "(0,0) .. (10,10) .. (20,0) .. (10, -10) .. cycle" "tension =3, blue"
#
#   Would produce
#   \draw[blue] (0.0000, 0.0000) .. controls (-0.00000, 1.84095) and (8.15905, 10.00000)..
#   (10.0000, 10.0000) .. controls (11.84095, 10.00000) and (20.00000, 1.84095)..
#   (20.0000, 0.0000) .. controls (20.00000, -1.84095) and (11.84095, -10.00000)..
#   (10.0000, -10.0000) .. controls (8.15905, -10.00000) and (0.00000, -1.84095)..(0.0000, 0.0000); 
#

from math import sqrt, sin, cos, atan2, atan, degrees, radians, pi
# Coordinates are stored and manipulated as complex numbers,
# so we require cmath module
import cmath

def arg(z):
    return atan2(z.imag, z.real)

def direc(angle):
    """Given an angle in degrees, returns a complex with modulo 1 and the
    given phase"""
    phi = radians(angle)
    return complex(cos(phi), sin(phi))

def direc_rad(angle):
    """Given an angle in radians, returns a complex with modulo 1 and the
    given phase"""
    return complex(cos(phi), sin(phi))

class Point():
    """This class implements the coordinates of a knot, and all kind of
    auxiliar parameters to compute a smooth path passing through it"""
    z = complex(0,0)     # Point coordinates
    alpha = 1             # Tension at point (1 by default)
    beta = 1
    theta = 0            # Angle at which the path leaves
    phi  = 0             # Angle at which the path enters
    xi = 0               # angle turned by the polyline at this point
    v_left = complex(0,0)   # Control points of the Bezier curve at this point
    u_right = complex(0,0)  # (to be computed later)
    d_ant  = 0              # Distance to previous point in the path
    d_post = 0              # Distance to next point in the path

    def __init__(self, z, alpha=1, beta=1, v=complex(0,0), u=complex(0,0)):
        """Constructor. Coordinates can be given as a complex number
        or as a tuple (pair of reals). Remaining parameters are optional
        and take sensible default vaules."""
        if type(z)==complex:
            self.z=z
        else:
            self.z=complex(z[0], z[1])
        self.alpha = alpha
        self.beta = beta
        self.v_left = v
        self.u_right = u
        self.d_ant  = 0
        self.d_post = 0
        self.xi   = 0
    def __str__(self):
        """Creates a printable representation of this object, for
        debugging purposes"""
        return """\
z=(%.3f, %.3f)  alpha=%.2f beta=%.2f theta=%.2f phi=%.2f
   [v=(%.2f, %.2f) u=(%.2f, %.2f) d_ant=%.2f d_post=%.2f xi=%.2f]""" % \
          (self.z.real, self.z.imag, self.alpha, self.beta,
                  degrees(self.theta), degrees(self.phi),
                  self.v_left.real, self.v_left.imag, self.u_right.real,
                  self.u_right.imag, self.d_ant, self.d_post, degrees(self.xi))

class Path():
    """This class implements a path, which is a list of Points"""
    p = None                       # List of points
    cyclic = True                  # Is the path cyclic?
    curl_begin = 1                 # If not, curl parameter at endpoints
    curl_end = 1
    def __init__(self, p, tension=1, cyclic=True, curl_begin=1, curl_end=1):
        self.p = []
        for pt in p:
            self.p.append(Point(pt, alpha=1.0/tension, beta=1.0/tension))
        self.cyclic = cyclic
        self.curl_begin = curl_begin
        self.curl_end = curl_end

    def range(self):
        """Returns the range of the indexes of the points to be solved.
        This range is the whole length of p for cyclic paths, but excludes
        the first and last points for non-cyclic paths"""
        if self.cyclic:
            return range(len(self.p))
        else:
            return range(1, len(self.p)-1)

    # The following functions allow to use a Path object like an array
    # so that, if x = Path(...), you can do len(x) and x[i]
    def append(self, data):
        self.p.append(data)

    def __len__(self):
        return len(self.p)

    def __getitem__(self, i):
        """Gets the point [i] of the list, but assuming the list is
        circular and thus allowing for indexes greater than the list
        length"""
        i %= len(self.p)
        return self.p[i]

    # Stringfication
    def __str__(self):
        """The printable representation of the object is one suitable for
        feeding it into tikz, producing the same figure than in metapost"""
        r = []
        L = len(self.p)
        last = 1
        if self.cyclic:
            last = 0
        for k in range(L-last):
            post = (k+1)%L
            z = self.p[k].z
            u = self.p[k].u_right
            v = self.p[post].v_left
            r.append("(%.4f, %.4f) .. controls (%.5f, %.5f) and (%.5f, %.5f)" %\
                    (z.real, z.imag, u.real, u.imag, v.real, v.imag))
        if self.cyclic:
            last_z = self.p[0].z
        else:
            last_z = self.p[-1].z
        r.append("(%.4f, %.4f)" % (last_z.real, last_z.imag))
        return "..".join(r)

    def __repr__(self):
        """Dumps internal parameters, for debugging purposes"""
        r = ["Path information"]
        r.append("Cyclic=%s, curl_begin=%s, curl_end=%s" % (self.cyclic,
            self.curl_begin, self.curl_end))
        for pt in self.p:
            r.append(str(pt))
        return "\n".join(r)

# Now some functions from John Hobby and METAFONT book.
# "Velocity" function
def f(theta, phi):
    n = 2+sqrt(2)*(sin(theta)-sin(phi)/16)*(sin(phi)-sin(theta)/16)*(cos(theta)-cos(phi))
    m = 3*(1 + 0.5*(sqrt(5)-1)*cos(theta) + 0.5*(3-sqrt(5))*cos(phi))
    return n/m

def control_points(z0, z1, theta=0, phi=0, alpha=1, beta=1):
    """Given two points in a path, and the angles of departure and arrival
    at each one, this function finds the appropiate control points of the
    Bezier's curve, using John Hobby's algorithm"""
    i = complex(0,1)
    u = z0 + cmath.exp(i*theta)*(z1-z0)*f(theta, phi)*alpha
    v = z1 - cmath.exp(-i*phi)*(z1-z0)*f(phi, theta)*beta
    return(u,v)

def pre_compute_distances_and_angles(path):
    """This function traverses the path and computes the distance between
    adjacent points, and the turning angles of the polyline which joins
    them"""
    for i in range(len(path)):
        v_post  = path[i+1].z - path[i].z
        v_ant   = path[i].z - path[i-1].z
        # Store the computed values in the Points of the Path
        path[i].d_ant = abs(v_ant)
        path[i].d_post = abs(v_post)
        path[i].xi = arg(v_post/v_ant)
    if not path.cyclic:
        # First and last xi are zero
        path[0].xi = path[-1].xi = 0
        # Also distance to previous and next points are zero for endpoints
        path[0].d_ant = 0
        path[-1].d_post = 0

def build_coefficients(path):
    """This function creates five vectors which are coefficients of a
    linear system which allows finding the right values of "theta" at
    each point of the path (being "theta" the angle of departure of the
    path at each point). The theory is from METAFONT book."""
    A=[];  B=[];  C=[];  D=[];   R=[]
    pre_compute_distances_and_angles(path)
    if not path.cyclic:
        # In this case, first equation doesnt follow the general rule
        A.append(0)
        B.append(0)
        curl = path.curl_begin
        alpha_0 = path[0].alpha
        beta_1 = path[1].beta
        xi_0 = (alpha_0**2) * curl / (beta_1**2)
        xi_1 = path[1].xi
        C.append(xi_0*alpha_0 + 3 - beta_1)
        D.append((3 - alpha_0)*xi_0 + beta_1)
        R.append(-D[0]*xi_1)

    # Equations 1 to n-1 (or 0 to n for cyclic paths)
    for k in path.range():
        A.append(   path[k-1].alpha  / ((path[k].beta**2)  * path[k].d_ant))
        B.append((3-path[k-1].alpha) / ((path[k].beta**2)  * path[k].d_ant))
        C.append((3-path[k+1].beta)  / ((path[k].alpha**2) * path[k].d_post))
        D.append(   path[k+1].beta   / ((path[k].alpha**2) * path[k].d_post))
        R.append(-B[k] * path[k].xi  - D[k] * path[k+1].xi)

    if not path.cyclic:
        # The last equation doesnt follow the general form
        n = len(R)     # index to generate
        C.append(0)
        D.append(0)
        curl = path.curl_end
        beta_n = path[n].beta
        alpha_n_1 = path[n-1].alpha
        xi_n = (beta_n**2) * curl / (alpha_n_1**2)
        A.append((3-beta_n)*xi_n + alpha_n_1)
        B.append(beta_n*xi_n + 3 - alpha_n_1)
        R.append(0)
    return (A, B, C, D, R)

import numpy as np    # Required to solve the linear equation system

def solve_for_thetas(A, B, C, D, R):
    """This function receives the five vectors created by
    build_coefficients() and uses them to build a linear system with N
    unknonws (being N the number of points in the path). Solving the system
    finds the value for theta (departure angle) at each point"""
    L=len(R)
    a = np.zeros((L, L))
    for k in range(L):
       prev = (k-1)%L
       post = (k+1)%L
       a[k][prev] = A[k]
       a[k][k]    = B[k]+C[k]
       a[k][post] = D[k]
    b = np.array(R)
    print a
    print b
    return np.linalg.solve(a,b)

def solve_angles(path):
    """This function receives a path in which each point is "open", i.e. it
    does not specify any direction of departure or arrival at each node,
    and finds these directions in such a way which minimizes "mock
    curvature". The theory is from METAFONT book."""

    # Basically it solves
    # a linear system which finds all departure angles (theta), and from
    # these and the turning angles at each point, the arrival angles (phi)
    # can be obtained, since theta + phi + xi = 0  at each knot"""
    x = solve_for_thetas(*build_coefficients(path))
    L = len(path)
    for k in range(L):
        path[k].theta = x[k]
        print("theta(",k,")=",path[k].theta)
    for k in range(L):
        path[k].phi = - path[k].theta - path[k].xi
        print("phi(",k,")=", path[k].phi)
    for k in range(L):
        print("psi(",k,")=",path[k].xi)

def find_controls(path):
    """This function receives a path in which, for each point, the values
    of theta and phi (leave and enter directions) are known, either because
    they were previously stored in the structure, or because it was
    computed by function solve_angles(). From this path description
    this function computes the control points for each knot and stores
    it in the path. After this, it is possible to print path to get
    a string suitable to be feed to tikz."""
    r = []
    for k in range(len(path)):
        z0 = path[k].z
        z1 = path[k+1].z
        theta = path[k].theta
        phi = path[k+1].phi
        alpha = path[k].alpha
        beta = path[k+1].beta
        u,v=control_points(z0, z1, theta, phi, alpha, beta)
        path[k].u_right = u
        path[k+1].v_left = v

def mp_to_tikz(path, command=None, options=None):
    """Utility funcion which receives a string containing a metapost path
    and uses all the above to generate the tikz version with explicit
    control points.
    It does not make a full parsing of the metapost path. Currently it is
    not possible to specify directions nor tensions at knots. It uses
    default tension = 1, default curl =1 for both ends in non-cyclic paths
    and computes the optimal angles at each knot. It does admit however
    cyclic and non-cyclic paths.
    To summarize, the only allowed syntax is z0 .. z1 .. z2, where z0, z1,
    etc are explicit coordinates such as (0,0) .. (1,0) etc.. And
    optionally the path can ends with the literal "cycle"."""
    tension = 1
    curl = 1
    if options:
        opt = []
        for o in options.split(","):
            o=o.strip()
            if o.startswith("tension"):
                tension = float(o.split("=")[1])
            elif o.startswith("curl"):
                curl = float(o.split("=")[1])
            else:
                opt.append(o)
        options = ",".join(opt)
    new_path = mp_parse(path, default_tension = tension, default_curl = curl)
    # print repr(new_path)
    solve_angles(new_path)
    find_controls(new_path)
    if command==None:
       command="draw"
    if options==None:
       options = ""
    else:
       options = "[%s]" % options
    return "\\%s%s %s;" % (command, options, str(new_path))


def mp_parse(mppath, default_tension = 1, default_curl = 1):
    """This function receives a string which contains a path in metapost syntax,
    and returns a Path object which stores the same path in the structure 
    required to compute the control points.
      The path should only contain explicit coordinates and numbers.
      Currently only "curl" and "tension" keywords are understood. Direction
    options are ignored."""
    if mppath.endswith(";"):  # Remove last semicolon
        mppath=mppath[:-1]
    pts = mppath.split("..")       # obtain points
    pts = [p.strip() for p in pts] # remove extra spaces

    if pts[-1] == "cycle":
        is_cyclic = True
        pts=pts[:-1]     # Remove this last keyword
    else:
        is_cyclic = False
    path = Path([], cyclic=is_cyclic)
    path.curl_begin = default_curl
    path.curl_end   = default_curl
    alpha = beta = 1.0/default_tension
    k=0
    for p in pts:
        if p.startswith("tension"):
            aux = p.split()
            alpha = 1.0/float(aux[1])
            if len(aux)>3:
                beta = 1.0/float(aux[3])
            else:
                beta = alpha
        else:
            aux = p.split("{")  # Extra options at the point
            p = aux[0].strip()
            if p.startswith("curl"):
                if k==0:
                    path.curl_begin=float(aux[1])
                else:
                    path.curl_end = float(aux[1])
            elif p.startswith("dir"):
                # Ignored by now
                pass

            path.append(Point(eval(p)))  # store the pair of coordinates
            # Update tensions
            path[k-1].alpha = alpha
            path[k].beta  = beta
            alpha = beta = 1.0/default_tension
            k = k + 1
    if is_cyclic:
        path[k-1].alpha = alpha
        path[k].beta = beta
    return path

def main():
    """Example of conversion. Takes a string from stdin and outputs the
    result in stdout.
    """
    import sys
    if len(sys.argv)>2:
        opts = sys.argv[2]
    else:
        opts = None
    path = sys.argv[1]
    print mp_to_tikz(path, options = opts)

if __name__ == "__main__":
    main()