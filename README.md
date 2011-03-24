Haskell bindings for the OpenKinect freenect library.

To compile the bindings, move to the src directory and run:

    ghc --make -c Device/Kinect.hs

If you want to run the example, compile and link it with GHC thusly:

    ghc --make -threaded Example.hs /usr/local/lib/libfreenect.dylib
    ./Example

Make sure to adjust the path to wherever your copy of the libfreenect library
file is.

*Note that if you're on Snow Leopard, the freenect library will build for
x86_64, which the default Haskell Platform won't support. You'll need to
install the optional x86_64 version of the Haskell Platform in order to compile
the bindings, or reinstall libfreenect for i386.*

*Also note that there is an incompatibility between XCode 4 and GHC 7.0.2 which
will prevent the bindings from compiling. Stick with XCode 3 until this issue
is resolved.*
