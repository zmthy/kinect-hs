Haskell bindings for the OpenKinect freenect library.

To compile the bindings, move to the src directory and run:

    ghc --make -c Device/Kinect.hs

If you want to run one of the examples, compile and link it with GHC thusly:

    ghc --make -threaded Example.hs /usr/local/lib/libfreenect.dylib
    ./Example

Make sure to adjust the name of the file and the path to wherever your copy of
the libfreenect library file is.

*Note that if you're on Snow Leopard, the freenect library will build for
x86_64, which the default Haskell Platform won't support. You'll need to
install the optional x86_64 version of the Haskell Platform in order to compile
the bindings, or reinstall libfreenect for i386.*
