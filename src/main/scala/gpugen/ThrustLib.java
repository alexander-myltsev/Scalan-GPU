package main.scala.gpugen;

import com.googlecode.javacpp.*;
import com.googlecode.javacpp.annotation.*;

import java.io.PrintStream;

@Platform(include = {
        "<thrust/device_vector.h>",
        "<thrust/transform.h>",
        "<thrust/sequence.h>",
        "<thrust/copy.h>",
        "<thrust/fill.h>",
        "<thrust/replace.h>",
        "<thrust/functional.h>",
        "d:/phd/Scalan-v2/tmp/fun.cpp"},
        includepath = {"d:/phd/thrust-src"})
public class ThrustLib {
    static { Loader.load(); }

    @Name("thrust::host_vector<int>")
    public static class HostVectorPointer extends Pointer {
        static { Loader.load(); }

        public HostVectorPointer() { allocate(); }

        public HostVectorPointer(long n) { allocate(n); }

        private native void allocate();

        private native void allocate(long n);

        @Name("operator[]") public native int get(long n);
    }

    @Name("thrust::device_vector<int>")
    public static class DeviceVectorIntPointer extends Pointer {
        static { Loader.load(); }

        public DeviceVectorIntPointer() { allocate(); }

        public DeviceVectorIntPointer(Pointer p) { super(p); }

        @Name("operator=") public native @ByRef
        DeviceVectorIntPointer copy(@ByRef DeviceVectorIntPointer x);

        public DeviceVectorIntPointer(long n) { allocate(n); }

        private native void allocate();

        private native void allocate(long n);

        public native long size();

        //public native @Index(1) void resize(long n);

        public native @Cast("bool") boolean empty();

        public native void resize(long n);

        @Name("operator[]") public native int get(long n);

        //public native @Adapter("thrust::device_vector<int>") PointerPointer at(long n);

        public native void push_back(int x);

        public native void pop_back();

        public native int front();
    }

    @Name("thrust::device_vector<float>")
    public static class DeviceVectorFloatPointer extends Pointer {
        static { Loader.load(); }

        public DeviceVectorFloatPointer() { allocate(); }

        public DeviceVectorFloatPointer(Pointer p) { super(p); }

        @Name("operator=") public native @ByRef
        DeviceVectorFloatPointer copy(@ByRef DeviceVectorFloatPointer x);

        public DeviceVectorFloatPointer(long n) { allocate(n); }

        private native void allocate();

        private native void allocate(long n);

        public native long size();

        //public native @Index(1) void resize(long n);

        public native @Cast("bool") boolean empty();

        public native void resize(long n);

        @Name("operator[]") public native int get(long n);

        //public native @Adapter("thrust::device_vector<int>") PointerPointer at(long n);

        public native void push_back(float x);

        public native void pop_back();

        public native int front();
    }


    @Name("thrust::tuple<int, int>")
    public static class TupleIntInt extends Pointer {
        static { Loader.load(); }

        public TupleIntInt() { allocate(); }

        private native void allocate();
    }

    /*
    @Name("Test")
    public static class TestClass extends Pointer {
        static { Loader.load(); }

        public TestClass() { allocate(); }

        private native void allocate();

        public native void test(DeviceVectorIntPointer x, DeviceVectorIntPointer x_out);

        public native void test1();

        public native void test2(@ByRef TupleIntInt tII);

        public native @ByPtr DeviceVectorIntPointer test3(DeviceVectorIntPointer x);
    }
    */

    @Name("thrust::make_tuple<int, int>")
    public native static @ByVal TupleIntInt makeTuple(int a, int b);

    @Name("thrust::get<0>")
    public native static int get0(@ByRef TupleIntInt tII);

    @Name("thrust::get<1>")
    public native static int get1(@ByRef TupleIntInt tII);


    // ----- Scalan-Thrust:begin -----
    @Name("scalan_thrust::base_array<float>")
    public static class BaseArrayFloat extends Pointer {
        static { Loader.load(); }

        public BaseArrayFloat() { allocate(); }

        public BaseArrayFloat(@ByRef DeviceVectorFloatPointer dvp) { allocate(dvp); }

        public native long length();

        private native void allocate();

        private native void allocate(@ByRef DeviceVectorFloatPointer dvp);

        public native void print();

        public native float get(int i);
    }

    @Name("scalan_thrust::base_array<int>")
    public static class BaseArrayInt extends Pointer {
        static { Loader.load(); }

        public BaseArrayInt() { allocate(); }

        public BaseArrayInt(@ByRef DeviceVectorIntPointer dvp) { allocate(dvp); }

        public native long length();

        private native void allocate();

        private native void allocate(@ByRef DeviceVectorIntPointer dvp);
    }

    @Name("scalan_thrust::pair_array<int, float>")
    public static class PairArrayIntFloat extends Pointer {
        static { Loader.load(); }

        public PairArrayIntFloat(@ByRef BaseArrayInt bai, @ByRef BaseArrayFloat baf) { allocate(bai, baf); }

        private native void allocate(@ByRef BaseArrayInt bai, @ByRef BaseArrayFloat baf);
    }

    @Name("scalan_thrust::nested_array<pair<int, float> >")
    public static class NestedArrayPairIntFloat extends Pointer {
        static { Loader.load(); }

        public NestedArrayPairIntFloat(PairArrayIntFloat paif, @ByRef BaseArrayInt segments) { allocate(paif, segments); }

        private native void allocate(PairArrayIntFloat paif, @ByRef BaseArrayInt segments);
    }

    @Name("scalan_thrust::pair<nested_array<pair<int, float> >, base_array<float> >")
    public static class InputType extends Pointer {
        static { Loader.load(); }

        public InputType(@ByRef NestedArrayPairIntFloat napif, @ByRef BaseArrayFloat baf) { allocate(napif, baf); }

        private native void allocate(@ByRef NestedArrayPairIntFloat napif, @ByRef BaseArrayFloat baf);
    }
    // ----- Scalan-Thrust:end -----

    @Name("fun")
    public native static @ByVal BaseArrayFloat mainFun1(@ByRef InputType input);

    private static void test1(PrintStream o) {
        DeviceVectorIntPointer vp_in = new DeviceVectorIntPointer();
        o.println(vp_in.empty());
        o.println(vp_in.size());
//        vp_in.resize(10);
//        o.println(vp_in.empty());
//        o.println(vp_in.size());

        vp_in.push_back(100);
        vp_in.push_back(20);
        vp_in.push_back(30);
        for (int i = 11; i < 28; i++) {
            vp_in.push_back(i);
        }

        o.println(vp_in.front());
        o.print("vp_in: ");
        for (int i = 0; i < vp_in.size(); i++) {
            o.print("(" + i + " | " + vp_in.get(i) + ") ");
        }
        o.println();
    }

    private static void test2(PrintStream o) {
        DeviceVectorIntPointer cols = new DeviceVectorIntPointer();
        cols.push_back(0); cols.push_back(2); cols.push_back(0); cols.push_back(1); cols.push_back(2); cols.push_back(3);

        DeviceVectorFloatPointer vals = new DeviceVectorFloatPointer();
        vals.push_back(1f); vals.push_back(2f); vals.push_back(3f); vals.push_back(4f); vals.push_back(5f); vals.push_back(6f);

        DeviceVectorFloatPointer v = new DeviceVectorFloatPointer();
        v.push_back(1f); v.push_back(2f); v.push_back(3f); v.push_back(4f);

        DeviceVectorIntPointer segs = new DeviceVectorIntPointer();
        segs.push_back(2); segs.push_back(3); segs.push_back(1);

        BaseArrayFloat res =
            ThrustLib.mainFun1(new InputType(
                new NestedArrayPairIntFloat(
                        new PairArrayIntFloat(new BaseArrayInt(cols), new BaseArrayFloat(vals)),
                        new BaseArrayInt(segs)),
                new BaseArrayFloat(v)));
        res.print();
    }

    public static void main(String... args) {
        PrintStream o = System.out;

        //System.setProperty("java.library.path", System.getProperty("java.library.path") + ":/host/Keldysh/prj/Scalan-v2/tmp");
        o.println(System.getProperty("java.library.path"));

        //test1(o);
        test2(o);
    }
}