// NOTE: This is implementation when SVMV operation is encoded in form of FlatGraph nodes
package scala.virtualization.lms

import internal._
import scalan.staged._
import scalan.samples.DslSamples
import scalan.dsl.ArraysBase
import java.io.{FileWriter, ByteArrayOutputStream, PrintWriter}
import scalan.dsl._
import scalan.common.Monoid
import main.scala.gpugen.ThrustLib
import scalan.sequential.ScalanSequential

class GenerationFailedException(msg: String) extends Exception(msg)

trait GpuArrayOperations extends ScalanStaged {
  // TODO: Should be DSL instead of direct graph nodes generation.
  def arraySum[T](s: Sym[Array[T]])(implicit m: Monoid[T]) = ArraySum(s, m)

  def sumLifted[B](s: PA[PArray[B]])(implicit e: Elem[B], m: Monoid[B]) = SumLiftedPA(s, m)

  //  def smvm: Rep[(PArray[PArray[(Int, Float)]], PArray[Float]) => PArray[Float]] = {
  //    val input = fresh[Pair[PArray[PArray[Pair[Int, Float]]], PArray[Float]]]
  //
  //    val m = First(input)
  //    val v = Second(input)
  //
  //    val naVals = NestedArrayValues(m)
  //    val bp = BackPermute(v, FirstPA(naVals))
  //    val ba = ExpBinopArray(NumericPlus(Const(0f), Const(0f), null), bp, SecondPA(naVals))
  //    val res: PA[Float] = sumLifted(ExpNestedArray(ba, NestedArraySegments(m)))
  //
  //    val lam = Lambda(null, input, res)
  //    lam
  //  }

  lazy val smvm = mkLambda((input: Rep[Pair[PArray[PArray[(Int, Float)]], PArray[Float]]]) => {
    //val input = fresh[Pair[PArray[PArray[Pair[Int, Float]]], PArray[Float]]]

    val m = First(input)
    val v = Second(input)

    val naVals = NestedArrayValues(m)
    val bp = BackPermute(v, FirstPA(naVals))
    val ba = ExpBinopArray(NumericPlus(Const(0f), Const(0f), null), bp, SecondPA(naVals))
    val res: PA[Float] = sumLifted(ExpNestedArray(ba, NestedArraySegments(m)))
    res
    //val lam = Lambda(null, input, res)
    //lam)
  })
}

trait GpuGenImproved extends GenericCodegen {
  self: ArraysBase with StagedImplementation =>

  //var globDefsArr: Array[TP[_]] = null

  def remap[A](m: Manifest[A]): String = m.toString match {
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case "Unit" => "void"
    case "java.lang.String" => "char *"
    case "Array[Float]" => "device_vector<float>*"
    case "Array[Int]" => "device_vector<int>*"
    case "scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]]" => "nested_array<pair<int, float> >"
    case "scalan.dsl.ArraysBase$PArray[Float]" => "base_array<float>" // TODO: Fix Thrust-library then this line. Should be 'parray<float>*'
    case _ => throw new GenerationFailedException("CGen: remap(m) : Unknown data type (%s)".format(m.toString))
  }

  override def emitNode(s: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Unit = {
    rhs match {
      case (c: Const[_]) =>
      //stream.println(quote(s) + " = const(" + c.x + ")")

      case (fst: First[_, _]) =>
        val tp = remap(fst.pair.Elem.manifest.typeArguments(0))
        stream.println(tp + " " + quote(s) + " = " + quote(fst.pair) + ".fst();")

      case (snd: Second[_, _]) =>
        val tp = remap(snd.pair.Elem.manifest.typeArguments(1))
        stream.println(tp + " " + quote(s) + " = " + quote(snd.pair) + ".snd();")

      case (sl: SumLiftedPA[_]) =>
        val tp = "base_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(tp + " " + quote(s) + " = sum_lifted(" + quote(sl.source) + ");")

      case (na: ExpNestedArray[_]) =>
        val tp = "nested_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(tp + " " + quote(s) + " = " + tp + "(&" + quote(na.arr) + ", " + quote(na.segments) + ");")

      case (ba: ExpBinopArray[_]) =>
        // TODO: analyse ba.op
        val tp = "base_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(tp + " " + quote(s) + " = binop_array(" + quote(ba.lhs) + ", " + quote(ba.rhs) + ");")

      case (nav: NestedArrayValues[_]) =>
        val tp = nav.nested.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]]" =>
            "pair_array<int, float>"
          case _ => !!!("Unsupported")
        }
        stream.println(tp + " " + quote(s) + " = " + quote(nav.nested) + ".values();")

      case (nas: NestedArraySegments[_]) =>
        val tp = nas.nested.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]]" =>
            "base_array<int>"
          case _ => !!!("Unsupported")
        }
        stream.println(tp + " " + quote(s) + " = " + quote(nas.nested) + ".segments();")

      case (bp: BackPermute[_]) =>
        val tp = "base_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(tp + " " + quote(s) + " = " + quote(bp.x) + ".back_permute(" + quote(bp.idxs) + ");")

      case (fpa: FirstPA[_, _]) =>
        val tp = fpa.source.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]" =>
            "base_array<int>"
          case _ => !!!("Unsupported")
        }
        stream.println(tp + " " + quote(s) + " = " + quote(fpa.source) + ".first();")

      case (spa: SecondPA[_, _]) =>
        val tp = spa.source.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]" =>
            "base_array<float>"
          case _ => !!!("Unsupported")
        }
        stream.println(tp + " " + quote(s) + " = " + quote(spa.source) + ".second();")

      case _ => super.emitNode(s, rhs)
    }
  }

  def remapOp(op: String) = op match {
    case "+" => "thrust::plus"
    case _ => !!!("Unexpected operation")
  }

  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }

  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }

  def emitAssignment(lhs: String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs)
  }
}

object GpuGenTest {
  val oGpu = new GpuArrayOperations with GpuGenImproved
  val seq = new ScalanSequential {}

  def main(args: Array[String]): Unit = {
    import seq._
    //import oGpu._

    //val f: Pair[PA[PA[Pair[Int, Float]]], PA[Float]] => PA[Float] = compile1(seq)(oGpu.smvm)
    val f = compile1[((oGpu.PArray[oGpu.PArray[(Int, Float)]], oGpu.PArray[Float])) => oGpu.PArray[Float],
      ((seq.PArray[seq.PArray[(Int, Float)]], seq.PArray[Float])) => seq.PArray[Float]](seq)(oGpu.smvm)

    val colsArr: Array[Int] = Array(0, 2, 0, 1, 2, 3)
    val cols: PA[Int] = fromArray(colsArr)

    val valsArr: Array[Float] = Array(1f, 2f, 3f, 4f, 5f, 6f)
    val vals: PA[Float] = fromArray(valsArr)

    //val rowsArr: Array[(Int, Float)] = Array((0, 1f), (2, 2f), (0, 3f), (1, 4f), (2, 5f), (3, 6f))
    // TODO: Why can't be written: fromArray(Array((0, 1f), (2, 2f), (0, 3f), (1, 4f), (2, 5f), (3, 6f)))
    //val rows: PArray[Pair[Int, Float]] = fromArray(rowsArr)
    val rows: PA[(Int, Float)] = cols zip vals

    val segsLenArr: Array[Int] = Array(2, 3, 1)
    val segsLen: PA[Int] = fromArray(segsLenArr)
    val segsIdxArr: Array[Int] = Array(0, 2, 5)
    val segsIdx: PA[Int] = fromArray(segsIdxArr)
    val segs: PA[Pair[Int, Int]] = segsIdx zip segsLen

    val m: PA[PA[(Int, Float)]] = mkNestedArray(rows, segs)

    val vArr: Array[Float] = Array(1f, 2f, 3f, 4f, 5f)
    val v: PA[Float] = fromArray(vArr)

    //val input: Pair[PA[PA[Pair[Int, Float]]], PA[Float]] = (m, v)
    //val res = f(input)
    val res = f(m, v)
    System.out.println(res)
  }

  /*
  def generateFunSignature(sx: Sym[_], eRes: Elem[_])(implicit stream: PrintWriter): Unit = {
    // TODO: Any better way to analyze types?
    // TODO: Should be pattern: stream.println(remap(RES_TYPE_str) + " test(" + remap(IN_TYPE_str) + ") {")
    eRes.manifest.erasure.getSimpleName match {
      case "int" => stream.print(remap(eRes.manifest) + " ")
      case _ => !!!("Unexpected. Implement it")
    }

    sx.Elem.manifest.erasure.getSimpleName match {
      case "int[]" => stream.println("test(" + remap(sx.Elem.manifest) + " " + quote(sx) + ") {")
      case _ => !!!("Unexpected type")
    }
  }
  */

  //def compile1[A1, A2, B](seq: ScalanSequential)(lam: oGpu.Lambda[(oGpu.PArray[A1], oGpu.PArray[A2]), oGpu.PArray[B]]): (seq.PA[A1], seq.PA[A2]) => seq.PA[B] =
  //def compile1[A, B](seq: ScalanSequential)(lam: oGpu.Lambda[oGpu.PArray[A], oGpu.PArray[B]]): seq.PA[A] => seq.PA[B] =
  def compile1[A, B](seq: ScalanSequential)(l: oGpu.Rep[A]): seq.Rep[B] = {
    //def compile1[A1, A2, B](seq: ScalanSequential)(lam: oGpu.Rep[(oGpu.PArray[A1], oGpu.PArray[A2]) => oGpu.PArray[B]]): (seq.PA[A1], seq.PA[A2]) => seq.PA[B] = {

    import oGpu._

    //globDefsArr = globalDefs.toArray

    val bytesStream = new ByteArrayOutputStream
    val stream = new PrintWriter(bytesStream, true) {
      override def println(s: String) = {
        //System.out.println(s)
        super.println(s)
        //System.out.println()
      }

      override def print(s: String) = {
        //System.out.print(s)
        super.print(s)
      }
    }

    val thrust_lib_code = io.Source.fromFile("src/scalan-thrust-lib.cpp")
    stream.println("// ----------------------------------------")
    stream.println("// ----- Scalan-Thrust CUDA/C library -----")
    stream.println("// ----------------------------------------")
    stream.println(thrust_lib_code.mkString)
    thrust_lib_code.close
    stream.println("// ----------------------------------------")

    findDefinition(l.asInstanceOf[Sym[_]]).get.definition.get match {
      case lam: Lambda[_, _] =>
        val tp = lam.y.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[Float]" =>
            "base_array<float>"
        }

        lam.x.Elem.manifest.toString match {
          case "scala.Tuple2[scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]], scalan.dsl.ArraysBase$PArray[Float]]" =>
            stream.println(tp + " fun(const pair<nested_array<pair<int, float> >, base_array<float> >& " + quote(lam.x) + ") {")
        }
        emitBlock(lam.y)(stream)
        stream.println("return " + quote(lam.y) + ";")
        stream.println("}")

        stream.println( """
     #define FLOAT_EQ(x, y) fabs((x) - (y)) < 0.001f
     void test_sum() {
       host_vector<int> x5(10, 5);
       base_array<int> x6(x5);
       int x7 = x6.sum(monoid(0.f, monoid::OP_PLUS));
       assert(FLOAT_EQ(x7, 0.f + 5.f * 10.f));
     }

     int test(device_vector<int>* input) {
        return input->size();
      }

    int main() {
    // init
    host_vector<int> cols_h(6);
    cols_h[0] = 0; cols_h[1] = 2; cols_h[2] = 0; cols_h[3] = 1; cols_h[4] = 2; cols_h[5] = 3;

    host_vector<float> vals_h(6);
    vals_h[0] = 1.f; vals_h[1] = 2.f; vals_h[2] = 3.f; vals_h[3] = 4.f; vals_h[4] = 5.f; vals_h[5] = 6.f;

    host_vector<float> v_h(4);
    v_h[0] = 1.f; v_h[1] = 2.f; v_h[2] = 3.f; v_h[3] = 4.f;

    host_vector<int> segs_h(3);
    segs_h[0] = 2; segs_h[1] = 3; segs_h[2] = 1;

    base_array<float> vals(vals_h), v(v_h);
    base_array<int> cols(cols_h), segs(segs_h);
    pair_array<int, float> rows(cols, vals);
    nested_array<pair<int, float> > m(&rows, segs);

           base_array<float> res = fun(pair<nested_array<pair<int, float> >, base_array<float> >(m, v));

      // verify
      assert(res.length() == segs.length());
      assert(FLOAT_EQ(res.data()[0], 7.f));
      assert(FLOAT_EQ(res.data()[1], 26.f));
      assert(FLOAT_EQ(res.data()[2], 24.f));

      std::cout << "OK!";

          return 0;
          }""")

        stream.flush
        val programText = new String(bytesStream.toByteArray)
        //System.out.println(programText)

        val fw = new FileWriter("tmp/fun.cpp")
        fw.write(programText)
        fw.flush
        fw.close

      case _ =>
        !!!("Not implemented")
    }

    val r: ((seq.PArray[seq.PArray[(Int, Float)]], seq.PArray[Float])) => seq.PArray[Float] = (x1: (seq.PArray[seq.PArray[(Int, Float)]], seq.PArray[Float])) => {
      import seq._
      x1 match {
        //    case (x: Float) =>
        //      !!!("not implemented")
        //    case (x: Int) =>
        //      !!!("not implemented")
        //    case (arr: Array[Float]) =>
        //      !!!("Unexpected type")
        //    case (x: Array[Int]) =>
        //      !!!("not implemented")
        case input: (Pair[PArray[PArray[(Int, Float)]], PArray[Float]]) =>
          import ThrustLib._

          val m = input._1
          val v = {
            val x = new DeviceVectorFloatPointer()
            input._2.toArray.foreach(v_i => x.push_back(v_i))
            x
          }

          val cols = new DeviceVectorIntPointer()
          val vals = new DeviceVectorFloatPointer()
          val segs = new DeviceVectorIntPointer()
          m.toArray.foreach(parr => {
            segs.push_back(parr.length)
            parr.toArray.foreach(v => {
              cols.push_back(v._1)
              vals.push_back(v._2)
            })
          })

          val resBA: BaseArrayFloat = mainFun1(
            new InputType(
              new NestedArrayPairIntFloat(
                new PairArrayIntFloat(
                  new BaseArrayInt(cols), new BaseArrayFloat(vals)),
                new BaseArrayInt(segs)), new BaseArrayFloat(v)))

          val resA = 0.until(resBA.length().toInt).map(i => resBA.get(i)).toArray
          seq.SeqStdArray(resA)(seq.floatElement)
        case _ =>
          !!!("Unexpected type")
      }
    }
    r.asInstanceOf[B]
  }

  /*
  def compile[A, B](lam: Rep[A => B])(implicit eA: Elem[A], eB: Elem[B]): A => B = {
    val bytesStream = new ByteArrayOutputStream
    val stream = new PrintWriter(bytesStream, true) {
      override def println(s: String) = {
        //System.out.println(s)
        super.println(s)
        System.out.println()
      }

      override def print(s: String) = {
        System.out.print(s)
        super.print(s)
      }
    }
    //val stream = new PrintWriter(System.out, true)
    //stream.println(globalDefs.mkString("globalDefs:[", ", ", "]"))

    val x = fresh[A]
    val y: Rep[B] = lam(x)

    System.out.println("x: " + x.toString)
    System.out.println("y: " + y.toString)
    System.out.println(globalDefs.mkString("globalDefs:[\n\t", ",\n\t", "\n]"))

    //val sA = eA.manifest.toString
    //val sB = eB.manifest.toString

    globDefsArr = globalDefs.toArray

    stream.println("/*****************************************\n" +
      "  Emitting Generated Code                  \n" +
      "*******************************************/")

    stream.println( """
  #include <thrust/device_vector.h>
  #include <thrust/transform.h>
  #include <thrust/sequence.h>
  #include <thrust/copy.h>
  #include <thrust/fill.h>
  #include <thrust/replace.h>
  #include <thrust/functional.h>
  #include <iostream>

  using namespace thrust;
                    """)

    generateFunSignature(x, eB)(stream)
    emitBlock(y)(stream)
    val resq = quote(getBlockResult(y))
    //    stream.println("out_size = " + resq + ".size();")
    //    stream.println("float* " + resq + "_tmp = new float[out_size];")
    //    stream.println("thrust::copy(" + resq + ".begin(), " + resq + ".end(), " + resq + "_tmp);")
    //    stream.println("return " + resq + "_tmp;")
    stream.println("return " + resq + ";")

    // Test output
    /*
    stream.println("""
  device_vector<int>* test(device_vector<int>* x) {
  device_vector<int>* x_out = new device_vector<int>(x->size());
  thrust::transform(x->begin(), x->end(), x_out->begin(), thrust::negate<int>());
  //cout << "from C++: " << flush;
  //for (int i = 0; i < x_out->size(); i++) {
  //    cout << (*x_out)[i] << " " << flush;
  //}
  return x_out;""")
  */

    stream.println("}")
    stream.println(
      "/*****************************************\n" +
      "  End of Generated Code                  \n" +
      "*******************************************/")

    // stream.flush // autoflush <- true

    val programText = new String(bytesStream.toByteArray)
    //System.out.println(programText)
    val fw = new FileWriter("tmp/fun.cu")
    fw.write(programText)
    fw.flush
    fw.close

    val r: A => B = (x: A) => {
      x match {
        case (x: Float) =>
          !!!("not implemented")
        case (x: Int) =>
          !!!("not implemented")
        case (arr: Array[Float]) =>
          /*
          val len_out = new IntByReference(0)
          val res = CLibrary.FunLibInstance.fun(arr, arr.length, len_out)
          val arr_out = res.getPointer.getFloatArray(0, len_out.getValue)
          arr_out.asInstanceOf[B] // TODO: How to return PArray(Array(Float))
          */
          !!!("Unexpected type")
        case (x: Array[Int]) =>
          val vp_in = new ThrustLib.DeviceVectorIntPointer()
          (0 to 128).foreach(x => vp_in push_back x) // TODO: replace (0 to 128) with x.
          val out = ThrustLib.test(vp_in)
          System.out.println(out)
          !!!("not implemented") // TODO: return out
        case _ =>
          !!!("Unexpected type")
      }
    }
    r
  }
  */
}
