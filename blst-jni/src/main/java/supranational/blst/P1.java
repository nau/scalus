/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package supranational.blst;

public class P1 {
  private transient long[] swigCPtr;

  protected P1(long[] cPtr) { swigCPtr = cPtr; }

  protected static long[] getCPtr(P1 obj) {
    return obj != null ? obj.swigCPtr : null;
  }

  public P1 dup() { return new P1(swigCPtr.clone()); }

  public P1() { this(blstJNI.new_P1__SWIG_0()); }

  public P1(SecretKey sk) { this(blstJNI.new_P1__SWIG_1(SecretKey.getCPtr(sk))); }

  public P1(byte[] in) { this(blstJNI.new_P1__SWIG_2(in)); }

  public P1(P1_Affine affine) { this(blstJNI.new_P1__SWIG_3(P1_Affine.getCPtr(affine))); }

  public P1_Affine to_affine() {
    return new P1_Affine(blstJNI.P1_to_affine(swigCPtr));
}

  public byte[] serialize() { return blstJNI.P1_serialize(swigCPtr); }

  public byte[] compress() { return blstJNI.P1_compress(swigCPtr); }

  public boolean on_curve() {
    return blstJNI.P1_on_curve(swigCPtr);
  }

  public boolean in_group() {
    return blstJNI.P1_in_group(swigCPtr);
  }

  public boolean is_inf() {
    return blstJNI.P1_is_inf(swigCPtr);
  }

  public boolean is_equal(P1 p) {
    return blstJNI.P1_is_equal(swigCPtr, P1.getCPtr(p));
  }

  public void aggregate(P1_Affine in) {
    blstJNI.P1_aggregate(swigCPtr, P1_Affine.getCPtr(in));
  }

  public P1 sign_with(SecretKey sk) { blstJNI.P1_sign_with__SWIG_0(swigCPtr, SecretKey.getCPtr(sk)); return this; }

  public P1 sign_with(Scalar scalar) { blstJNI.P1_sign_with__SWIG_1(swigCPtr, Scalar.getCPtr(scalar)); return this; }

  public P1 hash_to(byte[] msg, String DST, byte[] aug) { blstJNI.P1_hash_to__SWIG_0(swigCPtr, msg, DST, aug); return this; }

  public P1 hash_to(byte[] msg, String DST) { blstJNI.P1_hash_to__SWIG_2(swigCPtr, msg, DST); return this; }

  public P1 hash_to(byte[] msg) { blstJNI.P1_hash_to__SWIG_3(swigCPtr, msg); return this; }

  public P1 encode_to(byte[] msg, String DST, byte[] aug) { blstJNI.P1_encode_to__SWIG_0(swigCPtr, msg, DST, aug); return this; }

  public P1 encode_to(byte[] msg, String DST) { blstJNI.P1_encode_to__SWIG_2(swigCPtr, msg, DST); return this; }

  public P1 encode_to(byte[] msg) { blstJNI.P1_encode_to__SWIG_3(swigCPtr, msg); return this; }

  public P1 mult(java.math.BigInteger scalar) { blstJNI.P1_mult__SWIG_0(swigCPtr, scalar.toByteArray()); return this; }

  public P1 mult(Scalar scalar) { blstJNI.P1_mult__SWIG_1(swigCPtr, Scalar.getCPtr(scalar)); return this; }

  public P1 cneg(boolean flag) { blstJNI.P1_cneg(swigCPtr, flag); return this; }

  public P1 neg() { blstJNI.P1_neg(swigCPtr); return this; }

  public P1 add(P1 a) { blstJNI.P1_add__SWIG_0(swigCPtr, P1.getCPtr(a)); return this; }

  public P1 add(P1_Affine a) { blstJNI.P1_add__SWIG_1(swigCPtr, P1_Affine.getCPtr(a)); return this; }

  public P1 dbl() { blstJNI.P1_dbl(swigCPtr); return this; }

  public static P1 generator() {
    return new P1(blstJNI.P1_generator());
}

  public static P1_Affine uncompress(byte[] in) {
    return new P1_Affine(blstJNI.P1_uncompress(in));
}

}
