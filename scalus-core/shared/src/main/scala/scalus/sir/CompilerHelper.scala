package scalus.sir

object CompilerHelper {

    /** @param codedSir - sir encoded as String in Latin1 encoding.
      * @param originTree - the tree from which the SIR was generated, used to keep it in code, to allow
      *                   compiler to maintain the dependencies. Note, that this code can generate exception when
      *                    called, so it passed by name to not trigger execution.
      * @return decoded SIR as String.
      */
    def decodeSirStringWithOrigin(codedSir: String, originTree: => Any): SIR = {
        scalus.sir.ToExprHSSIRFlat.decodeStringLatin1(codedSir)
    }

}
