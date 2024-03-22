package scalus.uplc

import org.scalacheck.Prop
import org.scalacheck.Properties
import scalus.*
import scalus.utils.Utils
import org.scalatest.funsuite.AnyFunSuite

class ProgramFlatSpec extends Properties("ScalaCheck-scalajs") with ArbitraryInstances {

    propertyWithSeed(
      "Program flat encoding is identical to Plutus",
      Some("9wHoDRPcC1XMs3Ub_zJYQQqw1KLZQMRnXm8hPYsmV9D=")
    ) = Prop.forAll { (p: Program) =>
        val str = p.pretty.render(80)
        val bytes = UplcCli.uplcToFlat(str)
        Utils.bytesToHex(bytes) == Utils.bytesToHex(p.flatEncoded)
    }

}

class BugSpec extends AnyFunSuite {
    test("Bug") {
        import scalus.uplc.*, Term.*, DefaultFun.*
        import scalus.builtin.ByteString.StringInterpolators
        import scalus.builtin.Data
        import scalus.builtin.Data.{B, I, Constr}
        val p = Program(
          (43, 38, 18),
          Const(
            Constant.Data(
              Data.List(
                List(
                  Data.List(
                    List(
                      B(
                        hex"FD6E21FFFFE9BC428000F38C640180400100019100A97F807F7F3F9F000080807F0901FF804C4DB864800A9E7F8000877F0D7FDF00AE00FFC2017F3385B200B6DE0101808028CE011E07807F287F35D3CFFFE67F0880"
                      ),
                      Data.List(
                        List(
                          B(
                            hex"8A5C7FD0750300D0E206806DFFFF8AFF9BE15269C6018000A2011480018DFFFFCC01808036004B7E3601FF8B9D7F"
                          ),
                          I(1),
                          B(hex"017FEDC980580180FFFFC5FF38010080000001BD0180FF77"),
                          Data.List(List(I(1000), Data.Map(Nil)))
                        )
                      ),
                      B(hex"4E1454FF835759B9E5FF007F7F7F9480500101C803BD80971E01FF"),
                      I(BigInt("9223372036854775807")),
                      I(0),
                      B(
                        hex"F47CC8E5C7007F7FE2B57F7F0106FFFF1380FF9D85B87F486680034005800151011F8CDF09008F00EB826100E10080A601800F80803C4EFA303D8EF33AE05E7F45FF9AD33000DF857414560180"
                      )
                    )
                  ),
                  B(
                    hex"7FDF0001D33F6D15D8007FFFAC017001FF1BAC657F211D7B4211370080F401C0A7807F0002A1203343803C7F927FE680809FCA7F58234F00243301FFA2ECFF5D0000FF01FF80FF8096016F014DDFFF7F0000677F7F8080A46900459E"
                  ),
                  B(
                    hex"01709D04FFFF572080FFDE017F7F48338001808AD8478000FFFF7F0000001BF31F240100C27F2180800001009001D4D980F2AA7F9C8E000E5B00A04D7FEAD6BA3DC31E4C80D2FF990110"
                  ),
                  Data.List(
                    List(
                      Constr(
                        27,
                        List(
                          Data.Map(
                            List(
                              (
                                B(
                                  hex"0088ECFF01C7FA802D8080FFCA01D2018BFFF3010100A87F0001018101802C"
                                ),
                                Data.Map(Nil)
                              )
                            )
                          ),
                          B(
                            hex"FFFF5E017FFF24BFFFBBE915018DBD825501D580B980017F00630BFF00017FED81FF80FF933AE3FF00804ACC3ECA00FF417815E699A501F4010001FF7F80018B5DFF00007F7F80809980FF00B9FFA180017F80015B"
                          ),
                          Data.List(
                            List(
                              B(
                                hex"FFAAF39D7F3CD7FF6A3501AEFF01D500010001DC2BFF877F1D0100189A338701017FF41438387F7F9FFF"
                              ),
                              B(
                                hex"01B37F05A2FF7F7901078080560101EF80BE63FFBF00257F141D010001FF7F951CFF80807F807FFF41F9016680D40480C39D0135FF7FF639F980FF796A7F03CFF6CA8034EC8062000101FFD77F06FCFF7F4257C0E5DD"
                              )
                            )
                          ),
                          Data.Map(
                            List(
                              (
                                B(
                                  hex"6F0001C2FF4F013EFF7F7F3A017F00207F10DD8CA36E01A2FFFF7F807FCE96F100D46F6AE4CE9319007F62E301FF888A80019B0000CC9FAE808D65E080808F80BE1E08807FD4ED00C5DE80C780337AC1004BD3B380C000CB5880"
                                ),
                                B(hex"DB018000B9B451D0003EA4002D3901808000FFFA7FFF9A01")
                              )
                            )
                          )
                        )
                      ),
                      Constr(
                        16,
                        List(
                          I(1),
                          Constr(1, List(I(BigInt("-9223372036854775808")))),
                          I(0),
                          I(BigInt("2147483647")),
                          B(
                            hex"7B88FFC0009F80D6FC1B9533AB64808069007FFF43C3010C367F00D2E4005601D78085005C01"
                          )
                        )
                      ),
                      Data.Map(
                        List(
                          (
                            Data.Map(
                              List(
                                (
                                  I(0),
                                  B(
                                    hex"007FFF00E86E007FFFD78C8F01802A7F01A7D2007F00533100719800DD8011C87FFF222C9ADC0001FD1480FF0180014363FF7F41AC8080FF9E"
                                  )
                                )
                              )
                            ),
                            I(BigInt("9223372036854775807"))
                          ),
                          (
                            I(1),
                            Data.Map(
                              List(
                                (
                                  B(hex"B1823A"),
                                  B(
                                    hex"3780C4FFBC7F4D73FF00007F80B8007F801C3201FF007F80800080BF00FF00F4AF56F5BEFFD0D7E480F101FF"
                                  )
                                ),
                                (
                                  I(1),
                                  B(
                                    hex"0113015D97807E82803B7F7FFBB57FFF017F526D7F957F30D0466B1A017F800000FF80FF7DCF008000067F0001BE7F900000017FB9"
                                  )
                                )
                              )
                            )
                          ),
                          (I(BigInt("2147483647")), I(-1))
                        )
                      ),
                      B(
                        hex"A597FFF201FF8000351F0049808F7F01228D016A448075E684FFF3BB80017D0100E00001ED7FFF7F72290080FF25867A7F8F80B6807FD48080C8807F257F4745809080F47F8C80E07F7FFF005FFD7F00C6010178FF7F2EFFDB80A400"
                      ),
                      B(
                        hex"01B5A0047FB92D01FFFFEC49BF001AFF01FF1A00007FA27F257F7F97FF00FF7F80ACBE6DFF804C01B1322F00C91E7BFF80FF7FD6FF7F7F8080809397D705B7FF80B38D4D7FE18000C26EFF4A42177FFFFF007F8001"
                      ),
                      B(
                        hex"7F80FF00FF1A4900FFE8FCFF80DDC4018000D0006B807FA4FF7FFFFF7F00FF007FFFFF7F0000977F7288420101FF"
                      ),
                      B(hex"8039FF84C2FA51AB"),
                      B(
                        hex"7F583E00C780D3FF019F0D7F1929C801807F730D0153E07FA8004380B9D5FF6500705A51008041360000FF4E8089CE80AFFF2701EAFFFF1A0101011580"
                      )
                    )
                  ),
                  B(hex"0165D201E9D500A47F1ADFFF0100D5"),
                  Data.Map(
                    List(
                      (
                        B(
                          hex"837F00808080807F88004A000E80B6D0007F6F8000F201C5228CD74301C8FF80FF44CAFF8000017300EA6ED200017FAB00807FFF80BB8001018B802C6EF496FF7101FF7FC80034AF800101FF00F2034A694C1E5B80017F1EFFA9EE8D018E3CD801"
                        ),
                        Data.List(
                          List(
                            Constr(
                              32,
                              List(
                                B(
                                  hex"FF31807FE2000100007F007F0080827FB3008DF37F8000FFFF8D5A6180FF7FAD34019F010138B46A01F58048015959FFB2F02B9D415D80"
                                ),
                                B(hex"8001017F0077FF")
                              )
                            ),
                            I(-1000),
                            Data.List(
                              List(
                                B(
                                  hex"8080019F017F20F18C80B66BEDCA7A005701FF207FD47FFFA4E27F0001D7327F947F00F080057F7101DC9CF37F523218D5E22A80FF7FA7BA84BE3570FFFF7FB07FFF2AFFFFFF7F1E0000011E000B8A54728084002380009D01"
                                ),
                                B(
                                  hex"A0807FC0C5002F328000188082DC7F007F152501610029177F33EB947F7F4E3722EF4A710171490080D35A9580018027FD800017015AFF7F00822694403101677F800023C97F80F47CFF01DAFF8000FF01177FE380143D4780FFF99C00484163"
                                )
                              )
                            )
                          )
                        )
                      ),
                      (
                        Data.List(
                          List(
                            Constr(35, List(B(hex"FFCEFFFA807F007F35D1"))),
                            Constr(
                              23,
                              List(Data.List(List()), B(hex"118080A2F2C0FFC0017CBA00E3"))
                            ),
                            I(-1)
                          )
                        ),
                        I(55)
                      ),
                      (B(hex"FF5ABF7F02DAFF67F70001B85C017FFF1C13"), I(1)),
                      (
                        I(0),
                        Data.Map(
                          List(
                            (
                              Constr(16, List(B(hex"D40006"), I(1))),
                              B(hex"FF8096A20A7FA5644EE41D")
                            ),
                            (
                              B(
                                hex"52A82478019880016F80D1BCF345014EF91F80807F007FA39BAD85FF004F7F8001D07F006AABFF80FF0190FF023D00FFCF8048214D0180484C527A994827FF43B20080440080D1E77FB88B8001017F018280FF019833DB8DFF00FF1E2C3701"
                              ),
                              Data.List(List(I(BigInt("2147483647"))))
                            ),
                            (
                              Data.Map(
                                List(
                                  (I(0), Data.Map(Nil)),
                                  (I(BigInt("9223372036854775807")), I(0))
                                )
                              ),
                              B(
                                hex"7F41FF0180D680FF007F4CFF0E8093AF80E57F42CBBAF5D8010001FFBE161B01C8BC01013201F04D0080A9D1809D01A0230180107F007F80347FEB0001600174FFAC977F51F4DB69007F01017F01FFEA720100"
                              )
                            ),
                            (
                              Data.List(List(Data.Map(Nil))),
                              B(
                                hex"B27AFFF8017F49F67A6A00B0FF01A0FFB43A0101B44AF87F21777FFF09FF7E00FF01AB0180B1527F59687F4B797F155E788684FF0001D6F78080807F9C7BE5DF0180B1008007"
                              )
                            ),
                            (
                              I(35),
                              B(
                                hex"017F00DB3800FF9FDB801C2C017D7FA601FF01AAFF01FAB23180C9EA7F920D7FE5008080E0A8B9B4100101DC7FDA9336829D012BFC00007FF1018E2A80BEFF947697B491018000447F007F0000F6D100005880"
                              )
                            )
                          )
                        )
                      ),
                      (
                        B(
                          hex"00807F1F29FF7EA81E00F19DA3807F7A01007BFC547F807F0080D85F80CCB64D7F7F010101303D7F9D012832FF012EB1011A3500D009007FB40180D9F5830180FF6201FF00B1017F7F00017FFF0879DE661E7F010100F47F80013344F0A8FFFF777F18FF"
                        ),
                        B(
                          hex"8D80FFFFFBFFFFEC79CA44802C805D0001BA00807FE33C7F6E0C5580B77FA7C980D17F9FFF639A7F3400F0F5DE5CA70500FFD4DB1292F258804C53800BFF"
                        )
                      ),
                      (
                        Data.List(
                          List(
                            B(
                              hex"ED80DC00800101397C7030010000380101FF7F5A80CE7F806A7F7F8180CB64F5807FFF5E800180FEAA95016125FDE401327F0080807F785C74C98080142680017FF4C04E80006301808022800EA0017FFB79CD01C8015800"
                            ),
                            Data.Map(
                              List(
                                (
                                  B(hex"80008078E0E7A857012F7F80010D50498001FF29E454"),
                                  I(BigInt("-9223372036854775808"))
                                )
                              )
                            ),
                            Constr(
                              26,
                              List(Constr(41, Nil), Data.List(List(Constr(42, Nil))))
                            ),
                            Constr(42, List(Data.Map(Nil)))
                          )
                        ),
                        I(-1)
                      ),
                      (
                        B(hex""),
                        B(hex"00FF0116278036666A008EFFCDDE41D05CB9B0802E198035007F0D7F39")
                      ),
                      (
                        I(BigInt("2147483647")),
                        Constr(
                          19,
                          List(
                            Data.Map(
                              List(
                                (
                                  I(BigInt("2147483647")),
                                  B(
                                    hex"01F862DB7F00FFFF409C808080C400FF01157149D53E050001007F6B00FF5701AB00134C89EBA2FF7F80832F012BFFFEFF01000001B2AA6EFF01"
                                  )
                                )
                              )
                            ),
                            B(
                              hex"FF7F64012AC913A72880808C987F7F8801FF00DBB4AB3701E20C80CEFF7A7FAD01005E01C96A"
                            ),
                            I(-1),
                            Data.Map(
                              List(
                                (
                                  B(
                                    hex"9FB8008035174F0100FFC5F5AC0C7F50001A76D34C002B014911FF36845B2BFF0008BE4225010001"
                                  ),
                                  B(hex"FFFF1F46F8CD")
                                ),
                                (I(1000), Data.List(List()))
                              )
                            ),
                            Data.Map(
                              List(
                                (B(hex"B54CC9"), I(BigInt("9223372036854775807"))),
                                (B(hex"FF0080907F80E180C0FFB8FF000D00"), I(0))
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  Data.Map(
                    List(
                      (
                        Data.List(
                          List(
                            B(hex"D70100804E7001FFA500FFFF01"),
                            Data.Map(
                              List(
                                (
                                  B(
                                    hex"807F0B3EFF6E5F7FFFA100004B3A7FFFA8007E20800C016D0B7F8080AFFF0001CEFFDF0771FF6EFF80CD01B0010001F30149007F8EFF7F7F005D000180D5C71A009B7FFFDC1C017F22997F7FF9B44400FB2880800123FF14011E017F"
                                  ),
                                  Data.List(List(Data.List(List())))
                                ),
                                (
                                  Data.Map(
                                    List(
                                      (
                                        B(
                                          hex"C97F007B01FE7B48647F807F7FC569348B5CFF0A005122B490FF800001"
                                        ),
                                        Constr(21, Nil)
                                      )
                                    )
                                  ),
                                  Data.Map(List((Data.List(List()), Data.List(List()))))
                                )
                              )
                            ),
                            B(
                              hex"7FE7FF80015D7FFC00FF011119306A1D8055C6FF51FFFD0100C07F43FF7F2580808E80830101AA80DC53F3FF011D034D277F12FF7F3E4A00FF840AE17F7F"
                            ),
                            I(-1000),
                            I(1000)
                          )
                        ),
                        B(
                          hex"FF31FF7F1C32DA0101C5F8016A01518084C1B0FFFF8080D8FFBC7F7F8001FA0154949AD4FF73CA790001C5FF808000B006BEFFFF627F93015DFF00FF7F807F004601B7D1FFB3FF6C0980FF80FF007F017E"
                        )
                      ),
                      (
                        Data.List(
                          List(
                            B(
                              hex"00DB00808080057F018A0DA87F0053A3C3BD55FF7F7FBAFF007FDF7F8F4E8180157F7FF80180AD00D280900180BF8000FF017FECBA11FF4A977F0000BD01829A00FF7F3DFFA47C"
                            ),
                            Data.Map(
                              List(
                                (
                                  Data.List(List()),
                                  B(
                                    hex"FFA17F63633F8E0013017FFF018C70E1007FE301B57F803000E500648001FF84C30157F7C58B01FE7F000101FFE36A086DDDB8ECFB2E7F8F8D5669FF4E10A6FF3B7FAB3BE7FFFFD2F7ADFF7FFF8B00801CFF01FF7F9F7FFF000983AAFF"
                                  )
                                ),
                                (
                                  B(
                                    hex"8D7FFF7FDB85D0B4ACC1FF2F800180C07F69CF84FF347FFF64016D618057E00101BF7BBD7880FF23FF80FF017F017395FFD07F01BE93017F71187F7580D8007EA5"
                                  ),
                                  I(BigInt("2147483647"))
                                )
                              )
                            ),
                            Constr(
                              4,
                              List(
                                Constr(
                                  41,
                                  List(
                                    B(
                                      hex"067D7F010195800169B7017FEF067FEE00CE0103FF7D7F0C80A88B004B807F0193807F7F01FF7FDD80E6B30E0D7F7FB21800CA0046003D80EED557B0807F060100D1B7FFD50015FFFF7F84"
                                    )
                                  )
                                )
                              )
                            ),
                            I(BigInt("2147483647")),
                            I(1)
                          )
                        ),
                        Data.List(
                          List(
                            Constr(2, List(Data.Map(Nil), Constr(31, Nil))),
                            B(hex"B5C9657F7F747F017E00CC7F"),
                            Data.List(
                              List(
                                I(BigInt("2147483647")),
                                Data.List(List(Data.List(List())))
                              )
                            )
                          )
                        )
                      ),
                      (
                        Data.List(
                          List(
                            Constr(
                              42,
                              List(
                                B(
                                  hex"00FF70587F007FE9FFDA7F1FF600FFFF43DF1A7F015580014A1D760000DC80FFD7FF66800101B37F7FA08046FF61F680FF01FFFF8000ACFF6900847F80388001A87F01012301810145517F009801B97FFF0085FF7F803FE4009FA70101A4E0B4"
                                ),
                                Data.List(List(Constr(3, Nil)))
                              )
                            ),
                            Constr(22, List(B(hex"5B8C041F80F9FF2AF0"))),
                            B(hex""),
                            Data.Map(
                              List(
                                (
                                  B(
                                    hex"0100B3AB94007F3A81B37FEB012001FF7FFFFF019600C280FFFF547F4401"
                                  ),
                                  Data.Map(
                                    List(
                                      (
                                        B(
                                          hex"FF8237027FA34D01D97FFF2001B86000297F007F3DC1090000"
                                        ),
                                        B(
                                          hex"01C10001010063088280C87F129701A8307F000000EEFF01A509560B78FF05B180BB15EFC2800DFFD3FFD93196DF4FFF80AA802CFFB90900F301002D00DB7FFFFF00004358004D2401FF01FF80FF011780"
                                        )
                                      )
                                    )
                                  )
                                ),
                                (Data.List(List(B(hex"38FF80A9FF00"))), I(-1))
                              )
                            )
                          )
                        ),
                        B(
                          hex"0CDD917F018080FF807501813E97DA01FFBA29FFDB69377A8062B7B2544AC53E7F7FB20A7F80005C2A287F003E82012A01FFFF01000000A763B2C6370100809E99B83C0D86FFFFE3FF7F80FFBE55E27E8F4614F9709D"
                        )
                      ),
                      (
                        I(BigInt("2147483647")),
                        B(
                          hex"61017F00BA0137896E15C2FFC5700100D400CFA554FF00F4008000BC807F7FF65BFF7F7A0100C8FF007F9780FFF48013657FCB00FFBC"
                        )
                      ),
                      (
                        Data.List(
                          List(
                            B(
                              hex"8172012030B5DF7F7F007F7FC3FFB9D3013636017F127F80807F807F8F9CBC807F55EDFF6DFF995BB1EE7F6D807F5CE292C90001005E650001BA66808001FF808084970101FFBC00BEA182008022800001C1FF6E9D012BEF7F"
                            ),
                            Constr(41, List(Constr(30, Nil), Constr(9, Nil))),
                            Data.Map(List((I(BigInt("2147483647")), I(1000))))
                          )
                        ),
                        B(
                          hex"0101AAACDB2F8021E15080FF0118960799D6487F5A3180FF9D7F01E9D600E87FFF0101FF7FDCFF011E5A05D0DE8D1701EBFF01F0777FE4B70100AA80FF6A9B017F88FF7F8980FFA58021434E007601A01E7F80000000802000807F01FF01007FFF6B00"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        val str = p.pretty.render(80)
        val bytes = UplcCli.uplcToFlat(str)
        assert(Utils.bytesToHex(bytes) == Utils.bytesToHex(p.flatEncoded))
    }
}
