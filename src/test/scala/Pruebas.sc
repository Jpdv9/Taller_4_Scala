import SubsecuenciaMasLarga._

subindices(2,3)
subindices(0,3)

val s = Seq(20, 30, 10, 40, 15, 16, 17)
subSecuenciaAsoc(s, Seq())
subSecuenciaAsoc(s, Seq(0, 2, 4))
subSecuenciaAsoc(s, Seq(1, 2, 4, 6))

val s1 = Seq(20, 30, 10)
subSecuenciasDe(s1)

val s2 = Seq(1, 2, 3, 4, 5)

subSecuenciasInc(s)
subSecuenciasInc(s1)
subSecuenciasInc(s2)

subsecuenciaIncrementalMasLarga(s)
subsecuenciaIncrementalMasLarga(s1)
subsecuenciaIncrementalMasLarga(s2)

// 1.2

ssimlComenzandoEn(4, Seq(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11))
ssimlComenzandoEn(12, Seq(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11))

subSecIncMasLargaV2(Seq(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11))