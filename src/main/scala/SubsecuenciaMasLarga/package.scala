package object SubsecuenciaMasLarga {
  type Secuencia = Seq[Int]
  type Subsecuencia = Seq[Int]

  def subindices(i: Int, n: Int): Set[Seq[Int]] = {
    // Dados i y n, devuelve todas las posibles secuencias crecientes de enteros entre i y n
    if (i > n) {
      Set(Seq()) // Agregar la secuencia vacía cuando i > n
    } else {
      val indices = (i to n).toList // Crear una lista de enteros en el rango [i,n]

      val resultado = for {
        mascaraDeBists <- 0 until (1 << (n - i)) // Generamos todas  las combinaciones posibles
        subset = indices.zipWithIndex.collect {
              // Creamos una secuencia basada en mascaraDeBists
          case (value, index) if ((mascaraDeBists >> index) & 1) == 1 => value
        }
        if subset.nonEmpty && subset.head >= i && subset.last <= n - 1 // Filtramos las secuencias que son validas
      } yield subset // Agregamos las secuencias en el resultado

      resultado.toSet + Seq() // Convertimos el resultado en un conjunto y agregamos una secuencia vacia
    }
  }

  def subSecuenciaAsoc(s: Secuencia, inds: Seq[Int]): Subsecuencia = {
    //recorremos los indices 'inds' y creamos una nueva secuencia llamada 'Subsecuencia' que esta compuesta+
    // por elemntos de 's' que se encuntran en esos indices
    for (index <- inds) yield s(index)
  }

  def subSecuenciasDe(s: Secuencia): Set[Subsecuencia] = {
    // Creamos un conjunto llamado 'indices' que va a contener los indices de la secuencia 's'
    val indices = s.indices.toSet

    // Generamos subsecuencias incrementales de 's' mediante combinaciones de indices
    (0 to s.length).flatMap { len =>
      // Generamos todas las posibles combinaciones de indices de longitud 'len'
      indices.subsets(len).map { subset =>
        // Para cada combinacion de indices, se crea una subsecuencia llamada "Subsecuencia"
        // donde contiene los elemntos de 's' correspondientes a esos indices
        for (index <- subset.toSeq) yield s(index)
      }
    }.toSet
  }

  def incremental(s: Subsecuencia): Boolean = {

    // La variable llamada 'pares' es una coleccion de pares de elementos consecutivos de 's'
    val pares = for {
      i <- 0 until s.length - 1
    } yield s(i) <= s(i + 1)

    // La funcion 'forall' verifica si todos los elemntos de 'pares' son verdaderos, es decir,
    // que todos los elemntos de 's' son mayores  o iguales a sus predecesores
    pares.forall(identity)
  }

  def subSecuenciasInc(s: Secuencia): Set[Subsecuencia] = {
    // obtenemos los indices de la secuencia 's'
    val indices = s.indices

    (0 to s.length).flatMap(len => indices.combinations(len)).map { indexSeq =>

      // se crea una subsecuencia apartir de los indices
        val subsequence = indexSeq.map(index => s(index))
        if (subsequence.isEmpty || incremental(subsequence)) Some(subsequence) else None
      }.collect { case Some(subseq) => subseq }
      .toSet
  }


  def subsecuenciaIncrementalMasLarga(s: Secuencia): Subsecuencia = {
    // Obtenemos todas las subsecuencias incrementales de 's'
    val subsecuenciasInc = subSecuenciasInc(s)

    val maximaSubsecuencia = (for {
      subseq <- subsecuenciasInc
      if incremental(subseq) // Filtramos las subsecuencias incrementales
    } yield subseq).maxByOption(_.length) // Encontramos las subsecuencia mas larga

    // Devolvemos la subsecuencia mas larga encontrada o una secuencia vacia si no se encuentra ninguna
    maximaSubsecuencia.getOrElse(Seq())
  }

  def ssimlComenzandoEn(i: Int, s: Secuencia): Subsecuencia = {
    if (i < 0 || i >= s.length) return Seq() // verificamps si el indice esta a fuera de rango

    // Comenzamos con la subsecuencia formada por el elemento en la posición 'i'.
    val maximaSeq = (i until s.length).foldLeft(Seq(s(i))) { (acc, j) =>

      (for {
        // Buscamos elementos posteriores a la posición 'j'.
        k <- (j + 1) until s.length

        // Verificamos si el elemento 'k' es mayor que el último elemento en 'acc'.
        if s(k) > acc.last

        // Si se encuentra un elemento mayor, agrega ese elemento a 'acc'.
        // Si no se encuentra ninguno, 'acc' permanece sin cambios.
      } yield s(k)).headOption.map(next => acc :+ next).getOrElse(acc)
    }

    // 'maximaSeq' contiene la subsecuencia incremental más larga que comienza en 'i'.
    maximaSeq
  }

  ///

  def subSecIncMasLargaV2(s: Secuencia): Subsecuencia = {

    // Definimos una función interna para encontrar la subsecuencia incremental más larga comenzando en una posición 'i'.
    def subsecuenciaIncrementalMasLargaDesde(i: Int): Subsecuencia = {

      // Caso base: Si 'i' está fuera del rango, retornamos una secuencia vacía.
      if (i >= s.length) Seq()
      else {
        val subsecuencia = for {
          j <- i + 1 until s.length
          if s(j) > s(i)  // Buscamos elementos mayores que el elemento en la posición 'i'.
        } yield subsecuenciaIncrementalMasLargaDesde(j)

        // Encontramos la subsecuencia incremental más larga a partir de 'i' y guardamos el resultado en 'maximaSubsecuencia'.
        val maximaSubsecuencia = subsecuencia.maxByOption(_.length).getOrElse(Seq())
        // Agregamos el elemento en la posición 'i' al principio de la subsecuencia encontrada.
        s(i) +: maximaSubsecuencia
      }
    }

    // Generamos todas las posibles subsecuencias incrementales comenzando en cada posición 'i'.
    val subsecuencia = for {
      i <- s.indices
    } yield subsecuenciaIncrementalMasLargaDesde(i)

    // Encontramos la subsecuencia incremental más larga de todas las generadas.
    val maximaSubsecuencia = subsecuencia.maxByOption(_.length).getOrElse(Seq())

    maximaSubsecuencia // Retornamos la subsecuencia incremental más larga encontrada en toda la secuencia 's'.
  }


}
