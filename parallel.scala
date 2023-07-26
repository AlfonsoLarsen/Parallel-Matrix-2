import scala.util.Random
import scala.collection.mutable.{ListBuffer, Set}
import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global
import scala.util.control.Breaks._

object Main extends App {
  
  val rows = 9
  val cols = 45
  var matrix = Array.ofDim[Int](rows, cols)
  var usedNumbers = Set[Int]()
  var population = new ListBuffer[Array[Array[Int]]]()
  
  // Inicializa la matriz con números únicos
  for(i <- 0 until rows){
    for(j <- 0 until cols){
      var number = Random.nextInt(rows * cols) + 1
      while (usedNumbers.contains(number)) {
        number = Random.nextInt(rows * cols) + 1
      }
      usedNumbers += number
      matrix(i)(j) = number
    }
  }

  val groupSize = 3
  val rowGroups = matrix.grouped(groupSize).toList
  
  // Comienza el proceso de permutación y verificación
  var attempts = 0
  while (population.isEmpty && attempts < 20) {
    attempts += 1

    val futures = rowGroups.map { group =>
      Future {
        // Permuta las filas del grupo
        val shuffledGroup = Random.shuffle(group.toList).toArray
        shuffledGroup
      }
    }

    Future.sequence(futures).map { shuffledGroups =>
      val shuffledMatrix = shuffledGroups.flatten.toArray
      
      // Comprueba si la matriz resultante cumple con el criterio
      if (isValidMatrix(shuffledMatrix)) {
        // Almacena la matriz en la población si cumple con el criterio
        population += shuffledMatrix.map(_.clone()) // Clonar la matriz
      }
    }
  }

  // Verifica si la matriz es válida (sin duplicados en columnas)
  def isValidMatrix(matrix: Array[Array[Int]]): Boolean = {
    var isValid = true
    breakable {
      for (j <- 0 until cols) {
        val colSet = Set[Int]()
        for (i <- 0 until rows) {
          if (colSet.contains(matrix(i)(j))) {
            isValid = false
            break
          }
          colSet += matrix(i)(j)
        }
      }
    }
    isValid
  }
}
