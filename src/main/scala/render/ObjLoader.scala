package render

import java.io.{File, PrintWriter}
import java.util
import java.util.Scanner

class ObjLoader(var path:String) {
  def getVertices(): Array[Vector] ={
    var s:Scanner = new Scanner(new File(path))
    var vertices = new util.ArrayList[Vector]()
    var result = new util.ArrayList[Vector]()
    while (s.hasNextLine()) {
      var line = s.nextLine()
      var lineS = new Scanner(line)
      if(lineS.hasNext()) {
        var token = lineS.next()
        if (token.equals("v")) {
          var x: Double = lineS.next().toDouble
          var y: Double = lineS.next().toDouble
          var z: Double = lineS.next().toDouble
          vertices.add(new Vector(x, y, z))
        }
        if (token.equals("f")) {
          var x: Int = lineS.next().split("/")(0).toInt
          var y: Int = lineS.next().split("/")(0).toInt
          var z: Int = lineS.next().split("/")(0).toInt
          result.add(vertices.get(x - 1))
          result.add(vertices.get(y - 1))
          result.add(vertices.get(z - 1))
        }
      }
    }
    return result.toArray(new Array[Vector](0))
  }

  def getNormals(): Array[Vector] ={
    var s:Scanner = new Scanner(new File(path))
    var normals = new util.ArrayList[Vector]()
    var result = new util.ArrayList[Vector]()
    while (s.hasNextLine()) {
      var line = s.nextLine()
      var lineS = new Scanner(line)
      if(lineS.hasNext()) {
        var token = lineS.next()
        if (token.equals("vn")) {
          var x: Double = lineS.next().toDouble
          var y: Double = lineS.next().toDouble
          var z: Double = lineS.next().toDouble
          normals.add(new Vector(x, y, z))
        }
        if (token.equals("f")) {
          try {
            var x: Int = lineS.next().split("/")(2).toInt
            var y: Int = lineS.next().split("/")(2).toInt
            var z: Int = lineS.next().split("/")(2).toInt
            result.add(normals.get(x - 1))
            result.add(normals.get(y - 1))
            result.add(normals.get(z - 1))
          }
        }
      }
    }
    return result.toArray(new Array[Vector](0))
  }

  def getTextureCoardinates(): Array[Vector] ={
    var s:Scanner = new Scanner(new File(path))
    var normals = new util.ArrayList[Vector]()
    var result = new util.ArrayList[Vector]()
    while (s.hasNextLine()) {
      var line = s.nextLine()
      var lineS = new Scanner(line)
      if(lineS.hasNext()) {
        var token = lineS.next()
        if (token.equals("vt")) {
          var x: Double = lineS.next().toDouble
          var y: Double = lineS.next().toDouble
          normals.add(new Vector(x, y))
        }
        if (token.equals("f")) {
          try {
            var x: Int = lineS.next().split("/")(1).toInt
            var y: Int = lineS.next().split("/")(1).toInt
            var z: Int = lineS.next().split("/")(1).toInt
            result.add(normals.get(x - 1))
            result.add(normals.get(y - 1))
            result.add(normals.get(z - 1))
          }
        }
      }
    }
    return result.toArray(new Array[Vector](0))
  }
}
