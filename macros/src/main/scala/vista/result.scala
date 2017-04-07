package vista

import scala.annotation.StaticAnnotation

class product    extends StaticAnnotation {}
class union      extends StaticAnnotation {}
class intersect  extends StaticAnnotation {}
class difference extends StaticAnnotation {}

object product {
  class pair extends StaticAnnotation {}
}
