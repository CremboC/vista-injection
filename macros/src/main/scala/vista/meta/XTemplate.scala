package vista.meta

import scala.meta._

/**
  * Created by Crembo on 2017-03-21.
  */
trait XTemplate {

  implicit class XTemplate(self: Template) {
    @inline
    def ctorsWithArguments: Option[Term.Apply] =
      self.parents.map(_.syntax.parse[Term].get).collect { case t: Term.Apply => t }.headOption
  }
}
