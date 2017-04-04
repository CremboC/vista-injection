package vista.util.meta

import scala.meta._

trait XTemplate {

  implicit class XTemplate(self: Template) {
    @inline
    def ctorsWithArguments: Option[Term.Apply] =
      self.parents.map(_.syntax.parse[Term].get).collect { case t: Term.Apply => t }.headOption
  }
}
