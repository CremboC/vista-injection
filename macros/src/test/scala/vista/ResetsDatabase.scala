package vista

import org.scalatest._

/**
  * @author Paulius Imbrasas
  */
trait ResetsDatabase extends Suite with BeforeAndAfterEach {
  override def afterEach(): Unit = {
    semantics.Database.clear()
  }
}
