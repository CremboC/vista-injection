package vista.util

class Counter[A](private var _current: Int = 0) {
  def current: Int = _current

  def next: Int = {
    _current += 1
    _current
  }
}
