package week4.observer

trait Subscriber {
  def handler(pub: Publisher)
}
