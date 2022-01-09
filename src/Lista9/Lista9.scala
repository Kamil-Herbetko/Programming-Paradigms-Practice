package Lista9

//class Time(private var h: Int):
//  if h < 0 then h = 0
//
//  def hour: Int = h
//
//  def hour_=(newHour: Int) =
//    if newHour < 0 then h = 0
//    else h = newHour

//class Time(private var h: Int, private var min: Int):
//  require(0 <= h && h < 24, "Hour should be a number between 0 and 23!")
//  require(0 <= min && min < 60, "Minute should be a number between 0 and 59!")
//
//  def before(other: Time): Boolean =
//    if hour < other.hour then true
//    else if hour == other.hour then
//      if minute < other.minute then true
//      else false
//    else false
//
//  def hour_=(newHour: Int) =
//    require(0 <= newHour && newHour < 24, "Hour should be a number between 0 and 23!")
//    h = newHour
//
//  def hour: Int = h
//
//  def minute_=(newMinute: Int) =
//    require(0 <= min && min < 60, "Minute should be a number between 0 and 59!")
//    min = newMinute
//
//  def minute: Int = min


class Time(h: Int, min: Int):
  require(0 <= h && h < 24, "Hour should be a number between 0 and 23!")
  require(0 <= min && min < 60, "Minute should be a number between 0 and 59!")
  private var minutes: Int = h * 60 + min

  def before(other: Time): Boolean =
    minutes < other.minutes

  def hour: Int = minutes / 60

  def hour_=(newHour: Int) =
    require(0 <= newHour && newHour < 24, "Hour should be a number between 0 and 23!")
    minutes = (minutes % 60) + newHour * 60

  def minute: Int = minutes % 60

  def minute_=(newMinute: Int) =
    require(0 <= newMinute && newMinute < 60, "Minute should be a number between 0 and 59!")
    minutes = (minutes / 60) * 60 + newMinute

class Pojazd(val producer: String, val model: String, val yearOfProduction: Int, var registrationNumber: String = ""):
  def this(producer: String, model: String, registrationNumber: String) = this(producer, model, -1, registrationNumber)
  def this(producer: String, model: String) = this(producer, model, -1, "")


def metoda1() = metoda2()
def metoda2() = metoda3()
def metoda3() = throw new Exception("Wyjątek zgłoszony w metoda3")

object Main:
  def main(args: Array[String]): Unit =
    try
      metoda1()
    catch
      case e: Exception =>
        System.err.println(e.getMessage() + "\n")
        e.printStackTrace()