package forcomp

import common._

object Test extends App {
  import Anagrams._
  val x = List(('a', 2), ('b', 2))
  val q = 0 until x.length
  val e = ""
  println(((for (i <- "a") yield List())
    ++ (for (i <- x) yield i)
    ++ (for (i <- "a") yield x)
    ++(for (i <- x) yield Pair(i._1,i._2))).toList
    )

}

object Anagrams {

  /** Слово - всего лишь `String`. */
  type Word = String

  /** Предложение - это список `List` слов. */
  type Sentence = List[Word]

  /** `Occurrences` это `List` пар символов и положительных чисел, которые показывают частоту вхождения.
   *  Список отсортирован по алфавиту символа в паре.
   *  Все символы в нижнем регистре.
   *
   *  Любой список пар, который в нижнем регистре, но не отсортирован **НЕ** является списком вхождений
   *  
   *
   *  NB: Если частота символа равна 0, то этот символ не должен присутствовать в списке
   */
  type Occurrences = List[(Char, Int)]

  /** Словарь - это просто список слов
   *  Есть предопределенный метод-утилита для загрузки словаря, `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Конвертирует слово в список вхождений символов.
   *
   *  NB: символы рассматриваются без учета регистра и приводятся к нижнему в списке вхождений.
   *
   *  NB: вы обязаны использовать `groupBy` для реализации этого метода!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.toList.groupBy((element: Char) => element).mapValues(_.size).toList.sorted
//

  /** Конвертирует предложение в список вхождений его символов. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  /** `dictionaryByOccurrences` есть `Map` из различных списков вхождений в последовательность всех слов,
   *  которые имеют это число вхождений.
   *  Этот мэп предоставляет удобный способ получения всех анаграмм слова.
   *
   *  Например, слово "eat" имеет следующий список вхождений:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Как и слова "ate" и "tea".
   *
   *  Это означает, что `dictionaryByOccurrences` будет содержать запись:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy((element:Word)=>wordOccurrences(element).seq.toList)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Возвращает список всех подмножеств списка вхождений.
   *  Включает само вхождение `List(('k', 1), ('o', 1))`
   *  является подмножеством `List(('k', 1), ('o', 1))`.
   *  Также включает пустое подмножество `List()`.
   *
   *  Пример: подмножества списка вхождений `List(('a', 2), ('b', 2))` следующие:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Порядок не важен -- подмножества в примере выше могут идти в другом порядке.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = ???
//  {
//    ((for (i <- "a") yield List()) ++ (for (i <- occurrences) yield i) ++ (for (i <- "a") yield occurrences) ++ (for (i <- occurrences) yield Pair(i._1, i._2))).toList
//  }

  /** Вычитает список вхождения `y` из списка вхождений `x`.
   *
   *  Предусловие - то что `y` является подмножеством `x` -- любой символ входящий в `y`должен входить в `x`, и его частота должна быть меньше либо равна
   *  частоте в `x`.
   *
   *  NB: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = ???

  /** Возвращает список всех анаграмм предложения.
   *
   *  Анаграмма предложения формируется по списку вхождений всех символов
   *  всех слов предложения и возвращает все возможные комибнации слов с этими буквами,
   *  так что слова находятся в словаре.
   *
   *  Число слов в предложении и его анаграммы не обязаны совпадать.
   *  Например, предложение `List("I", "love", "you")` анаграмма предложения `List("You", "olive")`.
   *
   *  Также, предложения с теми же словами, нов  другом порядке являются двумя различными анаграммами.
   *  Например, предложения `List("You", "olive")` и `List("olive", "you")` различные анаграммы
   *  `List("I", "love", "you")`.
   *
   *  Здесь полный пример для предложения `List("Yes", "man")` и его анаграмм из словаря:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  Различные предложения не обязаны быть в указанном порядке - любой порядок подходит, если все анаграммы входят в множество.
   *  Каждое возвращаемое слово должно присутствовать в словаре.
   *
   *  NB: в случае, если слова в предложении из словаря, то предложение является анаграммой самого себя, поэтому оно должно присутствовать в этом списке.
   *
   *  NB: Есть только одна анаграмма пустого предложения.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???
}
