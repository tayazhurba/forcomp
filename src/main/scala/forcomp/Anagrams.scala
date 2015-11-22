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

  /** ����� - ����� ���� `String`. */
  type Word = String

  /** ����������� - ��� ������ `List` ����. */
  type Sentence = List[Word]

  /** `Occurrences` ��� `List` ��� �������� � ������������� �����, ������� ���������� ������� ���������.
   *  ������ ������������ �� �������� ������� � ����.
   *  ��� ������� � ������ ��������.
   *
   *  ����� ������ ���, ������� � ������ ��������, �� �� ������������ **��** �������� ������� ���������
   *  
   *
   *  NB: ���� ������� ������� ����� 0, �� ���� ������ �� ������ �������������� � ������
   */
  type Occurrences = List[(Char, Int)]

  /** ������� - ��� ������ ������ ����
   *  ���� ���������������� �����-������� ��� �������� �������, `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** ������������ ����� � ������ ��������� ��������.
   *
   *  NB: ������� ��������������� ��� ����� �������� � ���������� � ������� � ������ ���������.
   *
   *  NB: �� ������� ������������ `groupBy` ��� ���������� ����� ������!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.toList.groupBy((element: Char) => element).mapValues(_.size).toList.sorted
//

  /** ������������ ����������� � ������ ��������� ��� ��������. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  /** `dictionaryByOccurrences` ���� `Map` �� ��������� ������� ��������� � ������������������ ���� ����,
   *  ������� ����� ��� ����� ���������.
   *  ���� ��� ������������� ������� ������ ��������� ���� �������� �����.
   *
   *  ��������, ����� "eat" ����� ��������� ������ ���������:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  ��� � ����� "ate" � "tea".
   *
   *  ��� ��������, ��� `dictionaryByOccurrences` ����� ��������� ������:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy((element:Word)=>wordOccurrences(element).seq.toList)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** ���������� ������ ���� ����������� ������ ���������.
   *  �������� ���� ��������� `List(('k', 1), ('o', 1))`
   *  �������� ������������� `List(('k', 1), ('o', 1))`.
   *  ����� �������� ������ ������������ `List()`.
   *
   *  ������: ������������ ������ ��������� `List(('a', 2), ('b', 2))` ���������:
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
   *  ������� �� ����� -- ������������ � ������� ���� ����� ���� � ������ �������.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = ???
//  {
//    ((for (i <- "a") yield List()) ++ (for (i <- occurrences) yield i) ++ (for (i <- "a") yield occurrences) ++ (for (i <- occurrences) yield Pair(i._1, i._2))).toList
//  }

  /** �������� ������ ��������� `y` �� ������ ��������� `x`.
   *
   *  ����������� - �� ��� `y` �������� ������������� `x` -- ����� ������ �������� � `y`������ ������� � `x`, � ��� ������� ������ ���� ������ ���� �����
   *  ������� � `x`.
   *
   *  NB: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = ???

  /** ���������� ������ ���� �������� �����������.
   *
   *  ��������� ����������� ����������� �� ������ ��������� ���� ��������
   *  ���� ���� ����������� � ���������� ��� ��������� ���������� ���� � ����� �������,
   *  ��� ��� ����� ��������� � �������.
   *
   *  ����� ���� � ����������� � ��� ��������� �� ������� ���������.
   *  ��������, ����������� `List("I", "love", "you")` ��������� ����������� `List("You", "olive")`.
   *
   *  �����, ����������� � ���� �� �������, ���  ������ ������� �������� ����� ���������� �����������.
   *  ��������, ����������� `List("You", "olive")` � `List("olive", "you")` ��������� ���������
   *  `List("I", "love", "you")`.
   *
   *  ����� ������ ������ ��� ����������� `List("Yes", "man")` � ��� �������� �� �������:
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
   *  ��������� ����������� �� ������� ���� � ��������� ������� - ����� ������� ��������, ���� ��� ��������� ������ � ���������.
   *  ������ ������������ ����� ������ �������������� � �������.
   *
   *  NB: � ������, ���� ����� � ����������� �� �������, �� ����������� �������� ���������� ������ ����, ������� ��� ������ �������������� � ���� ������.
   *
   *  NB: ���� ������ ���� ��������� ������� �����������.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???
}
