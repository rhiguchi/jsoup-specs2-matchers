package jp.scid.specs2.jsoup

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import org.specs2.matcher.Matcher

/**
 * Jsoup の Element オブジェクトの検証メソッドのミックスイン
 */
trait ElementMatchers {
  import Specs2Matchers._

  /**
   * 要素の ID が一致するかを検証します
   * @param query 選択クエリ
   */
  def haveId(expected: String) = be_===(expected) ^^ { element: Element =>
    element.id aka "id of element '%s'".format(element)
  }

  /**
   * 要素が指定したクラスを持っているかを検証します
   * @param expected クラス名
   */
  def haveDomClass[E](expected: String)(implicit domClassified: DomClassified[E]) = beTrue ^^ { element: E =>
    domClassified.haveClass(element, expected) aka
      "having class '%s' in '%s'".format(expected, element)
  }

  /**
   * 要素が指定したクラスを持っているかを検証します
   * `not have domClass(expected)` と記述するのに利用できます
   * @param expected クラス名
   */
  def domClass[E](expected: String)(implicit domClassified: DomClassified[E]) =
    haveDomClass(expected)(domClassified)

  /**
   * 指定する属性を要素が持っているかを検証します
   * @param attrName 属性名
   * @param value 属性値
   */
  def haveAttr[E](attrName: String)(implicit attr: DomAttributed[E]) = beTrue ^^ { element: E =>
    attr.haveAttr(element, attrName) aka "'%s' attribute in element '%s'".format(attrName, element)
  }

  /**
   * 指定する属性と値を要素が持っているかを検証します
   * `not have attr(attrName)` と記述するのに利用できます
   * @param attrName 属性名
   * @param expected 属性値
   */
  def attr[E](attrName: String)(implicit attr: DomAttributed[E]) = haveAttr(attrName)(attr)

  /**
   * 指定する属性と値を要素が持っているかを検証します
   * @param attrName 属性名
   * @param expected 属性値
   */
  def haveAttr[E](attrName: String, expected: String)(implicit attr: DomAttributed[E]) = be_===(expected) ^^ { element: E =>
    attr.attr(element, attrName) aka "the value of attribute '%s' on element '%s'".format(attrName, element)
  }

  /**
   * 指定する属性と値を要素が持っているかを検証します
   * `not have attr(attrName, expected)` と記述するのに利用できます
   * @param attrName 属性名
   * @param expected 属性値
   */
  def attr[E](attrName: String, expected: String)(implicit attr: DomAttributed[E]) = haveAttr(attrName, expected)(attr)

  /**
   * セレクタで参照できる DOM 要素を 1 つ以上含んでいるかを検証します
   * @param query 選択クエリ
   */
  def haveElements(query: String) = haveSize[Elements](be_>=(1)) ^^ { element: Element =>
    element.select(query) aka
      "elements selected by query '%s' in '%s'".format(query, element)
  }

  /**
   * セレクタで参照できる DOM 要素を指定数分含んでいるかを検証します
   * @param query 選択クエリ
   * @param count 要素数
   */
  def haveElements(query: String, count: Int) = haveSize[Elements](count) ^^ { element: Element =>
    element.select(query) aka
      "elements selected by query '%s' in '%s'".format(query, element)
  }

  /**
   * 要素内にテキストが含まれているかを検証します
   * @param expected 文字列
   */
  def haveText(expected: String) = be_===(expected) ^^ { element: Element =>
    element.text aka "text in '%s'".format(element)
  }

  /**
   * 選択される要素が 1 つあり、それが指定した文字列をもっているかを検証します
   * @param query 選択クエリ
   * @param expected 文字列
   */
  def haveElementWithText(query: String, expectedText: String) = {
    haveElements(query, 1) and haveText(expectedText) ^^ { element: Element =>
      element select query get 0
    }
  }

  /**
   * `val()` で取得できる値が一致するかを検証します
   * @param value 検証する値
   */
  def haveVal(value: String) = be_===(value) ^^ { element: Element =>
    element.`val`() aka
      "the value of '%s'".format(element)
  }

  /**
   * 指定された名前である input 要素が 1 つあるかを検証します
   * @param name name 属性値
   */
  def haveInputElement(name: String) = haveElements(s"input[name=$name]", 1)

  /**
   * 指定された名前である type が submit の input 要素があるかを検証します
   * @param name name 属性値
   */
  def haveInputSubmitElement(name: String) = haveElements(s"input[type=submit][name=$name]", 1)
}
