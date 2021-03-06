package jp.scid.specs2.jsoup

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

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
  def haveClass(expected: String) = beTrue ^^  { element: Element =>
    element.hasClass(expected) aka
      "having class '%s' in '%s'".format(expected, element)
  }

  /**
   * 要素が指定したクラスを持っているかを検証します
   * `not have domClass(expected)` と記述するのに利用できます
   * @param expected クラス名
   */
  def domClass(expected: String) = haveClass(expected)

  /**
   * 指定する属性を要素が持っているかを検証します
   * @param attrName 属性名
   * @param value 属性値
   */
  def haveAttr(attrName: String) = beTrue ^^ { element: Element =>
    element hasAttr attrName aka "attribute '%s' on element '%s'".format(attrName, element)
  }

  /**
   * 指定する属性と値を要素が持っているかを検証します
   * @param attrName 属性名
   * @param expected 属性値
   */
  def haveAttr(attrName: String, expected: String) = be_===(expected) ^^ { element: Element =>
    element attr attrName aka "the value of attribute '%s' on element '%s'".format(attrName, element)
  }

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

  /**
   * 指定された名前である input 要素に値が指定されているかを検証します
   * @param name name 属性値
   * @param value 値
   */
  def haveInputElementWithValue(name: String, value: String) = {
    val query = s"input[name=$name]"
    haveInputElement(name) and haveVal(value) ^^ { element: Element =>
      element.select(query).first
    }
  }

  /**
   * 名前の指定された input 要素がチェック属性をもっているかを検証します
   * @param name name 属性値
   * @param isChecked check 属性の有無
   */
  def haveInputElementWithCheck(name: String, isChecked: Boolean) = {
    val query = "input[checked]" + attrSelector("name", name)
    val count = if (isChecked) 1 else 0
    haveElements(query, count)
  }

  /**
   * 名前の指定された input 要素のうち指定した値の要素がチェック属性をもっているかを検証します
   * @param name name 属性値
   * @param value 値
   * @param isChecked check 属性の有無
   */
  def haveInputElementWithCheck(name: String, value: String, isChecked: Boolean) = {
    val query = "input" + attrSelector("name", name) + attrSelector("value", value)
    val haveChecked = haveAttr("checked") ^^ { (_: Element).select(query).first }
    val checkMatcher = if (isChecked) haveChecked else not(haveChecked)
    haveElements(query, 1) and checkMatcher
  }

  /**
   * 指定された名前の textarea 要素にテキストがせっていされているかを検証します
   * @param name name 属性値
   * @param text テキスト値
   */
  def haveTextareaElementWithText(name: String, text: String) = {
    val query = s"textarea[name=$name]"
    haveElements(query, 1) and haveText(text) ^^ { element: Element =>
      element.select(query).first
    }
  }

  /** 属性値のセレクタを返します */
  private def attrSelector(attr: String, value: String): String = """[%s="%s"]""".format(attr, value)
}

