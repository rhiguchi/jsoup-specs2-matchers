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
  def haveElements(selector: String, count: Int) = haveSize[Elements](count) ^^ { element: Element =>
    element.select(selector) aka
      "elements selected by query '%s' in '%s'".format(selector, element)
  }

  /**
   * 要素が指定する属性を持つかを検証します
   * @param attrName 属性名
   */
  def haveAttr(attrName: String) = beTrue ^^ { element: Element =>
    element hasAttr attrName aka "attribute '%s' of element '%s'".format(attrName, element)
  }

  /**
   * 要素が指定する属性に値を持つかを検証します
   * @param attrName 属性名
   * @param value 属性値
   */
  def haveAttr(attrName: String, value: String) = be_===(value) ^^ { element: Element =>
    element attr attrName aka "attribute '%s' of element '%s'".format(attrName, element)
  }

  /**
   * 要素に id 属性がありその ID が一致するかを検証します
   * @param expected id の値
   */
  def haveId(expected: String) = haveAttr("id") and be_===(expected) ^^ { element: Element =>
    element.id aka "id of element '%s'".format(element)
  }

  /**
   * 要素がクラスを持っているかを検証します
   * @param expected 検証する class 名
   */
  def haveClass(expected: String) = beTrue ^^ { element: Element =>
    element.hasClass(expected) aka
      "having class '%s' in '%s'".format(expected, element)
  }

  /**
   * 要素指定したテキストをもっているかを検証します
   * @param expected 検証する文字列
   */
  def haveText(expected: String) = be_===(expected) ^^ { element: Element =>
    element.text aka "text in '%s'".format(element)
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
}

